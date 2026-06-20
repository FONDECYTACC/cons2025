## nomogram_predict.R ----------------------------------------------------------
## Prediction engine for the DE-IDENTIFIED multistate nomogram kit.
##
## This file is the runtime engine for the Shiny demonstrator. It consumes a
## `kit` object built by build_nomogram_kit.R and contains NO microdata: every
## prediction uses precomputed per-stratum baseline cumulative hazards plus a
## data-free Cox object (kept only to evaluate linear predictors), so there is
## no survfit() call and no individual-level data at runtime.
##
## It reproduces, to machine precision, the authoritative engine functions
## predict_profile_markov_pooled() and predict_profile_ci_mvn_pooled() from
## pooled_probtrans_engine_INTERNAL.rds. The ONLY substitution is the baseline
## source: H0_stratum(t) * exp(lp) instead of survfit(model, newdata = profile).
##
## States: 1 = alive, not readmitted; 2 = alive, readmitted; 3 = dead (absorbing).
## Markov clock-forward (primary). Pooled over 5 multiple imputations (Rubin).
##
## Kit structure (see build_nomogram_kit.R):
##   kit$models[[i]] : list(m1, m2, m3, b1, b2, b3) for imputation i
##                       mk = stripped coxph (predict type="lp" only)
##                       bk = list(strata = list(per stratum: time, H0), combos, vars)
##   kit$tmat, kit$predictor_spec, kit$reference, kit$horizons, kit$m_imp, kit$version

suppressPackageStartupMessages({
  library(survival)
  library(mstate)
})

## --- build a one-row profile data.frame from raw predictor values ------------
## `raw` is a named list/vector of raw predictor columns; anything omitted keeps
## its reference value. Factor columns are coerced to the kit's factor levels.
kit_profile_from_raw <- function(raw, kit) {
  p <- kit$reference
  for (nm in names(raw)) {
    if (!nm %in% names(p)) next
    if (is.factor(p[[nm]])) p[[nm]] <- factor(as.character(raw[[nm]]), levels = levels(p[[nm]]))
    else                    p[[nm]] <- as.numeric(raw[[nm]])
  }
  p
}

## --- internal: locate the stratum column for a profile -----------------------
.kit_match_stratum <- function(profile, base) {
  key <- vapply(base$vars, function(v) as.character(profile[[v]]), character(1))
  hit <- which(apply(base$combos, 1, function(r) all(as.character(r) == key)))
  if (length(hit) != 1) stop("stratum match failed for: ", paste(base$vars, collapse = ", "))
  hit
}

## --- internal: profile cumulative hazard for one transition ------------------
## H0_stratum(t) * exp(lp). predict(reference = "sample") matches survfit's
## global-mean centering exactly (the stratum-centering default would not).
.kit_cumhaz <- function(m, base, profile) {
  j  <- .kit_match_stratum(profile, base)
  lp <- as.numeric(predict(m, newdata = profile, type = "lp", reference = "sample"))
  list(time = base$strata[[j]]$time, Haz = base$strata[[j]]$H0 * exp(lp))
}

## --- internal: stratum-centred z-template for CI draws -----------------------
## The MVN draw scaling exp(z . Delta) must use STRATUM-centred z to match the
## perturbed-coefficient survfit (verified by ground truth): the fixed point
## baseline is stratum-centred, so the draw ratio is exp((x - stratum_mean) . D).
## predict's stratum centring needs the training data, so the kit computes the
## data-free sample-centred z and adds the precomputed per-stratum correction
## corr = (sample_mean - stratum_mean). This reproduces the full engine's z.
.kit_z <- function(m, base, profile) {
  p <- length(coef(m)); if (!p) return(numeric(0))
  j <- .kit_match_stratum(profile, base)
  bb <- m; z <- numeric(p)
  for (k in seq_len(p)) { bk <- numeric(p); bk[k] <- 1; bb$coefficients <- bk
    z[k] <- as.numeric(predict(bb, newdata = profile, type = "lp", reference = "sample")) }
  z + base$strata[[j]]$corr
}

## --- internal: MVN(0, V) draws via Cholesky (no MASS dependency) -------------
.kit_mvn0 <- function(V, B) {
  p <- nrow(V); if (!p) return(matrix(0, B, 0))
  matrix(rnorm(B * p), B, p) %*% chol((V + t(V)) / 2)
}

## --- internal: assemble a stacked msfit (3 transitions) on a common grid -----
.kit_msfit <- function(profile, mods, bases, trans) {
  raw <- lapply(seq_along(mods), function(k) .kit_cumhaz(mods[[k]], bases[[k]], profile))
  ut  <- sort(unique(c(0, unlist(lapply(raw, `[[`, "time")))))
  Haz <- do.call(rbind.data.frame, lapply(seq_along(raw), function(k) {
    df <- raw[[k]]; idx <- findInterval(ut, df$time)
    data.frame(time = ut, Haz = ifelse(idx == 0, 0, df$Haz[pmax(idx, 1)]), trans = k)
  }))
  structure(list(Haz = Haz, trans = trans), class = "msfit")
}

## --- POINT estimate, single imputation ---------------------------------------
.kit_point_one <- function(profile, tvec, M, trans) {
  pt <- probtrans(.kit_msfit(profile, list(M$m1, M$m2, M$m3),
                             list(M$b1, M$b2, M$b3), trans),
                  predt = 0, variance = FALSE)[[1]]
  do.call(rbind, lapply(tvec, function(h) {
    i <- which.min(abs(pt$time - h))
    c(pt$pstate1[i], pt$pstate2[i], pt$pstate3[i])
  }))
}

## --- POINT estimate, pooled over imputations (Rubin Q-bar) -------------------
kit_predict_point <- function(profile, kit, tvec = c(3, kit$horizons)) {
  preds <- lapply(kit$models, function(M) .kit_point_one(profile, tvec, M, kit$tmat))
  acc   <- Reduce(`+`, preds) / length(preds)
  data.frame(month = tvec,
             p_alive_no_readm = acc[, 1],
             p_readmitted     = acc[, 2],
             p_dead           = acc[, 3])
}

## --- CI, single imputation (mirrors engine predict_profile_ci_mvn) -----------
.kit_ci_one <- function(profile, M, tvec, trans, B = 300, n_grid = 200, cl = 0.95, seed = 2125) {
  set.seed(seed)
  mods  <- list(M$m1, M$m2, M$m3); bases <- list(M$b1, M$b2, M$b3); K <- 3
  grid  <- sort(unique(c(seq(0, max(tvec), length.out = n_grid), tvec)))
  base_grid <- sapply(seq_len(K), function(k) {
    ch <- .kit_cumhaz(mods[[k]], bases[[k]], profile)
    idx <- findInterval(grid, ch$time); ifelse(idx == 0, 0, ch$Haz[pmax(idx, 1)])
  })
  z   <- lapply(seq_len(K), function(k) .kit_z(mods[[k]], bases[[k]], profile))
  Del <- lapply(mods, function(m) .kit_mvn0(vcov(m), B))
  hidx <- match(tvec, grid)
  run_pt <- function(sc) {
    Hb <- data.frame(time = rep(grid, K), Haz = as.numeric(sweep(base_grid, 2, sc, `*`)),
                     trans = rep(seq_len(K), each = length(grid)))
    probtrans(structure(list(Haz = Hb, trans = trans), class = "msfit"), predt = 0, variance = FALSE)[[1]]
  }
  pt0 <- run_pt(rep(1, K)); occ <- array(NA_real_, c(B, length(tvec), 3))
  for (b in seq_len(B)) {
    sc <- vapply(seq_len(K), function(k) if (length(z[[k]])) exp(sum(z[[k]] * Del[[k]][b, ])) else 1, numeric(1))
    pt <- run_pt(sc); occ[b, , 1] <- pt$pstate1[hidx]; occ[b, , 2] <- pt$pstate2[hidx]; occ[b, , 3] <- pt$pstate3[hidx]
  }
  a <- (1 - cl) / 2
  out <- data.frame(month = tvec, alive = pt0$pstate1[hidx], readm = pt0$pstate2[hidx], dead = pt0$pstate3[hidx])
  for (s in 1:3) { nm <- c("alive", "readm", "dead")[s]; qq <- apply(occ[, , s], 2, quantile, c(a, 1 - a), na.rm = TRUE)
    out[[paste0(nm, "_lo")]] <- qq[1, ]; out[[paste0(nm, "_hi")]] <- qq[2, ] }
  out
}

## --- CI, pooled over imputations (Rubin total variance) ----------------------
## within  = mean of per-imputation MVN variances; between = variance of the
## per-imputation point estimates; total = within + (1 + 1/m) * between.
kit_predict_ci <- function(profile, kit, tvec = c(3, kit$horizons), B = 300, cl = 0.95) {
  zc <- qnorm(1 - (1 - cl) / 2); m <- length(kit$models)
  per <- lapply(kit$models, .kit_ci_one, profile = profile, tvec = tvec, trans = kit$tmat, B = B, cl = cl)
  out <- data.frame(month = tvec)
  for (s in c("alive", "readm", "dead")) {
    pts <- sapply(per, function(p) p[[s]])
    hw  <- sapply(per, function(p) (p[[paste0(s, "_hi")]] - p[[paste0(s, "_lo")]]) / 2)
    if (is.null(dim(pts))) { pts <- matrix(pts, nrow = 1); hw <- matrix(hw, nrow = 1) }
    qbar <- rowMeans(pts); within <- rowMeans((hw / zc)^2); between <- apply(pts, 1, var)
    total <- within + (1 + 1 / m) * between
    out[[s]] <- qbar
    out[[paste0(s, "_lo")]] <- pmax(0, qbar - zc * sqrt(total))
    out[[paste0(s, "_hi")]] <- pmin(1, qbar + zc * sqrt(total))
  }
  out
}
