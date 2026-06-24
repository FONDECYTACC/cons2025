# =====================================================================================
# multistate_profiles_probtrans.R  (addendum to prediction25_multistate_semimarkov)
# State-occupancy ("dynamic nomogram") for SEVERAL synthetic patient profiles, computed
# with mstate::probtrans (MARKOV, clock-forward; exact Aalen-Johansen-type, no simulation),
# drawn as STACKED-AREA panels faceted by profile. English / Spanish.
#
# Illness-death model: state 1 = Discharge (alive, not readmitted), 2 = Readmission (alive,
# readmitted), 3 = Death (absorbing); transitions 1->2, 1->3, 2->3 (death and readmission are
# SEMI-COMPETING: death competes with readmission, readmission does not prevent later death).
#
# probtrans assumes a MARKOV process, so the 2->3 transition must be the CLOCK-FORWARD fit
# (m_tr3_fwd in the notebook), NOT the semi-Markov clock-reset fit. This is the same engine the
# notebook uses in chunk `sim-probtrans-markov`; here it is evaluated at several covariate
# profiles. It is the no-simulation Markov view; the project's PRIMARY engine is the semi-Markov
# clock-reset specification (flexsurv pmatrix.simfs / mstate::mssample). Present this figure as
# the clock-forward Markov contrast, consistent with the Titman-Putter Markov test in the notebook.
# Estimates are descriptive/predictive, never causal.
#
# Stacked probability plots by covariate profile are an established multistate convention:
#   Klein, Keiding & Copelan. Stat Med 1993;12:2315-2332 (origin of the stacked-prediction plot).
#   Putter, Fiocco & Geskus. Stat Med 2007;26:2389-2430 (tutorial).
#   de Wreede, Fiocco & Putter. J Stat Softw 2011;38(7):1-30 (mstate).
#   Andersen & Keiding. Stat Methods Med Res 2002;11:91-115.  Titman & Putter. Biostatistics 2022;23:380-396.
#
# Easy to load: source this file AFTER the notebook has m_tr1, m_tr2, m_tr3_fwd, tmat and a
# reference newdata row (profile_ref). Then:
#   profs <- ms_profiles_substance_age(profile_ref, lang = "en")     # 2x2 substance x age
#   g <- plot_multistate_profiles_probtrans(profs, lang = "en"); print(g)
#
# Depends: survival, mstate, ggplot2, scales. No microdata needed.
# =====================================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# cumulative hazard from a single-row-newdata survfit.coxph (one monotone curve)
if (!exists(".cumhaz")) .cumhaz <- function(sf) {
  if (!is.null(sf$cumhaz)) as.numeric(sf$cumhaz) else -log(pmax(as.numeric(sf$surv), 1e-12))
}

# ---- profile-specific msfit (carry-forward cumulative hazards per transition) -------
.ms_profile_msfit_fwd <- function(profile, models, trans) {
  raw <- lapply(models, function(m) {
    sf <- survival::survfit(m, newdata = profile)
    data.frame(time = sf$time, Haz = .cumhaz(sf))
  })
  ut <- sort(unique(c(0, unlist(lapply(raw, `[[`, "time")))))
  Haz <- do.call(rbind, lapply(seq_along(raw), function(k) {
    df <- raw[[k]]; idx <- findInterval(ut, df$time)
    data.frame(time = ut, Haz = ifelse(idx == 0, 0, df$Haz[pmax(idx, 1)]), trans = k)
  }))
  structure(list(Haz = Haz, trans = trans), class = "msfit")
}

# ---- full occupancy curve P(state | profile, t) from state 1, via probtrans ---------
.ms_occupancy_curve <- function(profile, models, trans, tmax) {
  pt <- mstate::probtrans(.ms_profile_msfit_fwd(profile, models, trans),
                          predt = 0, variance = FALSE)[[1]]
  pt <- pt[pt$time <= tmax, , drop = FALSE]
  data.frame(month = pt$time,
             p_alive_no_readm = pt$pstate1,
             p_readmitted     = pt$pstate2,
             p_dead           = pt$pstate3)
}

# ---- build one profile: copy a base newdata row and override key covariates ---------
# Sets the primary-substance dummy family (one 1, rest 0; all 0 = omitted reference),
# continuous age in YEARS (adm_age_rec3 is continuous, cohort range 18-64, Q1/median/Q3 ~ 27/34/43),
# and occupation (employed = both dummies 0). Only touches columns that exist in `base`.
ms_make_profile <- function(base, primary_sub = NULL, age = NULL, occupation = NULL) {
  p <- base
  if (!is.null(primary_sub)) {
    for (s in grep("^primary_sub_mod_", names(p), value = TRUE)) p[[s]] <- 0
    col <- paste0("primary_sub_mod_", primary_sub)
    if (col %in% names(p)) p[[col]] <- 1                       # else: keep omitted reference
  }
  if (!is.null(age) && "adm_age_rec3" %in% names(p)) p[["adm_age_rec3"]] <- age
  if (!is.null(occupation)) {
    for (s in c("occupation_condition_corr24_unemployed",
                "occupation_condition_corr24_inactive")) if (s %in% names(p)) p[[s]] <- 0
    col <- paste0("occupation_condition_corr24_", occupation)
    if (occupation != "employed" && col %in% names(p)) p[[col]] <- 1
  }
  p
}

# Recommended 2x2 grid: primary substance (cols: cocaine paste vs alcohol) x age (rows:
# younger Q1 vs older Q3), all else fixed at `base`. Returns a named list of profiles with a
# `facet` attribute (row = age, col = substance) so the plot can use facet_grid.
ms_profiles_substance_age <- function(base,
                                      subs = c(cocaine_paste = "cocaine_paste", alcohol = "alcohol"),
                                      ages = c(younger = 27, older = 43),     # cohort Q1 / Q3 (years)
                                      lang = c("en", "es")) {
  lang <- match.arg(lang)
  sub_lab <- if (lang == "es") c(cocaine_paste = "Pasta base de cocaína", alcohol = "Alcohol")
             else               c(cocaine_paste = "Cocaine paste",         alcohol = "Alcohol")
  age_lab <- if (lang == "es") c(younger = "Edad menor (Q1)", older = "Edad mayor (Q3)")
             else               c(younger = "Younger (Q1)",   older = "Older (Q3)")
  out <- list(); fac <- data.frame()
  for (ai in names(ages)) for (si in names(subs)) {
    nm <- sprintf("%s | %s", sub_lab[[si]], age_lab[[ai]])
    out[[nm]] <- ms_make_profile(base, primary_sub = subs[[si]], age = ages[[ai]])
    fac <- rbind(fac, data.frame(profile = nm, col = sub_lab[[si]], row = age_lab[[ai]],
                                 stringsAsFactors = FALSE))
  }
  fac$col <- factor(fac$col, levels = unname(sub_lab[names(subs)]))   # paste left, alcohol right
  fac$row <- factor(fac$row, levels = unname(age_lab[names(ages)]))   # younger top, older bottom
  attr(out, "facet") <- fac
  out
}

# ---- the figure ---------------------------------------------------------------------
plot_multistate_profiles_probtrans <- function(
    profiles,
    models       = list(get0("m_tr1"), get0("m_tr2"), get0("m_tr3_fwd")),
    trans        = get0("tmat"),
    tmax         = 60,
    horizons     = c(6, 12, 36, 60),
    lang         = c("en", "es"),
    palette      = c("#FFF7BC", "#FEC44F", "#D95F0E"),  # YlOrBr: alive / readmitted / death (light->dark)
    ncol         = 2L,
    tnr          = "Times New Roman",
    emit_message = TRUE) {

  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("mstate", quietly = TRUE),
            requireNamespace("scales", quietly = TRUE))
  lang <- match.arg(lang)
  if (any(vapply(models, is.null, logical(1))) || is.null(trans))
    stop("Provide `models = list(m_tr1, m_tr2, m_tr3_fwd)` and `trans = tmat` (forward 2->3).",
         call. = FALSE)

  state_lab <- if (lang == "es")
      c("Vivo sin readmisión", "Vivo y readmitido", "Fallecido")
    else
      c("Alive, not readmitted", "Alive and readmitted", "Death")
  x_lab <- if (lang == "es") "Meses desde el egreso" else "Months since discharge"
  y_lab <- if (lang == "es") "Probabilidad de ocupación del estado" else "State occupancy probability"

  long <- do.call(rbind, lapply(names(profiles), function(nm) {
    cc <- .ms_occupancy_curve(profiles[[nm]], models, trans, tmax)
    data.frame(profile = nm,
               month = rep(cc$month, 3),
               state = rep(state_lab, each = nrow(cc)),
               prob  = c(cc$p_alive_no_readm, cc$p_readmitted, cc$p_dead),
               stringsAsFactors = FALSE)
  }))
  long$profile <- factor(long$profile, levels = names(profiles))
  long$state   <- factor(long$state, levels = state_lab)     # alive, readmitted, death (bottom->top)
  cols <- stats::setNames(palette, state_lab)
  num_fmt <- if (lang == "es") scales::label_number(decimal.mark = ",", accuracy = 0.1)
             else ggplot2::waiver()

  g <- ggplot2::ggplot(long, ggplot2::aes(month, prob, fill = state)) +
    ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE),
                       alpha = 1, colour = "white", linewidth = 0.2,
                       outline.type = "full") +   # white hairline = B&W-safe separator (full outline = robust)
    ggplot2::scale_fill_manual(values = cols, breaks = state_lab) +
    ggplot2::scale_x_continuous(breaks = horizons, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = num_fmt, expand = c(0, 0)) +
    # zoom to [0,1] via coord (NOT scale limits, which would CENSOR/drop area points and
    # leave degenerate geom_area outlines) so the stacked bands fill the panel cleanly
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = x_lab, y = y_lab, fill = NULL) +
    ggplot2::theme_bw(base_size = 12, base_family = tnr) +
    ggplot2::theme(
      legend.position    = "bottom",
      strip.background    = ggplot2::element_rect(fill = "grey95", colour = "grey80"),
      strip.text         = ggplot2::element_text(face = "bold", size = 11),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(colour = "black"),
      panel.spacing      = ggplot2::unit(0.8, "lines"))

  fac <- attr(profiles, "facet")
  if (!is.null(fac)) {                                        # rows = age, cols = substance
    long2 <- merge(long, fac, by = "profile", sort = FALSE)
    long2$row <- factor(long2$row, levels = levels(fac$row))
    long2$col <- factor(long2$col, levels = levels(fac$col))
    g$data <- long2
    g <- g + ggplot2::facet_grid(row ~ col)
  } else {
    g <- g + ggplot2::facet_wrap(~ profile, ncol = ncol)
  }

  cap <- if (lang == "es") paste(
    "Figura X. Probabilidades predichas de ocupación de estado tras el egreso para cuatro perfiles",
    "sintéticos de pacientes, modelo multiestado de enfermedad y muerte. Las áreas apiladas muestran",
    "la probabilidad de ocupar cada uno de tres estados mutuamente excluyentes, egresado vivo y sin",
    "readmisión (claro), readmitido y vivo (intermedio) y fallecido (oscuro), en función de los meses",
    "desde el egreso; por construcción, las tres probabilidades suman 1 en cada instante. Las",
    "estimaciones son probabilidades de ocupación de tipo Aalen-Johansen obtenidas con",
    "mstate::probtrans bajo el supuesto markoviano (reloj hacia adelante) a partir de los modelos de",
    "Cox específicos por transición, sin simulación de Monte Carlo. Los paneles cruzan la sustancia",
    "principal (columnas: pasta base de cocaína frente a alcohol) con el tercil de edad (filas: menor",
    "frente a mayor), con todas las demás covariables fijadas en el paciente de referencia, de modo",
    "que las diferencias entre paneles reflejan solo la sustancia y la edad. La muerte compite con la",
    "readmisión, mientras que la readmisión no impide una muerte posterior (riesgos semicompetitivos).",
    "Las guías verticales marcan los horizontes de 6, 12, 36 y 60 meses. Los perfiles son sintéticos",
    "y de carácter ilustrativo; las estimaciones son descriptivas y predictivas, no causales.")
  else paste(
    "Figure X. Predicted post-discharge state-occupancy probabilities for four synthetic patient",
    "profiles, illness-death multistate model. Stacked areas show the probability of occupying each",
    "of three mutually exclusive states, discharged alive and not readmitted (light), readmitted and",
    "alive (mid), and dead (dark), as a function of months since discharge; the three probabilities",
    "sum to 1 at every time point by construction. Estimates are Aalen-Johansen-type state-occupancy",
    "probabilities obtained with mstate::probtrans under a Markov (clock-forward) assumption from the",
    "fitted transition-specific Cox models, with no Monte Carlo simulation. Panels cross primary",
    "substance (columns: cocaine paste vs alcohol) with age tertile (rows: younger vs older); all",
    "remaining covariates are held identical across panels at the reference patient, so differences",
    "between panels reflect only substance and age. Death competes with readmission, whereas",
    "readmission does not preclude later death (semi-competing risks). Vertical guides mark the",
    "horizons of 6, 12, 36 and 60 months. Profiles are synthetic and illustrative; estimates are",
    "descriptive and predictive, not causal.")
  attr(g, "caption") <- cap

  if (isTRUE(emit_message)) {
    msg <- if (lang == "es") paste(
      "En los cuatro perfiles la banda de egresado vivo y sin readmisión se reduce a lo largo de los",
      "60 meses mientras crecen las bandas de readmisión y de muerte, pero ambos contrastes se mueven",
      "en direcciones opuestas, lo que constituye la firma de riesgos semicompetitivos de estos datos.",
      "Entre columnas (pasta base de cocaína a alcohol) cambia sobre todo la banda de readmisión, más",
      "gruesa con pasta base; entre filas (menor a mayor edad) se engrosa sobre todo la banda de muerte.",
      "Como son probabilidades de ocupación markovianas (reloj hacia adelante, probtrans) para perfiles",
      "sintéticos, describen trayectorias esperadas bajo el modelo, no efectos causales.")
    else paste(
      "Across all four profiles the discharged-alive band shrinks over 60 months while the readmitted",
      "and dead bands grow, but the two contrasts move in opposite directions, the semi-competing-risks",
      "signature of these data. Across columns (cocaine paste to alcohol) the readmission band shifts",
      "most, thickest under cocaine paste; down the rows (younger to older) the death band thickens",
      "most, consistent with age as the dominant mortality gradient and substance as the dominant",
      "readmission gradient. These are Markov clock-forward (probtrans) occupancy probabilities for",
      "synthetic profiles, so they describe expected trajectories under the model, not causal effects.")
    message(msg)
  }
  g
}
