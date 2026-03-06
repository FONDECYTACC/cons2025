library(survival)
library(survex)
set.seed(42)

n <- 300
time_all <- rexp(n, 0.1)
status_all <- rbinom(n, 1, 0.4)

idx_train <- 1:200
idx_test  <- 201:300

time_tr <- time_all[idx_train]; status_tr <- status_all[idx_train]
time_te <- time_all[idx_test];  status_te <- status_all[idx_test]

times <- seq(0.5, quantile(time_te, 0.75), length.out = 50)
surv_mat <- matrix(0.5, nrow = length(idx_test), ncol = length(times))

# ---- survex: uses TEST censoring KM ----
y_true <- Surv(time_te, status_te)
ibs_survex <- integrated_brier_score(y_true = y_true, surv = surv_mat, times = times)

# ---- Manual with TEST censoring KM (should match survex) ----
km_cens_test <- survfit(Surv(time_te, 1L - status_te) ~ 1)
G_test <- stepfun(km_cens_test$time, c(1, km_cens_test$surv))

bs_test_cens <- sapply(seq_along(times), function(j) {
  t0 <- times[j]
  S0 <- surv_mat[, j]
  Y0 <- as.numeric(time_te > t0)
  Dt <- status_te
  ind1 <- as.numeric(time_te <= t0 & Dt == 1)
  ind2 <- as.numeric(time_te > t0)
  w <- ind1 / G_test(time_te) + ind2 / G_test(t0)
  mean(w * ((Y0 - S0)^2))
})
ibs_manual_test <- sum(diff(times) * (head(bs_test_cens, -1) + tail(bs_test_cens, -1)) / 2) / (max(times) - min(times))

# ---- Manual with TRAIN censoring KM ----
km_cens_train <- survfit(Surv(time_tr, 1L - status_tr) ~ 1)
G_train <- stepfun(km_cens_train$time, c(1, km_cens_train$surv))

bs_train_cens <- sapply(seq_along(times), function(j) {
  t0 <- times[j]
  S0 <- surv_mat[, j]
  Y0 <- as.numeric(time_te > t0)
  Dt <- status_te
  ind1 <- as.numeric(time_te <= t0 & Dt == 1)
  ind2 <- as.numeric(time_te > t0)
  w <- ind1 / G_train(time_te) + ind2 / G_train(t0)
  mean(w * ((Y0 - S0)^2))
})
ibs_manual_train <- sum(diff(times) * (head(bs_train_cens, -1) + tail(bs_train_cens, -1)) / 2) / (max(times) - min(times))

# ---- Manual with TRAIN censoring + G(T-eps) ----
bs_train_left <- sapply(seq_along(times), function(j) {
  t0 <- times[j]
  S0 <- surv_mat[, j]
  Y0 <- as.numeric(time_te > t0)
  Dt <- status_te
  ind1 <- as.numeric(time_te <= t0 & Dt == 1)
  ind2 <- as.numeric(time_te > t0)
  w <- ind1 / pmax(G_train(pmax(time_te - 1e-8, 0)), 0.05) + ind2 / pmax(G_train(t0), 0.05)
  mean(w * ((Y0 - S0)^2))
})
ibs_manual_train_left <- sum(diff(times) * (head(bs_train_left, -1) + tail(bs_train_left, -1)) / 2) / (max(times) - min(times))

results <- data.frame(
  method = c("survex (test cens)", "manual (test cens, G(T))", "manual (train cens, G(T))", "manual (train cens, G(T-))"),
  IBS = c(ibs_survex, ibs_manual_test, ibs_manual_train, ibs_manual_train_left)
)

write.csv(results, "test_ibs_results.csv", row.names = FALSE)
cat("Done\n")
print(results)
