
```{r schoenfeld-res2}
#| echo: true
#| error: false
#| warning: true
#| message: true
#| paged.print: true
#| results: "hold"
#| eval: true

# ensure x/y are stored
fit <- cox_iiw_proposed_post
evt <- fit$y[, ncol(fit$y)] == 1            # status==1 rows
X   <- fit$x[evt, , drop = FALSE]

# 1) columns with ~zero variance among events
bad_const <- names(which(vapply(as.data.frame(X), var, 0.0) < .Machine$double.eps))

# 2) linear dependencies among events
rankX <- qr(X)$rank; p <- ncol(X)

# (optional) if you have caret:
# bad_lc <- try(caret::findLinearCombos(X)$remove, silent = TRUE)

bad_const
rankX; p

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# ensure x/y are stored
fit2 <- cox_iiw_proposed_d_post
evt2 <- fit2$y[, ncol(fit2$y)] == 1            # status==1 rows
X2   <- fit2$x[evt, , drop = FALSE]

# 1) columns with ~zero variance among events
bad_const2 <- names(which(vapply(as.data.frame(X2), var, 0.0) < .Machine$double.eps))

# 2) linear dependencies among events
rankX2 <- qr(X2)$rank; p2 <- ncol(X2)

# (optional) if you have caret:
# bad_lc <- try(caret::findLinearCombos(X)$remove, silent = TRUE)

bad_const2
rankX2; p2
```

We identified which specific covariates failed the PH test due to data issues (like sparsity or collinearity), rather than a true violation of the assumption.

```{r schoenfeld-res3}
#| echo: true
#| error: false
#| warning: true
#| message: true
#| paged.print: true
#| results: "hold"
#| eval: true

safe_zph <- function(fit, tr = "rank") {
  out <- vector("list", length(coef(fit))); names(out) <- names(coef(fit))
  for (i in seq_along(out)) out[[i]] <- try(cox.zph(fit, terms = i, transform = tr, global = FALSE), silent = TRUE)
  ok <- !vapply(out, inherits, logical(1), what = "try-error")
  list(ok = ok, results = out[ok], failed = names(out)[!ok])
}
zres <- safe_zph(cox_iiw_proposed_post, "rank")

zres$failed|> 
  data.frame()|> 
  knitr::kable("markdown", caption="terms that are untestable with PH due to sparsity, Discharge to Readmission")

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

safe_zph2 <- function(fit, tr = "rank") {
  out <- vector("list", length(coef(fit))); names(out) <- names(coef(fit))
  for (i in seq_along(out)) out[[i]] <- try(cox.zph(fit, terms = i, transform = tr, global = FALSE), silent = TRUE)
  ok <- !vapply(out, inherits, logical(1), what = "try-error")
  list(ok = ok, results = out[ok], failed = names(out)[!ok])
}
zres2 <- safe_zph(cox_iiw_proposed_d_post, "rank")

zres2$failed|> 
  data.frame()|> 
  knitr::kable("markdown", caption="terms that are untestable with PH due to sparsity, Discharge to Mortality")
```


```{r schoenfeld-res4}
#| echo: true
#| error: false
#| warning: true
#| message: true
#| paged.print: true
#| results: "hold"
#| eval: true
# Residuals: discharge to readmission
res <- resid(cox_iiw_proposed_post, "scaledsch")
time <- as.numeric(dimnames(res)[[1]])
z2 <- loess(res[,"tr_outcome=completion"] ~ time, span=0.50)

# Residuals: discharge to mortality
res_d <- resid(cox_iiw_proposed_d_post, "scaledsch")
time_d <- as.numeric(dimnames(res_d)[[1]])
z2_d <- loess(res_d[,"tr_outcome=completion"] ~ time_d, span=0.50)

# Two plots side by side
par(mfrow=c(1,2))

# Plot 1: Readmission
plot(time, fitted(z2), type="l", col="blue",
     main="Sch. res. – Completion (Readmission)",
     xlab="Time", ylab="Residuals")
lines(supsmu(time, res[,"tr_outcome=completion"]), lty=2, col="darkgray")

# Plot 2: Mortality
plot(time_d, fitted(z2_d), type="l", col="red",
     main="Sch. res. – Completion (Mortality)",
     xlab="Time", ylab="Residuals")
lines(supsmu(time_d, res_d[,"tr_outcome=completion"]), lty=2, col="darkgray")

# Reset layout
par(mfrow=c(1,1))
```


```{r schoenfeld-res5}
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_}
plot(cox.zph(cox_iiw_proposed_post)[which(attr(cox_iiw_proposed_post$coefficients, "names") == "log_lag_dias_treat_imp_sin_na")], lwd=2)
abline(0,0, col="red", lty=3, lwd=2)
abline(h=cox_iiw_proposed_post$coefficients["log_lag_dias_treat_imp_sin_na"], col=3, lwd=2, lty= 3)
legend("bottomleft", legend= c("Reference line for the null effect", "Average hazard over time", "Time-varying hazard"), lty=c(3,2,1), col= c("red",3,1), lwd=2)
#_#_#_#_#_#_#_#_#_#_#_}
km_days_tr <- survfit(Surv(lag_time,time,event)~ log_lag_dias_treat_imp_sin_na+
                        cluster(id), data = data_mine_miss_proc2)

plot(km_days_tr, fun = "cloglog", xlab = "Time (in days) using log",
     ylab = "log-log survival", main = "log-log curves by lag_less_90d_tr1")
legend("bottomright", legend=c("=0", "=1"), col=c("black", "red"), lty=1)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

```


### 2.2. Arjas plots

Arjas plots for PH diagnostics in Cox models (parallel lines ~ slope 1 indicate good fit).

```{r arjasplot}
#| echo: true
#| error: false
#| warning: true
#| message: true
#| paged.print: true
#| results: "hold"
#| eval: true

# Improved Arjas Plot Function (no library calls; uses :: for survival functions)
# Handles data.table or data.frame; converts to data.frame if needed for consistency
arjasplot <- function(formulastring, time, stratifier, status, mydata, subset_cond = NULL) {
  # Convert to data.frame if it's a data.table (avoids scoping issues without loading data.table)
  if ("data.table" %in% class(mydata)) {
    mydata <- as.data.frame(mydata)
  }
  
  # Subset data upfront to exclude NA/non-positive (improves efficiency)
  if (!is.null(subset_cond)) {
    mydata <- mydata[eval(subset_cond, mydata), ]
  } else {
    time_val <- mydata[[time]]
    status_val <- mydata[[status]]
    valid_idx <- !is.na(time_val) & !is.na(status_val) & time_val > 0 & status_val %in% c(0, 1)
    mydata <- mydata[valid_idx, ]
  }
  
  if (nrow(mydata) == 0) {
    stop("No valid data after subsetting. Check variables and conditions.")
  }
  
  # Fit Cox PH model
  fit <- survival::coxph(as.formula(formulastring), data = mydata)
  
  # Get stratifier as factor/integer
  strat_vals <- as.integer(as.factor(mydata[[stratifier]]))
  mmm <- max(strat_vals, na.rm = TRUE)
  if (is.na(mmm)) stop("Stratifier has no valid levels.")
  
  # Unique sorted times
  utime <- sort(unique(mydata[[time]]))
  if (length(utime) == 0) stop("No unique positive times found.")
  
  # Pre-allocate matrices: rows = strata, cols = unique times
  totdel <- matrix(0, nrow = mmm, ncol = length(utime))  # Cumulative events per stratum per time
  totgmat <- matrix(0, nrow = mmm, ncol = length(utime))  # Cumulative hazard per stratum per time
  
  # Group data by stratum for efficiency (vectorized per group)
  unique_strata <- unique(strat_vals)
  for (jj in unique_strata) {
    # Subset for this stratum
    stratum_data <- mydata[strat_vals == jj, ]
    if (nrow(stratum_data) == 0) next
    
    # Fit Cox for this stratum (or use overall fit, but per-stratum for accuracy in Arjas)
    stratum_fit <- survival::coxph(as.formula(formulastring), data = stratum_data)
    
    # Get cumhaz for this stratum (using survfit for the group)
    sf <- survival::survfit(stratum_fit)
    cumhaz_times <- sf$time
    cumhaz_vals <- -log(sf$surv)  # Cumulative hazard Lambda(t)
    
    # Interpolate cumhaz to utime grid (for each observation's time, but aggregate)
    # For Arjas: For each time point, sum cumhaz up to that point for events in stratum
    for (kk in seq_along(utime)) {
      # Events up to utime[kk] in this stratum
      events_up_to_kk <- sum(stratum_data[[status]] * (stratum_data[[time]] <= utime[kk]))
      totdel[jj, kk] <- events_up_to_kk
      
      # Approximate cumhaz at utime[kk]: interpolate or step-function
      if (length(cumhaz_times) > 0 && utime[kk] <= max(cumhaz_times)) {
        # Find closest cumhaz (simple approx; for precision, use approx() function from stats)
        idx_kk <- max(1, which(cumhaz_times <= utime[kk])[1])  # Left-continuous
        cumhaz_at_kk <- cumhaz_vals[idx_kk]
        
        # For Arjas, it's the sum of individual cumhaz at event times, but here aggregate as mean * events (approx)
        # Better: Since per-stratum, use the group's cumhaz * number of events up to t
        totgmat[jj, kk] <- events_up_to_kk * cumhaz_at_kk  # Scaled cumhaz for stratum
      }
    }
  }
  
  # Plot: Events (x) vs Cumhaz (y) per stratum
  x_range <- range(totdel, na.rm = TRUE)
  y_range <- range(totgmat, na.rm = TRUE)
  if (diff(x_range) == 0 || diff(y_range) == 0) {
    stop("No variation in events or cumhaz; check data for events.")
  }
  
  plot(x_range, y_range, main = "Arjas Plot",
       xlab = "Number of Failures (Events) in Stratum",
       ylab = "Estimated Cumulative Hazard",
       type = "n", las = 1)
  
  # Add lines for each stratum
  for (ll in seq_len(mmm)) {
    if (sum(totdel[ll, ]) > 0) {  # Only if events in stratum
      lines(totdel[ll, ], totgmat[ll, ], lty = ll, lwd = 1.5, col = ll)
    }
  }
  
  # Legend
  stratum_levels <- levels(as.factor(mydata[[stratifier]]))
  legend("topleft",  # Adjust position as needed
         legend = paste(stratifier, ":", stratum_levels),
         lty = seq_along(stratum_levels), lwd = 1.5, col = seq_along(stratum_levels),
         bty = "n", cex = 0.8)
  
  # Return matrices for inspection (optional)
  return(list(events = totdel, cumhaz = totgmat))
}

# Conditional Execution: Run the Arjas plot only if the R session is interactive (e.g., RStudio/console), 
# otherwise skip and print a message (useful for batch scripts/jobs where plotting isn't needed or possible)
if (interactive()) {
  # Run the job (Arjas plot)
  cat("Interactive session detected. Running Arjas plot...\n")
  
  result <- arjasplot(
    formulastring = formula_str_corr2, 
    time = "readmit_time_from_adm_m", 
    stratifier = "plan_type_corr", 
    status = "readmit_event", 
    mydata = SISTRAT23_c1_2010_2024_df_prev1z
  )
  
  cat("Arjas plot completed. Check the plot window.\n")
  # Optionally save plot: pdf("arjas_plot.pdf"); [plot code inside function]; dev.off()
} else {
  # Not interactive (e.g., batch job/script) - skip plotting, perhaps compute summary stats instead
  cat("Non-interactive session (e.g., batch job). Skipping Arjas plot generation.\n")
  cat("Data summary for readmission from admission:\n")
  valid_data <- SISTRAT23_c1_2010_2024_df_prev1z[!is.na(readmit_time_from_adm_m) & !is.na(readmit_event) & readmit_time_from_adm_m > 0, ]
  print(summary(valid_data[, c("readmit_time_from_adm_m", "readmit_event", "plan_type_corr")]))
  # Or compute non-plot outputs, e.g., fit the Cox model and print summary(fit)
}

```


### 2.3. Sparsity


## 3. Best subset selection


To close the project, we erase polars objects.

```{r erase-polar-objs}
rm(list = ls()[grepl("_pl$", ls())])
```


```{=html}
<!---
  - Definir libro de códigos: https://docs.google.com/spreadsheets/d/1_eBk7x4ICK7lp5xa72rNfaFrAAwc4W0C3a8326GTlGg/edit?gid=330249768#gid=330249768
Variables: 
  - tiempo a la primera readmisión
- tiempo a la mortalidad
- sexo
- situación de viviend
- condición ocupacional
- estado conyugal
- indice de pobreza de comuna de residencia
- clasificación de urbainización de la comuna de residencia
- escolaridad
- estado de convivencia y cohabitación
- comorbilidad psiquiátrica
- severidad del TUS
- frecuencia de consuom de la sustancia principal al ingreso
- policonsumo
- tiempo de estadía en tratamiento (meses)
- resultado tratamiento
- origen de ingreso a tratamiento
- sustancia principal
- modalidad de tratamiento
Variables interesantes:
  - usuario_de_tribunales_tratamiento_drogas
- tipo_de_vivienda
--->
  ```

## 4. Outliers, influence, Cook D
