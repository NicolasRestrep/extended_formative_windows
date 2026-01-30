



set.seed(465200)

nsim   <- 1000   # number of simulations
n_id   <- 1000   # individuals
n_wave <- 4      # waves / time periods

results <- matrix(NA, nrow = nsim, ncol = 4)
colnames(results) <- c("coef_yr", "coef_yt", "coef_yj", "coef_yt_conv")

for (i in 1:nsim) {
  ## ---- Data generation (panel in long format) ----
  
  df <- data.frame(id = rep(1:n_id, each = n_wave),
                   wave = rep(1:n_wave, times = n_id),
                   time = 1:n_wave,
                   # individual random intercepts and time trends (one per id, replicated across waves)
                   u = rep(rnorm(n_id, mean = 3, sd = 1), each = n_wave),
                   ut = rep(rnorm(n_id, mean = 0.2, sd = 0.05), each = n_wave),
                   # id-wave error
                   v  = rnorm(n_id * n_wave, mean = 0, sd = 1)) |> 
    # Data generating processes
    mutate(yr = u + v,           # random change
           yt = u + ut*time + v) |>  # linear time trend 
    mutate(yj = ifelse(time %in% c(1,3), u + v + 1, u + v)) #jump process
  
  
  ## ---- Conventional regression: yt on time ----
  # (Cluster-robust SEs not needed for the coefficient itself; vce(cluster)
  # only affects SEs, not the point estimate.)
  fit_conv <- lm(yt ~ time, data = df)
  coef_yt_conv <- coef(fit_conv)["time"]
  

  ## ---- Reshape wide to compute absolute differences ----
  # This mimics: reshape wide yr yt yj time, i(id) j(wave)
  wide <- reshape(df, idvar = "id", timevar = "wave", direction = "wide")
  # wide now has columns like yr.1, yr.2, yr.3, etc.
  
  for (var in c("yr", "yt", "yj", "time")) {
    wide[[paste0(var, "_D1")]] <- wide[[paste0(var, ".1")]] - wide[[paste0(var, ".2")]]
    wide[[paste0(var, "_D2")]] <- wide[[paste0(var, ".2")]] - wide[[paste0(var, ".3")]]
    wide[[paste0(var, "_D3")]] <- wide[[paste0(var, ".1")]] - wide[[paste0(var, ".3")]]
    wide[[paste0(var, "_D4")]] <- wide[[paste0(var, ".1")]] - wide[[paste0(var, ".4")]]
    wide[[paste0(var, "_D5")]] <- wide[[paste0(var, ".2")]] - wide[[paste0(var, ".4")]]
    wide[[paste0(var, "_D6")]] <- wide[[paste0(var, ".3")]] - wide[[paste0(var, ".4")]]
    # wide[[paste0(var, "_D7")]] <- abs(wide[[paste0(var, ".1")]] - wide[[paste0(var, ".5")]])
    # wide[[paste0(var, "_D8")]] <- abs(wide[[paste0(var, ".2")]] - wide[[paste0(var, ".5")]])
    # wide[[paste0(var, "_D9")]] <- abs(wide[[paste0(var, ".3")]] - wide[[paste0(var, ".5")]])
    # wide[[paste0(var, "_D10")]] <- abs(wide[[paste0(var, ".4")]] - wide[[paste0(var, ".5")]])
  }

  t <- wide |> 
    select(id, yt_D1, yt_D2, yt_D3, yt_D4, yt_D5, yt_D6, time_D1, time_D2, time_D3,
           time_D4, time_D5, time_D6) |> 
    pivot_longer(yt_D1:time_D6) |> 
    separate(name, into = c("var", "pair"), sep = "_") |> 
    spread(var, value) |> 
    mutate(yt = yt^2,
           time = abs(time)) 
  
  t |> 
    group_by(time) |> 
    summarise(mean = mean(yt)) 
  
  lm(yt ~ time, data = t)
  
  
  ## ---- Reshape back to long on *_D variables ----
  # This mimics:
  #   reshape long yr_D yt_D yj_D time_D , i(id) j(di)
  long <- reshape(
    wide,
    varying = c("yr_D1", "yr_D2", "yr_D3",
                "yt_D1", "yt_D2", "yt_D3",
                "yj_D1", "yj_D2", "yj_D3",
                "time_D1", "time_D2", "time_D3"),
    v.names = c("yr_D", "yt_D", "yj_D", "time_D"),
    timevar = "di",
    times   = 1:3,
    direction = "long"
  )
  
  # Keep only the differenced variables and id
  long <- long[, c("id", "yr_D", "yt_D", "yj_D", "time_D")]
  
  ## ---- Regressions using differenced data ----
  fit_yr <- lm(yr_D ~ time_D, data = long)
  fit_yt <- lm(yt_D ~ time_D, data = long)
  fit_yj <- lm(yj_D ~ time_D, data = long)
  
  coef_yr <- coef(fit_yr)["time_D"]
  coef_yt <- coef(fit_yt)["time_D"]
  coef_yj <- coef(fit_yj)["time_D"]
  
  ## ---- Store results ----
  results[i, ] <- c(coef_yr, coef_yt, coef_yj, coef_yt_conv)
}

results_df <- as.data.frame(results)

## Summary statistics (similar to: sum coef_*, de)
summary(results_df)
