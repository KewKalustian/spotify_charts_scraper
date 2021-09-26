# Monte carlo simulation to identify the median (and its 95% CI) to be used as 
# factor according to which the estimated scraping and rendering time
# is calculated in the App.

# Random sample month / cumulative scraping and plot rendering time for 31 days 
# (in seconds).

time <- c(10.93,
          15.8,
          23.04,
          28.69,
          35.59,
          41.62,
          47.14,
          49.68,
          56.68,
          60.31,
          69.05,
          72.09,
          81.40,
          83.05,
          90.01,
          100.34,
          114.91,
          115.12,
          121.42,
          135.43,
          138.17,
          164.55,
          152.67,
          158.21,
          174.41,
          179.76,
          190.56,
          171.90,
          199.03,
          205.6,
          209.12)
#########################
### Monte Carlo Sim. ####
#########################

# Reps.
b <- 10e4

# sample size
n <- length(time)

# repro.
set.seed(123, sample.kind = "Rounding")

# estimating the medians of the scraping time diffs between days.

avg_diff <- replicate(b,{
 boot_time <- sample(diff(time), n-1, replace = T)
 median(boot_time)
})


# 95% CI
round(quantile(avg_diff, c(.025,.975)),2)


##################################################################
