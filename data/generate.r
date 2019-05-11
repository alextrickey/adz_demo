

# Part I - TS Modeling - Average Hourly Revenue
rev_ts <- function(t, amplitude = 1, ave_value = 1.5, s_noise = 0.5, shift = 7) {
  r <- ave_value + amplitude * sin(2 * pi * (t - shift) / 24)
  r <- r + rnorm(n = length(t), mean = 0, sd = s_noise)
  pmax(r, 0)
}



t = 1:24
plot(t, sin(2 * pi * t / 24))
plot(t, rev_ts(t))
