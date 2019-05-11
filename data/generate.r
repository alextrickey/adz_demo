#Define Data for First Examples

library(lubridate)

#Function to Generate Cyclic Data with Noise
cyclic_data <- function(t, amplitude = 1, ave_value = 0, s_noise = 0, shift = 0) {
  c <- ave_value + amplitude * sin(2 * pi * (t - shift) / 24)
  c + rnorm(n = length(t), mean = 0, sd = s_noise)
}

#Function to Generate Click/Volume-like Data
vol_ts <- function(t, amplitude = 9000, ave_value = 8000, s_noise = 2000, shift = 7) {
  r <- cyclic_data(t, amplitude, ave_value, s_noise, shift)
  r <- round(r, digits = 0)
  pmax(r, 0)
}

#Function to Generate Revenue Data
rev_ts <- function(t, amplitude = 1, ave_value = 1.5, s_noise = 0.5, shift = 7) {
  r <- cyclic_data(t, amplitude, ave_value, s_noise, shift)
  r <- round(r, digits = 2)
  pmax(r, 0)
}
