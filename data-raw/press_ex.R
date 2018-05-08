# Example of environmental variables that exert potential pressures on
# the pelagic food web in the Central Baltic Sea. The data has been slightly modified from the
# orginal data represented in Otto et al. (2018).

press_ex <- read.csv("data-raw/press_ex.csv", sep =";")

devtools::use_data(press_ex, overwrite = TRUE)
