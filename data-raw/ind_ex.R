# Example of indicators for assessing the state of the pelagic food web
# in the Central Baltic Sea. The data has been slightly modified from the
# orginal data represented in Otto et al. (2018).

ind_ex <- read.csv("data-raw/ind_ex.csv", sep =",")

devtools::use_data(ind_ex, overwrite = TRUE)
