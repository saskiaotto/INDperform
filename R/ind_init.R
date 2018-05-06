#' Initialization of indicator-pressure models
#'
#' \code{ind_init} combines the time vector and the indicator (IND) and pressure data into
#' one tibble with defined training and test observations. All INDs are combined
#' with all pressures provided as input.
#'
#' @param ind_tbl A data frame, matrix or tibble containing only the (numeric) IND
#'  variables. Single indicators should be coerced into a data frame to keep the
#'  indicator name. If kept as vector, default name will be 'ind'.
#' @param press_tbl A data frame, matrix or tibble containing only the (numeric)
#'  pressure variables. Single pressures should be coerced into a data frame to keep
#'  the pressure name. If kept as vector, default name will be 'press'.
#' @param time A vector containing the actual time steps (e.g. years; should be the same
#'  as in the IND and pressure data).
#' @param train The proportion of observations that should go into the training data
#'  on which the GAMs are later fitted. Has to be a numeric value between 0 and 1;
#'  the default is 0.9.
#' @param random logical; should the observations for the training data be randomly
#'  chosen? Default is FALSE, so that the last time units (years) are chosen as test data.
#'
#' @details
#' \code{ind_init} will combine every column in ind_tbl with every column in press_tbl
#' so that each row will represent one IND~press combination. The input data will be
#' splitted into a training and a test data set. The returned tibble is the basis for all
#' IND~pressure modelling functions.
#'
#' If not all IND~pressure combinations should be modelled,
#' the respective rows can simply be removed from the output tibble or \code{ind_init} is
#' applied multiple times on data subsets and their output tibbles merged later using
#' e.g. \code{\link[dplyr]{bind_rows}}.
#'
#' @return
#' The function returns a \code{\link[tibble]{tibble}}, which is a trimmed down version of
#' the data.frame(), including the following elements:
#' \describe{
#'   \item{\code{id}}{Numerical IDs for the IND~press combinations.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{ind_train}}{A list-column with indicator values of the training data.}
#'   \item{\code{press_train}}{A list-column with pressure values of the training data.}
#'   \item{\code{time_train}}{A list-column with the time steps of the training data.}
#'   \item{\code{ind_test}}{A list-column with indicator values of the test data.}
#'   \item{\code{press_test}}{A list-column with pressure values of the test data.}
#'   \item{\code{time_test}}{A list-column with the time steps of the test data.}
#'   \item{\code{train_na}}{logical; indicates the joint missing values in the training
#'   IND and pressure data. That includes the original NAs as well as randomly selected
#'   test observations that are within the training period. This vector is needed later
#'   for the determination of temporal autocorrelation.}
#' }
#'
#' @seealso \code{\link[tibble]{tibble}} and the \code{vignette("tibble")} for more
#'  informations on tibbles
#' @family IND~pressure modelling functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in this package
#' press_tbl <- press_ex[ ,-1] # excl. Year
#' ind_tbl <- ind_ex[ ,-1] # excl. Year
#' time <- ind_ex[ ,1]
#' # Assign randomly 50% of the observations as training data and
#' # the other 50% as test data
#' ind_init(ind_tbl, press_tbl, time, train = 0.5, random = TRUE)
#' # To keep the name when testing only one indicator and pressre, coerce both vectors
#' data frames
#' ind_init(ind_tbl = data.frame(MS = ind_tbl$MS), press_tbl = data.frame(Tsum = press_tbl$Tsum),
#'  time, train = .5, random = TRUE)
ind_init <- function(ind_tbl, press_tbl, time, train = 0.9,
  random = FALSE) {

		# Data input validation -----------------------
		# Check parameters
		x_ <- check_ind_press(press_tbl, input = "press")
		y_ <- check_ind_press(ind_tbl)
		time_ <- check_input_vec(time, "time")
		# equal length?
		if ( nrow(y_) != length(time_) || nrow(y_) != nrow(x_) ) {
			 stop("The number of time steps in 'time', 'ind_tbl' and 'press_tbl' have to be the same!")
		}

  if (train < 0 | train > 1) {
  	 stop("The train argument has to be between 0 and 1!")
  }

  # ----------------

  # Creates a vector with id's for each combination
  # ind ~ press
  id <- 1:(ncol(x_) * ncol(y_))
  # Creates a vector with ind
  indicator <- rep(x = names(y_), each = ncol(x_))
  # Creates a vector with press
  pressure <- rep(x = names(x_), times = ncol(y_))
  # Calc n to split input in train and test data
  nr_train <- round(nrow(x_) * train)
  if (random) {
    select_train <- sort(sample(1:nrow(x_),
      size = nr_train, replace = FALSE))
    select_test <- sort((1:nrow(x_))[!(1:nrow(x_) %in%
      select_train)])
  } else {
    select_train <- 1:nr_train
    select_test <- (nr_train + 1):nrow(x_)
  }

  init_tab <- tibble::tibble(id = id, ind = indicator,
    press = pressure)

  # Add train and test data to output
  ind_train <- purrr::map(init_tab$ind,
  	 ~y_[select_train,.])
  press_train <- purrr::map(init_tab$press,
  	 ~x_[select_train,.])
  time_train <- purrr::map(init_tab$ind, ~time_[select_train])
  ind_test <- purrr::map(init_tab$ind,
  	 ~y_[select_test,.])
  press_test <- purrr::map(init_tab$press,
  	 ~x_[select_test,.])
  time_test <- purrr::map(init_tab$press, ~time_[select_test])

  # Name the values
  name_values <- function(x, y) {
    names(x) <- y
    return(x)
  }
  init_tab$ind_train <- purrr::map2(ind_train, time_train,
    ~name_values(.x, .y))
  init_tab$press_train <- purrr::map2(press_train,
    time_train, ~name_values(.x, .y))
  init_tab$time_train <- time_train
  init_tab$ind_test <- purrr::map2(ind_test, time_test,
    ~name_values(.x, .y))
  init_tab$press_test <- purrr::map2(press_test,
    time_test, ~name_values(.x, .y))
  init_tab$time_test <- time_test


  # Add a logical vector of NA presences in EITHER
  # ind_train or press_train (incl. NAs in the
  # original data AND of the test data if random =
  # TRUE)
  if (random == FALSE) {
    train_na <- purrr::map2(init_tab$ind, init_tab$press,
      ~is.na(y_[select_train, .x]) | is.na(x_[select_train,
        .y]))
    init_tab$train_na <- purrr::map2(train_na,
      time_train, ~name_values(.x, .y))
  } else {
    train_na <- purrr::map2(init_tab$ind, init_tab$press,
      ~is.na(y_[select_train, .x]) | is.na(x_[select_train,
        .y]))
    train_na <- purrr::map2(train_na, time_train,
      ~name_values(.x, .y))
    # All test observations within the training period
    # will be considered as NAs (so TRUE):
    test_na <- purrr::map(init_tab$ind, ~rep(TRUE,
      length(select_test)))
    test_na <- purrr::map2(test_na, time_test,
      ~name_values(.x, .y))
    test_na_sub <- seq_along(test_na) %>%
    	 purrr::map(~test_na[[.]][time_test[[.]] %in%
       min(time_train[[.]]):max(time_train[[.]])])
    # Merge test into train
    sorted_years <- order(as.numeric(c(names(train_na[[1]]),
      names(test_na_sub[[1]]))))
    # (as selected years are for all the same I get the
    # sorted years from the first list)
    init_tab$train_na <- seq_along(train_na) %>%
      purrr::map(~c(train_na[[.]], test_na_sub[[.]])[sorted_years])
  }

  ### END OF FUNCTION
  return(init_tab)
}

