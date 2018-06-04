#' Create a code indicating the significance of values given by p_val.
#'
#' \code{get_signif_code} is a helper function for \code{\link{model_gam}} and
#' \code{\link{model_gamm}} to create an easy-to-read output for the significance
#' of the smoothing term in the GAM(M)s (shown in the \code{signif_code}
#' column of the output tibbles).
#'
#' @param p_val A vector with the p-values of the smoothing term.
#'
#' @details
#' The following code is adopted from \code{summary.gam}:
#' \tabular{lllll}{
#'   " "  \tab = not significant (> .1)\cr
#'   "."  \tab = close to significant (.05 < .1)\cr
#'   "*"  \tab  = significant (.01 < .05)\cr
#'   "**"  \tab = highly significant (.001 < 0.01)\cr
#'   "***"  \tab = absolute significant (<= .001)
#' }
#'
#' @return
#' The function returns a character vector of the same length then the
#' input vector, with one symbol for each p-value.
#'
#' @seealso \code{\link{model_gam}}, \code{\link{model_gamm}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' x <- tibble::tibble(p_val = list("test", NA, runif(1, 0, .1), runif(1, 0, .01)))
#' x$code <- get_signif_code(x$p_val)
get_signif_code <- function(p_val) {
  if (any(purrr::map_dbl(p_val, length) != 1)) {
    stop("Every object in p_val should have length 1!")
  }
  x <- suppressWarnings(as.numeric(p_val))
  signif_code <- vector("character", length = length(x))
  signif_code[x > 0.1] <- " "
  signif_code[x <= 0.1] <- "."
  signif_code[x <= 0.05] <- "*"
  signif_code[x <= 0.01] <- "**"
  signif_code[x <= 0.001] <- "***"
  return(signif_code)
}
