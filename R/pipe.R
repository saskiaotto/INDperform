#' Pipe operator
#'
#' INDperform makes heavy use of the tidy data principles.
#' Therefore it is advisable to import the pipe operator \code{\%>\%}
#' from magrittr.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
NULL

if (getRversion() >= "4.0")  utils::globalVariables(c("."))
