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

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
