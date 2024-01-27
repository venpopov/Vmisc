#' Get attribute of an object
#'
#' @param object an object whose attributes are to be accessed.
#' @param a an expression specifying the attribute to be accessed.
#'
#' @return For the extractor, the value of the attribute matched, or NULL if no exact match is found and no or more than one partial match is found.
#' @export
#'
#' @examples
#' x <- 1
#' attr(x, "name") <- "ven"
#' x%a%name
`%a%` <- function(object,a) {
  attr(object, as.character(substitute(a)))
}
