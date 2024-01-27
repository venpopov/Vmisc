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



#' Shortcut for paste0
#'
#' @param a an R object to be converted to a character vector.
#' @param b an R object to be converted to a character vector.
#'
#' @return String concatenation of a and b
#' @export
#'
#' @examples
#' "name" %+% 1:10
`%+%` <- function(a, b) {paste0(a, b)}
