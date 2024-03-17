#' Get attribute of an object
#'
#' An infix operator to get the attribute of an object (equivalent to attr(x,
#' "name", exact = TRUE))
#'
#' @inheritParams base::attr
#' @inherit base::attr return
#' @details These functions provide access to a single attribute of an object.
#'   The replacement form causes the named attribute to take the value specified
#'   (or create a new attribute with the value given).
#' @name attr
#' @usage x\%a\%which
#' @export
#' @examples
#' x <- 1
#' attr(x, "name") <- "John"
#' x%a%name
#' x%A%name
#'
#' x%a%name <- "Alice"
#' attr(x, "name")
#'
#' x <- 1:10
#' x%A%dim <- c(2,5)
#' x
`%a%` <- function(x, which) {
  attr(x, as.character(substitute(which)))
}

#' @rdname attr
#' @usage x\%a\%which <- value
#' @export
"%a%<-" <- function(x, which, value) {
  attr(x, as.character(substitute(which))) <- value
  x
}

#' @rdname attr
#' @usage x\%A\%which
#' @export
`%A%` <- `%a%`

#' @rdname attr
#' @usage x\%A\%which <- value
#' @export
"%A%<-" <- function(x, which, value) {
  attr(x, as.character(substitute(which))) <- value
  x
}

#' Concatenate strings python style
#'
#' Equivalent to python's string concatenation via + operator
#'
#' @param a an R object to be converted to a character vector.
#' @param b an R object to be converted to a character vector.
#'
#' @return String concatenation of a and b
#' @export
#'
#' @examples
#' # adding a string and a vector
#' "name" %+% 1:10
#'
#' # adding vector variables and strings
#' names <- c("John", "Sarah")
#' ages <- c(34,23)
#' res <- names %+% " is " %+% ages %+% " years old"
#' identical(res, c("John is 34 years old", "Sarah is 23 years old"))
`%+%` <- function(a, b) paste0(a, b)


#' @name op-null-default
#' @inherit rlang::`%||%`
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y  else x
}
