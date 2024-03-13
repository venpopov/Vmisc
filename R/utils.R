#' Concatenate and collapse to a single string
#'
#' Wrapper around `paste` with `collapse` argument set to `""`. This results in
#' a single string.
#' @param ... one or more R objects, to be converted to character vectors and
#'   concatenated.
#' @param sep a character string to separate the terms.
#'
#' @return A single string.
#' @export
#'
#' @examples
#' x <- 1:10
#' res <- collapse(x)
#' identical(res, '12345678910')
collapse <- function(..., sep = "") {
  paste(..., sep = sep, collapse = "")
}



#' Remove all attributes of an object except those specified as protected
#'
#' @param x an R object
#' @param protect a character vector of attribute names to keep. Default is
#'   `c("names", "row.names", "class")`, which are the attributes that a
#'   data.frame has by default.
#'
#' @return An R object with all attributes removed except those specified in
#'   `protect`.
#' @export
#' @examples
#' x <- data.frame(a = 1:10, b = 11:20)
#' attr(x, "remove_me") <- "I want to be removed"
#' attributes(x)
#' x <- strip_attributes(x, protect = c("names", "row.names", "class"))
#' attributes(x)
strip_attributes <- function(x, protect = c("names", "row.names", "class")) {
  to_remove <- names(attributes(x))
  to_remove <- to_remove[!to_remove %in% protect]
  attributes(x)[to_remove] <- NULL
  return(x)
}



#' Wrappers around `stop` and `warning` that do not print the call stack
#'
#' @param ... zero or more objects which can be coerced to character (and which
#'   are pasted together with no separator) or a single condition object
#'
#' @return Stops execution and prints the error message
#' @export
#'
stop2 <- function(...) {
  stop(paste0(...), call. = FALSE)
}

#' @rdname stop2
warning2 <- function(...) {
  warning(paste0(...), call. = FALSE)
}

#' Wrapper around stopifnot allowing for a custom error message
#'
#' @param ... zero or more expressions to be evaluated
#' @param msg a custom error message to be printed if the expressions are not
#'  all true
#' @export
stopifnot2 <- function(..., msg = NULL) {
  if (is.null(msg)) {
    eval.parent(substitute(stopifnot(exprs = ...)))
  }
  if (!all(...)) {
    stop2(msg)
  }
}

