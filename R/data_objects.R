#' Create Named List from Arguments
#'
#' This function creates a named list from its arguments. If the arguments are named,
#' those names are used in the resulting list. If some arguments are unnamed, the variable
#' names themselves are used as names in the list. This can be useful for creating lists
#' where the names are important for later indexing or manipulation, and ensures all
#' elements in the list have names.
#'
#' @param ... Arbitrary arguments to be included in the list. These can be named or unnamed.
#' Unnamed arguments will be named based on their variable names.
#'
#' @return A list where each element corresponds to an argument passed to the function.
#' Elements of the list are named based on either their original names or the names of
#' the variables passed as arguments.
#'
#' @export
#'
#' @examples
#' var1 <- 1
#' var2 <- 1:10
#' # This will return a list with names: c("a", "b", "var1", "var2")
#' nlist(a = 1, b = 2, var1, var2)

nlist <- function(...) {
  # adapted from brms
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE else nzchar(names(dots))
  if (all(has_name)) return(dots)
  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
}


#' Report Space Allocated for the Components of a List
#'
#' @param x A list object
#' @param units A string specifying the units to use for the size. Default is "auto".
#'
#' @return A list with the same names as the input list, where each element is the size
#' @export
#'
#' @examples
#' x <- list(a = 1:10, b = 1:100)
#' component_size(x)
component_size <- function(x, units = "auto") {
  stopifnot(is.list(x))
  sizes <- structure(lapply(x, utils::object.size),
                     class = "component_size",
                     units = units)
  sizes
}

#' @export
print.component_size <- function(x, units, ...) {
  if (missing(units)) {
    units = attr(x, "units")
  }
  for (i in seq_along(x)) {
    size <- utils::capture.output(print(x[[i]], units = units))
    cat(names(x)[i], ":", size, "\n")
  }
}
