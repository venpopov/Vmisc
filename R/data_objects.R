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
