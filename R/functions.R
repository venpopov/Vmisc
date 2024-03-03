#' Convert Function Arguments to Strings
#'
#' This function takes any number of R language objects and converts their names into strings.
#' This is particularly useful for programming where variable names or symbols need to be used
#' as strings without evaluating them. It leverages `rlang`'s tidy evaluation framework.
#'
#' @param ... Arbitrary arguments representing R language objects or symbols.
#'
#' @return A character vector where each element is the string representation of the corresponding
#' argument passed to the function. The order of the strings in the output matches the order of
#' the arguments.
#'
#' @export
#'
#' @examples
#' # returns the arguments as strings even though functions bmm() and brms() are not defined
#' arg2string(bmm('0.4.0'), brms('2.20.4'))
arg2string <- function(...) {
  args <- rlang::enquos(...)
  labels <- as.character(sapply(args, rlang::as_label))
  # if the argument was already a string, it will be returned as is
  labels <- sapply(labels, function(x)
    tryCatch({
      eval(parse(text = x))
    },
    error = function(e) {
      x
      }
    )
  )
  names(labels) <- NULL
  labels
}
