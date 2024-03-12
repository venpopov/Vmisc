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



#' Evaluate a function and return the result on exit in the calling environment
#'
#' return_on_exit() can be called from within a parent function to set a return
#' value when the parent function exits for whatever reason. This is useful when
#' you want to specify a conditional return value even if a function exits
#' abruptly.
#'
#' @param fun Function to evaluate upon exit and return the result from the
#'   calling function
#' @param ... Arguments to pass to the function
#' @param env Environment to evaluate the function in and to return from. The
#'   function will have access to the variables in this environment and their
#'   state at the time of the exit (not the time of the function call). If you
#'   want to pass variable values at the time of the function call, use `...`
#'
#' @return The result of the function call
#' @export
#'
#' @examples
#' \dontrun{
#' # function to evaluate on exit
#' f <- function(y, ...) {
#'   dots <- list(...)
#'   if ("x" %in% names(dots)) {
#'     x <- dots$x
#'   }
#'   x + y
#' }
#'
#' # calling function
#' g <- function(...) {
#'  x <- 1    # current value of x; will not be used if it changes before g exists
#'  y <- 10   # value of y to pass to f, will be used as is
#'
#'  # setup conditional return
#'  return_on_exit(f, y, ...)
#'
#'  # do some work
#'  for (i in 1:1000000) {
#'    x <- i
#'    if (i == 100) stop("Error, but I will return something!")
#'  }
#'
#'
#'  # this will not be executed
#'  cat("This will not be printed")
#'  return("This will not be returned")
#' }
#'
#' # calling g() will return 110
#' g()
#'
#' # calling g() with x as an argument will return 30
#' # because x is passed to f() via the dots argument
#' g(x = 20)
#' }
#'
return_on_exit <- function(fun, ..., env = parent.frame()) {
  environment(fun) <- env
  dots <- list(...)

  withr::defer(
    rlang::return_from(env, do.call(fun, dots)),
    envir = env
  )
}
