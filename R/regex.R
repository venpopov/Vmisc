#' Extract a substring defined by a prefix, and a matched opening and closing
#' character
#'
#' Used to, for example, extract a function call from code, ignoring paranthesis
#' within function arguments
#'
#' @param string A character vector
#' @param prefix A string to match at the beginning of the substring
#' @param opening A character to match as the opening character
#' @param closing A character to match as the closing character
#'
#' @return A character vector with the extracted substring
#' @export
#'
#' @details There will be a match only if the function is not used as text in
#'   quotes immediately before the function call. For example, in the string
#'   "something myFunction(x = 1, y = mean(x)) otherFunction()", the function
#'   call to `myFunction` will be matched, but the function call in the string
#'   "var = \"myFunction(x = 1, y = mean(x))\"" or print("myFunction(x = 1, y =
#'   mean(x))") will not. This ensures that we do not retrieve examples or
#'   instructions about usage
#'
#' @examples
#'
#' x <- "something myFunction(x = 1, y = mean(x)) otherFunction()"
#' str_extract_nested_balanced(x, "myFunction", "(", ")")
str_extract_nested_balanced <- function(string, prefix, opening="(", closing=")") {
  expr <- "(?<!\")" %+%
    prefix %+%
    "(\\" %+%
    opening %+%
    "(?!\\\\)(?>[^" %+%
    opening %+%
    closing %+%
    "]|(?1))*\\" %+%
    closing %+%
    ")"
  reg <- gregexpr(expr, string, perl = TRUE)
  out <- lapply(1:length(string), function(x) {
    substring(string[x], reg[[x]], reg[[x]] + attr(reg[[x]], "match.length") - 1)
  })
  attributes(out) <- attributes(string)
  class(out) <- c('attr_list', 'list')
  out
}

#' @export
print.attr_list <- function(x, ...) {
  x <- strip_attributes(x, protect = c("names","class"))
  class(x) <- "list"
  print(x, ...)
}

#' @export
`[.attr_list` <- function(x, i, ...) {
  attrib <- attributes(x)
  out <- x
  class(out) <- "list"
  out <- out[i]
  for (attr in names(attrib)) {
    if (length(attrib[[attr]]) == length(x)) {
      attr(out, attr) <- attrib[[attr]][i]
    }
    else {
      attr(out, attr) <- attrib[[attr]]
    }
  }
  out
}
