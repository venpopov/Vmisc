#' View the current or default global options for a package
#'
#' [packageOptions()] scrapes the source code of a package to find all calls to
#' `getOption()` and returns a tidied list of the current values of the options.
#' Initial running time might be slow if a package contains a large amount of
#' code. Repeated calls to the function will be significantly faster.
#'
#' @param pkg Name of the package to extract options from
#' @param own_only Logical. If `TRUE`, only options defined by the package
#'   itself will be returned. These are defined as options whose name starts
#'   with the package name, followed by a dot. E.g.
#'   `getOption("knitr.device.fallback")`. If `FALSE`, all options will be
#'   returned. Default is `FALSE`.
#' @param max_length Integer. Controls the maximum length of individual the
#'   default values to be printed. You will rarely need to change this. However,
#'   if your results include strange output with very long strings defining
#'   default values via internal functions, you can decrease this value to
#'   suppress them. Default is 50.
#' @param show_defaults Logical. If `TRUE`, the default values of the options
#'  will be printed as well. Default is `FALSE`.
#'
#' @return A named list of the current values of the options. This list can be
#'  saved to a variable and used with option() to restore the options to these
#'  values at a later time should you change them.
#' @export
#' @import utils
#' @examples
#' packageOptions('utils')
packageOptions <- function(pkg, own_only = FALSE, max_length = 50, show_defaults = FALSE) {
  opts <- extract_pkg_fun_calls(pkg, "getOption")
  # assuming options without defaults are not user-facing but only used internally
  if (own_only) {
    opts <- opts[stringr::str_detect(opts, paste0("getOption\\(\"", pkg, "\\."))]
  }
  # opts <- gsub(" ", "", opts)

  if (length(opts) == 0) {
    message(paste0("No options found for package ", pkg))
    return(invisible(NULL))
  }

  split_opts <- parse(text = opts)
  nargs <- sapply(split_opts, function(x) length(x)) - 1
  opt_names <- sapply(split_opts, function(x) x[[2]])
  def_values <- lapply(1:length(split_opts), function(x) {
    tryCatch(split_opts[[x]][[3:(nargs[x] + 1)]], error = function(e) {
      return(NULL)
    })
  })

  # remove options whose default value is very long, like an internal function logic
  opt_list_defaults <- lapply(def_values, function(x) enquote(x))
  names(opt_list_defaults) <- opt_names
  # don't sort because it causes problems when there are duplicate names
  # opt_list_defaults <- opt_list_defaults[sort(names(opt_list_defaults))]

  # TODO: would need to check where to find options specified as functions from
  # imports of the package. E.g. is_R_CMD_check() shows up in the knitr options,
  # but is not defined in the knitr package. It is defined in the xfun package
  # opt_list_defaults <- lapply(opt_list_defaults, function(x) deparse1(eval(x)))

  opt_list_defaults <- lapply(opt_list_defaults, function(x) eval(x))


  opt_list_defaults <- unique_list(opt_list_defaults)
  # opt_list_current <- lapply(names(opt_list_defaults), function(x) getOption(x, default = eval(parse(text = opt_list_defaults[[x]]))))
  opt_list_current <- lapply(names(opt_list_defaults), function(x) getOption(x, default = opt_list_defaults[[x]]))
  names(opt_list_current) <- names(opt_list_defaults)

  opt_names <- opt_names[nchar(def_values) < max_length]
  opt_list_current <- opt_list_current[nchar(opt_list_current) < max_length]

  class(opt_list_current) <- "packageOptions"
  attr(opt_list_current, "pkg") <- pkg
  attr(opt_list_current, "defaults") <- opt_list_defaults[nchar(opt_list_current) < max_length]
  attr(opt_list_current, "show_defaults") <- show_defaults
  opt_list_current
}

#' Extract all calls to a function from package code
#'
#' Given a function name and a package, this function will extract all calls to
#' the said function from the package. The function can be any function name,
#' either imported or defined in the package.
#'
#' @param pkg Name of the package from which to extract function calls
#' @param fun Name of the function to extract calls to
#'
#' @return A character vector with the extracted function calls. Each value is
#'   of the form "fun(arg1=value1, arg2=value2, ...)"
#' @export
#'
#' @examples
#' extract_pkg_fun_calls("utils", "getOption")
extract_pkg_fun_calls <- function(pkg, fun) {
  opts <- list()
  nmsp <- asNamespace(pkg)
  funs <- utils::lsf.str(nmsp)
  fcode <- c()
  for (f in funs) {
    code <- deparse1(get(f, envir = nmsp),collapse = "\n")
    fcode <- c(fcode, code)
  }
  attr(fcode, "source") <- funs
  opts <- str_extract_nested_balanced(fcode, fun)
  opts <- opts[opts != ""]
  sources <- rep(attr(opts, "source"), sapply(opts, length))
  opts <- unlist(opts)
  opts <- stringr::str_replace_all(opts, " +", " ")
  dups <- duplicated(opts)
  opts <- opts[!dups]
  attr(opts, "source") <- sources[!dups]
  opts
}

#' @export
print.packageOptions <- function(x, ...) {
  cat("Package", attr(x, "pkg"), "current options:\n\n")
  max_name_length <- max(nchar(names(x)))
  for (i in 1:length(x)) {
    if (length(x[[i]]) <= 1) {
      cat(names(x[i]), strrep(" ", max_name_length - nchar(names(x)[i])), " : ", deparse1(x[[i]]), "\n")
    } else {
      cat(names(x[i]), strrep(" ", max_name_length - nchar(names(x)[i])), " : ", deparse1(x[[i]]), "\n")
    }
  }

  if (attr(x, "show_defaults")) {
    cat("\nPackage", attr(x, "pkg"), "default options:\n\n")
    for (i in 1:length(x)) {
      if (length(x[[i]]) <= 1) {
        cat(names(x[i]), strrep(" ", max_name_length - nchar(names(x)[i])), " : ", deparse1(attr(x, "defaults")[[i]]), "\n")
      } else {
        cat(names(x[i]), strrep(" ", max_name_length - nchar(names(x)[i])), " : ", deparse1(attr(x, "defaults")[[i]]), "\n")
      }
    }
  }
}



help2md <- function(packageName, methodName) {
  db <- tools::Rd_db(packageName)
  return(db[[paste0(methodName, ".Rd")]])
}




unique_list <- function(x) {
  out <- x
  names <- names(x)
  counts <- table(names)
  n_dupes <- counts[counts > 1]
  for (el in names(n_dupes)) {
    is_dupe <- duplicated(names(out))
    is_el <- names(out) == el
    values <- unlist(x[is_el])
    if (length(unique(values)) == 1) {
      out[is_dupe & is_el] <- NULL
    } else {
      cond <- is_el & out == "NULL"
      if (any(cond)) {
        out[is_el & out == "NULL"] <- NULL
        is_el <- names(out) == el
      }
      if (length(out[is_el]) > 1) {
        warning2(paste0("Duplicate values for ", el, ": ", paste0(unique(unlist(out[is_el])), collapse = ", ")))
      }
    }
  }
  out
}
