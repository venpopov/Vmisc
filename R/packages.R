#' Load and/or install packages with specific versions
#'
#' pkg_vload() attempts to load a package and, if it is not available, installs
#' it. It can also install a specific version of a package. If the package is
#' already installed, it will check if the version is the same as the one
#' specified in the call. If the version is different, it will attempt to unload
#' the package and install the specified version in a separate library, allowing
#' the user to have multiple versions of the same package installed at the same
#' time.
#'
#' @param ... One or more calls to the package name with version (if desired).
#'   The calls should be of the form `pkg('version')` where `pkg` is the package
#'   name and `version` is the version number. If the version is not specified,
#'   the function will check for the default version of the package.
#' @param reload Logical. If `TRUE`, the function will attempt to unload the
#'   package and load it again, regardless of whether the version is the same as
#'   the one specified in the call. Default is `FALSE`. If the package is
#'   already loaded, it will be reloaded even if reload is `FALSE`, if the
#'   specified version is different from the one currently loaded.
#' @param path A character vector of paths to search for the package. Default is
#'   the default library paths.
#' @param repos A character vector of repository URLs to use for installing the
#'   package. Default is the value of `getOption("repos")`.
#' @param install_args A list of additional arguments to be passed to
#'   `install.packages()` or `remotes::install_version()`. Default is `NULL`.
#' @return This function does not return a value. Instead, it will stop the
#'  execution and display a message if the requirements are not met.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the 'brms' package and install version 2.0.0 if it is not available
#' pkg_vload(brms("2.0.0"))
#'
#' # Load multiple packages and install specific versions if they are not available
#' pkg_vload(brms("2.0.0"), utils)
#' }
pkg_vload <- function(..., reload = FALSE, path = .libPaths(), repos = getOption("repos"), install_args = NULL) {
  pkgs <- pkg_vavailable(..., path = path)
  if (any(!is.na(pkgs$pkg_version))) {
    require_pkg("remotes",
                message_prefix = "Installing specific versions of packages" %+%
                " requires you to first install:")
  }


  npkgs <- length(pkgs$pkg_name)
  for (i in 1:npkgs) {
    if (pkgs$pkg_folder[i] == pkgs$pkg_name[i]) {
      install_path <- path
    } else {
      install_path <- pkgs$path[i]
    }
    if (reload | !pkgs$available[i] | pkgs$pkg_version[i] != packageVersion(pkgs$pkg_name[i])) {
      tryCatch(
        {
          devtools::unload(pkgs$pkg_name[i])
        },
        error = function(e) {
          invisible(NULL)
        },
        warning = function(w) {
          invisible(NULL)
        }
      )
    }

    if (pkgs$available[i]) {
      require(pkgs$pkg_name[i], lib.loc = install_path, character.only = TRUE)
    } else {
      tryCatch(
        {
          xfun::dir_create(install_path)
          if (is.na(pkgs$pkg_version[i])) {
            args <- c(list(pkgs = pkgs$pkg_name[i],
                           lib = install_path,
                           dependencies = TRUE,
                           repos = repos),
                      install_args)
            do.call('install.packages', args)
          } else {
            args <- c(list(package = pkgs$pkg_name[i],
                           lib = install_path,
                           dependencies = TRUE,
                           repos = repos,
                           version = pkgs$pkg_version[i]),
                      install_args)
            do.call(remotes::install_version, args)
          }
          require(pkgs$pkg_name[i], lib.loc = install_path, character.only = TRUE)
        },
        error = function(e) {
          warning2(
            "\nPackage ", pkgs$pkg_name[i], " could not be installed. The attempt",
            " returned the following error: \n", e
          )
          if (is_dir_empty(install_path)) {
            unlink(install_path, recursive = TRUE, force = TRUE)
          }
        }
      )
    }
  }
  return(invisible(NULL))
}


#' Parse package name and version from a pkg('verions') call
#'
#' @param ... a number of calls to objects of type pkg('version') where pkg is
#'   the package name and version is the version number
#'
#' @return A list with two elements: names and versions. The names are the package
#'  names and the versions are of class 'package_version'. If the version is not
#'  specified, the version will be NA.
#' @export
#'
#' @examples
#' parse_pkg_version(brms("2.20.4"), bmm("0.4-0"), utils)
parse_pkg_version <- function(...) {
  x <- arg2string(...)
  names <- gsub("\\(.*", "", x)
  versions_str <- stringr::str_extract(x, "\\d+(\\.|\\-)\\d+(\\.|\\-)\\d+")
  versions <- package_version(versions_str, strict = FALSE)
  nchar_ver <- ifelse(is.na(versions_str), 0, nchar(versions_str)+4)
  cant_parse <- (nchar(names) + nchar_ver) != nchar(x)
  if (any(cant_parse)) {
    stop2("Invalid version format: ", paste0((x[cant_parse]), collapse=", "))
  }
  nlist(names, versions)
}

is.named <- function(x) {
  !is.null(names(x))
}


#' Check Required Packages and Their Versions
#'
#' This function checks if the required R packages are available and if their
#' versions meet the specified minimum requirements. It will stop the execution
#' and display a message if any required package is missing or does not meet
#' the version requirement.
#'
#' @param ... Variable arguments representing required package names and,
#' optionally, their minimum versions. The versions should be specified
#' immediately after the package names, in the format `packageName(version)`.
#' @param message_prefix A character string to be displayed before the message
#' if the requirements are not met.
#'
#' @return This function does not return a value. Instead, it will stop
#' the execution and display a message if the requirements are not met.
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if 'dplyr' and 'ggplot2' are installed (any versions):
#' require_pkg(dplyr, ggplot2)
#'
#' # Check if 'dplyr' (version 1.0.0 or higher) and 'ggplot2' (version 8.3.0 or higher) are installed:
#' require_pkg(dplyr('1.0.0'), ggplot2('8.3.0'))
#' }
require_pkg <- function(..., message_prefix = "Please install the following packages:") {
  pkgs <- pkg_vavailable(..., exact = FALSE)
  available <- pkgs$available
  min_available <-
  required_version <- pkgs$pkg_version_specified
  m <- c()
  for (i in 1:length(available)) {
    if (!available[i] || isTRUE(required_version[i] > packageVersion(pkgs$pkg_name[i]))) {
      if (is.na(required_version[i])) {
        m <- c(m, paste0("   ", pkgs$pkg_name[i], "\n"))
      } else {
        m <- c(m, paste0("   ", pkgs$pkg_name[i], " (version ", required_version[i], " or higher)\n"))
      }
    }
  }
  if (length(m) > 0) {
    stop2(message_prefix, "\n", collapse(m))
  }
}

#' Check if a specific package version is available in the library
#'
#' [pkg_vavailable()] is an alternative to [xfun::pkg_available()] that checks
#' for a specific version of the package rather than a minimal version. If the
#' version is not specified, the function will check for the default version of
#' the package.
#'
#' @param ... One or more calls to the package name with version (if desired)
#'   specified in parantheses. E.g. brms("2.14.4") or brms or "brms"
#'
#' @param path A character vector of paths to search for the package. Default is
#'   the default library paths.
#' @param exact Logical. If `TRUE`, the function will only return `TRUE` if the
#'  exact version is available. If `FALSE`, the function will return `TRUE` if
#'  the version is available or if a higher version is available. Default is `TRUE`.
#' @return a named list with the following elements:
#'  - available: A logical vector indicating whether the package is available
#'  - pkg_name: The name of the package
#'  - pkg_version: The version of the package in the library
#'  - pkg_version_specified: The version of the package specified in the call to pkg('version')
#'  - pkg_folder: The folder name of the package in the library
#' @details To check for a specific version, the function assumes that this
#'   version was installed using pkg_load(pkg(version)), which has
#'   created a folder named "pkg-version" in the library.
#' @export
#' @examples
#' \dontrun{
#' pkg_vavailable(utils)
#' pkg_vavailable(xfun("0.1.0"))
#' pkg_vavailable(utils, brms("2.14.4"), xfun("0.1.0"))
#'
#' # compare with xfun::pkg_available()
#' xfun::pkg_available("xfun", "0.1.0") # returns TRUE
#' }
pkg_vavailable <- function(..., path = .libPaths(), exact = TRUE) {
  pkgs <- parse_pkg_version(...)
  names <- pkgs[["names"]]
  versions <- pkgs[["versions"]]
  default_versions <- sapply(seq_along(names), function(i) {
    tryCatch(
      {
        packageVersion(names[i], lib.loc = path)
      },
      error = function(e) {
        package_version(NA, strict = FALSE)
      }
    )
  })
  class(default_versions) <- c("package_version", "numeric_version")
  is_default <- is.na(versions) | is.na(default_versions) | default_versions == versions
  if (!exact) {
    is_default <- is.na(versions) | is.na(default_versions) | versions <= default_versions
  }
  affix <- sapply(seq_along(versions), function(x) ifelse(is_default[x], "", paste0("-", versions[x])))
  pkg_folder <- paste0(names, affix)
  paths <- c()
  for (pkg in pkg_folder) {
    where <- path[dir.exists(file.path(path, pkg))]
    if (length(where) == 0) {
      where <- path[1]
    }
    paths <- c(paths, file.path(where, pkg))
  }
  available <- pkg_folder %in% available_packages(path)

  versions[is_default] <- default_versions[is_default]
  out <- list(available = available,
              pkg_name = names,
              pkg_version = versions,
              pkg_version_specified = pkgs[["versions"]],
              pkg_folder = pkg_folder,
              path = paths)
  out
}

#' A character vector of available packages in the library
#'
#' @param path A character vector of paths to search for the package. Default is
#'   the default library paths.
#'
#' @return A character vector of available package names in the library. If any
#'  package versions were installed via pkg_vload(), the version will be shown as
#'  "pkg-version"
#' @export
available_packages <- function(path = .libPaths()) {
  libs_full <- dir(path, full.names = TRUE)
  libs <- basename(libs_full)
  libs[!is_dir_empty(libs_full) & !(libs %in% c(".", ".."))]
}


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
#' packageOptions("utils")
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
    code <- deparse1(get(f, envir = nmsp), collapse = "\n")
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
