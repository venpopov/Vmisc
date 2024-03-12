# Vmisc 0.1.5 +

### New features
* add component_size() function to get the space allocated to a component in a list 
* return_on_exit() - Evaluate a function and return the result on exit in the calling environment
* add pkg_switch_default() function which switches the default version of a package to the specified one

# Vmisc 0.1.5

### New features
* add nlist() function to create a named list
* add is_dir_empty() function to check if a directory is empty
* add arg2string() to defuse a function's arguments into strings
* add pkg_vload() function which can load and/or install a specific version of multiple packages. This function takes calls to packages of the form pkg(version), e.g. dplyr('1.0.0').
* add parse_pkg_version() function that parses calls such as dplyr('1.0.0') into a list with package names and versions
* add require_pkg() function which checks if one or more packages are installed and if their versions are at least the specified one. If not, it gives an error message and stops the execution.
* add pkg_vavailable() function which is is an alternative to [xfun::pkg_available()] that checks for a specific version of the package rather than a minimal version. 
* add available_packages() function which returns a simple character vector of all installed packages, including specific multiple versions created by pkg_vload()

# Vmisc 0.1.0

### New features
* add infix operator %a% as a shortcut for attr()
* add infix operator %+% to use python's style string concatenation instead of paste0()
* add collapse function to collapse a vector into a single string
* add extract_pkg_fun_calls function to extract all occurrences a specific function call from a package's source code
* add packageOptions() function that retrieves all global options and their defaults that are used by a package
* add str_extract_nested_balanced() function which can extract a a pattern with matchin nested brackets or other symbols. Useful in parsing code when multiple functions are nested within each other.
* add strip_attributes() function which removes all attributes from an object, except those that are explicitly specified as protected. Useful for removing attributes from custom data.frame objects, list, etc, without destroying the object's structure.
* add stop2() and warning2() functions as wrapper around stop() and warning() that suppress the call stack
