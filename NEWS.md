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
