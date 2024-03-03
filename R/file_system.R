#' Check if Directories are Empty
#'
#' This function checks whether one or more directories are empty.
#' An empty directory is one that contains no files or subdirectories.
#' If the directory does not exist, it is considered as empty.
#'
#' @param paths A character vector containing one or more file paths.
#' Each path is checked to determine if the corresponding directory is empty.
#'
#' @return A logical vector where each element corresponds to a directory
#' specified in `paths`. `TRUE` indicates that the directory is empty,
#' and `FALSE` indicates that it is not.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create two temporary directories one of which is empty
#' library(fs)
#' dir1 <- tempfile()
#' dir2 <- tempfile()
#' dir_create(c(dir1, dir2))
#' dir_create(file.path(dir1, "subdir"))
#'
#' # Check if the directories are empty (should return FALSE, TRUE)
#' is_dir_empty(c(dir1, dir2))
#'
#' # Clean up
#' dir_delete(c(dir1, dir2))
#' }
is_dir_empty <- function(paths) {
  .is_dir_empty <- function(path) {
    if (dir.exists(path)) {
      files <- list.files(path)
      return(length(files) == 0)
    }
    return(TRUE)
  }
  sapply(paths, .is_dir_empty)
}
