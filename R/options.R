#' Set the base directory for ecosscraper downloads.
#'
#' Creates the directory, recursively, if it doesn't exist.
#'
#' @param path Path to base directory for downloads from ECOS
#' @return Nothing
#' @export
#' @examples
#' set_base_dir(path = "~/Downloads/test5")
set_base_dir <- function(path) {
  if(!dir.exists(path)) {
    dir.create(path)
    cat(paste0("Created base_dir ", path, "\n"))
  }
  options(base_dir = path)
}
