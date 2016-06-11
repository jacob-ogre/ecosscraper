#' Default base_dir for ecosscraper downloads.
#' 
#' Other options may be defined and set using \link{set_ecosscraper_options}.
#' @import settings
#' @export

#' Set the base directory for ecosscraper downloads.
#'
#' Creates the directory, recursively, if it doesn't exist.
#'
#' @param path Full path to be used for the base directory for downloads.
#' @return Nothing
#' @export
#' @examples
#' set_base_dir("~/Downloads/testing")
set_base_dir <- function(path) {
  print(c(base_dir, path))
  if(!dir.exists(path)) {
    dir.create(path)
    assign("base_dir", path, envir = .GlobalEnv)
    cat(paste0("Created and set base directory: ", path, "\n"))
  } else if(base_dir != path) {
    assign("base_dir", path, envir = .GlobalEnv)
    cat(paste0("Set base directory to: ", path, "\n"))
  } else {
    cat(paste0("Kept base directory: ", path, "\n"))
  }
}
