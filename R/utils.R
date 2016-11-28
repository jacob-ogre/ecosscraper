#' Remove useless links from ECOS-scraped webpage.
#'
#' Expects a data.frame with an href variable for filtering. The default 
#' patterns are based on an examination of unfiltered link tables from ECOS
#' species pages, but additional link patterns to be removed can be added using
#' the optional \code{patterns} parameter, with the patterns using regex 
#' expressions expected by \link{grepl}.
#'
#' @param df A data.frame with four columns, including href, link, and text
#' @param patterns A list of additional patterns to be filtered out
#' @return A filtered version of df
#' @export
remove_silly_links <- function(df, patterns = list()) {
  base_patt <- list("^#", "^javascript", "^http://www.fws.gov", 
                    "^http://www.usa.gov", "^http://www.doi.gov",
                    "^/ecp")
  patterns <- c(base_patt, patterns)
  filt <- dplyr::filter(df, !is.na(href))
  for(i in patterns) {
    filt <- dplyr::filter(filt, !grepl(href, pattern = i))
  }
  filt <- dplyr::distinct(filt)
  return(filt)
}

