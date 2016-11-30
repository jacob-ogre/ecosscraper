# Make sure the data is loaded; required for several functions.
check_load <- function() {
  if(!exists("TECP_table")) {
    data("TECP_table")
  }
}

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
                    "^/ecp$")
  patterns <- c(base_patt, patterns)
  filt <- filter(df, !is.na(href))
  for(i in patterns) {
    filt <- filter(filt, !grepl(href, pattern = i))
  }
  filt <- distinct(filt, href, .keep_all = TRUE)
  return(filt)
}

#' Get a data.frame of links and their titles from a web page
#' 
#' @export
get_link_df <- function(pg) {
  a_nodes <- html_nodes(pg, "a")
  pg_links <- html_attr(a_nodes, "href")
  link_txt <- html_text(a_nodes)
  link_tbl <- data.frame(Doc_Link = pg_links, 
                         Title = link_txt,
                         stringsAsFactors = FALSE)
  return(link_tbl)
} 

#' Set the URL of the TESS table
#' 
#' @param url The new URL from which the base TESS data is scraped
#' @export
set_TE_list_opt <- function(url) {
  options("TE_list" = url)
}
