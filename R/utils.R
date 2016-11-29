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

#' Set the URL of the TESS table
#' @export
set_TE_list_opt <- function() {
  options("TE_list" = "http://ecos.fws.gov/tess_public/reports/ad-hoc-species-report?kingdom=V&kingdom=I&kingdom=P&status=E&status=T&status=EmE&status=EmT&status=EXPE&status=EXPN&status=SAE&status=SAT&status=C&status=P&fcrithab=on&fstatus=on&fspecrule=on&finvpop=on&fgroup=on&fleadreg=on&fspcode=on&fmapstatus=on&header=Listed+Species")
}

# update_TECP_table <- function() {
#   TECP_table <- get_TECP_table()
#   devtools::use_data(TECP_table, overwrite = TRUE)
# }