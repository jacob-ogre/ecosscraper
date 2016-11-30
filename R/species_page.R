#' Return the ECOS page for a given species as an \link[xml2]{xml2} objects
#'
#' @details There are several \code{try} statements because of the 
#' persnickettiness of #' ECOS. Most errors should caught by 
#' \link{get_species_url}.
#' 
#' @note Unlike most other functions in \link{ecosscraper}, \code{verbose} is
#' \code{FALSE} by default. This function is mostly a helper and other "primary"
#' functions are \code{verbose} by default.
#'
#' @param url The url for a species' ECOS profile
#' @param verbose Whether message that the page is being fetched [default = FALSE]
#' @return A page returned by \code{rvest::read_html}
#' @examples
#' \dontrun{
#'   page <- get_species_url("Abies guatemalensis") %>% get_species_page()
#' }
get_species_page <- function(url, verbose = FALSE) {
  url <- URLencode(url)
  if(!http_error(url)) {
    if(verbose) message(paste("Getting page", url))
    page <- try(read_html(url))
    if(class(page[1]) != "try-error") {
      if(is_species_profile(page)) {
        return(page)
      } else {
        warning(paste(url, "is not a link to a species profile"))
        return(NULL)
      }
    } else {
      warning(paste("Error reading", url))
      return(NULL)
    }
  } else {
    warning(paste("http_error for", url))
    return(NULL)
  }
}
