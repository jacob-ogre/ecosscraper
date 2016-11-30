#' ecosscraper: Get data and documents from ECOS
#' 
#' @section About:
#' 
#' The U.S. Fish and Wildlife Service provides documents (and some limited data
#' about the documents) on ECOS, \url{http://ecos.fws.gov}. But there is no API
#' for the documents, so we have to scrape the contents.
#' 
#' @importFrom dplyr filter_ filter left_join select bind_rows
#' @importFrom httr http_error
#' @importFrom magrittr %>%
#' @importFrom parallel mclapply
#' @importFrom rvest html_nodes html_attr html_table html_text
#' @importFrom xml2 read_html
#' @docType package
#' @name ecosscraper
NULL