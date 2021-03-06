#' ecosscraper: Get data and documents from ECOS
#' 
#' @section About:
#' 
#' The U.S. Fish and Wildlife Service provides documents (and some limited data
#' about the documents) on ECOS, \url{http://ecos.fws.gov}. But there is no API
#' for the documents, so we have to scrape the contents.
#' 
#' @importFrom digest digest
#' @importFrom dplyr bind_rows distinct filter_ filter left_join select %>%
#' @importFrom httr GET http_error write_disk content
#' @importFrom magrittr %>%
#' @importFrom parallel mclapply
#' @importFrom pdfdown download_pdf
#' @importFrom rvest html_attr html_node html_nodes html_table html_text
#' @importFrom stringr str_trim str_split str_extract
#' @importFrom tibble data_frame as_data_frame
#' @importFrom webshot install_phantomjs
#' @importFrom xml2 read_html
#' @docType package
#' @name ecosscraper
NULL