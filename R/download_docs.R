#' GET a document from ECOS.
#'
#' Simple function to download a file from ECOS
#' 
#' @details This is a very thin wrapper for \link[pdfdown]{download_pdf}. Keeping
#' the function here just in case removal would break something else.
#' 
#' As with \link[pdfdown]{download_pdf}, this function does an auto-
#' rename to convert the URL to the file name, and places it in the destination
#' directory, subd.
#'
#' @param url A URL from ECOS to download a document
#' @param subd Subdirectory to which the document will be downloaded
#' @param pause Whether to pause for 0.5-3 seconds during scraping
#' @return A data.frame with destination and success 
#' @seealso \link[pdfdown]{download_pdf}
#' @export
#' @examples
#' \dontrun{
#'   res <- download_document("http://ecos.fws.gov/docs/five_year_review/doc3847.pdf", "~/Downloads")
#' }
download_document <- function(url, subd, pause = TRUE) {
  pdfdown::download_pdf(url = url, subd = subd, pause = pause)
}

