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
#' @return A data_frame with destination and success 
#' @seealso \link[pdfdown]{download_pdf}
#' @export
#' @examples
#' \dontrun{
#'   res <- download_document("http://ecos.fws.gov/docs/five_year_review/doc3847.pdf", "~/Downloads")
#' }
download_document <- function(url, subd, pause = TRUE) {
  pdfdown::download_pdf(url = url, subd = subd, pause = pause)
}

#' Download an image file
#' 
#' @details Just a wrapper around \link[httr]{GET} with several checks to make
#' downloads more likely / robust. 
#'
#' @param url The URL of the image to download
#' @param subd The directory to which the image will be written
#' @export
download_pic <- function(url, subd, pause = TRUE) {
  if(!dir.exists(subd)) dir.create(subd, recursive = TRUE)
  dest <- file.path(subd, paste0(Sys.Date(), "_", basename(url)))
  if(class(try(http_error(url), silent = TRUE)) != "try-error") {
    res <- try(GET(url, write_disk(dest, overwrite = TRUE)))
    if(class(res) == "try-error") { # Try once more
      res <- try(GET(url, write_disk(dest, overwrite = TRUE)))
      if(class(res) == "try-error" | res$all_headers[[1]]$status != 200) {
        return(data_frame(url = url,
                          dest = NA,
                          success = "Failed",
                          imgCheck = NA))
      }
    }
    return(data_frame(url = url,
                      dest = dest,
                      success = "Success",
                      imgCheck = is_img(dest)))
  } else {
    return(data_frame(url = url,
                      dest = NA,
                      success = "Failed",
                      imgCheck = NA))
  }
}

is_img <- function(f) {
  if(grepl(f, pattern = "png$|PNG$|jpg$|JPG$|jpeg$|JPEG$")) return(TRUE)
  return(FALSE)
}
