download_species_page <- function(url, file, type = c("plain", "dynamic")) {
  url <- URLencode(url)
  if(type == "plain") {
    if(class(try(http_error(url), silent = TRUE)) != "try-error") {
      pg <- httr::GET(url)
      if(pg$status == 200) {
        html <- content(pg, as = "text")
        writeLines(html, con = file)
      }
    }
  }
}
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
#' @param file File to which the document will be downloaded
#' @param pause Whether to pause for 0.5-3 seconds during scraping
#' @return A data_frame with destination and success 
#' @seealso \link[pdfdown]{download_pdf}
#' @export
#' @examples
#' \dontrun{
#'   res <- download_document("http://ecos.fws.gov/docs/five_year_review/doc3847.pdf", 
#'                            "~/Downloads/doc3847.pdf")
#' }
download_document <- function(url, file, pause = TRUE) {
  pdfdown::download_pdf(url = url, subd = file, pause = pause)
}

#' Download an image file
#' 
#' @details Just a wrapper around \link[httr]{GET} with several checks to make
#' downloads more likely / robust. 
#'
#' @param url The URL of the image to download
#' @param file The file to which the image will be written
#' @export
download_pic <- function(url, file) {
  subd <- dirname(file)
  if(!dir.exists(subd)) dir.create(subd, recursive = TRUE)
  if(class(try(http_error(url), silent = TRUE)) != "try-error") {
    res <- try(GET(url, write_disk(file, overwrite = TRUE)))
    if(class(res) == "try-error") { # Try once more
      res <- try(GET(url, write_disk(file, overwrite = TRUE)))
      if(class(res) == "try-error" | res$status != 200) {
        return(data_frame(url = url,
                          dest = NA,
                          success = "Failed",
                          imgCheck = NA))
      }
    }
    return(data_frame(url = url,
                      dest = file,
                      success = "Success",
                      imgCheck = is_img(file)))
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
