#' GET a document from ECOS.
#'
#' Simple function to download a file from ECOS.
#'
#' @param link A URL from ECOS to download a document
#' @param subd Subdirectory to which the document will be downloaded
#' @param pause Whether to pause for 0.5-3 seconds during scraping
#' @return A data.frame with destination and success 
#' @export
#' @examples
#' # one or more lines to demo the function
get_document <- function(url, subd, pause = TRUE) {
  dest <- make_pdf_dest(url, subd)
  if(!file.exists(dest) | !is_pdf(dest)) {
    if(pause == TRUE) Sys.sleep(runif(1, 0.5, 3))
    if(httr::http_error(url))
    res <- try(httr::GET(URLencode(url), 
                         httr::write_disk(dest, overwrite = TRUE)))
    if(class(res) == "try-error") {
      # Try once more because the error may be a temp connection issue
      res <- try(httr::GET(URLencode(url), 
                           httr::write_disk(dest, overwrite = TRUE)))
      if(class(res) == "try-error") {
        print(paste("GET Error:", res))
        return(data.frame(dest = dest, 
                          success = "Failed", 
                          stringsAsFactors = FALSE))
      }
    }
    return(data.frame(dest = dest, 
                      success = "Success", 
                      stringsAsFactors = FALSE))
  } else {
    return(data.frame(dest = dest, 
                      success = "Pre-exist", 
                      stringsAsFactors = FALSE))
  }
}

#' Construct a path for a download PDF
#'
#' @param link A URL from ECOS to download a PDF document
#' @param subd The subdirectory in which the download will be written
#' @return The file path where the download will be written
#' @export
make_pdf_dest <- function(link, subd = "") {
  fname <- basename(link)
  outf <- ifelse(grepl(fname, pattern = "pdf$"), fname, paste0(fname, ".pdf"))
  outf <- gsub(outf, pattern = " ", "_")
  if(!dir.exists(subd)) dir.create(subd, recursive = TRUE)
  dest <- file.path(subd, outf)
  return(dest)
}

#' Test if a file is a pdf
#' 
#' @param f Path to a file to test
#' @return TRUE if pdftools::pdf_info thinks it's a PDF, else FALSE
#' @export
is_pdf <- function(f) {
  res <- try(pdftools::pdf_info(f))
  if(class(res) != "try-error") return(TRUE)
  return(FALSE)
}


