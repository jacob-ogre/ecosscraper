#' GET a document from ECOS.
#'
#' Simple function to download a file from ECOS.
#'
#' @param url A URL from ECOS to download a document
#' @param subd Subdirectory to which the document will be downloaded
#' @param pause Whether to pause for 0.5-3 seconds during scraping
#' @return A data.frame with destination and success 
#' @export
#' @examples
#' # one or more lines to demo the function
get_document <- function(url, subd, pause = TRUE) {
  dest <- make_pdf_dest(url, subd)
  url <- URLencode(url)
  if(!file_check(dest)) {
    if(pause == TRUE) Sys.sleep(runif(1, 0.5, 3))
    if(!httr::http_error(url)) {
      res <- try(httr::GET(url, httr::write_disk(dest, overwrite = TRUE)))
      if(class(res) == "try-error") { # Try once more 
        res <- try(httr::GET(url, httr::write_disk(dest, overwrite = TRUE)))
        if(class(res) == "try-error") {
          return(data.frame(url = url
                            dest = dest, 
                            success = "Failed", 
                            pdfCheck = NA,
                            stringsAsFactors = FALSE))
        }
      }
      pdfCheck <- is_pdf(dest)
      return(data.frame(url = url,
                        dest = dest, 
                        success = "Success", 
                        pdfCheck = pdfCheck,
                        stringsAsFactors = FALSE))
    } else {
      return(data.frame(url = url,
                        dest = dest, 
                        success = "Failed", 
                        pdfCheck = NA,
                        stringsAsFactors = FALSE))
    }
  } else {
    pdfCheck <- is_pdf(dest)
    return(data.frame(url = url,
                      dest = dest, 
                      success = "Pre-exist", 
                      pdfCheck = pdfCheck,
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
  outf <- gsub(outf, pattern = " ", replacement = "_")
  if(!dir.exists(subd)) dir.create(subd, recursive = TRUE)
  dest <- file.path(subd, outf)
  return(dest)
}

#' Check if file exists and if so, if it is really a PDF
#' 
#' @param f File to check
#' @return TRUE if file exists and is PDF, else FALSE
#' @export
file_check <- function(f) {
  if(file.exists(f)) {
    if(is_pdf(f)) {
      return(TRUE)
    }
    return(FALSE)
  }
  return(FALSE)
}

#' Test if a file is a pdf
#' 
#' @param f Path to a file to test
#' @return TRUE if pdftools::pdf_info thinks it's a PDF, else FALSE
#' @export
is_pdf <- function(f) {
  if(file.exists(f)) {
    res <- suppressMessages(try(pdftools::pdf_info(f), silent = TRUE))
    if(class(res) != "try-error") return(TRUE)
    return(FALSE)
  }
  return(FALSE)
}


