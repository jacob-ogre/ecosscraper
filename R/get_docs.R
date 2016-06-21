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
get_document <- function(link, subd, pause = TRUE) {
  dest <- make_pdf_dest(link, subd)
  if(!file.exists(dest)) {
    if(pause == TRUE) Sys.sleep(runif(1, 0.5, 3))
    res <- try(httr::GET(link, httr::write_disk(dest)))
    if(class(res) == "try-error") {
      # Try once more because the error may be a temp connection issue
      res <- try(httr::GET(link, httr::write_disk(dest)))
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
make_pdf_dest <- function(link, subd = "") {
  data <- unlist(stringr::str_split(link, "/"))
  outf <- ifelse(grepl(data[length(data)], pattern = "pdf$"),
                 data[length(data)],
                 paste0(data[length(data)], ".pdf"))
  dest <- options()$base_dir
  if(subd != "") {
    dest <- paste(dest, subd, sep = "/")
  }
  if(!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }
  dest <- paste(dest, outf, sep = "/")
  return(dest)
}



