#' Get the table of counties of occurrence from ECOS
#' 
#' @param url The URL of the counties-of-occurrence page for a species
#' @return A data_frame of counties of occurrence provided by ECOS
#' @export
#' @examples
#' \dontrun{
#'   url <- "https://ecos.fws.gov/ecp0/profile/countiesBySpecies?entityId=2"
#'   URARHO <- get_counties(url)
#' }
get_counties <- function(url) {
  url <- URLencode(url)
  if(!grepl(url, pattern = "countiesBySpecies")) {
    warning("Expected a URL to a species' counties list.")
    return(NULL)
  }
  con <- try(http_error(url), silent = TRUE)
  if(class(con) != "try-error") {
    pg <- try(xml2::read_html(url))
    if(class(pg)[1] != "try-error") {
      tab <- html_table(pg, fill = TRUE)
      return(tab[[1]])
    } else {
      warning("read_html error; check URL and try again later.")
      return(NULL)
    }
  } else {
    warning("http_error; check URL and try again later.")
    return(NULL)
  }
}

#' Get data linked to a section 10 conservation plan
#' 
#' @details This function returns two distinct items, a table of summary data
#' and a table of linked documents. Most other functions in the package should
#' return a single item, but rather than visiting the conservation plan page 
#' twice, we get both pieces of content at the same time.
#' 
#' @param url The URL of the summary page for a section 10 conservation plan
#' @return A list of two items \describe{
#'   \item{table}{A data_frame of summary data about the agreement}
#'   \item{pdfs}{A data_frame of links to agreement documents}
#' }
#' @export
#' @examples
#' \dontrun{
#'   url <- "https://ecos.fws.gov/conserv_plans/conservationPlan/plan?plan_id=1470"
#'   URARHO <- get_conservation_plan_table(url)
#' }
get_conservation_plan_data <- function(url) {
  url <- gsub(url, pattern = "conserv_plans", replacement = "ecp0")
  url <- URLencode(url)
  con <- try(http_error(url), silent = TRUE)
  if(class(con) != "try-error") {
    pg <- try(xml2::read_html(url))
    if(class(pg)[1] != "try-error") {
      tabs <- html_table(pg, fill = TRUE)
      tab <- bind_rows(tabs)
      names(tab) <- c("Variable", "Value")
      atags <- html_nodes(pg, "a")
      hrefs <- html_attr(atags, "href")
      hrefs <- ifelse(grepl(hrefs, pattern = "^http"),
                      hrefs,
                      paste0("http://ecos.fws.gov", hrefs))
      texts <- html_text(atags)
      links <- data_frame(hrefs = hrefs,
                          texts = texts)
      pdf_links <- dplyr::filter(links,
                                 grepl(links$hrefs, pattern = "pdf|PDF"))
      return(list(table = tab, pdfs = pdf_links))
    } else {
      warning("read_html error; check URL and try again later.")
      return(NULL)
    }
  } else {
    warning("http_error; check URL and try again later.")
    return(NULL)
  }
}

