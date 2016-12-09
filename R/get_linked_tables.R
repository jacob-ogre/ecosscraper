#' Get the table of counties of occurrence from ECOS
#' 
#' @param url The URL of the counties-of-occurrence page for a species
#' @param species The scientific name of the species
#' @return A data_frame of counties of occurrence provided by ECOS
#' @export
#' @examples
#' \dontrun{
#'   url <- "https://ecos.fws.gov/ecp0/profile/countiesBySpecies?entityId=2"
#'   URARHO <- get_counties(url)
#' }
get_counties <- function(url, species) {
  url <- URLencode(url)
  if(!grepl(url, pattern = "countiesBySpecies")) {
    warning("Expected a URL to a species' counties list.")
    return(NULL)
  }
  con <- try(http_error(url), silent = TRUE)
  if(class(con) != "try-error") {
    pg <- try(xml2::read_html(url))
    if(class(pg)[1] != "try-error") {
      tab <- html_table(pg, fill = TRUE)[[1]]
      tab$Scientific_Name <- rep(species, length(tab$State))
      return(tab)
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
#' @details The ECOS pages for conservation plans include covered species,
#' (\code{Listed_Species / Non-Listed_Species}), but those "lists" are just a
#' concatenation of common names, scientific names, where listed, etc. The
#' \code{species} parameter is used to tie outgoing links from a species' ECOS
#' page to the summary data. These can later be checked to make sure that all
#' covered species listed on the plan page are linked to the species' page.
#' 
#' @param url The URL of the summary page for a section 10 conservation plan
#' @param species The species from whose ECOS page the plan URL was gathered
#' @return A data_frame of summary data about the agreement or NULL
#' @export
#' @examples
#' \dontrun{
#'   url <- "https://ecos.fws.gov/conserv_plans/conservationPlan/plan?plan_id=1470"
#'   URARHO <- get_conservation_plan_data(url, "Ursus arctos horribilis")
#' }
get_conservation_plan_data <- function(url, species) {
  url <- gsub(url, pattern = "conserv_plans", replacement = "ecp0")
  url <- URLencode(url)
  con <- try(http_error(url), silent = TRUE)
  if(class(con) != "try-error") {
    pg <- try(xml2::read_html(url))
    if(class(pg)[1] != "try-error") {
      tabs <- html_table(pg, fill = TRUE)
      tab <- bind_rows(tabs)
      names(tab) <- c("Variable", "Value")
      rownames(tab) <- gsub(
                         gsub(tab$Variable,
                              pattern = " |/",
                              replacement = "_"),
                         pattern = "\\(|\\)",
                         replacement = "")
      tab <- as_data_frame(t(tab))
      tab <- tab[-1, ]
      tab$Plan_Name <- html_text(html_nodes(pg, "h3")[1])
      tab$Scientific_Name <- species
      return(tab)
    } else {
      warning("read_html error; check URL and try again later.")
      return(NULL)
    }
  } else {
    warning("http_error; check URL and try again later.")
    return(NULL)
  }
}
      
#' Get links to documents on a section 10 conservation plan page
#' 
#' @param url The URL of the summary page for a section 10 conservation plan
#' @param species The species from whose ECOS page the plan URL was gathered
#' @return A data_frame of links to agreement documents
#' @export
#' @examples
#' \dontrun{
#'   url <- "https://ecos.fws.gov/conserv_plans/conservationPlan/plan?plan_id=1470"
#'   conplan_docs <- get_conservation_plan_doc_links(url, 
#'                                                   "Ursus arctos horribilis")
#' }
get_conservation_plan_doc_links <- function(url, species) {
  url <- gsub(url, pattern = "conserv_plans", replacement = "ecp0")
  url <- URLencode(url)
  if(class(try(http_error(url), silent = TRUE)) != "try-error") {
    pg <- try(xml2::read_html(url))
    if(class(pg)[1] != "try-error") {
      atags <- html_nodes(pg, "a")
      hrefs <- html_attr(atags, "href")
      hrefs <- ifelse(grepl(hrefs, pattern = "^http"),
                      hrefs,
                      paste0("http://ecos.fws.gov", hrefs))
      texts <- html_text(atags)
      links <- data_frame(url = hrefs,
                          text = texts)
      links <- filter(links, grepl(links$url, pattern = "pdf|PDF"))
      plname <- html_text(html_nodes(pg, "h3")[1])
      links$Plan_Name <- rep(plname, length(links$url))
      links$Scientific_Name <- rep(species, length(links$url))
      return(links)
    } else {
      warning("read_html error; check URL and try again later.")
      return(NULL)
    }
  } else {
    warning("http_error; check URL and try again later.")
    return(NULL)
  }
}

