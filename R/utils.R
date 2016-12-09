# Make sure the data is loaded; required for several functions.
check_load <- function() {
  if(!exists("TECP_table")) {
    data("TECP_table")
  }
}

#' Return the profile page on ECOS for a given species
#'
#' @param df A data.frame returned from get_TECP_table
#' @param species The scientific name of a species, as given by ECOS
#' @return The URL of the species' ECOS profile
#' @importFrom dplyr filter
#' @export
#' @examples
#' get_species_url("Gila purpurea")
get_species_url <- function(species) {
  check_load()
  record <- filter(TECP_table, Scientific_Name == species)
  n_hits <- length(unique(record$Species_Page))
  if(n_hits == 1) {
    return(record$Species_Page[1])
  } else if(n_hits > 1) {
    stop(paste("Multiple matches for", species, "in lookup"))
  } else {
    stop(paste(species, "not found in lookup"))
  }
}

#' Remove useless links from ECOS-scraped webpage
#'
#' Expects a data.frame with an href variable for filtering. The default 
#' patterns are based on an examination of unfiltered link tables from ECOS
#' species pages, but additional link patterns to be removed can be added using
#' the optional \code{patterns} parameter, with the patterns using regex 
#' expressions expected by \link{grepl}.
#'
#' @param df A data.frame with four columns, including href, link, and text
#' @param patterns A list of additional patterns to be filtered out
#' @return A filtered version of df
#' @export
remove_silly_links <- function(df, patterns = list()) {
  base_patt <- list("^#", "^javascript", "^http://www.fws.gov", 
                    "^http://www.usa.gov", "^http://www.doi.gov",
                    "^/ecp/(help|about)$", "^/ecp$", "crithab$")
  patterns <- c(base_patt, patterns)
  filt <- filter(df, !is.na(href))
  for(i in patterns) {
    filt <- filter(filt, !grepl(href, pattern = i))
  }
  filt <- distinct(filt, href, .keep_all = TRUE)
  return(filt)
}

# Get a data.frame of links and their titles from a web page
get_link_df <- function(pg) {
  a_nodes <- html_nodes(pg, "a")
  pg_links <- html_attr(a_nodes, "href")
  pg_links <- ifelse(grepl(pg_links, pattern = "^http|^www"),
                     pg_links,
                     paste0("http://ecos.fws.gov", pg_links))
  link_txt <- html_text(a_nodes)
  link_tbl <- data_frame(Doc_Link = pg_links, 
                         Title = str_trim(link_txt))
  return(link_tbl)
} 

#' Set the URL of the TESS table
#' 
#' @param url The new URL from which the base TESS data is scraped
#' @export
set_TE_list_opt <- function(url) {
  options("TE_list" = url)
}

#' Get a summary of an ECOS page scrape
#'
#' By default, \code{get_species_page_summary} will fetch the species' ECOS page
#' given the URL, but there may be times when the page has already been fetched.
#' In those cases, the page can be specified to save time.
#'
#' @note Apparently, FWS does not serve the same version of a species' page up
#' twice in a row. Instead, the same information will be presented in different
#' orders. We have to use \code{strsplit} along with \link[stringr]{str_trim} to
#' get clean lines, then sort before doing the MD5 hash.
#'
#' @param url The url of the species page
#' @param species The scientific name of the species
#' @param pause Pause for 0.5-3s during scraping [default = TRUE]
#' @return A \link[tidyr]{data_frame} with four variables: \itemize{
#'   \item{Species}
#'   \item{Page}
#'   \item{Scrape_Date}
#'   \item{Page_Text_MD5}
#' }
#' @export
get_species_page_summary <- function(url, species, pause = TRUE) {
  if(grepl(url, pattern = "^http|^www")) {
    pg <- get_species_page(url)
  } else {
    if(pause) Sys.sleep(runif(1, 0, 3))
    pg <- xml2::read_html(url)
  }
  page_txt <- html_text(pg)
  page_txt <- unlist(strsplit(page_txt, split = "\n"))
  page_txt <- unlist(str_trim(page_txt))
  page_txt <- sort(page_txt)
  md5_hash <- digest(page_txt)
  tab_1 <- data_frame(Species = species,
                      Page = url,
                      Scrape_Date = Sys.Date(),
                      Page_Text_MD5 = md5_hash)
  return(tab_1)
}

#' Return a species' TSN from its ECOS page
#' 
#' @details FWS uses at least four different keys for species, including the TSN
#' that is defined by ITIS (\url{http://itis.gov}). The TSN is used for some 
#' JSON data queries; this function simplifies extraction.
#'
#' @param url The URL or path to a local HTML file for a species
#' @export
#' @examples
#' \dontrun{
#'   url <- "https://ecos.fws.gov/ecp0/profile/speciesProfile?spcode=A001"
#'   get_species_tsn(url)
#' }
get_species_tsn <- function(url) {
  if(grepl(url, pattern = "^http|^www")) {
    pg <- get_species_page(url)
  } else {
    pg <- xml2::read_html(url)
  }
  scpt <- html_nodes(pg, "script")
  text <- html_text(scpt)
  text <- unlist(str_split(text, pattern = "\n"))
  text <- text[grep(text, pattern = "var tsn")]
  text <- str_extract(text, "[0-9]+")
  return(text)
}