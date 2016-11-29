# BSD_2_clause

#' Get all tables from a species' ECOS page
#'
#' @param url The path to the species' ECOS page
#' @return A list of tables, named per \link{get_table_type}, and one
#'   table (\code{scrape_info}) that records information about the scrape
#' @seealso \link{get_table}, \link{get_table_type}
#' 
#' @importFrom rvest html_text html_nodes html_attr
#' @importFrom dplyr filter
#' @importFrom digest digest
#' @export
#' @examples
#' get_species_tables(TECP_domestic$Species_Page[1])
get_species_tables <- function(url) {
  if(!exists("TECP_table")) {
    data("TECP_table")
  }
  sp_dat <- dplyr::filter(TECP_table, Species_Page == url)
  species <- unique(sp_dat$Scientific_Name)
  cur_page <- get_species_page(url)
  if(is.null(cur_page)) return(NULL)
  p_tables <- rvest::html_nodes(cur_page, "table")
  tab_res <- lapply(p_tables, get_table)
  if(is.null(tab_res)) return(NULL)
  
  link_tbl <- get_link_table(cur_page)  
  tab_upd <- lapply(tab_res, join_for_links, links = link_tbl, sp = species)
  tab_names <- lapply(tab_upd, function(x) suppressWarnings(get_table_type(x)))
  names(tab_upd) <- unlist(tab_names)
  
  summary <- get_species_page_summary(cur_page, url, species)
  tab_upd[["scrape_info"]] <- summary
  return(tab_upd)
}

#' Return a table from an ECOS page
#'
#' @param tab A table from an rvest::html_nodes object
#' @return The table as a data.frame
#' @seealso \link{get_tables}
#' @importFrom rvest html_table
get_table <- function(tab) {
  res <- try(suppressWarnings(html_table(tab, fill = TRUE)), silent = TRUE)
  if(class(res) != "try-error") {
    return(res)
  } else {
    return(NULL)
  }
}

# Join an ECOS table of text with the links to documents
# 
# Extracting tables with links in cells using rvest isn't straight-forward.
# But by listing the links with \code{html_nodes(..., "href")} and the link 
# text with \code{html_text(...)} (i.e., the link title), then joining with the 
# table Title field, we can associate every doc link with the other fields in 
# the table.
#
# @param tab The table (as a data.frame) to be joined if it has a Title var
# @param links A data.frame with the Title of the link and the URL
# @param species The scientific name to be included in the returned data.frame
# @return A data.frame with URL, if tab includes a Title variable
# @importFrom dplyr left_join
join_for_links <- function(tab, links, species) {
  if(!is.null(tab)) {
    if("Title" %in% names(tab)) {
      res <- dplyr::left_join(tab, links, by = "Title")
      res$Species <- rep(species, length(res[[1]]))
      return(res)
    }
    tab$Species <- rep(species, length(tab[[1]]))
    return(tab)
  }
  return(NULL)
}

# Return the category of ECOS table
#
# @details Each species' ECOS page contains several tables, but which tables
# are present on any given page can vary. The tables are not given attr IDs, 
# which would allow easy identification of the table types, so we use the 
# headings of each table to determine the types.
# 
# @param df A data.frame from an ECOS table
# @return The category for the table; one of
#   \itemize{
#     \item{SP_TAB}{A table with basic listed species information}
#     \item{FR_TAB}{A table with Federal Register documents}
#     \item{CH_TAB}{A table with critical habitat documents}
#     \item{REC_TAB}{A table with recovery plan information}
#     \item{DOC_TAB}{A table with additional documents}
#     \item{REV_TAB}{A table with 5-year review documents}
#     \item{others}{One of several table types, e.g., HCP documents}
#   }
get_table_type <- function(df) {
  SP_TAB <- c("Status", "Date Listed", "Lead Region", "Where Listed", "Species")
  FR_TAB <- c("Date", "Citation Page", "Title", "Doc_Link", "Species")
  CH_TAB <- c("Date", "Citation Page", "Title", "Document Type", "Status",
              "Doc_Link", "Species")
  REC_TAB <- c("Date", "Title", "Plan Action Status", "Plan Status",
               "Doc_Link", "Species")
  DOC_TAB <- c("Date", "Citation Page", "Title", "Document Type", "Doc_Link",
               "Species")
  REV_TAB <- c("Date", "Title", "Doc_Link", "Species")

  if(is.null(names(df))) {
    return("UNK_TAB")
  } else if(all(names(df) == SP_TAB)) {
    return("SP_TAB") 
  } else if(all(names(df) == FR_TAB)) {
    return("FR_TAB")
  } else if(all(names(df) == CH_TAB)) {
    return("CH_TAB")
  } else if(all(names(df) == REC_TAB)) {
    return("REC_TAB")
  } else if(all(names(df) == DOC_TAB)) {
    return("DOC_TAB")
  } else if(all(names(df) == REV_TAB)) {
    return("REV_TAB")
  } else {
    return(names(df)[1])
  }
}

# Get a summary data.frame of an ECOS page scrape
# 
# @param pg An ECOS species page from \link{get_species_page}
# @param url The url of the species page
# @param species The scientific name of the species whose page was scraped
# @importFrom rvest html_nodes html_attr html_text
get_species_page_summary <- function(pg, url, species) {
  page_txt <- rvest::html_text(pg)
  md5_hash <- digest::digest(page_txt)
  tab_1 <- data.frame(Species = species,
                      Page = url, 
                      Scrape_Date = Sys.Date(), 
                      Page_Text_MD5 = md5_hash,
                      stringsAsFactors = FALSE)
  return(tab_1)
}
  
#' Extract named tables from a list of tables extracted for species
#'
#' @param ls The list of tables from \link{get_tables}
#' @return A data.frame of type specified by \code{table}
#' @seealso \link{get_tables}
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \dontrun{
#' dplyr::distinct(bind_tables(test, "SP_TAB"))
#' }
bind_tables <- function(ls, table) {
  res <- lapply(names(ls), function(x) ls[[x]][[table]])
  return(dplyr::bind_rows(res))
}
