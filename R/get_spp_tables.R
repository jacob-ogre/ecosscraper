# BSD_2_clause

#' Return a table from an ECOS page
#'
#' @param tab A table from an rvest::html_nodes object
#' @return The table as a data.frame
#' @seealso \link{get_tables}
#' @importFrom rvest html_table
get_table <- function(tab) {
  res <- try(suppressWarnings(
               rvest::html_table(tab, fill = TRUE)), silent = TRUE)
  if(class(res) != "try-error") {
    return(res)
  } else {
    return(NA)
  }
}

#' Join an ECOS table of text with the links to documents
#' 
#' Extracting tables with links in cells using rvest isn't straight-forward.
#' But by listing the links with \code{html_nodes(..., "href")} and the link 
#' text with \code{html_text(...)} (i.e., the title), then joining with the 
#' table Title field, we can associate every doc link with the other fields
#' in the table.
#'
#' @param tab The table (as a data.frame) to be joined if it has a Title var
#' @param links A data.frame with the Title of the link and the URL
#' @param sp The scientific name to be included in the returned data.frame
#' @return A data.frame with URL, if tab includes a Title variable
#' @importFrom dplyr left_join
join_for_links <- function(tab, links, sp) {
  if(!is.null(tab)) {
    if("Title" %in% names(tab)) {
      res <- dplyr::left_join(tab, links, by = "Title")
      res$Species <- rep(sp, length(res[[1]]))
      return(res)
    }
    tab$Species <- rep(sp, length(tab[[1]]))
    return(tab)
  }
  return(NULL)
}

#' Return the category of ECOS table
#'
#' More detailed description of the function
#' @param df A data.frame from an ECOS table
#' @return The category for the table; one of
#'   \itemize{
#'     \item{SP_TAB}{A table with basic listed species information}
#'     \item{FR_TAB}{A table with Federal Register documents}
#'     \item{CH_TAB}{A table with critical habitat documents}
#'     \item{REC_TAB}{A table with recovery plan information}
#'     \item{DOC_TAB}{A table with additional documents}
#'     \item{REV_TAB}{A table with 5-year review documents}
#'     \item{others}{One of several table types, e.g., HCP documents}
#'   }
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

  if(length(names(df)) == length(SP_TAB)) {
    if(all(names(df) == SP_TAB)) {
      return("SP_TAB") 
    }
  } else if(length(names(df)) == length(FR_TAB)) {
    if(all(names(df) == FR_TAB)) {
      return("FR_TAB")
    }
  } else if(length(names(df)) == length(CH_TAB)) {
    if(all(names(df) == CH_TAB)) {
      return("CH_TAB")
    }
  } else if(length(names(df)) == length(REC_TAB)) {
    if(all(names(df) == REC_TAB)) {
      return("REC_TAB")
    }
  } else if(length(names(df)) == length(DOC_TAB)) {
    if(all(names(df) == DOC_TAB)) {
      return("DOC_TAB")
    }
  } else if(length(names(df)) == length(REV_TAB)) {
    if(all(names(df) == REV_TAB)) {
      return("REV_TAB")
    }
  } else {
    return(names(df)[1])
  }
}

#' Wrapper to get all tables from a species' ECOS page
#'
#' More detailed description of the function
#' @param sp The path to the species' ECOS page
#' @return A list of tables, named per \link{get_table_type}, and one
#'   table that records information about the scrape
#' @seealso \link{get_table}, \link{get_table_type}
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_nodes html_attr
#' @importFrom dplyr filter
#' @importFrom digest digest
#' @export
#' @examples
#' get_tables(TECP_domestic$Species_Page[1])
get_tables <- function(sp) {
  cur_date <- Sys.Date()
  species <- unique(dplyr::filter(TECP_domestic, 
                           Species_Page == sp)$Scientific_Name)
  cur_page <- xml2::read_html(sp)
  page_txt <- rvest::html_text(cur_page)
  md5_hash <- digest::digest(page_txt)
  tab_1 <- data.frame(Species = species,
                      Page = sp, 
                      Date = cur_date, 
                      Page_txt_md5 = md5_hash,
                      stringsAsFactors = FALSE)
  
  a_nodes <- rvest::html_nodes(cur_page, "a")
  pg_links <- rvest::html_attr(a_nodes, "href")
  link_txt <- rvest::html_text(a_nodes)
  link_tbl <- data.frame(Doc_Link = pg_links, 
                         Title = link_txt,
                         stringsAsFactors = FALSE)
  
  p_tables <- rvest::html_nodes(cur_page, "table")
  tab_res <- lapply(p_tables, get_table)
  tab_upd <- lapply(tab_res, join_for_links, links = link_tbl, sp = species)
  tab_names <- lapply(tab_upd, get_table_type)
  names(tab_upd) <- unlist(tab_names)
  tab_upd[["scrape_info"]] <- tab_1
  return(tab_upd)
}

#' Extract named tables from a list of tables extracted for species
#'
#' More detailed description of the function
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
  return(dplyr::bind_rows(lapply(names(ls), 
                                 function(x) ls[[x]][[table]])))
}

