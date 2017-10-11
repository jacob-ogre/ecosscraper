# BSD_2_clause

#' Get all tables from a species' ECOS page
#'
#' @param url The path to the species' ECOS page; alternative to page
#' @param page An HTML page; alternative to url
#' @param verbose Print a message about tables being fetched [default = TRUE]
#' @return A list of tables, named per \link{get_table_type}, and one
#'   table (\code{scrape_info}) that records information about the scrape
#' @seealso \link{get_table}, \link{get_table_type}
#' @export
#' @examples
#' \dontrun{
#'   tabs <- get_species_tables(TECP_domestic$Species_Page[1])
#'   tab2 <- get_species_url("Abies guatemalensis") %>% get_species_tables()
#' }
get_species_tables <- function(url = NULL, page = NULL, 
                               species = NULL, verbose = TRUE) {
  check_load()
  if(!is.null(url)) {
    if(grepl(url, pattern = "^http|^www")) {
      sp_dat <- filter(TECP_table, species_page == url)
      species <- unique(sp_dat$species)
      cur_page <- get_species_page(url, verbose = verbose)
      if(is.null(cur_page)) return(NULL)
    } else {
      sp_code <- strsplit(basename(url), split = "_")[[1]][1]
      sp_dat <- filter(TECP_table, species_code == sp_code)
      species <- unique(sp_dat$species)
      cur_page <- xml2::read_html(url)
    }
  } else if(!is.null(page)) {
    if(is.null(species)) stop("Species must be specified.")
    if(class(page) == "character") {
      cur_page <- xml2::read_html(page)
    } else {
      cur_page <- page
    }
  } else {
    stop("Either a URL or an HTML page is required.")
  }
  if(verbose) message(paste("Getting tables for", species))
  p_tables <- html_nodes(cur_page, "table")
  tab_res <- lapply(p_tables, get_table)
  tab_res <- Filter(function(x) !is.null(x), tab_res)
  if(is.null(tab_res)) return(NULL)
  
  link_tbl <- get_link_df(cur_page)
  tab_upd <- lapply(tab_res, join_for_links, links = link_tbl, sp = species)
  tab_names <- lapply(tab_upd, function(x) suppressWarnings(get_table_type(x)))
  names(tab_upd) <- unlist(tab_names)
  return(tab_upd)
}

#' Return a table from an ECOS page
#'
#' @param tab A table from an rvest::html_nodes object
#' @return The table as a data.frame
#' @seealso \link{get_tables}
get_table <- function(tab) {
  res <- try(suppressWarnings(html_table(tab, fill = TRUE)), silent = TRUE)
  if(class(res) != "try-error") {
    return(res)
  } else {
    return(NULL)
  }
}

#' Join an ECOS table of text with the links to documents
#' 
#' Extracting tables with links in cells using rvest isn't straight-forward.
#' But by listing the links with \code{html_nodes(..., "href")} and the link 
#' text with \code{html_text(...)} (i.e., the link title), then joining with the 
#' table Title field, we can associate every doc link with the other fields in 
#' the table.
#'
#' @param tab The table (as a data.frame) to be joined if it has a Title var
#' @param links A data.frame with the Title of the link and the URL
#' @param species The scientific name to be included in the returned data.frame
#' @return A data.frame with URL, if tab includes a Title variable
join_for_links <- function(tab, links, species) {
  if(!is.null(tab) & dim(tab)[1] > 0) {
    if("Lead Region" %in% names(tab)) {
      tab$Species <- rep(species, length(tab[[1]]))
      tab$Status <- str_extract(tab$Status, pattern = '\\([A-Za-z, -\\"]+\\)')
      tab$Status <- gsub(tab$Status, pattern = '\\(\\"|\\"\\)', replacement = "")
      return(tab)
    } else if("Title" %in% names(tab)) {
      res <- left_join(tab, links, by = "Title")
      res$Species <- rep(species, length(res[[1]]))
      res <-  distinct(res, Species, Title, .keep_all = TRUE)
      return(res)
    } else if("HCP Plan Summaries" %in% names(tab)) {
      res <- left_join(tab, links, by = c("HCP Plan Summaries" = "Title"))
      res$Species <- rep(species, length(res[[1]]))
      res <- distinct(res, Doc_Link, .keep_all = TRUE)
      return(res)
    } else if("SHA Plan Summaries" %in% names(tab)) {
      res <- left_join(tab, links, by = c("SHA Plan Summaries" = "Title"))
      res$Species <- rep(species, length(res[[1]]))
      res <- distinct(res, Doc_Link, .keep_all = TRUE)
      return(res)
    } else if("CCA Plan Summaries" %in% names(tab)) {
      res <- left_join(tab, links, by = c("CCA Plan Summaries" = "Title"))
      res$Species <- rep(species, length(res[[1]]))
      res <- distinct(res, Doc_Link, .keep_all = TRUE)
      return(res)
    } else if("CCAA Plan Summaries" %in% names(tab)) {
      res <- left_join(tab, links, by = c("CCAA Plan Summaries" = "Title"))
      res$Species <- rep(species, length(res[[1]]))
      res <- distinct(res, Doc_Link, .keep_all = TRUE)
      return(res)
    } else if("Petition Title" %in% names(tab)) {
      res <- left_join(tab, links, by = c("Petitions Document(s)" = "Title"))
      res$Species <- rep(species, length(res[[1]]))
      res <- distinct(res, Doc_Link, .keep_all = TRUE)
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
  PET_TAB <- "Petition Title"
  HCP_TAB <- "HCP Plan Summaries"
  SHA_TAB <- "SHA Plan Summaries"
  CCA_TAB <- "CCA Plan Summaries"
  CCAA_TAB <- "CCAA Plan Summaries"
  
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
  } else if(PET_TAB %in% names(df)) {
    return("PET_TAB")
  } else if(HCP_TAB %in% names(df)) {
    return("HCP_TAB")
  } else if(SHA_TAB %in% names(df)) {
    return("SHA_TAB")
  } else if(CCA_TAB %in% names(df)) {
    return("CCA_TAB")
  } else if(CCAA_TAB %in% names(df)) {
    return("CCAA_TAB")
  } else {
    return(names(df)[1])
  }
}
  
#' Extract named tables from a list of tables extracted for >1 species
#'
#' @param ls The list of tables from \link{get_tables}
#' @return A data.frame of type specified by \code{table}
#' @seealso \link{get_tables}
#' @export
bind_tables <- function(ls, table) {
  res <- lapply(names(ls), function(x) ls[[x]][[table]])
  return(dplyr::bind_rows(res))
}

#' Get the table at https://ecos.fws.gov/ecp0/pub/speciesRecovery.jsp
#' 
#' @details In addition to the recovery plans listed on each species' page in
#' ECOS, FWS maintains a single table of recovery plans and their purported 
#' status (e.g., final). This fetches that table and splits apart the species 
#' information variable.
#' @return A data_frame with nine variables: \enumerate{
#'   \item{Scientific_Name}
#'   \item{Common_Name}
#'   \item{Where_Listed}
#'   \item{Plan_Name}
#'   \item{Plan_URL}
#'   \item{Plan_Date}
#'   \item{Plan_Stage}
#'   \item{Lead_Region}
#'   \item{List_Status}
#' }
#' @export
#' 
#' @examples
#' \dontrun{
#'   rec <- get_recovery_table()
#' }
get_recovery_table <- function() {
  url <- URLencode("https://ecos.fws.gov/ecp0/pub/speciesRecovery.jsp")
  if(class(try(http_error(url))) != "try-error") {
    pg <- try(xml2::read_html(url), silent = TRUE)
    if(class(pg)[1] != "try-error") {
      tab <- html_table(pg)[[1]]
      names(tab) <- c("Species_Info", "Plan_Name", "Plan_Act_Status",
                      "Plan_Date", "Plan_Stage", "Lead_Region", "List_Status")
      tab <- filter_rep_rows(tab)
      entity <- extract_name_info(tab)
      newt <- data_frame(Scientific_Name = entity$scientific,
                         Common_Name = entity$common,
                         Where_Listed = entity$place,
                         Plan_Name = tab$Plan_Name,
                         Plan_Date = as.Date(tab$Plan_Date, "%m/%d/%Y"),
                         Plan_Stage = tab$Plan_Stage,
                         Lead_Region = tab$Lead_Region,
                         List_Status = tab$List_Status)
      
      atag <- html_nodes(pg, "a")
      href <- html_attr(atag, "href")
      ltxt <- str_trim(html_text(atag))
      atag_df <- data_frame(Plan_URL = ifelse(
                                         grepl(href, 
                                               pattern = "^http|^javascript"),
                                         href,
                                         paste0("http://ecos.fws.gov", href)),
                            Plan_Name = ltxt)
      new_df <- left_join(newt, atag_df, by = "Plan_Name")
      new_df <- distinct(new_df, Scientific_Name, Where_Listed, Plan_Name,
                         .keep_all = TRUE)
      new_df <- data_frame(new_df[, 1:4],
                           Plan_URL = new_df$Plan_URL,
                           new_df[, 5:8])
      new_df <- as_data_frame(new_df)
      return(new_df)
    }
    warning("read_html error.")
    return(NULL)
  }
  warning("http_error")
  return(NULL)
}

# Remove the nonsense taxonomic group rows
filter_rep_rows <- function(tab) {
  res <- filter(tab, tab$Species_Info != tab$Plan_Name)
  return(res)
}

# Undo the name info concatenation from tab$Species_Info
extract_name_info <- function(tab) {
  spl <- str_split(tab$Species_Info, "\n")
  if(length(spl[[1]]) != 4) {
    message(paste("Length:", length(spl[[1]])))
    stop("Something is wrong with the table: should be 4 elements after split.")
  }
  com <- unlist(lapply(seq_along(spl),
                       function(x) str_trim(spl[[x]][1])))
  plc <- unlist(lapply(seq_along(spl),
                       function(x) str_trim(spl[[x]][2])))
  plc <- str_replace(plc, "^-- ", "")
  sci <- unlist(lapply(seq_along(spl),
                       function(x) str_trim(spl[[x]][4])))
  return(list(common = com, place = plc, scientific = sci))
}



