#' Get data on ESA action petitions
#' 
#' @details ECOS provides a 'slot' for a table of petitions for species, but 
#' that table is javascript-generated. Interestingly, we can get the raw data
#' using a \link[httr]{GET} call. The request is keyed to the species TSN, which
#' is the key used by ITIS, not the `Species Code` that FWS uses for most other 
#' identifiers. The default \code{tsn = NULL} will get all of the petition data,
#' but there is no reliable key to link petitions to species. 
#' 
#' @note The structure of the returned data_frame could be improved to expand
#' the variables that are stored as strings of semicolon-delimited lists, but 
#' at the cost of substantially larger (and perhaps more confusing) dfs.
#'
#' @param tsn The TSN identifier for a species, or NULL for all [default = NULL]
#' @return A \link[tidyr]{data_frame} with 11 variables: \describe{
#'   \item{Petition_Title}{The title given by ECOS}
#'   \item{Petition_Date}{The date of the petition}
#'   \item{Area_Covered}{The states, countries, or regions covered; comma-sep}
#'   \item{Petitioners}{A semicolon-separated list of petitioners}
#'   \item{Action_Requested}{A semicolon-separated list of requested actions}
#'   \item{Action_Result}{FWS's conclusion on the petition}
#'   \item{Action_Result_URL}{URL to FWS's conclusion, if any}
#'   \item{Active_Petition}{Whether FWS considers the petition active}
#'   \item{Petition_Doc_Title}{A semicolon-separated list of documents that
#'       comprise the petition}
#'   \item{Petition_Doc_URL}{A semicolon-separated list of URLs to the petition
#'       documents, if any}
#'   \item{TSN}{The TSN of the query}
#' }
#' @export
#' @examples
#' \dontrun{
#'   url <- "https://ecos.fws.gov/ecp0/profile/speciesProfile?spcode=A001"
#'   pet <- get_species_tsn(url) %>% get_petitions_table()
#'   # or
#'   pet <- get_species_url("Myotis sodalis") %>%
#'            get_species_tsn() %>%
#'            get_petitions_table()
#' }
get_petitions_table <- function(tsn = NULL) {
  resp <- GET("http://ecos.fws.gov/ecp/report/table/petitions-received.json", 
              query = list(tsn = tsn, active = "any"))
  json <- httr::content(resp, as = "text")
  df <- petition_json_to_df(json)
  if(dim(df)[1] > 0) {
    df$TSN <- rep(tsn, length(df[[1]]))
  }
  return(df)
}

petition_json_to_df <- function(json) {
  res <- jsonlite::fromJSON(json)
  dat <- res$data
  dat_ls <- lapply(dat, to_df)
  dat_df <- bind_rows(dat_ls)
  return(dat_df)
}

# This function is a little ridiculous...
to_df <- function(dat) {
  if(length(dat[[8]]) > 0) {
    doc_name <- paste(str_trim(dat[[8]]$name), collapse = ", ")
    doc_url <- str_trim(dat[[8]]$url)
    doc_url <- ifelse(grepl(doc_url, pattern = "^http|^www"),
                      doc_url,
                      paste0("http://ecos.fws.gov", doc_url))
    doc_url <- paste(doc_url, collapse = "; ")
  } else {
    doc_name <- doc_url <- NA
  }
  
  if(length(dat[[6]]) > 0) {
    act_name <- paste(str_trim(dat[[6]]$name), collapse = "; ")
    act_url <- str_trim(dat[[6]]$url)
    act_url <- ifelse(!is.na(act_url), act_url, NA)
    act_url <- paste(act_url, collapse = "; ")
  } else {
    act_name <- act_url <- NA
  }
  
  len_ch <- function(x) {
    if(length(x) > 1) return(paste(str_trim(x), collapse = "; "))
    if(length(x) == 1) return(str_trim(x))
    return(NA)
  }
  
  res <- data_frame(
    Petition_Title = len_ch(dat[[1]]),
    Petition_Date = as.Date(dat[[2]], "%m/%d/%Y"),
    Area_Covered = len_ch(dat[[3]]),
    Petitioners = len_ch(dat[[4]]),
    Action_Requested = len_ch(dat[[5]]),
    Action_Result = act_name,
    Action_Result_URL = act_url,
    Active_Petition = len_ch(dat[[7]]),
    Petition_Doc_Title = doc_name,
    Petition_Doc_URL = doc_url)
  return(res)
}
