#' Get a df of threatened, endangered, candidate, and proposed species from ECOS
#'
#' Returns threatened, endangered, candidate, and proposed species from ECOS.
#' 
#' @details Uses the URL set in options()$TE_list to get a data.frame that 
#' includes links for fetching data for any species with pages on TESS. This 
#' data is loaded
#'
#' @return A data.frame with 11 variables:
#' \itemize{
#'   \item Scientific_Name
#'   \item Common_Name
#'   \item Species_Code
#'   \item Critical_Habitat
#'   \item Species_Group
#'   \item Lead_Region
#'   \item Federal_Listing_Status
#'   \item Special_Rules
#'   \item U_S__or_ForeignListed
#'   \item Where_Listed
#'   \item Species_Page
#' }
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom httr http_error
#' @export
#' @examples
#' # To replace the TECP_table that is loaded on startup (attach)
#' TECP_table <- get_TECP_table()
#' head(TECP_table)
get_TECP_table <- function() {
  if(!http_error(options()$TE_list)) {
    page <- read_html(options()$TE_list)
    tabl <- html_nodes(page, "table")
    all_spp <- as.data.frame(html_table(tabl))
    names(all_spp) <- gsub(x = names(all_spp), 
                           pattern = ".", 
                           replacement = "_",
                           fixed = TRUE)
    all_spp$Species_Page <- paste0(options()$ECOS_sp_prefix,
                                   all_spp$Species_Code)
    return(all_spp)
  } else {
    stop("Cannot get the website to scrape the TECP table.")
  }
}
