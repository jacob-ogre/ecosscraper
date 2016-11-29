#' Data.frame of base information for all ECOS species
#'
#' @details The data is the table returned using the URL:
#' 
#' \url{http://ecos.fws.gov/tess_public/reports/ad-hoc-species-report?kingdom=V&
#' kingdom=I&kingdom=P&status=E&status=T&status=EmE&status=EmT&status=EXPE&
#' status=EXPN&status=SAE&status=SAT&status=C&status=P&fcrithab=on&fstatus=on&
#' fspecrule=on&finvpop=on&fgroup=on&fleadreg=on&fspcode=on&fmapstatus=on&
#' header=Listed+Species}
#' 
#' By default, \code{ecosscraper} checks this URL \code{.onAttach} to ensure - 
#' assuming the user restarts their sessions regularly - that the source is 
#' up-to-date. If the URL throws a \link[httr]{http_error} then this built-in
#' data.frame will be loaded. 
#' 
#' @note Also notice that \code{ecosscraper} includes \link{get_TECP_table}, 
#' which can be used to 'override' the built-in data.
#' 
#' @note The source URL can be changed using options()$TE_list. The current data 
#' was scraped on \code{29 Nov 2016}
#' 
#' @seealso \link{get_TECP_table}
#' 
#' @format A data frame with 2445 rows and 10 variables
#' \describe{
#'   \item{Scientific_Name}{The scientific name, as recorded in ECOS}
#'   \item{Common_Name}{The common name, as recorded in ECOS}
#'   \item{Species_Code}{The four-character code assigned to species in ECOS}
#'   \item{Critical_Habitat}{CFR section under which CH was declared}
#'   \item{Species_Group}{Taxonomic group of species, as recorded in ECOS}
#'   \item{Lead_Region}{FWS region responsible for recovery}
#'   \item{Federal_Listing_Status}{At time of scraping}
#'   \item{Special_Rules}{CFR section under which any special rules were made}
#'   \item{U_S__or_ForeignListed}{One of US, US/Foreign, Foreign}
#'   \item{Where_Listed}{Geographic extent of listed entity}
#'   \item{Species_Page}{URL dedicated to the species on ECOS}
#' }
#' @source \link{http://ecos.fws.gov/tess_public}
"TECP_table"

