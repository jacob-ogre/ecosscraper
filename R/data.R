#' Data.frame of base information for all ECOS species
#'
#' The data is the table returned using the URL:
#' 
#' http://ecos.fws.gov/tess_public/reports/ad-hoc-species-report?kingdom=V&
#' kingdom=I&kingdom=P&status=E&status=T&status=EmE&status=EmT&status=EXPE&
#' status=EXPN&status=SAE&status=SAT&status=C&status=P&fcrithab=on&fstatus=on&
#' fspecrule=on&finvpop=on&fgroup=on&fleadreg=on&fspcode=on&header=
#' Listed+Species
#' 
#' which can be set using options()$TE_list. The current data was scraped on
#' \code{21 June 2016}.
#' 
#' @format A data frame with 2474 rows and 10 variables
#' \itemize{
#'   \item{Scientific_Name}{The scientific name, as recorded in ECOS}
#'   \item{Common_Name}{The common name, as recorded in ECOS}
#'   \item{Species_Code}{The four-character code assigned to species in ECOS}
#'   \item{Critical_Habitat}{CFR section under which CH was declared}
#'   \item{Species_Group}{Taxonomic group of species, as recorded in ECOS}
#'   \item{Lead_Region}{FWS region responsible for recovery}
#'   \item{Federal_Listing_Status}{At time of scraping}
#'   \item{Special_Rules}{CFR section under which any special rules were made}
#'   \item{Where_Listed}{Geographic extent of listed entity}
#'   \item{Species_Page}{URL dedicated to the species on ECOS}
#' }
#' @source \link{http://ecos.fws.gov/tess_public}
"TECP"

#' Data.frame of base information for domestic ECOS species
#'
#' The data is the table is the \link{TECP} data filtered for domestic species.
#' 
#' @format A data frame with 1835 rows and 10 variables
#' \itemize{
#'   \item{Scientific_Name}{The scientific name, as recorded in ECOS}
#'   \item{Common_Name}{The common name, as recorded in ECOS}
#'   \item{Species_Code}{The four-character code assigned to species in ECOS}
#'   \item{Critical_Habitat}{CFR section under which CH was declared}
#'   \item{Species_Group}{Taxonomic group of species, as recorded in ECOS}
#'   \item{Lead_Region}{FWS region responsible for recovery}
#'   \item{Federal_Listing_Status}{At time of scraping}
#'   \item{Special_Rules}{CFR section under which any special rules were made}
#'   \item{Where_Listed}{Geographic extent of listed entity}
#'   \item{Species_Page}{URL dedicated to the species on ECOS}
#' }
#' @source \link{http://ecos.fws.gov/tess_public}
"TECP_domestic"

#' Data.frame of document links for each species in ECOS
#' 
#' @format A data frame with 23022 rows and 5 variables
#' \itemize{
#'   \item{Scientific_Name}{The scientific name, as recorded in ECOS}
#'   \item{href}{The URL given on the ECOS page}
#'   \item{link}{The URL after completing with the correct prefix}
#'   \item{text}{The text of the link, i.e., the <a> tag}
#'   \item{type}{The type of document, e.g., federal_register, recovery_plan}
#' }
#' @source \link{http://ecos.fws.gov/tess_public}
"ecos_doc_links"