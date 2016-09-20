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
#' @source \link{http://ecos.fws.gov}
"ecos_doc_links"

#' Data.frame of all links for each species in ECOS
#' 
#' @format A data frame with 40815 rows and 4 variables
#' \itemize{
#'   \item{Scientific_Name}{The scientific name, as recorded in ECOS}
#'   \item{href}{The URL given on the ECOS page}
#'   \item{link}{The URL after completing with the correct prefix}
#'   \item{text}{The text of the link, i.e., the <a> tag}
#' }
#' @source \link{http://ecos.fws.gov}
"all_links"

#' Data.frame of "other recovery documents" tables from ECOS
#' 
#' ECOS \url{http://ecos.fws.gov} includes a table named "Other Recovery 
#' Documents" for most but not all species. This data.frame was collected by
#' scraping all of these tables from each listed species' page and \code{rbind}
#' the rows to a single df.
#' 
#' @format A data frame with 3136 rows and 6 variables
#' \itemize{
#'   \item{Date}{The document date as recorded in ECOS}
#'   \item{Citation Page}{The Federal Register citation for the document}
#'   \item{Title}{The text of the document link, i.e., the \code{<a>} tag}
#'   \item{Document Type}{The type of document, e.g., 5-year review notice}
#'   \item{Doc_Link}{The URL of the document, i.e., the \code{href}; may be
#'     relative or absolute}
#'   \item{Species}{The ECOS-standard scientific name of the species}
#' }
#' @source \link{http://ecos.fws.gov}
"add_document_table"

#' Summary data on ESA section 10 agreements
#'
#' Section 10(a)(1)(B) of the US Endangered Species Act sets the mechanism by
#' which non-federal entities - private landowners, state governments, and
#' others - receive authorization to 'take' listed species, or coverage for
#' certain types of activities involving candidate species, in return for
#' taking conservation actions for those species. The U.S. Fish and Wildlife
#' Service (FWS) keeps basic records about these agreements on their ECOS
#' website, \url{http://ecos.fws.gov}. This data.frame was created by scraping
#' the ECOS pages for all listed, candidate, and proposed species and extracting
#' the summary tables for all section 10 agreements. The types of agreements
#' in the data include: Habitat Conservation Plans (HCPs); Safe Harbor
#' Agreements (SHAs); Candidate Conservation Agreements (CCAs); and Candidate
#' Conservation Agreements with Assurances (CCAAs).
#'
#' @format A data frame with 1434 rows and 25 variables
#' \describe{
#' \item{\code{Link}}{Link to the local copy of the agreement page}
#' \item{\code{Outlink}}{Link(s) to any documents outside of the agreement page}
#' \item{\code{Plan_Name}}{Name of the agreement, as given on ECOS}
#' \item{\code{Agreement_Type}}{Type of agreement; one of HCP, SHA, CCA, CCAA}
#' \item{\code{USFWS_Regions}}{FWS regions in which the agreement occurs}
#' \item{\code{USFWS_Field_Offices}}{FWS offices managing the agreement}
#' \item{\code{Status}}{In-progress or Expired}
#' \item{\code{Involved_Agency}}{Agency(ies) involved in the agreement}
#' \item{\code{Date_Agmt_Permit}}{Start date of the agreement/permit}
#' \item{\code{Date_Agmt_Permit_Expired}}{End date of the agreement/permit}
#' \item{\code{Location}}{Location of the agreement, as given in ECOS}
#' \item{\code{Habitat}}{Habitat encompassed by the agreement, as given in ECOS}
#' \item{\code{States}}{State in which the agreement occurs}
#' \item{\code{Area_Covered}}{Area (acres, usually) covered by the agreement}
#' \item{\code{Area_Enrolled}}{Area (acres, usually) enrolled in the agreement}
#' \item{\code{Land_Use}}{Land use(s) covered by the agreement, as given in ECOS}
#' \item{\code{Duration}}{Agreement duration, years and months}
#' \item{\code{Plan_Agreement_Documents}}{Any attached agreement documents}
#' \item{\code{FR_Documents}}{Any attached Federal Register documents}
#' \item{\code{Region_ls}}{List data structure for FWS region}
#' \item{\code{States_ls}}{List data structure for state(s) of the agreement}
#' \item{\code{Location_ls}}{List data structure for location of the agreement}
#' \item{\code{Acres_Covered_num}}{Numeric data type for agreement acreage}
#' \item{\code{Duration_Years}}{Numeric data type for agreement duration, years}
#' \item{\code{Duration_Months}}{Numeric data for agreement duration, months}
#' }
#' @source \url{http://ecos.fws.gov}
"cons_agmt"

#' Summary data on Candidate Conservation Agreements
#'
#' Section 10(a)(1)(B) of the US Endangered Species Act sets the mechanism by
#' which non-federal entities - private landowners, state governments, and
#' others - receive authorization to 'take' listed species, or coverage for
#' certain types of activities involving candidate species, in return for
#' taking conservation actions for those species. The U.S. Fish and Wildlife
#' Service (FWS) keeps basic records about these agreements on their ECOS
#' website, \url{http://ecos.fws.gov}. 
#'
#' @format A data frame with 112 rows and 22 variables
#' \describe{
#' \item{\code{Link}}{Link to the local copy of the agreement page}
#' \item{\code{Outlink}}{Link(s) to any documents outside of the agreement page}
#' \item{\code{Plan_Name}}{Name of the agreement, as given on ECOS}
#' \item{\code{Agreement_Type}}{Type of agreement; one of HCP, SHA, CCA, CCAA}
#' \item{\code{USFWS_Regions}}{FWS regions in which the agreement occurs}
#' \item{\code{USFWS_Field_Offices}}{FWS offices managing the agreement}
#' \item{\code{Status}}{In-progress or Expired}
#' \item{\code{Involved_Agency}}{Agency(ies) involved in the agreement}
#' \item{\code{Date_Agmt_Signed}}{Start date of the agreement}
#' \item{\code{Date_Agmt_Withdrawn}}{Date agreement withdrawn}
#' \item{\code{Date_Agmt_Expired}}{End date of the agreement/permit}
#' \item{\code{Location}}{Location of the agreement, as given in ECOS}
#' \item{\code{Species}}{Species covered by the CCA; free text that needs parsing}
#' \item{\code{Habitat}}{Habitats encompassed by the agreement, as given in ECOS}
#' \item{\code{States}}{State in which the agreement occurs}
#' \item{\code{Size_Total_Area_Covered_by_State}}{Area (acres, usually) covered 
#'   by the agreement; free text that needs to be parsed}
#' \item{\code{Enrolled Size_Total_Area_Covered_by_State}}{Area (acres, usually) 
#'   enrolled under the agreement; free text that needs to be parsed}
#' \item{\code{Partner_Type}}{Category of partner in the CCA}
#' \item{\code{Land_Use}}{Land use(s) covered by the agreement, as given in ECOS}
#' \item{\code{Duration}}{Agreement duration, years and months}
#' \item{\code{Plan_Agreement_Documents}}{Any attached agreement documents}
#' \item{\code{Federal_Register_Documents}}{Any attached Federal Register documents}
#' }
#' @source \url{http://ecos.fws.gov}
"CCA"