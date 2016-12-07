#' Get data on ESA action petitions
#' 
#' @details ECOS provides a 'slot' for a table of petitions for species, but 
#' that table is javascript-generated. Interestingly, we can get the raw data
#' using a \link[httr]{GET} call. The request is keyed to the species TSN, which
#' is the key used by ITIS, rather than the `Species Code` that FWS uses for 
#' most other identifiers. However, no TSN is needed to get all of the petition
#' data.
#'
#' @param name description
#' @importFrom  package function(s)
#' @export
#' @examples
#' An example to be run during package build
#' \dontrun{
#'    An example, not run
#' }
get_petitions_data <- function(tsn = NULL) {
  json <- GET("http://ecos.fws.gov/ecp/report/table/petitions-received.json", 
              query = list(tsn = tsn, active = "any"))
  df <- petition_json_to_df(json)
}

petition_json_to_df <- function(json) {
  
}