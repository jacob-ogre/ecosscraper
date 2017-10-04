#' Filter TECP_table to return only domestic ESA-listed species
#'
#' @details Expects a data.frame from get_TECP_table. The df may have already 
#' passed through another filter (e.g., filter_listed), but must contain the 
#' variable \code{U_S__or_ForeignListed}.
#'
#' @param df A data.frame derived from \link{get_TECP_links}
#' @param patterns A list of additional patterns to be filtered out
#' @return A data.frame containing only domestic species 
#' @importFrom dplyr filter
#' @export
filter_domestic <- function(df) {
  if(!("u_s_or_foreign_listed" %in% names(df))) {
    stop("Expects a data.frame from get_TECP_table.")
  }
  filt <- dplyr::filter(df, u_s_or_foreign_listed != "Foreign")
  return(filt)
}

#' Filter TECP_table to return only threatened and endangered species
#'
#' @details Expects a data.frame from get_TECP_table. The df may have already 
#' passed through another filter (e.g., filter_domestic), but must contain the 
#' variable \code{Federal_Listing_Status}.
#'
#' @param df A data.frame derived from \link{get_TECP_links}
#' @param patterns A list of additional patterns to be filtered out
#' @return A data.frame containing only ESA-listed species 
#' @importFrom dplyr filter
#' @export
filter_listed <- function(df) {
  if(!("federal_listing_status" %in% names(df))) {
    stop("Expects a data.frame from get_TECP_table.")
  }
  filt <- dplyr::filter(df, federal_listing_status == "Threatened" |
                          federal_listing_status == "Endangered")
  return(filt)
}

#' Filter TECP_table to return only species from a given taxonomic group
#'
#' @details Expects a data.frame from get_TECP_table. The df may have already 
#' passed through another filter (e.g., filter_domestic), but must contain the 
#' variable \code{Species_Group}.
#'
#' @param df A data.frame derived from \link{get_TECP_links}
#' @param group One or more of \itemize{
#'   \item{Amphibians}
#'   \item{Arachnids}
#'   \item{Birds}
#'   \item{Clams}
#'   \item{Conifers and Cycads}
#'   \item{Corals}
#'   \item{Crustaceans}
#'   \item{Ferns and Allies}
#'   \item{Fishes}
#'   \item{Flowering Plants}
#'   \item{Insects}
#'   \item{Lichens}
#'   \item{Mammals}
#'   \item{Reptiles}
#'   \item{Snails}
#' }
#' @param patterns A list of additional patterns to be filtered out
#' @return A data.frame containing only species from the selected taxa
#' @importFrom dplyr filter
#' @export
filter_taxa <- function(df, group = c()) {
  if(!("species_group" %in% names(df))) {
    stop("Expects a data.frame from get_TECP_table.")
  }
  filt <- dplyr::filter(df, species_group %in% group)
  return(filt)
}


