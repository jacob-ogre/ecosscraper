#' Remove entirely foreign species from a TECP data.frame
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
  if(!("U_S__or_ForeignListed" %in% names(df))) {
    stop("Expects a data.fram from get_TECP_table.")
  }
  filt <- dplyr::filter(df, U_S__or_ForeignListed != "Foreign")
  return(filt)
}

#' Remove candidate and proposed species from a TECP data.frame
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
  if(!("Federal_Listing_Status" %in% names(df))) {
    stop("Expects a data.fram from get_TECP_table.")
  }
  filt <- dplyr::filter(df, Federal_Listing_Status == "Threatened" |
                          Federal_Listing_Status == "Endangered")
  return(filt)
}

#' Return species from a specific taxonomic group TECP data.frame
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
  if(!("Species_Group" %in% names(df))) {
    stop("Expects a data.fram from get_TECP_table.")
  }
  filt <- dplyr::filter(df, Species_Group %in% group)
  return(filt)
}


