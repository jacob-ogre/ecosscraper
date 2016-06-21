#' Scrape the state pages in ECOS to get ESA-listed species.
#'
#' @return A data.frame with
#' @seealso if any see alsos
#' @export
#' @examples
#' one or more lines to demo the function
get_spp_states <- function() {
  spp_links_ls <- list()
  for(i in 1:length(state_links[,1])) {
    spp_links_ls <- c(spp_links_ls,
                      get_state_spp(state_links[i,1], state_links[i,2]))
  }
  species_states_links <- rbind.fill(spp_links_ls)
  species_states_links
}
