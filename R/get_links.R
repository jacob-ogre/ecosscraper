#' Gather all links for all or a subset of species on ECOS
#'
#' Collects all of the links (URLs) from ECOS for a set of species.
#'
#' @details Automates the process of gathering links, which may in turn be 
#' followed or, in the case of documents or tables, be downloaded. This is a
#' convenience wrapper over \link{get_species_links}.
#'
#' @param urls A vector of urls from ECOS to visit for link-gathering
#' @param parallel Try parallel scraping [default = TRUE]
#' @return A data.frame with variables:
#'   \describe{
#'     \item{Scientific_Name}{Self-explanatory}
#'     \item{href}{The URL as given on the ECOS page}
#'     \item{link}{The URL with domain information added}
#'     \item{text}{The text representation of the URL}
#'   }
#' @seealso \link{remove_silly_links} \link{get_species_links}
#' @export
#' @examples
#' \dontrun{
#'   res <- get_bulk_species_links(TECP_table$Species_Page[1:3])
#' }
get_bulk_species_links <- function(urls, parallel = TRUE) {
  if(parallel) {
    result <- try(mclapply(urls,
                           FUN = get_species_links,
                           mc.cores = 3))
  } else {
    result <- try(lapply(urls, FUN = get_species_links))
  }
  if(class(result[1]) != "try-error") {
    result <- bind_rows(result)
    return(result)
  }
  stop("A scraping error occurred. Please check the traceback, if available.")
}

#' Return all links on a species' ECOS page
#'
#' Return a data.frame with links (and anchor text) from a species' ECOS page.
#' 
#' @note Either \code{url} or \code{page} must be specified.
#' 
#' @param url The URL of a species' page on ECOS [default = NULL]
#' @param clean Remove useless links (e.g., www.usa.gov) [default = TRUE]
#' @param pause 0.5-3 second pause to be nice to the server [default = TRUE]
#' @param verbose Message the species being processed [default = TRUE]
#' @return A data.frame with variables:
#' \describe{
#'   \item{Scientific_Name}{The species' scientific name, as given by ECOS}
#'   \item{href}{The link as given on the ECOS page, if available}
#'   \item{link}{The link, in absolute form, if available}
#'   \item{text}{The anchor text of the <a> tag, if available}
#' }
#' @seealso \link{remove_silly_links} \link{get_bulk_species_links}
#' @export
#' @examples
#' \dontrun{
#'   res <- get_species_url("Gila purpurea") %>% get_species_links()
#'   head(res)
#' }
get_species_links <- function(url, clean = TRUE, pause = TRUE, verbose = TRUE) {
  check_load()
  record <- filter(TECP_table, Species_Page == url)
  species <- unique(record$Scientific_Name)
  if(verbose) message(paste("Fetching page for", species))
  if(pause) Sys.sleep(runif(1, 0, 3))
  page <- get_species_page(url)
  a_nodes <- try(html_nodes(page, "a"))
  if(class(a_nodes) == "try-error") err_res("No <a> nodes for", species)
  hrefs <- html_attr(a_nodes, "href")
  base_ln <- dirname(url)
  full_ln <- simplify2array(lapply(hrefs, FUN = fill_link, base_ln))
  texts <- html_text(a_nodes)
  res <- data.frame(Scientific_Name = rep(species, length(hrefs)),
                    href = hrefs,
                    link = full_ln,
                    text = texts,
                    stringsAsFactors = FALSE)
  if(clean) res <- remove_silly_links(res)
  return(res)
}

# Warning with a data.frame return for bad get_species_links
err_res <- function(e, species) {
  warning(paste("Warning:", e, species))
  err_res <- data.frame(Scientific_Name = species,
                        href = "No page",
                        link = "No page",
                        text = "No page",
                        stringsAsFactors = FALSE)
  return(err_res)
}

# Fill relative links with appropriate prefix
# 
# @param x A link (href) string; may be relative or absolute
# @param base_ln A string indicating the base URL of current page
# @return The complete link, as available.
fill_link <- function(x, base_ln) {
  base_url <- "http://ecos.fws.gov"
  if(is.na(x)) return(NA)
  if(grepl(x, pattern = "^http")) return(x)
  if(grepl(x, pattern = "^/")) return(paste0(base_url, x))
  if(grepl(x, pattern = "^#")) return(paste0(base_ln, x))
  return(x)
}

# Check if the requested page is an ECOS profile page
#
# ECOS will return a page rather than 404 if the species code is wrong. This
# checks that the page is not the "No species profile" page.
# 
# @param page An rvest read_html page
# @return logical; TRUE if a species profile, FALSE if "No species profile"
# @importFrom rvest html_text html_node
# @examples
# get_species_url("Gila purpurea") %>%
#   get_species_page() %>%
#   is_species_profile()
is_species_profile <- function(page) {
  text <- rvest::html_text(page)
  if(grepl(text, pattern = "No species profile", ignore.case = TRUE)) {
    return(FALSE)
  }
  return(TRUE)
}

#' Get the links for ESA five-year review documents from ECOS species' pages
#'
#' @param df A data.frame of species' links.
#' @return df, filtered for only five-year review links
#' @importFrom dplyr filter
#' @export
#' @examples
#' \dontrun{
#'   five_yr <- get_species_url("Gila purpurea") %>% 
#'                get_species_links() %>%
#'                get_5yrev_links()
#' }
get_5yrev_links <- function(df) {
  res <- dplyr::filter(df, grepl(df$href, pattern = "five_year_review"))
  return(res)
}

#' Get the links for recovery plans on ECOS species' pages
#'
#' @param df A data.frame of species' links.
#' @return df, filtered for only recovery plan links
#' @importFrom dplyr filter
#' @export
#' @examples
#'   recovery <- get_species_url("Gila purpurea") %>% 
#'                 get_species_links() %>%
#'                 get_recovery_links()
get_recovery_links <- function(df) {
  res <- dplyr::filter(df, grepl(href, pattern = "recovery_plan"))
  return(res)
}

#' Get the links for Federal Register documents on ECOS species' pages
#'
#' @param df A data.frame of links from \link{get_species_links}
#' @return df, filtered for only Fed. Reg. links
#' @importFrom dplyr filter
#' @export
#' @examples
#'   fed_reg <- get_species_url("Gila purpurea") %>% 
#'                get_species_links() %>%
#'                get_fedreg_links()
get_fedreg_links <- function(df) {
  res <- dplyr::filter(df, grepl(href, pattern = "federal_register|gpo"))
  return(res)
}

#' Get the links to conservation plan pages on ECOS species' pages
#'
#' @param df A data.frame of species' links.
#' @return df, filtered for links to conservation plans (SHA, HCP, CCAA)
#' @importFrom dplyr filter
#' @export
#' @examples
#' \dontrun{
#'   get_species_url("Gila purpurea") %>% 
#'     get_species_links() %>% 
#'     get_cons_plan_links()
#' }
get_cons_plan_links <- function(df) {
  res <- filter(df, grepl(href, pattern = "conservationPlan"))
  return(res)
}

#' Get a listing of link suffixes for HCPs, SHA, and CCA/As
#'
#' @note Does not use the ECOS conservation plan page
#' because we know that many plans linked on species' ECOS pages do not 
#' appear in the conservation plan portal.
#' 
#' @param url The species' ECOS page URL to scrape
#' @param type The type of conservation agreement to search for; one of
#'   \itemize{
#'     \item{HCP}
#'     \item{SHA}
#'     \item{CCA}
#'     \item{CCAA}
#'   }
#' @param verbose Print messages while processing [default = TRUE]
#' @return A data.frame with plan type, plan name, species, and link to the plan
#' @export
#' @examples
#' \dontrun{
#' agmt <- get_species_url("Gila purpurea") %>%
#'           get_agmt_type_links(type = "HCP")
#' }
get_agmt_type_links <- function(url, type, verbose = TRUE) {
  check_load()
  species <- unique(filter(TECP_table, 
                    Species_Page == url)$Scientific_Name)
  tabs <- get_species_tables(url, verbose = FALSE)
  if(type == "HCP") {
    cur_tab <- tabs[["HCP Plan Summaries"]]
  } else if(type == "SHA") {
    cur_tab <- tabs[["SHA Plan Summaries"]]
  } else if(type == "CCA") {
    cur_tab <- tabs[["CCA Plan Summaries"]]
  } else if(type == "CCAA") {
    cur_tab <- tabs[["CCAA Plan Summaries"]]
  } else {
    message("Please specify a type of HCP, SHA, CCA, or CCAA")
    cur_tab <- NULL
  }
  if(is.null(cur_tab)) {
    message(sprintf("No plans of type %s for %s", type, species))
    return(NULL)
  }
  link <- get_species_links(url, verbose = verbose)
  names(cur_tab)[1] <- "text"
  join <- left_join(cur_tab, link, by = "text")
  join$Type <- rep(type, length(join[[1]]))
  res <- select_(join, "Type", "Plan" = "text", "Scientific_Name", 
                 "Link" = "link")
  return(res)
}
