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
#' @importFrom dplyr filter rbind_all
#' @importFrom parallel mclapply
#' @export
#' @examples
#' \dontrun{
#'   res <- get_bulk_species_links(TECP_table$Species_Page[1:3])
#' }
get_bulk_species_links <- function(urls, parallel = TRUE) {
  if(parallel) {
    result <- try(parallel::mclapply(urls,
                                     FUN = get_species_links,
                                     mc.cores = 3))
  } else {
    result <- try(lapply(urls, FUN = get_species_links))
  }
  if(class(result[1]) != "try-error") {
    result <- dplyr::bind_rows(result)
    return(result)
  }
  stop("A scraping error occurred. Please check the traceback, if present.")
}

#' Return all links on a species' ECOS page
#'
#' Return a data.frame with links (and their text) from a species' page on ECOS.
#' 
#' @param url The URL of a species' page on ECOS; see Example
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
#' @importFrom rvest html_nodes html_attr
#' @export
#' @examples
#' res <- get_species_url("Ursus maritimus") %>% get_species_links()
#' head(res)
get_species_links <- function(url, clean = TRUE, pause = TRUE, verbose = TRUE) {
  if(!exists("TECP_table")) {
    data("TECP_table")
  }
  record <- dplyr::filter(TECP_table, Species_Page == url)
  species <- unique(record$Scientific_Name)

  if(pause == TRUE) Sys.sleep(runif(1, 0, 3))
  if(verbose) message(paste("Fetching page for", species))
  page <- get_species_page(record$Species_Page)
  if(is.null(page)) {
    err_res("Error getting page for", species)
  }
  a_nodes <- try(rvest::html_nodes(page, "a"))
  if(class(a_nodes) == "try-error") err_res("No <a> nodes for", species)
  hrefs <- rvest::html_attr(a_nodes, "href")
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

#' Return the profile page on ECOS for a given species.
#'
#' @param df A data.frame returned from get_TECP_table
#' @param species The scientific name of a species, as given by ECOS
#' @return The URL of the species' ECOS profile
#' @importFrom dplyr filter
#' @export
#' @examples
#' get_species_url("Ursus arctos horribilis")
get_species_url <- function(species) {
  if(!exists("TECP_table")) {
    data("TECP_table")
  }
  record <- dplyr::filter(TECP_table, Scientific_Name == species)
  n_hits <- length(unique(record$Species_Page))
  if(n_hits == 1) {
    return(record$Species_Page[1])
  } else if(n_hits > 1) {
    stop(paste("Multiple matches for", species, "in lookup"))
  } else {
    stop(paste(species, "not found in lookup"))
  }
}

# Warning with a data.frame return
err_res <- function(e, species) {
  warning(paste("Warning:", e, species))
  err_res <- data.frame(Scientific_Name = species,
                      href = "No page",
                      link = "No page",
                      text = "No page",
                      stringsAsFactors = FALSE)
  return(err_res)
}

# Return the ECOS species profile for a given species
#
# Each species in ECOS has its own profile page, which can be scraped; this
# simply returns the profile page.
#
# There are several \code{try} statements because of the persnickettiness of
# ECOS and/or to guard against bad species names. Most errors are caught by
# \link{get_species_url}.
# @param url The url for a species' ECOS profile
# @return A page returned by \code{rvest::read_html}
# @examples
# \dontrun{
#   page <- get_species_url("Abies guatemalensis") %>% get_species_page()
# }
get_species_page <- function(url) {
  url <- URLencode(url)
  if(!httr::http_error(url)) {
    page <- try(xml2::read_html(url))
    if(class(page[1]) != "try-error") {
      if(is_species_profile(page)) {
        return(page)
      } else {
        warning(paste(url, "not a link to a species profile"))
        return(NULL)
      }
    } else {
      warning(paste("Error reading", url))
      return(NULL)
    }
  } else {
    warning(paste("http_error for", url))
    return(NULL)
  }
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

# Check if the requested page is an ECOS profile page.
#
# ECOS will return a page rather than 404 if the species code is wrong. This
# checks that the page is not the "No species profile" page.
# 
# @param page An rvest read_html page
# @return logical; TRUE if a species profile, FALSE if "No species profile"
# @importFrom rvest html_text html_node
# @examples
# get_species_url("Ursus arctos horribilis") %>%
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
#'   five_yr <- get_species_url("Ursus arctos horribilis") %>% 
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
#'   recovery <- get_species_url("Ursus arctos horribilis") %>% 
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
#'   fed_reg <- get_species_url("Ursus arctos horribilis") %>% 
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
#' # get_cons_plan_links(all_links)
get_cons_plan_links <- function(df) {
  res <- dplyr::filter(df, grepl(href, pattern = "conservationPlan"))
  return(res)
}

#' Get a listing of link suffixes for HCPs, SHA, and CCA/As.
#'
#' Uses the ECOS conservation plan page as the root and selects from dropdown.
#' 
#' THIS FUNCTION SHOULD BE DEPRECATED BECAUSE WE KNOW THE TESS CONSERVATION PLAN
#' PORTAL IS MISSING PLANS THAT ARE LINKED ON SPECIES' PAGES.
#'
#' @return A data.frame with region, type, and suffix
#' @importFrom rvest html_nodes html_attr
#' @importFrom xml2 read_html
#' @export
#' @examples
#' \dontrun{
#'   agmt <- get_agreement_links()
#' }
get_agreement_links <- function() {
  regions <- seq(1, 9)
  types <- c("HCP", "SHA", "CCA", "CCAA")
  base <- "http://ecos.fws.gov/tess_public/conservationPlan/region?region="
  suff <- "&type="
  region <- c()
  type <- c()
  suffix <- c()
  for(i in regions) {
    for(j in types) {
      pg <- paste0(base, i, suff, j)
      page <- try(xml2::read_html(URLencode(pg)))
      if(class(page[1]) == "list") {
        form <- try(rvest::html_nodes(page, "select option"))
        pgln <- try(rvest::html_attr(form, "value"))
        region <- c(region, rep(i, length(pgln)))
        type <- c(type, rep(j, length(pgln)))
        suffix <- c(suffix, pgln)
      }
    }
  }
  res <- data.frame(region = region, type = type, suffix = suffix)
  res$dups <- duplicated(res$suffix)
  res <- res[res$dups == FALSE, ]
  res <- subset(res, select=-dups)
  return(res)
}

