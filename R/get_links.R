#' Get a table of T, E, C, and P data.
#'
#' Use the URL in options()$TE_list to get data for fetching species data.
#'
#' @return A data.frame with variables:
#' \itemize{
#'   \item Scientific_Name
#'   \item Common_Name
#'   \item Species_Code
#'   \item Critical_Habitat
#'   \item Species_Group
#'   \item Lead_Region
#'   \item Federal_Listing_Status
#'   \item Special_Rules
#'   \item Where_Listed
#'   \item Species_Page
#' }
#' @export
#' @examples
#' all_spp <- get_TECP_table()
#' head(all_spp)
get_TECP_table <- function() {
  page <- xml2::read_html(options()$TE_list)
  tabl <- html_nodes(page, "table")
  all_spp <- as.data.frame(html_table(tabl))
  names(all_spp) <- gsub(x = names(all_spp), 
                         pattern = ".", 
                         replacement = "_",
                         fixed = TRUE)
  all_spp$Species_Page <- paste0(options()$ECOS_sp_prefix,
                                 all_spp$Species_Code)
  return(all_spp)
}

#' Gather all links for all or a subset of ECOS species.
#'
#' Collects all of the links (<a href...>) from ECOS for species.
#'
#' Automates the process of gathering links, which may in turn be followed or,
#' in the case of documents or tables, be downloaded. 
#'
#' @param data A data.frame with 
#' @param ... Zero or more filter conditions as used in \code{dplyr::filter}
#' @param parallel TRUE (default) to try parallel scraping
#' @return A data.frame with links, species, etc.
#' @seealso \link{remove_silly_links}
#' @export
#' @examples
#' # one or more lines to demo the function
bulk_species_links <- function(data, ..., parallel = TRUE) {
  subdf <- dplyr::filter(data, ...)
  if(parallel) {
    result <- try(parallel::mclapply(subdf$Scientific_Name,
                                     FUN = get_species_links,
                                     mc.cores = 3))
  } else {
    result <- try(lapply(subdf$Scientific_Name, FUN = get_species_links))
  }
  if(class(result[1]) != "try-error") {
    result <- dplyr::rbind_all(result)
    return(result)
  }
  stop("A scraping error occurred.")
}

#' Return all links on a species' ECOS page.
#'
#' Return a data.frame with links and their text, from a species' page on ECOS.
#'
#' @param species Scientific name of a species, as given on ECOS
#' @return A 1-row data.frame with variables:
#' \describe{
#'   \item{Scientific_Name}{The species' scientific name, as given by ECOS}
#'   \item{href}{The link as given in the <a> tag on the ECOS page}
#'   \item{link}{The link, in absolute form, if available}
#'   \item{text}{The anchor text of the <a> tag, if available}}
#' @import rvest
#' @export
#' @examples
#' # None at the moment...
get_species_links <- function(species, pause = TRUE) {
  err_res <- function(e, species) {
    print(paste(e, species))
    err_res <- data.frame(Scientific_Name = species,
                        href = "No page",
                        link = "No page",
                        text = "No page",
                        stringsAsFactors = FALSE)
    return(err_res)
  }

  if(pause == TRUE) Sys.sleep(runif(1, 0, 3))
  print(paste("Fetching page for", species))
  base_ln <- try(get_species_url(species))
  if(class(base_ln) == "try-error") err_res("URL not found for", species)
  page <- try(get_species_page(species))
  if(class(page[1]) == "try-error") err_res("Error getting page for", species)
  a_nodes <- try(html_nodes(page, "a"))
  if(class(a_nodes) == "try-error") err_res("No <a> nodes for", species)
  hrefs <- html_attr(a_nodes, "href")
  full_ln <- simplify2array(lapply(hrefs, FUN = fill_link, base_ln))
  texts <- html_text(a_nodes)
  res <- data.frame(Scientific_Name = rep(species, length(hrefs)),
                    href = hrefs,
                    link = full_ln,
                    text = texts,
                    stringsAsFactors = FALSE)
  return(res)
}

#' Return the ECOS species profile for a given species.
#'
#' Each species in ECOS has its own profile page, which can be scraped; this
#' simply returns the profile page.
#'
#' There are several \code{try} statements because of the persnickettiness of
#' ECOS and/or to guard against bad species names. Most errors are caught by
#' \link{get_species_url}.
#' @param species The scientific name of a species, as given in ECOS
#' @return A page from \code{rvest::read_html}
#' @export
#' @examples
#' # one or more lines to demo the function
get_species_page <- function(species) {
  link <- try(get_species_url(species))
  if(class(link) != "try-error") {
    page <- try(read_html(link))
    if(class(page)[1] != "try-error") {
      if(is_species_profile(page)) {
        return(page)
      } else {
        print(paste("Link not to a profile for", species))
        return(NULL)
      }
    } else {
      print(link)
      return(NULL)
    }
  }
  print(link)
  return(NULL)
}

#' Fill relative links with appropriate prefix.
#'
#' @param x A link (href) string; may be relative or absolute
#' @param base_ln A string indicating the base URL of current species page
#' @return The complete link, as available.
#' @export
#' @examples
#' # None at the moment...
fill_link <- function(x, base_ln) {
  base_url <- "http://ecos.fws.gov"
  if(is.na(x)) return(NA)
  if(length(grep(x, pattern = "^http")) == 1) return(x)
  if(length(grep(x, pattern = "^/")) == 1) return(paste0(base_url, x))
  if(length(grep(x, pattern = "^#")) == 1) return(paste0(base_ln, x))
  return(x)
}

#' Check if the requested page is an ECOS profile page.
#'
#' ECOS will return a page rather than 404 if the species code is wrong. This
#' checks that the page is not the "No species profile" page.
#' @param page An rvest read_html page
#' @return logical; TRUE if a species profile, FALSE if "No species profile"
#' @export
#' @examples
#' # one or more lines to demo the function
is_species_profile <- function(page) {
  h3 <- try(html_node(page, "h3"), silent = TRUE)
  if(class(h3) != "try-error") {
    if(html_text(h3) == "No species profile") return(FALSE)
  }
  return(TRUE)
}

#' Return the profile page on ECOS for a given species.
#'
#' Uses the species code in ecos_listed_spp data.frame to form the URL.
#'
#' @param species The scientific name of a species, as given by ECOS
#' @return The URL of the species' ECOS profile
#' @export
#' @examples
#' # get_species_url("Ursus arctos horribilis")
#' #
#' # [1] "http://ecos.fws.gov/tess_public/profile/speciesProfile?spcode=A001"
get_species_url <- function(species) {
  base_url <- "http://ecos.fws.gov/tess_public/profile/speciesProfile?spcode="
  record <- dplyr::filter(ecos_listed_spp, Scientific_Name == species)
  if(dim(record)[1] > 0) {
    return(paste0(base_url, record$Species_Code[1]))
  } else {
    print(paste(species, "not found in lookup."))
    return(NULL)
  }
}

#' Get the links for five-year review documents.
#'
#' For when we want to just get the reviews.
#'
#' @param df A data.frame of species' links.
#' @return df, filtered for only five-year review links
#' @import dplyr
#' @export
#' @examples
#' # get_5yrev_links(all_links)
get_5yrev_links <- function(df) {
  res <- dplyr::filter(df, grepl(href, pattern = "five_year_review"))
  return(res)
}

#' Get the links for recovery plans.
#'
#' For when we want to just get the recovery plans.
#'
#' @param df A data.frame of species' links.
#' @return df, filtered for only recovery plan links
#' @import dplyr
#' @export
#' @examples
#' # get_recovery_links(all_links)
get_recovery_links <- function(df) {
  res <- dplyr::filter(df, grepl(href, pattern = "recovery_plan"))
  return(res)
}

#' Get the links for Federal Register documents.
#'
#' For when we want to just get the Fed. Reg. documents.
#'
#' @param df A data.frame of species' links.
#' @return df, filtered for only Fed. Reg. links
#' @import dplyr
#' @export
#' @examples
#' # get_fedreg_links(all_links)
get_fedreg_links <- function(df) {
  res <- dplyr::filter(df, grepl(href, pattern = "federal_register|gpo"))
  return(res)
}

#' Get the links to conservation plan pages.
#'
#' @param df A data.frame of species' links.
#' @return df, filtered for links to conservation plans (SHA, HCP, CCAA)
#' @import dplyr
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
#' @return A data.frame with region, type, and suffix
#' @import dplyr
#' @import rvest
#' @export
#' @examples
#' # EXAMPLE
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
      page <- try(read_html(pg))
      if(class(page[1]) == "list") {
        form <- try(html_nodes(page, "select option"))
        pgln <- try(html_attr(form, "value"))
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

