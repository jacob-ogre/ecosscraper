#' Get the table of counties of occurrence from ECOS
#' 
#' @param url The URL of the counties-of-occurrence page for a species
#' @param species The scientific name of the species
#' @return A data_frame of counties of occurrence provided by ECOS
#' @export
#' @examples
#' \dontrun{
#'   url <- "https://ecos.fws.gov/ecp0/profile/countiesBySpecies?entityId=2"
#'   URARHO <- get_counties(url)
#' }
get_counties <- function(url = NULL, page = NULL, species) {
  if(is.null(page) & !is.null(url)) {
    url <- URLencode(url)
    if(!grepl(url, pattern = "countiesBySpecies")) {
      warning("Expected a URL to a species' counties list.")
      return(NULL)
    }
    con <- try(http_error(url), silent = TRUE)
    if(class(con) != "try-error") {
      pg <- try(xml2::read_html(url))
    }
  } else {
    pg <- read_html(page)
  }
  if(class(pg)[1] != "try-error") {
    tab <- html_table(pg, fill = TRUE)[[1]]
    tab$Scientific_Name <- rep(species, length(tab$State))
    return(tab)
  } else {
    warning("read_html error; check URL and try again later.")
    return(NULL)
  }
}

#' Get data linked to a section 10 conservation plan
#' 
#' @details The ECOS pages for conservation plans include covered species,
#' (\code{Listed_Species / Non-Listed_Species}), but those "lists" are just a
#' concatenation of common names, scientific names, where listed, etc. The
#' \code{species} parameter is used to tie outgoing links from a species' ECOS
#' page to the summary data. These can later be checked to make sure that all
#' covered species listed on the plan page are linked to the species' page.
#' 
#' @param url The URL of the summary page for a section 10 conservation plan
#' @param pg An html page (character) of a section 10 conservation plan
#' @return A data_frame of summary data about the agreement or NULL
#' @export
#' @examples
#' \dontrun{
#'   grizz <- "https://ecos.fws.gov/ecp0/conservationPlan/plan?plan_id=1470"
#'   URARHO <- get_conservation_plan_data(grizz)
#' }
get_conservation_plan_data <- function(url = NULL, pg = NULL) {
  if(!is.null(pg)) {
    pg <- try(xml2::read_html(pg))
  } else if(!is.null(url)) {
    url <- gsub(url, pattern = "conserv_plans", replacement = "ecp0")
    url <- URLencode(url)
    con <- try(http_error(url), silent = TRUE)
    if(class(con) != "try-error") {
      pg <- try(xml2::read_html(url))
    } else {
      warning("read_html error; check URL and try again later.")
      return(NULL)
    }
  } else {
    warning("Either url or pg (page obj) must be specified. Try again.")
    return(NULL)
  }
  if(class(pg)[1] != "try-error") {
    tab <- html_table(pg, fill = TRUE) %>% bind_rows()
    names(tab) <- c("Variable", "Value")
    rownames(tab) <- gsub(
                       gsub(tab$Variable,
                            pattern = " |/",
                            replacement = "_"),
                       pattern = "\\(|\\)",
                       replacement = "")
    tab <- as_data_frame(t(tab))
    tab <- tab[-1, ]
    tab$Plan_Name <- html_text(html_nodes(pg, "h3")[1])
    tab$plan_url <- ifelse(is.null(url), NA, url)
    return(tab)
  } else {
    warning("http_error; check URL and try again later.")
    return(NULL)
  }
}
      
#' Get links to documents on a section 10 conservation plan page
#' 
#' @param url The URL of the summary page for a section 10 conservation plan
#' @param pg An html page (character) of a section 10 conservation plan
#' @return A data_frame of links to agreement documents
#' @export
get_conservation_plan_doc_links <- function(url = NULL, pg = NULL) {
  if(!is.null(pg)) {
    pg <- xml2::read_html(pg)
  } else if(!is.null(url)) {
    url <- gsub(url, pattern = "conserv_plans", replacement = "ecp0")
    url <- URLencode(url)
    if(class(try(http_error(url), silent = TRUE)) != "try-error") {
      pg <- try(xml2::read_html(url))
    } else {
      warning("read_html error; check URL and try again later.")
      return(NULL)
    } 
  }
  if(class(pg)[1] != "try-error") {
    atags <- html_nodes(pg, "a")
    hrefs <- html_attr(atags, "href")
    hrefs <- ifelse(grepl(hrefs, pattern = "^http"),
                    hrefs,
                    paste0("http://ecos.fws.gov", hrefs))
    texts <- html_text(atags)
    links <- data_frame(
      url = hrefs,
      text = texts
    )
    links <- filter(links, grepl(links$url, pattern = "pdf|PDF"))
    if(dim(links)[1] == 0) {
      links <- data_frame(
        url = c(NA),
        text = c(NA)
      )
    }
    plname <- html_text(html_nodes(pg, "h3")[1])
    links$Plan_Name <- plname
    links$plan_url <- ifelse(is.null(url), NA, url)
    return(links)
  } else {
    warning("http_error; check URL and try again later.")
    return(NULL)
  }
}

#' Get table of species covered by a conservation plan
#' 
#' @param spp A character vector of covered species from a conservation plan's
#'   data table, e.g., from get_conservation_plan_data
#' @param plan_url The url for the conservation plan summary page; needed for 
#'   JOINing with other cons. plan datasets
#' @return A table of species by plan url, with one species per row
#' @export
get_conservation_plan_listed_species <- function(spp, plan_url) {
  ls1 <- str_split(spp, "\n\n")
  p1 <- str_split(ls1[[1]], " \n ")
  p2 <- lapply(p1, str_replace, pattern = "^\n", replacement  = "")
  names <- lapply(p2, function(x) {
    str_split(x[1], "\n ")
  }) %>% unlist(recursive = FALSE)
  stats <- lapply(p2, function(x) {
    str_split(x[2], "\n ")
  }) %>% unlist(recursive = FALSE)
  data_frame(
    common_name = lapply(names, `[`, 1) %>% unlist(),
    species = lapply(names, `[`, 2) %>%
      str_replace("^\\(", "") %>%
      str_replace("\\)$", "") %>%
      unlist(),
    population = lapply(stats, `[`, 1) %>% unlist(),
    status = lapply(stats, `[`, 2) %>% unlist(),
    plan_url = plan_url
  )
}

#' Get table of non-listed species covered by a conservation plan
#' 
#' NOTE: we can't pull out just the scientific names of covered species because 
#' of variation in how FWS records scientific names in the conservation plan 
#' summary pages. \code{grepl} and other tools should be used to filter.
#' 
#' @param spp A character vector of covered species from a conservation plan's
#'   data table, e.g., from get_conservation_plan_data
#' @param plan_url The url for the conservation plan summary page; needed for 
#'   JOINing with other cons. plan datasets
#' @return A table of species by plan url, with one species per row
#' @export
get_conservation_plan_nonlisted_spp <- function(spp, plan_url) {
  ul1 <- spp %>%  str_split("\n") %>% 
    unlist(recursive = F) %>%
    str_split("\\|")
  codes <- lapply(ul1, `[[`, 1) %>% unlist()
  spp_info <- lapply(ul1, function(x) {
    ifelse(length(x) == 1, x, x[[2]])}) %>% unlist() %>%
    str_split(" - ")
  common_name <- lapply(spp_info, `[[`, 1)
  sci_name_place <- lapply(spp_info, function(x) {
    ifelse(length(x) == 1, x, x[[2]])
  })
  data_frame(
    species_code = codes,
    common_name = common_name,
    sci_name_place = sci_name_place,
    plan_url = plan_url
  )
}

#' Get table of land use types per conservation plan
#' 
#' @param uses A character vector of covered species from a conservation plan's
#'   data table, e.g., from get_conservation_plan_data
#' @param plan_url The url for the conservation plan summary page; needed for 
#'   JOINing with other cons. plan datasets
#' @return A table of land use by plan url, with one land use per row
#' @export
get_conservation_plan_land_use <- function(uses, plan_url) {
  ul1 <- uses %>% str_split("\n\n") %>% unlist()
  data_frame(
    land_use = ul1,
    plan_url = plan_url
  )
}

#' Get a table of species covered by CCA/As
#' 
#' @param txt The text to be parsed
#' @param url The url of the CCA/A plan summary data
#' @export
#' 
get_cca_species <- function(txt, url) {
  p1 <- str_split(txt, "\n\n\n") %>% unlist() %>% str_split("\n")
  sp <- lapply(p1, `[[`, 1) %>% unlist() %>% str_split(" - ")
  common <- lapply(sp, `[[`, 1) %>% unlist()
  p2 <- try(lapply(sp, `[[`, 2) %>% unlist(), silent = TRUE)
  if(class(p2) == "try-error") {
    species <- c(NA)
    popn <- c(NA)
  } else {
    species <- str_extract(p2, "\\([A-Za-z \\.\\(\\)=]+\\)") %>%
      str_replace_all("^\\(|\\)$", "") %>% unlist()
    popn <- str_replace(p2, "\\([A-Za-z \\.\\(\\)=]+\\)", "") %>% unlist()
  }
  
  cur_stat <- try(lapply(p1, `[[`, 2) %>% unlist(), silent = TRUE)
  if(class(cur_stat) == "try-error") {
    cur_stat <- c(NA)
  }
  past_stat <- try(lapply(p1, `[[`, 3) %>% unlist(), silent = TRUE)
  if(class(past_stat) == "try-error") {
    past_stat <- c(NA)
  }
  result <- data_frame(
    species = species,
    common_name = common,
    population = popn,
    cur_esa_status = cur_stat,
    past_esa_status = past_stat,
    plan_url = url
  )
  return(result)
}
