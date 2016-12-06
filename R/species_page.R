#' Return the ECOS page for a given species as an \link[xml2]{xml2} objects
#'
#' @details There are several \code{try} statements because of the 
#' persnickettiness of #' ECOS. Most errors should caught by 
#' \link{get_species_url}.
#' 
#' @note Unlike most other functions in \link{ecosscraper}, \code{verbose} is
#' \code{FALSE} by default. This function is mostly a helper and other "primary"
#' functions are \code{verbose} by default.
#'
#' @param url The url for a species' ECOS profile
#' @param verbose Whether message that the page is being fetched [default = FALSE]
#' @return A page returned by \code{rvest::read_html}
#' @export
#' @examples
#' \dontrun{
#'   page <- get_species_url("Abies guatemalensis") %>% get_species_page()
#' }
get_species_page <- function(url, verbose = FALSE) {
  url <- URLencode(url)
  if(!http_error(url)) {
    if(verbose) message(paste("Getting page", url))
    page <- try(read_html(url))
    if(class(page[1]) != "try-error") {
      if(is_species_profile(page)) {
        return(page)
      } else {
        warning(paste(url, "is not a link to a species profile"))
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

#' Use phantomjs to get a page from ECOS
#' 
#' @details A significant amount of content on ECOS pages is rendered from 
#' javascript functions, and 'standard' scraping tools like \link[xml2]{read_html}
#' miss those components. \href{https://github.com/ariya/phantomjs}{phantomjs}
#' provides a function (\code{phantomjs}) to render the full page, complete with
#' javascript elements, and write them to file. 
#' 
#' This function should be preferred over \link{get_species_page} in most cases 
#' even though it is slightly slower with the scrape because it will get certain
#' tables (e.g., the petitions table) and content like the species' range map
#' data.
#'
#' @param url The URL of the species' ECOS page
#' @param file The path to which the HTML will be written
#' @return The status of the phantomjs scrape (1 or 0) or NULL if an HTTP error
#' @export
#' @examples
#' \dontrun{
#'    An example, not run
#' }
get_ECOS_page <- function(url = NULL, file = NULL, wait = TRUE) {
  if(is.null(url) | is.null(file)) stop("Both url and file are required.")
  
  url <- URLencode(url)
  if(!http_error(url)) {
    file <- path.expand(file)
    optsList <- data_frame(url = url, file = file)
    
    args <- list(
      shQuote(system.file("scrape_page.js", package = "ecosscraper")),
      shQuote(jsonlite::toJSON(optsList))
    )
    
    res <- run_phantom(args, wait)
    
    if(is.null(res)) return(NULL)
    
    if(res != 0) {
      message(paste("phantomjs returned error", res))
      return(res)
    }
    
    return(res)
  } else {
    message(paste("HTTP error for", url))
    return(NULL)
  }
}

run_phantom <- function(args, wait = TRUE) {
  phantom_bin <- find_phantom()
  
  if(is.null(phantom_bin)) return(NULL)
  
  args <- as.character(args)
  system2(phantom_bin, args = args, wait = wait)
}

#' Find the phantomjs binary
#' 
#' @details This is taken directly from \link[webshot]{webshot}, where 
#' \code{find_phantomjs} is a non-exported function.
#' 
#' @export
find_phantom <- function() {
  # TAKEN FROM WCH's WEBSHOT package (where the function is not exported)
  path <- Sys.which( "phantomjs" )
  if (path != "") return(path)
  
  for (d in phantom_paths()) {
    exec <- if (is_windows()) "phantomjs.exe" else "phantomjs"
    path <- file.path(d, exec)
    if (utils::file_test("-x", path)) break else path <- ""
  }
  
  if (path == "") {
    message(
      "PhantomJS not found. You can install it with webshot::install_phantomjs(). ",
      "If it is installed, please make sure the phantomjs executable ",
      "can be found via the PATH variable."
    )
    return(NULL)
  }
  path.expand(path)
}

#' Install phantomjs
#' 
#' @details A simple wrapper over \link[webshot]{install_phantomjs}. NOTE that 
#' this requires using the fork at \url{https://github.com/jacob-ogre/webshot}
#' if you are using R version 3.3.2, at least until the pull request is merged
#' into the main branch of \code{webshot}.
#' 
#' @export
install_phantom <- function() {
  webshot::install_phantomjs(baseURL = "https://bitbucket.org/ariya/phantomjs/downloads/")
}

# Possible locations of the PhantomJS executable
phantom_paths <- function() {
  if (is_windows()) {
    path <- Sys.getenv('APPDATA', '')
    path <- if (dir_exists(path)) file.path(path, 'PhantomJS')
  } else if (is_osx()) {
    path <- '~/Library/Application Support'
    path <- if (dir_exists(path)) file.path(path, 'PhantomJS')
  } else {
    path <- '~/bin'
  }
  path <- c(path, system.file('PhantomJS', package = 'webshot'))
  path
}

is_windows <- function() .Platform$OS.type == "windows"
is_osx     <- function() Sys.info()[['sysname']] == 'Darwin'
is_linux   <- function() Sys.info()[['sysname']] == 'Linux'
is_solaris <- function() Sys.info()[['sysname']] == 'SunOS'

dir_exists <- function(path) utils::file_test('-d', path)

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

