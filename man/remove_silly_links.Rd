% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{remove_silly_links}
\alias{remove_silly_links}
\title{Remove useless links from ECOS-scraped webpage}
\usage{
remove_silly_links(df, patterns = list())
}
\arguments{
\item{df}{A data.frame with four columns, including href, link, and text}

\item{patterns}{A list of additional patterns to be filtered out}
}
\value{
A filtered version of df
}
\description{
Expects a data.frame with an href variable for filtering. The default 
patterns are based on an examination of unfiltered link tables from ECOS
species pages, but additional link patterns to be removed can be added using
the optional \code{patterns} parameter, with the patterns using regex 
expressions expected by \link{grepl}.
}
