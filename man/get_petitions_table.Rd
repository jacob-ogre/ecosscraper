% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_json_data.R
\name{get_petitions_table}
\alias{get_petitions_table}
\title{Get data on ESA action petitions}
\usage{
get_petitions_table(tsn = NULL)
}
\arguments{
\item{tsn}{The TSN identifier for a species, or NULL for all [default = NULL]}
}
\value{
A \link[tidyr]{data_frame} with 11 variables: \describe{
  \item{Petition_Title}{The title given by ECOS}
  \item{Petition_Date}{The date of the petition}
  \item{Area_Covered}{The states, countries, or regions covered; comma-sep}
  \item{Petitioners}{A semicolon-separated list of petitioners}
  \item{Action_Requested}{A semicolon-separated list of requested actions}
  \item{Action_Result}{FWS's conclusion on the petition}
  \item{Action_Result_URL}{URL to FWS's conclusion, if any}
  \item{Active_Petition}{Whether FWS considers the petition active}
  \item{Petition_Doc_Title}{A semicolon-separated list of documents that
      comprise the petition}
  \item{Petition_Doc_URL}{A semicolon-separated list of URLs to the petition
      documents, if any}
  \item{TSN}{The TSN of the query}
}
}
\description{
Get data on ESA action petitions
}
\details{
ECOS provides a 'slot' for a table of petitions for species, but 
that table is javascript-generated. Interestingly, we can get the raw data
using a \link[httr]{GET} call. The request is keyed to the species TSN, which
is the key used by ITIS, not the `Species Code` that FWS uses for most other 
identifiers. The default \code{tsn = NULL} will get all of the petition data,
but there is no reliable key to link petitions to species.
}
\note{
The structure of the returned data_frame could be improved to expand
the variables that are stored as strings of semicolon-delimited lists, but 
at the cost of substantially larger (and perhaps more confusing) dfs.
}
\examples{
\dontrun{
  url <- "https://ecos.fws.gov/ecp0/profile/speciesProfile?spcode=A001"
  pet <- get_species_tsn(url) \%>\% get_petitions_table()
  # or
  pet <- get_species_url("Myotis sodalis") \%>\%
           get_species_tsn() \%>\%
           get_petitions_table()
}
}
