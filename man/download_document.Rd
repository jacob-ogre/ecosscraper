% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_files.R
\name{download_document}
\alias{download_document}
\title{GET a document from ECOS.}
\usage{
download_document(url, file, pause = TRUE)
}
\arguments{
\item{url}{A URL from ECOS to download a document}

\item{file}{File to which the document will be downloaded}

\item{pause}{Whether to pause for 0.5-3 seconds during scraping}
}
\value{
A data_frame with destination and success information
}
\description{
Simple function to download a file from ECOS
}
\details{
This is a very thin wrapper for \link[pdfdown]{download_pdf}. Keeping
the function here just in case removal would break something else.

As with \link[pdfdown]{download_pdf}, this function does an auto-
rename to convert the URL to the file name, and places it in the destination
directory, subd.
}
\examples{
\dontrun{
  res <- download_document("http://ecos.fws.gov/docs/five_year_review/doc3847.pdf", 
                           "~/Downloads/doc3847.pdf")
}
}
\seealso{
\link[pdfdown]{download_pdf}
}
