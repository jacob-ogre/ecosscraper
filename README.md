# ecosscraper

An R package to scrape the U.S. Fish and Wildlife Service's ECOS website.

## Early version

This early version package currently has functions for:

- getting a table of listed, candidate, and proposed species, to make links to each species' ECOS page
- getting all of the links from each species' ECOS page (and cleaning out the 'silly' links)
- filtering the set of links to documents available for a species
- downloading documents
- scraping tables on section 10 agreements (e.g., Habitat Conservation Plans)

## Usage

Install using `devtools`, i.e.,

```r
devtools::install_github("jacob-ogre/ecosscraper")
```

We've tried to make the API as consistent as possible. Most functions that will be of direct use have `get_` as the prefix; they simply get the target to return as an R object. Most but not all functions take a URL as the primary argument. Many function have a `verbose` parameter, and with one exception (because it calls other functions that are verbose by default), the default is `TRUE`.

To get all of the links from the ECOS page of a single species using the species' scientific name (which must match the name recorded in ECOS),

```r
urls <- get_species_url("Gila purpurea") %>% get_species_links()
```

To scrape the tables on a species' ECOS page, which will (usually) include a date for the linked resource (e.g., PDF) and other information,

```r
tabs <- get_species_url("Gila purpurea") %>% get_species_tables()
```

There are also helper functions to extract data for particular types of documents, such as five-year reviews:

```r
five_yr <- get_species_url("Gila purpurea") %>% 
             get_species_links() %>%
             get_5yrev_links()
```

Another function that might get considerable use is `download_document`, which writes the requested document (PDFs) to file,

```r
result <- download_document("http://ecos.fws.gov/docs/five_year_review/doc3847.pdf", "~/Downloads")
```

## Updating

The package includes one dataset, the table of threatened, endangered, candidate, and proposed species (`TECP_table`) using a search from [here](http://ecos.fws.gov/ecp0/reports/ad-hoc-species-report-input). The built in TECP_table was scraped 30 Nov 2016, but the content of ECOS can change at any time. The package scrapes the report page `.onAttach` so that the newest data is available for each session. If ECOS can't be reached, then the built-in TECP_table is used. Alternatively, `TECP_table <- get_TECP_table()` may be used to try updating mid-session.

Checking for new links across all listed species can take a bit of time using defaults: the main page-getter will pause for 0.5-3 seconds by default, so as to not hammer the FWS server too much. (Checking for updates for a single species is quick.) We are using `ecosscraper` to set up a `cron` job that checks all species' pages on a weekly basis; the script and `cron` settings will be added to this repo in the future.

## Bugs

## Contributing

Interested in helping improve `ecosscraper`? [Get in touch with us](mailto:esa@defenders.org).
