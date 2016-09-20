## ecosscraper

An R package for scraping the U.S. Fish and Wildlife Service's ECOS website.

## Updating

The package includes datasets scraped 21 June 2016, but the content of ECOS can change at any time. Checking for new links across all species can take a bit of time (in part because the main page-getter will pause for 0.5-3 seconds, so as to not hammer the FWS server too much), but regular refreshes of the base datasets is warranted. (Checking for updates for a single species is quick.) We will update the datasets as time allows.

## Early version

The package currently has functions for:

- getting a table of listed, candidate, and proposed species, to make links to each species' ECOS page
- getting all of the links from each species' ECOS page (and cleaning the 'silly' links)
- filtering the set of links to documents available for a species
- downloading documents
- scraping tables on section 10 agreements

Some functions that could be useful don't yet exist, e.g., extracting TIFFs of species ranges (which we can see in a javascript console).

And we still need to write one or two vignettes.

## Contributing

Interested in helping improve `ecosscraper`? [Get in touch with us](mailto:esa@defenders.org).
