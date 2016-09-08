library(lubridate)
library(readr)
library(stringr)

ecos_listed_spp <- read_tsv("./data-raw/ecos_listed_spp.tsv")
names(ecos_listed_spp) <- str_replace_all(names(ecos_listed_spp),
                                          " ",
                                          "_")
names(ecos_listed_spp)

devtools::use_data(ecos_listed_spp, overwrite = TRUE)

## Now for a table of all species in ECOS
# To get this table, go to 
#   http://ecos.fws.gov/ecp0/reports/ad-hoc-species-report-input
# and select:
#   Species Code
#   Critical Habitat
#   Family
#   First Listed
#   Group
#   Lead Region
#   Federal Listing Status
#   US or Foreign Listed
#   Where Listed
# from the checkboxes at the bottom of the page. From the generated report,
# click the 'Excel' button, which will download a file with a CSV (comma-
# separated variable) extension BUT THAT IS A TSV (tab-separated). The encoding
# appears to be UTF-16SE, so use `iconv` to convert to UTF-8 as expected by
# readr::read_tsv.
ecos_all_spp <- read_tsv("./data-raw/ECOS_all_spp.tsv")
names(ecos_all_spp) <- str_replace_all(names(ecos_all_spp),
                                       " ",
                                       "_")
ecos_all_spp$First_Listed <- mdy(ecos_all_spp$First_Listed)
names(ecos_all_spp)[10] <- "US_Foreign"
names(ecos_all_spp)[1] <- "Scientific_Name"

devtools::use_data(ecos_all_spp, overwrite = TRUE)
