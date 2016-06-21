library(readr)
library(stringr)

ecos_listed_spp <- read_tsv("./data-raw/ecos_listed_spp.tsv")
names(ecos_listed_spp) <- str_replace_all(names(ecos_listed_spp),
                                          " ",
                                          "_")
names(ecos_listed_spp)

devtools::use_data(ecos_listed_spp, overwrite = TRUE)
