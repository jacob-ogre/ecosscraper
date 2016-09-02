# BSD_2_clause

library(dplyr)
library(ecosscraper)
library(parallel)
# library(rvest)
# library(tibble)
# library(xml2)

data("TECP_domestic")

species <- TECP_domestic$Species_Page[1:5]
test <- lapply(species, get_tables)
names(test) <- TECP_domestic$Scientific_Name[1:5]
length(test)
lapply(test, names)

test_SP_TAB <- distinct(bind_tables(test, "SP_TAB"))
test_FR_TAB <- distinct(bind_tables(test, "FR_TAB"))
test_CH_TAB <- distinct(bind_tables(test, "CH_TAB"))
test_REC_TAB <- distinct(bind_tables(test, "REC_TAB"))

# Try the parallel version...what will break???
species <- TECP_domestic$Species_Page[1:5]
test <- parallel::mclapply(species, get_tables, mc.cores = 2)
names(test) <- TECP_domestic$Scientific_Name[1:5]
length(test)
lapply(test, names)

test_SP_TAB <- distinct(bind_tables(test, "SP_TAB"))
test_FR_TAB <- distinct(bind_tables(test, "FR_TAB"))
test_CH_TAB <- distinct(bind_tables(test, "CH_TAB"))
test_REC_TAB <- distinct(bind_tables(test, "REC_TAB"))