# BSD_2_clause

library(dplyr)
library(ecosscraper)
library(parallel)

data("TECP_domestic")

# Let's try to get all of the ECOS tables...for domestic species, at least
species <- unique(TECP_domestic$Species_Page)
# ECOS_tabs <- parallel::mclapply(species, get_tables, mc.cores = 3)
ECOS_tabs <- lapply(species, get_tables)
names(ECOS_tabs) <- TECP_domestic$Scientific_Name
save(ECOS_tabs, file = "data/ECOS_tabs.rda")

test_SP_TAB <- distinct(bind_tables(ECOS_tabs, "SP_TAB"))
test_FR_TAB <- distinct(bind_tables(ECOS_tabs, "FR_TAB"))
test_CH_TAB <- distinct(bind_tables(ECOS_tabs, "CH_TAB"))
test_REC_TAB <- distinct(bind_tables(ECOS_tabs, "REC_TAB"))

##########
# Testing
#
# species <- TECP_domestic$Species_Page[1:5]
# test <- lapply(species, get_tables)
# names(test) <- TECP_domestic$Scientific_Name[1:5]
# length(test)
# lapply(test, names)
# 
# test_SP_TAB <- distinct(bind_tables(test, "SP_TAB"))
# test_FR_TAB <- distinct(bind_tables(test, "FR_TAB"))
# test_CH_TAB <- distinct(bind_tables(test, "CH_TAB"))
# test_REC_TAB <- distinct(bind_tables(test, "REC_TAB"))
# 
# # Try the parallel version...what will break???
# species <- TECP_domestic$Species_Page[1:5]
# test <- parallel::mclapply(species, get_tables, mc.cores = 2)
# names(test) <- TECP_domestic$Scientific_Name[1:5]
# length(test)
# lapply(test, names)
# 
# test_SP_TAB <- distinct(bind_tables(test, "SP_TAB"))
# test_FR_TAB <- distinct(bind_tables(test, "FR_TAB"))
# test_CH_TAB <- distinct(bind_tables(test, "CH_TAB"))
# test_REC_TAB <- distinct(bind_tables(test, "REC_TAB"))

