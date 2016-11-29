# BSD_2_clause

library(dplyr)
library(ecosscraper)
library(parallel)

# Let's try to get all of the ECOS tables...for domestic species, at least
TECP_domestic <- filter_domestic(TECP_table) %>% filter_listed()
species <- unique(TECP_domestic$Species_Page)
# ECOS_tabs <- parallel::mclapply(species, get_tables, mc.cores = 6)
ECOS_tabs <- lapply(species[1:4], get_tables)
names(ECOS_tabs) <- unique(TECP_domestic$Scientific_Name[1:4])
save(ECOS_tabs, file = "data-raw/ECOS_tabs.rda")

species_info_table <- distinct(bind_tables(ECOS_tabs, "SP_TAB"))
federal_register_table <- distinct(bind_tables(ECOS_tabs, "FR_TAB"))
critical_habitat_table <- distinct(bind_tables(ECOS_tabs, "CH_TAB"))
recovery_plan_table <- distinct(bind_tables(ECOS_tabs, "REC_TAB"))
add_document_table <- distinct(bind_tables(ECOS_tabs, "DOC_TAB"))
five_year_review_table <- distinct(bind_tables(ECOS_tabs, "REV_TAB"))
scraping_table <- distinct(bind_tables(ECOS_tabs, "scrape_info"))

save(species_info_table, file = "data/species_info_table.rda")
save(federal_register_table, file = "data/federal_register_table.rda")
save(critical_habitat_table, file = "data/critical_habitat_table.rda")
save(recovery_plan_table, file = "data/recovery_plan_table.rda")
save(add_document_table, file = "data/add_document_table.rda")
save(five_year_review_table, file = "data/five_year_review_table.rda")
save(scraping_table, file = "data/scraping_table.rda")

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

