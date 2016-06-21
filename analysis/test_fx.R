# BSD_2_clause

library(ecosscraper)
TECP <- ecosscraper::get_TECP_table()
dim(TECP)
head(TECP)

# First a test of single-species grab
spp_test <- get_species_links(TECP, TECP$Scientific_Name[1])
head(spp_test)

# Next a test of multi-species grab
spp_test <- lapply(TECP$Scientific_Name[1:5], 
                   FUN = get_species_links,
                   df = TECP)
spp_test <- dplyr::rbind_all(spp_test)
spp_t2 <- remove_silly_links(spp_test)

# Next, test the link subset functions
fiveyr <- get_5yrev_links(spp_t2)
head(fiveyr)

recov <- get_recovery_links(spp_t2)
head(recov)

fr <- get_fedreg_links(spp_t2)
head(fr)
