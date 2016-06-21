library(ecosscraper)
library(readr)

data(ecos_listed_spp)

# First, get all of the links
# all_links <- gather_links("All")
save(all_links, file = "data-raw/all_links_v1.rda")
dim(all_links)
complete_links <- dplyr::filter(all_links, href != "No page" | is.na(href))

# Next, go back for species that were missed
missed_spp <- dplyr::filter(all_links, href == "No page")
dim(missed_spp)
miss_spp_ls <- missed_spp$Scientific_Name 
missed_recs <- parallel::mclapply(miss_spp_ls,
                                  FUN = get_species_links,
                                  mc.cores = 3)
missed_recs <- dplyr::rbind_all(missed_recs)

# combine the complete and missed dfs
unfilt_links <- rbind(complete_links, missed_recs)
thinned_links <- remove_silly_links(unfilt_links)
links_06Jun2016 <- thinned_links
dim(links_06Jun2016)
devtools::use_data(links_06Jun2016, overwrite = TRUE)

all_5y_links <- get_5yrev_links(links_06Jun2016)
dim(all_5y_links)
save(all_5y_links, file = "data-raw/all_5y_links.rda")

all_recplan_links <- get_recovery_links(links_06Jun2016)
dim(all_recplan_links)
save(all_recplan_links, file = "data-raw/all_recplan_links.rda")

all_fedreg_links <- get_fedreg_links(links_06Jun2016)
dim(all_fedreg_links)
save(all_fedreg_links, file = "data-raw/all_fedreg_links.rda")

all_consplan_links <- get_cons_plan_links(links_06Jun2016)
dim(all_consplan_links)
save(all_consplan_links, file = "data-raw/all_consplan_links.rda")
length(unique(all_consplan_links$link))

################################################################
# Five-year reviews, processed in chunks in case of failure
fiveyear_downloads <- mclapply(all_5y_links$link[1:500], 
                               FUN = get_document, 
                               dtype = "5yr", 
                               mc.cores=3)
fiveyear_downloads1_500 <- dplyr::rbind_all(fiveyear_downloads)

fiveyear_downloads <- mclapply(all_5y_links$link[501:1000], 
                               FUN = get_document, 
                               dtype = "5yr", 
                               mc.cores=3)
fiveyear_downloads501_1000 <- dplyr::rbind_all(fiveyear_downloads)

fiveyear_downloads <- mclapply(all_5y_links$link[1001:1319], 
                               FUN = get_document, 
                               dtype = "5yr", 
                               mc.cores=3)
fiveyear_downloads1001_1319 <- dplyr::rbind_all(fiveyear_downloads)

five_year_downloads <- rbind(fiveyear_downloads1_500,
                             fiveyear_downloads501_1000,
                             fiveyear_downloads1001_1319)
save(five_year_downloads, file = "data-raw/five_year_downloads.rda")

################################################################
# Recovery plans, processed in chunks in case of failure
unique_recplans <- unique(all_recplan_links$link)
recplan_downloads <- mclapply(unique_recplans[1:500], 
                              FUN = get_document, 
                              dtype = "recplan", 
                              mc.cores=3)
recplan_downloads1_500 <- dplyr::rbind_all(recplan_downloads)

recplan_downloads <- mclapply(all_5y_links$link[501:657], 
                              FUN = get_document, 
                              dtype = "5yr", 
                              mc.cores=3)
recplan_downloads501_657 <- dplyr::rbind_all(recplan_downloads)

recovery_plan_downloads <- rbind(recplan_downloads1_500,
                                 recplan_downloads501_657)
save(recovery_plan_downloads, file = "data-raw/recovery_plan_downloads.rda")

################################################################
# Fed Reg notices
FR_downloads <- parallel::mclapply(test_frs, 
                                   FUN = get_document, 
                                   dtype = "FR", 
                                   mc.cores = 2)
FR_downloads <- dplyr::rbind_all(FR_downloads)

################################################################
# Conservation plan tables
# res <- parallel::mclapply(unique(all_consplan_links$link), 
#                           FUN = try(get_cons_plan_tables),
#                           mc.cores = 2)
# res <- dplyr::rbind_all(res)

# all_agreement_suffixes <- get_agreement_links()
# agmt_suff <- all_agreement_suffixes
dim(agmt_suff[agmt_suff$type %in% c("HCP", "SHA"), ])

cur_suff <- unlist(str_match_all(all_consplan_links$link, pattern = "[0-9]+$"))

length(intersect(cur_suff, agmt_suff[agmt_suff$type %in% c("HCP", "SHA"), ]$suffix))
length(setdiff(cur_suff, agmt_suff[agmt_suff$type %in% c("HCP", "SHA"), ]$suffix))
length(setdiff(agmt_suff[agmt_suff$type %in% c("HCP", "SHA"), ]$suffix, cur_suff))

res <- list()
plans <- unique(all_consplan_links$link)
other_links <- paste0("http://ecos.fws.gov/tess_public/conservationPlan/plan?plan_id=",
                      agmt_suff$suffix)
all_links <- union(plans, other_links)
upd_links <- gsub(all_links, pattern = "conserv_plans", replacement = "tess_public")

res <- c()
for(i in 1:length(upd_links)) {
  patt <- str_match(upd_links[i], "[0-9]+$")
  res <- c(res, get_cons_plan_pages(upd_links[i], patt))
}

loc_files <- list.files("~/Downloads/cons_plan_pages/")
loc_paths <- paste0("~/Downloads/cons_plan_pages/", loc_files)

res <- list()
for(i in 1:length(loc_paths)) {
  res[[i]] <- try(get_cons_plan_tables(loc_paths[[i]]), TRUE)
}
length(res)
lens <- lapply(res, FUN = length)
table(unlist(lens))

l30 <- list()
l27 <- list()
l22 <- list()
l4 <- list()
ones <- c()
for(i in 1:length(res)) {
  if(length(res[[i]]) == 30) {
    l30[[length(l30) + 1]] <- res[[i]]
  } else if(length(res[[i]]) == 27) {
    l27[[length(l27) + 1]] <- res[[i]]
  } else if(length(res[[i]]) == 22) {
    l22[[length(l22) + 1]] <- res[[i]]
  } else {
    ones <- c(ones, i)
    l4[[length(l4) + 1]] <- res[[i]]
  }
}
primary <- dplyr::rbind_all(l30)
secondary <- dplyr::rbind_all(l27)
tertiary <- dplyr::rbind_all(l22)
singles <- res[ones]
# ones <- dplyr::rbind_all(l4)

tmp_names <- gsub(x = names(secondary), 
                  pattern = " |\\/",
                  replacement = "_")
tmp_names <- gsub(x = tmp_names, 
                  pattern = "\\(|\\)|,",
                  replacement = "")
names(secondary) <- tmp_names

tmp_names <- gsub(x = names(tertiary), 
                  pattern = " |\\/",
                  replacement = "_")
tmp_names <- gsub(x = tmp_names, 
                  pattern = "\\(|\\)|,",
                  replacement = "")
names(tertiary) <- tmp_names

HCP_SHA <- primary
CCAA <- secondary
CCA <- tertiary
devtools::use_data(HCP_SHA)
devtools::use_data(CCAA)
devtools::use_data(CCA)



# save(primary, file = "data-raw/cons_plans_regular.rda")
# save(secondary, file = "data-raw/cons_plans_small.rda")
# save(tertiary, file = "data-raw/cons_plans_tiny.rda")
# write_tsv(secondary, path = "~/cons_plans_small.tsv")
# write_tsv(tertiary, path = "~/cons_plans_tiny.tsv")

names(primary)
names(secondary)

