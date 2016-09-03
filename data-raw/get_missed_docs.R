# BSD_2_clause

library(dplyr)
library(ecosscraper)
library(stringr)
library(doParallel)
library(foreach)

#############################################################################
# First, the recovery plans:
recplan_res <- readr::read_tsv("data-raw/missing_recplan_pdfs.tsv")
head(recplan_res)

miss_rp_df <- filter(recplan_res, status == "NOT_PDF")
head(miss_rp_df)
miss_rp_spl <- str_split(miss_rp_df$file, pattern = "/")
miss_rp <- unlist(lapply(miss_rp_spl, `[`, 2))
miss_rp_df$filename <- miss_rp

load("data-raw/all_recplan_links.rda")
head(data.frame(all_recplan_links))
all_recplan_links$dups <- duplicated(all_recplan_links$href)
all_recplan_links <- filter(all_recplan_links, dups == FALSE)
filespl <- str_split(all_recplan_links$href, pattern = "/")
filenames <- unlist(lapply(filespl, `[`, 4))
all_recplan_links$filename <- filenames

missing_rp_search <- left_join(miss_rp_df, all_recplan_links, by = "filename")
dim(missing_rp_search)

save(missing_rp_search, file = "data-raw/missing_rp_search.rda")
head(data.frame(missing_rp_search))

# Now do the actual download
cl <- makeCluster(2)
doParallel::registerDoParallel(cl)
tmp <- unique(missing_rp_search$link)
res <- foreach(i = tmp, 
               .packages = c("dplyr", "rvest", "httr", "ecosscraper"), 
               .errorhandling = "pass", 
               .verbose = TRUE) %dopar% {
                 get_document(i, 
                              subd = "/datadrive/data/NLP/recovery_plan") 
               }
recplan_res <- dplyr::bind_rows(res)
save(recplan_res, file = "gathered_missing_recplans.rda")


