# BSD_2_clause
#
# Brief script to pull FR docs and recovery plans from ECOS; also an example
# of how to use the ecosscraper package (though that is incomplete at this time)

library(ecosscraper)
library(doParallel)
library(foreach)

fr_links <- get_fedreg_links(all_links)
recplans <- get_recovery_links(all_links)
fiveyrev <- get_5yrev_links(all_links)

tmp <- head(recplans$link)
cl <- makeCluster(7)
doParallel::registerDoParallel(cl)

tmp <- unique(recplans$link)
res <- foreach(i = tmp, 
               .packages = c("dplyr", "rvest", "httr", "ecosscraper"), 
               .errorhandling = "pass", 
               .verbose = TRUE) %dopar% {
                 get_document(i, 
                              subd = "/datadrive/data/NLP/recovery_plan") 
               }
recplan_res <- dplyr::rbind_all(res)

tmp <- unique(fr_links$link)
res <- foreach(i = tmp, 
               .packages = c("dplyr", "rvest", "httr", "ecosscraper"), 
               .errorhandling = "pass", 
               .verbose = TRUE) %dopar% {
                 get_document(i, 
                              subd = "/datadrive/data/NLP/federal_register") 
               }
recplan_res <- dplyr::rbind_all(res)
