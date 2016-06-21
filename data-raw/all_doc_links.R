# BSD_2_clause

library(ecosscraper)
data(TECP)

all_links <- lapply(unique(TECP$Scientific_Name),
                    FUN = get_species_links,
                    df = TECP)
all_links <- remove_silly_links(all_links)