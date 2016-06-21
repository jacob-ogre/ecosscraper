# BSD_2_clause

library(rvest)

state_page <- paste0(ECOS_PRE,
                     "/tess_public/reports/species-listed-by-state-totals-report")
states <- read_html(state_page)
st_list <- html_nodes(states, "a")
st_list <- st_list[grep(st_list, pattern = 'state=[A-Z0-9]+')]
st_links <- html_attr(st_list, "href")
st_names <- str_trim(html_text(st_list))
state_links <- data.frame(link = st_links,
                          state = st_names,
                          stringsAsFactors = FALSE)
save(state_links, file = "data-raw/state_links.rda")

