# BSD_2_clause

library(ecosscraper)
TECP <- ecosscraper::get_TECP_table()

devtools::use_data(TECP)