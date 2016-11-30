library(ecosscraper)

TECP_table <- get_TECP_table()
save(TECP_table, file = "~/Downloads/TECP_table.rda")
