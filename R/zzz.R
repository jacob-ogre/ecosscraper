# BSD_2_clause

.onLoad <- function(libname, pkgname) {
  options("base_dir" = "~/Downloads")
  options("TE_list" = "http://ecos.fws.gov/tess_public/reports/ad-hoc-species-report?kingdom=V&kingdom=I&kingdom=P&status=E&status=T&status=EmE&status=EmT&status=EXPE&status=EXPN&status=SAE&status=SAT&status=C&status=P&fcrithab=on&fstatus=on&fspecrule=on&finvpop=on&fgroup=on&fleadreg=on&fspcode=on&fmapstatus=on&header=Listed+Species")
  options("ECOS_sp_prefix" = "http://ecos.fws.gov/tess_public/profile/speciesProfile.action?spcode=")
  options("ECOS_prefix" = "http://ecos.fws.gov")
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("
    While `ecosscraper` includes a core dataset, TECP_table, you are **strongly**
    encouraged to run `TECP_table <- get_TECP_table()` to refresh the data.
  ")
  
  options("base_dir" = "~/Downloads")
  options("TE_list" = "http://ecos.fws.gov/tess_public/reports/ad-hoc-species-report?kingdom=V&kingdom=I&kingdom=P&status=E&status=T&status=EmE&status=EmT&status=EXPE&status=EXPN&status=SAE&status=SAT&status=C&status=P&fcrithab=on&fstatus=on&fspecrule=on&finvpop=on&fgroup=on&fleadreg=on&fspcode=on&fmapstatus=on&header=Listed+Species")
  options("ECOS_sp_prefix" = "http://ecos.fws.gov/tess_public/profile/speciesProfile.action?spcode=")
  options("ECOS_prefix" = "http://ecos.fws.gov")
  
  data("TECP_table")
  # packageStartupMessage("
  #   ecosscraper is getting a fresh version of the table of threatened,
  #   endangered, candidate, and proposed species. Please wait a moment...
  # ")
  # if(!httr::http_error(options()$TE_list)) {
  #   page <- xml2::read_html(options()$TE_list)
  #   tabl <- rvest::html_nodes(page, "table")
  #   TECP_table <- as.data.frame(rvest::html_table(tabl))
  #   names(TECP_table) <- gsub(x = names(TECP_table),
  #                          pattern = ".",
  #                          replacement = "_",
  #                          fixed = TRUE)
  #   TECP_table$Species_Page <- paste0(options()$ECOS_sp_prefix,
  #                                  TECP_table$Species_Code)
  #   packageStartupMessage("
  #     Fresh data has been loaded as TECP_table.
  #   ")
  # } else {
  #   packageStartupMessage("
  #     Unable to reach FWS's ECOS page to refresh the table. Loading the built-
  #     in data from November, 2016. You may want to use 
  # 
  #         `TECP_table <- get_TECP_table()` 
  # 
  #     later to try to get a fresh version.
  #   ")
  #   data("TECP_table")
  # }
}
