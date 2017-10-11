# BSD_2_clause

.onLoad <- function(libname, pkgname) {
  options("base_dir" = "~/Downloads")
  options("TE_list" = "http://ecos.fws.gov/tess_public/reports/ad-hoc-species-report?kingdom=V&kingdom=I&kingdom=P&status=E&status=T&status=EmE&status=EmT&status=EXPE&status=EXPN&status=SAE&status=SAT&status=C&status=P&fcrithab=on&fstatus=on&fspecrule=on&finvpop=on&fgroup=on&fleadreg=on&fspcode=on&fmapstatus=on&flistingdate=on&header=Listed+Species")
  options("ECOS_sp_prefix" = "http://ecos.fws.gov/tess_public/profile/speciesProfile.action?spcode=")
  options("ECOS_prefix" = "http://ecos.fws.gov")
  options("cons_plans_prefix" = "https://ecos.fws.gov/ecp0/conservationPlan/")
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("
    While `ecosscraper` includes a core dataset, TECP_table, you are **strongly**
    encouraged to run `TECP_table <- get_TECP_table()` to refresh the data or,
    better yet, request the table `tecp_table` from the ESC database.
  ")
}
