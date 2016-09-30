options("base_dir" = "~/Downloads")
options("TE_list" = "http://ecos.fws.gov/tess_public/reports/ad-hoc-species-report?kingdom=V&kingdom=I&kingdom=P&status=E&status=T&status=EmE&status=EmT&status=EXPE&status=EXPN&status=SAE&status=SAT&status=C&status=P&fcrithab=on&fstatus=on&fspecrule=on&finvpop=on&fgroup=on&fleadreg=on&fspcode=on&header=Listed+Species")
options("ECOS_sp_prefix" = "http://ecos.fws.gov/tess_public/profile/speciesProfile.action?spcode=")
options("ECOS_prefix" = "http://ecos.fws.gov")

set_TE_list_opt <- function() {
  options("TE_list" = "http://ecos.fws.gov/tess_public/reports/ad-hoc-species-report?kingdom=V&kingdom=I&kingdom=P&status=E&status=T&status=EmE&status=EmT&status=EXPE&status=EXPN&status=SAE&status=SAT&status=C&status=P&fcrithab=on&fstatus=on&fspecrule=on&finvpop=on&fgroup=on&fleadreg=on&fspcode=on&header=Listed+Species")
}

#' Set the base directory for ecosscraper downloads.
#'
#' Creates the directory, recursively, if it doesn't exist.
#'
#' @param path Path to base directory for downloads from ECOS
#' @return Nothing
#' @export
#' @examples
#' set_base_dir(path = "~/Downloads/test5")
set_base_dir <- function(path) {
  if(!dir.exists(path)) {
    dir.create(path)
    cat(paste0("Created base_dir ", path, "\n"))
  }
  options(base_dir = path)
}
