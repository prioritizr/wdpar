# set default availability for chrome
is_chrome_installed <- FALSE
if (interactive() && identical(Sys.getenv("NOT_CRAN"), "true")) {
  is_chrome_installed <- TRUE
}

# define expected column names
wdpa_column_names <- c(
  "SITE_ID", "SITE_PID", "SITE_TYPE", "NAME_ENG", "NAME", "DESIG",
  "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT", "INT_CRIT", "REALM", "REP_M_AREA",
  "REP_AREA", "NO_TAKE", "NO_TK_AREA",
  "STATUS", "STATUS_YR", "GOV_TYPE", "GOVSUBTYPE", "OWN_TYPE",
  "OWNSUBTYPE", "MANG_AUTH", "MANG_PLAN", "VERIF", "METADATAID",
  "PRNT_ISO3", "ISO3", "SUPP_INFO", "CONS_OBJ", "INLND_WTRS", "OECM_ASMT",
  "geometry"
)

# define default values
default_retain_status <- c("Designated", "Inscribed", "Established")
