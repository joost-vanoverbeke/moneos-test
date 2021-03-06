

# pad pc joost
# pad_prj_schelde <- "G:/Mijn Drive/INBODATA/PROJECTEN/PRJ_SCHELDE/"
# pad citrix joost
# basis_pad <- "//Client/G$/Mijn Drive/INBODATA/PROJECTEN/PRJ_SCHELDE/"

# pad pc Gunther
pad_prj_schelde <- "G:/Mijn Drive/PRJ_SCHELDE/"

pad_moneos <- "VNSC/Rapportage_INBO/2020/"

maak_pad <- function(hoofdstuk, onderdeel = NULL) {
  pad <- paste0(pad_prj_schelde, pad_moneos, hoofdstuk, "/")
  if (!is.null(onderdeel))
    pad <- paste0(pad, onderdeel, "/")
  if (!dir.exists(pad))
    dir.create(pad, recursive = TRUE)
  return(pad)
}

