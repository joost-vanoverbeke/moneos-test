
# pad pc joost
# basis_pad <- "G:/Mijn Drive/INBODATA/PROJECTEN/PRJ_SCHELDE/VNSC/Rapportage_INBO/2020/"

# pad citrix joost
basis_pad <- "//Client/G$/Mijn Drive/INBODATA/PROJECTEN/PRJ_SCHELDE/VNSC/Rapportage_INBO/2020/"


  
maak_pad <- function(hoofdstuk, onderdeel = NULL) {
  pad <- paste0(basis_pad, hoofdstuk, "/")
  if (!is.null(onderdeel))
    pad <- paste0(pad, onderdeel, "/")
  return(pad)
}

