globalVariables(c(
  ":=", "NO_PRJ", "NO_VIREE", "NO_PE", "TYPE_PE",
  "RESEAU", "LATITUDE", "LONGITUDE", "IN_GPS", "DERN_SOND",
  "NO_MES", "ID_PE_MES", "VERSION", " NO_PRJ_MES", "DATE_SOND",
  "ALTITUDE", "CL_AGE", "CL_HAUT", "CL_DENS", "STADE_DEV", "ORIGINE", "PERTURB",
  "ID_ARBRE", "NO_ARBRE", "ID_ARB_MES", "ETAT", "ESSENCE", "DHP",
  "STADE_DEGR", "ST_HA", "HAUT_ESTI","Species", "DBH", "Height", "TreeNumber","MeasureYear"
))


#' standardize and treat the QUEBEC PSP data

#' @param QuebecPSP list of PSP data.tables obtained via `prepInputsQCPSP`
#' @return a list of standardized plot and tree data.tables
#'
#' @export
#' @importFrom data.table merge setnames
dataPurification_QCPSP <- function(QuebecPSP, codesToExclude = NULL, excludeAllObs = TRUE) {

  # PLACETTE <- import ("D:/Quebec 12 December/RAW_CODE/PLACETTE.txt")
  # PLACETTE_MES <- import ("D:/Quebec 12 December/RAW_CODE/PLACETTE_MES.txt")
  # STATION_PE <- import ("D:/Quebec 12 December/RAW_CODE/STATION_PE.txt")
  # DENDRO_ARBRES_ETUDES <- import("D:/Quebec 12 December/RAW_CODE/DENDRO_ARBRES_ETUDES.txt")
  #
  #
  PLACETTE <- QuebecPSP[["PLACETTE"]]
  PLACETTE_MES <- QuebecPSP[["PLACETTE_MES"]]
  STATION_PE <- QuebecPSP[["STATION_PE"]]
  DENDRO_ARBRES_ETUDES<- QuebecPSP[["DENDRO_ARBRES_ETUDES"]]


  PLACETTE <- PLACETTE[, .(ID_PE, NO_PRJ,NO_PE,RESEAU, LATITUDE,LONGITUDE, IN_GPS,DERN_SOND)]
  PLACETTE_MES[, c("OBJECTID", "DIMENSION", "STATUT_MES","VER_PEP") := NULL]
  PLACETTE_FINAL<-merge(PLACETTE_MES,PLACETTE,by = "ID_PE")
  STATION_PE <- STATION_PE[, .(ID_PE,ID_PE_MES, ALTITUDE,CL_HAUT,CL_AGE,CL_DENS,HAUT_DOMI,DEP_SUR,STADE_DEV, ORIGINE, PERTURB)]
  PLACETTE_FINAL <- merge(STATION_PE, PLACETTE_FINAL, by = c("ID_PE_MES", "ID_PE"))
  #Filtered base on ORIGINE and PERTURB column (BR Means TOTALLY BURNED)
  PLACETTE_FINAL  <- PLACETTE_FINAL[ORIGINE == "" | ORIGINE == "BR"]
  PLACETTE_FINAL  <- PLACETTE_FINAL[PERTURB == "" ]
  PLACETTE_LOCATION <- PLACETTE_FINAL[, .(ID_PE,ID_PE_MES, ALTITUDE, LATITUDE, LONGITUDE)]
  PLACETTE_LOCATION <- unique(PLACETTE_LOCATION, by = c("ID_PE_MES", "ID_PE"))
  #PLACETTE_FINAL  <- PEE_ORI_SOND[, .(ID_PE_MES, CL_AGE)][PLACETTE_FINAL, on = c("ID_PE_MES")]
  ##TREES  File
  # <- DENDRO_ARBRES_ETUDES[!is.na(AGE) | !is.na(AGE_SANSOP)]
  #PLACETTE_AGE <- PLACETTE_AGE[!is.na(DBH)]
  #PLACETTE_AGE <- PLACETTE_AGE[NO_MES == 1 ##& !crown_class %in% c("I", "S"),]

  DENDRO_ARBRES_ETUDES <- DENDRO_ARBRES_ETUDES[, .(ID_PE,ID_ARBRE, NO_MES,ID_PE_MES,NO_ARBRE, ID_ARB_MES,ETAT,ESSENCE,HAUT_ARBRE,
                                                   DHP,CL_QUAL,ETAGE_ARB,AGE,AGE_SANSOP, SOURCE_AGE)]
  #Different VIVANT trees with these codes
  DENDRO_ARBRES_ETUDES  <- DENDRO_ARBRES_ETUDES[ETAT == "10"|ETAT =="12"|ETAT =="40"|ETAT ==""|ETAT =="50"|ETAT =="30"]
  #ADD Measure Year to DENDRO_ARBRES FIL
  DENDRO_ARBRES_ETUDES <- merge(DENDRO_ARBRES_ETUDES, PLACETTE_FINAL[, c("ID_PE_MES", "DERN_SOND")], by = "ID_PE_MES")
  setnames(DENDRO_ARBRES_ETUDES, c("ESSENCE", "DHP", "HAUT_ARBRE", "ID_ARBRE", "DERN_SOND"), c("Species", "DBH", "Height", "TreeNumber","MeasureYear"))
  #DENDRO_ARBRES <- standardizeSpeciesNames(DENDRO_ARBRES, forestInventorySource = "QCPSP")

  DENDRO_ARBRES_ETUDES <- DENDRO_ARBRES_ETUDES[DENDRO_ARBRES_ETUDES$ID_PE %in% unique(PLACETTE_FINAL$ID_PE), ]
  DENDRO_ARBRES_ETUDES$ID_PE <- paste0("QC", DENDRO_ARBRES_ETUDES$ID_PE)
  PLACETTE_FINAL$ID_PE <- paste0("QC", PLACETTE_FINAL$ID_PE)


  return(list(PLOTQC = PLACETTE_FINAL,
              TREESQC = DENDRO_ARBRES_ETUDES))
}
##

#' retrieve the Quebec PSP raw data
#' @param dPath data directory for raw data
#'
#' @return a list of plot, tree, measurement, and location data.tables after exporting mdb to csv txt
#'
#' @export
#' @importFrom reproducible prepInputs
prepInputsQCPSP <- function(dPath) {

  DENDRO_ARBRES <- prepInputs(targetFile = "DENDRO_ARBRES_ETUDES.txt",
                              url = "https://drive.google.com/file/d/1llBYSrlusmwF9que40XWEXAWJ6eck0cu/view?usp=drive_link",
                              fun = "data.table::fread",
                              destinationPath = dPath)

  PLACETTE_MES <- prepInputs(targetFile = "PLACETTE_FINAL.txt",
                             url = "https://drive.google.com/file/d/1iC2utoWxn81kAWLX9R85xh9c2_o3nM11/view?usp=drive_link",
                             destinationPath = dPath,
                             fun = "data.table::fread")

  PLACETTE <- prepInputs(targetFile = "PLACETTE.txt",
                         url = "https://drive.google.com/file/d/1WnB__CJpwQhgkidAXQWKkfZx9hX3Y2Pq/view?usp=drive_link",
                         destinationPath = dPath,
                         fun = "data.table::fread")

  STATION_PE <- prepInputs(targetFile = "STATION.txt",
                           url ="https://drive.google.com/file/d/1vrLb0rkd2cjTNJjDbQCDm2ZkWm1YpDz6/view?usp=drive_link",
                           destinationPath = dPath,
                           fun = "data.table::fread")

  return(list(
    "DENDRO_ARBRES_ETUDES" = DENDRO_ARBRES,
    "PLACETTE_MES" = PLACETTE_MES,
    "PLACETTE" = PLACETTE,
    "STATION_PE" = STATION_PE
  ))
}
