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
#' @importFrom data.table setnames
dataPurification_QCPSP <- function(QuebecPSP, codesToExclude = NULL, excludeAllObs = TRUE) {

  PLACETTE <- QuebecPSP[["PLACETTE"]]
  PLACETTE_MES <- QuebecPSP[["PLACETTE_MES"]]
  STATION_PE <- QuebecPSP[["STATION_PE"]]
  DENDRO_ARBRES_ETUDES<- QuebecPSP[["DENDRO_ARBRES_ETUDES"]]


  PLACETTE <- PLACETTE[, .(ID_PE,NO_PE, LATITUDE,LONGITUDE)]
  #keep survey date, plot id and measure ID
  PLACETTE_MES <- PLACETTE_MES[, .(ID_PE, NO_MES, ID_PE_MES, DATE_SOND)]

  PLACETTE_FINAL <- merge(PLACETTE_MES,PLACETTE,by = "ID_PE")

  STATION_PE <- STATION_PE[, .(ID_PE,ID_PE_MES, ALTITUDE, ORIGINE, PERTURB)]
  PLACETTE_FINAL <- merge(STATION_PE, PLACETTE_FINAL, by = c("ID_PE_MES", "ID_PE"))

  #Filtered base on ORIGINE and PERTURB column
  #ORIGINE is stand-originating disturbance, PERTURB is partial disturbance
  #CP/CB/CD CDL/CE/CJ/DLD/EPC all involve management activities
  #CHP/CHT/CH are partial, snag, and full chablis (blowdown or windthrow),
  #EL = épidémie légère (mild epidemic)
  #BRP = partial burn, VEP = partial ice (verglas)
  #DP partial die-back, DLD partial die-back of hardwood
  #BR total burn, CBA cut, CBT/CBA strip cut, CPR cutting with regen,
  #CT clear cut, DT total die-back, ENS seeding, ES epidemic, FR abandoned (wasteland?),
  #P plantation, PLN bare root planting, PLR planting with container seedlings
  PLACETTE_FINAL  <- PLACETTE_FINAL[PERTURB %in% c("", "BRP", "CHP", "CHT", "CH", "EL","DP", "DLD", "VEP")]

  #there are other categories of natural disturbance but they are not present in the PEP at this point
  #(i.e. they are in the temporary plots or have been previously filtered out)
  PLACETTE_FINAL  <- PLACETTE_FINAL[ORIGINE %in% c("", "BR", "CHT", "DT", "ES")]


  PLACETTE_FINAL[, date := as.Date(DATE_SOND, format = "%m/%d/%Y")]
  PLACETTE_FINAL[, MeasureYear := as.integer(format(date, "%Y"))]
  PLACETTE_FINAL[, c("date", "DATE_SOND") := NULL]
  PLACETTE_FINAL[, baseYear := min(MeasureYear), .(ID_PE)]
  #assume area of 400m from radius of 11.28 m

  DENDRO_ARBRES_ETUDES <- DENDRO_ARBRES_ETUDES[, .(ID_PE,ID_ARBRE, NO_MES,ID_PE_MES,NO_ARBRE,
                                                   ID_ARB_MES,ETAT,ESSENCE,HAUT_ARBRE,
                                                   DHP,CL_QUAL,ETAGE_ARB,AGE, SOURCE_AGE)]
  #Filter to living (VIVANT) trees with these codes
  #10 = living, 12 = living snag, 40 = living recruit (Google Translate), 30 = living/forgotten (?)
  DENDRO_ARBRES_ETUDES  <- DENDRO_ARBRES_ETUDES[ETAT == "10"|ETAT =="12"|ETAT =="40"|ETAT ==""|ETAT =="50"|ETAT =="30"]

  StandAge <- DENDRO_ARBRES_ETUDES[!is.na(AGE) | !is.na(AGE_SANSOP)]
  #use STAND_AGE$ETAGE_ARB to estimate stand age from trees that are codominant or dominant
  #C Codominant, D Dominant, F = NonApplicable car fut casse (no Fs..)
  # I = Intermediaire, O = Opprime (oppressed or suppressed), V = Veteran
  #Stand age formula is to take the mean of dominant if N dominant > 1
  #else take dominant plus co-dominant
  #we do not estimate stand age if individual ages derived from intermediate, suppressed, or veteran trees.
  # lab estimates are prioritized over field estimates, complete over incomplete,

  # English translation from Google translate/Ian Eddy and should be regarded very skeptically
  # 1	Carotte complète pour analyse en laboratoire. (complete core to be analyzed in lab)
  # 10	Carotte incomplète sans moëlle analysée en forêt.(incomplete core, without pith, analyzed in field)
  # 11	Carotte incomplète pour analyse en laboratoire.(incomplete core for lab analysis)
  # 12	Carotte incomplète analysée en forêt. (incomplete core analyzed in field)
  # 13	Carotte incomplète analysée en laboratoire avec plus de cinq cernes absents
        #(incomplete core, analyzed in lab, with more than five missign tree rings)
  # 14	Carotte non analysée (unanalyzed core)
  # 2	Carotte en attente d'une analyse.(core awaiting analysis)
  # 3	Carotte complète analysée en forêt.(complete core analyzed in field)
  # 4	Carotte complète analysée en laboratoire. (complete core analyzed in lab)
  # 5	Âge déduit de la carotte cariée.(age deduced from decayed core)
  # 6	Carotte incomplète analysée en laboratoire avec un à cinq cernes absents.
  #   (incomplete core analyzed in lab wtih more than five tree rings missing)
  # 7	Mise à jour de l'âge avec l'inter mesure.(age update with inter measurement)
  # 8	Carotte cariée. (decayed core)
  # 9	Carotte absente. (missing core)

  #remove NA age, trees without cores or with rotten cores, and trees where age is inferred from prior measurement
  StandAge <- StandAge[!is.na(SOURCE_AGE) & SOURCE_AGE %in% c(4, 3, )]



  #ADD Measure Year to DENDRO_ARBRES FIL
  DENDRO_ARBRES_ETUDES <- merge(DENDRO_ARBRES_ETUDES, PLACETTE_FINAL[, c("ID_PE_MES", "DERN_SOND")], by = "ID_PE_MES")
  setnames(DENDRO_ARBRES_ETUDES, c("ESSENCE", "DHP", "HAUT_ARBRE", "ID_ARBRE", "DERN_SOND"),
           c("Species", "DBH", "Height", "TreeNumber","MeasureYear"))
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

  PLACETTE_MES <- prepInputs(targetFile = "PLACETTE_MES.txt",
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

# #(forestInventorySource == "QCPSP") {
# #### QUEBEC ####
# # speciesTable[Species == "  ", newSpeciesName := "unknown"]
# speciesTable[Species == "ERS", newSpeciesName := "sugar maple"]
# speciesTable[Species == "ERR", newSpeciesName := "red maple"]
# speciesTable[Species == "OSV", newSpeciesName := "hop-hornbeam"]
# speciesTable[Species == "BOJ", newSpeciesName := "yellow birch"]
# speciesTable[Species == "HEG", newSpeciesName := "beech"]
# speciesTable[Species == "TIL", newSpeciesName := "trembling aspen"]
# speciesTable[Species == "FRA", newSpeciesName := "white ash"]
# speciesTable[Species == "PRU", newSpeciesName := "eastern hemlock"]
# speciesTable[Species == "EPB", newSpeciesName := "white spruce"]
# speciesTable[Species == "EPR", newSpeciesName := "red spruce"]
# speciesTable[Species == "SAB", newSpeciesName := "balsam fir"]
# speciesTable[Species == "EPN", newSpeciesName := "black spruce"]
# speciesTable[Species == "BOP", newSpeciesName := "alaska paper birch"]
# speciesTable[Species == "PET", newSpeciesName := "trembling aspen"]
# speciesTable[Species == "MEL", newSpeciesName := "tamarack larch"]
# speciesTable[Species == "THO", newSpeciesName := "eastern white cedar"]
# speciesTable[Species == "PIB", newSpeciesName := "white pine"]
# speciesTable[Species == "ORA", newSpeciesName := " "]
# speciesTable[Species == "CET", newSpeciesName := "black cherry"]
# speciesTable[Species == "FRN", newSpeciesName := "black ash"]
# speciesTable[Species == "CHG", newSpeciesName := "bur oak"]
# speciesTable[Species == "PIG", newSpeciesName := "jack pine"]
# speciesTable[Species == "BOG", newSpeciesName := "grey birch"]
# speciesTable[Species == "ERA", newSpeciesName := "silver maple"]
# speciesTable[Species == "FRP", newSpeciesName := "green ash"]
# speciesTable[Species == "PEB", newSpeciesName := "balsam poplar"]
# speciesTable[Species == "PEG", newSpeciesName := "lagretooth aspen"]
# speciesTable[Species == "PIR", newSpeciesName := "red pine"]
# speciesTable[Species == "CHR", newSpeciesName := "red oak"]
# speciesTable[Species == "EPO", newSpeciesName := ""]
# speciesTable[Species == "CHE", newSpeciesName := "white oak"]
# speciesTable[Species == "PED", newSpeciesName := "eastern cottonwood"]
# speciesTable[Species == "ORT", newSpeciesName := ""]
# speciesTable[Species == "AUR", newSpeciesName := ""]
# speciesTable[Species == "NOC", newSpeciesName := ""]
# speciesTable[Species == "ERG", newSpeciesName := ""]
# speciesTable[Species == "CHB", newSpeciesName := "white oak"]
# speciesTable[Species == "ORR", newSpeciesName := ""]
# speciesTable[Species == "PIS", newSpeciesName := "scots pine"]
# speciesTable[Species == "SOA", newSpeciesName := ""]
# speciesTable[Species == "PRP", newSpeciesName := ""]
# speciesTable[Species == "CAF", newSpeciesName := ""]
# speciesTable[Species == "PID", newSpeciesName := ""]
# speciesTable[Species == "ERN", newSpeciesName := ""]
# speciesTable[Species == ""   , newSpeciesName := "unknown"]
# }
# #assuming that American ash is white ash(FRA)
# #aasuming that HEG	Hêtre à grandes feuilles is beech(HEG)
#
# #assuming Chêne bicolore is white oak(CHV)

# #assuming Ostryer de Virginie is hop-hornbeam(OSV)
# #Orme d'Amérique (ORA) =do not know the new name
# #Érable noir (ERN) =do not know the new name
# #Sorbier d'Amérique(SOA)=do not know the new name
# #Orme de Thomas (ORT)=do not know the new name
# #Pin rigide (PID)=do not know the new name
# #Caryer à fruits doux(CAF)=do not know the new name
# #Cerisier de Pennsylvanie(PRP)=do not know the new name
# #Orme rouge(ORR)=  slipper ehlm
# #Érable à Giguère(ERG)=do not know the new name Manitoba maple
# #Noyer cendré(NOC) = White walnut, butternut, juglas cinerea
# #Aulne rugueu(AUR)=do not know the new name
# #Épinette de Norvège(EPO)=do not know the new name
# #assuming that Frêne de Pennsylvanie is green ash(FRP)
# #assuming that Cerisier tardif is black cherry(CET)
