globalVariables(c(
  ":=", "NO_PRJ", "NO_VIREE", "NO_PE", "TYPE_PE",
  "RESEAU", "LATITUDE", "LONGITUDE", "IN_GPS", "DERN_SOND",
  "ETAGE_ARB", "AGE", "baseTreeAge", "group", "baseStandAge",
  "NO_MES", "ID_PE_MES", "VERSION", " NO_PRJ_MES", "DATE_SOND",
  "ALTITUDE", "ORIGINE", "PERTURB",
  "AGE_SANSOP", "CL_QUAL", "HAUT_ARBRE", "ID_PE", "QCPSP", "SOURCE_AGE",
  "ID_ARBRE", "NO_ARBRE", "ID_ARB_MES", "ETAT", "ESSENCE", "DHP",
  "Species", "DBH", "Height", "TreeNumber","MeasureYear"
))


#' standardize and treat the QUEBEC PSP data

#' @param QuebecPSP list of PSP data.tables obtained via `prepInputsQCPSP`
#' @param codesToExclude TODO: eventually add codes for pest disturbance if applicable
#' @param excludeAllObs assuming codesToExclude is not NULL, exclude these obs or prior ones too
#' @param sppEquiv table of species names - see `LandR::sppEquiv`
#' @return a list of standardized plot and tree data.tables
#'
#' @export
#' @importFrom data.table setnames
#' @importFrom bit64 as.integer64
dataPurification_QCPSP <- function(QuebecPSP, codesToExclude = NULL, excludeAllObs = TRUE,
                                   sppEquiv = LandR::sppEquivalencies_CA) {
 #DENDRO_ARBRES_ETUDES is a subset of DENDRO_ARBRES with additional information (e.g. age, height)
  PLACETTE <- QuebecPSP[["PLACETTE"]]
  PLACETTE_MES <- QuebecPSP[["PLACETTE_MES"]]
  STATION_PE <- QuebecPSP[["STATION_PE"]]
  DENDRO_ARBRES_ETUDES<- QuebecPSP[["DENDRO_ARBRES_ETUDES"]]
  trees <- QuebecPSP[["DENDRO_ARBRES"]]

  PLACETTE <- PLACETTE[, .(ID_PE,NO_PE, LATITUDE,LONGITUDE)]
  PLACETTE[, ID_PE := as.integer64(ID_PE)] #it is probably already 64bit
  #because this loads by default as long as bit64 is loaded
  #but this guarantees the loading
  STATION_PE[, ID_PE := as.integer64(ID_PE)]
  PLACETTE_MES[, ID_PE := as.integer64(ID_PE)]
  DENDRO_ARBRES_ETUDES[, ID_PE := as.integer64(ID_PE)]
  trees[, ID_PE := as.integer64(ID_PE)]

  #keep survey date, plot id and measure ID
  PLACETTE_MES <- PLACETTE_MES[, .(ID_PE, NO_MES, ID_PE_MES, DATE_SOND)]

  PLACETTE_FINAL <- PLACETTE_MES[PLACETTE, on = "ID_PE"]

  STATION_PE <- STATION_PE[, .(ID_PE,ID_PE_MES, ALTITUDE, ORIGINE, PERTURB)]
  PLACETTE_FINAL <- STATION_PE[PLACETTE_FINAL, on = c("ID_PE_MES", "ID_PE")]

  #Miscellaneous clean up
  #inconsistent tree numbers across all measures, or too many NAs.
  #or trees were all removed for some reason, but they came back
  #this could do with further analysis.
  badPlots <- c("7501108401", "7000408802", "7100400602","7409701402",
                "7101002302", "7102200101", "7102200302", "7209904202",
                "7409702801", "7501404801", "7506900302", "7506900601",
                "7608604902", "7608804202", "7409701702", "7601203202")
  badMeasure <- c("750110930101") #untrustworthy tree numbers in first

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

  #management is recorded for specific measurements but all plot measurements in a given plot must be dropped
  mauvaisPlots  <- PLACETTE_FINAL[!PERTURB %in% c("", "BRP", "CHP", "CHT", "CH", "EL","DP", "DLD", "VEP")]$ID_PE
  PLACETTE_FINAL <- PLACETTE_FINAL[!ID_PE %in% mauvaisPlots]

  #there are other categories of natural disturbance but they are not present in the PEP at this point
  #(i.e. they are in the temporary plots or have been previously filtered out)
  mauvaisPlots <- PLACETTE_FINAL[!ORIGINE %in% c("", "BR", "CHT", "DT", "ES")]$ID_PE
  PLACETTE_FINAL <- PLACETTE_FINAL[!ID_PE %in% mauvaisPlots]

  PLACETTE_FINAL[, date := as.Date(DATE_SOND, format = "%m/%d/%Y")]
  PLACETTE_FINAL[, MeasureYear := as.integer(format(date, "%Y"))]
  PLACETTE_FINAL[, c("date", "DATE_SOND") := NULL]
  PLACETTE_FINAL[, baseYear := min(MeasureYear), .(ID_PE)]
  #assume area of 400m from radius of 11.28 m
  DENDRO_ARBRES_ETUDES <- DENDRO_ARBRES_ETUDES[, .(ID_PE,ID_ARBRE, NO_MES,ID_PE_MES, NO_ARBRE,
                                                   ID_ARB_MES, ETAT,ESSENCE, HAUT_ARBRE, AGE_SANSOP,
                                                   DHP, CL_QUAL, ETAGE_ARB, AGE, SOURCE_AGE)]
  #Filter to living (VIVANT) trees
  #By definition the recruits are younger than the stand (even though they may be classified as codominant)
  #10 = living, 12 = living snag, 40 = living recruit (Google Translate), 30 = living/forgotten (?)
  DENDRO_ARBRES_ETUDES  <- DENDRO_ARBRES_ETUDES[ETAT %in% c(10, 12, 30, 40, 50),]
  #not 100% confident about GA
  #allow living recruit blowdown (42) and living sapling ("GV") - unsure what GA - "abandoned sapling?" - means
  trees <- trees[ETAT %in% c("10", "12", "30", "40", "50", "GV", "42")]

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
        #(incomplete core, analyzed in lab, with more than five missing tree rings)
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
  #see example ID_PE == 7100808502  for why source age 7 is allowed.
  standAge <- DENDRO_ARBRES_ETUDES[!is.na(AGE) & ETAT != 40]  #drop recruits
  standAge <- standAge[!is.na(SOURCE_AGE) & SOURCE_AGE %in% c(4, 3, 7),
                       .(ID_PE_MES, NO_ARBRE, ID_ARBRE, ID_ARB_MES, AGE,
                         ETAGE_ARB, SOURCE_AGE)] #don't need species or state
  standAge <- PLACETTE_FINAL[, .(ID_PE, ID_PE_MES, MeasureYear, baseYear)][standAge, on = c("ID_PE_MES")]
  #drop plots that were already filtered out
  #some plots will be missing in join as they were removed for other reasons

  standAge <- standAge[!is.na(baseYear) & ETAGE_ARB %in% c("C", "D")]

  #for now we will only use plots with age measured in baseYear. This removes some ~300 usable plots
  #TODO: revisit this decision later
  standAge <- standAge[MeasureYear == baseYear]
  #note the code below has no effect while the line above is run
  standAge[, baseTreeAge := AGE - MeasureYear + baseYear] #some plots have different trees measured at different times
  standAge <- standAge[, .N, .(baseTreeAge, ID_PE, ETAGE_ARB)]
  #join with measure year to get year age was recorded - estimate revised age - find etage
  # the PSPclean approach derives standAge as mean of N trees with crown class Dominant, where N > 1.
  # if none are available, we take the mean of all dominant and co-dominant trees
  standRep <- standAge[, .N, .(ETAGE_ARB, ID_PE)]

  standRep[ETAGE_ARB == "D" & N > 1, group := "D"]
  standRep[is.na(group), group := "CandD"]

  #TODO: if we allow non-base year measurements, we want unique tree ages
  # so we aren't double-counting trees with mean - or alternatively, use max
  standAge[ID_PE %in% standRep[group == "D",]$ID_PE & ETAGE_ARB == "D",
           baseStandAge := as.integer(mean(baseTreeAge)), .(ID_PE)]
  standAge[ID_PE %in% standRep[group == "CandD",]$ID_PE,
           baseStandAge := as.integer(mean(baseTreeAge)), .(ID_PE)]
  # standAge[ID_PE %in% standRep[group == "CandD",]$ID_PE,
  #          standAge_wmax := as.integer(max(baseTreeAge)), .(ID_PE)]
  # standAge[ID_PE %in% standRep[group == "CandD",]$ID_PE,
  #          standAge_wmax := as.integer(max(baseTreeAge)), .(ID_PE)]

  standAge <- standAge[!is.na(baseStandAge)] #drop the C trees in plots with > 1 D trees
  standAge <- unique(standAge[, .(baseStandAge, ID_PE)])
  PLACETTE_FINAL <- standAge[PLACETTE_FINAL, on = c("ID_PE")]
  PLACETTE_FINAL <- PLACETTE_FINAL[!is.na(baseStandAge)]

  #standardize species
  sppEquiv <- unique(sppEquiv[, .(QCPSP, PSP)])#unique because some species have multiple rows (e.g. due to common name)
  setnames(sppEquiv, old = c("QCPSP", "PSP"), new = c("ESSENCE", "newSpeciesName"))
  # DENDRO_ARBRES_ETUDES <- sppEquiv[DENDRO_ARBRES_ETUDES, on = c("ESSENCE")]
  trees <- sppEquiv[trees, on = c("ESSENCE")]
  #standardize attribute names
  PLACETTE_FINAL <- PLACETTE_FINAL[, .(ID_PE, ID_PE_MES, MeasureYear, ALTITUDE,
                                       LATITUDE, LONGITUDE, baseStandAge, baseYear)]
  #per documentation, all PEP (placette echantillon permanente) are 400m2
  #there does not appear to be an area field that explicitly defines the plot area
  #see PLAN_DESC_TYPE_PE
  PLACETTE_FINAL$PlotSize <- 0.04
  #standardize height and dbh
  height <- DENDRO_ARBRES_ETUDES[, .(ID_PE_MES, NO_ARBRE, HAUT_ARBRE)]
  trees <- height[trees, on = c("ID_PE_MES" = "ID_PE_MES", "NO_ARBRE" = "NO_ARBRE")]
  trees[, DHP := DHP/10] #from millimetre to centimetre
  trees[, HAUT_ARBRE := HAUT_ARBRE/10] #from decimetre to metre

  #ensure all measurements have associated plots
  trees <- trees[PLACETTE_FINAL[, .(ID_PE_MES, MeasureYear)], on = "ID_PE_MES"]
  trees <- trees[, .(ID_PE, ID_PE_MES, NO_ARBRE, MeasureYear,
                     ESSENCE, DHP, HAUT_ARBRE, newSpeciesName)]
  setnames(trees,
           old = c("ESSENCE", "DHP", "HAUT_ARBRE", "NO_ARBRE", "ID_PE", "ID_PE_MES"),
           new = c("Species", "DBH", "Height", "TreeNumber", "OrigPlotID1", "MeasureID"))

  setnames(PLACETTE_FINAL,
           old = c("ID_PE", "ID_PE_MES", "ALTITUDE", "baseStandAge", "LATITUDE", "LONGITUDE"),
           new = c("OrigPlotID1", "MeasureID", "Elevation", "baseSA", "Latitude", "Longitude"))

  trees[, c("MeasureID", "OrigPlotID1") := .(paste0("QCPSP_", MeasureID),
                                             paste0("QCPSP_", OrigPlotID1))]
  PLACETTE_FINAL[, c("MeasureID", "OrigPlotID1") := .(paste0("QCPSP_", MeasureID),
                                                      paste0("QCPSP_", OrigPlotID1))]

  #some plots do not have trees remaining
  PLACETTE_FINAL <- PLACETTE_FINAL[MeasureID %in% trees$MeasureID]


  setkey(trees, MeasureID, OrigPlotID1, MeasureYear,
         TreeNumber, Species, DBH, Height, newSpeciesName)
  setcolorder(trees)

  setkey(PLACETTE_FINAL, OrigPlotID1, MeasureID, MeasureYear)
  setcolorder(PLACETTE_FINAL)

  PLACETTE_FINAL[, source := "QC"]
  trees[, source := "QC"]


  return(list(plotHeaderData = PLACETTE_FINAL,
              treeData = trees))
}

#' retrieve the Quebec PSP raw data
#' @param dPath data directory for raw data
#'
#' @return a list of plot, tree, measurement, and location data.tables after exporting mdb to csv txt
#'
#' @export
#' @importFrom reproducible prepInputs
prepInputsQCPSP <- function(dPath) {

  DENDRO_ARBRES_ETUDES <- prepInputs(targetFile = "DENDRO_ARBRES_ETUDES.txt",
                                     url = "https://drive.google.com/file/d/1llBYSrlusmwF9que40XWEXAWJ6eck0cu/view?usp=drive_link",
                                     fun = "data.table::fread",
                                     overwrite = TRUE,
                                     destinationPath = dPath)

  PLACETTE_MES <- prepInputs(targetFile = "PLACETTE_MES.txt",
                             url = "https://drive.google.com/file/d/1iC2utoWxn81kAWLX9R85xh9c2_o3nM11/view?usp=drive_link",
                             destinationPath = dPath,
                             overwrite = TRUE,
                             fun = "data.table::fread")

  PLACETTE <- prepInputs(targetFile = "PLACETTE.txt",
                         url = "https://drive.google.com/file/d/1WnB__CJpwQhgkidAXQWKkfZx9hX3Y2Pq/view?usp=drive_link",
                         destinationPath = dPath,
                         overwrite = TRUE,
                         fun = "data.table::fread")

  STATION_PE <- prepInputs(targetFile = "STATION.txt",
                           url ="https://drive.google.com/file/d/1vrLb0rkd2cjTNJjDbQCDm2ZkWm1YpDz6/view?usp=drive_link",
                           destinationPath = dPath,
                           overwrite = TRUE,
                           fun = "data.table::fread")

  DENDRO_ARBRES <- prepInputs(targetFile = "DENDRO_ARBRES.txt",
                              url = "https://drive.google.com/file/d/1A0hTWZDQwI0262aP8qFHeW7g6QsHgGsq/view?usp=drive_link",
                              fun = "data.table::fread",
                              overwrite = TRUE,
                              destinationPath = dPath)

  return(list(
    "DENDRO_ARBRES_ETUDES" = DENDRO_ARBRES_ETUDES,
    "PLACETTE_MES" = PLACETTE_MES,
    "PLACETTE" = PLACETTE,
    "STATION_PE" = STATION_PE,
    "DENDRO_ARBRES" = DENDRO_ARBRES
  ))
}
