globalVariables(c(
  ":=", "PlotType","Treatment","Plot", "dbh", "EstabDate", "EstabAge", "EstabYear",
  "Status","species","TreeNumber","SpeciesCode","Species","MeasNum","RemeasID",
  "LatinName", "CommonName", "PLOT", "lat", "long_", "MeasYr", "SilvID",
  "OrigPlotID1","MeasureYear","PlotSize","baseSA","LATITUDE","LONGITUDE","treenum"
))

#' standardize and treat the New Brunswick PSP data
#'
#' @param NB_PSP_Data list of data tables resulting from `prepInputsNBPSP`
#' @param sppEquiv species equivalencies table with column `Latin-full`
#'
#' @return a list of standardized plot and tree data.tables
#'
#' @export
#' @importFrom data.table set setcolorder
dataPurification_NBPSP <- function(NB_PSP_Data, sppEquiv = LandR::sppEquivalencies_CA) {

  PSP_PLOTS <- NB_PSP_Data[["PSP_PLOTS"]]
  PSP_PLOTS_YR <- NB_PSP_Data[["PSP_PLOTS_YR"]]
  PSP_TREE_YIMO <- NB_PSP_Data[["PSP_TREE_YIMO"]]
  PSP_LOC_LAT_LONG <- NB_PSP_Data[["PSP_LOC_LAT_LONG"]]

  #start by taking the YIMO
  #these are the young immature mature and overmature plots
  PSP_PLOTS <- PSP_PLOTS[PlotType == "M",]
  PSP_PLOTS <- PSP_PLOTS[SilvID == 0]

  #get rid of bad plot - trees were numbered differently in 4th measure of 5 (many trees in common in 3 and 5)
  PSP_PLOTS_YR <- PSP_PLOTS_YR[!RemeasID %in% "10405_4",]
  PSP_TREE_YIMO <- PSP_TREE_YIMO[!RemeasID %in% "10405_4"]

  #this rejects most of the data, unfortunately
  PSP_PLOTS <- PSP_PLOTS[YearTreated == 0]
  #no dead trees
  PSP_TREE_YIMO <- PSP_TREE_YIMO[!cause %in% 1:9]
  # has age

  #generate eventual plot header
  PSP_PLOTS_YR <- PSP_PLOTS_YR[Plot %in% PSP_PLOTS$Plot, .(Plot, RemeasID, MeasYr, measNum)]

  #standardize
  PSP_TREE_YIMO[, DBH := dbh/10] #dbh is measured in mm - convert to cm
  PSP_PLOTS[, PlotSize := PlotSize/10000] # convert square metres to hectares

  PSP_TREE_YIMO <- PSP_TREE_YIMO[, .(RemeasID, treenum, species, DBH, Plot, MeasNum)]
  # internal standardization of DBH (min DBH was 5.1 cm except for plots established in 1987,
  # or for alder and mountain maple)
  #to simplify, remove all trees under 5.1 cm DBH
  PSP_TREE_YIMO <- PSP_TREE_YIMO[DBH > 5.0]

  #join with measurement year
  PSP_PLOTS <- PSP_PLOTS[, .(Plot, EstabAge, EstabDate, PlotSize)]
  PSP_PLOTS <- PSP_PLOTS[PSP_PLOTS_YR, on = "Plot"]

  #remove plots without stand age
  PSP_PLOTS <- PSP_PLOTS[!is.na(EstabAge) & EstabAge != 0,]

  #assume ages were measured at plot establishment year, not measure year
  PSP_PLOTS[, EstabYear := as.integer(format(EstabDate, "%Y"))]
  PSP_PLOTS[, baseYear := min(MeasYr), .(Plot)]
  PSP_PLOTS[, baseSA := EstabAge + c(baseYear-EstabYear)] #there may be a year difference

  #species may be unknown in NB data - but there should be no NA due to bad joins
  sppNB <- NB_PSP_Data[["LookUp_Species"]][, .(species, LatinName, CommonName)]
  PSP_TREE_YIMO[is.na(species), species := 999] #coded as unknown in table
  sppEquivPrep <- sppEquiv[, .(Latin_full, PSP)]
  PSP_TREE_YIMO <- sppNB[PSP_TREE_YIMO, on = c("species")]
  PSP_TREE_YIMO <- sppEquivPrep[PSP_TREE_YIMO, on = c("Latin_full" = "LatinName")]
  #note that most species lack biomass equations
  PSP_TREE_YIMO[is.na(PSP)|PSP == "", PSP := CommonName]
  PSP_TREE_YIMO[, c("species", "CommonName") := NULL]

  #this ensures elm trees have biomass equations (they are very likely white elms)...
  PSP_TREE_YIMO[Latin_full == "Ulmus spp.", PSP := "white elm"]

  PSP_LOC_LAT_LONG <- PSP_LOC_LAT_LONG[, .(PLOT, lat, long_)]
  PSP_PLOTS[, Plot := as.integer(Plot)]
  PSP_PLOTS <- PSP_LOC_LAT_LONG[PSP_PLOTS, on = c("PLOT" = "Plot")]

  PSP_PLOTS <- PSP_PLOTS[!is.na(lat) & !is.na(long_)]
  #a dozen plots are missign location date - but do have x and y coordinates
  #they are not in a UTM projection but lacking additional information, they are filtered

  PSP_TREE_YIMO <- PSP_TREE_YIMO[Plot %in% PSP_PLOTS$PLOT]
  PSP_TREE_YIMO[, MeasNum := NULL]

  #some RemeasIDs exist in the plot table but have no recorded tree measurements, e.g. 9079_5
  PSP_PLOTS <- PSP_PLOTS[RemeasID %in% PSP_TREE_YIMO$RemeasID]
  PSP_TREE_YIMO <- PSP_TREE_YIMO[PSP_PLOTS[,.(RemeasID, MeasYr)], on = c("RemeasID")] #MeasYr is necessary

  setnames(PSP_PLOTS,
           old = c("PLOT", "lat", "long_", "RemeasID", "MeasYr"),
           new = c("OrigPlotID1", "Latitude", "Longitude", "MeasureID", "MeasureYear"))

  setnames(PSP_TREE_YIMO,
           old = c("Latin_full", "PSP", "RemeasID", "treenum", "Plot", "MeasYr"),
           new = c("Species", "newSpeciesName", "MeasureID", "TreeNumber", "OrigPlotID1", "MeasureYear"))

  setcolorder(PSP_TREE_YIMO, c("MeasureID", "OrigPlotID1", "MeasureYear",
                               "TreeNumber", "Species", "DBH", "newSpeciesName"))
  setcolorder(PSP_PLOTS, c("MeasureID", "OrigPlotID1", "MeasureYear", "Longitude",
                           "Latitude", "PlotSize", "baseYear", "baseSA"))

  #assign NB
  PSP_TREE_YIMO[, OrigPlotID1 := paste0("NBPSP_", OrigPlotID1)]
  PSP_PLOTS[, OrigPlotID1 := paste0("NBPSP_", OrigPlotID1)]
  PSP_TREE_YIMO[, MeasureID := paste0("NBPSP_", MeasureID)]
  PSP_PLOTS[, MeasureID := paste0("NBPSP_", MeasureID)]

  return(list(
    "plotHeaderData" = PSP_PLOTS,
    "treeData" = PSP_TREE_YIMO
  ))
}

#' retrieve the New Brunswick PSP raw data
#' @param dPath data directory for raw data
#'
#' @return a list of plot, tree, measurement, and location data.tables after exporting mdb to csv txt
#'
#' @export
#' @importFrom reproducible prepInputs
prepInputsNBPSP <- function(dPath) {

  pspNBtree <- prepInputs(targetFile = "PSP_TREE_YIMO.txt",
                          url = "https://drive.google.com/file/d/1o61Ky4HifJlVqQsAVtmhViq6j4J5q5R0/view?usp=drive_link",
                          fun = 'fread',
                          destinationPath = dPath)

  pspNBplot <- prepInputs(targetFile = "PSP_PLOTS.txt",
                          url = "https://drive.google.com/file/d/1_a7ciMI_1W7iR60a5uKAqYOJ_W1W6399/view?usp=drive_link",
                          destinationPath = dPath,
                          fun = 'fread')

  pspNByear <- prepInputs(targetFile = "PSP_PLOTS_YR.txt",
                          url = "https://drive.google.com/file/d/1dfMcCrHGRFIz9S4lH9elqOg5rkVPeAet/view?usp=drive_link",
                          fun = "fread",
                          destinationPath = dPath)
  pspNBloc <- prepInputs(targetFile = "PSP_LOC_LAT_LONG.txt",
                        url = "https://drive.google.com/file/d/1lBbuXuVIQ0QkO80a6DnxQlXm8wwtWHmN/view?usp=drive_link",
                        destinationPath = dPath,
                        fun = "fread")

  lookupSpecies <- prepInputs(targetFile = "LookUp_Species_NB.txt",
                              url = "https://drive.google.com/file/d/1DBeZ5LOk8Io3Zp2uSeYCDveMsLCy7zei/view?usp=drive_link",
                              destinationPath = dPath,
                              fun = "fread")

  return(list(
    "PSP_PLOTS" = pspNBplot,
    "PSP_TREE_YIMO" = pspNBtree,
    "PSP_PLOTS_YR" = pspNByear,
    "PSP_LOC_LAT_LONG" = pspNBloc,
    "LookUp_Species" = lookupSpecies
  ))
}
