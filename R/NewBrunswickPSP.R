globalVariables(c(
  ":=", "PlotType","Treatment","Plot", "dbh", "EstabDate", "EstabAge",
  "EstabYear", "Status","SilvID", "treenum", "YearTreated", "long_", "MeasYr",
  "species","TreeNumber","SpeciesCode","Species","MeasNum", "measNum",
  "RemeasID", "LatinName", "Latin_full", "PSP", "CommonName", "PLOT", "lat",
  "OrigPlotID1","MeasureYear","PlotSize","baseSA","LATITUDE","LONGITUDE"
))

#' Standardize and Treat the New Brunswick PSP Data
#'
#' This function cleans and standardizes New Brunswick PSP data, including tree and plot data.
#' Species names can be standardized using a species equivalency table.
#'
#' @param NB_PSP_Data A list of `data.table`s resulting from `prepInputsNBPSP`, containing raw PSP data.
#' @param sppEquiv A table providing species name equivalencies between the original PSP species names
#'                 and the final standardized naming format. Default is `LandR::sppEquivalencies_CA`.
#' @param sppEquivCol Character string. The column in `sppEquiv` that contains the standardized species names.
#'                    Default is `"Latin_full"`.
#'
#' @return A list containing standardized `plotData` and `treeData` as `data.table`s.
#'
#' @export
#' @importFrom data.table set setcolorder

dataPurification_NBPSP <- function(NB_PSP_Data,
                                   sppEquiv = LandR::sppEquivalencies_CA,
                                   sppEquivCol = "Latin_full") {

  PSP_PLOTS <- NB_PSP_Data[["PSP_PLOTS"]]
  PSP_PLOTS_YR <- NB_PSP_Data[["PSP_PLOTS_YR"]]
  PSP_TREE_YIMO <- NB_PSP_Data[["PSP_TREE_YIMO"]]
  PSP_LOC_LAT_LONG <- NB_PSP_Data[["PSP_LOC_LAT_LONG"]]

  # Filter plots
  PSP_PLOTS <- PSP_PLOTS[PlotType == "M" & SilvID == 0]

  # Remove bad measurements
  PSP_PLOTS_YR <- PSP_PLOTS_YR[!RemeasID %in% "10405_4"]
  PSP_TREE_YIMO <- PSP_TREE_YIMO[!RemeasID %in% "10405_4"]
  PSP_TREE_YIMO <- PSP_TREE_YIMO[!cause %in% 1:9]

  # Misc fixes
  PSP_TREE_YIMO[RemeasID == "5040_5" & treenum  > 100, treenum := treenum + 200]
  PSP_TREE_YIMO[RemeasID == "5040_5" & treenum  < 100, treenum := treenum + 400]
  MiscBad <- c("10308_5", "1035_5", "1066_4", "3091_4", "5037_4", "5038_4", "5042_4",
               "5044_4", "5046_4", "5047_4", "5048_4", "5050_4", "5051_4", "5053_4",
               "5055_4", "5056_4", "7089_5")
  PSP_TREE_YIMO <- PSP_TREE_YIMO[!RemeasID %in% MiscBad]

  #generate eventual plot header
  PSP_PLOTS_YR <- PSP_PLOTS_YR[Plot %in% PSP_PLOTS$Plot, .(Plot, RemeasID, MeasYr, measNum)]

  # Standardize DBH and PlotSize
  PSP_TREE_YIMO[, DBH := dbh / 10]
  PSP_PLOTS[, PlotSize := PlotSize / 10000]

  PSP_TREE_YIMO <- PSP_TREE_YIMO[, .(RemeasID, treenum, species, DBH, Plot, MeasNum)]
  # internal standardization of DBH (min DBH was 5.1 cm except for plots established in 1987,
  # or for alder and mountain maple)
  #to simplify, remove all trees under 5.1 cm DBH
  PSP_TREE_YIMO <- PSP_TREE_YIMO[DBH > 5.0]

  # DBH corrections
  PSP_TREE_YIMO <- PSP_TREE_YIMO[PSP_PLOTS_YR[, .(RemeasID, MeasYr)], on = "RemeasID"]
  PSP_TREE_YIMO <- PSP_TREE_YIMO %>%
    mutate(DBH = case_when(
      Plot == 33047 & treenum == 815 & DBH == 995.4 & MeasYr == 2010 ~ 9.5,
      Plot == 1076  & treenum == 284 & DBH == 224.3 & MeasYr == 2012 ~ 24.3,
      Plot == 2057  & treenum == 197 & DBH == 120.8 & MeasYr == 2011 ~ 20.8,
      Plot == 2063  & treenum == 25  & DBH == 116.9 & MeasYr == 2011 ~ 16.9,
      Plot == 38040 & treenum == 492 & DBH == 221.4 & MeasYr == 2010 ~ 21.4,
      TRUE ~ DBH
    ))

  # Join plot measurements
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

  sppEquiv <- unique(sppEquiv[, .SD, .SDcols = c(sppEquivCol, "NB_forestry")])
  PSP_TREE_YIMO <- sppNB[PSP_TREE_YIMO, on = c("species")]
  PSP_TREE_YIMO <- sppEquiv[PSP_TREE_YIMO, on = c("Latin_full" = "LatinName")]
  # #note that most species lack biomass equations
  # PSP_TREE_YIMO[is.na(PSP)|PSP == "", PSP := CommonName]
  PSP_TREE_YIMO[, c("species", "CommonName") := NULL]

  #this ensures elm trees have biomass equations (they are very likely white elms)...
  # PSP_TREE_YIMO[Latin_full == "Ulmus spp.", PSP := "white elm"]

  PSP_LOC_LAT_LONG <- PSP_LOC_LAT_LONG[, .(PLOT, lat, long_)]
  PSP_PLOTS[, Plot := as.integer(Plot)]
  PSP_PLOTS <- PSP_LOC_LAT_LONG[PSP_PLOTS, on = c("PLOT" = "Plot")]

  PSP_PLOTS <- PSP_PLOTS[!is.na(lat) & !is.na(long_)]
  #a dozen plots are missing location data - but do have x and y coordinates
  #they are not in a UTM projection but lacking additional information, they are filtered

  PSP_TREE_YIMO <- PSP_TREE_YIMO[Plot %in% PSP_PLOTS$PLOT]
  PSP_TREE_YIMO[, MeasNum := NULL]

  #some RemeasIDs exist in the plot table but have no recorded tree measurements, e.g. 9079_5
  PSP_PLOTS <- PSP_PLOTS[RemeasID %in% PSP_TREE_YIMO$RemeasID]

  PSP_TREE_YIMO <- PSP_TREE_YIMO[PSP_PLOTS[,.(RemeasID, MeasYr)], on = c("RemeasID", "MeasYr")]

  setnames(PSP_PLOTS,
           old = c("PLOT", "lat", "long_", "RemeasID", "MeasYr"),
           new = c("OrigPlotID1", "Latitude", "Longitude", "MeasureID", "MeasureYear"))

  setnames(PSP_TREE_YIMO,
           old = c("Latin_full", "NB_forestry", "RemeasID", "treenum", "Plot", "MeasYr"),
           new = c("Species", "PSP", "MeasureID", "TreeNumber", "OrigPlotID1", "MeasureYear"))

  # Check
  PSP_TREE_YIMO[, .(PSP, Species)]

  PSP_TREE_YIMO[is.na(Species)| Species == "", Species := "unknown"]
  PSP_TREE_YIMO[is.na(PSP) | PSP == "", PSP := "unknown"]

  plotCols <- c("MeasureID", "OrigPlotID1", "MeasureYear", "Longitude",
                "Latitude", "PlotSize", "baseYear", "baseSA")
  PSP_PLOTS <- PSP_PLOTS[, .SD, .SDcol = plotCols]

  PSP_TREE_YIMO <- PSP_TREE_YIMO[, .(
     MeasureID, OrigPlotID1, MeasureYear, TreeNumber, PSP, Species, DBH
  )]
  #assign NB
  PSP_TREE_YIMO[, OrigPlotID1 := paste0("NBPSP_", OrigPlotID1)]
  PSP_PLOTS[, OrigPlotID1 := paste0("NBPSP_", OrigPlotID1)]
  PSP_TREE_YIMO[, MeasureID := paste0("NBPSP_", MeasureID)]
  PSP_PLOTS[, MeasureID := paste0("NBPSP_", MeasureID)]

  PSP_PLOTS[, source := "NB"]
  PSP_TREE_YIMO[, source := "NB"]

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
