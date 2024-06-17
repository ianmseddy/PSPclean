globalVariables(c(
  ":=", ".N", "cause", "cause1", "cause2", "cause3", "company_plot_number",
  "condition_code1", "condition_code2", "condition_code3", "crown_class",
  "dbh", "dbh_age", "elevation", "height", "latitude", "longitude",
  "maxN", "MeasureID", "measurement_number", "measurement_year",
  "MeasureYear", "minMeasure", "N", "OrigPlotID1", "PlotSize",
  "sizes", "species", "Species", "stand_age", "stand_origin", "stump_age",
  "tempyear", "total_age", "totalBad", "tree_location_id", "tree_number",
  "tree_origin", "tree_plot_area", "trees_measurement_comment"
))

#' standardize and treat the Alberta PSP data
#'
#' @param treeMeasure the tree measurement csv
#' @param plotMeasure the plot_measurement csv
#' @param tree the tree csv
#' @param plot the plot csv
#' @param codesToExclude damage agent codes used to filter tree data - see GOA PSP Manual.
#' Measurements with these codes will be removed
#' @param excludeAllObs if removing observations of individual trees due to damage codes,
#' @param areaDiffThresh the threshold of plot size discrepancy to allow below which
#' plots will be given a new ID column. Expressed as `min(PlotSize)/max(PlotSize)`
#' remove all prior and future observations if `TRUE`.
#'
#' @return a list of plot and tree data.tables
#'
#' @export
#' @importFrom data.table copy data.table set setcolorder setkey
#'
dataPurification_ABPSP <- function(treeMeasure, plotMeasure, tree, plot,
                                   codesToExclude = 3, excludeAllObs = TRUE,
                                   areaDiffThresh = 0.95) {
  plot <- copy(plot)
  plotMeasure <- copy(plotMeasure)
  treeMeasure <- copy(treeMeasure)
  treeMeasure[, trees_measurement_comment := NULL] # for more legible printing
  tree <- copy(tree)
  # TODO: determine what this column represents - values above 0 seem to identify trees measured only once
  tree <- tree[tree_location_id == 0]
  tree <- tree[, .(company_plot_number, tree_number, tree_origin, species)]
  treeMeasure <- tree[treeMeasure, on = c("company_plot_number", "tree_number")]
  rm(tree)

  plotMeasure[, company_plot_number := as.factor(company_plot_number)]
  treeMeasure[, company_plot_number := as.factor(company_plot_number)]
  plot[, company_plot_number := as.factor(company_plot_number)]

  # fix measurement numbers - some plots start at measurement number > 1
  treeMeasure[, minMeasure := min(measurement_number), .(company_plot_number)]
  treeMeasure[, measurement_number := measurement_number - (minMeasure - 1)]
  set(treeMeasure, , "minMeasure", NULL)

  plotMeasure[, minMeasure := min(measurement_number), .(company_plot_number)]
  plotMeasure[, measurement_number := measurement_number - (minMeasure - 1)]
  set(plotMeasure, , "minMeasure", NULL)

  # select unmanaged PSPs (ie that were not planted)
  plotMeasure <- plotMeasure[!stand_origin == "P"]
  treeMeasure <- treeMeasure[company_plot_number %in% plotMeasure$company_plot_number]


  # correct ages -
  # From the PSP Field Procedures Manual 2005 Government of Alberta Forest Management Branch
  # Information pertaining to species, DBH, height, height to live crown, crown class, and condition codes
  # are transferred onto the PSP Header Sheet under tree number 0000.
  # in the updated dataset, there is no tree number 0000 -
  # there are also 296 plots with "total age" but most are DBH < 9
  headerData_SA <- treeMeasure[!is.na(dbh_age) | !is.na(stump_age) | !is.na(total_age)]

  # remove trees with no DBH measure so that the age estimate is not affected by these unsused measurements
  headerData_SA <- headerData_SA[!is.na(dbh)]

  # some ages were measured for trees with suppressed and intermediate crown class - exclude
  headerData_SA <- headerData_SA[measurement_number == 1 & !crown_class %in% c("I", "S"), ]

  # estimate the difference between age measured at DBH and at stump
  # and apply it to DBH age so that all trees are approximately aged from stump (or total age where present)
  SADiff <- as.integer(mean(headerData_SA[!is.na(dbh_age) & !is.na(stump_age)]$stump_age -
    headerData_SA[!is.na(dbh_age) & !is.na(stump_age)]$dbh_age))
  headerData_SA <- headerData_SA[!is.na(dbh_age) & is.na(stump_age), stump_age := dbh_age + SADiff]
  headerData_SA[!is.na(total_age) & is.na(stump_age), stump_age := total_age]

  # following Yong's workflow, we take the mean stump_age for each plot at measurement 1
  # then we update the stand age for subsequent measurements
  headerData_SA <- headerData_SA[, .(company_plot_number, measurement_number, stump_age)]

  headerData_SA <- headerData_SA[, .(stand_age = as.integer(mean(stump_age))), .(company_plot_number, measurement_number)]

  # keep measurement_numbers > 1, but drop company plot numbers that don't match
  plotMeasure <- headerData_SA[plotMeasure[company_plot_number %in% headerData_SA$company_plot_number, ],
    on = c("company_plot_number", "measurement_number")
  ]

  baseYear <- plotMeasure[measurement_number == 1]
  baseYear[, baseYear := measurement_year]
  baseYear[, baseSA := stand_age]
  plotMeasure <- baseYear[, .(company_plot_number, baseYear, baseSA), ][plotMeasure, on = c("company_plot_number")]

  # update standa age by measure year
  plotMeasure[is.na(stand_age), stand_age := baseSA + measurement_year - baseYear]

  # clean up
  treeMeasure <- treeMeasure[!tree_number == 0 & !is.na(dbh)]

  treeMeasure <- treeMeasure[, .(
    company_plot_number, measurement_number, tree_number,
    species, dbh, height,
    condition_code1, condition_code2, condition_code3,
    cause1, cause2, cause3
  )]

  treeMeasure <- treeMeasure[company_plot_number %in% plotMeasure$company_plot_number]

  # Previously we removed trees with followign classes: these are Dead Tree Standing, Dead and Down, MPB, and Spruce Beetle
  # now MPB and Spruce Beetle are causes, while dead tree standing and dead and down are conditions
  # Previous defaults were 1 = Spruce budworm, 3 = Mountain pine beetle, 20 = insect (other)
  # we cannot remove individual observations of trees due to MPB - we need to throw out the measurement.
  # From GOA PSP manual June 2019 - pg 26
  badConditions <- c(1, 2, 13, 14) # standing dead, dead down, cut down, missing
  # these must be removed no matter what
  treeMeasure <- treeMeasure[!condition_code1 %in% badConditions &
    !condition_code2 %in% badConditions &
    !condition_code3 %in% badConditions, ]

  # these are removed conditionally
  if (!is.null(codesToExclude)) {
    CauseLegend <- data.table(
      cause = 1:23,
      name = c(
        "Spruce budworm", "Defoliator", "Mountain pine beetle", "Root collar weevil",
        "Terminal weevil", "Armillaria root disease", "Shepherd's crook", "Dwarf mistletoe",
        "Stem disease", "Wstern gall rust", "Animal damage", "Wind damage", "Snow/ice damage",
        "Hail damage", "Fire damage", "Mechanical damage", "Improper planting",
        "Poor ground conditions", "Competition", "Insect (other)", "Disease (other)",
        "Climate/weather/flood damage", "Anthropogenic damage"
      )
    )
    CauseLegend <- CauseLegend[cause %in% codesToExclude, ]
    message(paste("removing trees marked as damaged by:", paste(CauseLegend$name, collapse = ", ")))
    badTrees <- treeMeasure[
      cause1 %in% codesToExclude | cause2 %in% codesToExclude | cause3 %in% codesToExclude,
      .(company_plot_number, measurement_number, tree_number)
    ]
    message(paste("removing", nrow(badTrees), "trees due to damage from Alberta PSP"))

    if (excludeAllObs) {
      treeMeasure <- treeMeasure[!badTrees, on = c("company_plot_number", "tree_number")]
      # then we need to remove trees with codesToExclude at any point
    } else {
      # only exclude the tree in the measured year
      treeMeasure <- treeMeasure[!badTrees, on = c("company_plot_number", "measurement_number", "tree_number")]
    }
  }

  # # check the plot size
  plotMeasure[, PlotSize := tree_plot_area / 10000]
  measureID <- unique(plotMeasure[, .(company_plot_number, measurement_number)])
  measureID[, MeasureID := paste0("ABPSP_", as.numeric(row.names(measureID)))]

  plotMeasure <- measureID[plotMeasure, on = c("company_plot_number", "measurement_number")]
  treeMeasure <- measureID[treeMeasure, on = c("company_plot_number", "measurement_number")]
  plotMeasure[, OrigPlotID1 := company_plot_number]
  treeMeasure[, OrigPlotID1 := company_plot_number]

  plotLocation <- plot[, .(company_plot_number, elevation, latitude, longitude)]
  plotMeasure <- plotLocation[plotMeasure, on = c("company_plot_number")]

  headerData <- plotMeasure[, .(
    MeasureID, OrigPlotID1, measurement_year, longitude, latitude, elevation,
    PlotSize, baseYear, baseSA
  )]
  treeData <- treeMeasure[, .(MeasureID, OrigPlotID1, tree_number, species, dbh, height)]

  setnames(treeData, c("species", "dbh", "height", "tree_number"), c("Species", "DBH", "Height", "TreeNumber"))
  # species code changed since 2015 - now the second letter is uncapitalized.
  treeData[, Species := toupper(Species)]
  treeData <- standardizeSpeciesNames(treeData, forestInventorySource = "ABPSP") # Need to add to pemisc

  setnames(
    headerData, c("measurement_year", "longitude", "latitude", "elevation"),
    c("MeasureYear", "Longitude", "Latitude", "Elevation")
  )

  # need MeasureYear in the tree data
  treeData <- headerData[, .(MeasureID, MeasureYear)][treeData, on = c("MeasureID")]
  setcolorder(treeData, neworder = c("MeasureID", "OrigPlotID1", "MeasureYear"))

  treeData$OrigPlotID1 <- paste0("ABPSP", treeData$OrigPlotID1)
  headerData$OrigPlotID1 <- paste0("ABPSP", headerData$OrigPlotID1)

  # correct two instances of tree number changing over time - we will give new plotIDs to these
  # so they are effectively counted as new plots in the same location
  badMeasures1 <- c("ABPSP49.1", "ABPSP49.2", "ABPSP49.3", "ABPSP49.4") # first measurement must be changed
  badMeasures2 <- c("ABPSP309.1", "ABPSP309.2") # first and second measures must be changed
  headerData[OrigPlotID1 %in% badMeasures1 & MeasureYear > 1961, baseYear := 1968]
  headerData[OrigPlotID1 %in% badMeasures1 & MeasureYear == 1961, OrigPlotID1 := paste0(OrigPlotID1, "f")]
  treeData[OrigPlotID1 %in% badMeasures1 & MeasureYear == 1961, OrigPlotID1 := paste0(OrigPlotID1, "f")]
  headerData[OrigPlotID1 %in% badMeasures1, baseSA := baseSA + 7] # treat 68 as first measurement

  headerData[OrigPlotID1 %in% badMeasures2 & MeasureYear >= 1986, baseYear := 1986]
  headerData[OrigPlotID1 %in% badMeasures2 & MeasureYear >= 1986, baseSA := baseSA + 20]
  headerData[OrigPlotID1 %in% badMeasures2 & MeasureYear < 1986, OrigPlotID1 := paste0(OrigPlotID1, "f")]
  treeData[OrigPlotID1 %in% badMeasures2 & MeasureYear < 1986, OrigPlotID1 := paste0(OrigPlotID1, "f")]

  # for now - drop plots where plot area changes substantially
  badPlots3 <- headerData[, .(sizes = length(unique(PlotSize))), .(OrigPlotID1)]
  badPlots3 <- headerData[OrigPlotID1 %in% badPlots3[sizes > 1, ]$OrigPlotID1, ]
  badPlots3[, diff := min(PlotSize) / max(PlotSize), .(OrigPlotID1)]
  needsNewID <- badPlots3[diff < areaDiffThresh]
  needsNewID <- headerData[OrigPlotID1 %in% needsNewID$OrigPlotID1, .N, .(OrigPlotID1, PlotSize)]
  needsNewID[, maxN := max(N), .(OrigPlotID1)]
  trulyBad <- needsNewID[N != maxN]
  trulyBad <- headerData[trulyBad, on = c("PlotSize", "OrigPlotID1")]

  # first update the PlotIDS - measureID can stay same as it is always unique
  headerData[MeasureID %in% trulyBad$MeasureID, OrigPlotID1 := paste0(OrigPlotID1, "f")]
  # next update the baseYear and baseSA
  headerData[, tempyear := baseYear]
  headerData[MeasureID %in% trulyBad$MeasureID, baseYear := min(MeasureYear), .(OrigPlotID1)]
  headerData[MeasureID %in% trulyBad$MeasureID, baseSA := baseSA + baseYear - tempyear, .(OrigPlotID1)]
  headerData[, tempyear := NULL]
  treeData[MeasureID %in% trulyBad$MeasureID, OrigPlotID1 := paste0(OrigPlotID1, "f")]

  # final clean up
  treeData[Height <= 0, Height := NA]
  treeData <- treeData[!is.na(DBH) & DBH > 0]

  headerData[, source := "AB"]
  treeData[, source := "AB"]

  headerData <- headerData[OrigPlotID1 %in% treeData$OrigPlotID1]

  return(list(
    plotHeaderData = headerData,
    treeData = treeData
  ))
}

#' source the Alberta PSP data
#' @param dPath passed to prepInputs destinationPath
#'
#' @return a list of Alberta PSP data.tables
#'
#' @export
#' @importFrom reproducible prepInputs
prepInputsAlbertaPSP <- function(dPath) {
  pspABtreeMeasure <- prepInputs(
    targetFile = "trees_measurement.csv",
    archive = "GOAPSP20191025.zip",
    url = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/",
    fun = "data.table::fread",
    overwrite = TRUE,
    destinationPath = dPath
  )

  pspABtree <- prepInputs(
    targetFile = "trees.csv",
    archive = "GOAPSP20191025.zip",
    url = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/",
    fun = "data.table::fread",
    overwrite = TRUE,
    destinationPath = dPath
  )

  pspABplotMeasure <- prepInputs(
    targetFile = "plot_measurement.csv",
    archive = "GOAPSP20191025.zip",
    url = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/",
    destinationPath = dPath,
    overwrite = TRUE,
    fun = "data.table::fread"
  )

  pspABplot <- prepInputs(
    targetFile = "plot.csv",
    archive = "GOAPSP20191025.zip",
    url = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/",
    fun = "data.table::fread",
    overwrite = TRUE,
    destinationPath = dPath
  )

  return(list(
    "pspABtreeMeasure" = pspABtreeMeasure,
    "pspABtree" = pspABtree,
    "pspABplotMeasure" = pspABplotMeasure,
    "pspABplot" = pspABplot
  ))
}
