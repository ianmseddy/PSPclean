globalVariables(c(
  ":=", "trees_measurement_comment", "tree_origin", "stand_origin", "dbh_age",
  "stump_age", "total_age", "crown_class", "measurement_year", "stand_age",
  "condition_code1", "condition_code2", "condition_code3", "cause1", "cause2",
  "cause3", ".N", "totalBad", "PlotSize", "tree_plot_area", "MeasureID", "latitude",
  "longitude", "OrigPlotID1", "Species", "MeasureYear", "company_plot_number", "tree_number",
  "species", "minMeasure", "measurement_number", "dbh", "height", "elevation"
))

#' standardize and treat the Alberta PSP data
#'
#' @param treeMeasure the tree measurement csv
#' @param plotMeasure the plot_measurement csv
#' @param tree the tree csv
#' @param plot the plot csv
#' @param codesToExclude damage agent codes used to filterr tree data - see GOA PSP Manual.
#' Measurements with these codes will be removed
#' @param excludeAllObs if removing observations of individual trees due to damage codes,
#' remove all prior and future observations if \code{TRUE}.
#'
#' @return a list of plot and tree data.tables
#'
#' @export
#' @importFrom data.table copy setkey set setcolorder
#'
dataPurification_ABPSP <- function(treeMeasure, plotMeasure, tree,
                                   plot, codesToExclude = 3, excludeAllObs = TRUE) {
  plot <- copy(plot)
  plotMeasure <- copy(plotMeasure)
  treeMeasure <- copy(treeMeasure)
  treeMeasure[, trees_measurement_comment := NULL] #for more legible printing
  tree <- copy(tree)

  tree <- tree[, .(company_plot_number, tree_number, tree_origin, species)]
  treeMeasure <- tree[treeMeasure, on = c("company_plot_number", "tree_number")]
  rm(tree)

  plotMeasure[, company_plot_number := as.factor(company_plot_number)]
  treeMeasure[, company_plot_number := as.factor(company_plot_number)]
  plot[, company_plot_number := as.factor(company_plot_number)]

  #fix measurement numbers - some plots start at measurement number > 1
  treeMeasure[, minMeasure := min(measurement_number), .(company_plot_number)]
  treeMeasure[, measurement_number:= measurement_number - (minMeasure - 1)]
  set(treeMeasure, , 'minMeasure', NULL)

  plotMeasure[, minMeasure := min(measurement_number), .(company_plot_number)]
  plotMeasure[, measurement_number:= measurement_number - (minMeasure - 1)]
  set(plotMeasure, , 'minMeasure', NULL)

  # select unmanaged PSPs (ie that were not planted)
  plotMeasure <- plotMeasure[!stand_origin == "P"]
  treeMeasure <- treeMeasure[company_plot_number %in% plotMeasure$company_plot_number]


  #correct ages -
  #From the PSP Field Procedures Manual 2005 Government of Alberta Forest Management Branch
  # Information pertaining to species, DBH, height, height to live crown, crown class, and condition codes
  # are transferred onto the PSP Header Sheet under tree number 0000.
  # in the updated dataset, there is no tree number 0000 -
  # there are also 296 plots with "total age" but most are DBH < 9
  headerData_SA <- treeMeasure[!is.na(dbh_age) | !is.na(stump_age) | !is.na(total_age)]

  #remove trees with no DBH measure so that the age estimate is not affected by these unsused measurements
  headerData_SA <- headerData_SA[!is.na(dbh)]

  #some ages were measured for trees with suppressed and intermediate crown class - exclude
  headerData_SA <- headerData_SA[measurement_number == 1 & !crown_class %in% c("I", "S"),]

  #estimate the difference between age measured at DBH and at stump
  #and apply it to DBH age so that all trees are approximately aged from stump (or total age where present)
  SADiff <- as.integer(mean(headerData_SA[!is.na(dbh_age) & !is.na(stump_age)]$stump_age -
                              headerData_SA[!is.na(dbh_age) & !is.na(stump_age)]$dbh_age))
  headerData_SA <- headerData_SA[!is.na(dbh_age) & is.na(stump_age), stump_age := dbh_age + SADiff]
  headerData_SA[!is.na(total_age) & is.na(stump_age), stump_age := total_age]

  #following Yong's workflow, we take the mean stump_age for each plot at measurement 1
  #then we update the stand age for subsequent measurements
  headerData_SA <- headerData_SA[, .(company_plot_number, measurement_number, stump_age)]

  headerData_SA <- headerData_SA[, .(stand_age = as.integer(mean(stump_age))), .(company_plot_number, measurement_number)]

  #keep measurement_numbers > 1, but drop company plot numbers that don't match
  plotMeasure <- headerData_SA[plotMeasure[company_plot_number %in% headerData_SA$company_plot_number,],
                               on = c("company_plot_number", "measurement_number")]

  baseYear <- plotMeasure[measurement_number == 1]
  baseYear[, baseYear := measurement_year]
  baseYear[, baseSA := stand_age]
  plotMeasure <- baseYear[, .(company_plot_number, baseYear, baseSA),][plotMeasure, on = c("company_plot_number")]

  #update standa age by measure year
  plotMeasure[is.na(stand_age), stand_age := baseSA + measurement_year - baseYear]

  #clean up
  treeMeasure <- treeMeasure[!tree_number == 0 & !is.na(dbh)]

  treeMeasure <- treeMeasure[, .(company_plot_number, measurement_number, tree_number,
                                 species, dbh, height,
                                 condition_code1, condition_code2, condition_code3,
                                 cause1, cause2, cause3)]

  treeMeasure <- treeMeasure[company_plot_number %in% plotMeasure$company_plot_number]

  #Previously we removed trees with followign classes: these are Dead Tree Standing, Dead and Down, MPB, and Spruce Beetle
  #now MPB and Spruce Beetle are causes, while dead tree standing and dead and down are conditions
  #we cannot remove individual observations of trees due to MPB - we need to throw out the measurement.
  #From GOA PSP manual June 2019 - pg 26
  badConditions <- c(1, 2, 13, 14) #standing dead, dead down, cut down, missing

  badCauses <- codesToExclude
  #Previous defaults were 1 = Spruce budworm, 3 = Mountain pine beetle, 20 = insect (other)

  #determine what proportion of a plot has a condition caused by MPB, SPB, or insects
  treeCheck <- treeMeasure[, .(totalBad = sum(cause1 %in% badCauses |
                                                cause2 %in% badCauses |
                                                cause3 %in% badCauses, na.rm = TRUE)
                               /.N), .(company_plot_number, measurement_number)]
  #this needs to be a parameter
  goodPlots <- treeCheck[totalBad < 0.1]

  treeMeasure <- treeMeasure[c(company_plot_number %in% goodPlots$company_plot_number &
                                 measurement_number %in% goodPlots$measurement_number),]
  plotMeasure <- plotMeasure[c(company_plot_number %in% goodPlots$company_plot_number &
                                 measurement_number %in% goodPlots$measurement_number),]

  #remove dead trees
  treeMeasure <- treeMeasure[!condition_code1 %in% badConditions &
                               !condition_code2 %in% badConditions &
                               !condition_code3 %in% badConditions,]
  # check the plot size
  plotMeasure[, PlotSize := tree_plot_area/10000]
  measureID <- unique(plotMeasure[, .(company_plot_number, measurement_number)])
  measureID[, MeasureID := paste0("ABPSP_", as.numeric(row.names(measureID)))]

  plotMeasure <- measureID[plotMeasure, on = c("company_plot_number", "measurement_number")]
  treeMeasure <- measureID[treeMeasure, on = c("company_plot_number", "measurement_number")]
  plotMeasure[, OrigPlotID1 := company_plot_number]
  treeMeasure[, OrigPlotID1 := company_plot_number]

  plotLocation <- plot[, .(company_plot_number, elevation, latitude, longitude)]
  plotMeasure <- plotLocation[plotMeasure, on = c("company_plot_number")]

  headerData <- plotMeasure[, .(MeasureID, OrigPlotID1, measurement_year, longitude, latitude, elevation,
                                PlotSize, baseYear, baseSA)]
  treeData <- treeMeasure[, .(MeasureID, OrigPlotID1, tree_number, species, dbh, height)]

  setnames(treeData, c("species", "dbh", "height", "tree_number"), c("Species", "DBH", "Height", "TreeNumber"))
  #species code changed since 2015 - now the second letter is uncapitalized.
  treeData[, Species := toupper(Species)]
  treeData <- standardizeSpeciesNames(treeData, forestInventorySource = "ABPSP") #Need to add to pemisc

  setnames(headerData, c("measurement_year", "longitude", "latitude", "elevation"),
           c("MeasureYear", "Longitude", "Latitude", "Elevation"))

  #need MeasureYear in the tree data
  treeData <- headerData[, .(MeasureID, MeasureYear)][treeData, on = c("MeasureID")]
  setcolorder(treeData, neworder = c("MeasureID", "OrigPlotID1", "MeasureYear"))

  treeData$OrigPlotID1 <- paste0("AB", treeData$OrigPlotID1)
  headerData$OrigPlotID1 <- paste0("AB", headerData$OrigPlotID1)


  return(list(plotHeaderData = headerData,
              treeData = treeData))

}

#' source the Alberta PSP data
#' @param dPath passed to prepInputs destinationPath
#'
#' @return a list of Alberta PSP data.tables
#'
#' @export
#' @importFrom reproducible prepInputs
prepInputsAlbertaPSP <- function(dPath) {

  pspABtreeMeasure <- prepInputs(targetFile = file.path(dPath, "trees_measurement.csv"),
                                 url = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/view?usp=sharing",
                                 fun = "fread",
                                 overwrite = TRUE,
                                 destinationPath = dPath)

  pspABtree <- prepInputs(targetFile = file.path(dPath, "trees.csv"),
                          url = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/view?usp=sharing",
                          fun = "fread",
                          overwrite = TRUE,
                          destinationPath = dPath)

  pspABplotMeasure <- prepInputs(targetFile = file.path(dPath, "plot_measurement.csv"),
                                 url = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/view?usp=sharing",
                                 destinationPath = dPath,
                                 overwrite = TRUE,
                                 fun = "fread")

  pspABplot <- prepInputs(targetFile = file.path(dPath, "plot.csv"),
                          url = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/view?usp=sharing",
                          fun = "fread",
                          overwrite = TRUE,
                          destinationPath = dPath)

  return(list(
    "pspABtreeMeasure" = pspABtreeMeasure,
    "pspABtree" = pspABtree,
    "pspABplotMeasure" = pspABplotMeasure,
    "pspABplot" = pspABplot
  ))

}

