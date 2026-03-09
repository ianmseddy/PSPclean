globalVariables(c(
  "DBH", "propStatus", "status", "MeasureID", "OrigPlotID1", "source", "elapsedTime",
  "diff", "diff_per_year", "diff_negative_flag", "implausible_growth", "expected_dbh",
  "Ntrees", "NtreesInStatus", "min_DBH", "previousMsrYear",
  "DBHdiffFromMax", "DBHdiffFromMax_perYear", "DBHflag", "minDBH", "yearOfAllRegen"
))

#' @title Analyze Regeneration Proportion and Tree DBH Growth
#'
#' @description
#' 1. Identifies plots where all trees identified, and their subsequent measurements. These are of interest
#' as the stand age is unlikely to be correct
#' 2. calculates elapsed time between successive measurements and flags trees with
#' biologically implausible DBH growth based on the max_assumed_growth_rate param
#'
#' @param plots A list containing at least a data frame `PSPmeasure` with tree measurements and `PSPplot` with measurement years.
#' @param max_assumed_growth_rate Maximum plausible DBH growth per year for
#' regenerating trees (cm/year, default = 1).
#'
#' @return A list with:
#' \describe{
#'   \item{problematicMeasurements}{MeasurementIDs with 100% regen at some point in plot history
#'   If the problematic measurement is the last measurement, then prior measurements are not included.
#'   If the problematic measurement is NOT the first, then all measurements are included}
#'   \item{problematic trees}{trees with problematic DBH (ie growing faster than max_assumed_growth_rate)}
#' }
#'
#' @importFrom data.table data.table as.data.table .SD :=
#' @export
assessTreeNumberConsistency <- function(plots,
                                  max_assumed_growth_rate = 1) {

  # Convert to data.table
  DT <- as.data.table(plots$PSPmeasure)
  DT <- copy(DT)

  # Compute number of trees per plot & per status
  DT[, Ntrees := .N, by = .(MeasureID, OrigPlotID1)]
  DT[, NtreesInStatus := .N, by = .(MeasureID, OrigPlotID1, status)]
  DT[, propStatus := NtreesInStatus/Ntrees]
  #------------------------------------------------------------
  # Identify plots with a measurement where all trees are regeneration (propStatus >= 1)
  # These can be plots where trees are tagged but not measured (i.e. DBH and species)
  # or possibly the result of disturbance
  #------------------------------------------------------------


  # Filter regeneration plots (these do not measure DBH)
  DT <- DT[DBH > 0]

  #TODO: these mininmums are an approximation
  minDBHs <- data.table(source = c("BC", "AB", "SK", "ON", "QC", "NB", "NFI"),
                        minDBH = c(4, 9.1, 9.7, 2.5, 9, 5, 9))

  #Alberta is 5 post 2015; BC varies and 4 is the min of mins, SK is 7 after 1977
  if (is.null(DT$minDBH)){
    DT <- DT[minDBHs, on = c("source")]
  }

  #------------------------------------------------------------
  # Compute elapsed time between measurements
  #------------------------------------------------------------
  plotShift <- unique(DT[, .(OrigPlotID1, MeasureID, MeasureYear)])
  setkey(plotShift, OrigPlotID1, MeasureYear)
  plotShift[, previousMsrYear := shift(MeasureYear, n = 1), .(OrigPlotID1)]
  plotShift[, elapsedTime := MeasureYear - previousMsrYear]

  stopifnot(plotShift[!is.na(previousMsrYear),]$MeasureYear >
              plotShift[!is.na(previousMsrYear),]$previousMsrYear)
  plotShift[, c("firstMeasureYear", "lastMeasureYear") :=
              .(min(MeasureYear), max(MeasureYear)), .(OrigPlotID1)]
  DT <- plotShift[DT, on = c("OrigPlotID1", "MeasureID", "MeasureYear")]

  a <- max_assumed_growth_rate
  DT[status == "Regeneration", expected_dbh  := elapsedTime * a + minDBH]
  #negative values are fine - they indicate the tree grew slower than 1 cm/year or appeared later
  #positive values become increasingly problematic
  DT[, DBHdiffFromMax := DBH - expected_dbh]
  DT[, DBHdiffFromMax_perYear := DBHdiffFromMax/elapsedTime]

  #plots with high values of DiffPerYear suggest the tree was incorrectly tracked or renumbered
  # check these first, as they contain more new trees
  DT[DBHdiffFromMax > 0, DBHflag := "problematic growth"]
  flagged <- DT[!is.na(DBHflag), .(OrigPlotID1, MeasureID, TreeNumber, source, DBH, DBHdiffFromMax, minDBH)]

  #deal with 100% regen
  possiblyRemove <- DT[propStatus == 1 & status == "Regeneration", .N, .(MeasureYear, OrigPlotID1)]
  possiblyRemove[, yearOfAllRegen := MeasureYear]
  #because stand age is relative to the first measurement, we cannot keep a plot once 100% regen occurs

  yearsAfter <- plots$PSPplot[possiblyRemove, on = c("OrigPlotID1")]
  yearsAfter <- yearsAfter[MeasureYear >= yearOfAllRegen,]$MeasureID

  # Return results
  return(list(
    problematicMeasurements = yearsAfter,
    problematicTrees = flagged
  ))
}
