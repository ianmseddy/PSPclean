globalVariables(c(
  "DBH", "propStatus", "status", "MeasureID", "OrigPlotID1", "source", "elapsedTime",
  "diff", "diff_per_year", "diff_negative_flag", "implausible_growth", "expected_dbh",
  "Ntrees", "NtreesInStatus", "min_DBH", "previousMsrYear"
))

#' @title Analyze Regeneration Proportion and Tree DBH Growth
#'
#' @description
#' Computes the proportion of regeneration trees (`propStatus`) per MeasureID, Plot, and Source;
#' identifies high-regeneration MeasureIDs; summarizes DBH statistics for high-regeneration plots;
#' calculates elapsed time between successive measurements; and flags trees with biologically implausible DBH growth.
#'
#' @param plots A list containing at least a data frame `PSPmeasure` with tree measurements and `PSPplot` with measurement years.
#' @param status_col Name of the column indicating tree status (default: "status").
#' @param plotID Name of the column indicating the plot ID (default: "OrigPlotID1").
#' @param source_col Name of the column indicating the data source or region (default: "source").
#' @param regen_value Value in `status_col` indicating regeneration trees (default: "Regeneration").
#' @param growth_rate Maximum expected DBH growth per year (cm/year, default = 1).
#' @param growth_threshold Minimum plausible growth per year (cm/year, default = -0.5).
#'
#' @return A list with:
#' \describe{
#'   \item{high_regeneration_measurements}{Filtered high-regeneration MeasureIDs with DBH summary, elapsed time, and growth flags.}
#'   \item{full_regeneration_measurements}{Measurements with 100% trees as regeneration.}
#'   \item{partial_regeneration_measurements}{Measurements with 50–99% trees as regeneration.}
#'   \item{plots_augmented}{Original `plots` list with `propStatus` added.}
#'   \item{dbh_summary_by_source}{Mean, min, and max DBH per source for high-regeneration trees.}
#'   \item{elapsed_time_between_measurements}{Elapsed years since previous measurement for all plots.}
#' }
#'
#' @importFrom data.table data.table as.data.table .SD :=
#' @export
plot_regen_proportion <- function(plots,
                                  maxAssumedGrowthDBHperYear = 1,
                                  status_col = "status",
                                  plotID = "OrigPlotID1",
                                  source_col = "source",
                                  regen_value = "Regeneration",
                                  growth_rate = 1,
                                  growth_threshold = -0.5) {

  # Convert to data.table
  DT <- as.data.table(plots$PSPmeasure)
  DT <- copy(DT)

  # Compute number of trees per plot & per status
  DT[, Ntrees := .N, by = c(measure_col, plotID)]
  DT[, NtreesInStatus := .N, by = c(measure_col, plotID, status_col)]

  #------------------------------------------------------------
  # Identify plots with a measurement where all trees are regeneration (propStatus >= 1)
  # These are considered errors as the plots have no continuous measurements
  #------------------------------------------------------------
  definitelyRemove <- PSPmeasure_regen[propStatus == 1, ]

<<<<<<<
  # Keep unique rows for analysis
  propStatus <- unique(DT)
=======
  #------------------------------------------------------------
  dubious <- PSPmeasure_regen[propStatus >= 0.75,]
>>>>>>>

<<<<<<<
  # Filter regeneration trees
  PSPmeasure_regen <- propStatus[get(status_col) == regen_value]
=======
  DT <- DT[DBH > 0]
  browser()
>>>>>>>

  #this may not work if DBH changed over time (QC, AB)
  DT[, minDBH := min(DBH), .(source)]
  #------------------------------------------------------------
  # Compute elapsed time between measurements for high regeneration plots
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

  a <- 1  # a generous assumed growth of 1 cm / year
  DT[status == "Regeneration", expected_dbh  := elapsedTime * a + minDBH]
  DT[, diff := DBH - expected_dbh]

  #plots with high values of Diff suggest the tree was incorrectly tracked or renumbered
  # check these first, as they contain more new trees
  dubious <- DT[MeasureID %in% dubious$MeasureID,]

<<<<<<<
  # Compute expected growth and flag implausible values
  PSPmeasure_regen_high[, expected_dbh := min_DBH + elapsedTime * growth_rate]
  PSPmeasure_regen_high[, diff := DBH - expected_dbh]
  PSPmeasure_regen_high[, diff_per_year := ifelse(elapsedTime > 0, diff / elapsedTime, NA_real_)]
  PSPmeasure_regen_high[, diff_negative_flag := diff < 0]
  PSPmeasure_regen_high[, implausible_growth := !is.na(diff_per_year) & diff_per_year < growth_threshold]

  # Remove rows with negative or implausible growth
  PSPmeasure_regen_high <- PSPmeasure_regen_high[
    diff_negative_flag == FALSE & implausible_growth == FALSE
  ]

=======
    browser()
  #------------------------------------------------------------
>>>>>>>
  # Return results
  return(list(
    high_regeneration_measurements = PSPmeasure_regen_high,
    full_regeneration_measurements = PSPmeasure_regen_full,
    partial_regeneration_measurements = PSPmeasure_regen_partial,
    plots_augmented = plots,
    dbh_summary_by_source = dbh_summary,
    elapsed_time_between_measurements = plotShift
  ))
}
