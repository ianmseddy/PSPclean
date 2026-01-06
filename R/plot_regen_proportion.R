globalVariables(c(
  "DBH", "propStatus", "status", "MeasureID", "OrigPlotID1", "source", "elapsedTime",
  "diff", "diff_per_year", "diff_negative_flag", "implausible_growth", "expected_dbh ",
  "Ntrees", "NtreesInStatus", "min_DBH"
))

#' @title Analyze Regeneration Proportion and Tree DBH Growth
#'
#' @description
#' Computes the proportion of regeneration trees (`propStatus`) per MeasureID, Plot, and Source;
#' identifies high-regeneration MeasureIDs; summarizes DBH statistics for high-regeneration plots;
#' produces a histogram of regeneration proportions; calculates elapsed time between successive measurements;
#' and flags trees with biologically implausible DBH growth.
#'
#' @param plots A list containing at least a data frame `PSPmeasure` with tree measurements and `PSPplot` with measurement years.
#' @param status_col Name of the column indicating tree status (default: "status").
#' @param measure_col Name of the column indicating MeasureID (default: "MeasureID").
#' @param plotID Name of the column indicating the plot ID (default: "OrigPlotID1").
#' @param source_col Name of the column indicating the data source or region (default: "source").
#' @param regen_value Value in `status_col` indicating regeneration trees (default: "Regeneration").
#' @param binwidth Bin width for the histogram of regeneration proportions (default: 0.05).
#'
#' @return A list with:
#' \describe{
#'   \item{high_regeneration_measurements}{Filtered high-regeneration MeasureIDs with DBH summary, elapsed time, and growth flags.}
#'   \item{full_regeneration_measurements}{Measurements with 100% trees as regeneration.}
#'   \item{partial_regeneration_measurements}{Measurements with 50–99% trees as regeneration.}
#'   \item{plots_augmented}{Original `plots` list with `propStatus` added.}
#'   \item{regeneration_histogram}{Histogram of regeneration proportions per source.}
#'   \item{dbh_summary_by_source}{Mean, min, and max DBH per source for high-regeneration trees.}
#'   \item{elapsed_time_between_measurements}{Elapsed years since previous measurement for high/questionable regeneration plots.}
#' }
#'
#' @importFrom data.table data.table as.data.table .SD :=
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap labs theme_minimal scale_x_continuous
#' @export

plot_regen_proportion <- function(plots,
                                  status_col = "status",
                                  measure_col = "MeasureID",
                                  plotID = "OrigPlotID1",
                                  source_col = "source",
                                  regen_value = "Regeneration",
                                  binwidth = 0.05) {

  # Convert to data.table
  DT <- as.data.table(plots$PSPmeasure)
  DT[, Ntrees := .N, .(MeasureID, OrigPlotID1)]
  DT[, NtreesInStatus := .N, by = .(MeasureID, OrigPlotID1, status)]

  propStatus <- DT[, propStatus := round(NtreesInStatus / Ntrees, 3), by = .(MeasureID, status)]
  propStatus <- unique(propStatus)
  PSPmeasure_regen <- propStatus[status == "Regeneration"]

  #------------------------------------------------------------
  # Identify plots with a measurement where all trees are regeneration (propStatus >= 1)
  #------------------------------------------------------------
  PSPmeasure_regen_partial_1 <-  PSPmeasure_regen[propStatus >= 1, ]

  #------------------------------------------------------------
  # Identify regeneration MeasureIDs (>= 0.5 and <= 1)
  #------------------------------------------------------------
  PSPmeasure_regen_partial <-  PSPmeasure_regen[propStatus >= 0.5 & propStatus < 1]

  #------------------------------------------------------------
  # Keep only propStatus between 0.75 or NA
  #------------------------------------------------------------
  PSPmeasure_regen_high <- PSPmeasure_regen_partial[is.na(propStatus)|(propStatus >= 0.75 )]

  #------------------------------------------------------------
  # Compute mean, min, max DBH per Plot and Source
  #------------------------------------------------------------
  dbh_summary <- PSPmeasure_regen_high[, .(
    mean_DBH = mean(DBH, na.rm = TRUE),
    min_DBH  = min(DBH, na.rm = TRUE),
    max_DBH  = max(DBH, na.rm = TRUE)
  ), by = .(source)]

  # Merge min_DBH from dbh_summary into PSPmeasure_regen_high
  PSPmeasure_regen_high <- merge(
    PSPmeasure_regen_high,
    dbh_summary[, .(source, min_DBH)],
    by = "source",
    all.x = TRUE
  )

  #------------------------------------------------------------
  # Histogram of regeneration proportions per Source
  #------------------------------------------------------------

  histogram_plot <- ggplot(PSPmeasure_regen_partial , aes(x = propStatus)) +
    geom_histogram(binwidth = binwidth,
                   fill = "forestgreen",
                   color = "black") +
    facet_wrap(~ source, scales = "free_y") +
    scale_x_continuous(limits = c(0,1)) +  # <- force x-axis between 0 and 1

    labs(
      title = "Distribution of Regeneration Proportion (>= 0.5) per MeasureID",
      x = "Proportion Regeneration",
      y = "Number of MeasureIDs"
    ) +
    theme_minimal(base_size = 13)

  #------------------------------------------------------------
  # Compute elapsed time between measurements for high regeneration plots
  #------------------------------------------------------------
  plotsWithQuestionableRegen <- unique(PSPmeasure_regen_high$MeasureID)

  years <- sapply(
    plotsWithQuestionableRegen,
    FUN = function(measureID, df = plots$PSPplot) {
      thePlot <- df[MeasureID == measureID]$OrigPlotID1
      thisYear <- df[MeasureID == measureID]$MeasureYear
      possibleYears <- df[OrigPlotID1 == thePlot]$MeasureYear
      sortedYears <- sort(possibleYears)
      lastMeasurement <- sortedYears[which(sortedYears == thisYear) - 1]
      elapsedTime <- thisYear - lastMeasurement
      return(elapsedTime)
    }
  )

  # Add elapsedTime as a new column to PSPmeasure_regen_high_unique Match by MeasureID
  names(years) <- plotsWithQuestionableRegen # Name the vector

  PSPmeasure_regen_high[, elapsedTime := years[as.character(MeasureID)]]
  #------------------------------------------------------------
  # Compute expected growth (expected_dbh ), difference, diff_per_year, and flags
  #------------------------------------------------------------
  a <- 1  # assumed growth per year in cm
  PSPmeasure_regen_high[, expected_dbh  := elapsedTime * a + min_DBH]
  PSPmeasure_regen_high[, diff := DBH - expected_dbh]
  PSPmeasure_regen_high[, diff_per_year := ifelse(elapsedTime > 0, diff / elapsedTime, NA_real_)]
  PSPmeasure_regen_high[, diff_negative_flag := diff < 0]

  cat("Rows with implausible negative DBH growth:\n")
  print(PSPmeasure_regen_high[diff_negative_flag == TRUE])

  # Flag biologically implausible negative growth
  growth_threshold <- -0.5  # cm/year
  PSPmeasure_regen_high[, implausible_growth :=
                          !is.na(diff_per_year) & diff_per_year < growth_threshold]

  cat("Rows with implausible negative DBH growth:\n")
  print(PSPmeasure_regen_high[implausible_growth == TRUE])

  # Remove flagged rows
  PSPmeasure_regen_high <- PSPmeasure_regen_high[
    diff_negative_flag == FALSE & implausible_growth == FALSE
  ]

  #------------------------------------------------------------
  # Return results
  #------------------------------------------------------------
  return(list(
    high_regeneration_measurements = PSPmeasure_regen_high,
    full_regeneration_measurements = PSPmeasure_regen_partial_1,
    partial_regeneration_measurements = PSPmeasure_regen_partial,
    plots_augmented = plots,
    regeneration_histogram = histogram_plot,
    dbh_summary_by_source = dbh_summary,
    elapsed_time_between_measurements = years
  ))
}



