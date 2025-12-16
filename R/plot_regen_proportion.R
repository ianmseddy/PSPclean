globalVariables(c(
  "DBH", "prop_regen", "status", "MeasureID", "OrigPlotID1", "source", "elapsedTime",
  "diff", "diff_per_year", "diff_negative_flag", "implausible_growth", "n_value"
))

#' @title Plot Regeneration Proportion, Summarize Tree DBH, and Check Growth
#'
#' @description
#' Computes the proportion of regeneration per MeasureID, Plot, and Source; identifies high-regeneration MeasureIDs;
#' summarizes tree diameter (DBH) statistics for high-regeneration plots; produces a histogram of regeneration proportions;
#' calculates the elapsed time between successive measurements for plots with high/questionable regeneration;
#' and flags trees with biologically implausible DBH growth based on elapsed time and expected growth rates.
#'
#' @param plots A list containing at least a data frame `PSPmeasure` with tree measurements and `PSPplot` with measurement years.
#' @param status_col Name of the column in `PSPmeasure` indicating tree status (default: "status").
#' @param measure_col Name of the column indicating MeasureID (default: "MeasureID").
#' @param plotID Name of the column indicating the plot ID (default: "OrigPlotID1").
#' @param source_col Name of the column indicating the data source or region (default: "source").
#' @param regen_value Value in `status_col` indicating regeneration trees (default: "Regeneration").
#' @param binwidth Bin width for the histogram of regeneration proportions (default: 0.05).
#'
#' @return A list with:
#' \describe{
#'   \item{regen_prop}{Proportion of regeneration per MeasureID, Plot, and Source.}
#'   \item{PSPmeasure_regen}{Filtered high-regeneration MeasureIDs with DBH summary statistics, elapsed time,
#'     and flags for implausible DBH growth (`diff_negative_flag`, `implausible_growth`).}
#'   \item{updated_plots}{Original `plots` list with `prop_regen` added.}
#'   \item{histogram}{Histogram of regeneration proportions per Source.}
#'   \item{dbh_summary}{Mean, min, and max DBH per Plot and Source for high-regeneration trees.}
#'   \item{elapsedTime_vector}{Elapsed years since previous measurement for high/questionable regeneration plots.}
#' }
#'
#' @importFrom data.table data.table as.data.table .SD :=
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap labs theme_minimal
#' @export

library(data.table)
library(ggplot2)


plot_regen_proportion <- function(plots,
                                  status_col = "status",
                                  measure_col = "MeasureID",
                                  plotID = "OrigPlotID1",
                                  source_col = "source",
                                  regen_value = "Regeneration",
                                  binwidth = 0.05) {

  # Convert to data.table
  DT <- as.data.table(plots$PSPmeasure)

  # Compute proportion of regeneration per MeasureID, Plot, and Source
  regen_prop <- DT[, prop_regen := mean(get(status_col) == regen_value, na.rm = TRUE),
                   by = .(MeasureID, OrigPlotID1, source)]

  #------------------------------------------------------------
  # Identify high regeneration MeasureIDs (>= 0.5 and < 1)
  #------------------------------------------------------------
  PSPmeasure_regen_unique <- regen_prop[prop_regen >= 0.5 & prop_regen < 1]

  #------------------------------------------------------------
  # Keep only prop_regen between 0.75 and 0.9 or NA
  #------------------------------------------------------------
  PSPmeasure_regen_high_unique <- PSPmeasure_regen_unique[is.na(prop_regen) |
                                                            (prop_regen >= 0.75 & prop_regen <= 0.9)]

  #------------------------------------------------------------
  # Compute mean, min, max DBH per Plot and Source
  #------------------------------------------------------------
  dbh_summary <- PSPmeasure_regen_high_unique[, .(
    mean_DBH = mean(DBH, na.rm = TRUE),
    min_DBH  = min(DBH, na.rm = TRUE),
    max_DBH  = max(DBH, na.rm = TRUE)
  ), by = .(source, OrigPlotID1)]

  # Merge min_DBH from dbh_summary into PSPmeasure_regen_high_unique
  PSPmeasure_regen_high_unique <- merge(
    PSPmeasure_regen_high_unique,
    dbh_summary[, .(source, OrigPlotID1, min_DBH)],
    by = c("source", "OrigPlotID1"),
    all.x = TRUE
  )

  #------------------------------------------------------------
  # Histogram per Source
  #------------------------------------------------------------
  histogram_plot <- ggplot(regen_prop, aes(x = prop_regen)) +
    geom_histogram(binwidth = binwidth,
                   fill = "forestgreen",
                   color = "black") +
    facet_wrap(~ source, scales = "free_y") +
    labs(
      title = "Distribution of Regeneration Proportion per MeasureID",
      x = "Proportion Regeneration",
      y = "Number of MeasureIDs"
    ) +
    theme_minimal(base_size = 13)

  #------------------------------------------------------------
  # Extract the MeasureIDs with high/questionable regeneration
  # Compute elapsedTime using your original sapply logic
  #------------------------------------------------------------
  plotsWithQuestionableRegen <- unique(PSPmeasure_regen_high_unique$MeasureID)

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

  PSPmeasure_regen_high_unique[, elapsedTime := years[MeasureID]]
  #------------------------------------------------------------
  # Compute n_value, diff, diff_per_year
  #------------------------------------------------------------
  a <- 1  # assumed growth per year in cm
  PSPmeasure_regen_high_unique[, n_value := elapsedTime * a + min_DBH]
  PSPmeasure_regen_high_unique[, diff := n_value - DBH]
  PSPmeasure_regen_high_unique[, diff_per_year := ifelse(elapsedTime > 0, diff / elapsedTime, NA_real_)]
  PSPmeasure_regen_high_unique[, diff_negative_flag := diff < 0]

  cat("Rows with implausible negative DBH growth:\n")
  print(PSPmeasure_regen_high_unique[diff_negative_flag == TRUE])

  #------------------------------------------------------------
  # Filter biologically implausible negative growth
  #------------------------------------------------------------
  growth_threshold <- -0.5  # cm/year
  PSPmeasure_regen_high_unique[, implausible_growth :=
                                 !is.na(diff_per_year) & diff_per_year < growth_threshold]

  cat("Rows with implausible negative DBH growth:\n")
  print(PSPmeasure_regen_high_unique[implausible_growth == TRUE])

  # Remove flagged rows
  PSPmeasure_regen_high_unique <- PSPmeasure_regen_high_unique[
    diff_negative_flag == FALSE & implausible_growth == FALSE
  ]

  #------------------------------------------------------------
  # Return results
  #------------------------------------------------------------
  return(list(
    regen_prop = regen_prop,
    PSPmeasure_regen = PSPmeasure_regen_high_unique,
    updated_plots = plots,
    histogram = histogram_plot,
    dbh_summary = dbh_summary,
    elapsedTime_vector = years
  ))
}



