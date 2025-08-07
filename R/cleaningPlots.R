
# To rewrite this part regarding the functions
globalVariables(c(
  "status", "zscore", "meanDBH", "sdDBH", "is_outlier_z", "diff_dbh", "total_growth",
  "total_neg_growth", "neg_growth_pct", "desc", "count", "coalesce", "Species.y",
  "Species.x", "newSpeciesName.y", "newSpeciesName.x"
))

#' @title Detect and flag DBH outliers using z-scores by plot
#'
#' @description
#' Flags statistically implausible DBH values based on the z-score method #' (using mean and standard deviation)
#' within each plot group. This can help identify data entry or measurement errors in repeated tree measurements.
#'
#' @param Trees A `data.table` or `data.frame` containing DBH and plot columns.
#' @param dbh_col Character. Name of the DBH column. Default is "DBH".
#' @param plot_col Character. Name of the plot/grouping column. Default is "OrigPlotID1".
#' @param z_thresh Numeric. Z-score threshold for identifying outliers. Default is 7.
#'
#' @return A list with:
#' \describe{
#'   \item{Trees}{The modified `data.table` with added columns: meanDBH, sdDBH, zscore, is_outlier_z}
#'   \item{plotIDs}{Unique plot/group IDs used in analysis}
#' }
#'
#' @importFrom data.table as.data.table copy fifelse
#' @importFrom stats sd
#' @export
#'
detect_dbh_outliers <- function(Trees, dbh_col = "DBH", plot_col = "OrigPlotID1", z_thresh = 7) {
  # Ensure Trees is a data.table
  Trees <- copy(Trees)
  Trees <- data.table::as.data.table(Trees)

  # Validate inputs
  if (!all(c(dbh_col, plot_col) %in% names(Trees))) {
    stop("Both `dbh_col` and `plot_col` must exist in the input data.")
  }

  # Ensure DBH is numeric
  if (!is.numeric(Trees[[dbh_col]])) {
    stop("The DBH column must be numeric.")
  }
  # Calculate statistics and flag outliers
  Trees[, `:=`(
    meanDBH = mean(get(dbh_col), na.rm = TRUE),
    sdDBH   = sd(get(dbh_col), na.rm = TRUE)
  ), by = plot_col]

  Trees[, zscore := (get(dbh_col) - meanDBH) / sdDBH]

  Trees[, is_outlier_z := abs(zscore) > z_thresh]

  # Return output
  return(list(
    Trees = Trees,
    plotIDs = unique(Trees[[plot_col]])
  ))
}


#' @title Identify Tree Numbers Linked to Multiple Species Names in a Plot
#'
#' @description
#' Ensures consistent tree numbering when a tree (TreeNumber) in a plot (OrigPlotID1) is associated with multiple species over time.
#'
#'
#' @param Trees A `data.table` of tree observations containing Species identifiers and measurement years.
#'
#' @return A list containing:
#' \describe{
#'   \item{incorrect_data}{Flagged inconsistent species data.}
#'   \item{correct_species}{Most likely species for each tree.}
#'   \item{regeneration}{Subset with apparent regeneration.}
#'   \item{last_measurement}{Subset with last recorded measurements.}
#'   \item{Trees_corrected}{Cleaned tree dataset.}
#'   \item{OrigPlotID1s}{Unique OrigPlotID1 identifiers.}
#' }
#'
#' @export
#'
#' @importFrom data.table as.data.table copy
#' @importFrom dplyr last first
#' @importFrom dplyr n distinct n_distinct row_number arrange slice_max
#' @importFrom dplyr group_by mutate ungroup filter summarise case_when
#' @importFrom dplyr semi_join anti_join left_join bind_rows select
#' @importFrom magrittr %>%
#'
treenum_to_multiplePSP <- function(Trees) {

  # Convert to data.table
  Trees <- copy(Trees)
  Trees <- as.data.table(Trees)

  # Identify a tree number assigned to multiple Species in the same plot
  incorrect_trees <- Trees %>%
    group_by(OrigPlotID1, TreeNumber) %>%
    filter(n_distinct(newSpeciesName) > 1) %>%
    ungroup()

   # Identify correct species by most frequent combination within each Plot
   correct_species <- incorrect_trees %>%
    group_by(OrigPlotID1, TreeNumber, newSpeciesName, Species) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count)) %>%
    group_by(OrigPlotID1, TreeNumber) %>%
    slice_max(count, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(OrigPlotID1, TreeNumber, newSpeciesName, Species)

   # Correct Species and newSpeciesName in the original dataset
   trees_corrected <- Trees %>%
     left_join(correct_species, by = c("OrigPlotID1", "TreeNumber")) %>%
     mutate(
       Species= coalesce(Species.y, Species.x),
       newSpeciesName = coalesce(newSpeciesName.y, newSpeciesName.x)) %>%
     select(-Species.x, -Species.y, -newSpeciesName.x, -newSpeciesName.y)  # Remove extra columns

   return(list(
     incorrect_trees = incorrect_trees,
               correct_species = correct_species,
               Trees_corrected = trees_corrected,
               OrigPlotID1s = unique(Trees$OrigPlotID1)
     ))
}

#' @title Process Implausible DBH Changes Across Measurement Years
#'
#' @description
#' Detects and manages inconsistencies in tree DBH (Diameter at Breast Height) measurements across years within species.
#' Flags implausible negative growth, cleans the dataset accordingly, and excludes OrigPlotID1s with high anomaly rates.
#'
#' @param Trees A `data.table` of tree measurements over time within Species.
#'
#' @return A list containing:
#' \describe{
#'   \item{Trees}{Cleaned dataset with problematic entries removed.}
#'   \item{dbh_check}{Subset of detected DBH inconsistencies.}
#'   \item{negative_growth_summary}{Summary of negative growth by OrigPlotID1.}
#'   \item{OrigPlotID1s}{List of retained OrigPlotID1 identifiers.}
#' }
#'
#' @export
#'
#' @importFrom data.table as.data.table copy setorder shift
#' @importFrom dplyr group_by summarise filter mutate select left_join arrange pull
#' @importFrom magrittr %>%
#`
process_dbh_issues <- function(Trees) {

  # Convert to data.table
  Trees <- copy(Trees)
  Trees <- as.data.table(Trees)

  # Sort before computing change
  data.table::setorder(Trees, OrigPlotID1, TreeNumber, MeasureYear)

  # Compute year-over-year DBH change for each tree
  Trees[, diff_dbh := DBH - data.table::shift(DBH), by = .(OrigPlotID1, TreeNumber)]

  # Creates two diagnostic subsets:negative_growth and na_values
  negative_growth <- Trees %>% filter(diff_dbh < -0.5)  # trees showing suspicious negative growth greater than 0.5 cm (used as a threshold for likely error).
  na_values <- Trees %>% filter(is.na(diff_dbh))        # records where diff_dbh is NA (typically the first measurement for a tree).

  # Flag invalid DBH values
  Trees <- Trees %>%
    mutate(DBH = ifelse(is.na(DBH) | DBH == 0, -1, DBH))   # Replaces missing (NA) or zero DBH values with -1 to flag them clearly as invalid values.

  # Subset DBH inconsistencies
  dbh_issues <- Trees %>%
    filter(diff_dbh < 0) %>%                                                       # Filters out all records with negative growth for inspection.
    select(newSpeciesName, OrigPlotID1, MeasureID, TreeNumber, DBH, MeasureYear, diff_dbh) %>%    # Selects only relevant columns and arranges them for easy review.
    arrange(OrigPlotID1, TreeNumber, MeasureYear)

  dbh_check <- dbh_issues %>%
    left_join(
      Trees %>% select(newSpeciesName, OrigPlotID1, MeasureID, TreeNumber, MeasureYear, Species),
                by = c("newSpeciesName", "OrigPlotID1", "MeasureID", "TreeNumber", "MeasureYear")
      )

 # Summarize negative growth
  negative_growth_summary <- Trees %>%
    group_by(OrigPlotID1) %>%
    summarise(
      total_neg_growth = sum(diff_dbh[!is.na(diff_dbh) & diff_dbh < 0], na.rm = TRUE),
      total_growth = sum(abs(diff_dbh[!is.na(diff_dbh)]), na.rm = TRUE),
      neg_growth_pct = ifelse(total_growth > 0,
                              100 * abs(total_neg_growth) / total_growth,
                              NA_real_)  # Will be NA for plots with only 1 measurement
    ) %>%
    arrange(OrigPlotID1)


  # Identify plots to keep: either low negative growth or no growth data
  OrigPlotID1s_to_keep <- negative_growth_summary %>%
    filter(neg_growth_pct <= 5 | is.na(neg_growth_pct)) %>% # Keeps only OrigPlotID1s where the negative DBH growth percentage is ≤ 5%, or missing (i.e., OrigPlotID1s with no DBH change data).
    pull(OrigPlotID1)

  Trees_corrected <- Trees %>%
    filter(OrigPlotID1 %in% OrigPlotID1s_to_keep) # Removes OrigPlotID1s with >5% negative DBH change from the final processed_data.

  # Returns a list containing:
  return(list(
    Trees = Trees_corrected,      # the cleaned and filtered tree dataset.
    dbh_check = dbh_check,       # the table of detected DBH inconsistencies.
    negative_growth_summary = negative_growth_summary,     # OrigPlotID1-level statistics on negative growth.
    OrigPlotID1s = OrigPlotID1s_to_keep
  ))
}


#' @title Classify Tree Status Based on Measurement History
#'
#' @description
#' Assigns status to each tree based on its observation history. Detects new regeneration, lost trees, and survivors.
#'
#' @param Trees A `data.table`  of tree records across multiple measurement years.
#'
#' @return A list containing:
#' \describe{
#'   \item{Trees}{The full dataset with status classification.}
#'   \item{Regeneration}{Trees first appearing after plot monitoring began.}
#'   \item{Last_Measurement}{Trees last measured before monitoring ended.}
#'   \item{Alive}{Trees present consistently or throughout.}
#'   \item{OrigPlotID1s}{Unique OrigPlotID1 identifiers.}
#' }
#'
#' @export
#'
#' @importFrom data.table data.table copy
#' @importFrom dplyr filter mutate group_by summarise left_join ungroup case_when
#' @importFrom magrittr %>%
#'

classify_tree_status <- function(Trees) {

  # Convert to data.table
  Trees <- copy(Trees)
  Trees <- as.data.table(Trees)

  # Determine first and last year of measurement per plot
  MeasureYear_interval_per_OrigPlotID1 <- Trees %>%
    group_by(OrigPlotID1) %>%
    summarise(
        first_OrigPlotID1_year = min(MeasureYear, na.rm = TRUE),
        last_OrigPlotID1_year = max(MeasureYear, na.rm = TRUE)
    )

  # Determine first and last year per tree
  MeasureYear_interval_per_TreeNumber <- Trees %>%
    group_by(OrigPlotID1, TreeNumber) %>%
    summarise(
        first_tree_year = min(MeasureYear, na.rm = TRUE),
        last_tree_year = max(MeasureYear, na.rm = TRUE)) %>%
    ungroup()

  #  Join OrigPlotID1-level data with tree-level data
  MeasureYear_interval_per_TreeNumber <- MeasureYear_interval_per_TreeNumber %>%
    left_join(MeasureYear_interval_per_OrigPlotID1, by = "OrigPlotID1")               # Adds the OrigPlotID1-level start and end years to each tree's record for comparison.

  # Merge intervals into main dataset
  Trees <- Trees %>%
    left_join(MeasureYear_interval_per_TreeNumber, by = c("OrigPlotID1", "TreeNumber"))   # Integrates tree-level and OrigPlotID1-level back into the original Trees dataset, preparing it for classification.

  # Assign status based on time intervals
  Trees <- Trees %>%
    mutate(status = case_when(           # Uses case_when() to define the status of each tree record
      MeasureYear == first_tree_year & first_tree_year > first_OrigPlotID1_year ~ "Regeneration", # the tree appears after the start of monitoring (not present initially).
      MeasureYear == last_tree_year & last_tree_year < last_OrigPlotID1_year ~ "Last Measurement", # the tree was not measured again in the last year of the OrigPlotID1, indicating possible death or removal.
      MeasureYear > first_tree_year & MeasureYear < last_tree_year ~ "Consistent",  # the tree appears between its first and last recorded years.
      TRUE ~ "Survival"   # a default label for all other conditions, mostly trees present across the full duration.
    ))

  # Subset by status
  Regeneration <- Trees %>% filter(status == "Regeneration")   # Regenerating trees.
  Last_Measurement <- Trees %>% filter(status == "Last Measurement")    # Trees last seen before final OrigPlotID1 year.
  Alive <- Trees %>% filter(status %in% c("Consistent", "Survival"))   # Trees with consistent or full-survival presence.

   #  Returns a list of datasets:Full tree data with status and Subsets by classification.
  return(list(
    Trees = Trees,
    Regeneration = Regeneration,
    Last_Measurement = Last_Measurement,
    Alive = Alive,
    OrigPlotID1s = unique(Trees$OrigPlotID1)
    ))
}







