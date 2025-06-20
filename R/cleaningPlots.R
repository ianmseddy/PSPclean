#'
#' -----------------------------------------------------------   detect_dbh_outliers function()  ----------------------------------------------
#' Detect and Flag DBH Outliers using Quantile Regression
#'
#' @description
#' Flags implausible DBH values (static outliers) using quantile regression,
#' to help identify data issues in repeated tree measurements.
#'
#' @param Trees A `data.table` or `data.frame` with columns: "DBH", "OrigPlotID1", "TreeNumber", "MeasureYear", etc.
#' @param lower_tau Lower quantile threshold (default = 0.05)
#' @param upper_tau Upper quantile threshold (default = 0.95)
#'
#' @return A list containing:
#' \describe{
#'   \item{Trees}{The full data.table with `outlier_5th`, `outlier_95th`, and `outlier_type`.}
#'   \item{OrigPlotID1s}{Unique `OrigPlotID1` identifiers.}
#' }
#'
#' @export
#' @importFrom data.table as.data.table setorderv fifelse copy
#' @importFrom quantreg rq
#'

detect_dbh_outliers <- function(Trees, lower_tau = 0.05, upper_tau = 0.95) {
  # Load required package
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is required. Please install it.")
  }

  # Convert to data.table
  Trees <- data.table::copy(Trees)

  # Check required columns
  required_cols <- c("MeasureID", "OrigPlotID1", "MeasureYear", "TreeNumber",
                     "Species", "DBH", "newSpeciesName", "source")
  missing_cols <- setdiff(required_cols, names(Trees))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Remove rows with missing DBH or predictors
  Trees <- Trees[!is.na(DBH) & !is.na(newSpeciesName) &
                   !is.na(MeasureYear) & !is.na(OrigPlotID1)]

  # Convert categorical variables to factors
  Trees[, newSpeciesName := as.factor(newSpeciesName)]
  Trees[, OrigPlotID1 := as.factor(OrigPlotID1)]

  # Define formula
  formula <- DBH ~ newSpeciesName + MeasureYear

  # Fit quantile regressions
  fit_low <- quantreg::rq(formula, data = Trees, tau = lower_tau, method = "fn")
  fit_high <- quantreg::rq(formula, data = Trees, tau = upper_tau, method = "fn")

  # Predict bounds
  Trees[, predicted_5th := predict(fit_low, newdata = Trees)]
  Trees[, predicted_95th := predict(fit_high, newdata = Trees)]

  # Flag outliers
  Trees[, outlier_5th := DBH < predicted_5th]
  Trees[, outlier_95th := DBH > predicted_95th]
  Trees[, outlier_type := fifelse(outlier_5th, "low",
                                  fifelse(outlier_95th, "high", "none"))]

  # Return list
  return(list(
    Trees = Trees,
    OrigPlotID1s = unique(Trees$OrigPlotID1)
  ))
}

#' #------------------------------------------------------------  treenum_to_multiplePSP function()   ----------------------------------------------------------
#' Identify Tree Numbers Linked to Multiple Species Names in a Plot
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
#' @importFrom dplyr n distinct n_distinct row_number arrange slice_max group_by mutate ungroup filter summarise case_when semi_join anti_join inner_join bind_rows select
#' @importFrom magrittr %>%
#'
treenum_to_multiplePSP <- function(Trees) {
  Trees <- copy(Trees)
  Trees <- as.data.table(Trees)

  # Step 1: Identify a tree number assigned to multiple Species
  incorrect_trees <- Trees %>%
    group_by(OrigPlotID1, TreeNumber) %>%
    filter(n_distinct(newSpeciesName) > 1) %>%
    ungroup()

  # Step 2: Identify correct species by most frequent combination
  correct_species <- incorrect_trees %>%
    group_by(OrigPlotID1, TreeNumber, newSpeciesName, Species) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count)) %>%
    group_by(OrigPlotID1, TreeNumber) %>%
    slice_max(count, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(OrigPlotID1, TreeNumber, newSpeciesName, Species)

  # Step 3: Identify non-positive DBH growth for possible regeneration or disappearance
  growth_info <- incorrect_trees %>%
    group_by(OrigPlotID1, TreeNumber, newSpeciesName, Species) %>%
    arrange(MeasureYear) %>%
    summarise(diff_DBH = last(DBH) - first(DBH), .groups = "drop")

  non_positive_growth <- growth_info %>%
    filter(diff_DBH <= 0)

  Trees_subset <- Trees %>%
    semi_join(non_positive_growth, by = c("OrigPlotID1", "TreeNumber", "newSpeciesName", "Species"))

  dbh_range <- Trees_subset %>%
    group_by(OrigPlotID1) %>%
    summarise(min_DBH = min(DBH), max_DBH = max(DBH), .groups = "drop")

  Trees_classified <- Trees_subset %>%
    left_join(dbh_range, by = "OrigPlotID1") %>%
    group_by(OrigPlotID1) %>%
    mutate(
      new_TreeNumber = ifelse(DBH == min_DBH, max(Trees$TreeNumber) + row_number(), TreeNumber),
      status = case_when(
        DBH == min_DBH ~ "Regeneration",
        DBH == max_DBH ~ "Last_measurement",
        TRUE ~ "Unclassified"
      )
    ) %>%
    ungroup()

  regeneration_data <- Trees_classified %>%
    filter(status == "Regeneration") %>%
    mutate(TreeNumber = new_TreeNumber) %>%
    select(-min_DBH, -max_DBH, -new_TreeNumber)

  last_measurement_data <- Trees_classified %>%
    filter(status == "Last_measurement") %>%
    select(-min_DBH, -max_DBH, -new_TreeNumber)

  # Step 4: Identify all rows part of correct species combo
  correct_species_data <- Trees %>%
    inner_join(correct_species, by = c("OrigPlotID1", "TreeNumber")) %>%
    mutate(
      newSpeciesName = newSpeciesName.y,
      Species = Species.y,
      status = "Correct_Species"
    ) %>%
    select(-newSpeciesName.x, -newSpeciesName.y, -Species.x, -Species.y)

  # Step 5: Merge all flagged incorrect rows
  incorrect_data <- bind_rows(
    regeneration_data %>% mutate(status = "Regeneration"),
    last_measurement_data %>% mutate(status = "Last_measurement"),
    correct_species_data %>% filter(!(OrigPlotID1 %in% regeneration_data$OrigPlotID1 & TreeNumber %in% regeneration_data$TreeNumber) &
                                      !(OrigPlotID1 %in% last_measurement_data$OrigPlotID1 & TreeNumber %in% last_measurement_data$TreeNumber)),
    incorrect_trees %>% anti_join(correct_species_data, by = c("OrigPlotID1", "TreeNumber", "newSpeciesName", "MeasureYear")) %>%
      anti_join(regeneration_data, by = c("OrigPlotID1", "TreeNumber", "newSpeciesName", "MeasureYear")) %>%
      anti_join(last_measurement_data, by = c("OrigPlotID1", "TreeNumber", "newSpeciesName", "MeasureYear")) %>%
      mutate(status = "Other_incorrect")
  ) %>%
    distinct(OrigPlotID1, TreeNumber, newSpeciesName, MeasureYear, .keep_all = TRUE)

  # Step 6: Remove incorrect data from main table
  clean_trees <- Trees %>%
    anti_join(incorrect_data, by = c("OrigPlotID1", "TreeNumber", "newSpeciesName", "MeasureYear"))

  # Step 7: Final corrected dataset
  final_trees_corrected <- bind_rows(
    clean_trees,
    correct_species_data,
    regeneration_data,
    last_measurement_data
  ) %>%
    arrange(OrigPlotID1, TreeNumber, MeasureYear)

  return(list(
    incorrect_data = incorrect_data,
    correct_species = correct_species,
    regeneration = regeneration_data,
    last_measurement = last_measurement_data,
    Trees_corrected = final_trees_corrected,
    OrigPlotID1s = unique(Trees$OrigPlotID1)
  ))
}
#'
#' #-------------------------------------------------------------   process_dbh_issues function()    ----------------------------------------------------------
#' Process Implausible DBH Changes Across Measurement Years
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
#' @import data.table
#' @importFrom dplyr group_by summarise filter mutate select left_join arrange pull
#' @importFrom magrittr %>%
#`
process_dbh_issues <- function(Trees) {


  # Sort and compute DBH differenc
  Trees <- Trees %>%
    arrange(OrigPlotID1, TreeNumber, MeasureYear) %>%
    group_by(OrigPlotID1, TreeNumber) %>%
    mutate(diff_dbh = DBH - lag(DBH))  #

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
    filter(!is.na(diff_dbh)) %>%
    group_by(OrigPlotID1) %>%
    summarise(
      total_neg_growth = sum(diff_dbh[diff_dbh < 0], na.rm = TRUE),                  # the sum of all negative DBH changes.
      total_growth = sum(abs(diff_dbh), na.rm = TRUE),                               # the sum of absolute DBH changes (positive and negative).
      neg_growth_pct = ifelse(total_growth > 0, 100 * abs(total_neg_growth) / total_growth, NA_real_) #  the percentage of negative growth relative to total measured change
    ) %>%
    arrange(OrigPlotID1)

  #  Filter by acceptable negative growth threshold  (
  OrigPlotID1s_to_keep <- negative_growth_summary %>%
    filter(neg_growth_pct <= 5 | is.na(neg_growth_pct)) %>% # Keeps only OrigPlotID1s where the negative DBH growth percentage is ≤ 5%, or missing (i.e., OrigPlotID1s with no DBH change data).
    pull(OrigPlotID1)

  Trees_corrected <- Trees %>% filter(OrigPlotID1 %in% OrigPlotID1s_to_keep) # Removes OrigPlotID1s with >5% negative DBH change from the final processed_data.

  # Returns a list containing:
  return(list(
    Trees = Trees_corrected,      # the cleaned and filtered tree dataset.
    dbh_check = dbh_check,       # the table of detected DBH inconsistencies.
    negative_growth_summary = negative_growth_summary,     # OrigPlotID1-level statistics on negative growth.
    OrigPlotID1s = OrigPlotID1s_to_keep
  ))
}

#' #--------------------------------------------------------------------   Classify tree status function() ---------------------------------------------------------------------
#'
#' Classify Tree Status Based on Measurement History
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
#' @importFrom data.table data.table
#' @importFrom dplyr filter mutate group_by summarise left_join ungroup case_when
#' @importFrom magrittr %>%
#'

classify_tree_status <- function(Trees) {
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







