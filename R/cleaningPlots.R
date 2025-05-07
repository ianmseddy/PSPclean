#' Detect and Flag DBH Outliers
#'
#' @description
#' Flags implausible DBH values (static outliers) and unusual year-over-year DBH changes (dynamic outliers)
#' to help identify data issues in repeated tree measurements.
#'
#' @param Trees A `data.table` with columns: "DBH", "OrigPlotID1", "TreeNumber", and "MeasureYear".
#' @param maxDBHrealistic threshold for realistic DBH that is not an error
#'
#' @return A list containing:
#' \describe{
#'   \item{Trees}{The full data.table with `DBH_flag`, `DBH_growth`, and `Growth_flag`.}
#'   \item{Sapling}{Subset flagged as "Sapling".}
#'   \item{Impossible}{Subset flagged as "Impossible".}
#'   \item{Exceptional}{Subset flagged as "Exceptional".}
#'   \item{VerifyData}{Subset flagged as "VerifyData".}
#' }
#'
#' @export
#' @importFrom data.table set setcolorder fifelse
detect_DBH_outliers <- function(Trees, maxDBHrealistic = 400) {
  Trees <- as.data.table(Trees)

  # A: Static DBH flag (changed "Small_Sapling" to "Sapling")
  Trees[, DBH_flag := fifelse(DBH <= 0, "Invalid",
                              fifelse(DBH <= 5, "Sapling",
                                      fifelse(DBH > maxDBHrealistic, "Impossible",
                                              fifelse(DBH > 200, "Exceptional",
                                                      fifelse(DBH > 90 & DBH <= 200, "VerifyData", "OK")))))]

  # B: Sort for time sequence
  setorderv(Trees, cols = c("OrigPlotID1", "TreeNumber", "MeasureYear"))

  # C: Calculate DBH growth per tree over years
  Trees[, DBH_growth := c(NA, diff(DBH)) / c(NA, diff(MeasureYear)),
        by = .(OrigPlotID1, TreeNumber)]

  # D: Flag dynamic outliers
  Trees[, Growth_flag := fifelse(DBH_growth < -0.5, "Negative",
                                 fifelse(DBH_growth > 2.5, "TooFast", "OK"))]

  # E: Extract flagged subsets
  Sapling     <- Trees[DBH_flag == "Sapling"]
  Impossible  <- Trees[DBH_flag == "Impossible"]
  Exceptional <- Trees[DBH_flag == "Exceptional"]
  VerifyData  <- Trees[DBH_flag == "VerifyData"]

  return(list(
    Trees = Trees,
    Sapling = Sapling,
    Impossible = Impossible,
    Exceptional = Exceptional,
    VerifyData = VerifyData
  ))
}

#' Identify all inconsistencies, when a tree is linked to multiple Species Names in a OrigPlotID1
#'
#' @description
#' Ensures consistent tree numbering in cases where the same  Species location appears multiple times across years.
#' This helps in tracking individual trees over time and prevents confusion caused by duplicated or missing IDs.
#'
#' @param Trees A `data.table` of tree observations containing Species identifiers and measurement years.
#'
#' @return A `data.table` with updated and harmonized tree numbers across Species entries.
#'
#' @export
#'
#' @import data.table
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom magrittr %>%
#'
#  Identifies cases where a single TreeNumber within a OrigPlotID1 is associated with more than one Species, which indicates inconsistent or incorrect data entry.
#  Defines the treenum_to_multiplePSP() function that takes as input a data frame or tibble called Trees.
treenum_to_multiplePSP <- function(Trees) {
  Trees <- as.data.table(Trees)

  # Step 1: Identify trees assigned to multiple Species
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
    PSP_TREE_YIMO_corrected = final_trees_corrected
  ))
}

#' Process Implausible DBH Changes Across Measurement Years
#'
#' @description
#' Detects and manages inconsistencies in tree diameter (DBH) measurements across years within Species.
#' The function flags implausible negative growth, cleans the dataset accordingly, and excludes OrigPlotID1s with a high proportion of anomalies.
#' This helps improve the reliability of forest dynamics analysis.
#'
#' @param Trees A `data.table` of tree measurements over time within Species.
#'
#' @return A cleaned `data.table` with problematic entries removed or flagged.
#'
#' @export
#'
#' @import data.table
#' @importFrom dplyr group_by summarise filter mutate case_when n_distinct
#' @importFrom magrittr %>%
#`
process_dbh_issues <- function(Trees, OrigPlotID1s) {    # two arguments: Trees: a data frame containing tree measurements over time, and OrigPlotID1s: a list of OrigPlotID1 identifiers to focus the processing on.
  Trees <- Trees %>% filter(OrigPlotID1 %in% OrigPlotID1s)      # Filters the input data to include only the OrigPlotID1s listed in OrigPlotID1s, narrowing down the analysis to relevant data.

  # Sorts the data chronologically by OrigPlotID1, tree ID (TreeNumber), and measurement year (MeasureYear).
  # Within each tree’s record across years, calculates the difference in DBH (Diameter at Breast Height) between successive measurements to evaluate growth
  Trees <- Trees %>%
    arrange(OrigPlotID1, TreeNumber, MeasureYear) %>%
    group_by(OrigPlotID1, TreeNumber) %>%
    mutate(diff_dbh = DBH - lag(DBH))  # # A negative value indicates a reduction in DBH, which is biologically implausible in most cases and may indicate a measurement error.

  # Creates two diagnostic subsets:negative_growth and na_values
  negative_growth <- Trees %>% filter(diff_dbh < -0.5)  # trees showing suspicious negative growth greater than 0.5 cm (used as a threshold for likely error).
  na_values <- Trees %>% filter(is.na(diff_dbh))        # records where diff_dbh is NA (typically the first measurement for a tree).

  Trees <- Trees %>%
    mutate(DBH = ifelse(is.na(DBH) | DBH == 0, -1, DBH))   # Replaces missing (NA) or zero DBH values with -1 to flag them clearly as invalid values.

  dbh_issues <- Trees %>%
    filter(diff_dbh < 0) %>%                                                       # Filters out all records with negative growth for inspection.
    select(newSpeciesName, OrigPlotID1, MeasureID, TreeNumber, DBH, MeasureYear, diff_dbh, MeasNum) %>%       # Selects only relevant columns and arranges them for easy review.
    arrange(OrigPlotID1, TreeNumber, MeasureYear)

  dbh_check <- dbh_issues %>%
    left_join(Trees %>%   #   Joins dbh_issues with additional metadata (e.g., species name, cause of mortality, age class) to assist in diagnosing potential causes of measurement error.
                select(newSpeciesName, OrigPlotID1, MeasureID, TreeNumber, MeasureYear, Species, cause, agecl, MeasNum),
              by = c("newSpeciesName", "OrigPlotID1", "MeasureID", "TreeNumber", "MeasureYear"))

  # 📊 Calculate total negative growth percentage per OrigPlotID1
  negative_growth_summary <- Trees %>%        # Computes a summary statistic per OrigPlotID1:
    filter(!is.na(diff_dbh)) %>%
    group_by(OrigPlotID1) %>%
    summarise(
      total_neg_growth = sum(diff_dbh[diff_dbh < 0], na.rm = TRUE),                  # the sum of all negative DBH changes.
      total_growth = sum(abs(diff_dbh), na.rm = TRUE),                               # the sum of absolute DBH changes (positive and negative).
      neg_growth_pct = ifelse(total_growth > 0, 100 * abs(total_neg_growth) / total_growth, NA_real_) #  the percentage of negative growth relative to total measured change
    ) %>%
    arrange(OrigPlotID1)

  # Exports both the detailed issue log and the summary report to CSV files for further inspection or documentation
  write.csv(dbh_check, "dbh_issues.csv", row.names = FALSE)
  write.csv(negative_growth_summary, "negative_growth_summary.csv", row.names = FALSE)

  # ❌ Remove OrigPlotID1s with more than 5% negative growth
  OrigPlotID1s_to_keep <- negative_growth_summary %>%
    filter(neg_growth_pct <= 5 | is.na(neg_growth_pct)) %>% # Keeps only OrigPlotID1s where the negative DBH growth percentage is ≤ 5%, or missing (i.e., OrigPlotID1s with no DBH change data).
    pull(OrigPlotID1)

  Trees <- Trees %>% filter(OrigPlotID1 %in% OrigPlotID1s_to_keep) # Removes OrigPlotID1s with >5% negative DBH change from the final processed_data.

  # Returns a list containing:
  return(list(
    processed_data = Trees,      # the cleaned and filtered tree dataset.
    dbh_check = dbh_check,       # the table of detected DBH inconsistencies.
    negative_growth_summary = negative_growth_summary     # OrigPlotID1-level statistics on negative growth.
  ))
}
#' Classify tree status
#'
#' @description
#' This function assigns a status to each tree record based on its observation history across measurement years.
#' It identifies whether a tree is a new regeneration, was lost (due to mortality or removal), or persisted throughout the observation period.
#'
#' @param Trees A `data.table` of individual tree measurements within Permanent Sample Plots (PSPs).
#'
#' @return A list or `data.table` categorizing each tree as "Regeneration", "Mortality", or "Alive".
#'
#' @export
#'
#' @importFrom data.table data.table
#' @importFrom dplyr filter mutate group_by summarise left_join ungroup case_when
#' @importFrom magrittr %>%
#'
# Defines a function named classify_tree_status() that takes a single argument Trees, which is a data frame containing repeated tree measurements (e.g., Species data).
classify_tree_status <- function(Trees) {
  MeasureYear_interval_per_OrigPlotID1 <- Trees %>%                    # Determine OrigPlotID1-level measurement interval, that defines the total monitoring duration for each OrigPlotID1.
    group_by(OrigPlotID1) %>%
    summarise(first_OrigPlotID1_year = min(MeasureYear, na.rm = TRUE), # Finds the earliest and latest measurement years.
              last_OrigPlotID1_year = max(MeasureYear, na.rm = TRUE))

  MeasureYear_interval_per_TreeNumber <- Trees %>%                          # Determine tree-level measurement interval, that shows the observation window for each individual tree.
    group_by(OrigPlotID1, TreeNumber) %>%
    summarise(first_tree_year = min(MeasureYear, na.rm = TRUE),          # Finds the first and last years the tree was measured.
              last_tree_year = max(MeasureYear, na.rm = TRUE)) %>%
    ungroup()

  #  Join OrigPlotID1-level data with tree-level data
  MeasureYear_interval_per_TreeNumber <- MeasureYear_interval_per_TreeNumber %>%
    left_join(MeasureYear_interval_per_OrigPlotID1, by = "OrigPlotID1")               # Adds the OrigPlotID1-level start and end years to each tree's record for comparison.

  # Add the joined year info to the main dataset
  Trees <- Trees %>%
    left_join(MeasureYear_interval_per_TreeNumber, by = c("OrigPlotID1", "TreeNumber"))   # Integrates tree-level and OrigPlotID1-level back into the original Trees dataset, preparing it for classification.

  #  Classify tree status
  Trees <- Trees %>%
    mutate(status = case_when(           # Uses case_when() to define the status of each tree record
      MeasureYear == first_tree_year & first_tree_year > first_OrigPlotID1_year ~ "Regeneration", # the tree appears after the start of monitoring (not present initially).
      MeasureYear == last_tree_year & last_tree_year < last_OrigPlotID1_year ~ "Last Measurement", # the tree was not measured again in the last year of the OrigPlotID1, indicating possible death or removal.
      MeasureYear > first_tree_year & MeasureYear < last_tree_year ~ "Consistent",  # the tree appears between its first and last recorded years.
      TRUE ~ "Survival"   # a default label for all other conditions, mostly trees present across the full duration.
    ))

  # Create status-based subsets
  # Divides the classified dataset into three categories for later analysis or visualization:
  Regen <- Trees %>% filter(status == "Regeneration")   # Regenerating trees.
  Last_Measurement <- Trees %>% filter(status == "Last Measurement")    # Trees last seen before final OrigPlotID1 year.
  Alive <- Trees %>% filter(status %in% c("Consistent", "Survival"))   # Trees with consistent or full-survival presence.

  # Writes the full Trees dataset with the newly classified status column to a CSV file
  write.csv(Trees, "Status of trees due to be Alive, Last Measurement and Regeneration", row.names = FALSE)

  #  Returns a list of datasets:Full tree data with status and Subsets by classification.
  return(list(Trees = Trees, Regen = Regen, Last_Measurement = Last_Measurement, Alive = Alive))
}







