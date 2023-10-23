# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.

# ---------------------------------------------------------
# renv::status()
# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(ggplot2))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest))
shhh(library(shinyGovstyle))
shhh(library(dplyr))
shhh(library(plotly))
shhh(library(DT))
shhh(library(metathis))
shhh(library(checkmate))
shhh(library(bslib))



# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(purrr))

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("R scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("R/", filetype = "r")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, test_scripts)
  return(script_changes)
}


site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"
site_overflow <- NA
sites_list <- c(site_primary) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "Key stage 4 performance" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised" # Update with parent publication link

# -----------------------------------------------------------------------------------------------------------------------------
# ---- Reading in the data ----
# -----------------------------------------------------------------------------------------------------------------------------

download_GCSE_Subjects_data <- read.csv("data/2023_Tidy_Data_Output_91_Scaled_Scores_Final.csv", stringsAsFactors = FALSE)
download_Combined_Science_data <- read.csv("data/2023_Tidy_Data_Output_Comb_Science_Scaled_Scores_Final.csv", stringsAsFactors = FALSE)
download_attainment_data <- read.csv("data/2023_Tidy_Data_Output_Attainment_Scaled_Scores_Final.csv", stringsAsFactors = FALSE)




# -----------------------------------------------------------------------------------------------------------------------------
# ---- Creating drop down lists ----
# -----------------------------------------------------------------------------------------------------------------------------

subject_dropdown <- download_GCSE_Subjects_data %>%
  select(subjects) %>%
  distinct() %>%
  add_row(subjects = "Combined Science") %>%
  arrange(subjects)

characteristic_dropdown <- download_GCSE_Subjects_data %>%
  select(characteristic_type) %>%
  distinct()


num_perc_dropdown <- list(
  "Number" = "Number of pupils",
  "Percentage" = "Percentage of pupils"
)


attainment_dropdown <- c(
  "EBacc Entry", "EBacc Achievement 9-4", "EBacc Achievement 9-5",
  "English & Mathematics Achievement 9-4", "English & Mathematics Achievement 9-5"
)

KS2_dropdown_attainment <- download_attainment_data %>%
  select(KS2_Prior) %>%
  distinct() %>%
  # arrange(KS2_Prior)%>%
  unlist(use.names = FALSE)



# -----------------------------------------------------------------------------------------------------------------------------
# ---- Subjects Table created from the tidy data csv file ----
# -----------------------------------------------------------------------------------------------------------------------------

## function to select the correct number / perc columns based on the user selection type
subject_col_selection <- function(data, num_perc) {
  if (num_perc == "Number of pupils") {
    data %>%
      select(KS2_Prior, characteristic_value, starts_with("num_"), "All Grades" = "all_grades", "Covid impacted" = "num_covid_impacted") %>%
      rename_at(vars(starts_with("num_")), list(~ sub("num_", "", .)))
  } else {
    data %>%
      select(KS2_Prior, characteristic_value, starts_with("perc_")) %>%
      rename_at(vars(starts_with("perc_")), list(~ sub("perc_", "% ", .)))
  }
}


# # Returns a table from the 9-1 subjects tidy data CSV
subject_table <- function(subj, char, num_perc) {
  if (subj == "Combined Science") {
    table <- download_Combined_Science_data %>%
      filter(characteristic_type == char) %>%
      # filter(LA_dropdown == LA_name) %>%
      subject_col_selection(., num_perc)
  } else {
    table <- download_GCSE_Subjects_data %>%
      filter(
        subjects == subj,
        characteristic_type == char
      ) %>%
      subject_col_selection(., num_perc)
  }

  return(table)
}


# -----------------------------------------------------------------------------------------------------------------------------
# ---- Attainment Table created from the tidy data csv file ----
# -----------------------------------------------------------------------------------------------------------------------------


## function to select the correct attainment columns based on the chosen attainment type
attainment_col_selection <- function(data, att_type) {
  data %>%
    select(KS2_Prior, characteristic_value, starts_with(att_type))
}

## function to re-format the column names to make them more reader friendly in the app
attainment_col_format <- function(data, att_type) {
  data %>%
    rename_at(vars(starts_with(att_type)), list(~ sub(att_type, "", .))) %>%
    rename_at(vars(starts_with("perc_")), list(~ sub("perc_", "% ", .))) %>%
    rename_all(function(x) gsub("_", " ", x)) %>%
    rename(characteristic_value = `characteristic value`)
}


## returns a table from the attainment tidy data CSV
attainment_table <- function(att, char) {
  table <- download_attainment_data %>%
    filter(characteristic_type == char)


  if (att == "EBacc Entry") {
    table <- table %>%
      attainment_col_selection(., "EBacc_all_") %>%
      attainment_col_format(., "EBacc_all_")
  } else if (att == "EBacc Achievement 9-4") {
    table <- table %>%
      attainment_col_selection(., "EBacc_9.4_") %>%
      attainment_col_format(., "EBacc_9.4_")
  } else if (att == "EBacc Achievement 9-5") {
    table <- table %>%
      attainment_col_selection(., "EBacc_9.5_") %>%
      attainment_col_format(., "EBacc_9.5_")
  } else if (att == "English & Mathematics Achievement 9-4") {
    table <- table %>%
      attainment_col_selection(., "Basics_9.4_") %>%
      attainment_col_format(., "Basics_9.4_")
  } else if (att == "English & Mathematics Achievement 9-5") {
    table <- table %>%
      attainment_col_selection(., "Basics_9.5_") %>%
      attainment_col_format(., "Basics_9.5_")
  }

  return(table)
}
