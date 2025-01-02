#---- Loading in KS4 data ----


# These are the packages needed for this code, you may already have these installed but run this section if not
shhh(library(dplyr))
shhh(library(odbc))
shhh(library(DBI))
shhh(library(janitor))
shhh(library(tidyverse))
shhh(library(purrr))

pull_latest_data <- function() {
  # Load libraries==from here


  # round function
  round2 <- function(x, n) {
    posneg <- sign(x)
    z <- abs(x) * 10^n
    z <- z + 0.5
    z <- trunc(z)
    z <- z / 10^n
    z * posneg
  }




  # Connect to the SQL server
  SQL_con <- dbConnect(odbc(),
    Driver = "SQL Server",
    Server = "VMT1PR-DHSQL02",
    Database = "KS4_RESTRICTED", # Enter the folder containing your SQL data
    Trusted_Connection = "True"
  )




  # STEP 1
  # Import the exam data
  SQL_Statement <- "SELECT
exam.CANDNO
,WOLF_DISC_CODE
,exam.LAESTAB
,SUBLEVNO
,GRADE
,GNUMBER
,COVID_IMPACTED_FLAG
FROM [ks4_restricted].[dbo].[ks4_exam_24_cohort_amended_v2] exam
LEFT JOIN [ks4_restricted].[dbo].[ks4_pupil_24_cohort_amended_v2] pupil
ON exam.laestab = pupil.laestab and exam.candno = pupil.candno
WHERE PTQ_INCLUDE = 1
AND disc3B_ptq_ee = 0
AND sublevno in ('310', '391', '395', '450', '451', '760', '954')
AND ENDKS = 1
AND NATRES = 1"
  Exam_SQL_data <- dbGetQuery(SQL_con, SQL_Statement)

  # Import the exam data
  # Exam_SQL_data <- tbl(SQL_con, ("KS4_exam_23_cohort_amended_v3"))%>% # Enter the name of your SQL database #update file name
  #    as.data.frame() %>%

  # Create subject groupings based on wolf codes from the exam file
  #  Exam_SQL_data <- SQL_data %>%
  # filter(
  #   ptq_include == 1,
  #   disc3B_ptq_ee == 0,
  #   sublevno %in% c("310", "391", "395", "450", "451", "760", "954")
  # ) %>%
  # select(
  #   CANDNO,
  #   WOLF_DISC_CODE,
  #   LAESTAB,
  #   SUBLEVNO,
  #   GRADE,
  #   GNUMBER,
  #   COVID_IMPACTED_FLAG
  # ) %>%

  Exam_SQL_data <- Exam_SQL_data %>%
    mutate(subjects = case_when(
      WOLF_DISC_CODE == "FK2B" ~ "English Language",
      WOLF_DISC_CODE == "FC4" ~ "English Literature",
      (WOLF_DISC_CODE == "RB1" | WOLF_DISC_CODE == "RB15" | WOLF_DISC_CODE == "RB1A" | WOLF_DISC_CODE == "RB1B" | WOLF_DISC_CODE == "RB31" | WOLF_DISC_CODE == "RB55" |
        WOLF_DISC_CODE == "RB156" | WOLF_DISC_CODE == "RB71" | WOLF_DISC_CODE == "RB7B" | WOLF_DISC_CODE == "RB7E") & !(GNUMBER %in% c("10034912", "10060054", "10060091", "10060133", "10060170", "10060212", "1006025X", "10050395", "60310844", "60311770")) ~ "Mathematics",
      WOLF_DISC_CODE == "RA1E" ~ "Combined Science",
      WOLF_DISC_CODE == "RC1" ~ "Physics",
      WOLF_DISC_CODE == "RD1" ~ "Chemistry",
      WOLF_DISC_CODE == "RH3" ~ "Biology",
      WOLF_DISC_CODE == "CK1" ~ "Computer Science",
      WOLF_DISC_CODE == "RE1" | WOLF_DISC_CODE == "RF2" | WOLF_DISC_CODE == "RC52" |
        WOLF_DISC_CODE == "QA3" ~ "Other Sciences",
      WOLF_DISC_CODE == "VF1" ~ "Design and Technology",
      WOLF_DISC_CODE == "XA5A" ~ "D & T: Textiles Technology",
      WOLF_DISC_CODE == "VF2" | WOLF_DISC_CODE == "VF3" ~ "Other Design and Technology",
      WOLF_DISC_CODE == "XA1" ~ "Engineering",
      WOLF_DISC_CODE == "AA3" ~ "Business",
      WOLF_DISC_CODE == "NH6" ~ "Food Preparation and Nutrition",
      WOLF_DISC_CODE == "RF4" ~ "Geography",
      WOLF_DISC_CODE == "DB" ~ "History",
      WOLF_DISC_CODE == "DB21" ~ "Ancient History",
      WOLF_DISC_CODE == "EB" ~ "Economics",
      WOLF_DISC_CODE == "DE1" | WOLF_DISC_CODE == "EA" | WOLF_DISC_CODE == "EE31" | WOLF_DISC_CODE == "PK1" | WOLF_DISC_CODE == "EE2" ~ "Social Studies",
      WOLF_DISC_CODE == "FKF" ~ "French",
      WOLF_DISC_CODE == "FKG" ~ "German",
      WOLF_DISC_CODE == "FKS" ~ "Spanish",
      WOLF_DISC_CODE == "FKO" | WOLF_DISC_CODE == "FKQ" | WOLF_DISC_CODE == "F1H" | WOLF_DISC_CODE == "FKB" | WOLF_DISC_CODE == "F1P" | WOLF_DISC_CODE == "FKP" | WOLF_DISC_CODE == "FKI" |
        WOLF_DISC_CODE == "FKJ" | WOLF_DISC_CODE == "FKM" | WOLF_DISC_CODE == "FKK" | WOLF_DISC_CODE == "FKU" | WOLF_DISC_CODE == "FKX" | WOLF_DISC_CODE == "FKC" |
        WOLF_DISC_CODE == "FKR" | WOLF_DISC_CODE == "FKT" ~ "Other Modern Languages",
      WOLF_DISC_CODE == "F1L" ~ "Latin",
      WOLF_DISC_CODE == "DB2B" ~ "Classical Civilisation",
      WOLF_DISC_CODE == "F1K" ~ "Classical Greek",
      WOLF_DISC_CODE == "F1Z" ~ "Biblical Hebrew",
      WOLF_DISC_CODE == "JA2" | WOLF_DISC_CODE == "KJ1" ~ "Art and Design",
      WOLF_DISC_CODE == "KA2" & !(GNUMBER %in% c("60311150", "60311502", "60319434", "60320692", "60321052", "60322469")) ~ "Film Studies",
      WOLF_DISC_CODE == "LB1" ~ "Dance",
      WOLF_DISC_CODE == "LC11" ~ "Drama",
      WOLF_DISC_CODE == "PA1" ~ "Health and Social Care",
      WOLF_DISC_CODE == "KA2" & !(GNUMBER %in% c("10042799", "50022465", "50025995", "50030188", "60055029", "60308898", "60309702", "60309714", "60309726")) ~ "Media/Film/TV",
      WOLF_DISC_CODE == "LF1" | WOLF_DISC_CODE == "LJ9" ~ "Music",
      WOLF_DISC_CODE == "MA1" ~ "Physical Education",
      WOLF_DISC_CODE == "DD1" ~ "Religious Studies",
      WOLF_DISC_CODE == "RB71" & !(GNUMBER %in% c("60322615")) ~ "Statistics",
      WOLF_DISC_CODE == "AK6" ~ "Accounting",
      TRUE ~ NA_character_
    )) %>%
    mutate(GRADE = ifelse(COVID_IMPACTED_FLAG == "1" & !(GRADE %in% c("Q")), "covid impacted", GRADE)) # %>%
  #  mutate(GRADE = case_when(
  #   COVID_IMPACTED_FLAG == "1" & !(GRADE %in% c("Q")) ~"covid_impacted",
  #  TRUE ~ NA_character_
  # )) %>%

  # as.data.frame() # need to make it a dataframe so you can do the join lower down in step 4. You can then view it too in your environment on the right



  # count the number of times each subject is in the data
  Exam_SQL_data %>%
    count(subjects) %>%
    arrange(subjects)

  # count the number of pupils doing each subject and so removes duplicates,
  # this does not match the SQL output
  # Exam_SQL_data %>% select(CANDNO, subjects) %>% distinct() %>% count(subjects) %>%
  # arrange(subjects)

  # count the number of times each subject is in the data
  Exam_SQL_data %>%
    group_by(subjects) %>%
    summarise(n = n()) %>%
    arrange(subjects)




  # STEP 2 Define variables

  # Import the pupil data
  SQL_Statement <- "SELECT
  CANDNO, URN, LAESTAB, NFTYPE, SEX, EAL, KS2EMSS, L2BASICS_94,
  L2BASICS_95, EBACC_94, EBACC_95, EBACC_E_PTQ_EE, ATT8, P8SCORE, disadvantage, sen_description, LA_name

FROM [ks4_restricted].[dbo].[KS4_tidy_data_pupil_24_cohort_amended_v2]
WHERE
 ENDKS = 1
 AND natmtdres = 1
AND NFTYPE in (20,21,22,23,24,25,26,27,31,50,51,52,53,55,57,58)"



  Pupil_SQL_data <- dbGetQuery(SQL_con, SQL_Statement)

  # Import the pupil data
  # Pupil_SQL_data <- tbl(SQL_con, ("KS4_pupil_23_cohort_amended_v3"))  %>% # Enter the name of your SQL database #update file name
  # as.data.frame()
  #
  # Var_SQL_data <- Pupil_SQL_data %>%
  #   filter(
  #     endks == 1,
  #     natres == 1,
  #     (nftype %in% c("32", "33") | nftype %in% c("20", "21", "22", "23", "24", "25", "26", "27", "28", "31", "50", "51", "52", "53", "55", "57", "58") &
  #       norflage != 3)
  #   ) %>% # you can use norflage !=3 instead here :)
  #   select(
  #     CANDNO, URN, LAESTAB, NFTYPE, Sex, LANG1ST, KS2EMSS, L2BASICS_94,
  #     L2BASICS_95, EBACC_94, EBACC_95, EBACC_E_PTQ_EE, ATT8, P8SCORE, FSM6CLA1A, SENF
  #   ) %>%
  #  as.data.frame() %>% # Handy to cast it as a dataframe so you can view what's happening by clicking on the table in your environment on the right

  Pupil_SQL_data <- Pupil_SQL_data %>%
    mutate(Disadvantage = case_when(
      disadvantage == "Disadvantaged all other" ~ "Non Disadvantaged Pupils",
      disadvantage == "Disadvantaged" ~ "Disadvantaged Pupils",
      TRUE ~ NA_character_
    )) %>%
    mutate(Sex = case_when(
      SEX == "Girls" ~ "Female Pupils",
      SEX == "Boys" ~ "Male Pupils",
      TRUE ~ NA_character_
    )) %>%
    mutate(SEN = case_when(
      sen_description == "Any SEN" ~ "SEN Pupils",
      sen_description == "No identified SEN" ~ "Non SEN Pupils",
      TRUE ~ NA_character_
    )) %>%
    mutate(EAL = case_when(
      EAL == "Other than English" ~ "EAL Pupils", #
      EAL == "English" ~ "Non EAL Pupils",
      TRUE ~ NA_character_
    )) %>%
    mutate(ks2em = case_when(
      KS2EMSS >= 59 & KS2EMSS < 80 ~ "Less than 80",
      KS2EMSS >= 80 & KS2EMSS <= 89.5 ~ "80 - 89.5",
      KS2EMSS >= 90 & KS2EMSS <= 95.5 ~ "90 - 95.5",
      KS2EMSS >= 96 & KS2EMSS <= 99.5 ~ "96 - 99.5",
      KS2EMSS >= 100 & KS2EMSS <= 102 ~ "100 - 102",
      KS2EMSS >= 102.5 & KS2EMSS <= 104.5 ~ "102.5 - 104.5",
      KS2EMSS >= 105 & KS2EMSS <= 107 ~ "105 - 107",
      KS2EMSS >= 107.5 & KS2EMSS <= 109.5 ~ "107.5 - 109.5",
      KS2EMSS >= 110 & KS2EMSS <= 113 ~ "110 - 113",
      KS2EMSS >= 113.5 & KS2EMSS <= 116.5 ~ "113.5 - 116.5",
      KS2EMSS >= 117 & KS2EMSS <= 120 ~ "117 - 120",
      TRUE ~ NA_character_
    )) %>%
    mutate(ks2em_band = case_when(
      KS2EMSS >= 59 & KS2EMSS < 80 ~ "01", # this section makes sure the data is ordered by KS2 groups in the final tidy data files
      KS2EMSS >= 80 & KS2EMSS <= 89.5 ~ "02",
      KS2EMSS >= 90 & KS2EMSS <= 95.5 ~ "03",
      KS2EMSS >= 96 & KS2EMSS <= 99.5 ~ "04",
      KS2EMSS >= 100 & KS2EMSS <= 102 ~ "05",
      KS2EMSS >= 102.5 & KS2EMSS <= 104.5 ~ "06",
      KS2EMSS >= 105 & KS2EMSS <= 107 ~ "07",
      KS2EMSS >= 107.5 & KS2EMSS <= 109.5 ~ "08",
      KS2EMSS >= 110 & KS2EMSS <= 113 ~ "09",
      KS2EMSS >= 113.5 & KS2EMSS <= 116.5 ~ "10",
      KS2EMSS >= 117 & KS2EMSS <= 120 ~ "11",
      TRUE ~ NA_character_
    ))



  # # CHECKS
  # Var_SQL_data %>%
  #   count(LANG1ST) %>%
  #   arrange(LANG1ST)
  #
  # Pupil_SQL_data %>%
  #   count(Disadvantage) %>%
  #   arrange(Disadvantage)
  #
  # Pupil_SQL_data %>%
  #   count(EAL) %>%
  #   arrange(EAL)
  #
  # Pupil_SQL_data %>%
  #   count(SEN) %>%
  #   arrange(SEN)
  #
  # Pupil_SQL_data %>%
  #   count(Sex) %>%
  #   arrange(Sex)
  #
  # Pupil_SQL_data %>%
  #   count(Sex, Disadvantage, EAL, SEN) %>%
  #   group_by(ks2em)
  #
  # Pupil_SQL_data %>%
  #   group_by(ks2em) %>%
  #   count(Sex, Disadvantage, EAL, SEN)



  # STEP 3 Join the exam and pupil data

  # Create a shorter version of the exam dataframe containing only the variables you need for the join

  Exam_short <- Exam_SQL_data %>%
    select(
      CANDNO,
      LAESTAB,
      WOLF_DISC_CODE,
      SUBLEVNO,
      GRADE,
      subjects
    )

  # Create a dataframe containing the subjects to be included in the TMs, you need to check with the Data Production Team to see if any subjects have changed
  ks4_subjects <- c(
    "Ancient History", "Art and Design", "Biblical Hebrew", "Biology", "Business", "Chemistry", "Classical Civilisation", "Classical Greek", # , "Combined Science",
    "Computer Science", "Dance", "Design and Technology", "Drama", "Economics", "Engineering", "English Language", "English Literature", "Film Studies",
    "Food Preparation and Nutrition", "French", "Geography", "German", "History", "Latin", "Mathematics", "Media/Film/TV", "Music",
    "Other Modern Languages", "Other Sciences", "Physical Education", "Physics", "Religious Studies", "Social Studies", "Spanish", "Statistics"
  )

  ## combined science has its own output below due to double award

  #################################################################################################
  ##################                    9-1 subjects tidy data                    #################
  #################################################################################################

  # STEP 4 Tidy Data for subjects graded 9-1

  join_data <- Pupil_SQL_data %>%
    left_join(Exam_short,
      by = c("CANDNO", "LAESTAB")
    ) %>%
    filter(subjects %in% ks4_subjects) %>%
    drop_na(ks2em)

  # a <- join_data %>% filter(GRADE == 'covid impacted')


  #####################################################################################
  func_counts_char <- function(data, char, char_name) {
    data %>%
      count(GRADE, subjects, ks2em_band, ks2em, {{ char }}) %>%
      rename(characteristic_value = {{ char }}) %>%
      mutate(characteristic_type = char_name)
  }

  func_counts_all <- function(data) {
    data %>%
      count(GRADE, subjects, ks2em_band, ks2em) %>%
      mutate(characteristic_value = "All Pupils") %>%
      mutate(characteristic_type = "All Pupils")
  }


  # This section counts the number of grades in each breakdown and All Pupils
  grade_counts_Sex <- func_counts_char(join_data, Sex, "Sex")
  grade_counts_sen <- func_counts_char(join_data, SEN, "SEN")
  grade_counts_eal <- func_counts_char(join_data, EAL, "EAL")
  grade_counts_disadvantage <- func_counts_char(join_data, Disadvantage, "Disadvantage")
  grade_counts_all <- func_counts_all(join_data)


  # This combines the count dataframes
  grade_counts_comb <- rbind(
    grade_counts_Sex, grade_counts_sen, grade_counts_eal,
    grade_counts_disadvantage, grade_counts_all
  )


  # creates the separate grade columns
  grade_counts_spread <- grade_counts_comb %>%
    pivot_wider(names_from = GRADE, values_from = n) %>%
    select(-Q) %>%
    rename("covid_impacted" = "covid impacted")

  # Calculates the percentages
  grade_percentages_spread <- grade_counts_spread %>%
    # select(-X) %>% # removes X from the % calculation
    janitor::adorn_percentages() %>%
    mutate_if(is.numeric, function(x) {
      round2(x * 100, 1)
    }) %>%
    rename(
      "perc_U" = "U",
      "perc_1" = "1",
      "perc_2" = "2",
      "perc_3" = "3",
      "perc_4" = "4",
      "perc_5" = "5",
      "perc_6" = "6",
      "perc_7" = "7",
      "perc_8" = "8",
      "perc_9" = "9",
      "perc_X" = "X",
      "per_covid_impacted" = "covid_impacted"
    )

  # Creates the final tidy data
  tidy_data_subjects <- grade_counts_spread %>%
    left_join(grade_percentages_spread, by = c(
      "subjects", "ks2em", "ks2em_band",
      "characteristic_type", # comment back for app use,
      "characteristic_value"
    )) %>%
    mutate(
      time_period = 202324,
      time_identifier = "Academic year",
      geographic_level = "National",
      country_code = "E92000001",
      country_name = "England",
      version = "Provisional",
      all_grades = rowSums(.[, c("U", "1", "2", "3", "4", "5", "6", "7", "8", "9", "X", "covid_impacted")], na.rm = TRUE)
    ) %>%
    arrange(
      characteristic_type, # comment back for app use,
      characteristic_value, subjects, ks2em_band
    ) %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, # LA_name,
      version,
      characteristic_type, # comment back for app use,
      characteristic_value, subjects,
      KS2_Prior = ks2em,
      ## the grades below produce data files output when running app this need to be commented out and the section 2 commented back in.
      # '1' = '1', '2'= '2', '3' = '3', '4' = '4', '5' = '5', '6' = '6', '7' = '7', '8' = '8', '9' = '9', 'U' = 'U', 'X' = 'X', 'all_grades',
      #' %'='perc_1','%_2'='perc_2','%_3' ='perc_3', '%_4'='perc_4',' %_5'='perc_5', '%_6'='perc_6', '%_7'='perc_7', '%_8'='perc_8',
      #' %_9'='perc_9','%_U'='perc_U' ,'%_X'='perc_X', 'covid_impacted' ) %>% ##comment out for app

      ### (section2 for app use)
      "num_U" = "U", "num_1" = "1", "num_2" = "2", "num_3" = "3", "num_4" = "4", "num_5" = "5", "num_6" = "6",
      "num_7" = "7", "num_8" = "8", "num_9" = "9", "num_X" = "X", "num_covid_impacted" = "covid_impacted", "all_grades", "perc_1", "perc_2", "perc_3", "perc_4",
      "perc_5", "perc_6", "perc_7", "perc_8", "perc_9", "perc_U", "perc_X", "per_covid_impacted"
    ) %>% ## comment back for app
    mutate_all(~ replace(., is.na(.), 0))


  # Remove zero values from the characteristic_value column
  tidy_data_subjects <- tidy_data_subjects %>%
    filter(characteristic_value != 0)


  # copying data to an Excel file
  # save_tidy_data_file = 'Y:/Pre-16 development/Routine products/Transition Matrices/TM Dev/8.TM_in_R/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Scaled_Scores_Final.csv'
  # save_tidy_data_file = 'C:/Users/SMANCHESTER.AD/OneDrive - Department for Education/Documents/R Projects/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Scaled_Scores_Final.csv'
  save_tidy_data_file_subjects <- "C:/Users/MPARMAR/OneDrive - Department for Education/Documents/2024_publication/R/TM/Transition-matrices-dashboard/data/2024_Tidy_Data_Output_91_Scaled_Scores_Final_v2.csv" # update year
  write.table(tidy_data_subjects, save_tidy_data_file_subjects, row.names = FALSE, sep = ",")



  #################################################################################################
  ##################                 Combined Science tidy data                   #################
  #################################################################################################

  # STEP 5 Tidy Data for Combined Science

  join_data_cs <- Pupil_SQL_data %>%
    left_join(Exam_short,
      by = c("CANDNO", "LAESTAB")
    ) %>%
    filter(subjects == "Combined Science") %>%
    drop_na(ks2em)


  func_counts_char <- function(data, char, char_name) {
    data %>%
      count(GRADE, subjects, ks2em_band, ks2em, {{ char }}) %>%
      rename(characteristic_value = {{ char }}) %>%
      mutate(characteristic_type = char_name)
  }

  func_counts_all <- function(data) {
    data %>%
      count(GRADE, subjects, ks2em_band, ks2em) %>%
      mutate(characteristic_value = "All Pupils") %>%
      mutate(characteristic_type = "All Pupils")
  }

  # a <- join_data_cs %>% filter(GRADE == 'covid impacted')


  grade_counts_Sex_cs <- func_counts_char(join_data_cs, Sex, "Sex")
  grade_counts_sen_cs <- func_counts_char(join_data_cs, SEN, "SEN")
  grade_counts_eal_cs <- func_counts_char(join_data_cs, EAL, "EAL")
  grade_counts_disadvantage_cs <- func_counts_char(join_data_cs, Disadvantage, "Disadvantage")
  grade_counts_all_cs <- func_counts_all(join_data_cs)

  grade_counts_comb_cs <- rbind(
    grade_counts_Sex_cs, grade_counts_sen_cs, grade_counts_eal_cs,
    grade_counts_disadvantage_cs, grade_counts_all_cs
  ) %>%
    select(-subjects)


  # creates the separate grade columns
  grade_counts_spread_cs <- grade_counts_comb_cs %>% # creates the separate grade columns
    pivot_wider(names_from = GRADE, values_from = n) # %>%
  # rename("covid_impacted" = "covid impacted") #REMOVED

  # select(-Q)



  # calcs
  grade_percentages_spread_cs <- grade_counts_spread_cs %>%
    # select(-X) %>% # removes X from the % calculation, we are including X in the percentage calc
    janitor::adorn_percentages() %>%
    mutate_if(is.numeric, function(x) {
      round2(x * 100, 1)
    }) %>%
    rename(
      "perc_UU" = "U",
      "perc_11" = "11",
      "perc_21" = "21",
      "perc_22" = "22",
      "perc_32" = "32",
      "perc_33" = "33",
      "perc_43" = "43",
      "perc_44" = "44",
      "perc_54" = "54",
      "perc_55" = "55",
      "perc_65" = "65",
      "perc_66" = "66",
      "perc_76" = "76",
      "perc_77" = "77",
      "perc_87" = "87",
      "perc_88" = "88",
      "perc_98" = "98",
      "perc_99" = "99",
      "perc_XX" = "X"
      # "per_covid_impacted_double" = "covid_impacted"
    )

  ## CS output
  tidy_data_cs <- grade_counts_spread_cs %>%
    left_join(grade_percentages_spread_cs, by = c(
      "ks2em", "ks2em_band",
      "characteristic_type", # comment back for app use,
      "characteristic_value"
    )) %>%
    # ,"LA_name"
    mutate(
      time_period = "202324",
      time_identifier = "Academic year",
      geographic_level = "National",
      country_code = "E92000001",
      country_name = "England",
      version = "Provisional",
      all_grades = rowSums(.[, c("U", "11", "21", "22", "32", "33", "43", "44", "54", "55", "65", "66", "76", "77", "87", "88", "98", "99", "X")], na.rm = TRUE)
    ) %>%
    arrange(
      characteristic_type, # comment back for app use,
      characteristic_value, ks2em_band
    ) %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, version,
      characteristic_type, # comment back for app use,
      characteristic_value,
      KS2_Prior = ks2em,

      ## the grades below produce data files output when running app this need to be commented out and the section 2 commented back in.
      ## 'U' = 'U',
      ##' 11' = '11', '22'= '22', '33' = '33', '44' = '44', '55' = '55', '66' = '66',
      ## '77' = '77', '88' = '88', '99' = '99', 'X' = 'X', All_Grades,  '%_U'='perc_U' ,'%_11'='perc_11','%_22'='perc_22','%_33' ='perc_33', '%_44'='perc_44',
      ##' %_55'='perc_55', '%_66'='perc_66', '%_77'='perc_77', '%_88'='perc_88', '%_99'='perc_99') %>% ##comment out for app

      ### (section2 for app use)

      "num_11" = "11", "num_21" = "21", "num_22" = "22", "num_32" = "32", "num_33" = "33", "num_43" = "43", "num_44" = "44", "num_54" = "54", "num_55" = "55", "num_65" = "65", "num_66" = "66", "num_76" = "76",
      "num_77" = "77", "num_87" = "87", "num_88" = "88", "num_98" = "98", "num_99" = "99", "num_UU" = "U", "num_XX" = "X", "all_grades",
      "perc_UU", "perc_11", "perc_21", "perc_22", "perc_32", "perc_33", "perc_43", "perc_44", "perc_54", "perc_55", "perc_65", "perc_66", "perc_76",
      "perc_77", "perc_87", "perc_88", "perc_98", "perc_99", "perc_XX"
    ) %>% ## comment back for app
    mutate_all(~ replace(., is.na(.), 0))


  # Remove zero values from the characteristic_value column
  tidy_data_cs <- tidy_data_cs %>%
    filter(characteristic_value != 0)

  # add num_covid_impacted column
  tidy_data_cs <- tidy_data_cs %>%
    mutate(num_covid_impacted = 0)


  # copying data to an Excel file
  # save_tidy_data_file_cs = 'C:/Users/SMANCHESTER.AD/OneDrive - Department for Education/Documents/R Projects/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Comb_Science_Scaled_Scores_Final.csv'
  save_tidy_data_file_cs <- "C:/Users/MPARMAR/OneDrive - Department for Education/Documents/2024_publication/R/TM/Transition-matrices-dashboard/data/2024_Tidy_Data_Output_Comb_Science_Scaled_Scores_Final_v2.csv" # update year
  write.table(tidy_data_cs, save_tidy_data_file_cs, row.names = FALSE, sep = ",")




  #################################################################################################
  ##################                 Attainment tidy data                         #################
  #################################################################################################

  # STEP 6 Tidy Data for Attainment

  join_data_attainment <- Pupil_SQL_data %>% # check ok
    left_join(Exam_short,
      by = c("CANDNO", "LAESTAB")
    ) %>%
    drop_na(ks2em)


  ## function for calculating attainment columns for specified characteristic,
  ## the slice makes sure only one entry per pupil is selected
  func_attainment_char <- function(data, attainment, achieved, char, char_name) {
    data %>%
      filter({{ attainment }} == achieved) %>%
      group_by(CANDNO) %>%
      slice(1) %>%
      ungroup() %>%
      count({{ attainment }}, ks2em_band, ks2em, {{ char }}) %>%
      rename(characteristic_value = {{ char }}) %>%
      mutate(characteristic_type = char_name)
  }


  ## function for calculating all pupil attainment columns
  func_attainment_allgen <- function(data, attainment) {
    data %>%
      group_by(CANDNO) %>%
      slice(1) %>%
      ungroup() %>%
      count({{ attainment }}, ks2em_band, ks2em) %>%
      mutate(characteristic_value = "All Pupils") %>%
      mutate(characteristic_type = "All Pupils")
  }


  # Calculating pupils entered for EBacc
  EBACC_entered_Sex <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, Sex, "Sex")
  EBACC_notentered_Sex <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, Sex, "Sex")
  EBACC_entered_sen <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, SEN, "SEN")
  EBACC_notentered_sen <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, SEN, "SEN")
  EBACC_entered_eal <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, EAL, "EAL")
  EBACC_notentered_eal <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, EAL, "EAL")
  EBACC_entered_disadvantage <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, Disadvantage, "Disadvantage")
  EBACC_notentered_disadvantage <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, Disadvantage, "Disadvantage")
  EBACC_all_pupils <- func_attainment_allgen(join_data_attainment, EBACC_E_PTQ_EE)


  EBACC_all <- rbind(
    EBACC_entered_Sex, EBACC_notentered_Sex, EBACC_entered_sen, EBACC_notentered_sen,
    EBACC_entered_eal, EBACC_notentered_eal, EBACC_entered_disadvantage,
    EBACC_notentered_disadvantage, EBACC_all_pupils
  ) %>%
    pivot_wider(names_from = EBACC_E_PTQ_EE, values_from = n) %>%
    rename("Entered_for_EBacc" = "1") %>%
    rename("Not_Entered" = "0")

  # Percentages
  EBACC_all_perc <- left_join(EBACC_all,
    EBACC_all %>% janitor::adorn_percentages(),
    by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")
  ) %>%
    rename("EBacc_all_Entered" = "Entered_for_EBacc.x") %>%
    rename("EBacc_all_Not_Entered" = "Not_Entered.x") %>%
    rename("EBacc_all_perc_Entered" = "Entered_for_EBacc.y") %>%
    rename("EBacc_all_perc_Not_Entered" = "Not_Entered.y")




  # Calculating 9-4 grade achievement in EBacc
  EBACC94_achieved_Sex <- func_attainment_char(join_data_attainment, EBACC_94, 1, Sex, "Sex")
  EBACC94_notachieved_Sex <- func_attainment_char(join_data_attainment, EBACC_94, 0, Sex, "Sex")
  EBACC94_achieved_sen <- func_attainment_char(join_data_attainment, EBACC_94, 1, SEN, "SEN")
  EBACC94_notachieved_sen <- func_attainment_char(join_data_attainment, EBACC_94, 0, SEN, "SEN")
  EBACC94_achieved_eal <- func_attainment_char(join_data_attainment, EBACC_94, 1, EAL, "EAL")
  EBACC94_notachieved_eal <- func_attainment_char(join_data_attainment, EBACC_94, 0, EAL, "EAL")
  EBACC94_achieved_disadvantage <- func_attainment_char(join_data_attainment, EBACC_94, 1, Disadvantage, "Disadvantage")
  EBACC94_notachieved_disadvantage <- func_attainment_char(join_data_attainment, EBACC_94, 0, Disadvantage, "Disadvantage")
  EBACC94_all_pupils <- func_attainment_allgen(join_data_attainment, EBACC_94)

  EBACC94_all <- rbind(
    EBACC94_achieved_Sex, EBACC94_notachieved_Sex, EBACC94_achieved_sen, EBACC94_notachieved_sen,
    EBACC94_achieved_eal, EBACC94_notachieved_eal, EBACC94_achieved_disadvantage,
    EBACC94_notachieved_disadvantage, EBACC94_all_pupils
  ) %>%
    pivot_wider(names_from = EBACC_94, values_from = n) %>%
    rename("Achieved_EBacc_9-4" = "1") %>%
    rename("EBacc_9-4_Not_Achieved" = "0")

  EBACC94_all_perc <- left_join(EBACC94_all,
    EBACC94_all %>% janitor::adorn_percentages(),
    by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")
  ) %>%
    rename("EBacc_9-4_Achieved" = "Achieved_EBacc_9-4.x") %>%
    rename("EBacc_9-4_Not_Achieved" = "EBacc_9-4_Not_Achieved.x") %>%
    rename("EBacc_9-4_perc_Achieved" = "Achieved_EBacc_9-4.y") %>%
    rename("EBacc_9-4_perc_Not_Achieved" = "EBacc_9-4_Not_Achieved.y")




  # Calculating 9-5 grade achievement in EBacc
  EBACC95_achieved_Sex <- func_attainment_char(join_data_attainment, EBACC_95, 1, Sex, "Sex")
  EBACC95_notachieved_Sex <- func_attainment_char(join_data_attainment, EBACC_95, 0, Sex, "Sex")
  EBACC95_achieved_sen <- func_attainment_char(join_data_attainment, EBACC_95, 1, SEN, "SEN")
  EBACC95_notachieved_sen <- func_attainment_char(join_data_attainment, EBACC_95, 0, SEN, "SEN")
  EBACC95_achieved_eal <- func_attainment_char(join_data_attainment, EBACC_95, 1, EAL, "EAL")
  EBACC95_notachieved_eal <- func_attainment_char(join_data_attainment, EBACC_95, 0, EAL, "EAL")
  EBACC95_achieved_disadvantage <- func_attainment_char(join_data_attainment, EBACC_95, 1, Disadvantage, "Disadvantage")
  EBACC95_notachieved_disadvantage <- func_attainment_char(join_data_attainment, EBACC_95, 0, Disadvantage, "Disadvantage")
  EBACC95_all_pupils <- func_attainment_allgen(join_data_attainment, EBACC_95)

  EBACC95_all <- rbind(
    EBACC95_achieved_Sex, EBACC95_notachieved_Sex, EBACC95_achieved_sen, EBACC95_notachieved_sen,
    EBACC95_achieved_eal, EBACC95_notachieved_eal, EBACC95_achieved_disadvantage,
    EBACC95_notachieved_disadvantage, EBACC95_all_pupils
  ) %>%
    pivot_wider(names_from = EBACC_95, values_from = n) %>%
    rename("Achieved_EBacc_9-5" = "1") %>%
    rename("EBacc_9-5_Not_Achieved" = "0")

  EBACC95_all_perc <- left_join(EBACC95_all,
    EBACC95_all %>% janitor::adorn_percentages(),
    by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")
  ) %>%
    rename("EBacc_9-5_Achieved" = "Achieved_EBacc_9-5.x") %>%
    rename("EBacc_9-5_Not_Achieved" = "EBacc_9-5_Not_Achieved.x") %>%
    rename("EBacc_9-5_perc_Achieved" = "Achieved_EBacc_9-5.y") %>%
    rename("EBacc_9-5_perc_Not_Achieved" = "EBacc_9-5_Not_Achieved.y")


  # Calculating 9-4 grade achievement in English and Maths
  L2B94_achieved_Sex <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, Sex, "Sex")
  L2B94_notachieved_Sex <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, Sex, "Sex")
  L2B94_achieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, SEN, "SEN")
  L2B94_notachieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, SEN, "SEN")
  L2B94_achieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, EAL, "EAL")
  L2B94_notachieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, EAL, "EAL")
  L2B94_achieved_disadvantage <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, Disadvantage, "Disadvantage")
  L2B94_notachieved_disadvantage <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, Disadvantage, "Disadvantage")
  L2B94_all_pupils <- func_attainment_allgen(join_data_attainment, L2BASICS_94)

  L2B94_all <- rbind(
    L2B94_achieved_Sex, L2B94_notachieved_Sex, L2B94_achieved_sen, L2B94_notachieved_sen,
    L2B94_achieved_eal, L2B94_notachieved_eal, L2B94_achieved_disadvantage,
    L2B94_notachieved_disadvantage, L2B94_all_pupils
  ) %>%
    pivot_wider(names_from = L2BASICS_94, values_from = n) %>%
    rename("Achieved_Basics_9-4" = "1") %>%
    rename("Basics_9-4_Not_Achieved" = "0")

  L2B94_all_perc <- left_join(L2B94_all,
    L2B94_all %>% janitor::adorn_percentages(),
    by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")
  ) %>%
    rename("Basics_9-4_Achieved" = "Achieved_Basics_9-4.x") %>%
    rename("Basics_9-4_Not_Achieved" = "Basics_9-4_Not_Achieved.x") %>%
    rename("Basics_9-4_perc_Achieved" = "Achieved_Basics_9-4.y") %>%
    rename("Basics_9-4_perc_Not_Achieved" = "Basics_9-4_Not_Achieved.y")



  # Calculating 9-5 grade achievement in English and Maths
  L2B95_achieved_Sex <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, Sex, "Sex")
  L2B95_notachieved_Sex <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, Sex, "Sex")
  L2B95_achieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, SEN, "SEN")
  L2B95_notachieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, SEN, "SEN")
  L2B95_achieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, EAL, "EAL")
  L2B95_notachieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, EAL, "EAL")
  L2B95_achieved_disadvantage <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, Disadvantage, "Disadvantage")
  L2B95_notachieved_disadvantage <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, Disadvantage, "Disadvantage")
  L2B95_all_pupils <- func_attainment_allgen(join_data_attainment, L2BASICS_95)

  L2B95_all <- rbind(
    L2B95_achieved_Sex, L2B95_notachieved_Sex, L2B95_achieved_sen, L2B95_notachieved_sen,
    L2B95_achieved_eal, L2B95_notachieved_eal, L2B95_achieved_disadvantage,
    L2B95_notachieved_disadvantage, L2B95_all_pupils
  ) %>%
    pivot_wider(names_from = L2BASICS_95, values_from = n) %>%
    rename("Achieved_Basics_9-5" = "1") %>%
    rename("Basics_9-5_Not_Achieved" = "0")

  L2B95_all_perc <- left_join(L2B95_all,
    L2B95_all %>% janitor::adorn_percentages(),
    by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")
  ) %>%
    rename("Basics_9-5_Achieved" = "Achieved_Basics_9-5.x") %>%
    rename("Basics_9-5_Not_Achieved" = "Basics_9-5_Not_Achieved.x") %>%
    rename("Basics_9-5_perc_Achieved" = "Achieved_Basics_9-5.y") %>%
    rename("Basics_9-5_perc_Not_Achieved" = "Basics_9-5_Not_Achieved.y")



  # create a single table to join all 5 headline measures together
  attainment_TM <- EBACC_all_perc %>%
    left_join(EBACC94_all_perc, by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")) %>%
    left_join(EBACC95_all_perc, by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")) %>%
    left_join(L2B94_all_perc, by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")) %>%
    left_join(L2B95_all_perc, by = c("ks2em_band", "ks2em", "characteristic_type", "characteristic_value")) %>%
    mutate_at(vars(contains("perc")), ~ (round2(. * 100, 1)))



  # create the final tidy data file from the attainment_TM table
  attainment_tidy_data <- attainment_TM %>%
    mutate(
      time_period = "202324",
      time_identifier = "Academic year",
      geographic_level = "National",
      country_code = "E92000001",
      country_name = "England",
      version = "Provisional"
    ) %>%
    arrange(
      characteristic_type, # comment back for app use,
      characteristic_value, ks2em_band
    ) %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, version,
      characteristic_type, # comment back for app use,

      ## comment back in for data files
      ## characteristic_value, KS2_Prior=ks2em, 'EBacc_all_Entered',
      ## 'EBacc_all_Not_Entered','EBacc_all_%c_Entered', 'EBacc_all_%c_Not_Entered', 'EBacc_9-4_Achieved','EBacc_9-4_Not_Achieved',
      ## 'EBacc_9-4_%c_Achieved','EBacc_9-4_%c_Not_Achieved','EBacc_9-5_Achieved','EBacc_9-5_Not_Achieved',
      ## 'EBacc_9-5_%c_Achieved','EBacc_9-5_%c_Not_Achieved','Basics_9-4_Achieved','Basics_9-4_Not_Achieved',
      ## 'Basics_9-4_%c_Achieved','Basics_9-4_%c_Not_Achieved', 'Basics_9-5_Achieved','Basics_9-5_Not_Achieved',
      ## 'Basics_9-5_%c_Achieved', 'Basics_9-5_%c_Not_Achieved') %>%

      ## section (app use only)
      characteristic_value,
      KS2_Prior = ks2em, "EBacc_all_Entered",
      "EBacc_all_Not_Entered", "EBacc_all_perc_Entered", "EBacc_all_perc_Not_Entered", "EBacc_9-4_Achieved", "EBacc_9-4_Not_Achieved",
      "EBacc_9-4_perc_Achieved", "EBacc_9-4_perc_Not_Achieved", "EBacc_9-5_Achieved", "EBacc_9-5_Not_Achieved",
      "EBacc_9-5_perc_Achieved", "EBacc_9-5_perc_Not_Achieved", "Basics_9-4_Achieved", "Basics_9-4_Not_Achieved",
      "Basics_9-4_perc_Achieved", "Basics_9-4_perc_Not_Achieved", "Basics_9-5_Achieved", "Basics_9-5_Not_Achieved",
      "Basics_9-5_perc_Achieved", "Basics_9-5_perc_Not_Achieved"
    ) %>%
    mutate_all(~ replace(., is.na(.), 0))


  # Remove zero values from the characteristic_value column
  attainment_tidy_data <- attainment_tidy_data %>%
    filter(characteristic_value != 0)

  # copying data to an Excel file
  save_tidy_data_file_attainment <- "data/2024_Tidy_Data_Output_Attainment_Scaled_Scores_Final_v2.csv" # # update year
  write.table(attainment_tidy_data, save_tidy_data_file_attainment, row.names = FALSE, sep = ",")
}
