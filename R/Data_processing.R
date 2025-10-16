#---- Loading in KS4 data ----


# These are the packages needed for this code, you may already have these installed but run this section if not
shhh(library(dplyr))
shhh(library(odbc))
shhh(library(DBI))
shhh(library(janitor))
shhh(library(tidyverse))
shhh(library(purrr))

# suppressPackageStartupMessages(lapply(
#   c("dplyr", "odbc", "DBI", "janitor", "tidyverse", "purrr"),
#   library,
#   character.only = TRUE
# ))



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
CANDNO
,WOLF_DISC_CODE
,LAESTAB
,SUBLEVNO
,GRADE
,GNUMBER
,COVID_IMPACTED_FLAG
FROM [ks4_restricted].[dbo].[ks4_exam_24_result_amended_v2]
WHERE PTQ_INCLUDE = 1
AND disc3B_ptq_ee = 0
AND sublevno in ('310', '391', '395', '450', '451', '760', '954')"
# AND ENDKS = 1
# AND NATRES = 1"
  Exam_SQL_data <- dbGetQuery(SQL_con, SQL_Statement)


  Exam_SQL_data <- Exam_SQL_data %>%
    mutate(subjects = case_when(
      WOLF_DISC_CODE == 'FK2B' & !SUBLEVNO %in% c(121, 150, 225) ~ 'English Language',
      WOLF_DISC_CODE == 'FC4'  & !SUBLEVNO %in% c(121, 150, 225) ~ 'English Literature',
      WOLF_DISC_CODE %in% c('RB1','RB15','RB1A','RB1B','RB31','RB55','RB56','RB71','RB7B','RB7E') & 
        !GNUMBER %in% c('10034912','10060054','10060091','10060133','10060170','10060212','1006025X','10050395', '60310844', '60311770') & 
        !SUBLEVNO %in% c(121, 150, 225) ~ 'Mathematics',
      WOLF_DISC_CODE == 'RA1E' ~ 'Combined Science',
      WOLF_DISC_CODE == 'RC1' ~ 'Physics',
      WOLF_DISC_CODE == 'RD1' ~ 'Chemistry',
      WOLF_DISC_CODE == 'RH3' ~ 'Biology',
      WOLF_DISC_CODE == 'CK1' ~ 'Computer Science',
      WOLF_DISC_CODE %in% c('RE1','RF2','RC52','QA3') ~ 'Other Sciences',
      WOLF_DISC_CODE == 'VF1' ~ 'Design & Technology',
      WOLF_DISC_CODE == 'XA5A' ~ 'D & T: Textiles Technology',
      WOLF_DISC_CODE %in% c('VF2', 'VF3') ~ 'Other Design and Technology',
      WOLF_DISC_CODE == 'XA1' ~ 'Engineering',
      WOLF_DISC_CODE == 'AA3' ~ 'Business',
      WOLF_DISC_CODE == 'NH6' ~ 'Food Preparation & Nutrition',
      WOLF_DISC_CODE == 'RF4' ~ 'Geography',
      WOLF_DISC_CODE == 'DB' ~ 'History',
      WOLF_DISC_CODE == 'DB21' ~ 'Ancient History',
      WOLF_DISC_CODE == 'EB' ~ 'Economics',
      WOLF_DISC_CODE %in% c('EE2', 'PK1', 'EE31', 'DE1', 'EA') ~ 'Social Studies',
      WOLF_DISC_CODE == 'FKF' ~ 'French',
      WOLF_DISC_CODE == 'FKG' ~ 'German',
      WOLF_DISC_CODE == 'FKS' ~ 'Spanish',
      WOLF_DISC_CODE %in% c('FKO','FKQ','F1H','FKB','F1P','FKP','FKI','FKJ','FKM','FKK','FKU','FKX','FKC','FKR','FKT') ~ 'Other Modern Languages',
      WOLF_DISC_CODE == 'F1L' ~ 'Latin',
      WOLF_DISC_CODE == 'DB2B' ~ 'Classical Civilisation',
      WOLF_DISC_CODE == 'F1K' ~ 'Classical Greek',
      WOLF_DISC_CODE == 'F1Z' ~ 'Biblical Hebrew',
      WOLF_DISC_CODE %in% c('JA2','KJ1') & !GNUMBER %in% c('60308448','60329646','60330739','6030845X','60047811') ~ 'Art and Design',
      WOLF_DISC_CODE == 'KA2' & !GNUMBER %in% c('60311150','60311502','60319434','60320692','60321052','60322469') ~ 'Film Studies',
      WOLF_DISC_CODE == 'LB1' & !GNUMBER %in% c('60304066') ~ 'Dance',
      WOLF_DISC_CODE == 'LC11' & !GNUMBER %in% c('60176799','60176805','60304066','60305083','60329609') ~ 'Drama',
      WOLF_DISC_CODE == 'PA1' & !GNUMBER %in% c('60303955','6004780X','60332943') ~ 'Health and Social Care',
      WOLF_DISC_CODE == 'KA2' & !GNUMBER %in% c('10042799','50022465','50025995','50030188','60055029','60308898','60309702','60309714','60309726') ~ 'Media/Film/TV',
      WOLF_DISC_CODE %in% c('LF1','LJ9') & !GNUMBER %in% c('6006657X','60066520','60066532','60066568','60068188','60167749','60167774','60329737','60333066','60333042') ~ 'Music',
      WOLF_DISC_CODE == 'MA1' & !GNUMBER %in% c('6014662X','60047793','60051218','60051231','60145341','60176787','60326505','60327054','6030473X') ~ 'Physical Education',
      WOLF_DISC_CODE == 'DD1' ~ 'Religious Studies',
      WOLF_DISC_CODE == 'RB71' & !GNUMBER %in% c('60322615') ~ 'Statistics',
      WOLF_DISC_CODE == 'AK6' ~ 'Accounting',
      TRUE ~ NA_character_  # Default case
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

FROM [ks4_restricted].[dbo].[KS4_tidy_data_pupil_24_result_amended_v2]
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
      disadvantage == "Disadvantaged all other" ~ "Disadvantaged all other",
      disadvantage == "Disadvantaged" ~ "Disadvantaged",
      TRUE ~ NA_character_
    )) %>%
    mutate(Sex = case_when(
      SEX == "Girls" ~ "Girls",
      SEX == "Boys" ~ "Boys",
      TRUE ~ NA_character_
    )) %>%
    mutate("SEN status" = case_when(
      sen_description == "Any SEN" ~ "Any SEN",
      sen_description == "No identified SEN" ~ "No identified SEN",
      TRUE ~ NA_character_
    )) %>%
    mutate("First language" = case_when(
      EAL == "Other than English" ~ "Other than English", #
      EAL == "English" ~ "English",
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
      is.na(KS2EMSS) ~ "No scaled score",  # Handling missing values
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
      is.na(KS2EMSS)  ~ "12",
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
    "Computer Science", "Dance", "Design & Technology", "Drama", "Economics", "Engineering", "English Language", "English Literature", "Film Studies",
    "Food Preparation & Nutrition", "French", "Geography", "German", "History", "Latin", "Mathematics", "Media/Film/TV", "Music",
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
    grouped_data <- data %>%
      count(GRADE, subjects, ks2em_band, ks2em, {{ char }}) %>%
      rename(breakdown = {{ char }}) %>%
      mutate(breakdown_topic = char_name)
    
    # Create total row by summarizing counts, setting ks2em = "Total"
    total_row <- grouped_data %>%
      group_by(GRADE, subjects, breakdown) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(ks2em = "Total", ks2em_band = "13", breakdown_topic = char_name)
    
    # Combine original and total row
    bind_rows(grouped_data, total_row)
  }
  

  func_counts_all <- function(data) {
    grouped_data <- data %>%
      count(GRADE, subjects, ks2em_band, ks2em) %>%
      mutate(breakdown = "Total", breakdown_topic = "Total")
    
    # Create total row by summarizing counts, setting ks2em = "Total"
    total_row <- grouped_data %>%
      group_by(GRADE, subjects) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(ks2em = "Total", ks2em_band = NA, breakdown = "Total", breakdown_topic = "Total")
    
    # Combine original and total row
    bind_rows(grouped_data, total_row)
  }
  

# This section counts the number of grades in each breakdown and All Pupils
grade_counts_Sex <- func_counts_char(join_data, Sex, "Sex")
grade_counts_sen <- func_counts_char(join_data, `SEN status`, "SEN status")
grade_counts_eal <- func_counts_char(join_data, `First language`, "First language")
grade_counts_disadvantage <- func_counts_char(join_data, Disadvantage, "Disadvantage")
grade_counts_all <- func_counts_all(join_data)

# This combines the count dataframes
grade_counts_comb <- rbind(
  grade_counts_Sex, grade_counts_sen, grade_counts_eal,
  grade_counts_disadvantage, grade_counts_all
)

  # group_bys <- c( "GRADE","subjects", "ks2em","ks2em_band", "Sex","SEN status", "First language", "Disadvantage")
  # set_lists = list(c("GRADE","subjects", "ks2em","ks2em_band",  "Sex")
  #                  ,c("GRADE","subjects", "ks2em","ks2em_band",  "SEN status")
  #                  ,c("GRADE","subjects", "ks2em","ks2em_band",  "First language")
  #                  ,c("GRADE","subjects", "ks2em","ks2em_band",  "Disadvantage")
  #                  ,c("GRADE","subjects", "Sex")
  #                  ,c("GRADE","subjects", "SEN status")
  #                  ,c("GRADE","subjects", "First language")
  #                  ,c("GRADE","subjects", "Disadvantage")
  #                  ,c("GRADE","subjects", "ks2em","ks2em_band" )
  #                  ,c("GRADE","subjects")
  # )
  # table_char <- data %>%
  #   as.data.table() %>%
  #   groupingsets(j = .(n = .N)
  #                ,by = group_bys
  #                ,sets = set_lists) %>%
  #       mutate(ks2em = case_when(is.na(ks2em)~"Total",TRUE~ks2em),
  #              ks2em_band = case_when(is.na(ks2em_band)~"13",TRUE~ks2em_band))%>%
  #   mutate(breakdown = coalesce(Sex, `SEN status`,`First language`, Disadvantage),
  #          breakdown_topic = case_when(!is.na(Sex)~"Sex",
  #                                      !is.na(`SEN status`)~"SEN status",
  #                                      !is.na(`First language`)~"First language",
  #                                      !is.na(Disadvantage)~"Disadvantage",TRUE~"Error")
  #          )%>%
  #   select(-c("Sex", "SEN status", "First language", "Disadvantage"))%>%
  #   mutate(n=as.numeric(n))%>%
  #   arrange(GRADE)%>%
  #   as.data.frame()
  # 




  # creates the separate grade columns
  # grade_counts_spread <- grade_counts_comb %>%
  #   pivot_wider(names_from = GRADE, values_from = n) %>%
  #   select(-Q) %>%
  #   mutate(across(where(is.list), ~ unlist(.x)))%>%
  #   rename("covid_impacted" = "covid impacted")
  
  grade_counts_spread <- grade_counts_comb %>%
    pivot_wider(names_from = GRADE, values_from = n) %>%
    mutate(X = coalesce(X, 0) + coalesce(Q, 0)) %>%  # Combine Q into X
    select(-Q) %>%  # Remove Q after merging
    mutate(across(where(is.list), ~ unlist(.x))) %>%
    rename("covid_impacted" = "covid impacted")
  
    
  # Calculates the percentages
  grade_percentages_spread <- grade_counts_spread %>%
    # select(-X) %>% # removes X from the % calculation
    janitor::adorn_percentages() %>%
    mutate_if(is.numeric, function(x) {
      round2(x * 100, 1)
    }) %>%
    rename(
      "perc_1" = "1",
      "perc_2" = "2",
      "perc_3" = "3",
      "perc_4" = "4",
      "perc_5" = "5",
      "perc_6" = "6",
      "perc_7" = "7",
      "perc_8" = "8",
      "perc_9" = "9",
      "perc_U" = "U",
      "perc_X" = "X",
      "perc_covid_impacted" = "covid_impacted"
    )

  # Creates the final tidy data
  tidy_data_subjects <- grade_counts_spread %>%
    left_join(grade_percentages_spread, by = c(
      "subjects", "ks2em", "ks2em_band",
      "breakdown_topic", # comment back for app use,
      "breakdown"
    )) %>%
    mutate(
      time_period = 202324,
      time_identifier = "Academic year",
      geographic_level = "National",
      country_code = "E92000001",
      country_name = "England",
      version = "Revised",
      establishment_type_group = "All state-funded",
      qualification_type = "GCSE",
      all_grades = rowSums(.[, c("U", "1", "2", "3", "4", "5", "6", "7", "8", "9", "X", "covid_impacted")], na.rm = TRUE)
    ) %>%
    arrange(
      breakdown_topic, # comment back for app use,
      breakdown, subjects, 
      ks2em_band
    ) %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, # LA_name,
      version, establishment_type_group,
      breakdown_topic, # comment back for app use,
      breakdown, 
      ks2_scaled_score_group = ks2em,
      subject = subjects,
      qualification_type,
      ## the grades below produce data files output when running app this need to be commented out and the section 2 commented back in.
      # '1' = '1', '2'= '2', '3' = '3', '4' = '4', '5' = '5', '6' = '6', '7' = '7', '8' = '8', '9' = '9', 'U' = 'U', 'X' = 'X', 'all_grades',
      #' %'='perc_1','%_2'='perc_2','%_3' ='perc_3', '%_4'='perc_4',' %_5'='perc_5', '%_6'='perc_6', '%_7'='perc_7', '%_8'='perc_8',
      #' %_9'='perc_9','%_U'='perc_U' ,'%_X'='perc_X', 'covid_impacted' ) %>% ##comment out for app

      ### (section2 for app use)
      "num_1" = "1", "num_2" = "2", "num_3" = "3", "num_4" = "4", "num_5" = "5", "num_6" = "6",
      "num_7" = "7", "num_8" = "8", "num_9" = "9", "num_U" = "U", "num_X" = "X", "num_covid_impacted" = "covid_impacted", "all_grades", "perc_1", "perc_2", "perc_3", "perc_4",
      "perc_5", "perc_6", "perc_7", "perc_8", "perc_9", "perc_U", "perc_X", "perc_covid_impacted"
    ) %>% ## comment back for app
    mutate_all(~ replace(., is.na(.), 0))


  # Remove zero values from the breakdown column
  tidy_data_subjects <- tidy_data_subjects %>%
    filter(breakdown != 0)


  # copying data to an Excel file
  # save_tidy_data_file = 'Y:/Pre-16 development/Routine products/Transition Matrices/TM Dev/8.TM_in_R/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Scaled_Scores_Final.csv'
  # save_tidy_data_file = 'C:/Users/SMANCHESTER.AD/OneDrive - Department for Education/Documents/R Projects/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Scaled_Scores_Final.csv'
  save_tidy_data_file_subjects <- "C:/Users/MPARMAR/OneDrive - Department for Education/Documents/Repo/Transition-matrices-dashboard_SQL_integration/data/2024_Tidy_Data_Output_91_Scaled_Scores_Final_v2.csv" # update year
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
    grouped_data <- data %>%
      count(GRADE, subjects, ks2em_band, ks2em, {{ char }}) %>%
      rename(breakdown = {{ char }}) %>%
      mutate(breakdown_topic = char_name)
    
    # Create total row by summarizing counts, setting ks2em = "Total"
    total_row <- grouped_data %>%
      group_by(GRADE, subjects, breakdown) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(ks2em = "Total", ks2em_band = "13", breakdown_topic = char_name)
    
    # Combine original and total row
    bind_rows(grouped_data, total_row)
  }
  
  
  func_counts_all <- function(data) {
    grouped_data <- data %>%
      count(GRADE, subjects, ks2em_band, ks2em) %>%
      mutate(breakdown = "Total", breakdown_topic = "Total")
    
    # Create total row by summarizing counts, setting ks2em = "Total"
    total_row <- grouped_data %>%
      group_by(GRADE, subjects) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(ks2em = "Total", ks2em_band = NA, breakdown = "Total", breakdown_topic = "Total")
    
    # Combine original and total row
    bind_rows(grouped_data, total_row)
  }
  
  # a <- join_data_cs %>% filter(GRADE == 'covid impacted')
  # This section counts the number of grades in each breakdown and All Pupils
  grade_counts_Sex_cs <- func_counts_char(join_data_cs, Sex, "Sex")
  grade_counts_sen_cs <- func_counts_char(join_data_cs, `SEN status`, "SEN status")
  grade_counts_eal_cs <- func_counts_char(join_data_cs, `First language`, "First language")
  grade_counts_disadvantage_cs <- func_counts_char(join_data_cs, Disadvantage, "Disadvantage")
  grade_counts_all_cs <- func_counts_all(join_data_cs)

  grade_counts_comb_cs <- rbind(
    grade_counts_Sex_cs, grade_counts_sen_cs, grade_counts_eal_cs,
    grade_counts_disadvantage_cs, grade_counts_all_cs
  ) %>%
    select(-subjects)


  # creates the separate grade columns
  grade_counts_spread_cs <- grade_counts_comb_cs %>% 
    pivot_wider(names_from = GRADE, values_from = n) %>%
    mutate(
      X = coalesce(X, 0) + coalesce(Q, 0),  # Combine Q into X
      `covid impacted` = 0  # Add the "covid impacted" column with all values as 0
    ) %>%
    select(-Q) %>%  # Remove Q after merging
    mutate(across(where(is.list), ~ unlist(.x))) %>%
    rename("covid_impacted" = "covid impacted")  # Rename after ensuring the column exists
    
    
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
      "perc_XX" = "X",
      "perc_covid_impacted" = "covid_impacted"
    )

  ## CS output
  tidy_data_cs <- grade_counts_spread_cs %>%
    left_join(grade_percentages_spread_cs, by = c(
      "ks2em", "ks2em_band",
      "breakdown_topic", # comment back for app use,
      "breakdown"
    )) %>%
    mutate(
      time_period = "202324",
      time_identifier = "Academic year",
      geographic_level = "National",
      country_code = "E92000001",
      country_name = "England",
      version = "Revised",
      establishment_type_group = "All state-funded",
      subject = "Combined Science",
      qualification_type = "GCSE",
      all_grades = rowSums(.[, c("U", "11", "21", "22", "32", "33", "43", "44", "54", "55", "65", "66", "76", "77", "87", "88", "98", "99", "X", "covid_impacted")], na.rm = TRUE)
    ) %>%
    arrange(
      breakdown_topic, # comment back for app use,
      breakdown, 
      ks2em_band
    ) %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, version,
           establishment_type_group,
           breakdown_topic, # comment back for app use,
           breakdown, 
           ks2_scaled_score_group = ks2em,
           subject,
           qualification_type,
           

      ## the grades below produce data files output when running app this need to be commented out and the section 2 commented back in.
      ## 'U' = 'U',
      ##' 11' = '11', '22'= '22', '33' = '33', '44' = '44', '55' = '55', '66' = '66',
      ## '77' = '77', '88' = '88', '99' = '99', 'X' = 'X', All_Grades,  '%_U'='perc_U' ,'%_11'='perc_11','%_22'='perc_22','%_33' ='perc_33', '%_44'='perc_44',
      ##' %_55'='perc_55', '%_66'='perc_66', '%_77'='perc_77', '%_88'='perc_88', '%_99'='perc_99') %>% ##comment out for app

      ### (section2 for app use)

      "num_11" = "11", "num_21" = "21", "num_22" = "22", "num_32" = "32", "num_33" = "33", "num_43" = "43", "num_44" = "44", "num_54" = "54", "num_55" = "55", "num_65" = "65", "num_66" = "66", "num_76" = "76",
      "num_77" = "77", "num_87" = "87", "num_88" = "88", "num_98" = "98", "num_99" = "99", "num_UU" = "U", "num_XX" = "X", "num_covid_impacted" = "covid_impacted", "all_grades",
       "perc_11", "perc_21", "perc_22", "perc_32", "perc_33", "perc_43", "perc_44", "perc_54", "perc_55", "perc_65", "perc_66", "perc_76",
      "perc_77", "perc_87", "perc_88", "perc_98", "perc_99","perc_UU", "perc_XX","perc_covid_impacted"
    ) %>% ## comment back for app
    mutate_all(~ replace(., is.na(.), 0))


  # Remove zero values from the breakdown column
  tidy_data_cs <- tidy_data_cs %>%
    filter(breakdown != 0)


  # copying data to an Excel file
  # save_tidy_data_file_cs = 'C:/Users/SMANCHESTER.AD/OneDrive - Department for Education/Documents/R Projects/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Comb_Science_Scaled_Scores_Final.csv'
  save_tidy_data_file_cs <- "C:/Users/MPARMAR/OneDrive - Department for Education/Documents/Repo/Transition-matrices-dashboard_SQL_integration/data/2024_Tidy_Data_Output_Comb_Science_Scaled_Scores_Final_v2.csv" # update year
  write.table(tidy_data_cs, save_tidy_data_file_cs, row.names = FALSE, sep = ",")

  #################################################################################################
  ##################                 Combine the 2 files above for EES            #################
  #################################################################################################


  # Ensure 'time_period' is character in both
  tidy_data_cs <- tidy_data_cs %>%
    mutate(time_period = as.character(time_period))
  
  tidy_data_subjects <- tidy_data_subjects %>%
    mutate(time_period = as.character(time_period))
  
  # Combine by row, keeping all columns
  combined_data <- bind_rows(tidy_data_subjects, tidy_data_cs)
  
  # Replace all NA values with "z"
  combined_data <- combined_data %>%
    mutate(across(everything(), ~replace(., is.na(.), "z")))
  
  
  # Save combined_data as a CSV
  save_tidy_data_file_TM_combined <- "C:/Users/MPARMAR/OneDrive - Department for Education/Documents/Repo/Transition-matrices-dashboard_SQL_integration/data/2024_Key stage 2 to 4 transition matrices GCSE subjects.csv"
  write.csv(combined_data, save_tidy_data_file_TM_combined, row.names = FALSE)
  

  #################################################################################################
  ##################                 Attainment tidy data                         #################
  #################################################################################################

  # STEP 6 Tidy Data for Attainment

  join_data_attainment <- Pupil_SQL_data %>% # check ok
    left_join(Exam_short,
      by = c("CANDNO", "LAESTAB")
    ) %>%
    drop_na(ks2em)



  func_attainment_char <- function(data, attainment, achieved, char, char_name) {
    # Group data and count per relevant category
    grouped_data <- data %>%
      filter({{ attainment }} == achieved) %>%
      group_by(CANDNO) %>%
      slice(1) %>%
      ungroup() %>%
      count({{ attainment }}, ks2em_band, ks2em, {{ char }}) %>%
      rename(breakdown = {{ char }}) %>%
      mutate(breakdown_topic = char_name)
    
    # Create total row for each breakdown category (Disadvantaged, Non-Disadvantaged, etc.)
    total_row <- grouped_data %>%
      group_by({{ attainment }}, breakdown_topic, breakdown) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(ks2em = "Total", ks2em_band = "13")
    
    # Combine original and total row
    final_data <- bind_rows(grouped_data, total_row)
    
    return(final_data)
  }
  


  ## function for calculating all pupil attainment columns
  func_attainment_allgen <- function(data, attainment) {
    # Group and summarize data as before
    grouped_data <- data %>%
      group_by(CANDNO) %>%
      slice(1) %>%
      ungroup() %>%
      count({{ attainment }}, ks2em_band, ks2em) %>%
      mutate(breakdown = "Total") %>%
      mutate(breakdown_topic = "Total")
    
    # Create total row by summarizing counts, setting ks2em = "Total"
    total_row <- grouped_data %>%
      group_by({{ attainment }}, breakdown_topic, breakdown) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      mutate(ks2em = "Total", ks2em_band = "13")
  
    
    # Combine original and total row
    final_data <- bind_rows(grouped_data, total_row)
    
    return(final_data)
  }
  


  # Calculating pupils entered for EBacc
  EBACC_entered_Sex <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, Sex, "Sex")
  EBACC_notentered_Sex <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, Sex, "Sex")
  EBACC_entered_sen <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, `SEN status`, "SEN status")
  EBACC_notentered_sen <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, `SEN status`, "SEN status")
  EBACC_entered_eal <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, `First language`, "First language")
  EBACC_notentered_eal <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, `First language`, "First language")
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
    by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")
  ) %>%
    rename("Entered_for_EBacc" = "Entered_for_EBacc.x") %>%
    rename("Not_Entered" = "Not_Entered.x") %>%
    rename("%_Entered_for_EBacc" = "Entered_for_EBacc.y") %>%
    rename("%_Not_Entered" = "Not_Entered.y")




  # Calculating 9-4 grade achievement in EBacc
  EBACC94_achieved_Sex <- func_attainment_char(join_data_attainment, EBACC_94, 1, Sex, "Sex")
  EBACC94_notachieved_Sex <- func_attainment_char(join_data_attainment, EBACC_94, 0, Sex, "Sex")
  EBACC94_achieved_sen <- func_attainment_char(join_data_attainment, EBACC_94, 1, `SEN status`, "SEN status")
  EBACC94_notachieved_sen <- func_attainment_char(join_data_attainment, EBACC_94, 0, `SEN status`, "SEN status")
  EBACC94_achieved_eal <- func_attainment_char(join_data_attainment, EBACC_94, 1, `First language`, "First language")
  EBACC94_notachieved_eal <- func_attainment_char(join_data_attainment, EBACC_94, 0, `First language`, "First language")
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
    by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")
  ) %>%
    rename("EBacc_9-4_Achieved" = "Achieved_EBacc_9-4.x") %>%
    rename("EBacc_9-4_Not_Achieved" = "EBacc_9-4_Not_Achieved.x") %>%
    rename("%_EBacc_9-4_Achieved" = "Achieved_EBacc_9-4.y") %>%
    rename("%_EBacc_9-4_Not_Achieved" = "EBacc_9-4_Not_Achieved.y")




  # Calculating 9-5 grade achievement in EBacc
  EBACC95_achieved_Sex <- func_attainment_char(join_data_attainment, EBACC_95, 1, Sex, "Sex")
  EBACC95_notachieved_Sex <- func_attainment_char(join_data_attainment, EBACC_95, 0, Sex, "Sex")
  EBACC95_achieved_sen <- func_attainment_char(join_data_attainment, EBACC_95, 1, `SEN status`, "SEN status")
  EBACC95_notachieved_sen <- func_attainment_char(join_data_attainment, EBACC_95, 0, `SEN status`, "SEN status")
  EBACC95_achieved_eal <- func_attainment_char(join_data_attainment, EBACC_95, 1, `First language`, "First language")
  EBACC95_notachieved_eal <- func_attainment_char(join_data_attainment, EBACC_95, 0, `First language`, "First language")
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
    by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")
  ) %>%
    rename("EBacc_9-5_Achieved" = "Achieved_EBacc_9-5.x") %>%
    rename("EBacc_9-5_Not_Achieved" = "EBacc_9-5_Not_Achieved.x") %>%
    rename("%_EBacc_9-5_Achieved" = "Achieved_EBacc_9-5.y") %>%
    rename("%_EBacc_9-5_Not_Achieved" = "EBacc_9-5_Not_Achieved.y")


  # Calculating 9-4 grade achievement in English and Maths
  L2B94_achieved_Sex <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, Sex, "Sex")
  L2B94_notachieved_Sex <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, Sex, "Sex")
  L2B94_achieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, `SEN status`, "SEN status")
  L2B94_notachieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, `SEN status`, "SEN status")
  L2B94_achieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, `First language`, "First language")
  L2B94_notachieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, `First language`, "First language")
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
    by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")
  ) %>%
    rename("Basics_9-4_Achieved" = "Achieved_Basics_9-4.x") %>%
    rename("Basics_9-4_Not_Achieved" = "Basics_9-4_Not_Achieved.x") %>%
    rename("%_Basics_9-4_Achieved" = "Achieved_Basics_9-4.y") %>%
    rename("%_Basics_9-4_Not_Achieved" = "Basics_9-4_Not_Achieved.y")



  # Calculating 9-5 grade achievement in English and Maths
  L2B95_achieved_Sex <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, Sex, "Sex")
  L2B95_notachieved_Sex <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, Sex, "Sex")
  L2B95_achieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, `SEN status`, "SEN status")
  L2B95_notachieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, `SEN status`, "SEN status")
  L2B95_achieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, `First language`, "First language")
  L2B95_notachieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, `First language`, "First language")
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
    by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")
  ) %>%
    rename("Basics_9-5_Achieved" = "Achieved_Basics_9-5.x") %>%
    rename("Basics_9-5_Not_Achieved" = "Basics_9-5_Not_Achieved.x") %>%
    rename("%_Basics_9-5_Achieved" = "Achieved_Basics_9-5.y") %>%
    rename("%_Basics_9-5_Not_Achieved" = "Basics_9-5_Not_Achieved.y")



  # create a single table to join all 5 headline measures together
  attainment_TM <- EBACC_all_perc %>%
    left_join(EBACC94_all_perc, by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")) %>%
    left_join(EBACC95_all_perc, by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")) %>%
    left_join(L2B94_all_perc, by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")) %>%
    left_join(L2B95_all_perc, by = c("ks2em_band", "ks2em", "breakdown_topic", "breakdown")) %>%
    mutate_at(vars(contains("%")), ~ (round2(. * 100, 1)))



  # create the final tidy data file from the attainment_TM table
  attainment_tidy_data <- attainment_TM %>%
    mutate(
      time_period = "202324",
      time_identifier = "Academic year",
      geographic_level = "National",
      country_code = "E92000001",
      country_name = "England",
      version = "Revised",
      establishment_type_group = "All state-funded"
      
    ) %>%
    arrange(
      breakdown_topic, # comment back for app use,
      breakdown, 
      ks2em_band
    ) %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, version, establishment_type_group,
      breakdown_topic, # comment back for app use,
      breakdown,
      ks2_scaled_score_group = ks2em, 
      "Entered_for_EBacc", "Not_Entered", "%_Entered_for_EBacc", "%_Not_Entered", 
      "EBacc_9-4_Achieved", "EBacc_9-4_Not_Achieved","%_EBacc_9-4_Achieved", "%_EBacc_9-4_Not_Achieved", 
      "EBacc_9-5_Achieved", "EBacc_9-5_Not_Achieved","%_EBacc_9-5_Achieved", "%_EBacc_9-5_Not_Achieved", 
      "Basics_9-4_Achieved", "Basics_9-4_Not_Achieved", "%_Basics_9-4_Achieved", "%_Basics_9-4_Not_Achieved", 
      "Basics_9-5_Achieved", "Basics_9-5_Not_Achieved", "%_Basics_9-5_Achieved", "%_Basics_9-5_Not_Achieved"
    ) %>%
    mutate_all(~ replace(., is.na(.), 0))


  # Remove zero values from the breakdown column
  attainment_tidy_data <- attainment_tidy_data %>%
    filter(breakdown != 0)

  # copying data to an Excel file
  save_tidy_data_file_attainment <- "data/2024_Tidy_Data_Output_Attainment_Scaled_Scores_Final_v2.csv" # # update year
  write.table(attainment_tidy_data, save_tidy_data_file_attainment, row.names = FALSE, sep = ",")
  
  # copying data to an Excel file for EES
  save_tidy_data_file_attainment <- "data/2024_Key stage 2 to 4 transition matrices KS4 measures.csv" # # update year
  write.table(attainment_tidy_data, save_tidy_data_file_attainment, row.names = FALSE, sep = ",")
  
}
