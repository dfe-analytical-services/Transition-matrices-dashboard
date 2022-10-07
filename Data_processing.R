#---- Loading in KS4 data ----

# Clear environment
rm(list = ls())

#These are the packages needed for this code, you may already have these installed but run this section if not
install.packages("dplyr")
install.packages("odbc")
install.packages("DBI")
install.packages("janitor")
install.packages("tidyverse")
install.packages ("purrr")


# Load libraries
library(dplyr)
library(odbc)
library(DBI)
library(janitor)
library(tidyverse)


#round function
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}



# Connect to the SQL server
SQL_con <- dbConnect(odbc(), Driver = "SQL Server",
                     Server = "3DCPRI-PDB16\\ACSQLS",
                     Database = "KS4_RESTRICTED", # Enter the folder containing your SQL data
                     Trusted_Connection = "True")



#STEP 1

# Import the exam data
SQL_data <- tbl(SQL_con,("KS4_exam_22_cohort_amended_v2")) # Enter the name of your SQL database #updare file name

# Create subject groupings based on wolf codes from the exam file
Exam_SQL_data <- SQL_data %>%
  filter(ptq_include == 1,			
         disc3B_ptq_ee == 0, 			
         sublevno %in% c('310','363','391','395','450','451','760','954')) %>% 
  select (CANDNO, 
          WOLF_DISC_CODE, 
          LAESTAB, 
          SUBLEVNO, 
          GRADE) %>% 
  mutate(subjects = case_when( wolf_disc_code == "FK2B"~ "English Language",
                               wolf_disc_code == "FC4" ~ "English Literature",
                               WOLF_DISC_CODE == "RB1" | WOLF_DISC_CODE == "RB15" | WOLF_DISC_CODE == "RB1A" |   WOLF_DISC_CODE == "RB1B" | WOLF_DISC_CODE == "RB31" | WOLF_DISC_CODE == "RB55"| 
                                 WOLF_DISC_CODE == "RB156" | WOLF_DISC_CODE == "RB71" | WOLF_DISC_CODE == "RB7B"|  WOLF_DISC_CODE == "RB7E"~ "Mathematics",  
                               wolf_disc_code == "RA1E"   ~ "Combined Science",					
                               wolf_disc_code == "RC1"   ~ "Physics", 
                               wolf_disc_code == "RD1"   ~ "Chemistry", 
                               wolf_disc_code == "RH3"   ~ "Biology" ,
                               wolf_disc_code == "CK1"   ~ "Computer Science",
                               WOLF_DISC_CODE == "RE1" | WOLF_DISC_CODE == "RF2" | WOLF_DISC_CODE == "RC52" | 
                                 WOLF_DISC_CODE == "QA3" ~ "Other Sciences",                                     
                               wolf_disc_code == "VF1"  ~ "Design & Technology"	,			
                               wolf_disc_code == "XA5A" ~ "D & T: Textiles Technology"	,	
                               wolf_disc_code == "VF2" | WOLF_DISC_CODE == "VF3" ~ "Other Design and Technology"	,	
                               wolf_disc_code =="XA1"   ~ "Engineering"		,		
                               wolf_disc_code =="AA3"		~ "Business" ,
                               wolf_disc_code =="NH6"  ~ "Food Preparation & Nutrition",
                               wolf_disc_code =="RF4"  ~ "Geography" ,
                               wolf_disc_code =="DB"   ~ "History" ,
                               wolf_disc_code =="DB21" ~ "Ancient History",
                               wolf_disc_code == "EB"  ~ "Economics",
                               WOLF_DISC_CODE == "EE22" | WOLF_DISC_CODE == "PK1"| WOLF_DISC_CODE == "EE31"|
                                 WOLF_DISC_CODE == "DE1"| WOLF_DISC_CODE == "EA"   ~ "Social Studies" ,
                               wolf_disc_code == "FKF" ~ "French" ,
                               wolf_disc_code == "FKG" ~ "German",
                               wolf_disc_code == "FKS"  ~ "Spanish", 
                               WOLF_DISC_CODE == "FKO" | WOLF_DISC_CODE == "F1H" | WOLF_DISC_CODE == "FKB"| WOLF_DISC_CODE == "F1P" | WOLF_DISC_CODE == "FKP" | WOLF_DISC_CODE == "FKI"
                               | WOLF_DISC_CODE == "FKJ" | WOLF_DISC_CODE == "FKM" | WOLF_DISC_CODE == "FKK"| WOLF_DISC_CODE == "F1U" | WOLF_DISC_CODE == "FKX" | WOLF_DISC_CODE == "FKC"
                               | WOLF_DISC_CODE == "FKR" | WOLF_DISC_CODE == "FKT"~ "Other Modern Languages",
                               wolf_disc_code == "F1L"   ~ "Latin", 
                               wolf_disc_code == "DB2B"  ~ "Classical Civilisation" ,
                               wolf_disc_code == "F1K"   ~ "Classical Greek" ,
                               wolf_disc_code == "F1Z"   ~ "Biblical Hebrew" ,
                               wolf_disc_code == "JA2" | WOLF_DISC_CODE == "KJ1" ~ "Art and Design" ,
                               wolf_disc_code == "KA2"             						 ~ "Film Studies" ,
                               wolf_disc_code == "LB1"      ~ "Dance" ,
                               wolf_disc_code == "LC11"		 ~ "Drama",
                               wolf_disc_code == "PA1" 										~ "Health and Social Care"	,		
                               wolf_disc_code == "KA2"											 ~ "Media/Film/TV" ,
                               wolf_disc_code == "LF1"| WOLF_DISC_CODE == "LJ9" 				     ~ "Music" ,
                               wolf_disc_code == "MA1" 										 ~ "Physical Education" ,
                               wolf_disc_code == "DD1"										 ~ "Religious Studies"	,			
                               wolf_disc_code == "RB71"          					 ~ "Statistics" ,
                               wolf_disc_code == "AK6"	 		                 ~ "Accounting"	,
                               
                               
                               TRUE ~ NA_character_)) %>% 
  as.data.frame() #need to make it a dataframe so you can do the join lower down in step 4. You can then view it too in your environment on the right



# count the number of times each subject is in the data
Exam_SQL_data %>% count(subjects) %>% arrange(subjects)

# count the number of pupils doing each subject and so removes duplicates, 
# this does not match the SQL output
#Exam_SQL_data %>% select(CANDNO, subjects) %>% distinct() %>% count(subjects) %>% 
# arrange(subjects)

# count the number of times each subject is in the data
Exam_SQL_data %>% group_by(subjects) %>% summarise(n = n()) %>% arrange(subjects)




#STEP 2 Define variables

# Import the pupil data
Pupil_SQL_data <- tbl(SQL_con, ("KS4_pupil_22_cohort_amended_v2")) # Enter the name of your SQL database #update file name


Var_SQL_data <- Pupil_SQL_data %>% 
  filter(endks == 1,  			
         natres == 1,  			
         (nftype %in% c('32','33') | nftype %in% c('20','21','22','23','24','25','26','27','28','50','51','52','53','55','57','58') &  
            norflage !=3)) %>% # you can use norflage !=3 instead here :)
  select(CANDNO, URN, LAESTAB, NFTYPE, GENDER, LANG1ST, KS2EMSS, L2BASICS_94, 
         L2BASICS_95, EBACC_94, EBACC_95, EBACC_E_PTQ_EE, ATT8, P8SCORE, FSM6CLA1A, SENF) %>%
  as.data.frame()%>% #Handy to cast it as a dataframe so you can view what's happening by clicking on the table in your environment on the right
  mutate(Disadvantage = case_when(FSM6CLA1A == 0 ~ "Non Disadvantaged Pupils",
                                  FSM6CLA1A == 1 ~ "Disadvantaged Pupils",
                                  TRUE ~ NA_character_)) %>% 
  mutate(Gender = case_when(GENDER == 'F' ~ "Female Pupils",
                            GENDER == 'M' ~ "Male Pupils",
                            TRUE ~ NA_character_)) %>% 
  mutate(SEN = case_when(SENF %in% c('S','E','A','P','K') ~ "SEN Pupils",
                         TRUE ~ "Non SEN Pupils")) %>% 
  mutate(EAL = case_when(LANG1ST %in% c('OTB','OTH') ~ "EAL Pupils", #
                         TRUE ~ "Non EAL Pupils")) %>%
  mutate(ks2em = case_when(  KS2EMSS >=59    &  KS2EMSS <82        ~ "82 or less",
                             KS2EMSS >=82.5  & KS2EMSS<= 86        ~  "82.5 - 86",
                             KS2EMSS >=86.5  & KS2EMSS<= 87.5      ~  "86.5 - 87.5",
                             KS2EMSS >=88    & KS2EMSS<= 89        ~  "88 - 89",
                             KS2EMSS >=89.5  & KS2EMSS<= 90.5      ~  "89.5 - 90.5",
                             KS2EMSS >=91    & KS2EMSS<= 92        ~  "91 - 92",
                             KS2EMSS >=92.5  & KS2EMSS<= 93        ~  "92.5 - 93",
                             KS2EMSS >=93.5  & KS2EMSS<= 94        ~  "93.5 - 94",
                             KS2EMSS >=94.5  & KS2EMSS<= 95        ~  "94.5 - 95",
                             KS2EMSS >=95.5  & KS2EMSS<= 96        ~  "95.5 - 96",
                             KS2EMSS >=96.5  & KS2EMSS<= 97        ~  "96.5 - 97",
                             KS2EMSS >=97.5  & KS2EMSS<= 98         ~  "97.5 - 98",
                             KS2EMSS >=98.5  & KS2EMSS<= 99         ~  "98.5 - 99",
                             KS2EMSS >=99.5  & KS2EMSS<= 100        ~  "99.5 - 100",
                             KS2EMSS >=100.5 & KS2EMSS<= 101        ~  "100.5 - 101",
                             KS2EMSS >=101.5 & KS2EMSS<= 102        ~  "101.5 - 102",
                             KS2EMSS >=102.5 & KS2EMSS<= 103        ~  "102.5 - 103",
                             KS2EMSS >=103.5 & KS2EMSS<= 104        ~  "103.5 - 104",
                             KS2EMSS >=104.5 & KS2EMSS<= 105        ~  "104.5 - 105",
                             KS2EMSS >=105.5 & KS2EMSS<= 106        ~  "105.5 - 106",
                             KS2EMSS >=106.5 & KS2EMSS<= 107        ~  "106.5 - 107",
                             KS2EMSS >=107.5 & KS2EMSS<= 108        ~  "107.5 - 108" ,
                             KS2EMSS >=108.5 & KS2EMSS<= 109        ~ "108.5 - 109",
                             KS2EMSS >=109.5 & KS2EMSS<= 110        ~  "109.5 - 110",
                             KS2EMSS >=110.5 & KS2EMSS<= 111        ~  "110.5 - 111",
                             KS2EMSS >=111.5 & KS2EMSS<= 112        ~  "111.5 - 112",
                             KS2EMSS >=112.5 & KS2EMSS<= 113        ~  "112.5 - 113",
                             KS2EMSS >=113.5 & KS2EMSS<= 114        ~  "113.5 - 114",
                             KS2EMSS >=114.5 & KS2EMSS<= 115        ~  "114.5 - 115",
                             KS2EMSS >=115.5 & KS2EMSS<= 116        ~  "115.5 - 116",
                             KS2EMSS >=116.5 & KS2EMSS<= 117        ~  "116.5 - 117",
                             KS2EMSS >=117.5 & KS2EMSS<= 118        ~  "117.5 - 118",
                             KS2EMSS >=118.5 & KS2EMSS<= 119        ~  "118.5 - 119",
                             KS2EMSS >=119.5 & KS2EMSS<= 120        ~  "119.5 - 120",
                             TRUE ~ NA_character_)) %>% 
  mutate(ks2em_band = case_when( KS2EMSS >=59    &  KS2EMSS <82        ~ "01", #this section makes sure the data is ordered by KS2 groups in the final tidy data files
                                 KS2EMSS >=82.5  & KS2EMSS<= 86        ~  "02",
                                 KS2EMSS >=86.5  & KS2EMSS<= 87.5      ~  "03",
                                 KS2EMSS >=88    & KS2EMSS<= 89        ~  "04",
                                 KS2EMSS >=89.5  & KS2EMSS<= 90.5      ~  "05",
                                 KS2EMSS >=91    & KS2EMSS<= 92        ~  "06",
                                 KS2EMSS >=92.5  & KS2EMSS<= 93        ~  "07",
                                 KS2EMSS >=93.5  & KS2EMSS<= 94        ~  "08",
                                 KS2EMSS >=94.5  & KS2EMSS<= 95        ~  "09",
                                 KS2EMSS >=95.5  & KS2EMSS<= 96        ~  "10",
                                 KS2EMSS >=96.5  & KS2EMSS<= 97        ~  "11",
                                 KS2EMSS >=97.5  & KS2EMSS<= 98         ~  "12",
                                 KS2EMSS >=98.5  & KS2EMSS<= 99         ~  "13",
                                 KS2EMSS >=99.5  & KS2EMSS<= 100        ~  "14",
                                 KS2EMSS >=100.5 & KS2EMSS<= 101        ~  "15",
                                 KS2EMSS >=101.5 & KS2EMSS<= 102        ~  "16",
                                 KS2EMSS >=102.5 & KS2EMSS<= 103        ~  "17",
                                 KS2EMSS >=103.5 & KS2EMSS<= 104        ~  "18",
                                 KS2EMSS >=104.5 & KS2EMSS<= 105        ~  "19",
                                 KS2EMSS >=105.5 & KS2EMSS<= 106        ~  "20",
                                 KS2EMSS >=106.5 & KS2EMSS<= 107        ~  "21",
                                 KS2EMSS >=107.5 & KS2EMSS<= 108        ~  "22",
                                 KS2EMSS >=108.5 & KS2EMSS<= 109        ~  "23",
                                 KS2EMSS >=109.5 & KS2EMSS<= 110        ~  "24",
                                 KS2EMSS >=110.5 & KS2EMSS<= 111        ~  "25",
                                 KS2EMSS >=111.5 & KS2EMSS<= 112        ~  "26",
                                 KS2EMSS >=112.5 & KS2EMSS<= 113        ~  "27",
                                 KS2EMSS >=113.5 & KS2EMSS<= 114        ~  "28",
                                 KS2EMSS >=114.5 & KS2EMSS<= 115        ~  "29",
                                 KS2EMSS >=115.5 & KS2EMSS<= 116        ~  "30",
                                 KS2EMSS >=116.5 & KS2EMSS<= 117        ~  "31",
                                 KS2EMSS >=117.5 & KS2EMSS<= 118        ~  "32",
                                 KS2EMSS >=118.5 & KS2EMSS<= 119        ~  "33",
                                 KS2EMSS >=119.5 & KS2EMSS<= 120        ~  "34",
                                 TRUE ~ NA_character_))

#CHECKS
Var_SQL_data %>% count(LANG1ST) %>% arrange(LANG1ST)

Var_SQL_data %>% count(Disadvantage) %>% arrange(Disadvantage)

Var_SQL_data %>% count(EAL) %>% arrange(EAL)

Var_SQL_data %>% count(SEN) %>% arrange(SEN)

Var_SQL_data %>% count(Gender) %>% arrange(Gender)

Var_SQL_data %>% count(Gender, Disadvantage, EAL, SEN) %>% group_by(ks2em)

Var_SQL_data %>% group_by(ks2em) %>% count(Gender, Disadvantage, EAL, SEN)



# STEP 3 Join the exam and pupil data

# Create a shorter version of the exam dataframe containing only the variables you need for the join

Exam_short <- Exam_SQL_data %>% 
  select(CANDNO,
         LAESTAB,
         WOLF_DISC_CODE,
         SUBLEVNO, 
         GRADE,
         subjects)

# Create a dataframe containing the subjects to be included in the TMs, you need to check with the Data Production Team to see if any subjects have changed
ks4_subjects = c('Ancient History', 'Art & Design',  'Biblical Hebrew', 'Biology', 'Business', 'Chemistry', 'Classical Civilisation' ,  'Classical Greek','Combined Science',
                 'Computer Science', 'Dance', 'Design and Technology', 'Drama', 'Economics', 'Engineering', 'English Language', 'English Literature','Film studies',
                 'Food Preparation and Nutrition', 'French', 'Geography', 'German', 'History', 'Latin', 'Mathematics', 'Music',
                 'Other Modern Languages', 'Other Sciences', 'Physical Education', 'Physics', 'Religious Studies', 'Sociology Studies', 'Spanish', 'Statistics')



#################################################################################################
##################                    9-1 subjects tidy data                    #################
#################################################################################################

#STEP 4 Tidy Data for subjects graded 9-1

join_data <- Var_SQL_data %>% 
  left_join(Exam_short,
            by = c('CANDNO', 'LAESTAB')) %>% 
  filter(subjects %in% ks4_subjects) %>%
  drop_na(ks2em)

#####################################################################################
func_counts_char <- function(data, char, char_name){
  data %>%
    count (GRADE, subjects, ks2em_band, ks2em, {{char}}) %>%
    rename(characteristic_value = {{char}}) %>%
    mutate(characteristic_type = char_name)}

func_counts_all <- function(data){
  data %>%
    count (GRADE, subjects, ks2em_band, ks2em) %>%
    mutate(characteristic_value = 'All Pupils') %>%
    mutate(characteristic_type = 'All Pupils')}


#This section counts the number of grades in each breakdown and All Pupils
grade_counts_gender <- func_counts_char(join_data, Gender, "Gender")
grade_counts_sen <- func_counts_char(join_data, SEN, "SEN")
grade_counts_eal <- func_counts_char(join_data, EAL, "EAL")
grade_counts_disadvantage <- func_counts_char(join_data, Disadvantage, "Disadvantage")
grade_counts_all <- func_counts_all(join_data)


#This combines the count dataframes
grade_counts_comb <- rbind(grade_counts_gender, grade_counts_sen, grade_counts_eal,
                           grade_counts_disadvantage, grade_counts_all)


#creates the separate grade columns
grade_counts_spread <- grade_counts_comb %>% 
  pivot_wider(names_from = GRADE, values_from = n) %>% 
  select(-Q)

# Calculates the percentages
grade_percentages_spread <- grade_counts_spread %>% 
  select(-X) %>% #removes X from the % calculation
  janitor::adorn_percentages() %>%
  mutate_if(is.numeric, function(x){round2(x*100, 1)}) %>%
  rename('perc_U' = 'U',
         'perc_1' = '1',
         'perc_2' = '2',
         'perc_3' = '3',
         'perc_4' = '4',
         'perc_5' = '5',
         'perc_6' = '6',
         'perc_7' = '7',
         'perc_8' = '8',
         'perc_9' = '9')

# Creates the final tidy data 
tidy_data <- grade_counts_spread %>%
  left_join(grade_percentages_spread, by = c('subjects', 'ks2em', 'ks2em_band', 
                                             'characteristic_type',#comment back for app use,
                                             'characteristic_value')) %>%
  mutate(time_period = 202122,
         time_identifier = 'Academic year',
         geographic_level = 'National',
         country_code = 'E92000001',
         country_name = 'England',
         version = 'Provisional',
         All_Grades = rowSums(.[,c('U', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'X')],na.rm=TRUE)) %>%
  arrange(characteristic_type,#comment back for app use,
          characteristic_value, subjects, ks2em_band) %>%
  select(time_period, time_identifier, geographic_level, country_code, country_name, version,
         characteristic_type, #comment back for app use,
         characteristic_value, subjects, KS2_Prior=ks2em,
         ##the grades below produce data files output when running app this need to be commented out and the section 2 commented back in.
         ## 'U' = 'U','1' = '1', '2'= '2', '3' = '3', '4' = '4', '5' = '5', '6' = '6', 
         ## '7' = '7', '8' = '8', '9' = '9', 'X' = 'X', All_Grades,  '%_U'='perc_U' ,'%_1'='perc_1','%_2'='perc_2','%_3' ='perc_3', '%_4'='perc_4',
         ##'%_5'='perc_5', '%_6'='perc_6', '%_7'='perc_7', '%_8'='perc_8', '%_9'='perc_9') %>% ##comment out for app
         
         ###(section2 for app use)
         'num_U' = 'U', 'num_1' = '1', 'num_2'= '2', 'num_3' = '3', 'num_4' = '4', 'num_5' = '5', 'num_6' = '6', 
         'num_7' = '7', 'num_8' = '8', 'num_9' = '9', 'num_X' = 'X', 'All_Grades', 'perc_U','perc_1','perc_2', 'perc_3', 'perc_4',
         'perc_5', 'perc_6', 'perc_7', 'perc_8', 'perc_9') %>% ##comment back for app
  mutate_all(~replace(.,is.na(.),0)) 


#copying data to an Excel file
#save_tidy_data_file = 'Y:/Pre-16 development/Routine products/Transition Matrices/TM Dev/8.TM_in_R/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Scaled_Scores_Final.csv'
#save_tidy_data_file = 'C:/Users/SMANCHESTER.AD/OneDrive - Department for Education/Documents/R Projects/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Scaled_Scores_Final.csv'
save_tidy_data_file = 'C:/Users/tabdulla/OneDrive - Department for Education/ONE DRIVE/TM/KS4_TM_Scaled_Scores/2022_Tidy_Data_Output_91_Scaled_Scores_Final.csv' #update year
write.table(tidy_data, save_tidy_data_file, row.names = FALSE, sep = ',')




#################################################################################################
##################                 Combined Science tidy data                   #################
#################################################################################################

#STEP 5 Tidy Data for Combined Science

join_data_cs <- Var_SQL_data %>% 
  left_join(Exam_short,
            by = c('CANDNO', 'LAESTAB')) %>% 
  filter(subjects == 'Combined Science') %>%
  drop_na(ks2em)


grade_counts_gender_cs <- func_counts_char(join_data_cs, Gender, "Gender")
grade_counts_sen_cs <- func_counts_char(join_data_cs, SEN, "SEN")
grade_counts_eal_cs <- func_counts_char(join_data_cs, EAL, "EAL")
grade_counts_disadvantage_cs <- func_counts_char(join_data_cs, Disadvantage, "Disadvantage")
grade_counts_all_cs <- func_counts_all(join_data_cs)

grade_counts_comb_cs <- rbind(grade_counts_gender_cs, grade_counts_sen_cs, grade_counts_eal_cs,
                              grade_counts_disadvantage_cs, grade_counts_all_cs) %>% 
  select(-subjects)


grade_counts_spread_cs <- grade_counts_comb_cs %>% #creates the separate grade columns
  pivot_wider(names_from = GRADE, values_from = n)
#select(-Q)


grade_percentages_spread_cs <- grade_counts_spread_cs %>% 
  select(-X) %>% #removes X from the % calculation, we are including X in the percentage calc
  janitor::adorn_percentages() %>%
  mutate_if(is.numeric, function(x){round2(x*100, 1)}) %>%
  rename('perc_U' = 'U',
         'perc_11' = '11',
         'perc_21' = '21',
         'perc_22' = '22',
         'perc_32' = '32',
         'perc_33' = '33',
         'perc_43' = '43',
         'perc_44' = '44',
         'perc_54' = '54',
         'perc_55' = '55',
         'perc_65' = '65',
         'perc_66' = '66',
         'perc_76' = '76',
         'perc_77' = '77',
         'perc_87' = '87',
         'perc_88' = '88',
         'perc_98' = '98',
         'perc_99' = '99'
         #,'perc_X' = 'X'
  )

##CS output
tidy_data_cs <- grade_counts_spread_cs %>%
  left_join(grade_percentages_spread_cs, by = c('ks2em', 'ks2em_band', 
                                                'characteristic_type',#comment back for app use
                                                'characteristic_value')) %>%
  mutate(time_period = '202122',
         time_identifier = 'Academic year',
         geographic_level = 'National',
         country_code = 'E92000001',
         country_name = 'England',
         version = 'Provisional',
         All_Grades = rowSums(.[,c('U', '11', '21', '22', '32', '33', '43', '44', '54', '55', '65',
                                   '66', '76', '77', '87', '88', '98', '99','X')],na.rm=TRUE)) %>%
  arrange(characteristic_type, #comment back for app use,
          characteristic_value, ks2em_band) %>% 
  select(time_period, time_identifier, geographic_level, country_code, country_name, version,
         characteristic_type,#comment back for app use, 
         characteristic_value, KS2_Prior=ks2em,
         
         ##the grades below produce data files output when running app this need to be commented out and the section 2 commented back in.
         ## 'U' = 'U',
         ##'11' = '11', '22'= '22', '33' = '33', '44' = '44', '55' = '55', '66' = '66', 
         ## '77' = '77', '88' = '88', '99' = '99', 'X' = 'X', All_Grades,  '%_U'='perc_U' ,'%_11'='perc_11','%_22'='perc_22','%_33' ='perc_33', '%_44'='perc_44',
         ##'%_55'='perc_55', '%_66'='perc_66', '%_77'='perc_77', '%_88'='perc_88', '%_99'='perc_99') %>% ##comment out for app
         
         ###(section2 for app use)
         'num_U' = 'U',
         'num_11' = '11', 'num_22'= '22', 'num_33' = '33', 'num_44' = '44', 'num_55' = '55', 'num_66' = '66', 
         'num_77' = '77', 'num_88' = '88', 'num_99' = '99', 'num_X' = 'X', All_Grades, 'perc_U','perc_11','perc_22', 'perc_33', 'perc_44',
         'perc_55', 'perc_66', 'perc_77', 'perc_88', 'perc_99') %>% ##comment back for app
  mutate_all(~replace(.,is.na(.),0)) 




#copying data to an Excel file
#save_tidy_data_file_cs = 'C:/Users/SMANCHESTER.AD/OneDrive - Department for Education/Documents/R Projects/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Comb_Science_Scaled_Scores_Final.csv'
save_tidy_data_file_cs = 'C:/Users/tabdulla/OneDrive - Department for Education/ONE DRIVE/TM/KS4_TM_Scaled_Scores/2022_Tidy_Data_Output_Comb_Science_Scaled_Scores_Final.csv' #update year
write.table(tidy_data_cs, save_tidy_data_file_cs, row.names = FALSE, sep = ',')




#################################################################################################
##################                 Attainment tidy data                         #################
#################################################################################################

#STEP 6 Tidy Data for Attainment

join_data_attainment <- Var_SQL_data %>% 
  left_join(Exam_short,
            by = c('CANDNO', 'LAESTAB')) %>% 
  drop_na(ks2em)


## function for calculating attainment columns for specified characteristic, 
## the slice makes sure only one entry per pupil is selected
func_attainment_char <- function(data, attainment, achieved, char, char_name){
  data %>% 
    filter({{attainment}} == achieved) %>% 
    group_by(CANDNO) %>% 
    slice(1) %>%
    ungroup() %>% 
    count ({{attainment}}, ks2em_band, ks2em, {{char}}) %>%
    rename(characteristic_value = {{char}}) %>%
    mutate(characteristic_type = char_name)}


## function for calculating all pupil attainment columns
func_attainment_allgen <- function(data, attainment){
  data %>%
    group_by(CANDNO) %>%
    slice(1) %>%
    ungroup() %>% 
    count({{attainment}}, ks2em_band, ks2em) %>%
    mutate(characteristic_value = 'All Pupils') %>%
    mutate(characteristic_type = 'All Pupils')}


#Calculating pupils entered for EBacc
EBACC_entered_gender <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, Gender, "Gender") 
EBACC_notentered_gender <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, Gender, "Gender")
EBACC_entered_sen <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, SEN, "SEN")
EBACC_notentered_sen <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, SEN, "SEN")
EBACC_entered_eal <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, EAL, "EAL")
EBACC_notentered_eal <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, EAL, "EAL")
EBACC_entered_disadvantage <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 1, Disadvantage, "Disadvantage")
EBACC_notentered_disadvantage <- func_attainment_char(join_data_attainment, EBACC_E_PTQ_EE, 0, Disadvantage, "Disadvantage")
EBACC_all_pupils <- func_attainment_allgen(join_data_attainment,EBACC_E_PTQ_EE)


EBACC_all <- rbind(EBACC_entered_gender, EBACC_notentered_gender, EBACC_entered_sen, EBACC_notentered_sen,
                   EBACC_entered_eal, EBACC_notentered_eal, EBACC_entered_disadvantage,
                   EBACC_notentered_disadvantage, EBACC_all_pupils) %>% 
  pivot_wider(names_from = EBACC_E_PTQ_EE, values_from = n) %>% 
  rename('Entered_for_EBacc' = '1') %>%  
  rename('Not_Entered' = '0')

#Percentages
EBACC_all_perc <- left_join(EBACC_all,
                            EBACC_all %>% janitor::adorn_percentages(),
                            by = c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>%
  rename('EBacc_all_Entered' = 'Entered_for_EBacc.x') %>% 
  rename('EBacc_all_Not_Entered' = 'Not_Entered.x') %>% 
  rename('EBacc_all_perc_Entered' = 'Entered_for_EBacc.y') %>% 
  rename('EBacc_all_perc_Not_Entered' = 'Not_Entered.y')




#Calculating 9-4 grade achievement in EBacc
EBACC94_achieved_gender <- func_attainment_char(join_data_attainment, EBACC_94, 1, Gender, "Gender") 
EBACC94_notachieved_gender <- func_attainment_char(join_data_attainment, EBACC_94, 0, Gender, "Gender")
EBACC94_achieved_sen <- func_attainment_char(join_data_attainment, EBACC_94, 1, SEN, "SEN")
EBACC94_notachieved_sen <- func_attainment_char(join_data_attainment, EBACC_94, 0, SEN, "SEN")
EBACC94_achieved_eal <- func_attainment_char(join_data_attainment, EBACC_94, 1, EAL, "EAL")
EBACC94_notachieved_eal <- func_attainment_char(join_data_attainment, EBACC_94, 0, EAL, "EAL")
EBACC94_achieved_disadvantage <- func_attainment_char(join_data_attainment, EBACC_94, 1, Disadvantage, "Disadvantage")
EBACC94_notachieved_disadvantage <- func_attainment_char(join_data_attainment, EBACC_94, 0, Disadvantage, "Disadvantage")
EBACC94_all_pupils <- func_attainment_allgen(join_data_attainment,EBACC_94)

EBACC94_all <- rbind(EBACC94_achieved_gender, EBACC94_notachieved_gender, EBACC94_achieved_sen, EBACC94_notachieved_sen,
                     EBACC94_achieved_eal, EBACC94_notachieved_eal, EBACC94_achieved_disadvantage,
                     EBACC94_notachieved_disadvantage, EBACC94_all_pupils) %>% 
  pivot_wider(names_from = EBACC_94, values_from = n) %>% 
  rename('Achieved_EBacc_9-4' = '1') %>%  
  rename('EBacc_9-4_Not_Achieved' = '0')

EBACC94_all_perc <- left_join(EBACC94_all,
                              EBACC94_all %>% janitor::adorn_percentages(),
                              by = c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>%
  rename('EBacc_9-4_Achieved' = 'Achieved_EBacc_9-4.x') %>% 
  rename('EBacc_9-4_Not_Achieved' = 'EBacc_9-4_Not_Achieved.x') %>% 
  rename('EBacc_9-4_perc_Achieved' = 'Achieved_EBacc_9-4.y') %>% 
  rename('EBacc_9-4_perc_Not_Achieved' = 'EBacc_9-4_Not_Achieved.y')




#Calculating 9-5 grade achievement in EBacc
EBACC95_achieved_gender <- func_attainment_char(join_data_attainment, EBACC_95, 1, Gender, "Gender") 
EBACC95_notachieved_gender <- func_attainment_char(join_data_attainment, EBACC_95, 0, Gender, "Gender")
EBACC95_achieved_sen <- func_attainment_char(join_data_attainment, EBACC_95, 1, SEN, "SEN")
EBACC95_notachieved_sen <- func_attainment_char(join_data_attainment, EBACC_95, 0, SEN, "SEN")
EBACC95_achieved_eal <- func_attainment_char(join_data_attainment, EBACC_95, 1, EAL, "EAL")
EBACC95_notachieved_eal <- func_attainment_char(join_data_attainment, EBACC_95, 0, EAL, "EAL")
EBACC95_achieved_disadvantage <- func_attainment_char(join_data_attainment, EBACC_95, 1, Disadvantage, "Disadvantage")
EBACC95_notachieved_disadvantage <- func_attainment_char(join_data_attainment, EBACC_95, 0, Disadvantage, "Disadvantage")
EBACC95_all_pupils <- func_attainment_allgen(join_data_attainment,EBACC_95)

EBACC95_all <- rbind(EBACC95_achieved_gender, EBACC95_notachieved_gender, EBACC95_achieved_sen, EBACC95_notachieved_sen,
                     EBACC95_achieved_eal, EBACC95_notachieved_eal, EBACC95_achieved_disadvantage,
                     EBACC95_notachieved_disadvantage, EBACC95_all_pupils) %>% 
  pivot_wider(names_from = EBACC_95, values_from = n) %>% 
  rename('Achieved_EBacc_9-5' = '1') %>%  
  rename('EBacc_9-5_Not_Achieved' = '0')

EBACC95_all_perc <- left_join(EBACC95_all,
                              EBACC95_all %>% janitor::adorn_percentages(),
                              by = c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>%
  rename('EBacc_9-5_Achieved' = 'Achieved_EBacc_9-5.x') %>% 
  rename('EBacc_9-5_Not_Achieved' = 'EBacc_9-5_Not_Achieved.x') %>% 
  rename('EBacc_9-5_perc_Achieved' = 'Achieved_EBacc_9-5.y') %>% 
  rename('EBacc_9-5_perc_Not_Achieved' = 'EBacc_9-5_Not_Achieved.y')


#Calculating 9-4 grade achievement in English and Maths
L2B94_achieved_gender <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, Gender, "Gender") 
L2B94_notachieved_gender <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, Gender, "Gender")
L2B94_achieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, SEN, "SEN")
L2B94_notachieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, SEN, "SEN")
L2B94_achieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, EAL, "EAL")
L2B94_notachieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, EAL, "EAL")
L2B94_achieved_disadvantage <- func_attainment_char(join_data_attainment, L2BASICS_94, 1, Disadvantage, "Disadvantage")
L2B94_notachieved_disadvantage <- func_attainment_char(join_data_attainment, L2BASICS_94, 0, Disadvantage, "Disadvantage")
L2B94_all_pupils <- func_attainment_allgen(join_data_attainment,L2BASICS_94)

L2B94_all <- rbind(L2B94_achieved_gender, L2B94_notachieved_gender, L2B94_achieved_sen, L2B94_notachieved_sen,
                   L2B94_achieved_eal, L2B94_notachieved_eal, L2B94_achieved_disadvantage,
                   L2B94_notachieved_disadvantage, L2B94_all_pupils) %>% 
  pivot_wider(names_from = L2BASICS_94, values_from = n) %>% 
  rename('Achieved_Basics_9-4' = '1') %>%  
  rename('Basics_9-4_Not_Achieved' = '0')

L2B94_all_perc <- left_join(L2B94_all,
                            L2B94_all %>% janitor::adorn_percentages(),
                            by = c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>%
  rename('Basics_9-4_Achieved' = 'Achieved_Basics_9-4.x') %>% 
  rename('Basics_9-4_Not_Achieved' = 'Basics_9-4_Not_Achieved.x') %>% 
  rename('Basics_9-4_perc_Achieved' = 'Achieved_Basics_9-4.y') %>% 
  rename('Basics_9-4_perc_Not_Achieved' = 'Basics_9-4_Not_Achieved.y')



#Calculating 9-5 grade achievement in English and Maths
L2B95_achieved_gender <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, Gender, "Gender") 
L2B95_notachieved_gender <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, Gender, "Gender") 
L2B95_achieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, SEN, "SEN")
L2B95_notachieved_sen <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, SEN, "SEN")
L2B95_achieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, EAL, "EAL")
L2B95_notachieved_eal <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, EAL, "EAL")
L2B95_achieved_disadvantage <- func_attainment_char(join_data_attainment, L2BASICS_95, 1, Disadvantage, "Disadvantage")
L2B95_notachieved_disadvantage <- func_attainment_char(join_data_attainment, L2BASICS_95, 0, Disadvantage, "Disadvantage")
L2B95_all_pupils <- func_attainment_allgen(join_data_attainment,L2BASICS_95)

L2B95_all <- rbind(L2B95_achieved_gender, L2B95_notachieved_gender, L2B95_achieved_sen, L2B95_notachieved_sen, 
                   L2B95_achieved_eal, L2B95_notachieved_eal, L2B95_achieved_disadvantage, 
                   L2B95_notachieved_disadvantage, L2B95_all_pupils) %>% 
  pivot_wider(names_from = L2BASICS_95, values_from = n) %>% 
  rename('Achieved_Basics_9-5' = '1') %>%  
  rename('Basics_9-5_Not_Achieved' = '0')

L2B95_all_perc <- left_join(L2B95_all,
                            L2B95_all %>% janitor::adorn_percentages(),
                            by = c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>%
  rename('Basics_9-5_Achieved' = 'Achieved_Basics_9-5.x') %>% 
  rename('Basics_9-5_Not_Achieved' = 'Basics_9-5_Not_Achieved.x') %>% 
  rename('Basics_9-5_perc_Achieved' = 'Achieved_Basics_9-5.y') %>% 
  rename('Basics_9-5_perc_Not_Achieved' = 'Basics_9-5_Not_Achieved.y')



# create a single table to join all 5 headline measures together
attainment_TM <- EBACC_all_perc %>% 
  left_join (EBACC94_all_perc, by =  c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>%
  left_join (EBACC95_all_perc, by =  c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>%
  left_join (L2B94_all_perc, by =  c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>%
  left_join (L2B95_all_perc, by =  c('ks2em_band', 'ks2em', 'characteristic_type', 'characteristic_value')) %>% 
  mutate_at(vars(contains("perc")), ~(round2(.*100, 1)))



#create the final tidy data file from the attainment_TM table
attainment_tidy_data <- attainment_TM %>%
  mutate(time_period = '202122',
         time_identifier = 'Academic year',
         geographic_level = 'National',
         country_code = 'E92000001',
         country_name = 'England',
         version = 'Provisional') %>%
  arrange(characteristic_type,#comment back for app use,
          characteristic_value, ks2em_band) %>% 
  select(time_period, time_identifier, geographic_level, country_code, country_name, version,
         characteristic_type,#comment back for app use,
         
         ##comment back in for data files
         ## characteristic_value, KS2_Prior=ks2em, 'EBacc_all_Entered',
         ## 'EBacc_all_Not_Entered','EBacc_all_%c_Entered', 'EBacc_all_%c_Not_Entered', 'EBacc_9-4_Achieved','EBacc_9-4_Not_Achieved',
         ## 'EBacc_9-4_%c_Achieved','EBacc_9-4_%c_Not_Achieved','EBacc_9-5_Achieved','EBacc_9-5_Not_Achieved',
         ## 'EBacc_9-5_%c_Achieved','EBacc_9-5_%c_Not_Achieved','Basics_9-4_Achieved','Basics_9-4_Not_Achieved',
         ## 'Basics_9-4_%c_Achieved','Basics_9-4_%c_Not_Achieved', 'Basics_9-5_Achieved','Basics_9-5_Not_Achieved',
         ## 'Basics_9-5_%c_Achieved', 'Basics_9-5_%c_Not_Achieved') %>% 
         
         ##section (app use only)
         characteristic_value, KS2_Prior=ks2em, 'EBacc_all_Entered',
         'EBacc_all_Not_Entered','EBacc_all_perc_Entered', 'EBacc_all_perc_Not_Entered', 'EBacc_9-4_Achieved','EBacc_9-4_Not_Achieved',
         'EBacc_9-4_perc_Achieved','EBacc_9-4_perc_Not_Achieved','EBacc_9-5_Achieved','EBacc_9-5_Not_Achieved',
         'EBacc_9-5_perc_Achieved','EBacc_9-5_perc_Not_Achieved','Basics_9-4_Achieved','Basics_9-4_Not_Achieved',
         'Basics_9-4_perc_Achieved','Basics_9-4_perc_Not_Achieved', 'Basics_9-5_Achieved','Basics_9-5_Not_Achieved',
         'Basics_9-5_perc_Achieved', 'Basics_9-5_perc_Not_Achieved') %>% 
  
  
  mutate_all(~replace(.,is.na(.),0))


#copying data to an Excel file
#save_tidy_data_file_attainment = 'Y:/Pre-16 development/Routine products/Transition Matrices/TM Dev/8.TM_in_R/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Attainment_Scaled_Scores_Final.csv'
#save_tidy_data_file_attainment = 'C:/Users/SMANCHESTER.AD/OneDrive - Department for Education/Documents/R Projects/KS4_TM_Scaled_Scores/2021_Tidy_Data_Output_Attainment_Scaled_Scores_Final.csv'
save_tidy_data_file_attainment = 'C:/Users/tabdulla/OneDrive - Department for Education/ONE DRIVE/TM/KS4_TM_Scaled_Scores/2022_Tidy_Data_Output_Attainment_Scaled_Scores_Final.csv'#update year
write.table(attainment_tidy_data, save_tidy_data_file_attainment, row.names = FALSE, sep = ',')
