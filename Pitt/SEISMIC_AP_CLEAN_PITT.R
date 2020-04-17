#SEISMIC AP - Variable Conversion
#### Setup ####
# Load packages and data
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse")

# Functions and settings ####
# Use dplyr for 'select'
select <- dplyr::select

# Load FULL MERGED Dataframe ####
df_full <- read.csv("~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/2191_MATH_FULL.csv")     
df_full <- df_full %>%
  filter(CAMPUS_CD == "PIT")           # TOTAL N = 670191
names(df_full)

#### CLEANING ####
## STABLE - Student Level ####
df_std <- df_full %>%
  # Rename variables
  mutate(st_id = EMPLID_H) %>%
  mutate(firstgen = recode(FIRST_GENERATION_DESCR, "First Generation" = 1, "Not First Generation" = 0, "Unknown" = 0)) %>%
  mutate(ethniccode = ETHNIC_GROUP_CD) %>%
  mutate(ethniccode_cat = recode(ETHNIC_GROUP_CD, "HISPA" = 1, "BLACK" = 1, "AMIND" = 1, "PACIF" = 1, "ASIAN" = 2, "WHITE" = 0)) %>%
  mutate(urm = recode(ETHNIC_GROUP_CD, "HISPA" = 1, "BLACK" = 1, "AMIND" = 1, "PACIF" = 1, "ASIAN" = 0, "WHITE" = 0)) %>%
  mutate(gender = recode(GENDER_CD, "F"=1, "M"=0, "m"=0, "U" = 2)) %>%
  mutate(female = recode(GENDER_CD, "F"=1, "M"=0, "m"=0, "U" = 2)) %>%
  mutate(famincome = abs(AGI)) %>%
  mutate(lowincomflag = if_else(is.na(AGI), 0,
                                if_else(AGI <= 46435, 1,0))) %>%
  mutate(transfer = if_else(TOT_TRNSFR_CREDITS <= 16 | is.na(TOT_TRNSFR_CREDITS), 0, 1)) %>%
  mutate(international = if_else(CITIZENSHIP_STATUS_DESCR == "U.S. Citizen", 0, 1)) %>%
  mutate(ell = if_else(TOEFL_SCORE > 0, 0, 1)) %>%
  mutate(us_hs = if_else(is.na(HS_GPA), 0, 1)) %>%
  separate(as.character("START_TRM_CD"), c("st_YEAR", "st_SEMESTER"), 3, remove = FALSE) %>%
  separate(as.character("st_YEAR"), c("st_DEC", "st_YEAR"), 1) %>%
  mutate(cohort = as.numeric(st_YEAR)) %>%
  mutate(cohort = ifelse(cohort >= 20, cohort + 1900, cohort + 2000)) %>%
  mutate(cohort_2013 = ifelse(cohort == 2013, 1,0)) %>%
  mutate(cohort_2014 = ifelse(cohort == 2014, 1,0)) %>%
  mutate(cohort_2015 = ifelse(cohort == 2015, 1,0)) %>%
  mutate(cohort_2016 = ifelse(cohort == 2016, 1,0)) %>%
  mutate(cohort_2017 = ifelse(cohort == 2017, 1,0)) %>%
  mutate(cohort_2018 = ifelse(cohort == 2018, 1,0)) %>%
  #HACK for apyear (cohort year - 1)
  mutate(apyear = cohort - 1) %>%
  mutate(englsr = if_else(ACT_HIGH_MATH == 36, 800,
                          if_else(ACT_HIGH_MATH == 35, 780,
                                  if_else(ACT_HIGH_MATH == 34, 760,
                                          if_else(ACT_HIGH_MATH == 33, 740,
                                                  if_else(ACT_HIGH_MATH == 32,	720,
                                                          if_else(ACT_HIGH_MATH == 31,	710,
                                                                  if_else(ACT_HIGH_MATH == 30,	700,
                                                                          if_else(ACT_HIGH_MATH == 29,	680,
                                                                                  if_else(ACT_HIGH_MATH == 28,	660,
                                                                                          if_else(ACT_HIGH_MATH == 27,	640,
                                                                                                  if_else(ACT_HIGH_MATH == 26,	610,
                                                                                                          if_else(ACT_HIGH_MATH == 25,	590,
                                                                                                                  if_else(ACT_HIGH_MATH == 24,	580,
                                                                                                                          if_else(ACT_HIGH_MATH == 23,	560,
                                                                                                                                  if_else(ACT_HIGH_MATH == 22,	540,
                                                                                                                                          if_else(ACT_HIGH_MATH == 21,	530,
                                                                                                                                                  if_else(ACT_HIGH_MATH == 20,	520,
                                                                                                                                                          if_else(ACT_HIGH_MATH == 19,	510,
                                                                                                                                                                  if_else(ACT_HIGH_MATH == 18,	500,
                                                                                                                                                                          if_else(ACT_HIGH_MATH == 17,	470,                                                                                                                                                                                                if_else(ACT_HIGH_MATH == 16	, 430,
                                                                                                                                                                                                                                                                                                                                                                                                                   if_else(ACT_HIGH_MATH == 15	, 400,
                                                                                                                                                                                                                                                                                                                                                                                                                           if_else(ACT_HIGH_MATH == 14	, 360,
                                                                                                                                                                                                                                                                                                                                                                                                                                   if_else(ACT_HIGH_MATH == 13	, 330,
                                                                                                                                                                                                                                                                                                                                                                                                                                           if_else(ACT_HIGH_MATH == 12,	310,
                                                                                                                                                                                                                                                                                                                                                                                                                                                   if_else(ACT_HIGH_MATH == 11,	280,
                                                                                                                                                                                                                                                                                                                                                                                                                                                           if_else(ACT_HIGH_MATH == 10,	260, NaN )))))))))))))))))))))))))))) %>%
  mutate(englsr = ifelse(SAT_HIGH_VERBAL %in% NA, englsr, SAT_HIGH_VERBAL)) %>%
  mutate(mathsr = if_else(ACT_HIGH_MATH == 36, 800,
                          if_else(ACT_HIGH_MATH == 35, 780,
                                  if_else(ACT_HIGH_MATH == 34, 760,
                                          if_else(ACT_HIGH_MATH == 33, 740,
                                                  if_else(ACT_HIGH_MATH == 32,	720,
                                                          if_else(ACT_HIGH_MATH == 31,	710,
                                                                  if_else(ACT_HIGH_MATH == 30,	700,
                                                                          if_else(ACT_HIGH_MATH == 29,	680,
                                                                                  if_else(ACT_HIGH_MATH == 28,	660,
                                                                                          if_else(ACT_HIGH_MATH == 27,	640,
                                                                                                  if_else(ACT_HIGH_MATH == 26,	610,
                                                                                                          if_else(ACT_HIGH_MATH == 25,	590,
                                                                                                                  if_else(ACT_HIGH_MATH == 24,	580,
                                                                                                                          if_else(ACT_HIGH_MATH == 23,	560,
                                                                                                                                  if_else(ACT_HIGH_MATH == 22,	540,
                                                                                                                                          if_else(ACT_HIGH_MATH == 21,	530,
                                                                                                                                                  if_else(ACT_HIGH_MATH == 20,	520,
                                                                                                                                                          if_else(ACT_HIGH_MATH == 19,	510,
                                                                                                                                                                  if_else(ACT_HIGH_MATH == 18,	500,
                                                                                                                                                                          if_else(ACT_HIGH_MATH == 17,	470,
                                                                                                                                                                                  if_else(ACT_HIGH_MATH == 16	, 430,
                                                                                                                                                                                          if_else(ACT_HIGH_MATH == 15	, 400,
                                                                                                                                                                                                  if_else(ACT_HIGH_MATH == 14	, 360,
                                                                                                                                                                                                          if_else(ACT_HIGH_MATH == 13	, 330,
                                                                                                                                                                                                                  if_else(ACT_HIGH_MATH == 12,	310,
                                                                                                                                                                                                                          if_else(ACT_HIGH_MATH == 11,	280,
                                                                                                                                                                                                                                  if_else(ACT_HIGH_MATH == 10,	260, NaN )))))))))))))))))))))))))))) %>%
  mutate(mathsr = ifelse(SAT_HIGH_MATH %in% NA, mathsr, SAT_HIGH_MATH)) %>%
  mutate(hsgpa = HS_GPA) %>%
  # Select only common-named variables used in analysis
  select(st_id, firstgen:hsgpa) %>%
  # Remove any duplicate cases
  distinct()

write.csv(df_std, file = "SEISMIC_AP_STUDENT_CLEAN.csv")          #UNIQUE N = 22976

#### Course Level ####
df_crs <- df_full %>%
  # Rename variables
  mutate(st_id = EMPLID_H) %>%
  mutate(crs_sbj = SUBJECT_CD) %>%
  mutate(crs_catalog = CATALOG_NBR) %>%
  mutate(crs_name	= CLASS_TITLE) %>%
  mutate(numgrade = GRADE_POINTS/UNITS_TAKEN) %>%
  mutate(numgrade_w = if_else(COURSE_GRADE_CD == "W", 1, 0)) %>%
  mutate(crs_retake = REPEAT_CD) %>%
  mutate(crs_term	= TERM_CD) %>%
  separate(as.character("TERM_CD"), c("crs_YEAR", "crs_SEMESTER"), 3, remove = FALSE) %>%
  separate(as.character("crs_YEAR"), c("crs_DEC", "crs_YEAR"), 1) %>% 
  mutate(crs_term_yr = crs_YEAR) %>%
  mutate(crs_term_sem = crs_SEMESTER) %>%
  mutate(summer_crs = if_else(endsWith(as.character(TERM_CD),"7"), 1, 0)) %>%
  mutate(TERM_REF = START_TRM_CD-TERM_CD) %>%
  separate(as.character("START_TRM_CD"), c("st_YEAR", "st_SEMESTER"), 3, remove = FALSE) %>%
  separate(as.character("st_YEAR"), c("st_DEC", "st_YEAR"), 1) %>%
  mutate(enrl_from_cohort = if_else(st_SEMESTER == "1" & TERM_REF == "0", 1,
                                    if_else(TERM_REF == -3, 1.5,
                                            if_else(TERM_REF == -6, 1.66,
                                                    if_else(TERM_REF == -10, 2,
                                                            if_else(TERM_REF == -13, 2.5,
                                                                    if_else(TERM_REF == -16, 2.66,
                                                                            if_else(TERM_REF == -20, 3,
                                                                                    if_else(TERM_REF == -23, 3.5,
                                                                                            if_else(TERM_REF == -26, 3.66,
                                                                                                    if_else(TERM_REF == -30, 4,
                                                                                                            if_else(TERM_REF == -33, 4.5,
                                                                                                                    if_else(TERM_REF == -36, 4.66, 
                                                                                                                            if_else(st_SEMESTER == "4" & TERM_REF == "0", 1,
                                                                                                                                    if_else(TERM_REF == -3, 1.5,
                                                                                                                                            if_else(TERM_REF == -7, 1.66,
                                                                                                                                                    if_else(TERM_REF == -10, 2,
                                                                                                                                                            if_else(TERM_REF == -13, 2.5,
                                                                                                                                                                    if_else(TERM_REF == -17, 2.66,
                                                                                                                                                                            if_else(TERM_REF == -20, 3,
                                                                                                                                                                                    if_else(TERM_REF == -23, 3.5,
                                                                                                                                                                                            if_else(TERM_REF == -27, 3.66,
                                                                                                                                                                                                    if_else(TERM_REF == -30, 4,
                                                                                                                                                                                                            if_else(TERM_REF == -33, 4.5,
                                                                                                                                                                                                                    if_else(TERM_REF == -37, 4.66, 
                                                                                                                                                                                                                            if_else(st_SEMESTER == "7" & TERM_REF == "0", 1,
                                                                                                                                                                                                                                    if_else(TERM_REF == -4, 1.5,
                                                                                                                                                                                                                                            if_else(TERM_REF == -7, 1.66,
                                                                                                                                                                                                                                                    if_else(TERM_REF == -10, 2,
                                                                                                                                                                                                                                                            if_else(TERM_REF == -14, 2.5,
                                                                                                                                                                                                                                                                    if_else(TERM_REF == -17, 2.66,
                                                                                                                                                                                                                                                                            if_else(TERM_REF == -20, 3,
                                                                                                                                                                                                                                                                                    if_else(TERM_REF == -24, 3.5,
                                                                                                                                                                                                                                                                                            if_else(TERM_REF == -26, 3.66,
                                                                                                                                                                                                                                                                                                    if_else(TERM_REF == -30, 4,
                                                                                                                                                                                                                                                                                                            if_else(TERM_REF == -34, 4.5,
                                                                                                                                                                                                                                                                                                                    if_else(TERM_REF == -36, 4.66, NaN ))))))))))))))))))))))))))))))))))))) %>%
  #mutate(gpao = ?) %>%
  #mutate(begin_term_cum_gpa = ?) %>%
  mutate(crs_credits = UNITS_TAKEN) %>%
  #mutate(instructor_name = NA) %>%
  mutate(crs_component= CLASS_COMPONENT_DESCR) %>%
  mutate(class_number	= CLASS_NBR) %>%
  mutate(current_major = ACADEMIC_SUBPLAN_DESCR) %>%
  # Select only common-named variables used in analysis
  select(st_id, crs_sbj:current_major)

write.csv(df_crs, file = "SEISMIC_AP_COURSES_CLEAN.csv")

# By Course (Taking only First Attempt) ####
# Bio
df_crs_bio1 <- df_crs %>%
  filter(crs_sbj == "BIOSC" & (crs_catalog == "0150")) %>% # | crs_catalog == "0715")) %>%
  #Only first time taking course
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num = row_number()) %>%
  filter(crs_retake_num == 1) %>%
  #Only first time taking course (incl. Honors)
  group_by(st_id) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num2 = row_number()) %>%
  filter(crs_retake_num2 == 1)

df_crs_bio2 <- df_crs %>%
  filter(crs_sbj == "BIOSC" & (crs_catalog == "0160")) %>% # | crs_catalog == "0716")) %>%
  #Only first time taking course
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num = row_number()) %>%
  filter(crs_retake_num == 1) %>%
  #Only first time taking course (incl. Honors)
  group_by(st_id) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num2 = row_number()) %>%
  filter(crs_retake_num2 == 1)

#Chem
df_crs_chem1 <- df_crs %>%
  filter(crs_sbj == "CHEM" & (crs_catalog == "0110")) %>% # | crs_catalog == "0710")) %>%
  #Only first time taking course
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num = row_number()) %>%
  filter(crs_retake_num == 1) %>%
  #Only first time taking course (incl. Honors)
  group_by(st_id) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num2 = row_number()) %>%
  filter(crs_retake_num2 == 1)

df_crs_chem2 <- df_crs %>%
  filter(crs_sbj == "CHEM" & (crs_catalog == "0120")) %>% # | crs_catalog == "0720")) %>%
  #Only first time taking course
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num = row_number()) %>%
  filter(crs_retake_num == 1) %>%
  #Only first time taking course (incl. Honors)
  group_by(st_id) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num2 = row_number()) %>%
  filter(crs_retake_num2 == 1)

#Phys
df_crs_phys1 <- df_crs %>%
  filter(crs_sbj == "PHYS" & (crs_catalog == "0174")) %>% # | crs_catalog == "0475")) %>%
  #Only first time taking course
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num = row_number()) %>%
  filter(crs_retake_num == 1) %>%
  #Only first time taking course (incl. Honors)
  group_by(st_id) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num2 = row_number()) %>%
  filter(crs_retake_num2 == 1)

df_crs_phys2 <- df_crs %>%
  filter(crs_sbj == "PHYS" & (crs_catalog == "0175")) %>% # | crs_catalog == "0476")) %>%
  #Only first time taking course
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num = row_number()) %>%
  filter(crs_retake_num == 1) %>%
  #Only first time taking course (incl. Honors)
  group_by(st_id) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num2 = row_number()) %>%
  filter(crs_retake_num2 == 1)

##### AP Level ####
# By course ####
# Bio
df_ap_bio <- df_full %>%
  mutate(st_id = EMPLID_H) %>%
  mutate(aptaker = ifelse(is.na(BY), 0, 1)) %>%
  mutate(apskipper = ifelse(BY >= 4 & !is.na(BY), 1, 0)) %>%
  mutate(apskipper_2 = ifelse(BY == 5 & !is.na(BY), 1, 0)) %>%
  mutate(tookcourse = ifelse(
    SUBJECT_CD == "BIOSC" & (CATALOG_NBR == "0150") & # | CATALOG_NBR == "0715") & 
      COURSE_GRADE_CD != "W", 1, 0)) %>%
  mutate(tookcourse_2 = ifelse(
    SUBJECT_CD == "BIOSC" & (CATALOG_NBR == "0160") & # | CATALOG_NBR == "0716") & 
      COURSE_GRADE_CD != "W", 1, 0)) %>%
  #mutate(apyear = ?) %>%
  mutate(apscore = as.character(BY)) %>%
  mutate(apscore_full = ifelse(is.na(BY), 0, BY)) %>%
  select(st_id, aptaker:apscore_full) %>%
  group_by(st_id) %>%
  summarize_at(vars(-group_cols()),max)

#Chem
df_ap_chem <- df_full %>%
  mutate(st_id = EMPLID_H) %>%
  mutate(aptaker = ifelse(is.na(CH), 0, 1)) %>%
  mutate(apskipper = ifelse(CH >= 3 & !is.na(CH), 1, 0)) %>%
  mutate(apskipper_2 = ifelse(CH == 5 & !is.na(CH), 1, 0)) %>%
  mutate(tookcourse = ifelse(
    SUBJECT_CD == "CHEM" & (CATALOG_NBR == "0110") & # | CATALOG_NBR == "0710") & 
      COURSE_GRADE_CD != "W", 1, 0)) %>%
  mutate(tookcourse_2 = ifelse(
    SUBJECT_CD == "CHEM" & (CATALOG_NBR == "0120") & # | CATALOG_NBR == "0720") & 
      COURSE_GRADE_CD != "W", 1, 0)) %>%
  #mutate(apyear = ?) %>%
  mutate(apscore = as.character(CH)) %>%
  mutate(apscore_full = ifelse(is.na(CH), 0, CH)) %>%
  select(st_id, aptaker:apscore_full) %>%
  group_by(st_id) %>%
  summarize_at(vars(-group_cols()),max)

#Phys
df_ap_phys <- df_full %>%
  mutate(st_id = EMPLID_H) %>%
  mutate(aptaker_CE = ifelse(is.na(PHCE), 0, 1)) %>%
  mutate(aptaker_CM = ifelse(is.na(PHCM), 0, 1)) %>%
  mutate(aptaker = ifelse(aptaker_CE == 1 | aptaker_CM == 1, 1, 0)) %>%
  mutate(apskipperCE = ifelse(PHCE == 5 & !is.na(PHCE), 1, 0)) %>%
  mutate(apskipperCM = ifelse(PHCM == 5  & !is.na(PHCM), 1, 0)) %>%
  mutate(apskipper = ifelse(apskipperCE == 1 | apskipperCM == 1, 1, 0)) %>%
  mutate(tookcourse = ifelse(
    SUBJECT_CD == "PHYS" & (CATALOG_NBR == "0174") & # | CATALOG_NBR == "0475") & 
      COURSE_GRADE_CD != "W", 1, 0)) %>%
  mutate(tookcourse_2 = ifelse(
    SUBJECT_CD == "PHYS" & (CATALOG_NBR == "0175") & # | CATALOG_NBR == "0476") & 
      COURSE_GRADE_CD != "W", 1, 0)) %>%
  #mutate(apyear = ?) %>%
  mutate(apscore_CE = PHCE) %>%
  mutate(apscore_CM = PHCM) %>%
  mutate(apscore = ifelse(is.na(PHCM), PHCE, PHCM)) %>%
  mutate(apscore = as.character(apscore)) %>%
  mutate(apscore_CE_full = ifelse(is.na(PHCE), 0, PHCE)) %>%
  mutate(apscore_CM_full = ifelse(is.na(PHCM), 0, PHCM)) %>%
  mutate(apscore_full = ifelse(is.na(PHCM), PHCE, PHCM)) %>%
  mutate(apscore_full = ifelse(is.na(apscore_full), 0, apscore_full)) %>%
  select(st_id, aptaker:apscore_full) %>%
  group_by(st_id) %>%
  summarize_at(vars(-group_cols()),max)

#### Create Stacked Dataset #### 
# (STILL MISSING: gpao, begin_term_cum_gpa, instructor_name, apyear)
# Bio (N=3090)
df_bio <- df_std %>%
  right_join(df_crs_bio2, by = "st_id") %>%
  full_join(df_crs_bio1, by = "st_id") %>%
  full_join(df_ap_bio, by = "st_id") %>%
  mutate(discipline = "BIO") %>%
  mutate(skipped_1 = ifelse(tookcourse == 0 & tookcourse_2 == 1, 1, 0)) %>%
  select(discipline, st_id:hsgpa, crs_sbj.x:current_major.x, crs_sbj.y:current_major.y, 
         aptaker, apscore, apscore_full, apskipper, 
         tookcourse, tookcourse_2, skipped_1) %>%
  filter(transfer == 0) %>%
  filter(international == 0) %>%
  filter(crs_term.x > 2137 & crs_term.x < 2191)

#Chem (N=3019)
df_chem <- df_std %>%
  right_join(df_crs_chem2, by = "st_id") %>%
  full_join(df_crs_chem1, by = "st_id") %>%
  full_join(df_ap_chem, by = "st_id") %>%
  mutate(discipline = "CHEM") %>%
  mutate(skipped_1 = ifelse(tookcourse == 0 & tookcourse_2 == 1, 1, 0)) %>%
  select(discipline, st_id:hsgpa, crs_sbj.x:current_major.x, crs_sbj.y:current_major.y, 
         aptaker, apscore, apscore_full, apskipper, 
         tookcourse, tookcourse_2, skipped_1) %>%
  filter(transfer == 0) %>%
  filter(international == 0) %>%
  filter(crs_term.x > 2147 & crs_term.x < 2191)

#Phys (N=1598)
df_phys <- df_std %>%
  right_join(df_crs_phys2, by = "st_id") %>%
  full_join(df_crs_phys1, by = "st_id") %>%
  full_join(df_ap_phys, by = "st_id") %>%
  mutate(discipline = "PHYS") %>%
  mutate(skipped_1 = ifelse(tookcourse == 0 & tookcourse_2 == 1, 1, 0)) %>%
  select(discipline, st_id:hsgpa, crs_sbj.x:current_major.x, crs_sbj.y:current_major.y, 
         aptaker, apscore, apscore_full, apskipper,
         tookcourse, tookcourse_2, skipped_1) %>%
  filter(transfer == 0) %>%
  filter(international == 0) %>%
  filter(crs_term.x > 2157 & crs_term.x < 2191)

#Stacked data
df_clean <- rbind(df_bio, df_chem, df_phys)
df_clean <- df_clean %>%
  rename_at(vars(ends_with(".x")), 
            ~(str_replace(., ".x", "_2"))) %>%
  rename_at(vars(ends_with(".y")), 
            ~(str_replace(., ".y", "")))

rm(df_ap_bio, df_ap_chem, df_ap_phys, 
   df_bio, df_chem, df_phys, 
   df_crs_bio1, df_crs_bio2, df_crs_chem1, df_crs_chem2, df_crs_phys1, df_crs_phys2,
   df_crs, df_std)

write.csv(df_clean, file = "~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/SEISMIC_AP_CLEAN.csv")
