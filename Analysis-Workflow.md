---
title: "SEISMIC WG1-P4"
author: "Eben"
date: "4/17/2020"
output:
  html_document:
    keep_md: true
    code_folding: show
    theme: cosmo
    toc: yes
    toc_depth: 2
    toc_float: yes
  pdf_document:
    toc: yes
    toc_depth: '2'
subtitle: Analysis Workflow
---



# **Data Processing (Institution Specific)**
(see [Data Description](https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1679989021) for shared SEISMIC variable names)

## 0. Startup

### a.  Load R pkgs

```r
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse")
```

### b.	Load full dataset



```r
# CHANGE TO YOUR FILE PATH
df_full <- read.csv("~/YOUR FILE PATH HERE.csv")
head(names(df_full))
```


```
## [1] "X"                      "EMPLID_H"               "ACADEMIC_PLAN_DESCR"   
## [4] "ACADEMIC_PROGRAM_CD"    "ACADEMIC_PROGRAM_DESCR" "BIRTH_DT"
```

## 1.	Clean Student level variables

### a. Rename and generate/recode student level variables as needed to match common SEISMIC AP variable names



```r
## Student Level ####
df_std <- df_full %>%
  # Renamed variables
  mutate(st_id = EMPLID_H) %>%
  mutate(ethniccode = ETHNIC_GROUP_CD) %>%
  mutate(famincome = abs(AGI)) %>%
  # Recoded variable
  mutate(firstgen = recode(FIRST_GENERATION_DESCR, "First Generation" = 1, "Not First Generation" = 0, "Unknown" = 0)) %>%
  mutate(ethniccode_cat = recode(ETHNIC_GROUP_CD, "HISPA" = 1, "BLACK" = 1, "AMIND" = 1, "PACIF" = 1, "ASIAN" = 2, "WHITE" = 0)) %>%
  mutate(urm = recode(ETHNIC_GROUP_CD, "HISPA" = 1, "BLACK" = 1, "AMIND" = 1, "PACIF" = 1, "ASIAN" = 0, "WHITE" = 0)) %>%
  mutate(gender = recode(GENDER_CD, "F"=1, "M"=0, "m"=0, "U" = 2)) %>%
  mutate(female = recode(GENDER_CD, "F"=1, "M"=0, "m"=0, "U" = 2)) %>%
  mutate(lowincomflag = if_else(is.na(AGI), 0,
                                if_else(AGI <= 46435, 1,0))) 

# etc...
```

## 2.	Clean Course level variables

### a.	Rename and generate/recode course level variables as needed to match common SEISMIC AP variable names



```r
## Course Level ####
df_crs <- df_full %>%
  # Renamed variables
  mutate(st_id = EMPLID_H) %>%
  mutate(crs_sbj = SUBJECT_CD) %>%
  mutate(crs_catalog = CATALOG_NBR) %>%
  mutate(crs_name	= CLASS_TITLE) %>%
  mutate(crs_retake = REPEAT_CD) %>%
  mutate(crs_term	= TERM_CD) %>%
  # Recoded variables
  mutate(numgrade = GRADE_POINTS/UNITS_TAKEN) %>%
  mutate(numgrade_w = if_else(COURSE_GRADE_CD == "W", 1, 0)) %>%
  separate(as.character("TERM_CD"), c("crs_YEAR", "crs_SEMESTER"), 3, remove = FALSE) %>%
  separate(as.character("crs_YEAR"), c("crs_DEC", "crs_YEAR"), 1) %>% 
  mutate(crs_term_yr = crs_YEAR) %>%
  mutate(crs_term_sem = crs_SEMESTER) %>%
  mutate(summer_crs = if_else(endsWith(as.character(TERM_CD),"7"), 1, 0))

# etc...
```

### b.	For each subject course (1 and 2), create dataframe of only first time taking that course



```r
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

# repeat for Bio2, Chem1, Chem2, Phys1, Phys2
```

## 3.	Clean AP Level variables

### a.  For each AP subject, rename and generate/recode course level variables as needed to match common SEISMIC AP variable names



```r
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

# repeat for Chem, Phys
```
        
## 4. Create stacked dataset

### a.  For each course subject, join previously create dataframes for first time taking Course1, Course2, and AP
- Include new variable: “discipline” as flag for each subject
    + BIO
    + CHEM
    + PHYS



```r
# Bio (N=3090)
df_bio <- df_std %>%
  right_join(df_crs_bio2, by = "st_id") %>%
  full_join(df_crs_bio1, by = "st_id") %>%
  full_join(df_ap_bio, by = "st_id") %>%
  mutate(discipline = "BIO") %>%
  mutate(skipped_1 = ifelse(tookcourse == 0 & tookcourse_2 == 1, 1, 0)) %>%
  select(discipline, st_id:hsgpa, crs_sbj.x:current_major.x, crs_sbj.y:current_major.y, 
         aptaker, apscore, apscore_full, apskipper, 
         tookcourse, tookcourse_2, skipped_1) 

# repeat for Chem, Phys
```

### b. Stack all dataframes, with course indicator

```r
# Stacked dataframe with Bio, Chem, Phys
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
```

### c. Should end up with something that looks like [this](https://docs.google.com/spreadsheets/d/1Sj5kaFNGUkBhRoOH3cIPm-97UEBZmcFkbKGjzBbKWc0/edit?usp=drive_open&ouid=118183464940790632947).


```r
names(df_clean)
```

```
##  [1] "discipline"         "st_id"              "firstgen"          
##  [4] "ethniccode"         "ethniccode_cat"     "urm"               
##  [7] "gender"             "female"             "famincome"         
## [10] "lowincomflag"       "transfer"           "international"     
## [13] "ell"                "us_hs"              "cohort"            
## [16] "cohort_2013"        "cohort_2014"        "cohort_2015"       
## [19] "cohort_2016"        "cohort_2017"        "cohort_2018"       
## [22] "apyear"             "englsr"             "mathsr"            
## [25] "hsgpa"              "crs_sbj_2"          "crs_catalog_2"     
## [28] "crs_name_2"         "numgrade_2"         "numgrade_w_2"      
## [31] "crs_retake_2"       "crs_term_2"         "crs_term_yr_2"     
## [34] "crs_term_sem_2"     "summer_crs_2"       "TERM_REF_2"        
## [37] "enrl_from_cohort_2" "crs_credits_2"      "crs_component_2"   
## [40] "class_number_2"     "current_major_2"    "crs_sbj"           
## [43] "crs_catalog"        "crs_name"           "numgrade"          
## [46] "numgrade_w"         "crs_retake"         "crs_term"          
## [49] "crs_termr.y"        "crs_term_sem"       "summer_crs"        
## [52] "TERM_REF"           "enrl_from_cohort"   "crs_credits"       
## [55] "crs_component"      "class_number"       "current_major"     
## [58] "aptaker"            "apscore"            "apscore_full"      
## [61] "apskipper"          "tookcourse"         "tookcourse_2"      
## [64] "skipped_1"
```

# **Data Analysis (Same Across Institutions)**

## 0. Startup

### a.  Load R pkgs

```r
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "epiDisplay")
```

### b.	Load full dataset



```r
# CHANGE TO YOUR FILE PATH
df_clean <- read.csv("~/YOUR FILE PATH HERE.csv")
head(names(df_clean))
```


```
## [1] "X"              "discipline"     "st_id"          "firstgen"      
## [5] "ethniccode"     "ethniccode_cat"
```

### c. Filter for student level inclusion/exclusion criteria
- Include: 
    + First time freshmen
    + Took course 2
    + Cohort 2013-2018
- Exclude:
    + Transfer students
    + International students
    + Honors

### d.	Create dataframes for each analysis
        
-	For each course: Full Sample, AP taken after test re-design



```r
# Bio
# Took 2nd course in sequence
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013)

# repeat for Chem, Phys
```

-	For each course: Took AP

```r
# Took AP 
df_BYtakers <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(aptaker == 1)

# repeat for Chem, Phys
```

- For each course: Recieved a skip-eligible score 

```r
# Skip eligible
df_BYeligible <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apskipper == 1)

# repeat for Chem, Phys
```

- For each course: Recieved a skip-eligible score (at each eligible score) 

```r
# Skip eligible (at each eligble scores)
df_BYeligible.4 <- df_bio2 %>%
  subset(apscore == 4)
df_BYeligible.5 <- df_bio2 %>%
  subset(apscore == 5)

# Repeat for Chem, Phys
```

## 1.	Run Models (for each Course)
(see also [Sample Descriptions](https://docs.google.com/spreadsheets/d/1rN8W_iz1mr7lEzBGfdTZHa45wKOSLiSF8VEpChCPsmE/edit#gid=129222174) for model specs.)

## RQ1: What student characteristics are associated with student participation and success in AP courses for students enrolled at the selected universities?

## *RQ1a: Who takes AP?*
      
- Sample: Took Course 2 dataframe
- DV: aptaker
- IV: factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) + scale(hsgpa) +                   scale(mathsr) + scale(englsr)
- COV: factor(cohort) 




```r
# Model 1a: Credits ####
#Bio
m1.a_bio <- glm(aptaker ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
               binomial(link = "logit"), df_bio2)
logistic.display(m1.a_bio)

# Repeat for Chem, Phys
```


```r
logistic.display(m1.a_bio)
```

```
##  
##                                        OR  lower95ci  upper95ci     Pr(>|Z|)
## factor(firstgen)1            1.061059e+00 0.77605614  1.4507279 7.103658e-01
## factor(lowincomflag)1        9.336068e-01 0.70265811  1.2404634 6.356362e-01
## factor(female)1              8.664525e-01 0.72289022  1.0385256 1.209106e-01
## factor(urm)1                 1.167308e+00 0.86487668  1.5754941 3.119529e-01
## scale(hsgpa)                 9.685349e-01 0.88305993  1.0622834 4.976338e-01
## scale(mathsr)                1.099148e+00 0.98850133  1.2221806 8.075492e-02
## scale(englsr)                1.036213e+00 0.93368411  1.1500009 5.033831e-01
## factor(enrl_from_cohort)1.5  4.782860e-01 0.37706970  0.6066716 1.205953e-09
## factor(enrl_from_cohort)1.66 8.037013e-01 0.50238773  1.2857315 3.619950e-01
## factor(enrl_from_cohort)2    4.246465e-01 0.26461771  0.6814536 3.862889e-04
## factor(enrl_from_cohort)2.5  7.921156e-02 0.01833322  0.3422461 6.837549e-04
## factor(enrl_from_cohort)2.66 6.623396e-01 0.31820030  1.3786718 2.707070e-01
## factor(enrl_from_cohort)3    9.115404e-01 0.30393872  2.7337941 8.687222e-01
## factor(enrl_from_cohort)3.5  1.883956e+00 0.16982774 20.8993588 6.059368e-01
## factor(enrl_from_cohort)3.66 7.875640e-01 0.25184226  2.4628793 6.814186e-01
## factor(enrl_from_cohort)4    1.283459e-06 0.00000000        Inf 9.797858e-01
## factor(enrl_from_cohort)4.5  7.058345e-07 0.00000000        Inf 9.788950e-01
## factor(enrl_from_cohort)4.66 1.264938e-06 0.00000000        Inf 9.797642e-01
```

## *RQ1b.* 

- Sample: Took AP dataframe
- DV: apscore
- IV: factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) + scale(hsgpa) +                   scale(mathsr) + scale(englsr)
- COV: factor(crs_term) 
            
## (Etc….)
