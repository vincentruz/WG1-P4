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
subtitle: Analysis Workflow
---



# **Data Processing (Institution Specific)**
(see [Data Description](https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1679989021) for shared SEISMIC variable names)

*Note:* 

- Specific syntax for these steps are likely to vary by institutional variable naming conventions
- Sample code shown here; see WG1-P4 GitHub repository institutional folders for complete data cleaning examples, and shared analysis folder for complete analysis code.

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
##  [1] "X"                      "EMPLID_H"               "ACADEMIC_PLAN_DESCR"   
##  [4] "ACADEMIC_PROGRAM_CD"    "ACADEMIC_PROGRAM_DESCR" "BIRTH_DT"              
##  [7] "GENDER_CD"              "ETHNIC_GROUP_CD"        "ETHNIC_AMIND_FLG"      
## [10] "ETHNIC_ASIAN_FLG"
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


```
## # A tibble: 6 x 24
##   st_id firstgen ethniccode ethniccode_cat   urm gender female famincome
##   <fct>    <dbl> <fct>               <dbl> <dbl>  <dbl>  <dbl>     <int>
## 1 075E…        0 WHITE                   0     0      0      0     64365
## 2 A743…        0 WHITE                   0     0      0      0        NA
## 3 B579…        0 WHITE                   0     0      0      0        NA
## 4 EA42…        1 ASIAN                   2     0      1      1     51651
## 5 FE52…        0 WHITE                   0     0      0      0        NA
## 6 7F05…        0 WHITE                   0     0      1      1    121485
## # … with 16 more variables: lowincomflag <dbl>, transfer <dbl>,
## #   international <dbl>, ell <dbl>, us_hs <dbl>, cohort <dbl>,
## #   cohort_2013 <dbl>, cohort_2014 <dbl>, cohort_2015 <dbl>, cohort_2016 <dbl>,
## #   cohort_2017 <dbl>, cohort_2018 <dbl>, apyear <dbl>, englsr <dbl>,
## #   mathsr <dbl>, hsgpa <dbl>
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


```
## # A tibble: 6 x 17
##   st_id crs_sbj crs_catalog crs_name numgrade numgrade_w crs_retake crs_term
##   <fct> <fct>   <fct>       <fct>       <dbl>      <dbl> <fct>         <int>
## 1 1343… CS      0441        DISCRET…     4             0 N              2171
## 2 1343… STAT    1000        APPLIED…     4             0 N              2174
## 3 1343… STAT    1361        STATSTC…     3.25          0 N              2184
## 4 1343… MATH    0240        ANALYTC…     4             0 N              2187
## 5 1343… HPS     1616        ARTFCL …     4             0 N              2191
## 6 1343… STAT    1631        INTERME…     3.25          0 N              2191
## # … with 9 more variables: crs_term_yr <chr>, crs_term_sem <chr>,
## #   summer_crs <dbl>, TERM_REF <int>, enrl_from_cohort <dbl>,
## #   crs_credits <dbl>, crs_component <fct>, class_number <int>,
## #   current_major <fct>
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


```
## # A tibble: 7,368 x 6
##    crs_credits crs_catalog numgrade_w crs_term_sem crs_retake_num crs_sbj
##          <dbl> <fct>            <dbl> <chr>                 <int> <fct>  
##  1           3 0150                 0 1                         1 BIOSC  
##  2           3 0150                 0 1                         1 BIOSC  
##  3           3 0150                 0 1                         1 BIOSC  
##  4           3 0150                 0 1                         1 BIOSC  
##  5           3 0150                 0 4                         1 BIOSC  
##  6           3 0150                 0 1                         1 BIOSC  
##  7           3 0150                 0 1                         1 BIOSC  
##  8           3 0150                 0 1                         1 BIOSC  
##  9           3 0150                 0 1                         1 BIOSC  
## 10           3 0150                 0 1                         1 BIOSC  
## # … with 7,358 more rows
```

## 3.	Clean AP Level variables

### a.  For each AP subject, rename and generate/recode course level variables as needed to match common SEISMIC AP variable names

*Note:*

- Taking highest (max) AP score each student recieved by subject




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
   

```
## # A tibble: 22,976 x 6
##    apskipper aptaker apscore apscore_full apskipper_2 tookcourse
##        <dbl>   <dbl> <chr>          <dbl>       <dbl>      <dbl>
##  1         0       0 <NA>               0           0          0
##  2         0       0 <NA>               0           0          0
##  3         0       0 <NA>               0           0          0
##  4         0       0 <NA>               0           0          0
##  5         0       0 <NA>               0           0          0
##  6         0       0 <NA>               0           0          0
##  7         1       1 5                  5           1          0
##  8         0       0 <NA>               0           0          0
##  9         0       0 <NA>               0           0          0
## 10         0       0 <NA>               0           0          0
## # … with 22,966 more rows
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


```
## # A tibble: 6 x 64
##   discipline st_id firstgen ethniccode ethniccode_cat   urm gender female
##   <chr>      <fct>    <dbl> <fct>               <dbl> <dbl>  <dbl>  <dbl>
## 1 BIO        0035…        0 ASIAN                   2     0      1      1
## 2 BIO        003E…        0 WHITE                   0     0      1      1
## 3 BIO        0043…        0 WHITE                   0     0      1      1
## 4 BIO        0047…        0 WHITE                   0     0      1      1
## 5 BIO        004D…        0 WHITE                   0     0      1      1
## 6 BIO        0061…        0 WHITE                   0     0      0      0
## # … with 56 more variables: famincome <int>, lowincomflag <dbl>,
## #   transfer <dbl>, international <dbl>, ell <dbl>, us_hs <dbl>, cohort <dbl>,
## #   cohort_2013 <dbl>, cohort_2014 <dbl>, cohort_2015 <dbl>, cohort_2016 <dbl>,
## #   cohort_2017 <dbl>, cohort_2018 <dbl>, apyear <dbl>, englsr <dbl>,
## #   mathsr <dbl>, hsgpa <dbl>, crs_sbj.x <fct>, crs_catalog.x <fct>,
## #   crs_name.x <fct>, numgrade.x <dbl>, numgrade_w.x <dbl>, crs_retake.x <fct>,
## #   crs_term.x <int>, crs_term_yr.x <chr>, crs_term_sem.x <chr>,
## #   summer_crs.x <dbl>, TERM_REF.x <int>, enrl_from_cohort.x <dbl>,
## #   crs_credits.x <dbl>, crs_component.x <fct>, class_number.x <int>,
## #   current_major.x <fct>, crs_sbj.y <fct>, crs_catalog.y <fct>,
## #   crs_name.y <fct>, numgrade.y <dbl>, numgrade_w.y <dbl>, crs_retake.y <fct>,
## #   crs_term.y <int>, crs_term_yr.y <chr>, crs_term_sem.y <chr>,
## #   summer_crs.y <dbl>, TERM_REF.y <int>, enrl_from_cohort.y <dbl>,
## #   crs_credits.y <dbl>, crs_component.y <fct>, class_number.y <int>,
## #   current_major.y <fct>, aptaker <dbl>, apscore <chr>, apscore_full <dbl>,
## #   apskipper <dbl>, tookcourse <dbl>, tookcourse_2 <dbl>, skipped_1 <dbl>
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


```
##  [1] "discipline"     "st_id"          "firstgen"       "ethniccode"    
##  [5] "ethniccode_cat" "urm"            "gender"         "female"        
##  [9] "famincome"      "lowincomflag"
```


# **Data Analysis (Same Across Institutions)**

*Note*: 
- Syntax for these steps should be able to be the same for all institutions (once data cleaning steps above are followed)
- Sample code shown here; see shared analysis folder in WG1-P4 GitHub repository for complete analysis code

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
##  [1] "X"              "discipline"     "st_id"          "firstgen"      
##  [5] "ethniccode"     "ethniccode_cat" "urm"            "gender"        
##  [9] "female"         "famincome"
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

### d.	Create subset dataframes for each analysis sample
        
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
(see [Sample Descriptions](https://docs.google.com/spreadsheets/d/1rN8W_iz1mr7lEzBGfdTZHa45wKOSLiSF8VEpChCPsmE/edit#gid=129222174) for full model specs.)

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
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
               binomial(link = "logit"), df_bio2)
logistic.display(m1.a_bio)

# Repeat for Chem, Phys
```


```
##  
##                              OR lower95ci upper95ci     Pr(>|Z|)
## factor(firstgen)1     1.0971642 0.8163172 1.4746342 5.387752e-01
## factor(lowincomflag)1 0.9473895 0.7262308 1.2358976 6.902949e-01
## factor(female)1       0.8201140 0.6921092 0.9717932 2.199673e-02
## factor(urm)1          1.0306634 0.7747606 1.3710906 8.356884e-01
## scale(hsgpa)          0.9756179 0.8966082 1.0615900 5.667314e-01
## scale(mathsr)         1.1669200 1.0562886 1.2891386 2.385424e-03
## scale(englsr)         1.1020438 0.9990431 1.2156639 5.227747e-02
## factor(cohort)2015    1.1474610 0.9050728 1.4547634 2.558981e-01
## factor(cohort)2016    1.2694275 1.0006982 1.6103219 4.933153e-02
## factor(cohort)2017    1.5352998 1.2029418 1.9594842 5.722756e-04
## factor(cohort)2018    2.1862614 1.6600413 2.8792891 2.581175e-08
```

## *RQ1b.* 

- Sample: Took AP dataframe
- DV: apscore
- IV: factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) + scale(hsgpa) +                   scale(mathsr) + scale(englsr)
- COV: factor(crs_term) 
            
## (Etc…)
