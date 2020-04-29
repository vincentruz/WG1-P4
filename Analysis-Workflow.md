---
title: "SEISMIC WG1-P4"
date: "Updated: April 28, 2020"
output:
  html_document:
    keep_md: true
    code_folding: hide
    theme: cosmo
    toc: yes
    toc_depth: 2
    toc_float: yes
subtitle: Analysis Workflow
---



#### - R/RStudio used for analyses
#### - Jupyter Notebooks/Google Co-Lab used for sharing code

# **I. Data Processing (Institution Specific)**

#### *Note:* 

- Exact syntax for these steps is likely to vary based on institution-specific variable naming conventions
- See [Data Description](https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1679989021) for shared SEISMIC variable names
- Sample code only shown here; see [WG1-P4 GitHub repository](https://github.com/seismic2020/WG1-P4) for complete examples of institution-specific data cleaning code

## 0. Startup

### a.  Load R pkgs

```r
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse")   # Data wrangling

# etc...
```

### b.	Load full dataset



```r
# CHANGE TO YOUR FILE PATH
df_full <- read.csv("~/YOUR FILE PATH HERE.csv")
head(names(df_full))
```


```
## [1] "Example RAW Varnames"
```

```
##  [1] "X"                        "EMPLID_H"                
##  [3] "ACADEMIC_PLAN_DESCR"      "ACADEMIC_PROGRAM_CD"     
##  [5] "ACADEMIC_PROGRAM_DESCR"   "BIRTH_DT"                
##  [7] "GENDER_CD"                "ETHNIC_GROUP_CD"         
##  [9] "ETHNIC_AMIND_FLG"         "ETHNIC_ASIAN_FLG"        
## [11] "ETHNIC_BLACK_FLG"         "ETHNIC_HISPA_FLG"        
## [13] "ETHNIC_PACIF_FLG"         "ETHNIC_WHITE_FLG"        
## [15] "CITIZENSHIP_STATUS_DESCR"
```

## 1.	Clean Student level variables

### a. Rename and generate/recode student level variables as needed to match common SEISMIC AP variable names

#### *Note:* 

- Student level data should contain unique rows per student




```r
## Student Level ####
df_std <- df_full %>%
  # Renamed variables
  mutate(st_id = EMPLID_H) %>%
  mutate(ethniccode = ETHNIC_GROUP_CD) %>%
  mutate(famincome = abs(AGI)) %>%
  # Recoded variables
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
## [1] "Example Student Level Dataframe"
```

```
## # A tibble: 10 x 24
##    st_id firstgen ethniccode ethniccode_cat   urm gender female famincome
##    <fct>    <dbl> <fct>               <dbl> <dbl>  <dbl>  <dbl>     <int>
##  1 2831…        0 WHITE                   0     0      1      1    228738
##  2 F369…        0 WHITE                   0     0      1      1        NA
##  3 7DEA…        0 WHITE                   0     0      0      0     24000
##  4 D896…        0 WHITE                   0     0      1      1    191722
##  5 A207…        1 WHITE                   0     0      0      0     20011
##  6 8E65…        0 WHITE                   0     0      1      1        NA
##  7 2FA8…        0 WHITE                   0     0      1      1        NA
##  8 46CB…        0 WHITE                   0     0      0      0         0
##  9 1509…        0 WHITE                   0     0      0      0        NA
## 10 5700…        0 WHITE                   0     0      0      0    167294
## # … with 16 more variables: lowincomflag <dbl>, transfer <dbl>,
## #   international <dbl>, ell <dbl>, us_hs <dbl>, cohort <dbl>,
## #   cohort_2013 <dbl>, cohort_2014 <dbl>, cohort_2015 <dbl>, cohort_2016 <dbl>,
## #   cohort_2017 <dbl>, cohort_2018 <dbl>, apyear <dbl>, englsr <dbl>,
## #   mathsr <dbl>, hsgpa <dbl>
```

## 2.	Clean Course level variables

### a.	Rename and generate/recode course level variables as needed to match common SEISMIC AP variable names

#### *Note:* 

- Course level data will likely contain multiple rows for each course, per student




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
## [1] "Example Course Level Dataframe"
```

```
## # A tibble: 670,191 x 17
##    st_id crs_sbj crs_catalog crs_name numgrade numgrade_w crs_retake crs_term
##    <fct> <fct>   <fct>       <fct>       <dbl>      <dbl> <fct>         <int>
##  1 1343… CS      0441        DISCRET…     4             0 N              2171
##  2 1343… STAT    1000        APPLIED…     4             0 N              2174
##  3 1343… STAT    1361        STATSTC…     3.25          0 N              2184
##  4 1343… MATH    0240        ANALYTC…     4             0 N              2187
##  5 1343… HPS     1616        ARTFCL …     4             0 N              2191
##  6 1343… STAT    1631        INTERME…     3.25          0 N              2191
##  7 AE6D… CHEM    0310        ORGANIC…     0             0 N              2084
##  8 AE6D… MATH    0230        ANALYTC…     0             0 N              2114
##  9 AE6D… PHYS    0175        BASC PH…     0             0 N              2114
## 10 AE6D… MATH    0230        ANALYTC…     0             0 N              2117
## # … with 670,181 more rows, and 9 more variables: crs_term_yr <chr>,
## #   crs_term_sem <chr>, summer_crs <dbl>, TERM_REF <int>,
## #   enrl_from_cohort <dbl>, crs_credits <dbl>, crs_component <fct>,
## #   class_number <int>, current_major <fct>
```

### b.	For each subject course (1 and 2), create dataframe of only first time taking that course

#### *Note:* 

- This step selects down to only a single row per course, per student




```r
# By Course (Taking only First Attempt) ####
# Bio
df_crs_bio1 <- df_crs %>%
  filter(crs_sbj == "BIOSC" & (crs_catalog == "0150")) %>% # | crs_catalog == "0715")) %>%
  #Only first time taking course
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group= TRUE) %>%
  mutate(crs_retake_num = row_number()) %>%
  filter(crs_retake_num == 1) 

# etc...
# repeat for Bio2, Chem1, Chem2, Phys1, Phys2
```


```
## [1] "Example Course Level Dataframe - First Course Only"
```

```
## # A tibble: 7,368 x 18
##    st_id crs_sbj crs_catalog crs_name numgrade numgrade_w crs_retake crs_term
##    <fct> <fct>   <fct>       <fct>       <dbl>      <dbl> <fct>         <int>
##  1 0036… BIOSC   0150        FOUNDAT…     0             0 R              2131
##  2 003E… BIOSC   0150        FOUNDAT…     3             0 N              2101
##  3 0042… BIOSC   0150        FOUNDAT…     2             0 N              2121
##  4 0042… BIOSC   0150        FOUNDAT…     3.25          0 N              2071
##  5 0043… BIOSC   0150        FOUNDAT…     3.25          0 N              2164
##  6 0043… BIOSC   0150        FOUNDAT…     1             0 R              2121
##  7 0047… BIOSC   0150        FOUNDAT…     3             0 N              2101
##  8 004D… BIOSC   0150        FOUNDAT…     3             0 N              2191
##  9 0061… BIOSC   0150        FOUNDAT…     3.75          0 N              2161
## 10 0067… BIOSC   0150        FOUNDAT…     2             0 N              2121
## # … with 7,358 more rows, and 10 more variables: crs_term_yr <chr>,
## #   crs_term_sem <chr>, summer_crs <dbl>, TERM_REF <int>,
## #   enrl_from_cohort <dbl>, crs_credits <dbl>, crs_component <fct>,
## #   class_number <int>, current_major <fct>, crs_retake_num <int>
```

## 3.	Clean AP Level variables

### a.  For each AP subject, rename and generate/recode course level variables as needed to match common SEISMIC AP variable names

#### *Note:*

- Taking highest (max) AP score recieved; single row per AP exam, per student




```r
##### AP Level ####
# By course ####
# Bio
df_ap_bio <- df_full %>%
  mutate(st_id = EMPLID_H) %>%
  mutate(aptaker = ifelse(is.na(BY), 0, 1)) %>%
  mutate(eligible_to_skip = ifelse(BY >= 4 & !is.na(BY), 1, 0)) %>%
  mutate(eligible_to_skip_2 = ifelse(BY == 5 & !is.na(BY), 1, 0)) %>%
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

# etc...
# repeat for Chem, Phys
```
   

```
## [1] "Example AP Level Dataframe - Highest Score by Exam"
```

```
## # A tibble: 22,976 x 8
##    st_id aptaker eligible_to_skip eligible_to_ski… tookcourse tookcourse_2
##    <fct>   <dbl>            <dbl>            <dbl>      <dbl>        <dbl>
##  1 0000…       0                0                0          0            0
##  2 0005…       0                0                0          0            0
##  3 0009…       0                0                0          0            0
##  4 000E…       0                0                0          0            0
##  5 000E…       0                0                0          0            0
##  6 0019…       0                0                0          0            0
##  7 001D…       1                1                1          0            0
##  8 001F…       0                0                0          0            0
##  9 001F…       0                0                0          0            0
## 10 0029…       0                0                0          0            0
## # … with 22,966 more rows, and 2 more variables: apscore <chr>,
## #   apscore_full <dbl>
```

## 4. Create stacked dataset

### a.  Join previously generated dataframes (Student, Course1, Course2, and AP) for each course subject (BIO, CHEM, PHYS) 

#### *Note:*

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
  mutate(skipped_course = ifelse(tookcourse == 0 & tookcourse_2 == 1, 1, 0)) %>%
  select(discipline, st_id:hsgpa, crs_sbj.x:current_major.x, crs_sbj.y:current_major.y, 
         aptaker, apscore, apscore_full, eligible_to_skip, 
         tookcourse, tookcourse_2, skipped_course) 

# etc...
# repeat for Chem, Phys
```


```
## [1] "Example Stacked BIO Dataframe"
```

```
## # A tibble: 22,976 x 64
##    discipline st_id firstgen ethniccode ethniccode_cat   urm gender female
##    <chr>      <fct>    <dbl> <fct>               <dbl> <dbl>  <dbl>  <dbl>
##  1 BIO        0035…        0 ASIAN                   2     0      1      1
##  2 BIO        003E…        0 WHITE                   0     0      1      1
##  3 BIO        0043…        0 WHITE                   0     0      1      1
##  4 BIO        0047…        0 WHITE                   0     0      1      1
##  5 BIO        004D…        0 WHITE                   0     0      1      1
##  6 BIO        0061…        0 WHITE                   0     0      0      0
##  7 BIO        006A…        1 HISPA                   1     1      0      0
##  8 BIO        0076…        0 WHITE                   0     0      0      0
##  9 BIO        0080…        0 WHITE                   0     0      0      0
## 10 BIO        0085…        1 ASIAN                   2     0      0      0
## # … with 22,966 more rows, and 56 more variables: famincome <int>,
## #   lowincomflag <dbl>, transfer <dbl>, international <dbl>, ell <dbl>,
## #   us_hs <dbl>, cohort <dbl>, cohort_2013 <dbl>, cohort_2014 <dbl>,
## #   cohort_2015 <dbl>, cohort_2016 <dbl>, cohort_2017 <dbl>, cohort_2018 <dbl>,
## #   apyear <dbl>, englsr <dbl>, mathsr <dbl>, hsgpa <dbl>, crs_sbj.x <fct>,
## #   crs_catalog.x <fct>, crs_name.x <fct>, numgrade.x <dbl>,
## #   numgrade_w.x <dbl>, crs_retake.x <fct>, crs_term.x <int>,
## #   crs_term_yr.x <chr>, crs_term_sem.x <chr>, summer_crs.x <dbl>,
## #   TERM_REF.x <int>, enrl_from_cohort.x <dbl>, crs_credits.x <dbl>,
## #   crs_component.x <fct>, class_number.x <int>, current_major.x <fct>,
## #   crs_sbj.y <fct>, crs_catalog.y <fct>, crs_name.y <fct>, numgrade.y <dbl>,
## #   numgrade_w.y <dbl>, crs_retake.y <fct>, crs_term.y <int>,
## #   crs_term_yr.y <chr>, crs_term_sem.y <chr>, summer_crs.y <dbl>,
## #   TERM_REF.y <int>, enrl_from_cohort.y <dbl>, crs_credits.y <dbl>,
## #   crs_component.y <fct>, class_number.y <int>, current_major.y <fct>,
## #   aptaker <dbl>, apscore <chr>, apscore_full <dbl>, eligible_to_skip <dbl>,
## #   tookcourse <dbl>, tookcourse_2 <dbl>, skipped_course <dbl>
```

### b. Stack complete dataframes for each course subject (BIO, CHEM, PHYS), including "discipline" indicator variable



```r
# Stacked dataframe with Bio, Chem, Phys
df_clean <- rbind(df_bio, df_chem, df_phys)

# etc...
```

#### *Note:*

- Should end up with dataset structured like this [Example Dataset](https://docs.google.com/spreadsheets/d/1Sj5kaFNGUkBhRoOH3cIPm-97UEBZmcFkbKGjzBbKWc0/edit?usp=drive_open&ouid=118183464940790632947)


```
## [1] "Example RE-CODED Varnames"
```

```
##  [1] "discipline"     "st_id"          "firstgen"       "ethniccode"    
##  [5] "ethniccode_cat" "urm"            "gender"         "female"        
##  [9] "famincome"      "lowincomflag"   "transfer"       "international" 
## [13] "ell"            "us_hs"          "cohort"
```

# **II. Data Analysis (Same Across Institutions)**

#### *Note:* 

- Syntax for these steps should be the same for all institutions (once data cleaning steps above are followed)
- Sample code shown here; use [Shared Analysis file](https://github.com/seismic2020/WG1-P4/tree/master/Shared%20Analysis) on WG1-P4 GitHub repository for complete analysis code

## 0. Startup

### a.  Load R pkgs

```r
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse",   # Data wrangling
               "epiDisplay")  # Display OR for logistic regressions

# etc...
```

### b.	Load full dataset



```r
# CHANGE TO YOUR FILE PATH
df_clean <- read.csv("~/YOUR FILE PATH HERE.csv")
head(names(df_clean))
```


```
## [1] "Example RE-CODED Varnames"
```

```
##  [1] "X"              "discipline"     "st_id"          "firstgen"      
##  [5] "ethniccode"     "ethniccode_cat" "urm"            "gender"        
##  [9] "female"         "famincome"      "lowincomflag"   "transfer"      
## [13] "transfer_cred"  "international"  "ell"
```

## 1. Sample selection (by RQ, for each Course)

### a. Filter for student level inclusion/exclusion criteria

#### *Note:*

- Use [Shared Analysis Syntax](https://github.com/seismic2020/WG1-P4/tree/master/Shared%20Analysis) or see [Inclusion/Exclusion Criteria](https://docs.google.com/spreadsheets/d/1rN8W_iz1mr7lEzBGfdTZHa45wKOSLiSF8VEpChCPsmE/edit#gid=727713658) for shared sample selection procedure


```r
df_clean <- df_clean %>%
  # Include
  filter(transfer == 0) %>%
  filter(tookcourse_2 == 1) %>%
  filter(cohort >= 2013 & cohort <= 2018) %>%
  # Exclude
  filter(international == 0)

# etc...
```

### b.	Create subset dataframes for each analysis sample

#### *Note:* 

- Use [Shared Analysis Syntax](https://github.com/seismic2020/WG1-P4/tree/master/Shared%20Analysis) or see [Sample Descriptions](https://docs.google.com/spreadsheets/d/1rN8W_iz1mr7lEzBGfdTZHa45wKOSLiSF8VEpChCPsmE/edit#gid=129222174) for full model specifications




```r
# Bio
# Took 2nd course in sequence
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013)

# etc...
# repeat for Chem, Phys
```


```r
# Took AP 
df_BYtakers <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(aptaker == 1)

# etc...
# repeat for Chem, Phys
```


```r
# Skip eligible
df_BYeligible <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(eligible_to_skip == 1)

# etc...
# repeat for Chem, Phys
```


```r
# Skip eligible, at each eligble score (NOTE: THESE WILL DIFFER BY INSTITUTION!)
df_BYeligible.4 <- df_bio2 %>%
  subset(apscore == 4)
df_BYeligible.5 <- df_bio2 %>%
  subset(apscore == 5)

# etc...
# Repeat for Chem, Phys
```

## 2.	Run Models (for each Course)

#### *Note:*

- Use [Shared Analysis Syntax](https://github.com/seismic2020/WG1-P4/tree/master/Shared%20Analysis) or see [Sample Descriptions](https://docs.google.com/spreadsheets/d/1rN8W_iz1mr7lEzBGfdTZHa45wKOSLiSF8VEpChCPsmE/edit#gid=129222174) for full model specifications

## RQ1: What student characteristics are associated with student participation and success in AP courses for students enrolled at the selected universities?

## *RQ1a: Who takes AP exams?*




```r
# Model 1a: Credits ####
#Bio
m1.a_bio <- glm(aptaker ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
               binomial(link = "logit"), df_bio2)
logistic.display(m1.a_bio)

# etc...
# Repeat for Chem, Phys
```


```
## [1] "Example: Model Parameters for RQ1a - Bio"
```

```
##  
##                              OR lower95ci upper95ci     Pr(>|Z|)
## factor(firstgen)1     1.1327672 0.8458207 1.5170610 4.029029e-01
## factor(lowincomflag)1 0.9091437 0.7067043 1.1695730 4.585982e-01
## factor(female)1       0.8449690 0.7144546 0.9993253 4.908547e-02
## factor(urm)1          0.9912676 0.7515754 1.3074024 9.504824e-01
## scale(hsgpa)          0.9925281 0.9133099 1.0786175 8.597277e-01
## scale(mathsr)         1.1684125 1.0582577 1.2900335 2.064939e-03
## scale(englsr)         1.1098205 1.0067102 1.2234916 3.622537e-02
## factor(cohort)2015    1.1675600 0.9138517 1.4917041 2.152381e-01
## factor(cohort)2016    1.1405278 0.8931304 1.4564545 2.918823e-01
## factor(cohort)2017    1.4508224 1.1342671 1.8557230 3.045026e-03
## factor(cohort)2018    1.6722230 1.3023230 2.1471862 5.558193e-05
```

## *RQ1b: Who gets what score on AP exams?* 




```r
#Bio
m1.b_bio <- lm(scale(apscore) ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term),
              df_BYtakers)
summary(m1.b_bio)

# etc...
```


```
## [1] "Example: Model Parameters for RQ1b - Bio"
```

```
##  
##                              OR lower95ci upper95ci     Pr(>|Z|)
## factor(firstgen)1     0.9919314 0.8125886 1.2108563 9.365547e-01
## factor(lowincomflag)1 0.8255599 0.6915657 0.9855161 3.410086e-02
## factor(female)1       0.7488713 0.6685817 0.8388030 6.704160e-07
## factor(urm)1          0.7239805 0.6032798 0.8688303 5.379178e-04
## scale(hsgpa)          1.1357923 1.0693442 1.2063693 3.732853e-05
## scale(mathsr)         1.1824570 1.1049289 1.2654250 1.448605e-06
## scale(englsr)         1.2551903 1.1741453 1.3418294 3.871251e-11
## factor(crs_term)2015  1.0093590 0.8458980 1.2044072 9.177080e-01
## factor(crs_term)2016  0.8605007 0.7240162 1.0227141 8.845368e-02
## factor(crs_term)2017  0.9218042 0.7782489 1.0918395 3.460371e-01
## factor(crs_term)2018  1.0497281 0.8878170 1.2411670 5.702780e-01
## factor(crs_term)2019  0.7206907 0.3634912 1.4289069 3.484743e-01
```

## *(Etc…)*
