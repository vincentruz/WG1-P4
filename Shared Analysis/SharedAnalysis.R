# Before running this code, check https://github.com/seismic2020/WG1-P4/blob/master/Analysis-Workflow.md for variable naming and data cleaning steps.

#### Setup ####
# Load libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "psych", "data.table", "broom", "summarytools",   # Data wrangling and descriptive stats
               "epiDisplay", "sjstats",                                       # Reporting Odds Ratio and Std. Betas
                "sjPlot")                                                     # Generating regression tables

# Load clean dataset
df_clean <- read.csv("~/YOUR RECODED FILE PATH HERE.csv")

# View data 
# Note: Variable names should follow SEISMIC conventions found here: https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1679989021
names(df_clean)
head(df_clean)

## Filter for student level inclusion/exclusion criteria ####
df_clean <- df_clean %>%
  # Include
  filter(transfer == 0) %>%
  filter(tookcourse_2 == 1) %>%
  filter(cohort >= 2013 & cohort <= 2018) %>%
  # Exclude
  filter(international == 0) %>%
  # Set URM as reference group
  mutate(ethniccode_cat = relevel(as.factor(ethniccode_cat), ref= "1"))


## Create subset dataframes for each analysis sample (for each discipline) ####
# Bio
# Took 2nd course in sequence
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013)
# Took AP 
df_bio_aptakers <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013) %>%
  subset(aptaker == 1)
# Skip eligible
df_bio_skeligible <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013) %>%
  subset(eligible_to_skip == 1)
# Skip eligible (at each score)
df_bio_skeligible.3 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013) %>%
  subset(apscore == 3)
df_bio_skeligible.4 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013) %>%
  subset(apscore == 4)
df_bio_skeligible.5 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013) %>%
  subset(apscore == 5)

# Chem
# Took 2nd course in sequence
df_chem2 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014)
# Took AP 
df_chem_aptakers <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014) %>%
  subset(aptaker == 1)
# Skip eligible
df_chem_skeligible <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014) %>%
  subset(eligible_to_skip == 1)
# Skip eligible (at each score)
df_chem_skeligible.3 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014) %>%
  subset(apscore == 3)
df_chem_skeligible.4 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014) %>%
  subset(apscore == 4)
df_chem_skeligible.5 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014) %>%
  subset(apscore == 5)


# Phys
# Took 2nd course in sequence
df_phys2 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015)
# Took AP 
df_phys_aptakers <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015) %>%
  subset(aptaker == 1)
# Skip eligible
df_phys_skeligible <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015) %>%
  subset(eligible_to_skip == 1)
# Skip eligible (at each score)
df_phys_skeligible.3 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015) %>%
  subset(apscore == 3)
df_phys_skeligible.4 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015) %>%
  subset(apscore == 4)
df_phys_skeligible.5 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015) %>%
  subset(apscore == 5)

#### Descriptive Stats ####
# Vector of Continuous Vars
contVar <- c("numgrade", "numgrade_2", "mathsr", "englsr", "hsgpa", "apscore")

# Full Dataset
view(dfSummary(df_clean))
lapply(df_chem2[contVar], function(x) summary(x))

# By Discipline
# Bio
view(dfSummary(df_bio2))
lapply(df_bio2[contVar], function(x) summary(x))
# Chem
view(dfSummary(df_chem2))
lapply(df_chem2[contVar], function(x) summary(x))
# Phys
view(dfSummary(df_phsy2))
lapply(df_phys2[contVar], function(x) summary(x))
         
#### Run Models (for each discipline) ####
# Note: For model specifications, check: https://docs.google.com/spreadsheets/d/1rN8W_iz1mr7lEzBGfdTZHa45wKOSLiSF8VEpChCPsmE/edit#gid=129222174

# BIO
# RQ1a - Who takes AP?
bio_rq1a <- glm(aptaker ~ 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                data=df_bio2, binomial(link="logit"))
summary(bio_rq1a)
robustse(bio_rq1a, coef="odd.ratio") #robustse is a command from sjstats package
logistic.display(bio_rq1a)

# RQ1b - Who gets what score on AP exams? (DV: apscore, Sample: Took AP)
bio_rq1b <- lm(scale(apscore) ~ 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
               data=df_bio_aptakers)
summary(bio_rq1b)
std_beta(bio_rq1b) # from sjstats package

# RQ1c.i - Of everyone, who earns an AP score that makes them eligible to skip? 
bio_rq1ci <- glm(eligible_to_skip ~ factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                 data=df_bio2, binomial(link="logit"))
summary(bio_rq1ci)
logistic.display(bio_rq1ci)
#robustse(bio_rq1ci, coef="odd.ratio")

# RQ1c.ii - Of those who take AP, who earns an AP score that makes them eligible to skip?
bio_rq1cii <- glm(eligible_to_skip ~ 
                    factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                    scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                  data=df_bio_aptakers, binomial(link="logit"))
summary(bio_rq1cii)
logistic.display(bio_rq1cii)
#robustse(bio_rq1cii, coef="odd.ratio")

# RQ2a - Of everyone who was eligible to skip, who actually skipped?
bio_rq2a <- glm(skipped_course ~ 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                data=df_bio_skeligible, binomial(link="logit"))
summary(bio_rq2a)
logistic.display(bio_rq2a)
#robustse(bio_rq2a, coef="odd.ratio")

# RQ2b - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
bio_rq2b <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_skeligible)
summary(bio_rq2b)
#std_beta(bio_rq2b)

# RQ2b - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
bio_rq2b.3 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_skeligible.3)
summary(bio_rq2b.3)
#std_beta(bio_rq2b.3)

# RQ2b.4 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
bio_rq2b.4 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_skeligible.4)
summary(bio_rq2b.4)
#std_beta(bio_rq2b.4)

# RQ2b.5 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
bio_rq2b.5 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_skeligible.5)
summary(bio_rq2b.5)
#std_beta(bio_rq2b.5)


# RQ2c - Of everyone, what was the effect of skipping on 2nd course grade (controlling for AP score)? 
bio_rq2c <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio2)
summary(bio_rq2c)
#std_beta(bio_rq2c)

# RQ2d - Of everyone who took AP, what was the effect of skipping on 2nd course grade?
bio_rq2d <- lm(scale(numgrade_2) ~ factor(skipped_course) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_aptakers)
summary(bio_rq2d)
#std_beta(bio_rq2d)

# RQ2e - Of everyone who took AP and skipped, what was the effect of skipping on 2nd course grade?
bio_rq2e <- lm(scale(numgrade_2) ~ factor(skipped_course) +
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio2)
summary(bio_rq2d)
#std_beta(bio_rq2d)

# RQ2f - Of everyone who took AP, what was the effect of being eligible to skip on 2nd course grade?
bio_rq2f <- lm(scale(numgrade_2) ~ factor(eligible_to_skip) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_aptakers)
summary(bio_rq2f)

#std_beta(bio_rq2f)

# RQ2g -  Of everyone, what was the effect of being eligible to skip on 2nd course grade?
bio_rq2g<- lm(scale(numgrade_2) ~ factor(eligible_to_skip) + 
                factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
              data=df_bio2)
summary(bio_rq2g)
#std_beta(bio_rq2g)

# CHEM
# RQ1a - Who takes AP?
chem_rq1a <- glm(aptaker ~ factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                 data=df_chem2, binomial(link="logit"))
summary(chem_rq1a)
robustse(chem_rq1a, coef="odd.ratio") #robustse is a command from sjstats package
logistic.display(chem_rq1a)

# RQ1b - Who gets what score on AP exams? (DV: apscore, Sample: Took AP)
chem_rq1b <- lm(scale(apscore) ~ 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                data=df_chem_aptakers)
summary(chem_rq1b)
std_beta(chem_rq1b) # from sjstats package

# RQ1c.i - Of everyone, who earns an AP score that makes them eligible to skip? 
chem_rq1ci <- glm(eligible_to_skip ~ factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                    scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                  data=df_chem2, binomial(link="logit"))
summary(chem_rq1ci)
logistic.display(chem_rq1ci)
#robustse(chem_rq1ci, coef="odd.ratio")

# RQ1c.ii - Of those who take AP, who earns an AP score that makes them eligible to skip?
chem_rq1cii <- glm(eligible_to_skip ~ 
                     factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                   data=df_chem_aptakers, binomial(link="logit"))
summary(chem_rq1cii)
logistic.display(chem_rq1cii)
#robustse(rq1ci, coef="odd.ratio")

# RQ2a - Of everyone who was eligible to skip, who actually skipped?
chem_rq2a <- glm(skipped_course ~ 
                   factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                 data=df_chem_skeligible, binomial(link="logit"))
summary(chem_rq2a)
logistic.display(chem_rq2a)
#robustse(chem_rq2a, coef="odd.ratio")

# RQ2b - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
chem_rq2b <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_chem_skeligible)
summary(chem_rq2b)
#std_beta(chem_rq2b)

# RQ2b.3 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
chem_rq2b.3 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_chem_skeligible.3)
summary(chem_rq2b.3)
#std_beta(chem_rq2b.3)

# RQ2b.4 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
chem_rq2b.4 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_chem_skeligible.4)
summary(chem_rq2b.4)
#std_beta(chem_rq2b.4)

# RQ2b.5 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
chem_rq2b.5 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_chem_skeligible.5)
summary(chem_rq2b.5)
#std_beta(chem_rq2b.5)

# RQ2c - Of everyone, what was the effect of skipping on 2nd course grade (controlling for AP score)? 
chem_rq2c <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_chem2)
summary(chem_rq2c)
#std_beta(chem_rq2c)

# RQ2d - Of everyone who took AP, what was the effect of skipping on 2nd course grade?
chem_rq2d <- lm(scale(numgrade_2) ~ factor(skipped_course) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_chem_aptakers)
summary(chem_rq2d)
#std_beta(chem_rq2d)

# RQ2e - Of everyone who took AP and skipped, what was the effect of skipping on 2nd course grade?
chem_rq2e <- lm(scale(numgrade_2) ~ factor(skipped_course) +
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_chem2)
summary(chem_rq2d)
#std_beta(chem_rq2d)

# RQ2f - Of everyone who took AP, what was the effect of being eligible to skip on 2nd course grade?
chem_rq2f <- lm(scale(numgrade_2) ~ factor(eligible_to_skip) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_chem_aptakers)
summary(chem_rq2f)

#std_beta(chem_rq2f)

# RQ2g -  Of everyone, what was the effect of being eligible to skip on 2nd course grade?
chem_rq2g<- lm(scale(numgrade_2) ~ factor(eligible_to_skip) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_chem2)
summary(chem_rq2g)
#std_beta(chem_rq2g)

# PHYS
# RQ1a - Who takes AP?
phys_rq1a <- glm(aptaker ~ factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                 data=df_phys2, binomial(link="logit"))
summary(phys_rq1a)
robustse(phys_rq1a, coef="odd.ratio") #robustse is a command from sjstats package
logistic.display(phys_rq1a)

# RQ1b - Who gets what score on AP exams? (DV: apscore, Sample: Took AP)
phys_rq1b <- lm(scale(apscore) ~ 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                data=df_phys_aptakers)
summary(phys_rq1b)
std_beta(phys_rq1b) # from sjstats package

# RQ1c.i - Of everyone, who earns an AP score that makes them eligible to skip? 
phys_rq1ci <- glm(eligible_to_skip ~ factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                    scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                  data=df_phys2, binomial(link="logit"))
summary(phys_rq1ci)
logistic.display(phys_rq1ci)
#robustse(phys_rq1ci, coef="odd.ratio")

# RQ1c.ii - Of those who take AP, who earns an AP score that makes them eligible to skip?
phys_rq1cii <- glm(eligible_to_skip ~ 
                     factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                   data=df_phys_aptakers, binomial(link="logit"))
summary(phys_rq1cii)
logistic.display(phys_rq1cii)
#robustse(rq1ci, coef="odd.ratio")

# RQ2a - Of everyone who was eligible to skip, who actually skipped?
phys_rq2a <- glm(skipped_course ~ 
                   factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                 data=df_phys_skeligible, binomial(link="logit"))
summary(phys_rq2a)
logistic.display(phys_rq2a)
#robustse(phys_rq2a, coef="odd.ratio")

# RQ2b - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
phys_rq2b <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_phys_skeligible)
summary(phys_rq2b)
#std_beta(phys_rq2b)

# RQ2b.3 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
phys_rq2b.3 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_phys_skeligible.3)
summary(phys_rq2b.3)
#std_beta(phys_rq2b.3)

# RQ2b.4 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
phys_rq2b.4 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_phys_skeligible.4)
summary(phys_rq2b.4)
#std_beta(phys_rq2b.4)

# RQ2b.5 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
phys_rq2b.5 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_phys_skeligible.5)
summary(phys_rq2b.5)
#std_beta(phys_rq2b.5)

# RQ2b.3 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
phys_rq2b <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_bio_skeligible)
summary(phys_rq2b)
#std_beta(phys_rq2b)

# RQ2b.4 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
phys_rq2b <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_bio_skeligible)
summary(phys_rq2b)
#std_beta(phys_rq2b)

# RQ2b.5 - Of those who were eligible to skip, what was the effect of skipping on 2nd course grade (controlling for AP score)?
phys_rq2b <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_bio_skeligible.5)
summary(phys_rq2b)
#std_beta(phys_rq2b)

# RQ2c - Of everyone, what was the effect of skipping on 2nd course grade (controlling for AP score)? 
phys_rq2c <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_phys2)
summary(phys_rq2c)
#std_beta(phys_rq2c)

# RQ2d - Of everyone who took AP, what was the effect of skipping on 2nd course grade?
phys_rq2d <- lm(scale(numgrade_2) ~ factor(skipped_course) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_phys_aptakers)
summary(phys_rq2d)
#std_beta(phys_rq2d)

# RQ2e - Of everyone who took AP and skipped, what was the effect of skipping on 2nd course grade?
phys_rq2e <- lm(scale(numgrade_2) ~ factor(skipped_course) +
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_phys2)
summary(phys_rq2d)
#std_beta(phys_rq2d)

# RQ2f - Of everyone who took AP, what was the effect of being eligible to skip on 2nd course grade?
phys_rq2f <- lm(scale(numgrade_2) ~ factor(eligible_to_skip) + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                data=df_phys_aptakers)
summary(phys_rq2f)

#std_beta(phys_rq2f)

# RQ2g -  Of everyone, what was the effect of being eligible to skip on 2nd course grade?
phys_rq2g<- lm(scale(numgrade_2) ~ factor(eligible_to_skip) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_phys2)
summary(phys_rq2g)
#std_beta(phys_rq2g)


#### Create Regression Output Tables ####
# By Discipline and RQ
BIO_RQ1models <- tab_model(bio_rq1a, bio_rq1b, bio_rq1ci, bio_rq1cii, bio_rq2a,
                           title = "BIO RQ1 Models", file = "BIO RQ1 Models.htm", 
                           digits = 3, show.intercept = FALSE)
print(BIO_RQ1models)
BIO_RQ2models <- tab_model(bio_rq2b, bio_rq2c, bio_rq2d, bio_rq2e, bio_rq2f, bio_rq2g, 
                           title = "BIO RQ2 Models", file = "BIO RQ2 Models.htm", 
                           digits = 3, show.intercept = FALSE)
print(BIO_RQ2models)

CHEM_RQ1models <- tab_model(chem_rq1a, chem_rq1b, chem_rq1ci, chem_rq1cii, chem_rq2a, 
                            title = "CHEM RQ1 Models", file = "CHEM RQ1 Models.htm", 
                            digits = 3, show.intercept = FALSE)
print(CHEM_RQ1models)
CHEM_RQ2models <- tab_model(chem_rq2b, chem_rq2c, chem_rq2d, chem_rq2e, chem_rq2f, chem_rq2g,
                            title = "CHEM RQ2 Models", file = "CHEM RQ2 Models.htm",
                            digits = 3, show.intercept = FALSE)
print(CHEM_RQ2models)

PHYS_RQ1models <- tab_model(phys_rq1a, phys_rq1b, phys_rq1ci, phys_rq1cii, phys_rq2a,
                            title = "PHYS RQ1 Models", file = "PHYS RQ1 Models.htm",
                            digits = 3, show.intercept = FALSE)
print(PHYS_RQ1models)
PHYS_RQ2models <- tab_model(phys_rq2b, phys_rq2c, phys_rq2d, phys_rq2e, phys_rq2f, phys_rq2g,
                            title = "PHYS RQ2 Models", file = "PHYS RQ2 Models.htm",
                            digits = 3, show.intercept = FALSE)
print(PHYS_RQ2models)


