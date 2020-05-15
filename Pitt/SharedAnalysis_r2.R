# Before running this code, check https://github.com/seismic2020/WG1-P4/blob/master/Analysis-Workflow.md for variable naming and data cleaning steps.

#### Setup ####
# Load libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "psych", "data.table", "broom", "summarytools",   # Data wrangling and descriptive stats
               "BaylorEdPsych", "mvnmle",                                     # Littles MCAR test
               "mice",                                                        # Multiple imputation
               "epiDisplay", "sjstats",                                       # Reporting Odds Ratio and Std. Betas
               "WeightIt", "cobalt", "survey",                                # Propensity score matching
               "sjPlot")                                                      # Generating regression tables

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
  filter(international == 0) 

## Create subset dataframes for each discipline ####
modelVars <- c("numgrade_2", "apscore", "apscore_full", "eligible_to_skip", "skipped_course", 
               "firstgen", "lowincomeflag", "female", "urm", "ethniccode_cat", 
               "hsgpa", "mathsr", "englsr", "enrl_from_cohort_2", "cohort", "crs_term_2")
coVars <- c("firstgen", "lowincomeflag", "female", "urm", "ethniccode_cat", 
                         "hsgpa", "mathsr", "englsr", "enrl_from_cohort_2", "cohort", "crs_term_2")

# Bio (complete info, after 2013)
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013) %>%
  select(modelVars) %>%
  filter_at(vars(coVars), all_vars(complete.cases(.)))

# Chem (complete info, after 2014)
df_chem2 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014) %>%
  select(modelVars) %>%
  filter_at(vars(coVars), all_vars(complete.cases(.)))

# Phys (complete info, after 2015)
# Took 2nd course in sequence
df_phys2 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015) %>%
  select(modelVars) %>%
  filter_at(vars(coVars), all_vars(complete.cases(.)))

#### Descriptive Stats ####
# Vector of Continuous Vars
contVar <- c("numgrade", "numgrade_2", "mathsr", "englsr", "hsgpa", "apscore")

# Full Dataset
view(dfSummary(df_clean))
lapply(df_clean[contVar], function(x) summary(x))

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

#### Weighting ####
## Bio 
# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + ethniccode_cat +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort_2),
        data = df_bio2, estimand = "ATT", m.threshold = .05)

# Estimate weights
bio.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + ethniccode_cat +
                      scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort_2),
                    data = df_bio2, estimand = "ATT")
summary(bio.out) 

# Check balance after weighting
bal.tab(bio.out, m.threshold = .05, disp.v.ratio = TRUE)

# Extract weights
bio.w <- svydesign(ids = ~1, weights = bio.out$weights,
                   data = df_bio2)

## Chem Weighting 
# Data with no missing
df_gchem2_comp <- df_chem2 %>%  
  select(numgrade_2, apscore_full, eligible_to_skip, skipped_course, firstgen, lowincomeflag, female, urm, 
         hsgpa, mathsr, englsr, enrl_from_cohort) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
        data = df_gchem2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
chem.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + urm +
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
                     data = df_gchem2_comp, estimand = "ATT")
summary(chem.out) 

# Check balance after weighting
bal.tab(chem.out, m.threshold = .05, disp.v.ratio = TRUE)

# Extract weights
chem.w <- svydesign(ids = ~1, weights = chem.out$weights,
                    data = df_gchem2_comp)

## Phys Weighting 
# Data with no missing
df_phys2_comp <- df_phys2 %>%  
  select(numgrade_2, apscore_full, eligible_to_skip, skipped_course, firstgen, lowincomeflag, female, urm, 
         hsgpa, mathsr, englsr, enrl_from_cohort) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
        data = df_phys2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
phys.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + urm +
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
                     data = df_phys2_comp, estimand = "ATT")
summary(phys.out) 

# Check balance after weighting
bal.tab(phys.out, m.threshold = .05, disp.v.ratio = TRUE)

#Extract weights
phys.w <- svydesign(ids = ~1, weights = phys.out$weights,
                    data = df_phys2_comp)

## Create weighted dataframes subsets for analysis samples (by each discipline) ####
# Bio
# Took AP 
df_bio_aptakers <- bio.w %>%
  subset(aptaker == 1)
# Skip eligible
df_bio_skeligible <- bio.w %>%
  subset(eligible_to_skip == 1)
# Skip eligible (at each score)
df_bio_skeligible.3 <- df_bio2 %>%
  subset(apscore == 3)
df_bio_skeligible.4 <- df_bio2 %>%
  subset(apscore == 4)
df_bio_skeligible.5 <- df_bio2 %>%
  subset(apscore == 5)

# Chem
# Took AP 
df_chem_aptakers <- df_chem2 %>%
  subset(aptaker == 1)
# Skip eligible
df_chem_skeligible <- df_chem2 %>%
  subset(eligible_to_skip == 1)
# Skip eligible (at each score)
df_chem_skeligible.3 <- df_chem2 %>%
  subset(apscore == 3)
df_chem_skeligible.4 <- df_chem2 %>%
  subset(apscore == 4)
df_chem_skeligible.5 <- df_chem2 %>%
  subset(apscore == 5)

# Phys
# Took AP 
df_phys_aptakers <- df_phys2 %>%
  subset(aptaker == 1)
# Skip eligible
df_phys_skeligible <- df_phys2 %>%
  subset(eligible_to_skip == 1)
# Skip eligible (at each score)
df_phys_skeligible.3 <- df_phys2 %>%
  subset(apscore == 3)
df_phys_skeligible.4 <- df_phys2 %>%
  subset(apscore == 4)
df_phys_skeligible.5 <- df_phys2 %>%
  subset(apscore == 5)

#### Run Models (for each discipline) ####
# Note: For model specifications, check: https://docs.google.com/spreadsheets/d/1rN8W_iz1mr7lEzBGfdTZHa45wKOSLiSF8VEpChCPsmE/edit#gid=129222174

# BIO
# RQ1 - Out of everyone, who earns an AP score that makes them eligible to skip? 
# (Formerly 1ci.)
bio_rq1 <- glm(eligible_to_skip ~ factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                 data=df_bio2, binomial(link="logit"))
summary(bio_rq1)
logistic.display(bio_rq1)

# RQ2 - Out of everyone who was eligible to skip, who actually skipped? 
# (Formerly 2a.)
bio_rq2a <- glm(skipped_course ~ 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                  scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort), 
                data=df_bio_skeligible, binomial(link="logit"))
summary(bio_rq2a)
logistic.display(bio_rq2a)
#robustse(bio_rq2a, coef="odd.ratio")

# RQ3A - Out of everyone, what was the effect of skipping on 2nd course grade (at each AP score)? 
# (Formerly 2b.)
bio_rq2b <- lm(scale(numgrade_2) ~ factor(skipped_course)*factor(apscore), 
               data=df_bio2)
summary(bio_rq2b)
#std_beta(bio_rq2b)

# (When Score  = 3)
bio_rq2b.3 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_skeligible.3)
summary(bio_rq2b.3)
#std_beta(bio_rq2b.3)

# (When Score  = 4)
bio_rq2b.4 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_skeligible.4)
summary(bio_rq2b.4)
#std_beta(bio_rq2b.4)

# (When Score  = 5)
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

##### Weighting ####
# Bio Weighting ####
#Data with no missing
df_bio2_comp <- df_bio2 %>%  
  select(numgrade_2, apscore_full, eligible_to_skip, skipped_course, firstgen, lowincomeflag, female, urm, 
         hsgpa, mathsr, englsr, enrl_from_cohort) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
        data = df_bio2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
bio.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + urm +
                      scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
                    data = df_bio2_comp, estimand = "ATT")
summary(bio.out) 

# Check balance after weighting
bal.tab(bio.out, m.threshold = .05, disp.v.ratio = TRUE)

#Extract weights
bio.w <- svydesign(ids = ~1, weights = bio.out$weights,
                   data = df_bio2_comp)

# Chem Weighting ####
#Data with no missing
df_gchem2_comp <- df_chem2 %>%  
  select(numgrade_2, apscore_full, eligible_to_skip, skipped_course, firstgen, lowincomeflag, female, urm, 
         hsgpa, mathsr, englsr, enrl_from_cohort) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
        data = df_gchem2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
chem.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + urm +
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
                     data = df_gchem2_comp, estimand = "ATT")
summary(chem.out) 

# Check balance after weighting
bal.tab(chem.out, m.threshold = .05, disp.v.ratio = TRUE)

#Extract weights
chem.w <- svydesign(ids = ~1, weights = chem.out$weights,
                    data = df_gchem2_comp)

# Phys Weighting ####
#Data with no missing
df_phys2_comp <- df_phys2 %>%  
  select(numgrade_2, apscore_full, eligible_to_skip, skipped_course, firstgen, lowincomeflag, female, urm, 
         hsgpa, mathsr, englsr, enrl_from_cohort) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
        data = df_phys2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
phys.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + urm +
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
                     data = df_phys2_comp, estimand = "ATT")
summary(phys.out) 

# Check balance after weighting
bal.tab(phys.out, m.threshold = .05, disp.v.ratio = TRUE)

#Extract weights
phys.w <- svydesign(ids = ~1, weights = phys.out$weights,
                    data = df_phys2_comp)

# Model 2e.2: Grade for everyone (c.skipped_course, no score) ####
#Bio
m2e.2_BY <- svyglm(scale(numgrade_2) ~ skipped_course + firstgen + factor(lowincomeflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = bio.w)
summary(m2e.2_BY)
cbind("Beta" = coef(m2e.2_BY), confint.default(m2e.2_BY, level = 0.95))

# w/ Robust SE 
summ(m2e.2_BY, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Chem
m2e.2_CH <- svyglm(scale(numgrade_2) ~ skipped_course + firstgen + factor(lowincomeflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = chem.w)
summary(m2e.2_CH)
cbind("Beta" = coef(m2e.2_CH), confint.default(m2e.2_CH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_CH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Phys
m2e.2_PH <- svyglm(scale(numgrade_2) ~ skipped_course + firstgen + factor(lowincomeflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = phys.w)
summary(m2e.2_PH)
cbind("Beta" = coef(m2e.2_PH), confint.default(m2e.2_PH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_PH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

# Model 2g.2: Grade for everyone (c.skip eligible, no score) ####
#Bio
m2g.2_BY <- svyglm(scale(numgrade_2) ~ eligible_to_skip + firstgen + factor(lowincomeflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = bio.w)
summary(m2g.2_BY)
cbind("Beta" = coef(m2g.2_BY), confint.default(m2g.2_BY, level = 0.95))

summ(m2g.2_BY, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Chem
m2g.2_CH <- svyglm(scale(numgrade_2) ~ eligible_to_skip + firstgen + factor(lowincomeflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = chem.w)
summary(m2g.2_CH)
cbind("Beta" = coef(m2g.2_CH), confint.default(m2g.2_CH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_CH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Phys
m2e.2_PH <- svyglm(scale(numgrade_2) ~ eligible_to_skip + firstgen + factor(lowincomeflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = phys.w)
summary(m2e.2_PH)
cbind("Beta" = coef(m2e.2_PH), confint.default(m2e.2_PH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_PH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 


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

#### Create Plots ####
# BIO
# Generate estimates
fit_bio <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
              df_bio2, na.action=na.exclude)
# Add column of fitted values
df_viz_bio <- df_bio2 %>%
  mutate(numgrade_2.fitted = fitted(fit_bio))

# Raw
raw_bio <- df_viz_bio %>%
  ggplot(aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = paste("Mean Grade in", "BIO", "2"), title= paste("BIO", "Uncontrolled Model"))
plot(raw_bio)

# Fitted
fit_bio <- df_viz_bio %>%
  ggplot(aes(y = numgrade_2.fitted, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = paste("Mean Grade in", "BIO", "2"), title= paste("BIO", "Fitted Model"))
plot(fit_bio)

# Histogram
hist_bio <- df_viz_bio %>%
  ggplot(aes(x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_histogram(stat='count', position = position_dodge(preserve = "single")) +
  scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.85),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")) +
  labs(x = "AP Score", y = "Number of Students", title= paste("Histogram of AP", "BIO", "Scores"))
plot(hist_bio)

#CHEM
# Generate estimates
fit_chem <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               df_chem2, na.action=na.exclude)
# Add column of fitted values
df_viz_chem <- df_chem2 %>%
  mutate(numgrade_2.fitted = fitted(fit_chem))

# Raw
raw_chem <- df_viz_chem %>%
  ggplot(aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = paste("Mean Grade in", "CHEM", "2"), title= paste("CHEM", "Uncontrolled Model"))
plot(raw_chem)

# Fitted
fit_chem <- df_viz_chem %>%
  ggplot(aes(y = numgrade_2.fitted, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = paste("Mean Grade in", "CHEM", "2"), title= paste("CHEM", "Fitted Model"))
plot(fit_chem)

# Histogram
hist_chem <- df_viz_chem %>%
  ggplot(aes(x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_histogram(stat='count', position = position_dodge(preserve = "single")) +
  scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.85),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")) +
  labs(x = "AP Score", y = "Number of Students", title= paste("Histogram of AP", "CHEM", "Scores"))
plot(hist_chem)

#PHYSICS
# Generate estimates
fit_phys <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               df_phys2, na.action=na.exclude)
# Add column of fitted values
df_viz_phys <- df_phys2 %>%
  mutate(numgrade_2.fitted = fitted(fit_phys))

# Raw
raw_phys <- df_viz_phys %>%
  ggplot(aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = paste("Mean Grade in", "PHYS", "2"), title= paste("PHYS", "Uncontrolled Model"))
plot(raw_phys)

# Fitted
fit_phys <- df_viz_phys %>%
  ggplot(aes(y = numgrade_2.fitted, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_point(stat = 'summary', fun.y = 'mean', size=3) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
  geom_smooth(stat = 'summary', method = 'loess') +
  scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.25),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")
  ) +
  labs(x = "AP Score", y = paste("Mean Grade in", "PHYS", "2"), title= paste("PHYS", "Fitted Model"))
plot(fit_phys)

# Histogram
hist_phys <- df_viz_phys %>%
  ggplot(aes(x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
  geom_histogram(stat='count', position = position_dodge(preserve = "single")) +
  scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL)) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill=NA),
    axis.title.x.top = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position=c(0.85, 0.85),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black")) +
  labs(x = "AP Score", y = "Number of Students", title= paste("Histogram of AP", "PHYS", "Scores"))
plot(hist_phys)

