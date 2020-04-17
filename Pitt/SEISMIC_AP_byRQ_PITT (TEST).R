#### Setup ####
# Load packages and data
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse")

# Turn on scientific notation up to 10 digits
options(scipen=999)

# Load Clean Dataframe ####
df_clean <- read.csv("~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/SEISMIC_AP_CLEAN.csv")
names(df_clean)

# Generate Subset Dataframes ####
df_full <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 |
           discipline == "CHEM" & apyear >= 2014 |
           discipline == "PHYS" & apyear >= 2015)

df_aptakers <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 & aptaker == 1 |
           discipline == "CHEM" & apyear >= 2014 & aptaker == 1 |
           discipline == "PHYS" & apyear >= 2015 & aptaker == 1) 

df_skipeligible <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 & aptaker == 1 & apskipper == 1 |
           discipline == "CHEM" & apyear >= 2014 & aptaker == 1 & apskipper == 1 |
           discipline == "PHYS" & apyear >= 2015 & aptaker == 1 & apskipper == 1) 

# Skip Eligible Scores by Course
# Bio
df_skipeligible_bio.4 <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 & aptaker == 1 & apskipper == 1 & apscore == 4)
df_skipeligible_bio.5 <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 & aptaker == 1 & apskipper == 1 & apscore == 5)
# Chem
df_skipeligible_chem.3 <- df_clean %>%
  filter(discipline == "CHEM" & apyear >= 2013 & aptaker == 1 & apskipper == 1 & apscore == 3)
df_skipeligible_chem.4 <- df_clean %>%
  filter(discipline == "CHEM" & apyear >= 2013 & aptaker == 1 & apskipper == 1 & apscore == 4)
df_skipeligible_chem.5 <- df_clean %>%
  filter(discipline == "CHEM" & apyear >= 2013 & aptaker == 1 & apskipper == 1 & apscore == 5)
# Phys
df_skipeligible_phys.5 <- df_clean %>%
  filter(discipline == "PHYS" & apyear >= 2013 & aptaker == 1 & apskipper == 1 & apscore == 5)

# Model 1a: Credits ####
RQ1a <- df_full %>%
  group_by(discipline) %>%
  do(mod = glm(aptaker ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
               family = binomial, data = .))

RQ1a <- RQ1a %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

# For Logistic: Create OR and 95% CI for OR 
RQ1a <- RQ1a %>%
  mutate(OR = exp(Estimate)) %>%
  mutate(OR_CILow = exp(X2.5..)) %>%
  mutate(OR_CIHi = exp(X97.5..))

write.csv(RQ1a, file = "SEISMIC_AP_Output_RQ1a.csv")

# Model 1b: Score ####
RQ1b <- df_aptakers %>%
  group_by(discipline) %>%
  do(mod = lm(scale(apscore) ~ factor(firstgen) + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
                data = .))

RQ1b <- RQ1b %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ1b, file = "SEISMIC_AP_Output_RQ1b.csv")

# Model 1c: Eligible to Skip ####
RQ1c <- df_full %>%
  group_by(discipline) %>%
  do(mod = glm(apskipper ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
               family = binomial, data = .))

RQ1c <- RQ1c %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

# For Logistic: Create OR and 95% CI for OR 
RQ1c <- RQ1c %>%
  mutate(OR = exp(Estimate)) %>%
  mutate(OR_CILow = exp(X2.5..)) %>%
  mutate(OR_CIHi = exp(X97.5..))

write.csv(RQ1c, file = "SEISMIC_AP_Output_RQ1c.csv")

#### RQ2 ####
# Model 2a: Skipped if eligible ####
RQ2a <- df_skipeligible %>%
  group_by(discipline) %>%
  do(mod = glm(skipped ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2),
               family = binomial, data = .))

RQ2a <- RQ2a %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

# For Logistic: Create OR and 95% CI for OR 
RQ2a <- RQ2a %>%
  mutate(OR = exp(Estimate)) %>%
  mutate(OR_CILow = exp(X2.5..)) %>%
  mutate(OR_CIHi = exp(X97.5..))

write.csv(RQ2a, file = "SEISMIC_AP_Output_RQ2a.csv")

# Model 2a.1-3: Skipped if eligible ####
# Eligibility at Each Cut Score 
#Bio if apscore=4
m2.a4_BY <- lm(skipped ~ factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_bio.4)
summary(m2.a4_BY)
exp(cbind("Odds Ratio" = coef(m2.a4_BY), confint.default(m2.a_PH, level = 0.95)))
logistic.display(m2.a4_BY)

#Bio if apscore=5
m2.a5_BY <- lm(skipped ~  factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_bio.5)
summary(m2.a5_BY)
exp(cbind("Odds Ratio" = coef(m2.a5_BY), confint.default(m2.a_PH, level = 0.95)))
logistic.display(m2.a5_BY)

#Chem if apscore = 3
m2.a3_CH <- lm(skipped ~  factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_chem.3)
summary(m2.a3_CH)
exp(cbind("Odds Ratio" = coef(m2.a3_CH), confint.default(m2.a_PH, level = 0.95)))
logistic.display(m2.a3_CH)

#Chem if apscore = 4
m2.a4_CH <- lm(skipped ~  factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_chem.4)
summary(m2.a4_CH)
exp(cbind("Odds Ratio" = coef(m2.a4_CH), confint.default(m2.a_PH, level = 0.95)))
logistic.display(m2.a4_CH)

#Chem if apscore = 5
m2.a5_CH <- lm(skipped ~  factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_chem.5)
summary(m2.a5_CH)
exp(cbind("Odds Ratio" = coef(m2.a5_CH), confint.default(m2.a_PH, level = 0.95)))
logistic.display(m2.a5_CH)

#Phys if PH = 5
m2.a5_PH <- lm(skipped ~  factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_PHeligible.5)
summary(m2.a5_PH)
exp(cbind("Odds Ratio" = coef(m2.a5_PH), confint.default(m2.a_PH, level = 0.95)))
logistic.display(m2.a5_PH)

# Model 2b: Grade for AP takers ####
RQ2b <- df_skipeligible %>%
  group_by(discipline) %>%
  do(mod = lm(scale(numgrade_2) ~ factor(skipped) + scale(apscore) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2),
                 data = .))

RQ2b <- RQ2b %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ2b, file = "SEISMIC_AP_Output_RQ2b.csv")

# Model 2b.1-3: Score if eligible (At Each Eligibility Score) #### 
#Bio if apscore=4
m2.b4_BY <- lm(scale(numgrade_2) ~ factor(skipped) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_bio.4)
summary(m2.b4_BY)

#Bio if apscore=5
m2.b5_BY <- lm(scale(numgrade_2) ~ factor(skipped) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_bio.5)
summary(m2.b5_BY)

#Chem if apscore = 3
m2.b3_CH <- lm(scale(numgrade_2) ~ factor(skipped) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_chem.3)
summary(m2.b3_CH)

#Chem if apscore = 4
m2.b4_CH <- lm(scale(numgrade_2) ~ factor(skipped) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_chem.4)
summary(m2.b4_CH)

#Chem if apscore = 5
m2.b5_CH <- lm(scale(numgrade_2) ~ factor(skipped) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_skipeligible_chem.5)
summary(m2.b5_CH)

#Phys if PH = 5
m2.b5_PH <- lm(scale(numgrade_2) ~ factor(skipped) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2), 
               df_PHeligible.5)
summary(m2.b5_PH)

# Model 2c: Grade for AP takers ####
RQ2c <- df_full %>%
  group_by(discipline) %>%
  do(mod = lm(scale(numgrade_2) ~ factor(skipped) + scale(apscore) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2),
              data = .))

RQ2b <- RQ2b %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ2b, file = "SEISMIC_AP_Output_RQ2b.csv")

# Model 2d: Grade for AP takers (c.skipped, no score)####
RQ2d <- df_aptakers %>%
  group_by(discipline) %>%
  do(mod = lm(scale(numgrade_2) ~ factor(skipped) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2),
              data = .))

RQ2d <- RQ2d %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ2d, file = "SEISMIC_AP_Output_RQ2d.csv")

# Model 2e: Grade for Everyone (c.skipped, no score) ####
RQ2e <- df_full %>%
  group_by(discipline) %>%
  do(mod = lm(scale(numgrade_2) ~ factor(skipped) + factor(firstgen) + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2),
              data = .))

RQ2e <- RQ2e %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ2e, file = "SEISMIC_AP_Output_RQ2e.csv")

# Model 2f: Grade for AP takers (c.skip eligible, no score) ####
RQ2f <- df_aptakers %>%
  group_by(discipline) %>%
  do(mod = lm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2),
              data = .))

RQ2f <- RQ2f %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ2f, file = "SEISMIC_AP_Output_RQ2f.csv")

# Model 2g: Grade for everyone (c.skip eligible, no score) ####
RQ2g <- df_aptakers %>%
  group_by(discipline) %>%
  do(mod = lm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_yr_2) + factor(crs_term_sem_2),
              data = .))

RQ2g <- RQ2g %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ2g, file = "SEISMIC_AP_Output_RQ2g.csv")
              
