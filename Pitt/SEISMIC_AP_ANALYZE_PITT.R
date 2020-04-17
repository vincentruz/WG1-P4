#SEISMIC AP Analysis

#### Setup ####
# Load packages and data
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "data.table", "psych", "summarytools", "haven", "Hmisc", "forcats", 
               "naniar", "QuantPsyc", "epiDisplay", "corrplot", "tidyselect", "mctest", 
               "MatchIt", "WeightIt", "cobalt", "survey", "jtools","mice")

# Functions and settings ####
# Use dplyr for 'select'
select <- dplyr::select
# Turn off scientific notation
options(scipen = 10)
# VIF function 
VIF <- function(linear.model, no.intercept=FALSE, all.diagnostics=FALSE, plot=FALSE) {
  require(mctest)
  if(no.intercept==FALSE) design.matrix <- model.matrix(linear.model)[,-1]
  if(no.intercept==TRUE) design.matrix <- model.matrix(linear.model)
  if(plot==TRUE) mc.plot(design.matrix,linear.model$model[1])
  if(all.diagnostics==FALSE) output <- imcdiag(design.matrix,linear.model$model[1], method='VIF')$idiags[,1]
  if(all.diagnostics==TRUE) output <- imcdiag(design.matrix,linear.model$model[1])
  output
}

# Load Raw Dataframe ####
df_clean <- read.csv("~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/SEISMIC_AP_CLEAN.csv")
names(df_clean)

# Subset Dataframes #### 
# Bio
# Took 2nd course in sequence
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013)
# Took AP 
df_BYtakers <- df_bio2 %>%
  subset(aptaker == 1)
# Skip eligible
df_BYeligible<- df_bio2 %>%
  subset(apskipper == 1)
df_BYeligible.4 <- df_bio2 %>%
  subset(apscore == 4)
df_BYeligible.5 <- df_bio2 %>%
  subset(apscore == 5)

#Chem
# Took 2nd course in sequence
df_chem2 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014)
# Took AP 
df_CHtakers <- df_chem2 %>%
  subset(aptaker == 1)
# Skip eligible
df_CHeligible<- df_chem2 %>%
  subset(apskipper == 1)
df_CHeligible.3 <- df_chem2 %>%
  subset(apscore == 3)
df_CHeligible.4 <- df_chem2 %>%
  subset(apscore == 4)
df_CHeligible.5 <- df_chem2 %>%
  subset(apscore == 5)

#Phys
# Took 2nd course in sequence
df_phys2 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015)
# Took AP 
df_PHtakers <- df_phys2 %>%
  subset(aptaker == 1)
# Skip eligible
df_PHeligible<- df_phys2 %>%
  subset(apskipper == 1)
df_PHeligible.5 <- df_phys2 %>%
  subset(apscore == 5)

# Descriptive Stats ####
# Correlations among key predictors #
correl1 <- df_clean %>%
  ungroup() %>%
  select(firstgen, lowincomflag, female, urm, 
         hsgpa, mathsr, englsr, 
         cohort_2014, cohort_2015, cohort_2016, cohort_2017, cohort_2018)
corrplot1 <- cor(correl1, use="pairwise.complete.obs")
corrplot(corrplot1,type = "lower")

#### RQ1 ####
# What student characteristics are associated with student participation and success in AP courses 
# for students enrolled at the selected universities?

# Model 1a: Credits ####
#Bio
m1.a_BY <- glm(aptaker ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
               binomial(link = "logit"), df_bio2)
summary(m1.a_BY)
exp(cbind("Odds Ratio" = coef(m1.a_BY), confint.default(m1.a_BY, level = 0.95)))
logistic.display(m1.a_BY)

#Chem
m1.a_CH <- glm(aptaker ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
               binomial(link = "logit"), df_chem2)
summary(m1.a_CH)   
exp(cbind("Odds Ratio" = coef(m1.a_CH), confint.default(m1.a_CH, level = 0.95)))
logistic.display(m1.a_CH)

#Physics
m1.a_PH <- glm(aptaker ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
               binomial(link = "logit"), df_phys2)
summary(m1.a_PH)
exp(cbind("Odds Ratio" = coef(m1.a_PH), confint.default(m1.a_PH, level = 0.95)))
logistic.display(m1.a_PH)

# Model 1b: Score ####
#Bio
m1.b_BY <- lm(scale(apscore) ~ firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
              df_BYtakers)
summary(m1.b_BY)
cbind("Beta" = coef(m1.b_BY), confint.default(m1.b_BY, level = 0.95))

#Chem
m1.b_CH <- lm(scale(apscore) ~ firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_CHtakers)
summary(m1.b_CH)   
cbind("Beta" = coef(m1.b_CH), confint.default(m1.b_CH, level = 0.95))

#Physics
m1.b_PH <- lm(scale(apscore) ~ firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_PHtakers)
summary(m1.b_PH)   
cbind("Beta" = coef(m1.b_PH), confint.default(m1.b_PH, level = 0.95))

# Model 1c: Eligible to Skip ####
#Bio
m1.c_BY <- glm(apskipper ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               binomial(link = "logit"), df_bio2)
summary(m1.c_BY)
exp(cbind("Odds Ratio" = coef(m1.c_BY), confint.default(m1.c_BY, level = 0.95)))
logistic.display(m1.c_BY)

#Chem
m1.c_CH <- glm(apskipper ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               binomial(link = "logit"), df_chem2)
summary(m1.c_CH)   
exp(cbind("OddsRatio" = coef(m1.c_CH), confint.default(m1.c_CH, level = 0.95)))
logistic.display(m1.c_CH)

#Physics
# Skip w/ CE or CM
m1.c_PH <- glm(apskipper ~ factor(firstgen) + factor(lowincomflag) + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               binomial(link = "logit"), df_phys2)
summary(m1.c_PH)   
exp(cbind("Odds Ratio" = coef(m1.c_PH), confint.default(m1.c_PH, level = 0.95)))
logistic.display(m1.c_PH)

#### RQ2 ####
# Model 2a: Skipped if eligible ####
#Bio
m2.a_BY <- glm(skipped ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               binomial(link = "logit"), df_BYeligible)
summary(m2.a_BY)
exp(cbind("Odds Ratio" = coef(m2.a_BY), confint.default(m2.a_BY, level = 0.95)))
logistic.display(m2.a_BY)

describe(df_BYeligible$skipped)

#Chem
m2.a_CH <- glm(skipped ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               binomial(link = "logit"), df_CHeligible)
summary(m2.a_CH)   
exp(cbind("Odds Ratio" = coef(m2.a_CH), confint.default(m2.a_CH, level = 0.95)))
logistic.display(m2.a_CH)

describe(df_CHeligible$skipped)

#Physics
# Skip w/ CE or CM
m2.a_PH <- glm(skipped ~ factor(firstgen) + factor(lowincomflag) + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
               binomial(link = "logit"), df_PHeligible)
summary(m2.a_PH)   
exp(cbind("Odds Ratio" = coef(m2.a_PH), confint.default(m2.a_PH, level = 0.95)))
logistic.display(m2.a_PH)

describe(df_PHeligible$skipped)

# Grade in second course if took AP?
# Model 2b: Grade for AP takers ####
#Bio
m2.b_BY <- lm(scale(numgrade_2) ~ skipped + scale(apscore) + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_BYtakers)
summary(m2.b_BY)
cbind("Beta" = coef(m2.b_BY), confint.default(m2.b_BY, level = 0.95))

#Bio eligible
m2.b.el_BY <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
                 df_BYeligible)
summary(m2.b.el_BY)
cbind("Beta" = coef(m2.b.el_BY), confint.default(m2.b.el_BY, level = 0.95))

#Bio if apscore=4
m2.b4_BY <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               df_BYeligible.4)
summary(m2.b4_BY)
cbind("Beta" = coef(m2.b4_BY), confint.default(m2.b4_BY, level = 0.95))

#Bio if apscore=5
m2.b5_BY <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               df_BYeligible.5)
summary(m2.b5_BY)
cbind("Beta" = coef(m2.b5_BY), confint.default(m2.b5_BY, level = 0.95))

#Chem
m2.b_CH <- lm(scale(numgrade_2) ~ skipped + scale(apscore) + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_CHtakers)
summary(m2.b_CH)
cbind("Beta" = coef(m2.b_CH), confint.default(m2.b_CH, level = 0.95))

#Chem eligible
m2.b.el_CH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
                 df_CHeligible)
summary(m2.b.el_CH)
cbind("Beta" = coef(m2.b.el_CH), confint.default(m2.b.el_CH, level = 0.95))

#Chem if apscore = 3
m2.b3_CH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               df_CHeligible.3)
summary(m2.b3_CH)
cbind("Beta" = coef(m2.b3_CH), confint.default(m2.b3_CH, level = 0.95))

#Chem if apscore = 4
m2.b4_CH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               df_CHeligible.4)
summary(m2.b4_CH)
cbind("Beta" = coef(m2.b4_CH), confint.default(m2.b4_CH, level = 0.95))

#Chem if apscore = 5
m2.b5_CH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               df_CHeligible.5)
summary(m2.b5_CH)
cbind("Beta" = coef(m2.b5_CH), confint.default(m2.b5_CH, level = 0.95))

#Phys
m2.b_PH <- lm(scale(numgrade_2) ~ skipped + scale(apscore) + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_PHtakers)
summary(m2.b_PH)
cbind("Beta" = coef(m2.b_PH), confint.default(m2.b_PH, level = 0.95))

#Phys eligible
m2.b.el_PH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
                 df_PHeligible)
summary(m2.b.el_PH)
cbind("Beta" = coef(m2.b.el_PH), confint.default(m2.b.el_PH, level = 0.95))

#Phys if PH = 5
m2.b5_PH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
               df_PHeligible.5)
summary(m2.b5_PH)
cbind("Beta" = coef(m2.b5_PH), confint.default(m2.b5_PH, level = 0.95))

# Model 2c: Grade for everyone ####
#Bio
m2.c_BY <- lm(scale(numgrade_2) ~ skipped + scale(apscore_full) + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_bio2)
summary(m2.c_BY)
cbind("Beta" = coef(m2.c_BY), confint.default(m2.c_BY, level = 0.95))

#Chem
m2.c_CH <- lm(scale(numgrade_2) ~ skipped + scale(apscore_full) + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_bio2)
summary(m2.c_CH)
cbind("Beta" = coef(m2.c_CH), confint.default(m2.c_CH, level = 0.95))

#Phys
m2.c_PH <- lm(scale(numgrade_2) ~ skipped + scale(apscore_full) + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_phys2)
summary(m2.c_PH)
cbind("Beta" = coef(m2.c_PH), confint.default(m2.c_PH, level = 0.95))

# Model 2d: Grade for AP takers (c.skipped, no score)####
#Bio
m2.d_BY <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_BYtakers)
summary(m2.d_BY)
cbind("Beta" = coef(m2.d_BY), confint.default(m2.d_BY, level = 0.95))

#Chem
m2.d_CH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_CHtakers)
summary(m2.d_CH)
cbind("Beta" = coef(m2.d_CH), confint.default(m2.d_CH, level = 0.95))

#Phys
m2.d_PH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_PHtakers)
summary(m2.d_PH)
cbind("Beta" = coef(m2.d_PH), confint.default(m2.d_PH, level = 0.95))

# Model 2e: Grade for everyone (c.skipped, no score) ####
#Bio
m2.e_BY <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_bio2)
summary(m2.e_BY)
cbind("Beta" = coef(m2.e_BY), confint.default(m2.e_BY, level = 0.95))

#Chem
m2.e_CH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_chem2)
summary(m2.e_CH)
cbind("Beta" = coef(m2.e_CH), confint.default(m2.e_CH, level = 0.95))

#Phys
m2.e_PH <- lm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_phys2)
summary(m2.e_PH)
cbind("Beta" = coef(m2.e_PH), confint.default(m2.e_PH, level = 0.95))

# Model 2f: Grade for AP takers (c.skip eligible, no score)####
#Bio
m2.f_BY <- lm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_BYtakers)
summary(m2.f_BY)
cbind("Beta" = coef(m2.f_BY), confint.default(m2.f_BY, level = 0.95))

#Chem
m2.f_CH <- lm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_CHtakers)
summary(m2.f_CH)
cbind("Beta" = coef(m2.f_CH), confint.default(m2.f_CH, level = 0.95))

#Phys
m2.f_PH <- lm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_PHtakers)
summary(m2.f_PH)
cbind("Beta" = coef(m2.f_PH), confint.default(m2.f_PH, level = 0.95))

# Model 2g: Grade for everyone (c.skip eligible, no score) ####
#Bio
m2.g_BY <- lm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_bio2)
summary(m2.g_BY)
cbind("Beta" = coef(m2.g_BY), confint.default(m2.g_BY, level = 0.95))

#Chem
m2.g_CH <- lm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_chem2)
summary(m2.g_CH)
cbind("Beta" = coef(m2.g_CH), confint.default(m2.g_CH, level = 0.95))

#Phys
m2.g_PH <- lm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), 
              df_phys2)
summary(m2.g_PH)
cbind("Beta" = coef(m2.g_PH), confint.default(m2.g_PH, level = 0.95))

##### Weighting ####
# Bio Weighting ####
#Data with no missing
df_bio2_comp <- df_bio2 %>%  
  select(numgrade_2, apscore_full, apskipper, skipped, firstgen, lowincomflag, female, urm, 
         hsgpa, mathsr, englsr, enrl_from_cohort) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped ~ + firstgen + factor(lowincomflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
        data = df_bio2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
bio.out <- weightit(skipped ~ firstgen + factor(lowincomflag) + female + urm +
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
  select(numgrade_2, apscore_full, apskipper, skipped, firstgen, lowincomflag, female, urm, 
         hsgpa, mathsr, englsr, enrl_from_cohort) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped ~ + firstgen + factor(lowincomflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
        data = df_gchem2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
chem.out <- weightit(skipped ~ firstgen + factor(lowincomflag) + female + urm +
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
  select(numgrade_2, apscore_full, apskipper, skipped, firstgen, lowincomflag, female, urm, 
         hsgpa, mathsr, englsr, enrl_from_cohort) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped ~ + firstgen + factor(lowincomflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
        data = df_phys2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
phys.out <- weightit(skipped ~ firstgen + factor(lowincomflag) + female + urm +
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
                     data = df_phys2_comp, estimand = "ATT")
summary(phys.out) 

# Check balance after weighting
bal.tab(phys.out, m.threshold = .05, disp.v.ratio = TRUE)

#Extract weights
phys.w <- svydesign(ids = ~1, weights = phys.out$weights,
                    data = df_phys2_comp)

# Model 2e.2: Grade for everyone (c.skipped, no score) ####
#Bio
m2e.2_BY <- svyglm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = bio.w)
summary(m2e.2_BY)
cbind("Beta" = coef(m2e.2_BY), confint.default(m2e.2_BY, level = 0.95))

# w/ Robust SE 
summ(m2e.2_BY, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Chem
m2e.2_CH <- svyglm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = chem.w)
summary(m2e.2_CH)
cbind("Beta" = coef(m2e.2_CH), confint.default(m2e.2_CH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_CH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Phys
m2e.2_PH <- svyglm(scale(numgrade_2) ~ skipped + firstgen + factor(lowincomflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = phys.w)
summary(m2e.2_PH)
cbind("Beta" = coef(m2e.2_PH), confint.default(m2e.2_PH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_PH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

# Model 2g.2: Grade for everyone (c.skip eligible, no score) ####
#Bio
m2g.2_BY <- svyglm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = bio.w)
summary(m2g.2_BY)
cbind("Beta" = coef(m2g.2_BY), confint.default(m2g.2_BY, level = 0.95))

summ(m2g.2_BY, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Chem
m2g.2_CH <- svyglm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = chem.w)
summary(m2g.2_CH)
cbind("Beta" = coef(m2g.2_CH), confint.default(m2g.2_CH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_CH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Phys
m2e.2_PH <- svyglm(scale(numgrade_2) ~ apskipper + firstgen + factor(lowincomflag) + female + urm +
                     scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort), design = phys.w)
summary(m2e.2_PH)
cbind("Beta" = coef(m2e.2_PH), confint.default(m2e.2_PH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_PH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 


#### PSM ####
# Bio Matching ####
# Covariates for PSM
AP_bio_cov <- c('female', 'firstgen', 'lowincomflag', 'englsr', 'hsgpa', 'mathsr', 'cohort_2015', 'cohort_2016', 'cohort_2017', 'cohort_2018', 'apscore')

# Estimate propensity score
bio.psm <- glm(skipped ~ female + englsr + mathsr + apscore,
               family = binomial(), data = df_bio2)

# Create dataframe with propensity scores
df_bio2_psm <- data.frame(pr_score = predict(bio.psm, type = "response"),
                          skipped = bio.psm$model$skipped)

# Matched dataset (no missing)
df_bio2_nomiss <- df_bio2 %>%  
  select(numgrade_2, apscore, skipped, one_of(AP_cov)) %>%
  na.omit()

bio_matched <- matchit(skipped ~ female + englsr + mathsr + apscore,
                       method = "nearest", data = df_bio2_nomiss)

df_bio2_matched <- match.data(bio_matched)

# Examine pre-matched and matched differences
df_bio2 %>%
  group_by(skipped) %>%
  select(one_of(AP_cov), apscore) %>%
  summarise_all(funs(mean(., na.rm = T)))

df_bio2_matched %>%
  group_by(skipped) %>%
  select(one_of(AP_cov), apscore) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Gchem Matching ####
# Covariates for PSM
AP_gchem_cov <- c('female', 'firstgen', 'lowincomflag', 'englsr', 'hsgpa', 'mathsr', 'cohort_2015', 'cohort_2016', 'cohort_2017', 'cohort_2018', 'apscore')

# Estimate propensity score
gchem.psm <- glm(skipped ~ female + englsr + mathsr + apscore,
                 family = binomial(), data = df_chem2)

# Create dataframe with propensity scores
df_gchem2_psm <- data.frame(pr_score = predict(gchem.psm, type = "response"),
                            skipped = gchem.psm$model$skipped)

# Matched dataset (no missing)
df_gchem2_nomiss <- df_chem2 %>%  
  select(numgrade_2, apscore, skipped, one_of(AP_gchem_cov)) %>%
  na.omit()

gchem_matched <- matchit(skipped ~ female + englsr + mathsr + apscore,
                         method = "nearest", data = df_gchem2_nomiss)

df_gchem2_matched <- match.data(gchem_matched)

# Examine pre-matched and matched differences
df_chem2 %>%
  group_by(skipped) %>%
  select(one_of(AP_gchem_cov), apscore) %>%
  summarise_all(funs(mean(., na.rm = T)))

df_gchem2_matched %>%
  group_by(skipped) %>%
  select(one_of(AP_gchem_cov), apscore) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Phys Matching ####
# Covariates for PSM
AP_phys_cov <- c('female', 'firstgen', 'lowincomflag', 'englsr', 'hsgpa', 'mathsr', 'cohort_2015', 'cohort_2016', 'cohort_2017', 'cohort_2018', 'apscore')

# Estimate propensity score
phys.psm <- glm(skipped ~ female + englsr + mathsr + apscore,
                family = binomial(), data = df_phys2)

# Create dataframe with propensity scores
df_phys2_psm <- data.frame(pr_score = predict(phys.psm, type = "response"),
                           skipped = phys.psm$model$skipped)

# Matched dataset (no missing)
df_phys2_nomiss <- df_phys2 %>%  
  select(numgrade_2, apscore, skipped, one_of(AP_cov)) %>%
  na.omit()

phys_matched <- matchit(skipped ~ female + englsr + mathsr + apscore,
                        method = "nearest", data = df_phys2_nomiss)

df_phys2_matched <- match.data(phys_matched)

# Examine pre-matched and matched differences
df_phys2 %>%
  group_by(skipped) %>%
  select(one_of(AP_cov), apscore) %>%
  summarise_all(funs(mean(., na.rm = T)))

df_phys2_matched %>%
  group_by(skipped) %>%
  select(one_of(AP_cov), apscore) %>%
  summarise_all(funs(mean(., na.rm = T)))
