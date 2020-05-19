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
# Bio (complete info, after 2013)
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013) 

# Chem (complete info, after 2014)
df_chem2 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014)

# Phys (complete info, after 2015)
# Took 2nd course in sequence
df_phys2 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015) 

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

#### Matching (using Weights) ####
## Bio 
# Data with no missing
df_bio2_comp <- df_bio2 %>%  
  select(numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + ethniccode_cat +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
        data = df_bio2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
bio.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + ethniccode_cat +
                      scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
                    data = df_bio2_comp, estimand = "ATT")
summary(bio.out) 

# Check balance after weighting
bal.tab(bio.out, m.threshold = .05, disp.v.ratio = TRUE)

# Extract weights
bio.w <- svydesign(ids = ~1, weights = bio.out$weights,
                   data = df_bio2_comp)

## Chem Weighting 
# Data with no missing
df_gchem2_comp <- df_chem2 %>%  
  select(numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, ethniccode_cat,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
        data = df_gchem2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
chem.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + urm +
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
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
  select(numgrade_2, apscore_full, eligible_to_skip, skipped_course, aptaker,
         firstgen, lowincomeflag, female, urm, cohort,
         hsgpa, mathsr, englsr, cohort, enrl_from_cohort_2, crs_term_2) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(skipped_course ~ + firstgen + factor(lowincomeflag) + female + urm +
          scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
        data = df_phys2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
phys.out <- weightit(skipped_course ~ firstgen + factor(lowincomeflag) + female + urm +
                       scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
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
df_bio_skeligible.3 <- bio.w %>%
  subset(apscore_full == 3)
df_bio_skeligible.4 <- bio.w %>%
  subset(apscore_full == 4)
df_bio_skeligible.5 <- bio.w %>%
  subset(apscore_full == 5)

# Chem
# Took AP 
df_chem_aptakers <- chem.w %>%
  subset(aptaker == 1)
# Skip eligible
df_chem_skeligible <- chem.w %>%
  subset(eligible_to_skip == 1)
# Skip eligible (at each score)
df_chem_skeligible.3 <- chem.w %>%
  subset(apscore_full == 3)
df_chem_skeligible.4 <- chem.w %>%
  subset(apscore_full == 4)
df_chem_skeligible.5 <- chem.w %>%
  subset(apscore_full == 5)

# Phys
# Took AP 
df_phys_aptakers <- phys.w %>%
  subset(aptaker == 1)
# Skip eligible
df_phys_skeligible <- phys.w %>%
  subset(eligible_to_skip == 1)
# Skip eligible (at each score)
df_phys_skeligible.3 <- phys.w %>%
  subset(apscore_full == 3)
df_phys_skeligible.4 <- phys.w %>%
  subset(apscore_full == 4)
df_phys_skeligible.5 <- phys.w %>%
  subset(apscore_full == 5)

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
bio_rq3a <- lm(scale(numgrade_2) ~ factor(skipped_course)*factor(apscore), 
               data=df_bio2)
summary(bio_rq3a)
#std_beta(bio_rq3a)

# RQ3B - In a matched sample, what was the effect of skipping on 2nd course grade?
# (Formerly 2b1.)
bio_rq3a.3 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                   factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                   scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
                 data=df_bio2)
summary(bio_rq3a.3)
#std_beta(bio_rq3a.3)

# RQ3B (When Score  = 3)
bio_rq3a.3 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio2)
summary(bio_rq3a.3)
#std_beta(bio_rq3a.3)

# RQ3B (When Score  = 4)
bio_rq3a.4 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_skeligible.4)
summary(bio_rq3a.4)
#std_beta(bio_rq3a.4)

# RQ3B (When Score  = 5)
bio_rq3a.5 <- lm(scale(numgrade_2) ~ factor(skipped_course) + scale(apscore) + 
                 factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) + 
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               data=df_bio_skeligible.5)
summary(bio_rq3a.5)
#std_beta(bio_rq3a.5)


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

