
# Load libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse")             # Reporting Odds Ratio and Std. Betas

# Load clean dataset
df_clean <- read.csv("~/YOUR FILE PATH HERE.csv")
df_clean <- read.csv("~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/SEISMIC_AP_CLEAN.csv")

# For each Course
# BIO
# Generate estimates
fit_bio <- lm(scale(numgrade) ~ scale(apscore) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term), 
              df_bio2, na.action=na.exclude)
# Add column of fitted values
df_bio2 <- df_bio2 %>%
  cbind(fitted = fitted(fit_bio))
  
#CHEM
# Generate estimates
fit_chem <- lm(scale(numgrade) ~ scale(apscore) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term), 
              df_chem2, na.action=na.exclude)
# Add column of fitted values
df_chem2 <- df_chem2 %>%
  cbind(fitted = fitted(fit_chem))

#PHYSICS
# Generate estimates
fit_phys <- lm(scale(numgrade) ~ scale(apscore) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term), 
              df_phys2, na.action=na.exclude)
# Add column of fitted values
df_phys2 <- df_phys2 %>%
  cbind(fitted = fitted(fit_phys))

#Combine Disciplines
df_viz <- bind_rows(df_bio2, df_chem2, df_phys2)

head(df_viz)
write.csv(df_viz, file = "SEISMIC_AP_VIZ.csv")

