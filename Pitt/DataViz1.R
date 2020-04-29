
# Load libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse")   

# Load clean dataset
# df_clean <- read.csv("~/YOUR FILE PATH HERE.csv")
df_clean <- read.csv("~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/SEISMIC_AP_CLEAN.csv")

# Filter for student level inclusion/exclusion criteria
df_clean <- df_clean %>%
  # Include
  filter(transfer == 0) %>%
  filter(tookcourse_2 == 1) %>%
  filter(cohort >= 2013 & cohort <= 2018) %>%
  # Exclude
  filter(international == 0)

# Create subset dataframes for each analysis sample (for each discipline)

# Bio
# Took 2nd course in sequence
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013)

# Chem
# Took 2nd course in sequence
df_chem2 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2013)

# Phys
# Took 2nd course in sequence
df_phys2 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2013)


# For each Course
# BIO
# Generate estimates
fit_bio <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
              df_bio2, na.action=na.exclude)
# Add column of fitted values
df_viz_bio <- df_bio2 %>%
  mutate(numgrade_2.fitted = fitted(fit_bio))
  
#CHEM
# Generate estimates
fit_chem <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
              df_chem2, na.action=na.exclude)
# Add column of fitted values
df_viz_chem <- df_chem2 %>%
  mutate(numgrade_2.fitted = fitted(fit_chem))

#PHYSICS
# Generate estimates
fit_phys <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
              df_phys2, na.action=na.exclude)
# Add column of fitted values
df_viz_phys <- df_phys2 %>%
  mutate(numgrade_2.fitted = fitted(fit_phys))

#Combine Disciplines
df_viz <- bind_rows(df_viz_bio, df_viz_chem, df_viz_phys) %>%
  mutate(skipped_course = if_else(eligible_to_skip == 0 & skipped_course == 1, 
                                  as.numeric(NA), as.numeric(skipped_course)))

head(names(df_viz))

write.csv(df_viz, file = "SEISMIC_AP_VIZ.csv")

