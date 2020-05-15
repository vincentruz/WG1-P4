#### Missing Data Analysis ####

# Load cleaned dataset
df_clean <- read.csv("~/YOUR RECODED FILE PATH HERE.csv")

# Check for MCAR
missVar <- c("apscore", "hsgpa", "mathsr", "englsr", "enrl_from_cohort", "cohort", "numgrade", "numgrade_2")

# Full Dataset
fullLittle <- df_clean %>%
  select(missVar) %>%
  LittleMCAR(.)
print(fullLittle$p.value) # If p-value < .05, data is NOT missing completely at random (MCAR)

# By Discipline (if needed)
# Bio
bioLittle <- df_bio2 %>%
  select(missVar) %>%
  LittleMCAR(.)
print(bioLittle$p.value) # If p-value < .05, data is NOT missing completely at random (MCAR)
# Chem
chemLittle <- df_chem2 %>%
  select(missVar) %>%
  LittleMCAR(.)
print(chemLittle$p.value) # If p-value < .05, data is NOT missing completely at random (MCAR)
# Phys
physLittle <- df_phys2 %>%
  select(missVar) %>%
  LittleMCAR(.)
print(physLittle$p.value) # If p-value < .05, data is NOT missing completely at random (MCAR)

## Imputation (if needed) ####
modelVar <- c("st_id", "eligible_to_skip", "aptaker", "apscore", 
              "ethniccode_cat", "firstgen", "lowincomeflag", "hsgpa",
              "skipped_course", "mathsr", "englsr", "enrl_from_cohort", "cohort", "crs_term", "numgrade_2")
# Full Dataset
fullImp <- df_clean %>%
  select(modelVar) %>%
  mice(., m = 5) 

# By Discipline
# Bio
df_bio2Imp <- df_bio2 %>%
  select(modelVar) %>%
  mice(., m = 5) 
# Chem
df_chem2Imp<- df_chem2 %>%
  select(modelVar) %>%
  mice(., m = 5) 
# Phys
df_phys2Imp <- df_phys2 %>%
  select(modelVar) %>%
  mice(., m = 5) 