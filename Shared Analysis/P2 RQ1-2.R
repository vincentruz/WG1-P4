# SEISMIC WG1-P4
# AP Project - Shared Analysis File

# Inclusion criteria: https://docs.google.com/spreadsheets/d/1SzU4PcIEUsAGnKKyAcugHO2O2aZW29sf9a_cC-FAElk/edit#gid=1702351745

# Load libraries
install.packages("pacman")
pacman::p_load("tidyverse", "dplyr", "data.table", "psych") # data wrangling/descriptive stats

# Need sjstats for reporting robust SE
# In R Studio you can just do install.packages("sjstats")
# library(sjstats)

# Load renamed dataset
apfull_SkipEl <- read.csv("/Users/thicn/Documents/AP Skip/apfull_SkipEl.csv")

names(apfull_SkipEl)
head(apfull_SkipEl)

# Notes: variable names all follow convention, except for hsgpa_z = scale(hsgpa)

table(apfull_SkipEl$apyear_Chem)

## Start with CHEM
# Create a dataset with unique student ID for those who took the second Chem course
# Here I retain those who didn't take the AP or only took it from 2014 onward.
chemap_2 <- subset(apfull_SkipEl, class_number=="0001B" & crs_name=="GENERAL CHEMISTRY" &
                     apyear_Chem%in%c("0", "2014", "2015", "2016", "2017"))
dim(chemap_2)
table(chemap_2$apyear_Chem)

chemap_unique <- chemap_2[!duplicated(chemap_2$st_id),]
dim(chemap_unique)

table(chemap_unique$aprealskipper_GChem1, chemap_unique$apscore_Chem)

# Recode variables
chemap_unique$aptaker_Chem <- as.factor(chemap_unique$aptaker_Chem)
table(chemap_unique$aptaker_Chem)

chemap_unique <- subset(chemap_unique, crs_term!="2013")
table(chemap_unique$crs_term)
class(chemap_unique$crs_term)
chemap_unique$crs_term <- as.factor(chemap_unique$crs_term)

# RQ1. 1a: Took AP, full sample
tookchem <- glm(aptaker_Chem~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                  hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=chemap_unique, binomial(link="logit"))
summary(tookchem)
#robustse(tookchem, coef="odd.ratio") #robustse is a command from sjstats package

# 1b. AP score for those who took AP
# Recode numeric variable to numeric
chemap_unique$apscore_Chem <- as.numeric(as.character(chemap_unique$apscore_Chem)) 
# Subset for those who took AP
chem_tookap <- subset(chemap_unique, aptaker_Chem!=0)
dim(chem_tookap)
table(chem_tookap$apscore_Chem)

# Logit model
chemsr <- lm(apscore_Chem~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
               hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=chem_tookap)
summary(chemsr)
#std_beta(chemsr) # from sjstats package

# 1c.i. Eligible out of those who took AP
table(chem_tookap$apskipper_GChem1)
skipchem <- glm(apskipper_GChem1~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                  hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
                data=chem_tookap, 
                binomial(link="logit"))
summary(skipchem)
#robustse(skipchem, coef="odd.ratio")

# 1c.ii. Eligible out of full sample
skipchem2 <- glm(apskipper_GChem1~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                   hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
                 data=chemap_unique, 
                 binomial(link="logit"))
summary(skipchem2)
#robustse(skipchem2, coef="odd.ratio")

# 2a. Took AP AND skip eligble and actually skip
table(chem_tookap$aprealskipper_GChem1)
# Make skip eligible data subset
skipel <- subset(chem_tookap, apskipper_GChem1==1)
dim(skipel)

skipreal <- glm(aprealskipper_GChem1~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                  hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
                data=skipel, 
                binomial(link="logit"))
summary(skipreal)
#robustse(skipreal, coef="odd.ratio")

# 2b. Took AP, include AP score
chemsr5 <- lm(numgrade~aprealskipper_GChem1 + apscore_Chem +
                factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) + 
                hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=chem_tookap)
summary(chemsr5)

#std_beta(chemsr5)

# 2c. Full sample, include AP score
chemsr6 <- lm(numgrade~aprealskipper_GChem1 + apscore_Chem +
                factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) + 
                hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=chemap_unique)
summary(chemsr6)

#std_beta(chemsr6)

# 2d. 2nd course grade; took AP sample for those who actually skip
chemsr <- lm(numgrade~aprealskipper_GChem1 + 
               factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) + 
               hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
             data=chem_tookap)
summary(chemsr)

#std_beta(chemsr)

#2e. 2nd course grade; everyone, actually skipped
#chemap_unique$numgrade <- as.numeric(as.character(chemap_unique$numgrade))
chemsr2 <- lm(numgrade ~ aprealskipper_GChem1 + factor(firstgen) + 
                factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) + 
                hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=chemap_unique)
summary(chemsr2)

#std_beta(chemsr2)

#2f. Course grade, Skip eligible for those who took AP
chemsr3 <- lm(numgrade~apskipper_GChem1 + factor(firstgen) + 
                factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) + 
                hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=chem_tookap)
summary(chemsr3)

#std_beta(chemsr3)

#2g. Skip eligible, full sample
chemsr4<- lm(numgrade~apskipper_GChem1 + factor(firstgen) + 
               factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) + 
               hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=chemap_unique)
summary(chemsr4)

#std_beta(chemsr4)

# Bring back the var names so I don't have to scroll up ...
names(apfull_SkipEl)

## PHYSICS
#table(apfull_SkipEl$apyear_Mec) # NA not coded
apfull_SkipEl$apyear_Mec <- ifelse(is.na(apfull_SkipEl$apyear_Mec), 0, apfull_SkipEl$apyear_Mec)
apfull_SkipEl$apyear_Elec <- ifelse(is.na(apfull_SkipEl$apyear_Elec), 0, apfull_SkipEl$apyear_Elec)

table(apfull_SkipEl$apyear_Mec)
table(apfull_SkipEl$apyear_Elec)

## PHYSICS
dim(apfull_SkipEl)
physicsap <- subset(apfull_SkipEl, class_number=="0007D" & crs_name=="CLASSICAL PHYSICS" & 
                      apyear_Mec%in%c("0", "2015", "2016", "2017") | 
                      class_number=="0007D" & crs_name=="CLASSICAL PHYSICS" & 
                      apyear_Elec%in%c("0", "2015", "2016", "2017"))
physicsap <- subset(physicsap, crs_term%in%c("2015", "2016", "2017"))
dim(physicsap)

physicsap_unique <- physicsap[!duplicated(physicsap$st_id),]
dim(physicsap_unique)

table(physicsap_unique$aprealskipper_Phys1, physicsap_unique$apscore_Mec)
table(physicsap_unique$aprealskipper_Phys1, physicsap_unique$apscore_Elec)

# More data processing
# Create a dummy var for taking physics (either Mec or Elec)
physicsap_unique$aptaker_Phys <- ifelse(physicsap_unique$aptaker_Elec==1|
                                          physicsap_unique$aptaker_Mec==1, 1, 0)

# Create a score for physics (as the best score from either Mec or Elec)
# Replace NA with 0
physicsap_unique$apscore_Elec[is.na(physicsap_unique$apscore_Elec)] <- 0
physicsap_unique$apscore_Mec[is.na(physicsap_unique$apscore_Mec)] <- 0
# Choose best score from either elec or mec: first replace with one, i.e., Mec score, and compare with Elec score; if higher then retain
physicsap_unique$apscore_Phys <- physicsap_unique$apscore_Mec

physicsap_unique$apscore_Phys <- ifelse(physicsap_unique$apscore_Elec > physicsap_unique$apscore_Phys, 
                                        physicsap_unique$apscore_Elec, physicsap_unique$apscore_Phys)
table(physicsap_unique$apscore_Phys, physicsap_unique$apscore_Mec) # double checking
table(physicsap_unique$apscore_Phys, physicsap_unique$aprealskipper_Phys1) # real skippers in those that score 4/5 on either test

#1a. Took AP, full sample
tookphysap <- glm(aptaker_Phys~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                    hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
                  data=physicsap_unique, binomial(link="logit"))
summary(tookphysap)
#robustse(tookphysap, coef="odd.ratio")

# 1b: AP Score for those who took AP
phys_tookap <- subset(physicsap_unique, apscore_Phys!=0)
dim(phys_tookap) 
table(phys_tookap$apscore_Phys)

physsr <- lm(apscore_Phys~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
               hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=phys_tookap)
summary(physsr)
#std_beta(physsr)

# 1c.i Eligible out of those who took AP
skipphys <- glm(apskipper_Phys1~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                  hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
                data=phys_tookap, 
                binomial(link="logit"))
summary(skipphys)
#robustse(skipphys, coef="odd.ratio")

# 1c.ii Eligible out of full sample
skipphys2 <- glm(apskipper_Phys1~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                   hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
                 data=physicsap_unique, 
                 binomial(link="logit"))
summary(skipphys2)
#robustse(skipphys2, coef="odd.ratio")

# 2a. Took AP and skip eligble and actually skip
table(phys_tookap$aprealskipper_Phys1)
skipel <- subset(phys_tookap, apskipper_Phys1==1)
dim(skipel)
table(skipel$aprealskipper_Phys1)

skipreal <- glm(aprealskipper_Phys1~factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                  hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
                data=skipel, 
                binomial(link="logit"))
summary(skipreal) # very small dataset in UCI case | not reliable coefficients.
#robustse(skipreal, coef="odd.ratio")

# 2b. Took AP, include AP score
physsr5 <- lm(numgrade~aprealskipper_Phys1 + apscore_Phys + 
                factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
              data=phys_tookap)
summary(physsr5)

#std_beta(physsr5)

# 2c. Full sample, include AP score
physsr6 <- lm(numgrade~aprealskipper_Phys1 + apscore_Phys + 
                factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=physicsap_unique)
summary(physsr6)
#std_beta(physsr6)

# 2d. 2nd course grade; took AP sample for those who actually skip
physsr <- lm(numgrade~aprealskipper_Phys1 + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
               hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), 
             data=phys_tookap)
summary(physsr)

#std_beta(physsr)

#2e. 2nd course grade; everyone, actually skipped
physsr2 <- lm(numgrade~aprealskipper_Phys1 + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=physicsap_unique)
summary(physsr2)

#std_beta(physsr2)

#2f. Skip eligible for those who took AP
physsrsr3 <- lm(numgrade~apskipper_Phys1 + 
                  factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
                  hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=phys_tookap)
summary(physsrsr3)

#std_beta(physsrsr3)

#2g. Skip eligible, full sample
physsr4<- lm(numgrade~apskipper_Phys1 + 
               factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat2) +
               hsgpa_z + scale(mathsr) + scale(englsr) + factor(crs_term), data=physicsap_unique)
summary(physsr4)

#std_beta(physsr4)

