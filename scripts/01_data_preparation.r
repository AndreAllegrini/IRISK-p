
  
# Load necessary libraries
library(stats)
library(psych)
library(lavaan)
library(tidyverse)
library(foreign)
library(data.table)

# Filepath to MoBa data
# Read the SPSS data file and select relevant columns
dat8yrv12 <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q8yrs_v12.sav", to.data.frame = TRUE) %>%
  select(
    PREG_ID_2306,
    BARN_NR,
    # SMFQ columns
    paste0(rep("NN", 9),seq(68,80,1)),
    # SCARED columns
    paste0(rep("NN", 5),seq(145,149,1)),
    # Disruptive behavior past 12 months/last year CND columns
    paste0(rep("NN", 8),seq(111,118,1)),
    # Disruptive behavior past 12 months/last year INAT columns
    paste0(rep("NN", 9),seq(119,127,1)),
    # Disruptive behavior past 12 months/last year HYP columns
    paste0(rep("NN", 9),seq(128,136,1)),
    # Disruptive behavior past 12 months/last year ODD columns
    paste0(rep("NN", 9),seq(137,144,1)),
    # Age return months
    AGE_RETURN_MTHS_Q8AAR
  ) %>%
  # Add a new column indicating the data version
  mutate(q8yrDATA=1)

# Display dimensions and the first few rows of the dataset
dim(dat8yrv12)
head(dat8yrv12)

# Define columns for each category of behavior
hyp <- c("NN128","NN129","NN130","NN131","NN132","NN133","NN134","NN135","NN136")
inat <- c("NN119","NN120","NN121","NN122","NN123","NN124","NN125","NN126","NN127")
odd <- c("NN137","NN138","NN139","NN140","NN141","NN142","NN143","NN144")
cnd <- c("NN111","NN112","NN113","NN114","NN115","NN116","NN117","NN118")
anx <- c("NN145","NN146","NN147","NN148","NN149")
mfq <- c("NN68","NN69","NN70","NN71","NN72","NN73","NN74","NN75","NN76","NN77","NN78","NN79","NN80")

# Check levels for different behavior columns
sapply(dat8yrv12[,c(odd,hyp,inat)],levels)
sapply(dat8yrv12[,c(cnd)],levels) 
sapply(dat8yrv12[,c(mfq,anx)],levels)

# Recode levels for disruptive behavior in the past 12 months columns
dat8yrv12 <- dat8yrv12 %>% 
  mutate_at(c(cnd), 
            function(x) case_when(
              x == "Never" ~ 0,
              x == "Seldom" ~ 1,
              x == "Sometimes" ~ 2,
              x == "Often" ~ 3
            ))

dat8yrv12 <- dat8yrv12 %>% 
  mutate_at(c(odd,hyp,inat), 
            function(x) case_when(
              x == "Never / seldom" ~ 0,
              x == "Sometimes" ~ 1,
              x == "Often" ~ 2,
              x == "Very often" ~ 3 
            ))

# Recode levels for mfq and anx columns
dat8yrv12 <- dat8yrv12 %>% 
  mutate_at(c(mfq,anx), 
            function(x) case_when(
              x == "Disagree" ~ 0,
              x == "Is not or nearly not correct" ~ 0,
              x ==  "Sometimes correct" ~ 1,
              x ==  "Often true" ~ 2,
              x == "Correct" ~ 2
            ))

# Display structure of selected columns
str(dat8yrv12[,c(odd,cnd,inat,hyp,anx,mfq)])

# Rename columns for clarity
names(dat8yrv12)[c(which(colnames(dat8yrv12) %in% odd))] <- sub("NN","odd",names(dat8yrv12[,odd]))
names(dat8yrv12)[c(which(colnames(dat8yrv12) %in% cnd))] <- sub("NN","cnd",names(dat8yrv12[,cnd]))
names(dat8yrv12)[c(which(colnames(dat8yrv12) %in% hyp))] <- sub("NN","hyp",names(dat8yrv12[,hyp]))
names(dat8yrv12)[c(which(colnames(dat8yrv12) %in% inat))] <- sub("NN","inat",names(dat8yrv12[,inat]))
names(dat8yrv12)[c(which(colnames(dat8yrv12) %in% anx))] <- sub("NN","anx",names(dat8yrv12[,anx]))
names(dat8yrv12)[c(which(colnames(dat8yrv12) %in% mfq))] <- sub("NN","mfq",names(dat8yrv12[,mfq]))

# Save column names for later use
hyp <- dat8yrv12 %>% select(starts_with('hyp')) %>% names()
inat <-dat8yrv12 %>% select(starts_with('inat')) %>% names()
odd <- dat8yrv12 %>% select(starts_with('odd')) %>% names()
cnd <- dat8yrv12 %>% select(starts_with('cnd')) %>% names()
anx <- dat8yrv12 %>% select(starts_with('anx')) %>% names()
mfq <- dat8yrv12 %>% select(starts_with('mfq')) %>% names()

# Save processed data and column names
save(dat8yrv12,hyp,inat,odd,cnd,anx,mfq, file = paste0("../data/dat8y_pfitems",format(Sys.time(), '%d%B%Y'),".RData"))

# To load saved data uncomment the following line
#load(paste0("../data/dat8y_pfitems",format(Sys.time(), '%d%B%Y'),".RData"))

 

## IDs and covariates

# Read in the MoBaPsychGen data 
ids <- read.table("/durable/data/genetic/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov.txt",
                  header = T, 
                  sep = "\t") %>%

    # Use dplyr to add new columns based on the 'Role' column.
    dplyr::mutate(PREG_ID_2306_BARN_NR = ifelse(Role == "Child", ID_2306, NA), 
                  F_ID_2306 = ifelse(Role == "Father", ID_2306, NA),
                  M_ID_2306 = ifelse(Role == "Mother", ID_2306, NA)) %>%
    
    #  split the 'PREG_ID_2306_BARN_NR' column into two separate columns 'PREG_ID_2306' and 'BARN_NR'
    tidyr::separate(PREG_ID_2306_BARN_NR, 
                    into = c("PREG_ID_2306", "BARN_NR"),  
                    sep = "_")
 

## Load, merge and residualize polygenic scores for relevant covariates 
  
# Define directories and stubs
smstDir <- "../LDpred2/out/scores_analyses/"
stubAuto <- '.gz_pred_auto.txt' 
#stubInf <- '.txt.gz_refUKB_pred_inf.txt'

#here I'm specifying the stub of the polygenic scores  files for later use, this can be useful if you have generated PGS with different approaches for example I've generated LDpred2-auto scores and infinitesimal scores
## scores were generated using LDred2-auto and infinitesimal options
## https://github.com/AndreAllegrini/LDpred2


# List of PRS names
PRSnames <- c('ADHD_Demontis_ieua1183','anorexia_PGC2019','Anxiety_Purves2019_TotAnx_effect','Autism_Grove_ieua1185', 'Bipolar_PGC2021','DEP_Howard2019_ieub102','PTSD_PGC019_EUR','SCZ_PGC2022_EUR_GWAS', 'UKB_HairColourRed_ukbd1747_2', 'neuroSum_Nagel2018', 'insomnia_Watanabe_2022','ChronicPain_Johnston_2019')

# Function to retrieve PRS
getPRS <- function(smstDir, PRSnames, stub){
    
    # Read PRS datasets
    PRSdat <- lapply(paste0(smstDir, PRSnames, stub), function(x) {read.table(x, header = T)})
    
    # Rename PRS column based on the trait name
    for(i in 1:length(PRSnames)) {
        nameOfTrait <- PRSnames[i]
        if("final_pred_auto" %in% names(PRSdat[[i]])) {
            names(PRSdat[[i]])[which(colnames(PRSdat[[i]]) == "final_pred_auto")] <- paste0(nameOfTrait, '_auto')
        } else {
            names(PRSdat[[i]])[which(colnames(PRSdat[[i]]) == "pred_inf")] <- paste0(nameOfTrait, '_inf')
        }
    }
    
    # Merge all datasets
    PRSdat2 <- Reduce(function(x, y) {merge(x, y)}, PRSdat)
    names(PRSdat2)[2] <- "IID"
    return(PRSdat2)
}

# Retrieve auto PRS
PRSauto <- getPRS(smstDir=smstDir, PRSnames=PRSnames, stub=stubAuto)

# Merge dataframes
dat <- plyr::join_all(list(ids, PRSauto[,c(paste0(c("IID", paste0(PRSnames, '_auto'))))]), type = 'left', by = 'IID')

# Define outcomes and covariates
outcomes <- c(paste0(PRSnames, '_auto'))
covariates = c("genotyping_center_num", "genotyping_chip_num", "genotyping_batch_num", "imputation_batch_num", paste0("PC", 1:20), "SEX", "YOB")

# Residualize outcomes for covariates
for(j in outcomes) {
    dat[,j] <- scale(dat[,j])#stnadradize PGS first
    dat[,paste0('r_', j)] <- resid(lm(as.formula(paste(j, paste(c(covariates), collapse=" + "), sep = " ~ ")), data = dat, na.action=na.exclude))
}

# Display the head and column names of the data
#head(dat)
#names(dat)

# Check a few correlations

dat %>% select(r_ADHD_Demontis_ieua1183_auto, PC1) %>% cor(., use = 'p')
dat %>% select(r_ADHD_Demontis_ieua1183_auto, SEX) %>% cor(., use = 'p')
dat %>% select(ADHD_Demontis_ieua1183_auto, SEX) %>% cor(., use = 'p')
dat %>% select(r_ADHD_Demontis_ieua1183_auto, YOB) %>% cor(., use = 'p')
dat %>% select(ADHD_Demontis_ieua1183_auto, YOB) %>% cor(., use = 'p')

# Save the cleaned data as an RDS file
saveRDS(dat, paste0("../data/LDpred2PRS_clean", format(Sys.time(), '%d%B%Y'), ".rds"))
prsdata <- dat #store with different name
 

## Subset PGS data based on Role, then merge again to get long format dataframe
  
dim(dat)

# Subset data for offspring.
# Filter data to include only rows where Role is "Child", and select the specified columns.
# Then rename the columns that start with 'r_' by appending "_Child" to their names.
prs_offspring = dat %>% 
  filter(Role=="Child") %>%
  select(IID, FID, PREG_ID_2306, BARN_NR, SEX, YOB, starts_with('r_')) %>% 
  rename_with(~paste0(., "_Child"), starts_with('r_'))

# Display dimensions to check the result.
dim(prs_offspring)

# Do similar subsets for "Mother" and "Father" roles.
prs_mother = dat %>%   
  filter(Role=="Mother") %>%
  select(IID, FID, M_ID_2306, starts_with('r_')) %>% 
  rename_with(~paste0(., "_Mother"), starts_with('r_'))

dim(prs_mother)

prs_father = dat %>%
  filter(Role=="Father") %>%
  select(IID, FID, F_ID_2306, starts_with('r_')) %>%
  rename_with(~paste0(., "_Father"), starts_with('r_'))

dim(prs_father)

# Read the ID linking file.
link_preg <- foreign::read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_v12/SPSS/PDB2306_SV_INFO_v12.sav", to.data.frame=T, colnames = T)

dim(link_preg)

# Extract data linking pregnancies to mother and father IDs.
link_mother <- link_preg %>% select(PREG_ID_2306, M_ID_2306)
link_father <- link_preg %>% select(PREG_ID_2306, F_ID_2306)

# Check the dimensions 
dim(link_mother)
dim(link_father)

# Check the number of unique mother and father IDs.
length(unique(link_mother$M_ID_2306))
length(unique(link_father$F_ID_2306))

# Merge the previously subsetted PRS data with the linking data to associate mothers and fathers with pregnancies.
prs_mother2 <- left_join(prs_mother, link_mother, by = "M_ID_2306")
prs_father2 <- left_join(prs_father, link_father, by = "F_ID_2306")

# Check dimensions 
dim(prs_mother2)
dim(prs_father2)

# Check the number of unique values 
length(unique(prs_mother2$PREG_ID_2306))
length(unique(prs_mother2$M_ID_2306))
length(unique(prs_mother2$FID))

# And for the father data.
length(unique(prs_father2$PREG_ID_2306))
length(unique(prs_father2$F_ID_2306))
length(unique(prs_father2$FID))

# And for the offspring data.
length(unique(prs_offspring$PREG_ID_2306))
length(unique(prs_offspring$FID))

# Merge the data to get a combined dataset with offspring, mother, and father data.
processed_prs2 <- merge(prs_offspring, prs_mother2, by = c("PREG_ID_2306"), suffixes = c("_child", "_mother"), all.x = T, all.y = T)
processed_prs2 <- merge(processed_prs2, prs_father2, by = c("PREG_ID_2306"), suffixes = c("_father"), all.x = T, all.y = T)
processed_prs2 <- processed_prs2 %>% rename(FID_father = FID, IID_father=IID)

# Filter to keep only rows where all three (child, mother, father) have non-missing values
prs_comp_trios <- processed_prs2[complete.cases(processed_prs2[ , c("r_ADHD_Demontis_ieua1183_auto_Child", "r_ADHD_Demontis_ieua1183_auto_Mother", "r_ADHD_Demontis_ieua1183_auto_Father")]),]

# number of complete trios
print(paste0("there are ", dim(prs_comp_trios)[1], " complete trios with genetic data"))

# Check the number of unique values for several columns post-merge.
length(unique(prs_comp_trios$FID_child))
length(unique(prs_comp_trios$FID_mother))
length(unique(prs_comp_trios$FID_father))
length(unique(prs_comp_trios$PREG_ID_2306))
length(unique(prs_comp_trios$M_ID_2306))
length(unique(prs_comp_trios$F_ID_2306))

# Store the processed data.
saveRDS(prs_comp_trios, file=paste0("../data/200k_prs_comp_trios_",format(Sys.time(), '%d%B%Y'),".rds"))

# Data type conversion for merging.
dat8yrv12$BARN_NR <- as.character(dat8yrv12$BARN_NR)
dat8yrv12$PREG_ID_2306 <- as.character(dat8yrv12$PREG_ID_2306)

# Check the length post-conversion.
length(dat8yrv12$PREG_ID_2306)

# Merge the prs_comp_trios with dat8yrv12 data.
df <- left_join(prs_comp_trios, dat8yrv12, by = c('PREG_ID_2306','BARN_NR'))

# Check dimensions after omitting NA values.
dim(na.omit(df))

# Reassign the merged datafram (bkp).
dat <- df

# Check final dimensions.
dim(dat)

# Store the merged data.
saveRDS(dat, file=paste0("../data/200k_phenoPlusPRS_",format(Sys.time(), '%d%B%Y'),".rds"))
 

# Keep only unrelated family trios, one sibling per family prioritizing on phenotype data
  
# copy / bkp data for processing
prs_comp_trios <- dat

# Calculate the mean phenotype for each row (individual) over the specific columns
prs_comp_trios$tmp <- rowMeans(prs_comp_trios[,c(odd,cnd,inat,hyp,anx,mfq)], na.rm=F)

# Count non-missing individuals based on the mean phenotype
sum(!is.na(prs_comp_trios$tmp))

# Create an index indicating whether an individual has at least one non-missing observation across phenotypes
prs_comp_trios$index_variable <- ifelse(rowSums(!is.na(prs_comp_trios[,c(odd,cnd,inat,hyp,anx,mfq)])) >= 1, 1, 0)

# Count individuals based on the new index
sum(prs_comp_trios$index_variable)

# Extract the IDs where phenotype data is available across measures
pheno_ids <- prs_comp_trios %>%
  select(PREG_ID_2306, tmp) %>% 
  subset(!is.na(tmp))

# Create a column indicating if phenotype data for the child is available
prs_comp_trios$trio_pheno <- ifelse(prs_comp_trios$PREG_ID_2306 %in% pheno_ids$PREG_ID_2306, 1, NA)

# Display counts based on the index_variable
table(prs_comp_trios$index_variable)

# Display counts for trio_pheno
table(prs_comp_trios$trio_pheno, useNA = "ifany")

# Sort data based on PREG_ID_2306, and retain the first distinct PREG_ID_2306
sort_trio_data <- prs_comp_trios %>% 
  arrange(PREG_ID_2306, trio_pheno) %>% 
  distinct(PREG_ID_2306, .keep_all = T)

# Sort data based on M_ID_2306 (Mother ID), and retain the first distinct M_ID_2306
sort_trio_data <- sort_trio_data %>% 
  arrange(M_ID_2306, trio_pheno) %>% 
  distinct(M_ID_2306, .keep_all = T)

# Sort data based on F_ID_2306 (Father ID), and retain the first distinct F_ID_2306
sort_trio_data <- sort_trio_data %>% 
  arrange(F_ID_2306, trio_pheno) %>% 
  distinct(F_ID_2306, .keep_all = T)

# Display the number of unique combinations of PREG_ID_2306 and BARN_NR
length(unique(paste0(sort_trio_data$PREG_ID_2306,sort_trio_data$BARN_NR)))

# Display dimensions of the sorted data
dim(sort_trio_data)

# Display number of unique IDs for child, mother, and father
length(unique(sort_trio_data$PREG_ID_2306))
length(unique(sort_trio_data$M_ID_2306))
length(unique(sort_trio_data$F_ID_2306))
length(unique(sort_trio_data$IID_child))

# Display number of unique family IDs for child, mother, and father
length(unique(sort_trio_data$FID_child))
length(unique(sort_trio_data$FID_mother))
length(unique(sort_trio_data$FID_father))

# Show table of phenotype data availability after sorting
table(sort_trio_data$trio_pheno, useNA = "ifany")

# Further filtering based on Family ID for child, mother, and father
sort_trio_data2 <- sort_trio_data %>% 
  arrange(FID_child, trio_pheno) %>% 
  distinct(FID_child, .keep_all = T)

sort_trio_data2 <- sort_trio_data2 %>% 
  arrange(FID_mother, trio_pheno) %>% 
  distinct(FID_mother, .keep_all = T)

sort_trio_data2 <- sort_trio_data2 %>% 
  arrange(FID_father, trio_pheno) %>% 
  distinct(FID_father, .keep_all = T)

# Show table of phenotype data availability after further filtering
table(sort_trio_data2$trio_pheno, useNA = "ifany")

# Display dimensions of the further filtered data
dim(sort_trio_data2)

# Display unique counts after further filtering
length(unique(paste0(sort_trio_data2$PREG_ID_2306,sort_trio_data2$BARN_NR)))
dim(sort_trio_data2)
length(unique(sort_trio_data2$IID_child))
dim(sort_trio_data2)

# Save the final filtered data as an RDS file
saveRDS(sort_trio_data2, file=paste0("../data/200k_phenoPlusPRS_strictUnrelated_", format(Sys.time(), '%d%B%Y'), ".rds"))

# Compute the number of complete trios 
index <- ifelse(rowSums(!is.na(sort_trio_data2[,c(odd,cnd,inat,hyp,anx,mfq)])) >= 1, 1, 0)
sum(index)
length(sort_trio_data2[,'r_neuroSum_Nagel2018_auto_Child'][index])
dim(na.omit(sort_trio_data2[,c(mfq,odd,hyp,cnd,inat,anx,'r_neuroSum_Nagel2018_auto_Child')]))
 

### Principal component analysis of polygenic scores

#In this chunk, Principal Component Analysis (PCA) is conducted on several psychiatric-related Polygenic Scores (PGS) 
#for different roles (Child, Mother, Father) in the dataset. 
#The first principal component (PC1) from each PCA is appended back to the original dataset, and the direction of PC1 is checked against an ADHD PRS to ensure it aligns with the original direction. 
#The processed dataset with PCA results is then saved.


# Load required lavaan library
library(lavaan)

# Read in the data file with the specific format
dat <- readRDS(paste0("../data/200k_phenoPlusPRS_strictUnrelated_", format(Sys.time(), '%d%B%Y'), ".rds"))

# Define the roles - Child, Mother, Father
roles = c('_Child', '_Mother', '_Father')

# Define the names of the PRS variables
PRSnames <- c('ADHD_Demontis_ieua1183', 'anorexia_PGC2019', 'Anxiety_Purves2019_TotAnx_effect', 'Autism_Grove_ieua1185', 'Bipolar_PGC2021', 'DEP_Howard2019_ieub102', 'PTSD_PGC019_EUR', 'SCZ_PGC2022_EUR_GWAS', 'neuroSum_Nagel2018', 'insomnia_Watanabe_2022', 'ChronicPain_Johnston_2019')

# Append 'r_' and '_auto' to PRS names
LdAuto <- paste0('r_', PRSnames, '_auto')

# Reduced version: Exclude insomnia, neuro, and chronic pain to keep only psychiatric diagnoses
LdAutoRed <- paste0('r_', PRSnames[-c(9,10,11)], '_auto')

# Initialize lists to store PCA loadings and explained variance
loadingsPca <- list()
r2pca <- list()     

# Create a list of PRS type names
list_psych <- list(LdAuto, LdAutoRed)
names(list_psych) <- c("LdAuto", "LdAutoRed")

# Loop through each role
for(i in roles) {
    # Loop through each PRS type name
    for(prsType in names(list_psych)) {
        # Select appropriate PRS based on current role and PRS type
        list_psychR <- paste0(list_psych[prsType][[1]], i)
        
        # Perform PCA on the selected PRS
        pca <- prcomp(~ ., data = select(dat, list_psychR), scale = TRUE, center= TRUE)
        
        # Store PCA loadings and explained variance in respective lists
        loadingsPca[[paste0('PSYCH_', prsType, i)]] <- pca$rotation 
        r2pca[[paste0('PSYCH_', prsType, i)]] <- summary(pca)
        
        # Append the first principal component (PC1) from PCA result to the original data
        dat[, paste0('PSYCH_', prsType, i)] <- as.vector(scale(pca$x[, 1]))
        
        # Ensure PC1 direction aligns with the direction of the original PRS (using ADHD as reference)
        # If the correlation between the computed PC1 and the original PRS is negative, reverse the direction of PC1
        if((dat %>% 
            select(paste0('PSYCH_', prsType, i), paste0('r_ADHD_Demontis_ieua1183_auto', i)) %>% 
            cor(use = 'p'))[2] < 0) {
            dat[paste0('PSYCH_', prsType, i)] <- dat[paste0('PSYCH_', prsType, i)] * -1
        }
    }
}

# Save the updated dataset with PCA results 
saveRDS(dat, file = paste0("../data/200k_phenoPlusPRS_cleanedTrios_", format(Sys.time(), '%d%B%Y'), "_unrel_pca.rds"))

saveRDS(loadingsPca, file = paste0("../output/loadingsPca_PGS_", format(Sys.time(), '%d%B%Y'), ".rds"))
saveRDS(r2pca, file = paste0("../output/r2pca_PGS_", format(Sys.time(), '%d%B%Y'), "_unrel_pca.rds"))

r2pca

# Save list polygenic scores

PRSnames <- c('PSCYH_LdAuto','PSYCH_LdAutoRed','r_ADHD_Demontis_ieua1183_auto','r_anorexia_PGC2019_auto','r_Anxiety_Purves2019_TotAnx_effect_auto','r_Autism_Grove_ieua1185_auto', 'r_Bipolar_PGC2021_auto','r_DEP_Howard2019_ieub102_auto','r_PTSD_PGC019_EUR_auto','r_SCZ_PGC2022_EUR_GWAS_auto', 'r_UKB_HairColourRed_ukbd1747_2_auto', 'r_neuroSum_Nagel2018_auto', 'r_insomnia_Watanabe_2022_auto','r_ChronicPain_Johnston_2019_auto')

write.table(PRSnames,"listPGS",col.names=F, row.names=F, quote=F)



 