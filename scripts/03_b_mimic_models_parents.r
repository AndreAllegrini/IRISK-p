#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

PRSname = as.character(args[1])

library(lavaan)

# load data

dat <- readRDS("200k_phenoPlusPRS_cleanedTrios_01February2023_unrel_pca.rds")

# vector of ordinal indicators
vars <- c("odd137","odd138","odd139","odd140","odd141","odd142","odd143","odd144","cnd111","cnd112","cnd113","cnd114", 
          "cnd115","cnd116","cnd117","cnd118","inat119", "inat120", "inat121", "inat122", "inat123", "inat124", "inat125", "inat126",
          "inat127", "hyp128","hyp129","hyp130","hyp131","hyp132","hyp133","hyp134","hyp135","hyp136","anx145","anx146", 
          "anx147","anx148","anx149","mfq68", "mfq69", "mfq70", "mfq71", "mfq72", "mfq73", "mfq74", "mfq75", "mfq76",
          "mfq77", "mfq78", "mfq79","mfq80")  


# NULL MODEL

null00 <-  paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

Pfact ~ 0*",PRSname,"_Mother + 0*",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")



# GENERAL FACTOR

Pfac <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

Pfact ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")

# SPECIFIC FACTORS

Spec <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

mfq + anx + cnd + odd + hyp + inat  ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")

# HETEROGENEITY - specific factors

# MFQ

Spec_a <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

 anx + cnd + odd + hyp + inat  ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80 ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")



### ANX

Spec_b <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

mfq + cnd + odd + hyp + inat  ~ ",PRSname,"_Mother + ",PRSname,"_Father

anx145+anx146+anx147+anx148+anx149 ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")



# CND

Spec_c <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

mfq + anx + odd + hyp + inat  ~ ",PRSname,"_Mother + ",PRSname,"_Father

cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118 ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")

# ODD

Spec_d <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

mfq + anx + cnd + hyp + inat  ~ ",PRSname,"_Mother + ",PRSname,"_Father

 odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144 ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")


#HYP


Spec_e <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

mfq + anx + cnd + odd + inat  ~ ",PRSname,"_Mother + ",PRSname,"_Father

hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136 ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")


#INAT


Spec_f <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

mfq + anx + cnd + odd + hyp  ~ ",PRSname,"_Mother + ",PRSname,"_Father

inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ ",PRSname,"_Mother + ",PRSname,"_Father

mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB

")

# SYMPTOM INDICATORS 

Items <-   paste0("
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat


mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127 ~ SEX + YOB + ",PRSname,"_Mother + ",PRSname,"_Father

")

#list of models

lsmodels <- list(null00 = null00, 
                 Pfac = Pfac, 
                 Spec = Spec, 
                 Spec_a = Spec_a, 
                 Spec_b = Spec_b, 
                 Spec_c = Spec_c, 
                 Spec_d = Spec_d, 
                 Spec_e = Spec_e, 
                 Spec_f = Spec_f, 
                 Items = Items)



for (MODEL in names(lsmodels)){
  
# fit models
fit <- sem(paste0(lsmodels[MODEL]), 
            data=dat,
            ordered = c(vars),
           missing =  "pairwise")

# save fit
saveRDS(fit, file = paste0("../output/",PRSname,"_",MODEL,"_parents.rds"))

}

