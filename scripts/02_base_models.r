
library(lavaan)


dat <- readRDS("200k_phenoPlusPRS_cleanedTrios_16December2022_unrel_pca.rds")

vars <- c("odd137","odd138","odd139","odd140","odd141","odd142","odd143","odd144","cnd111","cnd112","cnd113","cnd114", 
"cnd115","cnd116","cnd117","cnd118","inat119", "inat120", "inat121", "inat122", "inat123", "inat124", "inat125", "inat126",
"inat127", "hyp128","hyp129","hyp130","hyp131","hyp132","hyp133","hyp134","hyp135","hyp136","anx145","anx146", 
"anx147","anx148","anx149","mfq68", "mfq69", "mfq70", "mfq71", "mfq72", "mfq73", "mfq74", "mfq75", "mfq76",
"mfq77", "mfq78", "mfq79","mfq80")  


## second order

model <-  "
# measurement model
mfq =~ mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ anx145+anx146+anx147+anx148+anx149
cnd =~ cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ mfq + anx + cnd + odd + hyp + inat

"

start <- Sys.time()

fit <- sem(model, 
            data=dat,
            ordered = c(vars)
           ,missing =  "pairwise"
           )

end <- Sys.time()

end-start

summary(fit, standardized=TRUE, fit.measures=TRUE, rsq=T)

saveRDS(fit, file = "../output/baseModel_secondOrder.rds") 



#model fit 
modelFit <- NA
modelFit <- c(fitMeasures(fit)[["chisq"]],fitMeasures(fit)["df"],fitMeasures(fit)["pvalue"],fitMeasures(fit)["rmsea"], fitMeasures(fit)["srmr"], fitMeasures(fit)["cfi"])
names(modelFit) <- c("chisq","df","chisqPval","rmsea","srmr","cfi")

 round(modelFit,4)
 
 
##bifactor
 
model <-  "
# measurement model
mfq =~ 1*mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80
anx =~ 1*anx145+anx146+anx147+anx148+anx149
cnd =~ 1*cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118
odd =~ 1*odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144
hyp =~ 1*hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136
inat =~ 1*inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

Pfact =~ 1*mfq68+mfq69+mfq70+mfq71+mfq72+mfq73+mfq74+mfq75+mfq76+mfq77+mfq78+mfq79+mfq80+anx145+anx146+anx147+anx148+anx149+cnd111+cnd112+cnd113+cnd114+cnd115+cnd116+cnd117+cnd118+odd137+odd138+odd139+odd140+odd141+odd142+odd143+odd144+hyp128+hyp129+hyp130+hyp131+hyp132+hyp133+hyp134+hyp135+hyp136+inat119+inat120+inat121+inat122+inat123+inat124+inat125+inat126+inat127

#covariances
cnd + hyp + odd + inat + anx + mfq ~~ 0*Pfact

cnd ~~  0*hyp + 0*odd + 0*inat + 0*anx + 0*mfq 
hyp ~~  0*odd + 0*inat + 0*anx + 0*mfq 
odd ~~ 0*inat + 0*anx + 0*mfq 
inat ~~  0*anx + 0*mfq 
anx  ~~  0*mfq

"

start <- Sys.time()

fit2 <- sem(model, 
            data=dat,
            ordered = c(vars)
            ,missing =  "pairwise"
            )

end <- Sys.time()

end-start


summary(fit2, standardized=TRUE, fit.measures=TRUE, rsq=T)

saveRDS(fit2, file = "../output/baseModel_bifactor.rds") 

#model fit 
modelFit2 <- NA
modelFit2 <- c(fitMeasures(fit2)[["chisq"]],fitMeasures(fit2)["df"],fitMeasures(fit2)["pvalue"],fitMeasures(fit2)["rmsea"], fitMeasures(fit2)["srmr"], fitMeasures(fit2)["cfi"])
names(modelFit2) <- c("chisq","df","chisqPval","rmsea","srmr","cfi")

round(modelFit2,4)
 
anova(fit,fit2)
 
 