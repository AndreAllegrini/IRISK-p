
library(lavaan)
library(dplyr)

for (stub in c("trio","parents","child")) {

#extract coefficients

types <- c(paste0('_Pfac_',stub),paste0('_Spec_',stub))

listFiles <-list.files()

listRes <- NULL

for (type in types){

tmp <- listFiles  %>% as.data.frame() %>% filter(grepl(type, .))

for (i in tmp$.){

fit <- readRDS(paste0(i))

x <- summary(fit, standardized=TRUE, fit.measures=TRUE)

listRes <- rbind(listRes,x$PE %>%
                   filter(op == '~', rhs != 'SEX',rhs != 'YOB') %>%
                   select(lhs,rhs,est,se,pvalue,std.all))

}
}

saveRDS(listRes, file = paste0("listRes_",stub,"_",format(Sys.time(), '%d%B%Y'),".rds"))


#model comparison

comparison <- list()

listFiles <-list.files()

type <- c(paste0('_Pfac_',stub))

tmp <- listFiles  %>% as.data.frame() %>% filter(grepl(type, .))

listPRS <- gsub(paste0('_Pfac_',stub,'.rds'),'',tmp$.)

for(prs in listPRS){
  
  fit0 <- readRDS(paste0(prs,'_null00_',stub,'.rds'))
  
  fitP  <-  readRDS(paste0(prs,'_Pfac_',stub,'.rds'))
  
  fitS  <- readRDS(paste0(prs,'_Spec_',stub,'.rds'))

  a1 <- anova(fit0,fitP,fitS)
  a2 <- anova(fit0,fitS)
  
  comparison[[prs]] <- rbind(a1,a2)
  
}

saveRDS(comparison, file = paste0('compare_fit_',stub,'_',format(Sys.time(), '%d%B%Y'),'.rds'))


#same for first order factors (heterogeneity)

#extract coefficients

types <- paste0(c('_Spec_a_','_Spec_b_','_Spec_c_','_Spec_d_','_Spec_e_','_Spec_f_'),stub)

#list files 

listFiles <-list.files()

listRes <- NULL

for (type in types){

tmp <- listFiles  %>% as.data.frame() %>% filter(grepl(type, .))

for (i in tmp$.){

  fit <- readRDS(paste0(i))
  
  x <- summary(fit, standardized=TRUE, fit.measures=TRUE)
  
  listRes <- rbind(listRes,x$PE %>%
                     filter(op == '~', rhs != 'SEX',rhs != 'YOB') %>%
                     select(lhs,rhs,est,se,pvalue,std.all))
  
}

}

saveRDS(listRes, file = paste0("listRes_het_",stub,"_",format(Sys.time(), '%d%B%Y'),".rds"))


#model comparison

comparison <- list()

for(prs in listPRS){

  
fitS <- readRDS(paste0(prs,'_Spec_',stub,'.rds'))
  
mfq  <- readRDS(paste0(prs,'_Spec_a_',stub,'.rds'))

anx  <- readRDS(paste0(prs,'_Spec_b_',stub,'.rds'))

cnd  <- readRDS(paste0(prs,'_Spec_c_',stub,'.rds'))

odd  <- readRDS(paste0(prs,'_Spec_d_',stub,'.rds'))

hyp  <- readRDS(paste0(prs,'_Spec_e_',stub,'.rds'))

inat  <- readRDS(paste0(prs,'_Spec_f_',stub,'.rds'))

a1 <- anova(fitS,mfq)
a2 <- anova(fitS,anx)
a3 <- anova(fitS,cnd)
a4 <- anova(fitS,odd)
a5 <- anova(fitS,hyp)
a6 <- anova(fitS,inat)

comparison[[prs]] <- rbind(a1,a2,a3,a4,a5,a6)

}

saveRDS(comparison, file = paste0('compare_fit_het_',stub,'_',format(Sys.time(), '%d%B%Y'),'.rds'))

}
