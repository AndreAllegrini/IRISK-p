#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

################ (range of) child/mother/father effect sizes in trio model

child_b = c(sqrt(.002))
mother_b = c(sqrt(.001))
father_b = c(sqrt(.0001),sqrt(0.0005),sqrt(.001))

###############

if (length(args) == 0) {
  # Defaults
  sample_size <- 15000
  ITERATIONS <- 10000
  cores_cl <- 32
  SEED <- 48151623
  
  cat("No arguments supplied. Defaults used are:\n")
  cat(paste("sample_size =", sample_size, "\n"))
  cat(paste("ITERATIONS =", ITERATIONS, "\n"))
  cat(paste("cores_cl =", cores_cl, "\n"))
  cat(paste("SEED =", SEED, "\n"))
  
} else if (length(args) != 4) {
  stop("Usage: script_name.R sample_size iterations cores_cl SEED\n")
} else {
  sample_size <- as.numeric(args[1])
  ITERATIONS <- as.numeric(args[2])
  cores_cl <- as.numeric(args[3])
  SEED <- as.numeric(args[4])
}

if (any(is.na(c(sample_size, ITERATIONS, cores_cl, SEED)))) {
  stop("All arguments must be numeric.\n")
}

if (sample_size <= 0 || ITERATIONS <= 0 || cores_cl <= 0 || SEED <= 0) {
  stop("All arguments must be positive.\n")
}

cat("Arguments supplied:\n")
cat(paste("sample_size =", sample_size, "\n"))
cat(paste("ITERATIONS =", ITERATIONS, "\n"))
cat(paste("cores_cl =", cores_cl, "\n"))
cat(paste("SEED =", SEED, "\n"))

library(tidyverse)
library(lavaan)
library(simstandard)
library(foreach)
library(parallel)
library(doParallel)
library(glue)

#####################  Population models

#pluggind in empirical values for loadings as extracted by base models 

 # weights 
mfq <-  paste( c(0.559,
0.378,
0.756,
0.588,
0.793,
0.607,
0.813,
0.671,
0.612,
0.753,
0.78,
0.697,
0.919),paste0(rep("mfq", 13) ,seq( 13)) ,sep="*", collapse = " + ")

  anx <-  paste( c(0.891,
0.424,
0.752,
0.873,
0.121),paste0(rep("anx", 5) ,seq(5)) ,sep="*", collapse = " + ")      
  
  cnd <-  paste( c(0.85,
0.84,
0.834,
0.646,
0.534,
0.714,
0.665,
0.788),paste0(rep("cnd", 8) ,seq(8))  ,sep="*", collapse = " + ")     

    
odd <-  paste( c(0.795,
0.811,
0.835,
0.699,
0.718,
0.652,
0.813,
0.806),paste0(rep("odd", 8) ,seq(8)) ,sep="*", collapse = " + ")  
  
 hyp <- paste(c(0.763,
0.794,
0.856,
0.865,
0.698,
0.604,
0.653,
0.754,
0.744),paste0(rep("hyp", 9) ,seq(9)) ,sep="*", collapse = " + ") 
  
  
 inat <- paste(c(0.658,
0.844,
0.761,
0.888,
0.779,
0.717,
0.645,
0.776,
0.681),paste0(rep("inat", 9) ,seq(9)) ,sep="*", collapse = " + ") 


#### pop model 1 [general]

#effects only via P factor

population.model.1 <- function(beta_c, beta_m, beta_f) { 
  
model <- str_glue('
  
# Measurement model 

# specific factors -  randomly sampled from uniform 
mfq =~ {mfq}
anx =~ {anx}
odd =~ {odd}
cnd =~ {cnd}
hyp =~ {hyp}
inat =~ {inat}

# general factor
pfac =~ 0.74*mfq + 0.369*anx + 0.837*hyp + 0.768*inat + 0.764*odd + 0.718*cnd 
  
# child PGS - combination of maternal and paternal PGS

PGSc ~ 0.5*PGSm + 0.5*PGSf 

# residual variance/covariance 

PGSm ~~ 0*PGSf # no assortative mating

# structural model

pfac ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf

')
  return(model)
}


#### pop model 2 [specific]

#effects only on S factors, inconsistent with mediation 

#same effects across all specific factors (not scaled)

population.model.2 <- function(beta_c, beta_m, beta_f) { 
  
model <- str_glue('
  
# Measurement model 

# specific factors -  randomly sampled from uniform 
mfq =~ {mfq}
anx =~ {anx}
odd =~ {odd}
cnd =~ {cnd}
hyp =~ {hyp}
inat =~ {inat}

# general factor
pfac =~ 0.74*mfq + 0.369*anx + 0.837*hyp + 0.768*inat + 0.764*odd + 0.718*cnd 
  
# child PGS - combination of maternal and paternal PGS

PGSc ~ 0.5*PGSm + 0.5*PGSf 

# residual variance/covariance 

PGSm ~~ 0*PGSf # no assortative mating

# structural model

mfq ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf
anx ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf
odd ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf
cnd ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf
hyp ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf
inat ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf

')
  return(model)
}



#### pop model 3 [sparse]

#sparse effects on 3 random specific factors (all equal)


population.model.3 <- function(beta_c, beta_m, beta_f) { 
  
model <- str_glue('
  
# Measurement model 

# specific factors -  randomly sampled from uniform 
mfq =~ {mfq}
anx =~ {anx}
odd =~ {odd}
cnd =~ {cnd}
hyp =~ {hyp}
inat =~ {inat}

# general factor
pfac =~ 0.74*mfq + 0.369*anx + 0.837*hyp + 0.768*inat + 0.764*odd + 0.718*cnd 
  
# child PGS - combination of maternal and paternal PGS

PGSc ~ 0.5*PGSm + 0.5*PGSf 

# residual variance/covariance 

PGSm ~~ 0*PGSf # no assortative mating

# structural model

mfq ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf
anx ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf
hyp ~ {beta_c}*PGSc + {beta_m}*PGSm + {beta_f}*PGSf


')
  return(model)
}


### Sample models


mfqI <- paste0(rep("mfq", 13),seq( 13) ,sep="",collapse="+")
anxI <- paste0(rep("anx", 5),seq( 5) ,sep="",collapse="+")
oddI <- paste0(rep("odd", 8),seq( 8) ,sep="",collapse="+")
cndI <- paste0(rep("cnd", 8),seq( 8) ,sep="",collapse="+")
hypI <- paste0(rep("hyp", 9),seq( 9) ,sep="",collapse="+")
inatI <- paste0(rep("inat", 9),seq( 9) ,sep="",collapse="+")


samplModel0 <- paste(" 
mfq =~ ",mfqI,"
anx =~ ",anxI,"
odd =~ ",oddI,"
cnd =~ ",cndI,"
hyp =~ ",hypI,"
inat =~ ",inatI," 

# general factor

pfac =~ mfq + anx + hyp + inat + odd + cnd 

pfac ~ 0*PGSc + 0*PGSm + 0*PGSf

")



samplModel1 <- paste(" 
mfq =~ ",mfqI,"
anx =~ ",anxI,"
odd =~ ",oddI,"
cnd =~ ",cndI,"
hyp =~ ",hypI,"
inat =~ ",inatI," 

# general factor

pfac =~ mfq + anx + hyp + inat + odd + cnd 

pfac ~ PGSc + PGSm + PGSf

")


samplModel2 <- paste(" 
mfq =~ ",mfqI,"
anx =~ ",anxI,"
odd =~ ",oddI,"
cnd =~ ",cndI,"
hyp =~ ",hypI,"
inat =~ ",inatI," 

# general factor

pfac =~ mfq + anx + hyp + inat + odd + cnd 

mfq + anx + hyp + inat + odd + cnd  ~ PGSc +  PGSm +  PGSf

")

### Main simulation function

sim_so <- function(N = NULL, 
                      iter = NULL, 
                      out = NULL, 
                      child_b = NULL,
                      mother_b = NULL,
                      father_b = NULL,
                   modl=NULL,
                   CORES=NULL){
  
  if(is.null(CORES)){
numCores <- detectCores()
registerDoParallel(numCores-2)  # set N cores (-2)
  }else{
registerDoParallel(CORES)  
}




log_file <- paste0(out,"/log_sims_",format(Sys.time(), '%d%B%Y'),".log")

writeLines(c(""), log_file) #create log file

start <- Sys.time() #start time

cat(paste0("Analyses started at ", start),"\n"," ","\n", file=log_file,append=TRUE)


res <- foreach (j = N, .combine=rbind) %:%
      foreach (child_beta = child_b, .combine=rbind) %:%
      foreach (mother_beta = mother_b, .combine=rbind) %:%
      foreach (father_beta = father_b, .combine=rbind) %:%
            foreach (i = iter, .combine=rbind) %dopar% { 
              
              # keep track of iteration progress outside foreach 
              if(i %in% seq(0,max(iter),length(iter)/10)){
cat("\n","Iteration", i ,"of",max(iter),"for N = ", j ,"!", "\n", 
    file=log_file, append=TRUE)
            }
              
              
              if(is.null(modl)){
                stop("you need to provide a pop model name")
  }else{
  
   pop.model <- modl(child_beta, mother_beta, father_beta) 
  }
            
dat <- sim_standardized(
  pop.model,
  n = j,
  observed = TRUE,
  latent = FALSE,
  errors = FALSE,
 factor_scores = FALSE)


# fit models                       
fit1 <- sem(samplModel1, data=dat) #effects over general factor
fit2 <- sem(samplModel2, data=dat) #effects over specific factors



#extract regression estimates              
x1 <- parameterestimates(fit1, standardized = T) %>% 
filter(op == "~") # general factor
x2 <- parameterestimates(fit2, standardized = T) %>% 
filter(op == "~") # specific factors
 
# bind results + model info (iteration and N)
rbind(cbind(x1, N=j, iter=i,
            child_b = child_beta,
            mother_b = mother_beta,
            father_b = father_beta)
      ,
            
      cbind(x2, N=j, iter=i, 
            child_b = child_beta,
            mother_b = mother_beta,
            father_b = father_beta)
)

  }

end <- Sys.time()

cat(paste0("Analyses ended at ", end)," ","\n", file=log_file,append=TRUE)

cat(paste0("(Analyses took: ", round(as.numeric(difftime(end, start , units="mins")),digits=1) ," minutes)"),"\n"," ","\n", file=log_file,append=TRUE)

cat("###END###\n",file=log_file,append=TRUE)

stopImplicitCluster() #stop cluster

outname <- paste0(out,"/sims_secondOrder_child_",format(Sys.time(), '%d%B%Y'),".rds")

#save results
saveRDS(res,file = outname) 

return(res)
}



#### run sims TEST (based on values in  Tubbs et al 2020)
#TUBBS, J. D., ZHANG, Y. D. & SHAM, P. C. (2020). Intermediate confounding in trio relationships: The importance of complete data in effect size estimation. Genetic epidemiology, 44, 395-399.
nameOutDir <- "TEST" 

system(paste0("rm -r ",nameOutDir,"; mkdir ",nameOutDir,"; cd ",nameOutDir,"; pwd")) #make folder for results (erase it first if already existent)

out = nameOutDir
iter = seq(ITERATIONS) # how many iterations

N = c(sample_size) # range of sample sizes to test 

child_b = c(sqrt(.002))
mother_b = c(sqrt(.001))
father_b = c(sqrt(.0001),sqrt(0.0005),sqrt(.001))

set.seed(SEED) # to reproduce

res <- sim_so(N = N, 
                 iter = iter, 
                 child_b = child_b,
                mother_b = mother_b,
                father_b = father_b,
                 out = out,
              modl=population.model.1,
              CORES = cores_cl)

#very close to expectation (Tubbs et al., 2020 supplementary material)


### functions for plotting

ext_power_e <- function(res = NULL) {
  power <- res %>% 
    group_by(N, lhs, rhs, father_b, mother_b, child_b) %>%
    summarize(power = sum(pvalue < .05)/length(pvalue),
              power2 = sum(pvalue < .05/12)/length(pvalue))
  return(power)
}

print(ext_power_e(res),n=63)


power = ext_power_e(res)

power %>% filter(lhs == "pfac")




power_plot <- function(power = power,
                       save = FALSE,
                       out = NULL){
  
  power$lhs = factor(power$lhs, levels=c("pfac","odd","cnd","hyp","inat","mfq","anx"))
  
  plot <- power %>%

    ggplot(aes(y=power,x=father_b,group=rhs,color=rhs)) + 
    geom_line() +
    geom_point() + 
    facet_grid(.~lhs) + 
    geom_hline(aes(yintercept=.8), linetype="dashed", color = "blue")  
  
  if(save){
    ggsave(plot, file=paste0(out,"/Sims_power_error_",format(Sys.time(), '%d%B%Y'),".png"), width = 10, height=8, dpi=350)
  }
  
  plot
  
}

power_plot(power, save =T,out=nameOutDir)


#### run sims Model 1 


nameOutDir <- "popModel1" 

system(paste0("rm -r ",nameOutDir,"; mkdir ",nameOutDir,"; cd ",nameOutDir,"; pwd")) #make folder for results (erase it first if already existent)

out = nameOutDir
iter = seq(ITERATIONS) # how many iterations

N = c(sample_size) # range of sample sizes to test 

set.seed(SEED) # to reproduce

res <- sim_so(N = N, 
                 iter = iter, 
                 child_b = child_b,
                      mother_b = mother_b,
                      father_b = father_b,
                 out = out,modl=population.model.1,
              CORES = cores_cl)


print(ext_power_e(res),n=63)

power =ext_power_e(res)

power_plot(power, save =T,out=nameOutDir)


nameOutDir <- "popModel2" 

system(paste0("rm -r ",nameOutDir,"; mkdir ",nameOutDir,"; cd ",nameOutDir,"; pwd")) #make folder for results (erase it first if already existent)

out = nameOutDir
iter = seq(ITERATIONS) # how many iterations

N = c(sample_size) # range of sample sizes to test 

set.seed(SEED) # to reproduce

res <- sim_so(N = N, 
                 iter = iter, 
                 child_b = child_b,
                      mother_b = mother_b,
                      father_b = father_b,
                 out = out,modl=population.model.2,
              CORES = cores_cl)


print(ext_power_e(res),n=63)

power =ext_power_e(res)

power_plot(power, save =T,out=nameOutDir)



nameOutDir <- "popModel3" 

system(paste0("rm -r ",nameOutDir,"; mkdir ",nameOutDir,"; cd ",nameOutDir,"; pwd")) #make folder for results (erase it first if already existent)

out = nameOutDir
iter = seq(ITERATIONS) # how many iterations

N = c(sample_size) # range of sample sizes to test 

set.seed(SEED) # to reproduce

res <- sim_so(N = N, 
                 iter = iter, 
                 child_b = child_b,
                      mother_b = mother_b,
                      father_b = father_b,
                 out = out,modl=population.model.3,
              CORES = cores_cl)


print(ext_power_e(res),n=63)

power =ext_power_e(res)

power_plot(power, save =T,out=nameOutDir)


