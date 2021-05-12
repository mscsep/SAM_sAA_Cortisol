#' MCT Assumptions Check Source (version 28 aug 2018)
#' written by Milou Sep

library(influence.ME); library(lme4); library(LMERConvenienceFunctions); library(lmerTest)
library(moments)

LMER_assumptions <- function(check.data, dependent_var, logtransform){
  
  # Print on screen on which variable LMER assumptions are checked, using which contrast. Necessary because this function is used in loop's with multiple contrasts & variables
  print(paste("LMER assumption check on dependent variable", dependent_var)) # print dependent_var on screen
  
  # for datasets with emo & context & condition: bv dPrime; phit & pmiss
  if (dependent_var == 'dPrime' | dependent_var == 'pHit' | dependent_var == 'pMiss' | dependent_var == 'C'){
    # NB these values need to be assigned to the global environment. If not, problem's arise with accessibility of 'check.data' in the LMER object "main" at later call's of 'main' in the script.
    assign("dependent_var", value=dependent_var, pos=.GlobalEnv )
    assign("check.data", value=check.data, pos=.GlobalEnv ) # info: https://stat.ethz.ch/R-manual/R-devel/library/base/html/assign.html
    assign("main", value=(lmer(get(dependent_var)~1+Condition*context*emotion+(1|id)+(1|context:id)+(1|emotion:id), data=check.data, REML=T)), pos=.GlobalEnv, envir=.GlobalEnv)
    raw_data <- data_MCT ## NB same for all MCT analyses
    
    ## for datasets with condition & emotion variables e.g. dprime contextualization; pFA; pCR; Phit.ctx; pmiss.ctx
  }else if (dependent_var == "dprime_contextualization" | 
            dependent_var == 'pFA' | dependent_var == 'pCR' | dependent_var == 'pHit.ctx'|dependent_var == 'pMiss.ctx'){
    # NB these values need to be assigned to the global environment. If not, problem's arise with accessibility of 'check.data' in the LMER object "main" at later call's of 'main' in the script.
    assign("dependent_var", value=dependent_var, pos=.GlobalEnv )
    assign("check.data", value=check.data, pos=.GlobalEnv ) # info: https://stat.ethz.ch/R-manual/R-devel/library/base/html/assign.html
    assign("main", value=(lmer(get(dependent_var)~1+Condition*emotion+(1|id), data=check.data, REML=T)), pos=.GlobalEnv, envir=.GlobalEnv)
    raw_data <- data_MCT ## NB same for all MCT analyses
    
    # Analysis of CORT or sAA data 
  }else if (dependent_var == "CORT" | dependent_var == 'sAA'){
    assign("dependent_var", value=dependent_var, pos=.GlobalEnv )
    assign("check.data", value=check.data, pos=.GlobalEnv ) # info: https://stat.ethz.ch/R-manual/R-devel/library/base/html/assign.html
    
    if (logtransform == F){
      assign("main", value=(lmer(get(dependent_var)~1+Condition*Sample+(1|id), data=check.data, REML=T)), pos=.GlobalEnv, envir=.GlobalEnv)
    }else if (logtransform == T){
      assign("main", value=(lmer(log(get(dependent_var))~1+Condition*Sample+(1|id), data=check.data, REML=T)), pos=.GlobalEnv, envir=.GlobalEnv)
    }
    raw_data = check.data  # used to determine N, 120 subjects, but only 100 completed MCT)
  }
  
  
  ######### Start Assumption checks LMER ###################
  
  #' **Data distribution**
  datacolum<-check.data[which(colnames(check.data) == dependent_var)]
  if (logtransform == F){
    par(mfrow=c(1,1)); hist(data.matrix(datacolum), main=paste("Historgram raw data", dependent_var))
  }else if (logtransform ==T){
    par(mfrow=c(1,1));  hist(log(data.matrix(datacolum)), main=paste("Historgram logtransformed raw data", dependent_var))
  }
  
  #'  **Main effect Model:**
  #+ echo=FALSE
  #print(summary(main))
  print(anova(main))
  #'  **Model Criticism Plots:**
  mcp.fnc(main) 
  
  #' **Distribution Plots (Linearity, heteroskedastidcity, Normality)**
  #+ echo=FALSE
  
  {par(mfrow=c(2,2)); # Verdeel figure in 2*2 matrix
    #(1)linearity
    plot(fitted(main),residuals(main), main=paste(dependent_var, "Linearity contrast"));
    abline(0,0,lty=2,lwd=2);
    #(2) absence of collinearity
    # no collinearity by design
    #(3) absence of heteroscedasticity
    # plot(fitted(fit1),residuals(fit1), main=paste("Heteroscedasticity ExpCtr ", i)) # 22.5.18 commented because same plot as "(1) Linearity"
    plot(rnorm(100),rnorm(100),col="red", main=paste(dependent_var, "Reference plot")) ; # reference plot
    #(4) normality of residuals
    hist(residuals(main), main=paste(dependent_var, "Historgram Model Residuals"));
    qqnorm(residuals(main), main=paste(dependent_var, "QQ plot"));
    qqline(residuals(main),col = 2)
    
    # Statistics (need package moments)
    print(paste0("skewness (FullModel):",skewness(residuals(main))))
    print(paste0("kurtosis (FullModel):",kurtosis(residuals(main))))
  }
  
  #' **Outliers / Influencial points**
  #+ echo=FALSE
  # (5) analysis of influencial points ('outliers') following Nieuwenhuis et al. 2012 influence.ME for base & main model
  # 5.a Calculate cook's distance and identify measures above cutoff
  # Datapoints with Cook's Distance above cutoff 4/n in Main effect Model
  estex.main<-influence(main,"id") # NB het object dat uit influence komt is van de class 'estex'
  cooks_main<-cooks.distance(estex.main)
  print(cooks_main)
  print(cooks_main[(cooks_main>(4/(nrow(raw_data)))),]) # Toont measures met cook's distance above cutoff
  {
    plot(estex.main, which="cook",
         cutoff=.04, sort=TRUE,
         xlab="Cook's Distance",
         ylab="id",
         cex.lab=0.01, # Smaller lettertype for as labels.
         main=paste(dependent_var, "Cook's Distance")
    ) 
  }
  
  #' Measures that significantly change model-significance over threshold (per contrast):
  #+ collapse=TRUE
  sig_change <- sigtest(estex.main, test=1.96)
  outliers<-list()
  for (i in 1:(length(sig_change))){
    outliers[[i]] <-which(sig_change[[i]]$Changed.Sig)
  }
 # print(outliers) # Toont outliers van contrast opscherm.
  
  rm(list = c("check.data", "main", "dependent_var"), pos=.GlobalEnv)
  return(outliers)
}
