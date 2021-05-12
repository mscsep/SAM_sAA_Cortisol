#' ---	
#' title: "Analyses of Saliva: sAA and Cortisol"
#' author: "Milou Sep"	
#' date: "4/9/2018"	
#' output:	
#'   html_document: default	
#' ---	

#' 
#+ Load_data_&_functions, include=FALSE
#source('R/Load_sAA_sCORT_Lab_Results.r')# Load & Restructure data
Saliva_data_condition <- readRDS("processed_data/sAA_sCORT_condition_dataset.rds")

source("R/LMM_Results_to_Word.R") # To export tables to word
source("R/LMM_Assumptions.R") # Check lmer assumptions  # NB also used for MCT analyses used

library(lme4)
library(emmeans)
# citation("lme4")
# citation("emmeans")

# Select only participants that completed the MCT task! (n=100)
MCT_data <- read.csv2("data/SAM_MCT.csv", na.strings = 'NA')
MCT_participants <- MCT_data$subjName
MCT_participants <- gsub("SMGH","", MCT_participants) # remove "SMGH" from subjectames to use numbers on x-as (& in subset)
MCT_pattern<-paste(MCT_participants, collapse ="|") 
# matching multiple patterns https://stackoverflow.com/questions/6947587/matching-multiple-patterns
Saliva_data_condition <- Saliva_data_condition[grepl(MCT_pattern, as.character(Saliva_data_condition$id)),] # only MCT subjects
rm(MCT_pattern, MCT_participants) # remove unnecessary datasets & variables
# to check 14*100 = 1400

# Select only participants that completed the FGT task! (n=117)
FGT_data <- read.csv2("data/SAM_FGT.csv", na.strings = c("NaN","5555","8888","9999"))
FGT_participants <- FGT_data$subjName
FGT_participants <- gsub("SMGH","", FGT_participants) # remove "SMGH" van subjectames voor gebruik van nummers op x-as (& in subset)
FGT_pattern<-paste(FGT_participants, collapse ="|") 
# matching multiple patterns https://stackoverflow.com/questions/6947587/matching-multiple-patterns
Saliva_data_condition <- Saliva_data_condition[grepl(FGT_pattern, as.character(Saliva_data_condition$id)),] # only FGT subjects
# to check 14*117 = 1638

# Count missing values
sum(is.na(Saliva_data_condition$sAA)) # amount missing values
Saliva_data_condition[is.na(Saliva_data_condition$sAA),] # which value is missing
length(Saliva_data_condition$sAA) # total samples

sum(is.na(Saliva_data_condition$CORT)) # amount missing values
Saliva_data_condition[is.na(Saliva_data_condition$CORT),] # which value is missing
length(Saliva_data_condition$CORT) # total samples

# Save final dataset (with only MCT or FGT data), needed to make figures
# save(Saliva_data_condition, file="processed_data/Saliva_data_condition_MCT_ONLY.rda")
save(Saliva_data_condition, file="processed_data/Saliva_data_condition_FGT_ONLY.rda")


# Define LMER & PostHoc Functions -----------------------------------------

#' ### **Function for LMER posthoc testing**
#+ include=F
PostHocTest.Saliva <- function(dependent.var, FittedModel, selected_contrast){
  
  # Pairwise Comparisons ----------------------------------------------------
  PostHocResults <-emmeans::emmeans(FittedModel, #fitted model  
                                    eval(selected_contrast), #set contrasts
                                    adjust="tukey",#p-value adjustment for multiple comparisons family wise (note: Benjamini-Hochberg correction is not appropriate here because p-values are not independent http://www-stat.wharton.upenn.edu/~steele/Courses/956/Resource/MultipleComparision/Writght92.pdf)
                                    lmer.df = "kenward-roger")#use k-r method for computing denom df #Although it is computationally expensive, often times it is worth specifying the KR because it corrects for inflated Type I error rates.
  
  PostHocResults$emmeans # Estimated Means per group
  PostHocResults$contrasts # Statistical comparisons
  
  # Plot Results ------------------------------------------------------------
  PostHocPlot<-plot(PostHocResults, main="post-hoc tests", xlab=as.character(dependent.var), comparisons=T)#comparisons gives red errors for the tukey comparisons, use these to determine differences, not the purple confidence intervals
  
  # Calculate effect size's -------------------------------------------------
  # [for formula see http://www.bwgriffin.com/gsu/courses/edur9131/content/Effect_Sizes_pdf5.pdf]
  cohens.d<-(data.frame(PostHocResults$contrasts)$t.ratio*2)/ 
    sqrt(data.frame(PostHocResults$contrasts)$df)
  
  PostHoc.Table<-cbind(as.data.frame(PostHocResults$contrasts), cohens.d) # Bind's cohens d's to table with post hoc Results
  
  # Create function output --------------------------------------------------
  Results <- list(PostHocResults, PostHoc.Table, PostHocPlot) 
  names(Results) <- c("PostHoc.Tests", "PostHoc.Table", "PostHoc.Plot")
  return( Results)
}

#' ### **Function to compare LMER models**
#+ include=F
LMER_Salivettes_Log <- function(dependent.var, dataset){
  # Define Models
  FullModel_log<-lmerTest::lmer(log(get(dependent.var))~1+Condition*Sample+(1|id),data=dataset,REML=F)
  #  main_randomTime_log<-lmerTest::lmer(log(get(dependent.var))~1+Condition*Sample+(1|id)+(1|Sample),data=dataset,REML=F)
  Main_NO_2way_log<-lmerTest::lmer(log(get(dependent.var))~1+Condition+Sample+(1|id),data=dataset,REML=F)
  Main_Condition_log<-lmerTest::lmer(log(get(dependent.var))~1+Sample+(1|id),data=dataset,REML=F)
  Main_Sample_log<-lmerTest::lmer(log(get(dependent.var))~1+Condition+(1|id),data=dataset,REML=F)
  
  #Compare Models
  #  Random.Effects.Time <- anova(FullModel_log,main_randomTime_log)
  Condition.Sample <- anova(Main_NO_2way_log,FullModel_log)
  Condition <- anova(Main_Condition_log,Main_NO_2way_log)
  Sample <- anova(Main_Sample_log,Main_NO_2way_log)
  
  # BIND Models & Test voor output
  LMER.Models <- list(FullModel_log, Main_NO_2way_log,  Main_Condition_log, Main_Sample_log)
  names(LMER.Models) <- c("FullModel", "Main_NO_2ways", "Main_Condition", "Main_Sample")
  ANOVAs.Tests <- list(Condition.Sample, Condition, Sample)
  names(ANOVAs.Tests) <- c("Condition.Sample", "Condition", "Sample")
  
  # Table of model comparisons (Publication) --------------------------------
  # define model names in exported table
  Model <- c("Full Model", "Stress x Time", "Stress", "Time")
  # BIND relevant information for Publication table chi2
  Parameters <- (rbind(
    # FUll model
    cbind(Condition.Sample[2,c(1,4)], Condition.Sample[1,c(6,7,8)]), # last 3 empty
    # 2way's
    cbind(Condition.Sample[1,c(1,4)],  Condition.Sample[2,c(6, 7, 8)]),
    # Main effects
    cbind(Condition[1,c(1,4)], Condition[2,c(6,7,8)]),
    cbind(Sample[1,c(1,4)], Sample[2,c(6,7,8)])
  ))
  ChiSquTable <- cbind(Model, Parameters)
  names(ChiSquTable)<- c("Model", "df", "LogLikelihood", "Chi2", "deltadf", "pvalue")  # Rename variables, needed for flextable
  
  # Post hoc tesing using another function
  PostHoc.Stress.Time<- PostHocTest.Saliva(dependent.var=paste0("log(",as.character(dependent.var),")"), FittedModel=FullModel_log, selected_contrast=expression(pairwise ~ Condition|Sample)) 
  PostHoc.Time.Stress<- PostHocTest.Saliva(dependent.var=paste0("log(",as.character(dependent.var),")"), FittedModel=FullModel_log, selected_contrast=expression(pairwise ~ Sample|Condition)) 
  # NB PostHoc testing needs to be performed on the full model, if there is a significant interaction effects.
  PostHoc.Time<- PostHocTest.Saliva(dependent.var=paste0("log(",as.character(dependent.var),")"), FittedModel=Main_NO_2way_log, selected_contrast=expression(pairwise ~ Sample)) 
  PostHoc.Stress<- PostHocTest.Saliva(dependent.var=paste0("log(",as.character(dependent.var),")"), FittedModel=Main_NO_2way_log, selected_contrast=expression(pairwise ~ Condition)) 
  
  # Create function output --------------------------------------------------
  Results<-  list(LMER.Models, ANOVAs.Tests, ChiSquTable, PostHoc.Stress.Time, PostHoc.Time.Stress, PostHoc.Time, PostHoc.Stress)
  names(Results) <- c("LMER.Models", "ANOVAs.Tests", "ChiSqTable", "PostHoc.S.T", "PostHoc.T.S","PostHoc.T", "PostHoc.S")
  return(Results)
  
}


#' ## **SNS Activity (sAA)** 
#+ sAA_analysis, include=F
# sAA Assumption Check ----------------------------------------------------
sAA.Assumptions <- LMER_assumptions(dependent_var='sAA', check.data=Saliva_data_condition, logtransform=F)
# str(sAA.Assumptions)
#' - sAA data is not normal distributed, log-transformation makes it better
sAA.Assumptions.log <- LMER_assumptions(dependent_var='sAA', check.data=Saliva_data_condition, logtransform=T)

# sAA Analysis ------------------------------------------------------------
sAA.Analysis <- LMER_Salivettes_Log('sAA', Saliva_data_condition)
#' #### **Results sAA LMER Model Comparisons**
Export.Chi2.Table(Chi2Table=sAA.Analysis$ChiSqTable, TableName = "results/sAA.Chi2", file.export=F)
#' - There is a 2way interaction between, and a main effect of time.
#' 
#' #### **Results sAA LMER PostHoc Tests**  
Export.PostHoc.Table(PostHocTable = sAA.Analysis$PostHoc.S.T$PostHoc.Table, TableName = "results/sAA.PostHoc.S.T", file.export =F)
# Export.PostHoc.Table(PostHocTable = sAA.Analysis$PostHoc.T$PostHoc.Table, TableName = "results/sAA.PostHoc", file.export = T) # PostHoc Timing, Don't interpreted because no interaction
sAA.Analysis$PostHoc.S.T$PostHoc.Plot 
#' 

# Posthoc test timepoints per condition
Export.PostHoc.Table(PostHocTable = sAA.Analysis$PostHoc.T.S$PostHoc.Table, TableName = "results/sAA.PostHoc.T.S", file.export =F)
sAA.Analysis$PostHoc.T.S$PostHoc.Table

#' ## **HPA Activity (Cortisol)** 
#+ Cortisol_analysis, include=F
# Cortisol Assumption Check -----------------------------------------------
CORT.Assumptions <- LMER_assumptions(dependent_var='CORT', check.data=Saliva_data_condition, logtransform=F)
# str(CORT.Assumptions)
#' - Cort data is not normal distributed, log-transformation makes it better
#+ include=F
CORT.Assumptions.log <- LMER_assumptions(dependent_var='CORT', check.data=Saliva_data_condition, logtransform=T)
#' - Case 74 is extreme  influential point and needs to be excluded. High cortisol values were also observed in the lab.
Saliva_data_condition_ex74<-Saliva_data_condition[!Saliva_data_condition$id=="074",] # Dataset without case 74
CORT.Assumptions.log <- LMER_assumptions(dependent_var='CORT', check.data=Saliva_data_condition_ex74, logtransform=T)

# Cortisol Analysis -------------------------------------------------------
CORT.Analysis <- LMER_Salivettes_Log('CORT', Saliva_data_condition_ex74)
#' #### **Results CORT LMER Model Comparisons** 
Export.Chi2.Table(Chi2Table=CORT.Analysis$ChiSqTable, TableName = "results/CORT.Chi2", file.export=F)
#' - There is a 2way interaction between, and a main effect of time and a main effect of stress.
#' 
#' #### **Results CORT LMER PostHoc Tests**  
Export.PostHoc.Table(PostHocTable = CORT.Analysis$PostHoc.S.T$PostHoc.Table, TableName = "results/CORT.PostHoc", file.export = F)
CORT.Analysis$PostHoc.S.T$PostHoc.Plot

# Posthoc test timepoints per condition
Export.PostHoc.Table(PostHocTable = CORT.Analysis$PostHoc.T.S$PostHoc.Table, TableName = "results/CORT.PostHoc.T.S", file.export =F)
