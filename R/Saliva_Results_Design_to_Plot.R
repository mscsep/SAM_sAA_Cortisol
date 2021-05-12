# Visualize Experimental design, cortisol and sAA data (FGT paper)
# written by Milou Sep

library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(tidyr)
library(dplyr)
library(Rmisc)
# graph colours: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
library("ggsci") #https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html

# Load data that was used for analyses (created with LMM_sAA_sCort_Analysis.r)
# load("processed_data/Saliva_data_condition_MCT_ONLY.rda") # MCT task
load("processed_data/Saliva_data_condition_FGT_ONLY.rda") # FGT task
str(Saliva_data_condition$Sample) # to check

# Add real time! ----------------------------------------------------------
# NOTE available on Dataverse
Timepoints <- read.csv2("data/SAM_Timepoints_reactivity_measures.csv", na.strings = '-')
tibble(Timepoints)
# to check new colum (all correct!)
# diff((Timepoints$time_from_baseline_minutes))
# diff(Timepoints$time_to_fear_conditioning_minutes)
# diff(Timepoints$time_to_memory_minutes)

# Create Sample Time matrix, which contains the relative time in minuts of each sample with respect to encoding/retrieval
Timepoints %>% 
  filter(!is.na(Salivette_number)) %>% # Select only timepoints with saliva samples.
  select(Salivette_number, time_to_memory_minutes) %>% # FOR MCT!
  separate(., col=Salivette_number, into=c("Saliva", "Sample"), sep="_")  %>%
  filter(Sample != "01") %>% # select only samples 2 t/m 14 (note sample 1 is extra baseline sample)
  mutate(Sample = factor(Sample)) %>%# Change sample to 'factor', because the variable sample in Saliva_data_condition is also a factor. Need to be identical for "merge"
  select(Sample, time_to_memory_minutes) -> sample_time_matrix

# NOTE in the .cvs file are actual minutes listed in time_to_memory_minutes variable. 
# There is 24h between Sample S_13 and S_14, which is 'too long' for the plots, there for the last timepoints are corrected here
# for MCT
sample_time_matrix$time_to_memory_minutes[13] <- 90
sample_time_matrix$time_to_memory_minutes[14] <- 120
# # For FGT
# sample_time_matrix$time_to_fear_conditioning_minutes[13] <- 60
# sample_time_matrix$time_to_fear_conditioning_minutes[14] <- 90

# Merge both datasets by 'Sample' variable
Saliva_data_condition <- merge(Saliva_data_condition, sample_time_matrix, by=c("Sample"))

# Rename time AND Group variable
dplyr::rename(Saliva_data_condition, 
              Time = time_to_memory_minutes, 
              Group = Condition) -> Saliva_data_condition # Rename Condition in Group  (for FGT paper)
# glimpse(Saliva_data_condition)

# Create annotations for sig. ---------------------------------------------
# Create vectors which indicate significant samples in plots
t=sample_time_matrix$time_to_memory_minutes
s=sample_time_matrix$Sample

# 12.11.18: Results sAA analyses: sig differences (MCT paper) on sample: 03, 10, 11 (original names) # 1.2.19 Same timepoints sig for FGT paper no change needed
Sig.sAA<- c(t[s == "03"], t[s == "10"], t[s == "11"])

# 12.11.18: Results Cortisol analyses: sig differences (MCT paper) on sample: 04, 05, 06, 07, 11, 12, 13 (original names) # 1.2.19 Same timepoints sig for FGT paper no change needed
Sig.cort<- c(t[s == "04"], t[s == "05"], t[s == "06"], t[s == "07"], t[s == "11"], t[s == "12"],t[s == "13"])


# Reorder factor (for apperance in plots) ---------------------------------
Saliva_data_condition$Group <- factor(Saliva_data_condition$Group, 
                                      levels=c("Control", "Direct", "Delayed"), 
                                      labels=c("1: No stress","2: Acute phase", "3: Recovery phase"))

# Calculate M & 95% CI's --------------------------------------------------
# NB CIs are calculated manually, as ggplot did not calculate distinct CI's per variable level, and errorbars were not calculated correctly by ggplot.
#https://www.rdocumentation.org/packages/Rmisc/versions/1.5/topics/group.CI
sAA.Mean.CI <- group.CI(log(sAA)~Time+Group, Saliva_data_condition, ci = 0.95)
# str(sAA.Mean.CI)
sAA.Mean.CI

CORT.Mean.CI <- group.CI(log(CORT)~Time+Group, Saliva_data_condition[!Saliva_data_condition$id=="074",], ci = 0.95)
# str(CORT.Mean.CI)
CORT.Mean.CI$`log(CORT).mean`

# Salivary alpha amylase --------------------------------------------------

sAA <- 
  ggplot(data=sAA.Mean.CI, 
         aes(x=Time, y=`log(sAA).mean`,
             group=factor(Group), color=Group)) + 
  #ggtitle('Salivary Alpha Amylase') + 
  xlab("Time (min)") + ylab('Salivary \u03b1-amylase \n (mean ln(E/L) +/-95%CI)')  + # ax labels & title
  # Check tic's op x as & Add x-axis labels
  scale_x_continuous(breaks = unique(sAA.Mean.CI$Time)) +
  # scale_color_aaas(labels=c(Delayed = "Recovery phase (group 3)",
  #                           Direct = "Acute phase (group 2)",
  #                           Control = "No stress (group 1)"))+
  scale_color_brewer(palette="Dark2")+
  theme_classic() +
  # adjust fontsize legend
  theme(legend.title=element_text(size=12), legend.text=element_text(size=9))+
  
  ## Add MCT events to plot ...
  
  # ... Manipulation 1 (8min before T2, tot T3)
  annotate("rect",xmin=(unique(Saliva_data_condition$Time)[2]-8),xmax=unique(Saliva_data_condition$Time)[3],ymin=-Inf,ymax=Inf, alpha=1, fill="grey70") + # Add rectangle
  #  annotate("text", x=c(unique(Saliva_data_condition$Time)[2]-2), y = 11.95, label = "(placebo)\nTSST1", size=3 ) +
  # ... Manipulation 2 (T8, tot T10)
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[8],xmax=unique(Saliva_data_condition$Time)[10],ymin=-Inf,ymax=Inf, alpha=1, fill="grey70") + # Add rectangle
  #annotate("text", x=unique(Saliva_data_condition$Time)[9], y = 11.95, label = "(placebo)\nTSST2", size=3) + # angle=45
  
  # ... MCT encoding: (T10 tot T11)
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[10],xmax=unique(Saliva_data_condition$Time)[11],ymin=-Inf,ymax=Inf, alpha=1, fill="gray90") + # Add rectangle
  # annotate("text", x=15, y = 11.95, label = "Encoding\n(MCT)", size=3) +
  # MCT retrieval
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[13],xmax=unique(Saliva_data_condition$Time)[14],ymin=-Inf,ymax=Inf, alpha=1, fill="gray90") + # Add rectangle
  # annotate("text", x=105, y = 11.95, label = "Retrieval\n(MCT)", size=3 ) +
  
  # FGT acquisition
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[11],xmax=unique(Saliva_data_condition$Time)[12],ymin=-Inf,ymax=Inf, alpha=1, fill="grey80") + # Add rectangle
 #annotate("text", x=unique(Saliva_data_condition$Time)[11]+15, y = 13, label = "FGT Acquisition", angle=90, size=3.2) +
  
  # FGT retrieval
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[14],xmax=unique(Saliva_data_condition$Time)[14]+30,ymin=-Inf,ymax=Inf, alpha=1, fill="grey80") + # Add rectangle
  #NB timepoint after FGT does not exist --> therefore timepoint14 + 30 (= task duration)
  #annotate("text", x=unique(Saliva_data_condition$Time)[14]+15, y = 13, label = "FGT Test", angle=90, size=3.2) +  # Note + 15 to have text in middle of rectangle
  
  # Add sAA Data (points & line ...
  geom_line(aes(colour=Group), stat="summary" , fun.y="mean", na.rm=T, size=1) +# remove na is needed to prevent warning (there are some NA's in data)
  geom_point(stat="summary",fun.y="mean", na.rm=T, size=2)  + # Add datapoints
  # .. and errorbars (info: https://stackoverflow.com/questions/48800212/set-error-bars-to-standard-deviation-on-a-ggplot2-bar-graph)
  geom_errorbar(aes(ymin=`log(sAA).lower`, ymax=`log(sAA).upper`), position=position_dodge(3)) +  
  
  # Add white rectangle to split day1 & day2 (between T12 and T13)
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[12]+2,xmax=unique(Saliva_data_condition$Time)[13]-2, ymin=-Inf,ymax=Inf, alpha=1, fill="white")  + # FGT + dynamic (note + and - 2 are for visability in plot)
  annotate("text", x=unique(Saliva_data_condition$Time)[12]+15, y =12.4 , label = "- - 24h - -", size=3) +
  
  # Add marks for significance:
  annotate("text",x=Sig.sAA, y=13.2 , label = c("*"), size=6)

sAA 



# Salivary Cortisol -------------------------------------------------------

CORT <- 
  ggplot(data=CORT.Mean.CI, 
         aes(x=Time, y=`log(CORT).mean`, 
             group=factor(Group), color=Group)) + 
  #ggtitle('Salivary Cortisol') + 
  xlab("Time (min)") + ylab('Salivary cortisol \n (mean ln(nmol/L) +/-95%CI)')  + # ax labels & titles
  # set y-axis
  scale_y_continuous(limits = c(NA, 3.4) )+
  # Check tic's op x as & Add x-axis labels
   scale_x_continuous(breaks = c(unique(Saliva_data_condition$Time), 150),
                      label=c("-180", "-135", '-130', "-115", "-100", "-70",  "-40",  "-10", "-5", "0", "30", "60", "0","30", "60")) +  # MCT time labels!
                   # label=c("-210"	,	"-165",		"-160"	,	"-145"	,	"-130"	,	"-100"	,	"-70"	,	"-40"	,	"-35"	,	"-30"	,	"0"	,	"30",		"-30"	,	"0")) +  # FGT time labels!
  # scale_color_aaas(labels=c(Delayed = "Recovery phase (group 3)",
  #                           Direct = "Acute phase (group 2)",
  #                           Control = "No stress (group 1)"))+
  scale_color_brewer(palette="Dark2", labels=c(Control = "No stress (group 1)",
                                               Direct = "Acute phase (group 2)",
                                               Delayed = "Recovery phase (group 3)"))+
  theme_classic() +
  # adjust fontsize legent
  theme(legend.title=element_text(size=12), legend.text=element_text(size=9))+
  
  ## Add MCT events to plot ...
  # ... Manipulation 1 (8min before T2, tot T3)
  annotate("rect",xmin=(unique(Saliva_data_condition$Time)[2]-8),xmax=unique(Saliva_data_condition$Time)[3],ymin=-Inf,ymax=Inf, alpha=1, fill="gray70") + # Add rectangle
  annotate("text", x=c(unique(Saliva_data_condition$Time)[2]-2), y = 3, label = "(placebo-)TSST 1", angle=90, size=3.2) + # note -2 to place text in middle fo gray rectangle
  
  # ... Manipulation 2 (T8, tot T10)
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[8],xmax=unique(Saliva_data_condition$Time)[10],ymin=-Inf,ymax=Inf, alpha=1, fill="grey70") + # Add rectangle
  annotate("text", x=unique(Saliva_data_condition$Time)[9], y = 3, label = "(placebo-)TSST 2", angle=90, size = 3.2) + # angle=45
  
  # # # ... MCT encoding: (T10 tot T11)
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[10],xmax=unique(Saliva_data_condition$Time)[11],ymin=-Inf,ymax=Inf, alpha=1, fill="gray90") + # Add rectangle
  annotate("text", x=15, y = 3, label = "MCT Learning", angle=90, size=3.2) +
  # # 
  # # # MCT retrieval
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[13],xmax=unique(Saliva_data_condition$Time)[14],ymin=-Inf,ymax=Inf, alpha=1, fill="grey90") + # Add rectangle
  annotate("text", x=105, y = 3, label = "MCT Memory", angle=90, size=3.2) +
  
  # FGT acquisition
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[11],xmax=unique(Saliva_data_condition$Time)[12],ymin=-Inf,ymax=Inf, alpha=1, fill="grey80") + # Add rectangle
  annotate("text", x=unique(Saliva_data_condition$Time)[11]+15, y = 3, label = "FGT Learning", angle=90, size=3.2) +
  
  # FGT retrieval
  # annotate("rect",xmin=unique(Saliva_data_condition$Time)[14],xmax=unique(Saliva_data_condition$Time)[14]+30,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + # Add rectangle
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[14],xmax=unique(Saliva_data_condition$Time)[14]+30,ymin=-Inf,ymax=Inf, alpha=1, fill="grey80") + # Add rectangle
  #NB timepoint after FGT does not exist --> therefore timpoint14 + 30 (zoalang duurt de taak ong.)
   annotate("text", x=unique(Saliva_data_condition$Time)[14]+15, y = 3, label = "FGT Memory", angle=90, size=3.2) +  # NOte + 15 zo have text in middle of rectangle
  
  
  # Add CORT Data (points & line & errorbars)...
  geom_line(aes(colour=Group), stat="summary" , fun.y="mean", na.rm=T, size=1) + # remove na is needed to prevent warning (there are some NA's in data)
  geom_point(stat="summary",fun.y="mean", na.rm=T, size=2)  + # Add datapoints
  # .. and errorbars (info: https://stackoverflow.com/questions/48800212/set-error-bars-to-standard-deviation-on-a-ggplot2-bar-graph)
  geom_errorbar(aes(ymin=`log(CORT).lower`, ymax=`log(CORT).upper`), position=position_dodge(3)) +  
  
  # Add white rectangle to split day1 & day2 (between T12 and T13)
  annotate("rect",xmin=unique(Saliva_data_condition$Time)[12]+2,xmax=unique(Saliva_data_condition$Time)[13]-2, ymin=-Inf,ymax=Inf, alpha=1, fill="white")  + # FGT + dynamic (note + and - 2 are for visability in plot)
  annotate("text", x=unique(Saliva_data_condition$Time)[12]+15, y =2.2 , label = "- - 24h - -", size=3) +
  
  # Add marks for significance:
  annotate("text",x=Sig.cort, y=3.3 , label = c("*"), size=6)  +
  
  # Indicate sames timepoints in graph
  annotate("text", x=unique(Saliva_data_condition$Time), y = 1.8,
           label = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13", "T14"),
           size=2)

CORT

# To save cortisol plot alone (for example with all tasks, to discuss SAM explore)
#ggsave(paste0("results/CORT.MCT.FGT.Plot.colour.",date(),".eps"), device="eps", dpi = 800, height = 5, width = 10, limitsize = T ) 


# Merge plots in one figure -----------------------------------------------
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#annotate-the-arranged-figure
Saliva.plots<- ggarrange(sAA
                         + rremove("x.text")
                         + rremove("xlab")
                         + font("y.text", size = 9)
                         + font("ylab", size = 12),
                         CORT
                         + font("x.text", size = 9, 
                                angle = 90,
                                hjust=1,
                                vjust=0.5 
                                #  margin=margin(-15,0,0,0)
                         )
                         + font("y.text", size = 9)
                         + font("ylab", size = 12)
                         + font("x.text", size = 9)
                         + font("xlab", size = 12),
                         labels = c("A", "B"),
                         ncol = 1, nrow = 2,
                         font.label=list(size=12, face="bold"),
                         align = "v",
                         legend="top",
                         common.legend = T)
Saliva.plots

annotate_figure(Saliva.plots, 
                fig.lab=c("Figure 2"), 
                fig.lab.face = "bold",
                fig.lab.size = 12)

# Save figure ------------------------------------------------------
# ggsave(paste0("results/Figure3.Saliva.Plots.colour", date(),".pdf"), device="pdf", dpi = 500, height = 7, width = 9, limitsize = T ) # issue with alpha symbol
# ggsave(paste0("results/Figure3.Saliva.Plots.colour.",date(),".eps"), device="eps", dpi = 800, height = 7, width = 9, limitsize = T ) # issue with alpha symbol
ggsave("results/Figure3.Saliva.Plots.colour.tiff", device="tiff", dpi = 300, height = 7, width = 9, limitsize = T )
