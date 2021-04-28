library(tidyverse)
library(lme4)
library(emmeans)

#### Input Variables ####

setwd("change_me")

# Input is a csv with a column titled rbdREAP containing the REAP score for 
# SARS-CoV-2 RBD in the REAP screen, a column titled rbdELISA containing the 
# SARS-CoV-2 RBD ELISA results, a column titled rbdpos containing the classification
# of the sample as SARS-CoV-2 RBD ELISA positive or negative, and a column titled patient 
# containing the patient ID. 
# Input files used in the paper can be found in the source data.
input <- read_csv("input.csv")

#### LMM Analysis ####

rbdLMM <- lmer(rbdREAP ~ factor(rbdpos) + (1|patient), data = input)

emm <- emmeans(rbdLMM, "rbdpos", lmer.df = "satterthwaite")
output <- contrast(emm, "pairwise", ref = "1") %>% as.data.frame()

write_csv(output, "lmmRBD_output.csv")


