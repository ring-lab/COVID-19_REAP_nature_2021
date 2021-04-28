library(tidyverse)
library(lme4)
library(emmeans)

#### Input Variables ####

setwd("change_me")

# Input is a csv with a column titled category containing the disease severity
# (severe or moderate), a column titled DFSO containing days from symptom onset
# for each sample, and a column titled patient containing the patient ID. 
# Input files used in the paper can be found in the source data.
input <- read_csv("input.csv")

#### LMM Analysis ####

dfsoLMM <- lmer(DFSO ~ category + (1|patient), data = input)

emm <- emmeans(dfsoLMM, "category", lmer.df = "satterthwaite")
output <- contrast(emm, "pairwise", ref = "1") %>% as.data.frame()

write_csv(output, "lmmDFSO_output.csv")
