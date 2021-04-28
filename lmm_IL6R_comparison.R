library(tidyverse)
library(lme4)
library(emmeans)

#### Input Variables ####

setwd("change_me")

# Input is a csv with a column titled reapIL6R containing the REAP score for 
# IL6R in the REAP screen, a column titled treatment containing the classification
# of the sample as anti-IL6-R treated or non-treated, and a column titled patient 
# containing the patient ID. 
# Input files used in the paper can be found in the source data.
input <- read_csv("input.csv")
 
#### LMM Analysis ####

tociLMM <- lmer(reapIL6R ~ treatment + (1|patient), data = input)

emm <- emmeans(tociLMM, "treatment", lmer.df = "satterthwaite")
output <- contrast(emm, "pairwise", ref = "1") %>% as.data.frame()

write_csv(output, "lmmIL6R_output.csv")

