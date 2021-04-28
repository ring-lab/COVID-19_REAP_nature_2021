library(tidyverse)
library(lme4)
library(emmeans)

#### Input Variables ####

setwd("change_me")

# Input is a csv with columns (titled score1, score2, etc.) containing the number 
# of reactivities in each sample at various score cutoffs, a column (titled category) containing
# the disease group, and a column (titled patient) containing the patient ID. 
# Input files used in the paper can be found in the source data.
input <- read_csv("input.csv")

#### LMM Analysis ####

# scale reactivity counts
inputScale <- input %>% mutate(score1 = scale(score1),
                               score2 = scale(score2),
                               score3 = scale(score3),
                               score4 = scale(score4),
                               score5 = scale(score5),
                               score6 = scale(score6),
                               category = factor(category))

# perform tests for each score level
scores <- c("score1","score2","score3","score4","score5","score6")
testList <- vector(mode = "list", length = length(scores)) # create empty list for test results
for(i in 1:length(scores)){
  model <- lmer(paste(scores[i], "~ category + (1|patient)") %>% as.formula(), data = inputScale)
  test <- emmeans(model, "category", lmer.df = "satterthwaite")
  
  testList[[i]] <- contrast(test, "pairwise", ref = "control") %>% 
    as_tibble() %>% mutate(cutoff = scores[i])
}

# write output files
outputTest <- rbind(testList[[1]],
                    testList[[2]],
                    testList[[3]],
                    testList[[4]],
                    testList[[5]],
                    testList[[6]])


write_csv(outputTest, "lmmTests_results.csv")
