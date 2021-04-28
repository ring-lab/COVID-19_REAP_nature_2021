library(tidyverse)
library(lme4)
library(emmeans)

#### Input Variables ####

setwd("change_me")

# Input is a csv with columns titled score1, score2, etc. containing the number 
# of reactivities in each sample at various score cutoffs, a column titled category containing
# the disease severity (severe or moderate), a column titled DFSO containing days from symptom onset
# for each sample, a column titled age containing the patient age for each sample,
# and a column titled patient containing the patient ID. 
# Input files used in the paper can be found in the source data.
input <- read_csv("input.csv")

#### LMM Analysis ####

# scale reactivity counts, DFSO, and age
inputScale <- input %>% mutate(score1 = scale(score1),
                               score2 = scale(score2),
                               score3 = scale(score3),
                               score4 = scale(score4),
                               score5 = scale(score5),
                               score6 = scale(score6),
                               DFSO = scale(DFSO),
                               age = scale(age))

# perform tests for each score level
scores <- c("score1","score2","score3","score4","score5","score6")
testList <- vector(mode = "list", length = length(scores)) # create empty list for test results
for(i in 1:length(scores)){
  model <- lmer(paste(scores[i], "~ sex + category + DFSO + (1|patient)") %>% as.formula(), data = inputScale)
  test <- emmeans(model, "sex")
  
  testList[[i]] <- contrast(test, "pairwise", ref = "1") %>% 
    as_tibble() %>% mutate(cutoff = scores[i])
}

# write output files
outputTest <- rbind(testList[[1]],
                    testList[[2]],
                    testList[[3]],
                    testList[[4]],
                    testList[[5]],
                    testList[[6]])

write_csv(outputTest, "lmmReactivitySex_results.csv")
