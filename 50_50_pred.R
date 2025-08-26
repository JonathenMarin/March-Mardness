library(tidyverse)
library(data.table)
library(reshape)
library(stringr)


submission <- fread("march-machine-learning-mania-2025/SampleSubmissionStage2.csv")

submission <- submission %>% mutate(Pred = 0.5)
submission_final <- submission %>% select(ID, Pred)
