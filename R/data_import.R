# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning
dataset_tbl <- read_delim(file = "../data/dataset.csv", delim = "+") %>%
  mutate(employee_id = 1:1470) # added employee ID before doing anything else. Prompt states file is in id order by default. Row number and id column match
reviews_tbl <- read_delim(file = "../data/satisfaction_reviews.csv", delim = ".", col_names = FALSE) %>% 
  rename(employee_id = X3,
         q_good = X1,
         q_bad = X2) # Easier names for use later. Spot check: questions are correctly aligned. NAs in dataset are listed "NA" in the file. employee_id created for a join. Company is Google, people doing all kinds of weird punctuation and such to start their comments

final_tbl <- full_join(x = dataset_tbl, y = reviews_tbl, by = join_by(employee_id)) %>% # keeping everything, joining by the employee id column. Spot check: Case 1, 4 (missing 1 comment), 28 (missing 2 comments), 1130, and 1470 are correct. Correct order maintained, all 1470 cases present
  relocate(employee_id, .before = Age) # moved column to the beginning 

# Data Export
saveRDS(final_tbl, file = "../out/fulldataset.rds") # saved to out, imported from there hereafter