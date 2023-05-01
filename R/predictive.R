# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning
predictive_tbl <- readRDS(file = "../data/fulldataset.rds")
