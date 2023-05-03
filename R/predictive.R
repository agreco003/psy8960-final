# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(tm)
library(qdap) #had to install JavaScript to work properly
library(textstem)
library(RWeka)
library(topicmodels)
library(tidytext)
library(doParallel)
library(ldatuning)

# Data Import and Cleaning
# predictive_tbl <- readRDS(file = "../data/fulldataset.rds") Read original dataset
# lapply(predictive_tbl, class) # check original class of each column

predictive_tbl <- readRDS(file = "../data/fulldataset.rds") %>% # clean dataset
  mutate(Attrition = as.logical(str_replace_all(Attrition, c("No" = "FALSE", "Yes" = "TRUE"))),
         BusinessTravel = factor(BusinessTravel, levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"), ordered = TRUE), # factor, but ordered from no travel to frequent travel
         Department = factor(Department, ordered = FALSE), #factor, not ordered
         Education = factor(Education, ordered = TRUE), # Categorical variable: Education 2 is not necessarily 1 more unit of education than Education 1, or 3 less than Education 5, but levels are ordered
         EducationField = factor(EducationField, ordered = FALSE), # not ordered
         Gender = factor(Gender, ordered = FALSE), # Male and Female only
         JobRole = factor(JobRole, ordered = FALSE),
         MaritalStatus = factor(MaritalStatus, ordered = FALSE), #factor, no ordered
         Over18 = TRUE, # all of them where Y for yes before, TRUE by itself coercedd to 
         OverTime = as.logical(str_replace_all(OverTime, c("No" = "FALSE", "Yes" = "TRUE"))) # as.logical needed to coerce to logical
  )
# lapply(predictive_tbl, class) # check class of each column

## Comment Data
## Number of missing values
# predictive_tbl %>% 
#   filter(q_good != is.na(q_good)) %>% 
#   nrow() ## of observations for bad comments: 1374
# predictive_tbl %>% 
#   filter(q_bad != is.na(q_bad)) %>% 
#   nrow() # observations for bad comments: 1396

## Pre-processing function
pre_processing <- function(corpus) {
  corpus %>% 
    tm_map(content_transformer(replace_contraction)) %>% # contractions replaced
    tm_map(content_transformer(replace_abbreviation)) %>% # abbreviations replaced
    tm_map(content_transformer(replace_number)) %>% #numbers replaced but retained in case they are meaningful
    tm_map(content_transformer(str_to_lower)) %>% #all lower case
    tm_map(removePunctuation) %>% #remove punctuation
    tm_map(removeWords, c(stopwords("en"), "google")) %>% #removed variations of Google, the company name
    tm_map(content_transformer(lemmatize_words)) %>% #used a preset dictionary to homogenize word forms
    tm_map(stripWhitespace) %>% #reduced all whitespace to a single space whenever it occurs
    tm_filter(FUN = function(x) {return(nchar(stripWhitespace(x$content)[[1]]) > 1) }) #remove blank cases for LDA
}

### Good comments Corpus
good_corpus_original <- VCorpus(VectorSource(predictive_tbl$q_good))
good_corpus <- pre_processing(good_corpus_original)
### Bad comments Corpus
bad_corpus_original <- VCorpus(VectorSource(predictive_tbl$q_bad))
bad_corpus <- pre_processing(bad_corpus_original)

### Pre-processing check - comment out final tm_filter line, and the pipe that preceeds it, in pre_processing function above. Then rerun good_corpus and bad_corpus to use code below
# compare_them <- function(x, y) { 
#    index <- sample(1:length(x),1) #set an index
#    print(x[[index]]$content) #print content of index case x
#    print(y[[index]]$content) #print content of index case y
#  }
# compare_them(good_corpus_original, good_corpus) #run as many times you like for checks. Looks good to me!
# compare_them(bad_corpus_original, bad_corpus) #run as many times you like for checks. Looks good to me!

myTokenizer <- function(x) { 
  NGramTokenizer(x, Weka_control(min=1, max=2)) } #unigrams and bigrams
good_dtm <- DocumentTermMatrix(good_corpus, control = list(tokenize = myTokenizer)) #create a DTM
good_slim_dtm <- removeSparseTerms(good_dtm, .995) # used more harshly to limit predictors, per notes. N/k ratio ~3:1 (1374:283)
bad_dtm <- DocumentTermMatrix(bad_corpus, control = list(tokenize = myTokenizer)) #create a DTM
bad_slim_dtm <- removeSparseTerms(bad_dtm, .995) # used more harshly to limit predictors, per notes. N/k ratio ~ 3:1 (1396:405)

# Publication