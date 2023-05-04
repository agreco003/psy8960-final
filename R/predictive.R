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
library(caret)

# Data Import and Cleaning for NLP and ML
## Approach Explanation --> Using NLP to code comments into discrete topics, automating a process I would have done manually for surveys like these in the past. These topics will then be used in various Machine Learning models to complete a classification task: predicting turnover. Further, I have made the decision to treat good and bad comments as separate corpi -- a comment with "nothing" in the good column would be very different than a "nothing" in the bad column, and I want to retain similar distinctions in my dataset. Regex or sentiment analysis would be alternatives, but this is the path that I thought would work best! 

# predictive_tbl <- readRDS(file = "../data/fulldataset.rds") Read original dataset
# lapply(predictive_tbl, class) # check original class of each column
predictive_tbl <- readRDS(file = "../data/fulldataset.rds") %>% # clean dataset
  mutate(Attrition = factor(str_replace_all(Attrition, c("No" = "0", "Yes" = "1"))),
         BusinessTravel = as.numeric(factor(str_replace_all(BusinessTravel, c("Non-Travel" = "0", "Travel_Rarely" = "1", "Travel_Frequently" = "2")))),
         Gender = as.numeric(str_replace_all(Gender, c("Male" = "0", "Female" = "1"))),
         OverTime = as.numeric(str_replace_all(OverTime, c("No" = "0", "Yes" = "1"))), # the above variables have meaningful zeros for specific values, which are retained
         across(.cols = c(Department, EducationField, JobRole, MaritalStatus, Over18), .fns = ~ as.numeric(factor(.)))) # converting them all to numerics for ML, while also building factor structure
# ,
#          EducationField = as.numeric(factor(EducationField)),
#          JobRole = as.numeric(factor(JobRole)),
#          MaritalStatus = as.numeric(factor(MaritalStatus)),
#          Over18 = as.numeric(factor(Over18)),  
#   )
#####
# lapply(predictive_tbl, class) # check class of each column

## Comment Data Intial check
## Number of missing values
# predictive_tbl %>% 
#   filter(q_good != is.na(q_good)) %>% 
#   nrow() ## of observations for bad comments: 1374
# predictive_tbl %>% 
#   filter(q_bad != is.na(q_bad)) %>% 
#   nrow() # observations for bad comments: 1396

## Pre-processing function, tokenizer function
pre_processing <- function(corpus) {
  corpus %>% 
    tm_map(content_transformer(replace_contraction)) %>% # contractions replaced
    tm_map(content_transformer(replace_abbreviation)) %>% # abbreviations replaced
    tm_map(content_transformer(replace_number)) %>% #numbers replaced but retained in case they are meaningful
    tm_map(content_transformer(str_to_lower)) %>% #all lower case
    tm_map(removePunctuation) %>% #remove punctuation
    tm_map(removeWords, c(stopwords(kind = "en"), "google")) %>% #removed variations of Google, the company name
    tm_map(content_transformer(lemmatize_words)) %>% #used a preset dictionary to homogenize word forms
    tm_map(stripWhitespace) %>% #reduced all whitespace to a single space whenever it occurs
    tm_filter(FUN = function(x) {return(nchar(stripWhitespace(x$content)[[1]]) > 1) }) #remove blank cases for LDA
} # wrote a function because this is long!
myTokenizer <- function(x) { 
  NGramTokenizer(x, Weka_control(min=1, max=2)) 
} #unigrams and bigrams

### Good comments Corpus
good_corpus_original <- VCorpus(VectorSource(predictive_tbl$q_good))
good_corpus <- pre_processing(good_corpus_original) # Cases count remains the same: 1374

### Bad comments Corpus
bad_corpus_original <- VCorpus(VectorSource(predictive_tbl$q_bad))
bad_corpus <- pre_processing(bad_corpus_original) # Cases count remains the same: 1396

### Pre-processing check - comment out final tm_filter line, and the pipe that preceeds it, in pre_processing function above. Then rerun good_corpus and bad_corpus to use code below
# compare_them <- function(x, y) { 
#    index <- sample(1:length(x),1) #set an index
#    print(x[[index]]$content) #print content of index case x
#    print(y[[index]]$content) #print content of index case y
#  }
# compare_them(good_corpus_original, good_corpus) #run as many times you like for checks. Looks good to me!
# compare_them(bad_corpus_original, bad_corpus) #run as many times you like for checks. Looks good to me!

## DTMs
good_dtm <- DocumentTermMatrix(good_corpus, control = list(tokenize = myTokenizer)) #create a DTM
bad_dtm <- DocumentTermMatrix(bad_corpus, control = list(tokenize = myTokenizer)) #create a DTM

# Analysis
## NLP with each corpus separately for distinct categories
### Identifying number of topics
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster) # Run across multiple cores! Faster.
tuning_good <- FindTopicsNumber(good_dtm,
                           topics = seq(2, 10, by = 1), #originally set between 3 and 30 by 3, then this final restricted range, looking for alignment across models
                           metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), 
                           verbose = T,
                           control = list(seed =10))
tuning_bad <- FindTopicsNumber(bad_dtm,
                                topics = seq(2, 10, by = 1), #originally set between 3 and 30 by 3, then this final restricted range, looking for alignment across models
                                metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), 
                                verbose = T,
                                control = list(seed = 10))
FindTopicsNumber_plot(tuning_good) # 5 or 6 topics
FindTopicsNumber_plot(tuning_bad) # 4 - 6 topics, 5 looks best
stopCluster(local_cluster) 
registerDoSEQ() #closes clusters

### LDA Good comments
lda_good_results <- LDA(good_dtm, k = 6, method = "Gibbs", control = list(seed = 25)) # with 6 topics, 5 would also be reasonable for this, but made somewhat less sense when looking at betas, commented out below
# lda_good_betas <- tidy(lda_good_results, matrix="beta") %>%  # Reviewed but not needed for analysis
#   group_by(topic) %>%
#   slice_max(n = 5, beta) %>% #top 5 words per topic
#   arrange(topic, -beta) # probability that word(s) belong in a topic
# lda_good_betas
lda_good_gammas <- tidy(lda_good_results, matrix="gamma") %>% 
  group_by(document) %>% 
  slice_max(n = 1, gamma, with_ties = FALSE) %>% #highest 3 probability per document only, removes ties
  mutate(employee_id = as.numeric(document),
         good_topic = topic) %>%
  arrange(employee_id) # probabilities of document containing the topic

### LDA Bad comments
lda_bad_results <- LDA(bad_dtm, k = 5, method = "Gibbs", control = list(seed = 25)) # with 5 topics, 4 or 6 would also be reasonable for this per above
# lda_bad_betas <- tidy(lda_bad_results, matrix="beta") %>% # Reviewed but not needed for analysis
#   group_by(topic) %>%
#   slice_max(n = 5, beta) %>% #top 10 words per topic
#   arrange(topic, -beta) # probability that word(s) belong in a topic
# lda_bad_betas
lda_bad_gammas <- tidy(lda_bad_results, matrix="gamma") %>% 
  group_by(document) %>% 
  slice_max(n = 1, gamma, with_ties = FALSE) %>% #highest probability per document only, removes ties
  mutate(employee_id = as.numeric(document),
         bad_topic = topic) %>%
  arrange(employee_id)

### Create a table to interpret results
full_predictive_tbl <- predictive_tbl %>%
  full_join(y = lda_bad_gammas, by = join_by(employee_id)) %>%
  full_join(y = lda_good_gammas, by = join_by(employee_id)) %>%
  select(-gamma.x, -document.x, -topic.x, -gamma.y, -document.y, -topic.y, -q_good, -q_bad) # good and bad topic numbers retained, but comments themselves dropped. Document originally checked to ensure cases matched, which they did!

## Machine Learning models -> Classification task
set.seed(25)
partition <- createDataPartition(full_predictive_tbl$Attrition, p = 0.75, list = FALSE)
train_tbl <- full_predictive_tbl[partition, ]
train_nocomment_tbl <- predictive_tbl[partition, ] %>%
  select(-q_good, -q_bad) # Used for publication section later, removes comments from training model and analysis. Same training cases as the full model
holdout_tbl <- full_predictive_tbl[-partition, ]
fold_indices = createFolds(train_tbl$Attrition, k = 10)

local_cluster <- makeCluster(7)
registerDoParallel(local_cluster) # Run across multiple cores! Faster.
### Elastic Net Model
en_model <- train(
  Attrition ~ .,
  data = train_tbl, 
  method = "glmnet",
  tuneLength = 3,
  na.action = "na.pass", 
  preProcess = c("nzv", "center", "scale", "bagImpute"), #3 columns have no variance
  trControl =  trainControl(
    method = "cv",
    indexOut = fold_indices,
    verboseIter = TRUE
  )
)
en_model
en_predict <- predict(en_model, holdout_tbl, na.action=na.pass)

## Random Forest Model
rf_model <- train(
  Attrition ~ .,
  data = train_tbl, 
  tuneLength = 3, #3 hyperparameters total
  na.action = "na.pass", 
  preProcess = c("nzv", "center", "scale", "bagImpute", "pca"),
  method = "ranger", #runs random forest model, classification task
  trControl = trainControl(
    method = "cv", 
    indexOut = fold_indices,
    verboseIter = TRUE,
  )
)
rf_model
rf_predict <- predict(rf_model, holdout_tbl, na.action=na.pass)

## eXtreme Gradient Boosting Model
gb_model <- train(
  Attrition ~ .,
  data = train_tbl,
  method = "xgbTree",
  tuneLength = 3,
  na.action = "na.pass",
  preProcess = c("nzv", "center", "scale", "bagImpute", "pca"), 
  trControl =  trainControl(
    method = "cv",
    indexOut = fold_indices,
    verboseIter = TRUE
  )
)
gb_model
gb_predict <- predict(gb_model, holdout_tbl, na.action=na.pass)
stopCluster(local_cluster) 
registerDoSEQ()

## Best Performing Model without Text Data (added later)
en_model_nocomment <- train(
  Attrition ~ .,
  data = train_nocomment_tbl,
  method = "glmnet",
  tuneLength = 3,
  na.action = "na.pass", 
  preProcess = c("nzv", "center", "scale", "bagImpute"), #3 columns have no variance
  trControl =  trainControl(
    method = "cv",
    indexOut = fold_indices,
    verboseIter = TRUE
  )
)
en_model_nocomment
en_nocomment_predict <- predict(en_model_nocomment, holdout_tbl, na.action=na.pass)

# Publication
model_list <- list(ElasticNet = en_model, RandomForest = rf_model, GradientBoostingTree = gb_model, EN_NoComments = en_model_nocomment)
model_results <- summary(resamples(x = model_list))
ho_en_results <- confusionMatrix(data = en_predict, reference = holdout_tbl$Attrition)
ho_en_results
ho_rf_results <- confusionMatrix(data = rf_predict, reference = holdout_tbl$Attrition)
ho_rf_results
ho_gb_results <- confusionMatrix(data = en_nocomment_predict, reference = holdout_tbl$Attrition)
ho_gb_results
ho_en_simple_results <- confusionMatrix(data = gb_predict, reference = holdout_tbl$Attrition)
ho_en_simple_results
#caTools::colAUC(X=as.numeric(gb_predict), y = holdout_tbl$Attrition, plotROC=TRUE)
#caTools::colAUC(X=as.numeric(gb_predict), y = holdout_tbl$Attrition, plotROC=TRUE)
#caTools::colAUC(X=as.numeric(gb_predict), y = holdout_tbl$Attrition, plotROC=TRUE) checked each of these plots as an additional check, glm still outperforming the others

## Create full tibble
cv_accuracy <- model_results$statistics$Accuracy[,"Mean"] #mean values used because they correspond with each selected model
ho_accuracy <- c(ho_en_results$overall[[1]], ho_rf_results$overall[[1]], ho_gb_results$overall[[1]], ho_en_simple_results$overall[[1]])
cv_kappa <- model_results$statistics$Kappa[,"Mean"]
ho_kappa <- c(ho_en_results$overall[[2]], ho_rf_results$overall[[2]], ho_gb_results$overall[[2]], ho_en_simple_results$overall[[2]])

full_ml_tbl <- tibble(algo = model_results$models, cv_accuracy, cv_kappa, ho_accuracy, ho_kappa) %>%
  mutate(across(c(cv_accuracy:ho_kappa), ~ str_remove(format(round(., 2), nsmall = 2), "^0"))) #tbl filtered twice in remaining code
  
ML_models_comments_tbl <- filter(full_ml_tbl, algo != "EN_NoComments")
ML_models_comments_tbl
## Original Model Selection and Rationale: The glm_net model outperforms the others! Both Accuracy and Kappa are relatively stable from the cv sample to  the holdout sample, when compared with the others. glmnet punishes model complexity, and can help prevent overfitting, something the other models suffered form. 

## Best Model, with and without comments: 
EN_ML_nocomments_tbl <- filter(full_ml_tbl, algo %in% c("EN_NoComments", "ElasticNet"))
EN_ML_nocomments_tbl
## The comment-included model, Elastic Net, performs slightly better, but is functionally about the same. It demonstrates an prediction accuracy in our holdout sample, and a higher kappa overall in that same sample. This suggests the with-comment model may be a little more robust if it were asked to predict new cases from the same population. 