# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)
library(jtools) # APA formatting for ggplot2!

# Data Import and Cleaning
statisical_tbl <- readRDS(file = "../out/fulldataset.rds") %>%
  mutate(across(.cols = c(Attrition, Department, BusinessTravel, Gender, EducationField, JobRole, MaritalStatus, Over18, OverTime), .fns = ~ factor(.)))

# Analysis
## H1 - No base functions allowed
h1_cor <- cor_test(data = statisical_tbl, vars = c(MonthlyIncome, PerformanceRating), use = "everything") # Could also use MonthlyRate? Chose Income as a better interpretation of "monthly pay". These two potential pay variables can be very different from each other for each person in the dataset
h1_cor

## H2 - No base functions allowed
h2_anova <- anova_test(data = statisical_tbl, formula = MonthlyIncome ~ Department, dv = MonthlyRate, wid = employee_id, detailed = TRUE)
h2_anova

## H3 - Base functions allowed
h3_lm <- lm(YearsAtCompany ~ RelationshipSatisfaction*Gender, data = statisical_tbl)
h3_lm_result <- summary(h3_lm)
h3_lm_result
h3_predicted_tbl <- statisical_tbl %>%
  add_column(predicted_YearsAtCompany = predict.lm(h3_lm))

# Visualization
## Visualization for H1
(ggplot(statisical_tbl, aes(x = PerformanceRating, y = MonthlyIncome)) +
    #geom_point(position = "jitter", size= 0.5) +
    geom_jitter(width=.2, size = 0.5) + 
    geom_smooth(method = lm, se = FALSE, color = "black", linewidth = .75) +
    labs(x = "Performance Rating", y = "Monthly Income") +
    scale_x_continuous(limits = c(2.8,4.2), breaks = c(3,4)) + #expanded slightly for jitter
    theme_apa() #no title because titles are not supposed to be on plots, they appear in a heading above, per https://apastyle.apa.org/style-grammar-guidelines/tables-figures/figures
) %>%
  ggsave(filename = "../figs/fig_H1.png", dpi = 300, width = 1920, height = 1080, units = "px") # 1920 x 1080 used per notes

## Visualization for H2
(ggplot(statisical_tbl, aes(x = Department, y = MonthlyIncome)) +
    geom_boxplot(outlier.size = 1) +
    labs(x = "Department", y = "Monthly Income") +
    theme_apa() 
  ) %>%
  ggsave(filename = "../figs/fig_H2.png", dpi = 300, width = 1920, height = 1080, units = "px")

## Visualization for H3: Predicted Tenure
(ggplot(h3_predicted_tbl, aes(x = RelationshipSatisfaction, y = predicted_YearsAtCompany, group = Gender, color = Gender)) +
    #geom_point(size= 0.5) +
    geom_jitter(width=.2, size = 0.5) + 
    geom_smooth(method = lm, se = FALSE) +
    labs(x = "Relationship Satisfaction", y = "Predicted Tenure (Years)") +
    theme_apa() 
) %>%
  ggsave(filename = "../figs/fig_H3.png", dpi = 300, width = 1920, height = 1080, units = "px")
# ### Actual Values
# (ggplot(statisical_tbl, aes(x = RelationshipSatisfaction, y = YearsAtCompany, group = Gender, color = Gender)) +
#     #geom_point(size= 0.5) +
#     geom_jitter(width=.2, size = 0.5) + 
#     geom_smooth(method = lm, se = FALSE) +
#     labs(x = "Relationship Satisfaction", y = "Tenure (years)") +
#     theme_apa() 
# ) %>%
#   ggsave(filename = "../figs/fig_H3.png", dpi = 300, width = 1920, height = 1080, units = "px")

# Publication
### H1 Table
h1_tbl <- tibble(h1_cor) %>%
  mutate(across(c(cor:conf.high), ~ str_remove(format(round(., 2), nsmall = 2), "0"))) %>%
  select(-method)
h1_tbl
write_csv(h1_tbl, file = "../out/H1.csv")
## H1 Interpretation
ifelse(abs(as.numeric("1")) < 1, paste0("The correlation for H1, the relationship between Performance Rating and Monthly Income, was r(", nrow(statisical_tbl),") = ", h1_tbl$cor,", p-value = ",h1_tbl$p,". Therefore, H1 was ",ifelse(h1_cor$p > .05, "not ", ""),"supported."), paste0("Something has gone awry! Calculated correlation > 1."))

## H2 Table
h2_tbl <- tibble(Component = h2_anova$Effect, SSn = h2_anova$SSn, SSd = h2_anova$SSd, DFn = h2_anova$DFn, DFd = h2_anova$DFd, F = h2_anova$F, p = h2_anova$p) %>%
  mutate(across(c(F:p), ~ str_remove(format(round(., 2), nsmall = 2), "^0")))
h2_tbl
write_csv(h2_tbl, file = "../out/H2.csv")

## H2 Interpretation
paste0("The F-statisic for H2 was F(", h2_tbl$DFn,", ",h2_tbl$DFn,") = ",h2_tbl$F,", p-value = ",h2_tbl$p,". Therefore, H2 was ",ifelse(h2_anova$p > .05, "not ", ""),"supported.")

## H3 Table
h3_tbl <- tibble('Coefficient' = c("Intercept", "Relationship Satisfaction", "Gender", "Interaction:Gender*Rel. Sat."), "Estimate" = h3_lm_result$coefficients[,"Estimate"],'Std Error' = h3_lm_result$coefficients[,"Std. Error"], 't-value' = h3_lm_result$coefficients[,"t value"], 'p' = h3_lm_result$coefficients[,"Pr(>|t|)"]) %>%
  mutate(across(c(2:5), ~ str_remove(format(round(., 2), nsmall = 2), "0")))
h3_tbl
write_csv(h3_tbl, file = "../out/H3.csv")

## H3 Interpretation
sentence <- function (name_in_quotes, h3_tibblerow){
  words <- paste0("The ", name_in_quotes,
  " for model H3 was ",
  ifelse(h3_lm_result$coefficients[[h3_tibblerow,"Pr(>|t|)"]] > .05, "not ", ""),
  "significant, as t(",nrow(statisical_tbl),
  ") = ", 
  h3_tbl[[h3_tibblerow,3]],
  #", p-value = ", 
  ", p-value ", 
  ifelse(h3_lm_result$coefficients[[h3_tibblerow,"Pr(>|t|)"]] < .01, "< .01", paste0("= ", h3_tbl$p[[h3_tibblerow]])),
  #h3_tbl$p[[h3_tibblerow] d],
  ".")
  print(words)
} #replaced commented code below, built to improve repeat code. Found out after re-reading that it wasn't necessary, I think? But didn't want to delete it. Super proud of this!
sentence("intercept", 1)
sentence("coefficient for Relationship Satisfaction", 2)
sentence("coefficient for Gender", 3)
sentence("interaction effect between Satisfaction and Gender", 4) 

# paste0("The Intercept for model H3 was ",ifelse(h3_lm_result$coefficients[[1,"Pr(>|t|)"]] > .05, "not ", ""),"significant, as t(",nrow(statisical_tbl),") = ", h3_tbl[[1,3]],", p-value = ", h3_tbl$p[[1]],".")
# 
# paste0("The coefficient for Relationship Satisfaction in model H3 was ",ifelse(h3_lm_result$coefficients[[2,"Pr(>|t|)"]] > .05, "not ", ""),"significant, as t(",nrow(statisical_tbl),") = ", h3_tbl[[2,3]],", p-value = ", h3_tbl$p[[2]],".")
# 
# paste0("The coefficient for Gender in model H3 was ",ifelse(h3_lm_result$coefficients[[3,"Pr(>|t|)"]] > .05, "not ", ""),"significant, as t(",nrow(statisical_tbl),") = ", h3_tbl[[3,3]],", p-value = ", h3_tbl$p[[3]],".")
# 
# paste0("The coefficient for the interaction between Relationship Satisfaction and Gender in model H3 was ",ifelse(h3_lm_result$coefficients[[4,"Pr(>|t|)"]] > .05, "not ", ""),"significant, as t(",nrow(statisical_tbl),") = ", h3_tbl[[4,3]],", p-value = ", h3_tbl$p[[4]],".")