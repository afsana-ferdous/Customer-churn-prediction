##### 1- Install all necessary packages 

install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("tidymodels")
install.packages("ranger")
install.packages("xgboost")
install.packages("lightgbm")
install.packages("vip")
install.packages("yardstick")
install.packages("patchwork")
install.packages("themis")
install.packages("bonsai")
install.packages("tidyr")
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(tidymodels)
library(ranger)    # Random Forest Engine
library(xgboost) # XGBoost Engine
library(lightgbm) 
library(vip)       # Variable Importance
library(yardstick) # Model Evaluation Metrics
library(patchwork) # Combining Plots
library(themis)
library(bonsai)
library(tidyr)
#-----------------------------------------------------------------------------------------------------------------------------------------------#

##### 2 Load data

ted_Poppy_info <- read_csv("C:/Users/kada803/OneDrive - The University of Auckland/businfo 704/project GROUP/ted and poppy metadata final.csv")
ted_poppy_data  <- read_csv("C:/Users/kada803/OneDrive - The University of Auckland/businfo 704/project GROUP/tedpoppydata_final.csv")

#-----------------------------------------------------------------------------------------------------------------------------------------------#


##### 3 EDA analysis 

## SUPPORT TICKET AND SATISFACTION SURVEY 

# Fixing Support Experience Categorization
ted_poppy_data$support_experience <- case_when(
  ted_poppy_data$support_ticket == "NotLast6Months" ~ "No Support",
  ted_poppy_data$support_ticket == "Last6Months" & ted_poppy_data$satisfaction_survey %in% c(4, 5) ~ "Resolved",
  ted_poppy_data$support_ticket == "Last6Months" & ted_poppy_data$satisfaction_survey %in% c(1, 2, 3) ~ "Unresolved",
  ted_poppy_data$support_ticket == "Last6Months" & ted_poppy_data$satisfaction_survey == "NoResponse" ~ "No Response",
  TRUE ~ "No Support"
)

# Re-visualizing Retention by Support Experience
ggplot(ted_poppy_data, aes(x = support_experience, fill = as.factor(retained_binary))) +
  geom_bar(position = "dodge") +
  labs(title = "Retention by Support Experience", x = "Support Experience", y = "Count", fill = "Retained") +
  theme_minimal()

# number of those who compained not in last 6 months 
# Count customers who did NOT make a complaint in the last 6 months
no_complaint_count <- sum(ted_poppy_data$support_ticket == "NotLast6Months", na.rm = TRUE)

# Print the result
print(paste("Number of customers who did NOT make a complaint in the last 6 months:", no_complaint_count))


## SUBSCRIPTION PAYMENT PROBLEM AND PAYMENT TYPE  

# Payment Risk Categorization (Separate Credit & Debit Cards)
ted_poppy_data$payment_risk <- case_when(
  ted_poppy_data$subscription_payment_problem_last6Months == TRUE & ted_poppy_data$payment_type == "CreditCard" ~ "Very High",
  ted_poppy_data$subscription_payment_problem_last6Months == TRUE & ted_poppy_data$payment_type == "DebitCard" ~ "High",
  ted_poppy_data$subscription_payment_problem_last6Months == TRUE & ted_poppy_data$payment_type == "ApplePay" ~ "Medium",
  TRUE ~ "Low"  # No payment issues
)

# Check Payment Risk Distribution
table(ted_poppy_data$payment_risk, useNA = "ifany")

# Re-Visualize Retention by Payment Risk 
ggplot(ted_poppy_data, aes(x = payment_risk, fill = as.factor(retained_binary))) +
  geom_bar(position = "dodge") +
  labs(title = "Retention by Payment Risk (Separated Credit & Debit)",
       x = "Payment Risk",
       y = "Count",
       fill = "Retained") +
  theme_minimal()

## sUBSCRIPTION FREQUENCY, APP VISITS AND WEBSITE VISITS  


# Fix Subscription Weight Calculation
ted_poppy_data$sub_weight <- case_when(
  ted_poppy_data$subscription_frequency == "Weekly" ~ 1,
  ted_poppy_data$subscription_frequency == "Fortnightly" ~ 1.5,  
  ted_poppy_data$subscription_frequency == "Monthly" ~ 2, 
  ted_poppy_data$subscription_frequency == "6Weekly" ~ 2.5,
  TRUE ~ NA_real_  # Keep NA for now to check where it's missing
)

# Check for Missing Values in Subscription Weight
sum(is.na(ted_poppy_data$sub_weight))  # How many missing sub_weight values?

# ix Missing Subscription Weights
# If there are missing subscription weights, assign the most common (mode)
most_common_weight <- as.numeric(names(sort(table(ted_poppy_data$sub_weight), decreasing = TRUE)[1]))
ted_poppy_data$sub_weight[is.na(ted_poppy_data$sub_weight)] <- most_common_weight

# compute Subscription Interaction Rate (SIR)
ted_poppy_data$SIR <- (ted_poppy_data$app_visits + ted_poppy_data$website_visits) / ted_poppy_data$sub_weight

# andle NA Values in SIR
# Option 1: Replace NA with the median SIR value
median_SIR <- median(ted_poppy_data$SIR, na.rm = TRUE)
ted_poppy_data$SIR[is.na(ted_poppy_data$SIR)] <- median_SIR

# Convert SIR into Categorical Levels
ted_poppy_data$interaction_category <- cut(
  ted_poppy_data$SIR, 
  breaks = quantile(ted_poppy_data$SIR, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE), 
  labels = c("Low", "Moderate", "High"), 
  include.lowest = TRUE
)

# Verify the Fix
table(ted_poppy_data$interaction_category, useNA = "ifany")  # Check for remaining NA values

# Re-Visualize the Graph
ggplot(ted_poppy_data, aes(x = interaction_category, fill = as.factor(retained_binary))) +
  geom_bar(position = "dodge") +
  labs(title = "Retention by Subscription Interaction Rate",
       x = "Subscription Interaction Category",
       y = "Count",
       fill = "Retained") +
  theme_minimal()

#-----------------------------------------------------------------------------------------------------------------------------------------------#

##### 4 ADVANCED ANAYTICS 

# Set Seed for Reproducibility
set.seed(123)

# Load Data & Preprocessing
data <- ted_poppy_data %>%
  select(subscription_frequency, support_ticket, satisfaction_survey, subscription_payment_problem_last6Months,
         payment_type,app_visits,website_visits,  retained_binary) %>%
  
  mutate(
    across(where(is.character), as.factor),
    across(where(is.logical), as.factor),  # Convert logical vars to factor
    retained_binary = factor(retained_binary, levels = c("0", "1"), labels = c("Churned", "Retained")) # Ensure "Retained" is the positive class
  )

# Train-Test Split (75% Train, 25% Test)
set.seed(222)  # Ensure reproducibility
data_split <- initial_split(data, prop = 0.75, strata = retained_binary)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define Recipe for Preprocessing (SMOTE included)
data_recipe <- recipe(retained_binary ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%  # Convert categorical vars to dummy vars
  step_normalize(all_numeric_predictors()) %>%
  step_smote(retained_binary)  # Balancing classes with SMOTE

# Define Model Specifications
rf_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

xgb_spec <- boost_tree(trees = 500, tree_depth = 6, learn_rate = 0.1) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

lgbm_spec <- boost_tree(trees = 500, tree_depth = 6, learn_rate = 0.1) %>%
  set_engine("lightgbm") %>%
  set_mode("classification")

# Fit Models Using Correct Workflow
rf_fit <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(rf_spec) %>%
  fit(data = train_data)

xgb_fit <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(xgb_spec) %>%
  fit(data = train_data)

lgbm_fit <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(lgbm_spec) %>%
  fit(data = train_data)

# Predictions
rf_pred <- predict(rf_fit, new_data = test_data, type = "prob") %>% bind_cols(test_data)
xgb_pred <- predict(xgb_fit, new_data = test_data, type = "prob") %>% bind_cols(test_data)
lgbm_pred <- predict(lgbm_fit, new_data = test_data, type = "prob") %>% bind_cols(test_data)

# Convert Probabilities to Class Labels
rf_pred <- rf_pred %>%
  mutate(.pred_class = ifelse(.pred_Retained > 0.5, "Retained", "Churned") %>% factor(levels = c("Churned", "Retained")))

xgb_pred <- xgb_pred %>%
  mutate(.pred_class = ifelse(.pred_Retained > 0.5, "Retained", "Churned") %>% factor(levels = c("Churned", "Retained")))

lgbm_pred <- lgbm_pred %>%
  mutate(.pred_class = ifelse(.pred_Retained > 0.5, "Retained", "Churned") %>% factor(levels = c("Churned", "Retained")))

# Evaluate Performance (Fixing Event Levels)
roc_rf <- roc_auc(rf_pred, truth = retained_binary, .pred_Retained, event_level = "second")
roc_xgb <- roc_auc(xgb_pred, truth = retained_binary, .pred_Retained, event_level = "second")
roc_lgbm <- roc_auc(lgbm_pred, truth = retained_binary, .pred_Retained, event_level = "second")

accuracy_rf <- accuracy(rf_pred, truth = retained_binary, estimate = .pred_class)
accuracy_xgb <- accuracy(xgb_pred, truth = retained_binary, estimate = .pred_class)
accuracy_lgbm <- accuracy(lgbm_pred, truth = retained_binary, estimate = .pred_class)

# Additional Model Evaluation Metrics
metrics_rf <- rf_pred %>%
  metrics(truth = retained_binary, estimate = .pred_class)

metrics_xgb <- xgb_pred %>%
  metrics(truth = retained_binary, estimate = .pred_class)

metrics_lgbm <- lgbm_pred %>%
  metrics(truth = retained_binary, estimate = .pred_class)

# Selecting Best Model Based on AUC-ROC Score
auc_scores <- c("Random Forest" = roc_rf$.estimate,
                "XGBoost" = roc_xgb$.estimate,
                "LightGBM" = roc_lgbm$.estimate)

best_model <- names(which.max(auc_scores))

# Extract feature importance
importance_df <- extract_fit_parsnip(rf_fit) %>% vi()

# Clean up variable names (Group by meaningful categories)
importance_df <- importance_df %>%
  mutate(Variable = case_when(
    grepl("satisfaction_survey", Variable) ~ "Satisfaction Survey",
    grepl("support_ticket", Variable) ~ "Support Ticket",
    grepl("subscription_payment_problem_last6Months", Variable) ~ "Payment Problem",
    grepl("payment_type", Variable) ~ "Payment Type",
    grepl("app_visits", Variable) ~ "App Visits",
    grepl("webiste_visits", Variable) ~ "Website Visits",
    
    
    grepl("subscription_frequency", Variable) ~ "Subscription Frequency",
    TRUE ~ Variable
  )) %>%
  group_by(Variable) %>%
  summarise(Importance = sum(Importance, na.rm = TRUE)) %>%
  arrange(desc(Importance))

# Plot Aggregated Feature Importance
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Overall Feature Importance for Churn Prediction",
       x = "Features",
       y = "Relative Importance") +
  theme_minimal()


# Compare ROC Curves
rf_roc <- rf_pred %>% roc_curve(truth = retained_binary, .pred_Retained, event_level = "second") %>% mutate(Model = "Random Forest")
xgb_roc <- xgb_pred %>% roc_curve(truth = retained_binary, .pred_Retained, event_level = "second") %>% mutate(Model = "XGBoost")
lgbm_roc <- lgbm_pred %>% roc_curve(truth = retained_binary, .pred_Retained, event_level = "second") %>% mutate(Model = "LightGBM")

combined_roc <- bind_rows(rf_roc, xgb_roc, lgbm_roc)

ggplot(combined_roc, aes(x = 1 - specificity, y = sensitivity, color = Model)) +
  geom_line(size = 1.2) +
  labs(title = "Comparison of Churn Prediction Models",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

ggplot(rf_roc, aes(x = 1 - specificity, y = sensitivity, color = Model)) +
  geom_line(size = 1.2) +
  labs(title = "Comparison of Churn Prediction Models",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

ggplot(xgb_roc, aes(x = 1 - specificity, y = sensitivity, color = Model)) +
  geom_line(size = 1.2) +
  labs(title = "Comparison of Churn Prediction Models",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

ggplot(lgbm_roc, aes(x = 1 - specificity, y = sensitivity, color = Model)) +
  geom_line(size = 1.2) +
  labs(title = "Comparison of Churn Prediction Models",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

# Print feature importance to check if subscription frequency is missing
print(importance_df)

# Get Confusion Matrix for Random Forest
rf_conf_mat <- conf_mat(rf_pred, truth = retained_binary, estimate = .pred_class)
print(rf_conf_mat)

# Extract Sensitivity, Accuracy, and PPV for all models
performance_metrics <- tibble(
  Model = c("Random Forest", "XGBoost", "LightGBM"),
  Accuracy = c(
    accuracy(rf_pred, truth = retained_binary, estimate = .pred_class)$.estimate,
    accuracy(xgb_pred, truth = retained_binary, estimate = .pred_class)$.estimate,
    accuracy(lgbm_pred, truth = retained_binary, estimate = .pred_class)$.estimate
  ),
  Sensitivity = c(
    sens(rf_pred, truth = retained_binary, estimate = .pred_class)$.estimate,
    sens(xgb_pred, truth = retained_binary, estimate = .pred_class)$.estimate,
    sens(lgbm_pred, truth = retained_binary, estimate = .pred_class)$.estimate
  ),
  PPV = c(
    ppv(rf_pred, truth = retained_binary, estimate = .pred_class)$.estimate,
    ppv(xgb_pred, truth = retained_binary, estimate = .pred_class)$.estimate,
    ppv(lgbm_pred, truth = retained_binary, estimate = .pred_class)$.estimate
  )
)

# Print Performance Metrics
print(performance_metrics)




