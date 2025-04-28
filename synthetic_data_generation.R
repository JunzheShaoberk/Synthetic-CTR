library(tidyverse)
library("synthpop")
library(FNN)
set.seed(123)
trauma_truth = read.csv("df_selected_features.csv")
# trauma_truth = read.csv("true_data_subset.csv")

trauma_prediction = read.csv("synthetic_data_subset_LLM.csv")


# Define the numeric and factor columns based on Python output
numeric_columns <- c("age", "occupation", "fu_disposition_pod", "traveldistance", 
                      "vitals1_dbp", "dispo_cost", 
                     "treat_crystalloidvol", "fu_cryst_volume", 
                     "fu_total_cost", "fu_disposition_hd", "vitals1_hr", "vitals1_sbp")

factor_columns <- c("sex", "marital_status", "education", "i_seatbelt", "gcs_qualifier", 
                    "breath", "i_carseat", "respirations", "tempcontrol", "i_helmet", 
                    "scenecare", "trachealdeviation", "cable", "priorcare", 
                    "phq_depression_2", "pupils", "dispo_costcareimpede", "mixer", 
                    "i_alcohol_perpretrator", "i_overloading", "fu_obsrv_exam", 
                    "tetanus_wound", "i_intent", "tetanus", "i_activity", "chestrise", 
                    "airway", "emergency_finassist", "pregnant", "alcoholuse", 
                    "phq_depression", "prior_injury", "tv", "i_airbag", "tobaccouse", 
                    "transfer", "i_alcohol_patient")


# Transform NAs in factor columns to a separate factor level 'Missing'
df_transformed_truth <- trauma_truth %>%
  select(all_of(c(numeric_columns, factor_columns))) %>%
  mutate(across(all_of(factor_columns), ~ as.factor(ifelse(is.na(.), "Missing", as.character(.))))) %>%
  mutate(across(all_of(numeric_columns), as.numeric))

df_transformed_prediction <- trauma_prediction %>%
  select(all_of(c(numeric_columns, factor_columns))) %>%
  mutate(across(all_of(factor_columns), ~ as.factor(ifelse(is.na(.), "Missing", as.character(.))))) %>%
  mutate(across(all_of(numeric_columns), as.numeric))

# using R package synthpop to generate synthetic data
mysyn_df = syn(df_transformed_truth)
compare(mysyn_df, df_transformed_truth, stat = "counts")


multi.compare(mysyn_df, df_transformed_truth, var = "marital_status", by = "sex")
multi.compare(mysyn_df, df_transformed_truth, var = "marital_status", by = "i_carseat")



# make a pairwise correlation heatmap of df_transformed_truth, ordered and clusted by correlation
# for all columns including numeric and factor columns

# Compute the correlation matrix
df_transformed_numeric <- df_transformed_truth %>%
  mutate(across(all_of(factor_columns), as.numeric))

correlation_matrix <- cor(df_transformed_numeric, use = "pairwise.complete.obs")

# Perform hierarchical clustering on the correlation matrix
distance_matrix <- as.dist(1 - correlation_matrix) # Create a distance matrix
clustering <- hclust(distance_matrix) # Hierarchical clustering

# Order the correlation matrix according to the clustering
ordered_correlation_matrix <- correlation_matrix[clustering$order, clustering$order]

# Create a heatmap with the ordered correlation matrix
ordered_correlation_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable1") %>%
  pivot_longer(-variable1, names_to = "variable2", values_to = "correlation") %>%
  mutate(variable1 = factor(variable1, levels = rev(rownames(ordered_correlation_matrix))),
         variable2 = factor(variable2, levels = colnames(ordered_correlation_matrix))) %>%
  ggplot(aes(variable1, variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed() +
  labs(title = "Pairwise Correlation Heatmap with Clustering",
       x = "Variable 1",
       y = "Variable 2",
       fill = "Correlation") +
  theme(legend.position = "right")
# drop the source column
mysyn_df$syn <- mysyn_df$syn %>% select(-source)

df_transformed_numeric <- mysyn_df$syn %>%
  mutate(across(all_of(factor_columns), as.numeric))

correlation_matrix <- cor(df_transformed_numeric, use = "pairwise.complete.obs")

# Perform hierarchical clustering on the correlation matrix
distance_matrix <- as.dist(1 - correlation_matrix) # Create a distance matrix
clustering <- hclust(distance_matrix) # Hierarchical clustering

# Order the correlation matrix according to the clustering
ordered_correlation_matrix <- correlation_matrix[clustering$order, clustering$order]

# Create a heatmap with the ordered correlation matrix
ordered_correlation_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable1") %>%
  pivot_longer(-variable1, names_to = "variable2", values_to = "correlation") %>%
  mutate(variable1 = factor(variable1, levels = rev(rownames(ordered_correlation_matrix))),
         variable2 = factor(variable2, levels = colnames(ordered_correlation_matrix))) %>%
  ggplot(aes(variable1, variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed() +
  labs(title = "Pairwise Correlation Heatmap with Clustering",
       x = "Variable 1",
       y = "Variable 2",
       fill = "Correlation") +
  theme(legend.position = "right")

# Compute the correlation matrix
df_transformed_numeric <- df_transformed_prediction %>%
  mutate(across(all_of(factor_columns), as.numeric))

correlation_matrix <- cor(df_transformed_numeric, use = "pairwise.complete.obs")

# Perform hierarchical clustering on the correlation matrix
distance_matrix <- as.dist(1 - correlation_matrix) # Create a distance matrix
clustering <- hclust(distance_matrix) # Hierarchical clustering

# Order the correlation matrix according to the clustering
ordered_correlation_matrix <- correlation_matrix[clustering$order, clustering$order]

# Create a heatmap with the ordered correlation matrix
ordered_correlation_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable1") %>%
  pivot_longer(-variable1, names_to = "variable2", values_to = "correlation") %>%
  mutate(variable1 = factor(variable1, levels = rev(rownames(ordered_correlation_matrix))),
         variable2 = factor(variable2, levels = colnames(ordered_correlation_matrix))) %>%
  ggplot(aes(variable1, variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed() +
  labs(title = "Pairwise Correlation Heatmap Synthetic",
       fill = "Correlation") +
  theme(legend.position = "right")



# run regression for vitals1_sbp ~ age + sex + fu_cryst_volume + vitals1_hr + occupation + fu_disposition_hd + fu_disposition_pod + dispo_cost + traveldistance

# run the regression for both the truth and the prediction

# for the truth

model_truth = lm(vitals1_sbp ~ age + sex  +i_overloading+vitals1_dbp,  data = df_transformed_truth)

summary(model_truth)

# for the prediction

model_prediction = lm(vitals1_sbp ~ age + sex  +i_overloading+vitals1_dbp , data = mysyn_df$syn)

summary(model_prediction)

model_prediction_LLM = lm(vitals1_sbp ~ age + sex  +i_overloading+vitals1_dbp , data = df_transformed_prediction)

summary(model_prediction_LLM)





# Help me combine the two dataframe, add a new column to each dataframe to indicate the source of the data
# then use random forest to predict the source of the data

# add a new column to each dataframe to indicate the source of the data

df_transformed_truth$source = "truth"
mysyn_df$syn$source = "syn"

# combine the two dataframes

combined_df = rbind(df_transformed_truth, mysyn_df$syn)
combined_df$source <- as.factor(combined_df$source)

# Split the data into training and test sets
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(combined_df), 0.7 * nrow(combined_df)) # 70% training data
train_data <- combined_df[train_indices, ]
test_data <- combined_df[-train_indices, ]

# Define categorical features (column indices) if any
categorical_features <- c("sex", "marital_status", "education", "i_seatbelt", "gcs_qualifier", 
                          "breath", "i_carseat", "respirations", "tempcontrol", "i_helmet", 
                          "scenecare", "trachealdeviation", "cable", "priorcare", 
                          "phq_depression_2", "pupils", "dispo_costcareimpede", "mixer", 
                          "i_alcohol_perpretrator", "i_overloading", "fu_obsrv_exam", 
                          "tetanus_wound", "i_intent", "tetanus", "i_activity", "chestrise", 
                          "airway", "emergency_finassist", "pregnant", "alcoholuse", 
                          "phq_depression", "prior_injury", "tv", "i_airbag", "tobaccouse", 
                          "transfer", "i_alcohol_patient")  # Adjust this to include the indices of categorical features

# Create the CatBoost pool for training and testing data
train_pool <- catboost.load_pool(data = train_data[, -ncol(train_data)], # Exclude the target column
                                 label = as.numeric(train_data$source) - 1,  # Make labels start from 0
                                 cat_features = categorical_features)

test_pool <- catboost.load_pool(data = test_data[, -ncol(test_data)], # Exclude the target column
                                label = as.numeric(test_data$source) - 1,
                                cat_features = categorical_features)

# Define the model parameters for binary classification
params <- list(
  loss_function = 'Logloss',    # Logloss for binary classification
  eval_metric = 'AUC',          # AUC metric for evaluation
  iterations = 1000,            # Number of iterations
  learning_rate = 0.03,         # Learning rate
  depth = 6,                    # Depth of the tree
  random_seed = 42,             # Random seed for reproducibility
  verbose = 100                 # Print results every 100 iterations
)

# Train the CatBoost model
model <- catboost.train(train_pool, params = params)

# Predict on the test set
predictions <- catboost.predict(model, test_pool, prediction_type = 'Probability')

# check auc of the model predictions vs test_data$source

library(pROC)


actual_classes <- as.numeric(test_data$source) - 1

# Calculate and print the AUC
roc_obj <- roc(actual_classes, predictions)
auc_value <- auc(roc_obj)
print(paste('AUC:', auc_value))
#plot roc curve and apppend the auc value

plot(roc_obj, main = "ROC Curve")
text(0.8, 0.2, paste("AUC = ", round(auc_value, 2)), adj = c(0, 1))




# rewrite the above code to use df_transformed_prediction instead of mysyn_df$syn

# add a new column to each dataframe to indicate the source of the data

df_transformed_truth$source = "truth"
df_transformed_prediction$source = "prediction"

# combine the two dataframes

combined_df = rbind(df_transformed_truth, df_transformed_prediction)

combined_df$source <- as.factor(combined_df$source)


# Split the data into training and test

set.seed(123) # For reproducibility

train_indices <- sample(1:nrow(combined_df), 0.7 * nrow(combined_df)) # 70% training data

train_data <- combined_df[train_indices, ]

test_data <- combined_df[-train_indices, ]

# Define categorical features (column indices) if any


# Define categorical features (column indices) if any
categorical_features <- c("sex", "marital_status", "education", "i_seatbelt", "gcs_qualifier", 
                          "breath", "i_carseat", "respirations", "tempcontrol", "i_helmet", 
                          "scenecare", "trachealdeviation", "cable", "priorcare", 
                          "phq_depression_2", "pupils", "dispo_costcareimpede", "mixer", 
                          "i_alcohol_perpretrator", "i_overloading", "fu_obsrv_exam", 
                          "tetanus_wound", "i_intent", "tetanus", "i_activity", "chestrise", 
                          "airway", "emergency_finassist", "pregnant", "alcoholuse", 
                          "phq_depression", "prior_injury", "tv", "i_airbag", "tobaccouse", 
                          "transfer", "i_alcohol_patient")  # Adjust this to include the indices of categorical features

# Create the CatBoost pool for training and testing data
train_pool <- catboost.load_pool(data = train_data[, -ncol(train_data)], # Exclude the target column
                                 label = as.numeric(train_data$source) - 1,  # Make labels start from 0
                                 cat_features = categorical_features)

test_pool <- catboost.load_pool(data = test_data[, -ncol(test_data)], # Exclude the target column
                                label = as.numeric(test_data$source) - 1,
                                cat_features = categorical_features)

# Define the model parameters for binary classification
params <- list(
  loss_function = 'Logloss',    # Logloss for binary classification
  eval_metric = 'AUC',          # AUC metric for evaluation
  iterations = 1000,            # Number of iterations
  learning_rate = 0.03,         # Learning rate
  depth = 6,                    # Depth of the tree
  random_seed = 42,             # Random seed for reproducibility
  verbose = 100                 # Print results every 100 iterations
)

# Train the CatBoost model
model <- catboost.train(train_pool, params = params)

# Predict on the test set
predictions <- catboost.predict(model, test_pool, prediction_type = 'Probability')

# check auc of the model predictions vs test_data$source

library(pROC)


actual_classes <- as.numeric(test_data$source) - 1

# Calculate and print the AUC
roc_obj <- roc(actual_classes, predictions)
auc_value <- auc(roc_obj)
print(paste('AUC:', auc_value))
#plot roc curve and apppend the auc value

plot(roc_obj, main = "ROC Curve")
text(0.8, 0.2, paste("AUC = ", round(auc_value, 2)), adj = c(0, 1))



# save mysyn_df$syn to a csv file

write.csv(mysyn_df$syn, "synthetic_data_R.csv", row.names = FALSE)



# check privacy:

# Define a function to calculate the Exact Match Score
exact_match_score <- function(real_data, synthetic_data) {
  # Convert data frames to sets of unique rows
  real_set <- unique(real_data)
  synthetic_set <- unique(synthetic_data)
  
  # Find the intersection of real and synthetic sets
  common_records <- intersect(real_set, synthetic_set)
  
  # Calculate the exact match score
  score <- nrow(common_records) / nrow(real_set)
  
  return(score)
}

# drop the source column for df_transformed_truth

df_transformed_truth <- df_transformed_truth %>% select(-source)
score <- exact_match_score(df_transformed_truth, mysyn_df$syn)
print(paste("Exact Match Score:", score))

