# Load necessary libraries
library(tidyverse)
library(corrplot) # For correlation matrix visualization
library(cluster) # For k-means clustering
library(factoextra) # For visualizing clusters (optional, for enhanced cluster plots)

# Data Preprocessing
# ==================

# Load the dataset
data <- read.csv("~/Downloads/ObesityDataSet_raw_and_data_sinthetic.csv")

# Preprocess the data: calculate BMI and rename variables for clarity
data <- data %>%
  mutate(BMI = Weight / (Height^2)) %>%
  rename(Obesity_Level = NObeyesdad, Tech_Use_Time = TUE, Physical_Activity = FAF)

# Exploratory Data Analysis (EDA)
# ===============================

# Visualizing the distribution of BMI across obesity levels
ggplot(data, aes(x = Obesity_Level, y = BMI, fill = Obesity_Level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "BMI Distribution Across Obesity Levels", x = "Obesity Level", y = "BMI")

# Correlation matrix of numerical variables
numerical_data <- select(data, where(is.numeric))
correlation_matrix <- cor(numerical_data)
corrplot(correlation_matrix, method = "circle")

# Hypothesis Testing
# ==================

# Example: T-test to compare BMI between smokers and non-smokers
t_test_result <- t.test(BMI ~ Does_Smoke, data = data)
print(t_test_result)

# Predictive Modeling
# ===================

# Assuming we want to predict BMI based on other factors in the dataset
model <- lm(BMI ~ ., data = data)
summary(model)

# Creating a Scoring System
# ==========================

# Normalizing factors and creating a weighted score for obesity risk
data <- data %>%
  mutate(
    Physical_Activity_Norm = scale(Physical_Activity),
    Tech_Use_Time_Norm = scale(Tech_Use_Time),
    Veggie_Consump_Norm = scale(Veggie_Consump),
    Obesity_Risk_Score = (0.4 * Physical_Activity_Norm) + 
      (-0.3 * Tech_Use_Time_Norm) + 
      (0.3 * Veggie_Consump_Norm)
  )

# Inspecting the first few rows of the newly added Obesity Risk Score
head(data$Obesity_Risk_Score, 5)

# Conclusion: At this point, you can further analyze the Obesity_Risk_Score, 
# compare it across different groups, or use it as a feature in predictive modeling.

# Clustering Analysis
# ===================

# Example of performing k-means clustering on a few selected variables
set.seed(123)  # For reproducibility
clusters <- kmeans(select(data, Physical_Activity_Norm, Tech_Use_Time_Norm, Veggie_Consump_Norm), centers = 3)
data$Cluster <- as.factor(clusters$cluster)

# Visualizing the clusters
ggplot(data, aes(x = Physical_Activity_Norm, y = Tech_Use_Time_Norm, color = Cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Cluster Analysis on Obesity Risk Factors", x = "Physical Activity", y = "Tech Use Time")


# Load necessary libraries for decision tree
library(rpart)
library(rpart.plot)

# Preparing the data for the decision tree model
# Note: Make sure your Obesity_Level variable is a factor if it's not numeric
data$Obesity_Level <- as.factor(data$Obesity_Level)

# Building the decision tree model
# Adjust the formula to include the predictors relevant to your analysis
decision_tree_model <- rpart(Obesity_Level ~ Physical_Activity + Tech_Use_Time + Veggie_Consump, 
                             data = data, 
                             method = "class")

# Visualizing the decision tree
rpart.plot(decision_tree_model, main = "Decision Tree for Predicting Obesity Level", 
           extra = 102,  # Display splits and label leaves with class percentages
           under = TRUE, # Place node labels under the nodes when extra > 0
           faclen = 0)   # Do not abbreviate factor levels

# Evaluating the model
# You can split your data into training and testing sets to evaluate the model's performance
# Here is a simple split for demonstration purposes

set.seed(123) # For reproducibility
training_indices <- sample(1:nrow(data), 0.8 * nrow(data))
training_data <- data[training_indices, ]
testing_data <- data[-training_indices, ]

# Rebuilding the model with the training data
decision_tree_model_train <- rpart(Obesity_Level ~ Physical_Activity + Tech_Use_Time + Veggie_Consump,
                                   data = training_data, 
                                   method = "class")

# Making predictions on the testing set
predictions <- predict(decision_tree_model_train, newdata = testing_data, type = "class")

# Calculating accuracy
accuracy <- mean(predictions == testing_data$Obesity_Level)
print(paste("Accuracy of the decision tree model:", round(accuracy * 100, 2), "%"))
