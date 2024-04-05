# Load necessary libraries
library(tidyverse)
library(corrplot) # For correlation matrix visualization
library(cluster) # For k-means clustering
library(factoextra) # For visualizing clusters (optional, for enhanced cluster plots)

# Data Preprocessing
# ==================

# Load the dataset
data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")

# Preprocess the data: calculate BMI and rename variables for clarity
data <- data %>%
  mutate(BMI = Weight / (Height^2)) %>%
  rename(
    Obesity_Level = NObeyesdad, Transportation_Use = MTRANS, Alcohol_Consump = CALC,
    Tech_Time = TUE, Physical_Activ_Amt = FAF, Monitor_Calories = SCC, Water_Consump = CH2O,
    Does_Smoke = SMOKE, Food_bw_Meals = CAEC, Main_Meal_Consump = NCP, Veggie_Consump = FCVC,
    HiCal_Food_Consump = FAVC, Family_History_w_Overweight = family_history_with_overweight
  )
  

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

data <- data %>%
  mutate(
    # Score for Physical Activity: Higher activity, lower score
    Physical_Activity_Score = case_when(
      Physical_Activ_Amt <= 1 ~ 3,
      Physical_Activ_Amt > 1 & Physical_Activ_Amt <= 2 ~ 2,
      Physical_Activ_Amt > 2 ~ 1
    ),
    # Score for Tech Time: More tech time, higher score
    Tech_Time_Score = case_when(
      Tech_Time <= 1 ~ 1,
      Tech_Time > 1 & Tech_Time <= 2 ~ 2,
      Tech_Time > 2 ~ 3
    ),
    # Score for Vegetable Consumption: More veggies, lower score
    Veggie_Consumption_Score = case_when(
      Veggie_Consump < 2 ~ 3,
      Veggie_Consump >= 2 & Veggie_Consump <= 3 ~ 2,
      Veggie_Consump > 3 ~ 1
    ),
    # Score for Family History with Overweight: Presence of history, higher score
    Family_History_Score = case_when(
      Family_History_w_Overweight == "yes" ~ 3,
      TRUE ~ 1 # Assuming 'no' or other values indicate lower risk
    ),
    # Aggregate Obesity Risk Score: Adjust weights as necessary
    Obesity_Risk_Score = (0.25 * Physical_Activity_Score) + 
      (0.25 * Tech_Time_Score) + 
      (0.25 * Veggie_Consumption_Score) + 
      (0.25 * Family_History_Score)
  )
# Inspecting the first few rows of the newly added Obesity Risk Score
head(data$Obesity_Risk_Score, 5)


ggplot(data, aes(y = Obesity_Risk_Score)) + 
  geom_boxplot(fill = "green") + 
  labs(title = "Boxplot of Obesity Risk Score")

# Conclusion: At this point, you can further analyze the Obesity_Risk_Score, 
# compare it across different groups, or use it as a feature in predictive modeling.

model2 <- lm(BMI ~ Obesity_Risk_Score, data = data)
summary(model2)
