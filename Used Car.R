library(readr)
library(data.table)
# Get the total number of lines in the file
total_rows <- as.numeric(system("wc -l < Downloads/used_cars_data.csv", intern = TRUE))
# Read the first 8th of the file
half_rows <- floor(total_rows / 8)
used_cars_data <- fread("Downloads/used_cars_data.csv", nrows = half_rows)
#make a new dataset
fwrite(used_cars_data, "Downloads/used_cars_trimmed2.csv")

###

# Load required libraries
library(data.table)
library(dplyr)
library(tidyverse)
# Load the trimmed dataset
file_path <- "Downloads/used_cars_trimmed2.csv"
used_cars_data <- fread(file_path)

# Load required libraries
library(data.table)
library(dplyr)
library(ggplot2)

# Load the dataset
file_path <- "Downloads/used_cars_trimmed2.csv"
used_cars_data <- fread(file_path)

# Step 1: Basic Structure and Overview
cat("Structure of the dataset:\n")
str(used_cars_data)

cat("\nSummary of the dataset:\n")
summary(used_cars_data)

cat("\nColumn names:\n")
print(colnames(used_cars_data))

# Step 2: Missing Values and Basic Analysis
cat("\nMissing values in each column:\n")
missing_values <- colSums(is.na(used_cars_data))
print(missing_values[missing_values > 0])

cat("\nUnique values in key categorical columns:\n")
categorical_columns <- used_cars_data %>% select(where(is.character))
unique_values <- sapply(categorical_columns, uniqueN)
print(unique_values)

# Step 3: Distribution of Key Numerical Variables
numerical_columns <- used_cars_data %>% select(where(is.numeric))
cat("\nNumerical columns summary:\n")
print(summary(numerical_columns))

cat("\nHistograms for numerical columns:\n")
numerical_cols <- names(numerical_columns)
for (col in numerical_cols) {
  print(
    ggplot(used_cars_data, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      ggtitle(paste("Distribution of", col))
  )
}

# Step 4: Correlation Analysis
cat("\nCorrelation matrix for numerical columns:\n")
correlation_matrix <- cor(numerical_columns, use = "complete.obs")
print(correlation_matrix)

# Load necessary libraries
library(data.table)

# Step 1: Remove irrelevant or mostly missing columns
irrelevant_cols <- c(
  "combine_fuel_economy", "vehicle_damage_category", "bed", "bed_height", 
  "bed_length", "listing_id", "vin"
)
cleaned_data <- used_cars_data[, !irrelevant_cols, with = FALSE]

# Step 2: Impute missing values
# Impute numerical columns with median
num_cols <- c("mileage", "city_fuel_economy", "engine_displacement", "horsepower", "price")
cleaned_data[, (num_cols) := lapply(.SD, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)), .SDcols = num_cols]

# Impute binary/logical columns with FALSE
logical_cols <- c("frame_damaged", "fleet", "has_accidents", "salvage", "theft_title")
cleaned_data[, (logical_cols) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x)), .SDcols = logical_cols]

# Save intermediate cleaned dataset
fwrite(cleaned_data, "Downloads/used_cars_step1_2.csv")
cat("Dataset after Step 1 and Step 2 saved as 'used_cars_step1_2.csv'.\n")

####CLEANING PARTT#####

# Step 1: Remove irrelevant or duplicate columns
irrelevant_cols <- c(
  "combine_fuel_economy", "vehicle_damage_category", "bed", "bed_height", 
  "bed_length", "listing_id", "vin"
)
cleaned_data <- used_cars_data[, !irrelevant_cols, with = FALSE]

# Step 2: Impute missing values
# Impute numerical columns with median
num_cols <- c("mileage", "city_fuel_economy", "engine_displacement", "horsepower", "price")
cleaned_data[, (num_cols) := lapply(.SD, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)), .SDcols = num_cols]

# Impute binary/logical columns with FALSE
logical_cols <- c("frame_damaged", "fleet", "has_accidents", "salvage", "theft_title")
cleaned_data[, (logical_cols) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x)), .SDcols = logical_cols]

# Step 3: Create new features
# Create "age" from "year"
cleaned_data[, age := 2024 - year]

# Create average fuel economy
fuel_economy_cols <- c("city_fuel_economy", "highway_fuel_economy")
cleaned_data[, avg_fuel_economy := rowMeans(.SD, na.rm = TRUE), .SDcols = fuel_economy_cols]
cleaned_data[is.na(avg_fuel_economy), avg_fuel_economy := median(avg_fuel_economy, na.rm = TRUE)]

# Combine damage-related variables into "damage_score"
damage_cols <- c("frame_damaged", "salvage", "theft_title", "has_accidents")
cleaned_data[, damage_score := rowSums(.SD), .SDcols = damage_cols]

# Create regional variable based on "latitude" and "longitude"
cleaned_data[, region := fifelse(
  latitude > 37, 
  fifelse(longitude < -100, "Northwest", "Northeast"),
  fifelse(longitude < -100, "Southwest", "Southeast")
)]

# Popularity metric based on model frequency
cleaned_data[, model_popularity := .N, by = model_name]

# Factor in color popularity
color_popularity <- cleaned_data[, .N, by = exterior_color]
color_popularity[, color_rank := frank(-N)]
cleaned_data <- merge(cleaned_data, color_popularity[, .(exterior_color, color_rank)], by = "exterior_color", all.x = TRUE)

# Step 4: Save the dataset
fwrite(cleaned_data, "Downloads/used_cars_consolidated.csv")
cat("Consolidated dataset saved as 'used_cars_consolidated.csv'.\n")

# Load necessary libraries
library(data.table)
library(ggplot2)

# Step 1: Validate column removal
irrelevant_cols <- c("combine_fuel_economy", "vehicle_damage_category", "bed", "bed_height", 
                     "bed_length", "listing_id", "vin")
cat("Removed irrelevant columns:\n")
print(setdiff(irrelevant_cols, colnames(cleaned_data)))

# Step 2: Validate missing value imputation
# Numerical columns
num_cols <- c("mileage", "city_fuel_economy", "engine_displacement", "horsepower", "price")
cat("\nRemaining missing values in numerical columns:\n")
print(colSums(is.na(cleaned_data[, ..num_cols])))

# Logical columns
logical_cols <- c("frame_damaged", "fleet", "has_accidents", "salvage", "theft_title")
cat("\nRemaining missing values in logical columns:\n")
print(colSums(is.na(cleaned_data[, ..logical_cols])))

# Step 3: Validate new features
# Age
cat("\nSummary of 'age' feature:\n")
print(summary(cleaned_data$age))

# Average Fuel Economy
cat("\nRemaining missing values in 'avg_fuel_economy':\n")
print(sum(is.na(cleaned_data$avg_fuel_economy)))
cat("Summary of 'avg_fuel_economy':\n")
print(summary(cleaned_data$avg_fuel_economy))

# Damage Score
cat("\nPreview of damage score calculation:\n")
print(head(cleaned_data[, .(frame_damaged, salvage, theft_title, has_accidents, damage_score)]))

# Region
cat("\nRegional distribution:\n")
print(table(cleaned_data$region))

# Model Popularity
cat("\nModel popularity preview:\n")
print(head(cleaned_data[, .N, by = model_name][order(-N)]))

# Color Popularity
cat("\nColor popularity preview:\n")
print(head(cleaned_data[, .(exterior_color, color_rank)], 10))

# Step 4: Check for outliers
# Price distribution
ggplot(cleaned_data, aes(x = price)) + 
  geom_histogram(bins = 50, fill = "blue", color = "black")

# Step 1: Clean exterior_color
valid_colors <- c("Black", "White", "Gray", "Silver", "Red", "Blue", "Brown", "Green", "Yellow", "Orange", "Gold")
cleaned_data[, exterior_color := fifelse(
  exterior_color %in% valid_colors, exterior_color, "Other"
)]

# Step 2: Cap outliers
# Cap age at 30 years
cleaned_data[, age := pmin(age, 30)]

# Cap price at $150,000
cleaned_data[, price := pmin(price, 150000)]

# Cap mileage at 300,000 miles
cleaned_data[, mileage := pmin(mileage, 300000)]

# Cap average fuel economy at 60 mpg
cleaned_data[, avg_fuel_economy := pmin(avg_fuel_economy, 60)]

# Step 3: Preview changes
cat("\nSummary of capped numerical features:\n")
print(summary(cleaned_data[, .(age, price, mileage, avg_fuel_economy)]))

# Step 4: Reassess color popularity
cat("\nUpdated color popularity:\n")
color_popularity <- cleaned_data[, .N, by = exterior_color][order(-N)]
print(color_popularity)

# Save the updated dataset
fwrite(cleaned_data, "Downloads/used_cars_cleaned_with_colors.csv")
cat("Cleaned dataset with updated color and outlier handling saved as 'used_cars_cleaned_with_colors.csv'.\n")


##figuring out feature importance###

# Load necessary libraries
library(data.table)
library(corrplot)

# Step 1: Correlation Analysis
num_cols <- names(cleaned_data)[sapply(cleaned_data, is.numeric)]
cor_matrix <- cor(cleaned_data[, ..num_cols], use = "complete.obs")
price_correlation <- cor_matrix["price", ]
cat("Correlation with Price:\n")
print(price_correlation[order(-abs(price_correlation))])

# Visualize top correlated variables with Price
top_correlated <- names(price_correlation[order(-abs(price_correlation))][1:10])
corrplot(cor_matrix[top_correlated, top_correlated], method = "color", type = "upper", tl.cex = 0.7)

# Step 2: Variance and Frequency for Categorical Variables
cat("\nUnique value counts for categorical columns:\n")
cat_cols <- names(cleaned_data)[sapply(cleaned_data, is.character)]
unique_counts <- sapply(cleaned_data[, ..cat_cols], function(x) length(unique(x)))
print(unique_counts)

cat("\nFrequency distribution for top categorical variables:\n")
freq_table <- lapply(cleaned_data[, ..cat_cols], table)
print(freq_table)

# Step 3: Lightweight Linear Regression
# Subset dataset to numerical columns for regression
lm_data <- cleaned_data[, ..num_cols]
lm_data <- na.omit(lm_data)  # Drop rows with missing values for simplicity

# Fit linear model
lm_model <- lm(price ~ ., data = lm_data)
summary_lm <- summary(lm_model)

cat("\nLinear Model Summary:\n")
print(summary_lm)

# Extract significant variables (p-value < 0.05)
significant_vars <- rownames(summary_lm$coefficients)[summary_lm$coefficients[, "Pr(>|t|)"] < 0.05]
cat("\nSignificant variables:\n")
print(significant_vars)

# Load necessary libraries
library(data.table)
library(car)  # For VIF calculation
library(ggplot2)

# Step 1: Drop latitude and longitude if regional categories exist
if ("region" %in% colnames(cleaned_data)) {
  cleaned_data[, c("latitude", "longitude") := NULL]
  cat("Dropped latitude and longitude.\n")
} else {
  cat("Regional categories not found. Confirm creation.\n")
}

# Step 2: Check for multicollinearity using VIF
# Subset numerical variables for VIF analysis
num_cols_vif <- c("price", "horsepower", "mileage", "age", 
                  "engine_displacement", "avg_fuel_economy", 
                  "damage_score", "seller_rating", "daysonmarket")
vif_model <- lm(price ~ ., data = cleaned_data[, ..num_cols_vif])
vif_values <- vif(vif_model)
cat("\nVIF Values:\n")
print(vif_values)

# Identify high VIF variables
high_vif <- names(vif_values[vif_values > 5])
if (length(high_vif) > 0) {
  cat("\nVariables with high multicollinearity (VIF > 5):\n")
  print(high_vif)
} else {
  cat("\nNo multicollinearity issues detected (VIF < 5 for all variables).\n")
}

# Step 3: Identify outliers using Z-scores for numerical variables
numerical_vars <- c("price", "horsepower", "mileage", "age", 
                    "avg_fuel_economy", "damage_score", "seller_rating")

# Calculate Z-scores and identify outliers
outliers <- cleaned_data[, lapply(.SD, function(x) sum(abs(scale(x)) > 3)), .SDcols = numerical_vars]
cat("\nOutlier counts per variable:\n")
print(outliers)

# Visualize outliers using boxplots
for (var in numerical_vars) {
  print(ggplot(cleaned_data, aes_string(x = var)) + 
          geom_boxplot(outlier.color = "red") + 
          ggtitle(paste("Boxplot of", var)) + 
          theme_minimal())
}

# Step 4: Capping or removing extreme outliers
# Example: Capping price, mileage, and fuel economy
cleaned_data[, price := pmin(price, 150000)]
cleaned_data[, mileage := pmin(mileage, 300000)]
cleaned_data[, avg_fuel_economy := pmin(avg_fuel_economy, 60)]
cat("\nExtreme outliers capped for price, mileage, and avg_fuel_economy.\n")


# Step 1: Reasonability Check for horsepower and damage_score
variables_to_check <- c("horsepower", "damage_score")
for (var in variables_to_check) {
  if (var %in% colnames(cleaned_data)) {
    print(ggplot(cleaned_data, aes_string(y = var)) +
            geom_boxplot(outlier.color = "red") +
            ggtitle(paste("Boxplot of", var)) +
            theme_minimal())
    print(ggplot(cleaned_data, aes_string(x = var)) +
            geom_histogram(bins = 30, fill = "blue", color = "black") +
            ggtitle(paste("Histogram of", var)) +
            theme_minimal())
  }
}

# Step 2: Impute seller_rating
if ("seller_rating" %in% colnames(cleaned_data)) {
  median_seller_rating <- median(cleaned_data$seller_rating, na.rm = TRUE)
  cleaned_data[is.na(seller_rating), seller_rating := median_seller_rating]
  cat("\nImputed missing values in seller_rating with median value:", median_seller_rating, "\n")
}

# Step 3: Validation
# Recheck seller_rating distribution
if ("seller_rating" %in% colnames(cleaned_data)) {
  print(ggplot(cleaned_data, aes(x = seller_rating)) +
          geom_histogram(bins = 30, fill = "blue", color = "black") +
          ggtitle("Distribution of seller_rating after imputation") +
          theme_minimal())
}

# Summarize remaining outliers for all numerical variables
numerical_vars <- c("price", "horsepower", "mileage", "age", 
                    "avg_fuel_economy", "damage_score", "seller_rating")
outliers_after_imputation <- cleaned_data[, lapply(.SD, function(x) sum(abs(scale(x)) > 3)), .SDcols = numerical_vars]
cat("\nOutlier counts per variable after capping and imputation:\n")
print(outliers_after_imputation)


# Step 1: Summarize body type counts
body_type_summary <- cleaned_data[, .N, by = body_type][order(-N)]
cat("Body Type Summary:\n")
print(body_type_summary)

# Step 2: Visualize distribution
library(ggplot2)
ggplot(body_type_summary, aes(x = reorder(body_type, -N), y = N)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  ggtitle("Distribution of Vehicles by Body Type") +
  xlab("Body Type") +
  ylab("Number of Vehicles") +
  theme_minimal()


# Ensure necessary libraries are loaded
library(data.table)

# Sample data structure
# cleaned_data <- data.table(
#   model_name = c("Model A", "Model B", "Model C"),
#   wheelbase = c(104, 110, 118),
#   engine_displacement = c(2.0, 3.0, 4.0)
# )

# Function to classify SUV size
classify_suv_size <- function(wheelbase, engine_displacement) {
  if (!is.na(wheelbase)) {
    if (wheelbase < 105) {
      return("Small SUV")
    } else if (wheelbase <= 115) {
      return("Mid-Size SUV")
    } else {
      return("Large SUV")
    }
  } else if (!is.na(engine_displacement)) {
    if (engine_displacement < 2.5) {
      return("Small SUV")
    } else if (engine_displacement <= 3.5) {
      return("Mid-Size SUV")
    } else {
      return("Large SUV")
    }
  } else {
    return(NA)
  }
}

# Apply classification to dataset
cleaned_data[, suv_size := mapply(classify_suv_size, wheelbase, engine_displacement)]

# View the updated dataset
print(cleaned_data)

# Load required libraries
library(ggplot2)

# Check if the "suv_size" column exists in the dataset
if ("suv_size" %in% colnames(cleaned_data)) {
  # Create a bar plot to visualize the distribution of SUV sizes
  ggplot(cleaned_data, aes(x = suv_size)) +
    geom_bar(fill = "blue", color = "black") +
    ggtitle("Distribution of SUV Sizes") +
    xlab("SUV Size") +
    ylab("Count") +
    theme_minimal()
} else {
  cat("Column 'suv_size' not found in the dataset.")
}

#####MODEL CREATION#####

####PICKUP TRUCK#####

# Ensure necessary libraries are loaded
library(data.table)
library(caret)
library(randomForest)

# Step 1: Filter data for Pickup Trucks
pickup_truck_data <- cleaned_data[body_type == "Pickup Truck"]

# Step 2: Preprocess data
# Remove missing values for simplicity
pickup_truck_data <- na.omit(pickup_truck_data)
dim(pickup_truck_data)


cleaned_data[, body_type := trimws(tolower(body_type))]

pickup_truck_data <- cleaned_data[body_type == "pickup truck"]
dim(pickup_truck_data)

print(head(cleaned_data$body_type, 20))

# Normalize body_type values
cleaned_data[, body_type := trimws(tolower(body_type))]

# Filter for Pickup Trucks
pickup_truck_data <- cleaned_data[body_type == "pickup truck"]

# Check dimensions
dim(pickup_truck_data)

# If rows exist, proceed with modeling
if (nrow(pickup_truck_data) > 0) {
  # Remove missing values
  pickup_truck_data <- na.omit(pickup_truck_data)
  dim(pickup_truck_data)
} else {
  cat("No data found for 'Pickup Truck'. Check data formatting.\n")
}
#######----#####

missing_summary <- colSums(is.na(cleaned_data)) / nrow(cleaned_data)
print(missing_summary)

threshold <- 0.5
cols_to_keep <- names(missing_summary[missing_summary <= threshold])
cleaned_data <- cleaned_data[, ..cols_to_keep]

# Impute numeric columns
numeric_columns <- names(which(sapply(cleaned_data, is.numeric)))
for (col in numeric_columns) {
  cleaned_data[is.na(get(col)), (col) := median(get(col), na.rm = TRUE)]
}

# Impute categorical columns
categorical_columns <- names(which(sapply(cleaned_data, is.character)))
for (col in categorical_columns) {
  mode_value <- cleaned_data[, .N, by = col][which.max(N), col]
  cleaned_data[is.na(get(col)), (col) := mode_value]
}

cleaned_data <- na.omit(cleaned_data)

any(is.na(cleaned_data))  # Should return FALSE

dim(cleaned_data)


# Define the path to the Downloads folder
output_path <- "~/Downloads/cleaned_data.csv"

# Export the cleaned dataset to a CSV file
fwrite(cleaned_data, output_path)

cat("Exported the cleaned dataset to:", output_path, "\n")


##ANALYZING FINAL DATASET#####

# Load necessary libraries
library(data.table)
library(ggplot2)
library(caret)

# Step 1: Dataset Overview
cat("Dataset Dimensions:\n")
print(dim(cleaned_data))

cat("\nDataset Structure:\n")
print(str(cleaned_data))

cat("\nSummary Statistics:\n")
print(summary(cleaned_data))

# Step 2: Check for Missing Values
cat("\nMissing Value Summary:\n")
missing_summary <- colSums(is.na(cleaned_data))
print(missing_summary)

# Step 3: Verify Data Types
cat("\nColumn Data Types:\n")
print(sapply(cleaned_data, class))

# Step 4: Visualize Distributions of Numeric Columns
numeric_columns <- names(which(sapply(cleaned_data, is.numeric)))
cat("\nDistribution of Numeric Columns:\n")
for (col in numeric_columns) {
  print(
    ggplot(cleaned_data, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      ggtitle(paste("Distribution of", col)) +
      theme_minimal()
  )
}

# Step 5: Cardinality of Categorical Columns
categorical_columns <- names(which(sapply(cleaned_data, is.character)))
cat("\nCardinality of Categorical Columns:\n")
for (col in categorical_columns) {
  cat(paste(col, "- Unique Values:", length(unique(cleaned_data[[col]])), "\n"))
}

# Step 6: Correlation Analysis
cat("\nCorrelation Matrix for Numeric Features:\n")
cor_matrix <- cor(cleaned_data[, ..numeric_columns], use = "complete.obs")
print(round(cor_matrix, 2))

# Highlight high correlations
high_cor <- which(abs(cor_matrix) > 0.75 & abs(cor_matrix) < 1, arr.ind = TRUE)
if (length(high_cor) > 0) {
  cat("\nHigh Correlations Found:\n")
  print(high_cor)
} else {
  cat("\nNo High Correlations Found.\n")
}

#########

# Load necessary libraries
library(data.table)
library(caret)

# Step 1: Drop high-cardinality or irrelevant columns
high_cardinality_cols <- c("description", "main_picture_url", "major_options", "sp_name", "torque")
cleaned_data <- cleaned_data[, !names(cleaned_data) %in% high_cardinality_cols, with = FALSE]

# Step 2: Convert categorical columns to factors
categorical_columns <- c("body_type", "fuel_type", "region", "suv_size", "transmission", "wheel_system")
for (col in categorical_columns) {
  cleaned_data[[col]] <- as.factor(cleaned_data[[col]])
}

# Step 3: Train-Test Split
set.seed(123)  # Ensure reproducibility
trainIndex <- createDataPartition(cleaned_data$price, p = 0.8, list = FALSE)
train_data <- cleaned_data[trainIndex, ]
test_data <- cleaned_data[-trainIndex, ]

# Step 4: Save datasets (optional)
fwrite(train_data, "train_data.csv")
fwrite(test_data, "test_data.csv")

cat("Preprocessing complete. Ready for modeling!")



# List of body types to model
body_types <- unique(cleaned_data$body_type)

# Initialize a list to store results
model_results <- list()

# Loop through each body type
for (body_type in body_types) {
  cat("\nProcessing:", body_type, "\n")
  
  # Filter data for the current body type
  body_data <- cleaned_data[body_type == cleaned_data$body_type]
  
  # Ensure there are enough rows for modeling
  if (nrow(body_data) < 100) {
    cat("Skipping", body_type, "- Not enough data points.\n")
    next
  }
  
  # Split data into train and test sets
  set.seed(123)
  trainIndex <- createDataPartition(body_data$price, p = 0.8, list = FALSE)
  train_data <- body_data[trainIndex, ]
  test_data <- body_data[-trainIndex, ]
  
  # Step 2: Train Random Forest Model
  set.seed(123)
  rf_model <- randomForest(
    price ~ ., 
    data = train_data, 
    ntree = 100, 
    importance = TRUE
  )
  
  # Step 3: Evaluate on Test Data
  rf_predictions <- predict(rf_model, test_data)
  rf_r2 <- R2(rf_predictions, test_data$price)
  rf_rmse <- RMSE(rf_predictions, test_data$price)
  
  # Feature Importance
  importance_df <- as.data.table(importance(rf_model), keep.rownames = TRUE)
  colnames(importance_df) <- c("Feature", "Importance")
  
  # Plot Feature Importance
  importance_plot <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() +
    ggtitle(paste("Feature Importance for", body_type, "Model")) +
    theme_minimal()
  
  # Step 4: Save Results
  model_results[[body_type]] <- list(
    rf_model = rf_model,
    rf_r2 = rf_r2,
    rf_rmse = rf_rmse,
    importance_plot = importance_plot
  )
  
  # Print Model Performance
  cat(body_type, "Model Performance:\n")
  cat("Random Forest R-squared:", rf_r2, "\n")
  cat("Random Forest RMSE:", rf_rmse, "\n")
}



# Load necessary libraries
library(data.table)

# Read the data (ensure the path is correct)
cleaned_data <- fread("Downloads/cleaned_data.csv")

# Function to classify SUVs
classify_suv_size <- function(wheelbase, engine_displacement) {
  if (!is.na(wheelbase)) {
    if (wheelbase < 105) {
      return("Small SUV")
    } else if (wheelbase <= 115) {
      return("Mid-Size SUV")
    } else {
      return("Large SUV")
    }
  } else if (!is.na(engine_displacement)) {
    if (engine_displacement < 2.5) {
      return("Small SUV")
    } else if (engine_displacement <= 3.5) {
      return("Mid-Size SUV")
    } else {
      return("Large SUV")
    }
  } else {
    return(NA)
  }
}

# Ensure wheelbase is numeric
cleaned_data[, wheelbase := as.numeric(gsub("[^0-9.]", "", wheelbase))]

# Apply SUV classification using mapply
cleaned_data[, suv_size := ifelse(body_type == "suv / crossover",
                                  mapply(classify_suv_size, wheelbase, engine_displacement),
                                  body_type)]

# Verify the new suv_size column
print(table(cleaned_data$suv_size))

dim(cleaned_data)

# Desired sample size per category
sample_size <- 20000  # Adjust as needed

# Random sampling for each category
sampled_data <- cleaned_data[, .SD[sample(.N, min(.N, sample_size))], by = suv_size]

# Verify the new dataset size
cat("Sampled dataset dimensions:", dim(sampled_data), "\n")
print(table(sampled_data$suv_size))

fwrite(sampled_data, "Downloads/used_cars_sampled.csv")

###MID SIZE SUV MODELING###

# Load necessary libraries
library(randomForest)
library(caret)

# Filter data for Mid-Size SUV
mid_size_suv_data <- sampled_data[suv_size == "Mid-Size SUV"]

# Check for missing values and remove them if necessary
mid_size_suv_data <- na.omit(mid_size_suv_data)
dim(mid_size_suv_data)
# Select relevant features (adjust as needed)

mid_size_suv_data <- mid_size_suv_data[, ..features]

# Split into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(mid_size_suv_data$price, p = 0.8, list = FALSE)
train_data <- mid_size_suv_data[train_index]
test_data <- mid_size_suv_data[-train_index]

# Train Random Forest model
rf_model <- randomForest(price ~ ., data = train_data, ntree = 100, importance = TRUE)

# Evaluate on test set
predictions <- predict(rf_model, test_data)
r2 <- R2(predictions, test_data$price)
rmse <- RMSE(predictions, test_data$price)

cat("Random Forest Performance for Mid-Size SUV:\n")
cat("R²:", r2, "\n")
cat("RMSE:", rmse, "\n")

# Feature Importance
importance <- importance(rf_model)
importance_df <- data.table(Feature = rownames(importance), Importance = importance[, "IncNodePurity"])
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  ggtitle("Feature Importance for Mid-Size SUV Model") +
  theme_minimal()

####ALL BODY TYPES MODELING########

# Load necessary libraries
library(randomForest)
library(ggplot2)
library(data.table)
library(caret)

# Define a function to model each body type
model_body_type <- function(data, body_type) {
  cat("\nModeling for:", body_type, "\n")
  
  # Filter data for the specific body type
  body_data <- data[suv_size == body_type]
  
  # Split data into training and testing sets
  set.seed(123)  # For reproducibility
  trainIndex <- createDataPartition(body_data$price, p = 0.8, list = FALSE)
  train_data <- body_data[trainIndex, ]
  test_data <- body_data[-trainIndex, ]
  
  # Train Random Forest model
  rf_model <- randomForest(price ~ ., data = train_data, ntree = 100, importance = TRUE)
  
  # Predict on test data
  preds <- predict(rf_model, test_data)
  
  # Evaluate the model
  r2 <- R2(preds, test_data$price)
  rmse <- RMSE(preds, test_data$price)
  
  cat("R²:", r2, "\n")
  cat("RMSE:", rmse, "\n")
  
  # Feature Importance
  importance <- importance(rf_model)
  importance_df <- data.table(Feature = rownames(importance), Importance = importance[, "IncNodePurity"])
  
  # Plot Feature Importance
  ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() +
    ggtitle(paste("Feature Importance for", body_type, "Model")) +
    theme_minimal()
  
  return(list(model = rf_model, r2 = r2, rmse = rmse))
}

# List of body types
body_types <- unique(sampled_data$suv_size)

# Run the model for each body type
models <- list()
for (body_type in body_types) {
  models[[body_type]] <- model_body_type(sampled_data, body_type)
}

# Optional: Save models or results for later use
saveRDS(models, "body_type_rf_models.rds")


#################################################

# Load necessary libraries
library(randomForest)
library(caret)
library(data.table)
library(ggplot2)

# Read the sampled dataset
sampled_data <- fread("Downloads/sentiment_dataset_with_scores.csv")

# Filter data for Pickup Trucks
pickup_truck_data <- sampled_data[suv_size == "pickup truck"]

# Remove missing values and select relevant features
pickup_truck_data <- na.omit(pickup_truck_data)

# Split into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(pickup_truck_data$price, p = 0.8, list = FALSE)
train_data <- pickup_truck_data[train_index]
test_data <- pickup_truck_data[-train_index]

# Train Random Forest model
rf_model <- randomForest(price ~ ., data = train_data, ntree = 100, importance = TRUE)

# Evaluate the model on the test set
predictions <- predict(rf_model, test_data)
r2 <- R2(predictions, test_data$price)
rmse <- RMSE(predictions, test_data$price)

# Print performance metrics
cat("Random Forest Performance for Pickup Trucks:\n")
cat("R²:", r2, "\n")
cat("RMSE:", rmse, "\n")

#R²: 0.897146 
#RMSE: 5391.817 

downloads_path <- "~/Downloads/pickup_truck_rf_model.rds"
saveRDS(rf_model, downloads_path)
cat("Model saved to:", downloads_path, "\n")

# Filter data for Sedans
sedan_data <- sampled_data[suv_size == "sedan"]

# Remove missing values and select relevant features
sedan_data <- na.omit(sedan_data)

# Split into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(sedan_data$price, p = 0.8, list = FALSE)
train_data <- sedan_data[train_index]
test_data <- sedan_data[-train_index]

# Train Random Forest model
rf_model <- randomForest(price ~ ., data = train_data, ntree = 100, importance = TRUE)

# Evaluate the model on the test set
predictions <- predict(rf_model, test_data)
r2 <- R2(predictions, test_data$price)
rmse <- RMSE(predictions, test_data$price)

# Print performance metrics
cat("Random Forest Performance for Sedans:\n")
cat("R²:", r2, "\n")
cat("RMSE:", rmse, "\n")

# Save the model to the Downloads folder
downloads_path <- "~/Downloads/sedan_rf_model.rds"
saveRDS(rf_model, downloads_path)
cat("Model saved to:", downloads_path, "\n")

#R²: 0.8632113 
#RMSE: 5998.467 

# Filter data for Small SUVs
small_suv_data <- sampled_data[suv_size == "Small SUV"]

# Remove missing values and select relevant features
small_suv_data <- na.omit(small_suv_data)

# Split into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(small_suv_data$price, p = 0.8, list = FALSE)
train_data <- small_suv_data[train_index]
test_data <- small_suv_data[-train_index]

# Train Random Forest model
rf_model <- randomForest(price ~ ., data = train_data, ntree = 100, importance = TRUE)

# Evaluate the model on the test set
predictions <- predict(rf_model, test_data)
r2 <- R2(predictions, test_data$price)
rmse <- RMSE(predictions, test_data$price)

# Print performance metrics
cat("Random Forest Performance for Small SUVs:\n")
cat("R²:", r2, "\n")
cat("RMSE:", rmse, "\n")

# Save the model to the Downloads folder
downloads_path <- "~/Downloads/small_suv_rf_model.rds"
saveRDS(rf_model, downloads_path)
cat("Model saved to:", downloads_path, "\n")

#R²: 0.895534 
#RMSE: 3141.84 

# Filter data for Mid-Size SUVs
mid_size_suv_data <- sampled_data[suv_size == "Mid-Size SUV"]

# Remove missing values and select relevant features
mid_size_suv_data <- na.omit(mid_size_suv_data)

# Split into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(mid_size_suv_data$price, p = 0.8, list = FALSE)
train_data <- mid_size_suv_data[train_index]
test_data <- mid_size_suv_data[-train_index]

# Train Random Forest model
rf_model <- randomForest(price ~ ., data = train_data, ntree = 100, importance = TRUE)

# Evaluate the model on the test set
predictions <- predict(rf_model, test_data)
r2 <- R2(predictions, test_data$price)
rmse <- RMSE(predictions, test_data$price)

# Print performance metrics
cat("Random Forest Performance for Mid-Size SUVs:\n")
cat("R²:", r2, "\n")
cat("RMSE:", rmse, "\n")

# Save the model to the Downloads folder
downloads_path <- "~/Downloads/mid_size_suv_rf_model.rds"
saveRDS(rf_model, downloads_path)
cat("Model saved to:", downloads_path, "\n")

#R²: 0.8823206 
#RMSE: 5791.947 

# Filter data for Large SUVs
large_suv_data <- sampled_data[suv_size == "Large SUV"]

# Remove missing values and select relevant features
large_suv_data <- na.omit(large_suv_data)

# Split into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(large_suv_data$price, p = 0.8, list = FALSE)
train_data <- large_suv_data[train_index]
test_data <- large_suv_data[-train_index]

# Train Random Forest model
rf_model <- randomForest(price ~ ., data = train_data, ntree = 100, importance = TRUE)

# Evaluate the model on the test set
predictions <- predict(rf_model, test_data)
r2 <- R2(predictions, test_data$price)
rmse <- RMSE(predictions, test_data$price)

# Print performance metrics
cat("Random Forest Performance for Large SUVs:\n")
cat("R²:", r2, "\n")
cat("RMSE:", rmse, "\n")

# Save the model to the Downloads folder
downloads_path <- "~/Downloads/large_suv_rf_model.rds"
saveRDS(rf_model, downloads_path)
cat("Model saved to:", downloads_path, "\n")

#R²: 0.8989595 
#RMSE: 7571.666 



##TRYING ALL AT ONCE###


# Split into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(sampled_data$price, p = 0.8, list = FALSE)
train_data <- sampled_data[train_index]
test_data <- sampled_data[-train_index]

# Train Random Forest model
rf_model <- randomForest(price ~ ., data = train_data, ntree = 100, importance = TRUE)

# Evaluate the model on the test set
predictions <- predict(rf_model, test_data)
r2 <- R2(predictions, test_data$price)
rmse <- RMSE(predictions, test_data$price)

# Print performance metrics
cat("Random Forest Performance for Large SUVs:\n")
cat("R²:", r2, "\n")
cat("RMSE:", rmse, "\n")

colnames(sampled_data)

##ATTEMPTING SENTIMENTAL ANALYSIS###

# Load necessary library
library(data.table)

# Read the sampled dataset
dataset <- fread("~/Downloads/used_cars_sampled.csv")

# Ensure the 'description' column exists
if ("description" %in% colnames(dataset)) {
  # Randomly sample 10 rows
  set.seed(127)  # For reproducibility
  sample_descriptions <- dataset[sample(.N, 10), description]
  
  # Print the sampled descriptions
  print(sample_descriptions)
} else {
  cat("The 'description' column is not found in the dataset.\n")
}


# Positive Keywords
positive_keywords <- c(
  "fuel-efficient", "high MPG", "eco-friendly", "turbocharged", "powerful engine",
  "moonroof", "sunroof", "heated seats", "leather seats", "premium sound", 
  "dual climate control", "keyless entry", "remote start",
  "blind spot", "rear camera", "parking sensors", "lane assist",
  "certified", "clean carfax", "one-owner", "rigorous inspection", "low mileage",
  "alloy wheels", "chrome", "metallic paint", "tinted windows",
  "panoramic sunroof", "adaptive cruise control", "backup camera", 
  "power liftgate", "luxury group", "warranty included", "excellent condition",
  "effortless purchase experience", "seamless sales process", "customer satisfaction"
)

# Negative Keywords
negative_keywords <- c(
  "processing fee", "reconditioning fees", "destination charges",
  "as-is", "no warranty", "needs repair", "damaged", "scratches", "dents",
  "accident history", "prior lease", "recall", "high mileage", 
  "subject to availability", "cannot guarantee", "inspection required", "price may change",
  "reconditioning fees", "accident history", "dents or scratches", "needs maintenance", 
  "no warranty", "sold as-is", "limited availability", "basic interior"
)

# Sentiment Analysis Function
calculate_sentiment <- function(text, positive_keywords, negative_keywords) {
  positive_count <- sum(sapply(positive_keywords, function(x) grepl(x, text, ignore.case = TRUE)))
  negative_count <- sum(sapply(negative_keywords, function(x) grepl(x, text, ignore.case = TRUE)))
  sentiment_score <- positive_count - negative_count
  return(sentiment_score)
}

# Apply Sentiment Analysis to the Dataset
if ("description" %in% colnames(dataset)) {
  dataset[, sentiment_score := sapply(description, calculate_sentiment, positive_keywords, negative_keywords)]
  
  # Preview the dataset with sentiment scores
  print(head(dataset[, .(description, sentiment_score)]))
} else {
  cat("The 'description' column is not found in the dataset.\n")
}

colnames(dataset)
head(dataset$sentiment_score)

cor(dataset$sentiment_score, dataset$price, method = "pearson")  # Replace 'price' with your target variable
cor(dataset$sentiment_score, dataset$price, method = "spearman")

# Load the data.table package
library(data.table)

# Assuming your dataset is named 'dataset'
# Specify the file path where you want to save the dataset
file_path <- "Downloads/sentiment_dataset_with_scores.csv"

# Export the dataset
fwrite(dataset, file_path)

# Output confirmation
cat("Dataset successfully saved to", file_path)


