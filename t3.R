
data <- read.csv("C:\\Users\\sravy\\Downloads\\News_Final.csv")
head(data)
summary(data)
str(data)
install.packages(c("ggplot2", "dplyr", "tidyr", "corrplot"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

summary(data)
# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# Distribution of SentimentTitle
ggplot(data, aes(x = SentimentTitle)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of SentimentTitle", x = "SentimentTitle", y = "Frequency")

# Distribution of SentimentHeadline
ggplot(data, aes(x = SentimentHeadline)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  labs(title = "Distribution of SentimentHeadline", x = "SentimentHeadline", y = "Frequency")



# Assuming 'Facebook' as a proxy for popularity
ggplot(data, aes(x = SentimentTitle, y = Facebook)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Facebook Engagement vs. SentimentTitle", x = "SentimentTitle", y = "Facebook Engagement")

ggplot(data, aes(x = SentimentHeadline, y = Facebook)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Facebook Engagement vs. SentimentHeadline", x = "SentimentHeadline", y = "Facebook Engagement")


head(data)


# Distribution of Topic
ggplot(data, aes(x = Topic)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Distribution of Topic", x = "Topic", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Select numeric columns
numeric_data <- data %>% select(SentimentTitle, SentimentHeadline, Facebook, GooglePlus, LinkedIn)

# Compute correlations
correlations <- cor(numeric_data)

# Plot correlation matrix
library(corrplot)
corrplot(correlations, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.8)


#data processing
missing_values <- colSums(is.na(data))
print(missing_values)



#handiling missing data
# Load necessary libraries
library(dplyr)
library(lubridate)

# Convert PublishDate to Date type
data$PublishDate <- as.Date(data$PublishDate)

# Check and handle missing values
data <- data %>% na.omit()

# Check for and remove duplicates
data <- data %>% distinct()

# Glimpse at the cleaned dataset
glimpse(data)




# Feature engineering: Extract year and month from PublishDate
data <- data %>%
  mutate(Year = year(PublishDate),
         Month = month(PublishDate))

# Creating a total social media engagements column
data <- data %>%
  mutate(TotalEngagement = Facebook + GooglePlus + LinkedIn)




#datasplitting
# Split data into training and testing sets (80/20)
set.seed(123)  # For reproducibility
library(caTools)

# Splitting the data
split <- sample.split(data$SentimentTitle, SplitRatio = 0.8)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)





#model selection
# Example: Random Forest for classification
library(randomForest)

# Train the Random Forest model
rf_model <- randomForest(SentimentTitle ~ ., data=train, ntree=100)

# Prediction on test set
predictions <- predict(rf_model, newdata=test)

# Evaluate accuracy
accuracy <- mean(predictions == test$SentimentTitle)





# Tune number of trees and mtry for Random Forest
tuned_rf <- randomForest(SentimentTitle ~ ., data=train, ntree=200, mtry=3)

# Evaluate tuned model
predictions_tuned <- predict(tuned_rf, newdata=test)
accuracy_tuned <- mean(predictions_tuned == test$SentimentTitle)




# Confusion Matrix for classification model
library(caret)

confusionMatrix(predictions, test$SentimentTitle)

# Evaluation metrics: Accuracy, Precision, Recall


# Feature importance
importance(rf_model)

# Plot feature importance
varImpPlot(rf_model)



# Plotting ROC Curve for model evaluation
library(pROC)

# Predicting probabilities for ROC curve
probs <- predict(rf_model, newdata=test, type='prob')
roc_curve <- roc(test$SentimentTitle, probs[, 2])
plot(roc_curve)








# Plot Facebook Engagement vs. SentimentTitle with a linear regression line
ggplot(data, aes(x = SentimentTitle, y = Facebook)) +
  geom_point() +  # Adds scatter points
  geom_smooth(method = "lm", color = "blue") +  # Adds linear regression line
  labs(title = "Facebook Engagement vs. SentimentTitle", 
       x = "Sentiment Title", 
       y = "Facebook Engagement") + 
  theme_minimal()  # Clean and minimal theme

