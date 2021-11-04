# Imports
library(tidyverse)
library(rpart)

# Load the data
train <- read_csv('vw_train.csv')
test <- read_csv('vw_test.csv')

# Fit the model
decision_tree <- rpart(appliances ~ . -id, 
                       train,
                       minsplit = 10,
                       cp = 0.005)

# Print training accuracy
predictions <- predict(decision_tree, train, type = 'class')
training_accuracy <- mean(predictions == train$appliances, na.rm = TRUE)
print(paste0("Training classification accuracy: ", 
             round(100*training_accuracy, 2), '%'))


# Produce the predictions
submission <- test %>% 
  transmute(id = id,
            appliances = predict(decision_tree, 
                                 test, 
                                 type = 'class'))

# Save results
write_csv(submission, 'my_submission.csv')