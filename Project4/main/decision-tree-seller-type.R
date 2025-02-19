# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
#install.packages("lattice")
library(lattice)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("caret")
library(caret)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

# Read data from CSV
filename = "../data/carDetails.csv"
data <- read.csv(file = filename, sep =",", header = TRUE)

data$name <- NULL

data$year[strtoi(data$year) <= 2000] <- "<= 2000"
data$year[strtoi(data$year) > 2000 & strtoi(data$year) <= 2005] <- "2000 < x <= 2005"
data$year[strtoi(data$year) > 2005 & strtoi(data$year) <= 2010] <- "2005 < x <= 2010"
data$year[strtoi(data$year) > 2010 & strtoi(data$year) <= 2015] <- "2010 < x <= 2015"
data$year[strtoi(data$year) > 2015 & strtoi(data$year) <= 2020] <- "2015 < x <= 2020"
data$year[strtoi(data$year) > 2000] <- "<= 2020"

data$selling_price[strtoi(data$selling_price) <= 50000] <- "x <= 50000"
data$selling_price[strtoi(data$selling_price) >  50000 & strtoi(data$selling_price) <= 100000] <- "50000  < x <= 100000"
data$selling_price[strtoi(data$selling_price) > 100000 & strtoi(data$selling_price) <= 200000] <- "100000 < x <= 200000"
data$selling_price[strtoi(data$selling_price) > 200000 & strtoi(data$selling_price) <= 300000] <- "200000 < x <= 300000"
data$selling_price[strtoi(data$selling_price) > 300000 & strtoi(data$selling_price) <= 400000] <- "300000 < x <= 400000"
data$selling_price[strtoi(data$selling_price) > 400000 & strtoi(data$selling_price) <= 500000] <- "400000 < x <= 500000"
data$selling_price[strtoi(data$selling_price) > 500000 & strtoi(data$selling_price) <= 600000] <- "500000 < x <= 600000"
data$selling_price[strtoi(data$selling_price) > 600000 & strtoi(data$selling_price) <= 700000] <- "600000 < x <= 700000"
data$selling_price[strtoi(data$selling_price) > 700000 & strtoi(data$selling_price) <= 800000] <- "700000 < x <= 800000"
data$selling_price[strtoi(data$selling_price) > 800000] <- "x > 800000"

data$km_driven[strtoi(data$km_driven) <= 200000] <- "x <= 200000"
data$km_driven[strtoi(data$km_driven) > 200000 & strtoi(data$km_driven) <= 400000] <- "200000 < x <= 400000"
data$km_driven[strtoi(data$km_driven) > 400000 & strtoi(data$km_driven) <= 600000] <- "400000 < x <= 600000"
data$km_driven[strtoi(data$km_driven) > 600000 & strtoi(data$km_driven) <= 800000] <- "600000 < x <= 800000"
data$km_driven[strtoi(data$km_driven) > 800000 & strtoi(data$km_driven) <= 1000000] <- "800000 < x <= 1000000"
data$km_driven[strtoi(data$km_driven) > 1000000] <- "x > 1000000"

# Convert columns to factors
index <- 1:ncol(data)
data[ , index] <- lapply(data[ , index], as.factor)

# Percentaje of training examples
training_p <- 0.8

# Generate data partition 80% training / 20% test. The result is a vector with the indexes 
# of the examples that will be used for the training of the model.
training_indexes <- createDataPartition(y = data$seller_type, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]  # Extract training data using training_indexes
test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 

best <- NULL
bestAccuracy <- 0 #0 para poder seleccionar el número mayor de precisión

#Generamos 10 arboles de decisión y calculamos la precisión
for (i in 1:10){
  # Create Linear Model using training data. Formula = all the columns except Salary
  model <- rpart(formula = seller_type ~., data = training_data)
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$seller_type, prediction)
  matrix <- confusionMatrix(prediction_results)
  accuracy <- matrix$overall[1]
  
  if (accuracy > bestAccuracy){
    best <- model
    bestAccuracy <- accuracy
  }
}

model <- best #TODO xA
accuracy <- bestAccuracy

# Print the accuracy
accuracy <- paste0("Accuracy = ", round(100*accuracy, digits = 2), "%")
print(accuracy, quote = FALSE)

# Print attributes in descending relevance
attrs <- names(model$variable.importance)

print("Attributes in descending order of relevance")

for (i in 1:length(attrs)) {
  print(paste0("  ", attrs[i]), quote = FALSE)
}

# Plot tree (this method is slow, wait until pot is completed)
rpart.plot(model, 
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "GnYlRd",
           shadow.col = "darkgray",
           main = "Dealer, Individual or Trustmark?", 
           sub = accuracy)

# Print the rules that represent the Decision Tree
rpart.rules(model, 
            style="wide", 
            cover = TRUE, 
            eq = "=", 
            when = "IF", 
            and = "&&", 
            extra = 4)

