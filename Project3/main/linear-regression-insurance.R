# Clear Environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Clear plots & console
if(!is.null(dev.list())) dev.off()
cat("\014") 

#Libraries
library(ggplot2)
library(lattice)
library(caret)

filename = "../data/insurance.csv"
data <- read.csv(file=filename, sep=",", header = TRUE)

# Divide data for training
trainIndex <- createDataPartition(y=data$charges, p = 0.8, list = FALSE)

# Indexes are divided based on training data and testing data
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

best <- NULL
error <- 10000

# 10 Linear Regresion models are to be created, their avg error calculated and the best chosen
for (i in 1:10) {
  # Model creation
  model <- lm(formula = charges ~., data = train_data) 
  
  # Prediction based on model and test data
  prediction <- predict(model, test_data)
  
  # Calculate Mean Average Error
  meanAvgError <- mean(abs(prediction - test_data$charges))
  
  if (meanAvgError < error){
    best <- model
  }
  
}

model <- best

print(model)
print(paste0("- Mean average error: ", meanAvgError))

summary(model)

scatter.smooth(x=data$age, y=data$charges, main="X vs. Y")

cor(data$charges, data$children)

### Data for question answering
# Si una persona deja de fumar, ¿cuánto se reduciría el coste?
# Guardamos en dos variables diferentes las predicciones para la variable "smoker" en sus ambos estados,
# y calculamos la diferencia
data$smoker <- "no"
noSmoke <- mean(predict(model, newdata = data))

data$smoker <- "yes"
yesSmoke <- mean(predict(model, newdata = data))

smokeDifference <- yesSmoke - noSmoke
 