# Clear Environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Clear plots & console
if(!is.null(dev.list())) dev.off()
cat("\014") 

#Libraries
library(ggplot2)

data <- read.csv("../data/insurance.csv")

data$smoker
