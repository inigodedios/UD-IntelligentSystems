# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
install.packages("kableExtra")
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/informed/hill-climbing-search.R")
source("../algorithms/informed/random-restart-hill-climbing-search.R")
source("../algorithms/informed/local-beam-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/p-hub-problem.R")

random.restart.hill.climbing <- function(filename, p, times) {
  results <- vector(mode = "list", length = times)
  problem <- vector(mode = "list", length = times)
  for (i in 1:times) {
    problem[[i]] <- initialize.problem(filename, p)
    results[[i]] <- random.restart.hill.climbing.search(problem[[i]])
  }
  
  results_df <- single.analyze.results(results, problem) #Preguntar
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2), 
               " - Mean: ", round(mean(results_df$Evaluation), 2), 
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2), 
               " - Mean: ", round(mean(results_df$Runtime), 2), 
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  return(results_df)
}



#falta el metodo que busque el mejor



# Executes hill climbing search and return the results
execute.hill.climbing <- function(filename, p) {
  # Initialize problem
  problem <- initialize.problem(p = p, filename = filename)
  # Execute hill climbing
  return(hill.climbing.search(problem = problem))
}

# Execute Hill Climbing several times and analyze results
test.hill.climbing <- function(file, p, times) {
  # Execute hill climbing 'n' times
  results <- vector(mode = "list", length = times)
  
  for (i in 1:times) {
    results[[i]] <- execute.hill.climbing(filename = file, p = p)
  }
  
  # Initialize a problem instance for the analysis
  problem <- initialize.problem(filename = file, p = p)
  
  # Analyze results
  results_df <- local.analyze.results(results, problem)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2), 
               " - Mean: ", round(mean(results_df$Evaluation), 2), 
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2), 
               " - Mean: ", round(mean(results_df$Runtime), 2), 
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  
  return(results_df)
}


execute.local.beam.search <- function(problem, beams, max_iterations){ #!
  return (local.beam.search(problem = problem))
}

test.local.beam.search <- function(problem, beams, max_iterations, filename, p){
  # Execute hill climbing 'n' times
  results <- vector(mode = "list", length = beams)
  
  for (i in 1:times) {
    results[[i]] <- execute.local.beam.search(problem)
  }
  
  # Initialize a problem instance for the analysis
  problem <- initialize.problem(filename = filename, p = p)
  
  # Analyze results
  results_df <- local.analyze.results(results, problem)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2), 
               " - Mean: ", round(mean(results_df$Evaluation), 2), 
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2), 
               " - Mean: ", round(mean(results_df$Runtime), 2), 
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  
  return(results_df)
}

bestSolution <- function (results_df){
  
}

avgSolution <- function (results_df){
  
}

avgTimeSolution <- function (results_df){
  
}


# Clear console
cat("\014")
graphics.off()

# Test Hill climbing
file        <- "../data/p-hub/AP40.txt"
p           <- 4
times       <- 10
results_df  <- test.hill.climbing(file, p, times)

file        <- "../data/p-hub/AP40.txt"
p           <- 4
times       <- 20
results_df  <- test.hill.climbing(file, p, times)

file        <- "../data/p-hub/AP40.txt"
p           <- 4
times       <- 50
results_df  <- test.hill.climbing(file, p, times)

# Print results in an HTML Table
kable_material(kbl(results_df, caption = "p-hub AP40"),  c("striped", "hover", "condensed", "responsive"))


#Random restart hill climbing
file        <- "../data/p-hub/AP100.txt"
p           <- 3
times       <- 10
results_df  <- random.restart.hill.climbing(file, p, times)

file        <- "../data/p-hub/AP100.txt"
p           <- 3
times       <- 20
results_df  <- random.restart.hill.climbing(file, p, times)

file        <- "../data/p-hub/AP100.txt"
p           <- 3
times       <- 50
results_df  <- random.restart.hill.climbing(file, p, times)

# Print results in an HTML Table
kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))

#Local beam seach
file        <- "../data/p-hub/AP40.txt"
p           <- 3
beams       <- 3
results_df  <- test.local.beam.search(file, p, beams) 
## Poner metodos de mejor solucion, sol media y demas

file        <- "../data/p-hub/AP40.txt"
p           <- 3
beams       <- 5
results_df  <- test.local.beam.search(file, p, beams) 


file        <- "../data/p-hub/AP40.txt"
p           <- 3
beams       <- 10
results_df  <- test.local.beam.search(file, p, beams) 
