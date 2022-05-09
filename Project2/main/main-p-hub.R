# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
#install.packages("kableExtra")
#library(kableExtra)
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


execute.local.beam.search <- function(problem){ #!
  return (local.beam.search(problem = problem))
}

test.local.beam.search <- function(filename, p, beams){
  # Execute hill climbing 'n' times
  results <- vector(mode = "list", length = 1)  
  
  # Initialize a problem instance for the analysis
  problem <- initialize.problem(filename = filename, p = p)
  
  
  #El mejor resultado, ejecuta local.beam.search
  results[[1]] <- execute.local.beam.search(problem)
  
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
  bestSolution <- results_df [order(results_df$Evaluation),]
  return(bestSolution[1])#Si no funciona poner [1]
}


# Clear console
cat("\014")
graphics.off()

#Hill climbing

file        <- "../data/AP40.txt"
p           <- 4
times       <- 10
results_df  <- test.hill.climbing(file, p, times)
results_df
bestSolHc10 <- bestSolution(results_df)


file        <- "../data/AP40.txt"
p           <- 4
times       <- 20
results_df  <- test.hill.climbing(file, p, times)
results_df
bestSolHc20 <- bestSolution(results_df)


file        <- "../data/AP40.txt"
p           <- 4
times       <- 50
results_df  <- test.hill.climbing(file, p, times)
results_df
bestSolHc50 <- bestSolution(results_df)


# Print results in an HTML Table
kable_material(kbl(results_df, caption = "p-hub AP40"),  c("striped", "hover", "condensed", "responsive"))


#Random restart hill climbing
file        <- "../data/AP100.txt"
p           <- 4
times       <- 10
results_df  <- modification.random.restart.hill.climbing(file, p, times)
results_df
resultsRrhc10 <- bestSolution(results_df)



file        <- "../data//AP100.txt"
p           <- 4
times       <- 20
results_df  <- modification.random.restart.hill.climbing(file, p, times)
results_df
resultsRrhc20 <- bestSolution(results_df)


file        <- "../data/AP100.txt"
p           <- 4
times       <- 50
results_df  <- modification.random.restart.hill.climbing(file, p, times)
results_df
resultsRhh50 <- bestSolution(results_df)


# Print results in an HTML Table
kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))

#Local beam seach
file        <- "../data/AP40.txt"
p           <- 3
beams       <- 3
results_df  <- test.local.beam.search(file, p, beams) 
resultsLbs3 <- bestSolution(results_df)

file        <- "../data/AP40.txt"
p           <- 3
beams       <- 5
results_df  <- test.local.beam.search(file, p, beams) 
resultsLbs5 <- bestSolution(results_df)


file        <- "../data/AP40.txt"
p           <- 3
beams       <- 10
results_df  <- test.local.beam.search(file, p, beams) 
resultsLbs10 <- bestSolution(results_df)


totalResult      <- rbind(bestSolHc10,bestSolHc20,bestSolHc50,resultsRrhc10,resultsRrhc50,
                          resultsRrhc50,resultsLbs3,resultsLbs5,resultsLbs10)


# Print results in an HTML Table
kable_material(kbl(totalResult, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))
