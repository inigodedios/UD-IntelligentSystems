analyze.results <- function(results, problem) {
  analyzed_results <- data.frame()
  
  for (i in 1:length(results)) {
    result <- results[[i]]
    print(result$name)
    
    solution_found  <- any(!is.na(result$state_final))
    solution_length <- '?'
    solution_cost   <- '?'
    
    if (any(solution_found)) {
      # Checking the solution
      solution_length <- length(result$state_final$actions)
      solution_cost   <- result$state_final$cost
      print(paste0(" * Solution found after ", solution_length, " actions! :)"), quote = FALSE)
    } else {
      print(" * No Solution Found :(", quote = FALSE)
    }

    analyzed_results <- rbind(analyzed_results, data.frame(Name = result$name,
                                                           Solution = solution_found,
                                                           Actions = solution_length,
                                                           Cost = solution_cost,
                                                           Iterations = length(result$report$iteration),
                                                           Max_Depth = max(result$report$depth_of_expanded),
                                                           Max_Frontier = max(result$report$nodes_frontier),
                                                           Runtime = round(result$runtime, digits = 2)))
  }
  
  return(analyzed_results)
}

local.analyze.results <- function(results, problem) {
  analyzed_results <- data.frame()
  
  for (i in 1:length(results)) {
    result <- results[[i]]
    analyzed_results <- rbind(analyzed_results, data.frame(Name = result$name,
                                                           Initial_State = to.string(problem$state_initial, problem),
                                                           Final_State = to.string(result$state_final$state, problem),
                                                           Evaluation = get.evaluation(state = result$state_final$state, problem = problem),
                                                           Iterations = length(result$report$iteration),
                                                           Runtime = round(result$runtime, digits = 2)))
  }
   
  return(analyzed_results)
}