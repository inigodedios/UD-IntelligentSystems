# =======================================================================
# Group Name:
# Students:
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)

initialize.problem <- function(file) {
  problem <- list() # Default value is an empty list.
  
  problem$name                <- paste0("Linear regression - [", file, "]")
  problem$size                <- c(as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[1]),as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[2])) #y,x
  problem$table               <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1]) #y,x
  problem$state_initial       <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[2])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[1]+1)) #y,x # Buscamos la posición de la fila en la que se encuentra el estado, y al número correspondiente le sumamos uno, ya que los vectores en R comienzan por 1 
  problem$state_final         <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[2])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[1]+1)) #y,x # Hacemos la misma operiación que para el estado inicial, pero una fila más abajo
  problem$actions_possible    <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  problem$left                <- read.csv(file, sep=";", header = FALSE, skip=problem$size[1]+3, nrows = 1)    # NECESITAMOS SUMARLE 1 A CADA DATO
  
  # This attributes are compulsory
  # problem$age              <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1]) #y,x
  # problem$sex              <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1])
  # problem$bmi              <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1])
  # problem$children         <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1])
  problem$smoker            <-
  problem$region <-
  problem$charges <-
  
  # You can add additional attributes
  # problem$<aditional_attribute>  <- <INSERT CODE HERE>
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT CODE HERE TO CHECK THE APPLICABILITY OF EACH ACTION>
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  # <INSERT YOUR CODE HERE TO MODIFY CURRENT STATE>
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT YOUR CODE HERE TO CHECK WHETHER A STATE IS FINAL OR NOT> 
  
  return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  
  # <INSERT YOUR CODE HERE TO GENERATE A STRING THAT REPRESENTS THE STATE> 
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE> 
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
	return(1) # Default value is 1.
}