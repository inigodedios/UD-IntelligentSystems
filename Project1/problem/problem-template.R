# =======================================================================
# Group Name: Group 13
# Students: Iñigo de Dios, Alexandru Gabriel Nitu
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(file) {
  problem <- list() # Default value is an empty list.
  
  # This attributes are compulsory
  problem$name                <- paste0("Feet Maze - [", file, "]")
  problem$size                <- c(as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[1]),as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[2]))
  problem$table               <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1])
  problem$initial_state       <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[1])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[2]+1)) # Buscamos la posición de la fila en la que se encuentra el estado, y al número correspondiente le sumamos uno, ya que los vectores en R comienzan por 1 
  problem$final_state         <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[1])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[2]+1)) # Hacemos la misma operiación que para el estado inicial, pero una fila más abajo
  problem$actions             <- data.frame(left = 0, right = 0, down = 0, top = 0)
  problem$left_collumn        <- read.csv(file, sep=";", header = FALSE, skip=problem$size+3, nrows = 1)      # NECESITAMOS SUMARLE 1 A CADA DATO, PERO AÚN NO LOS TRENEMOS GUARDADOS COMO INTEGERS, NI LOS TENEMOS SEPARADOS, ASÍ QUE NO PODEMOS.  
  problem$right_collumn       <- read.csv(file, sep=";", header = FALSE, skip=problem$size+4, nrows = 1)   
  problem$down_collumn        <- read.csv(file, sep=";", header = FALSE, skip=problem$size+5, nrows = 1)   
  problem$top_collumn         <- read.csv(file, sep=";", header = FALSE, skip=problem$size+6, nrows = 1)   
  
  # You can add additional attributes
  # problem$<aditional_attribute>  <- <INSERT CODE HERE>
  
  return(problem)
}
initialize.problem("../data/feet-maze-3b.txt")
# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT CODE HERE TO CHECK THE APPLICABILITY OF EACH ACTION>
  # Dos condiciones: barrera, pie, y borde
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  # <INSERT YOUR CODE HERE TO MODIFY CURRENT STATE>   Aquñi definimos lo que hace cada acción
  
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