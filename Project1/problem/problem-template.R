# =======================================================================
# Group Name: Group 13
# Students: Iñigo de Dios & Alexandru Gabriel, Nitu
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
  #La coordenadas del txt son y,x. Fila = y ; columna = x
  problem$name                <- paste0("Feet Maze - [", file, "]")
  problem$size                <- c(as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[1]),as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[2])) #y,x
  problem$table               <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1]) #y,x
  problem$initial_state       <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[2])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[1]+1)) #y,x # Buscamos la posición de la fila en la que se encuentra el estado, y al número correspondiente le sumamos uno, ya que los vectores en R comienzan por 1 
  problem$final_state         <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[2])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[1]+1)) #y,x # Hacemos la misma operiación que para el estado inicial, pero una fila más abajo
  problem$actions_possible    <- data.frame("Up", "Down", "Left", "Right") 
  problem$left_collumn        <- read.csv(file, sep=";", header = FALSE, skip=problem$size+3, nrows = 1)    # NECESITAMOS SUMARLE 1 A CADA DATO, PERO AÚN NO LOS TRENEMOS GUARDADOS COMO INTEGERS, NI LOS TENEMOS SEPARADOS, ASÍ QUE NO PODEMOS.  
  problem$right_collumn       <- read.csv(file, sep=";", header = FALSE, skip=problem$size+4, nrows = 1)   
  problem$down_collumn        <- read.csv(file, sep=";", header = FALSE, skip=problem$size+5, nrows = 1)   
  problem$top_collumn         <- read.csv(file, sep=";", header = FALSE, skip=problem$size+6, nrows = 1)   
  
  return(problem)
  
}

# Transforms a state into a string
to.string <- function (state) { #OK
  actualState<- c(state[1]-1, state[2]-1)
  stateString <- toString (actualState)
  finalState <-gsub(" ", "", stateString)
  return (finalState)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.    FALSE?
  
  
  # IZQUIERDA
  if (action == "left"){
    #Borde #OK
    con1 = 1>state[2]
    if (con1) return(FALSE)
    #Pies #OK
    con2 = problem$table[state[1],state[2]-1] == problem$table[state[1],state[2]]
    if (con2) return (FALSE)
    #Barreras #OK
    con1 = to.string(state) %in% problem$left_collumn
    if (con1) return (FALSE)
    state2 <- c(state[1], state[2]-1) #Una posicion a la izquierda
    con2 = to.string(state2) %in% problem$right_collumn
    if (con2) return (FALSE) #Hay que restarle 1 ya que la comprobacion se hace desde la casilla de la izquierda
  }
  
  # DERECHA
  if (action == "right"){
    #Borde #OK
    if (problem$size[2]<state[2]) return (FALSE) #problem$Size[2] es la columna y state [1] tambien es la columna
    #Pies #OK
    con2 = problem$table[state[1],state[2]+1] == problem$table[state[1],state[2]]
    if (con2) return (FALSE)
    #Barreras #OK
    if (to.string(state) %in% problem$right_collumn) return (FALSE)
    state2 <- c(state[1], state[2]+1) #Una posicion a la derecha
    if (to.string(state2) %in% problem$left_collumn) return (FALSE)
  }
  
  # ABAJO #Corregir mañana
  if (action == "down"){
    #Borde  #OK
    if (1>state[1]) return (FALSE)
    #Pies  #OK
    if (problem$table[state[1]-1, state[2]] == problem$table[state[1], state[2]]) return (FALSE)
    #Barreras #OK
    if (to.string(state) %in% problem$down_collumn) return (FALSE)
    state2 <- c(state[1]-1, state[2])
    if (to.string(state2) %in% problem$top_collumn) return (FALSE)
  }
  
  #ARRIBA #Corregir mañana
  if (action == "up"){
    #Borde # OK
    if (problem$size[1]<state[1]) return (FALSE) #PREGUNTAR CUAL ES el limite real en el problema
    #Pies  #OK
    if (problem$table[state[1]+1,state[2]] == problem$table[state[1],state[2]]) return (FALSE)
    #Barreras #OK
    if (to.string(state) %in% problem$top_collumn) return (FALSE)
    state2 <- c(state[1]+1, state[2])
    if (to.string(state2) %in% problem$down_collumn) return (FALSE)
  }
  
  return(result)
}


# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  if (action == "left") (result <- c(state[1], state[2]-1))
  
  if (action == "right") (result <- c(state[1], state[2]+1))
  
  if (action == "down") (result <- c(state[1]-1, state[2]))
  
  if (action == "up") (result <- c(state[1]+1, state[2]))
  
  return (result)
}
# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) { #OK
  result <- FALSE # Default value is FALSE.
  if (state[1] == final_state[1] && state[2] == final_state[2]) (result <- TRUE)
  return (result)
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
	return(1) # Default value is 1.
}




