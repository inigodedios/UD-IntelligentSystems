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
  problem$name                <- paste0("Feet Maze - [", file, "]")
  problem$size                <- c(as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[1]),as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[2]))
  problem$table               <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1])
  problem$initial_state       <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[2])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[1]+1)) # Buscamos la posición de la fila en la que se encuentra el estado, y al número correspondiente le sumamos uno, ya que los vectores en R comienzan por 1 
  problem$final_state         <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[2])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[1]+1)) # Hacemos la misma operiación que para el estado inicial, pero una fila más abajo
  problem$actions_possible    <- data.frame("left", "right", "down", "top") #Preguntar Podriamos omitirlo no?
  problem$left_collumn        <- read.csv(file, sep=";", header = FALSE, skip=problem$size+3, nrows = 1)    # NECESITAMOS SUMARLE 1 A CADA DATO, PERO AÚN NO LOS TRENEMOS GUARDADOS COMO INTEGERS, NI LOS TENEMOS SEPARADOS, ASÍ QUE NO PODEMOS.  
  problem$right_collumn       <- read.csv(file, sep=";", header = FALSE, skip=problem$size+4, nrows = 1)   
  problem$down_collumn        <- read.csv(file, sep=";", header = FALSE, skip=problem$size+5, nrows = 1)   
  problem$top_collumn         <- read.csv(file, sep=";", header = FALSE, skip=problem$size+6, nrows = 1)   
  return(problem)
}

#HAY QUE PONER BIEN LO DE LA X Y LA Y

# Transforms a state into a string
to.string <- function (state) {
  actualState<- c(state[2]-1, state[1]-1)
  stateString <- toString (actualState)
  finalState <-gsub(" ", "", stateString)
  return (finalState)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- TRUE # Default value is FALSE.
  
  #Borde --> Acabado 
  #Barrera --> Bien¿?
  #Pies --> Mal
  
  # IZQUIERDA
  if (action == "left"){
    #Borde
    if (state[2]<1) return(FALSE)
    #Pies
    if (problem$table[state[2]][state[1]-1] == problem$table[state[2]][state[1]]) return (FALSE)
    #Barreras
    if (toString(state) %in% problem$left_collumn) return (FALSE)
    state2 <- c(state[2], state[1]-1) #Una posicion a la izquierda
    if (toString(state2) %in% problem$right_collumn) return (FALSE) #Hay que restarle 1 ya que la comprobacion se hace desde la casilla de la izquierda
  }
  
  # DERECHA
  if (action == "right"){
    #Borde
    if (problem$size[2]<state[1]) return (FALSE)
    #Pies
    if (problem$table[state[2]][state[1]-1] == state) return (FALSE)
    #Barreras
    if (toString(state) %in% problem$right_collumn) return (FALSE)
    state2 <- c(state[2], state[1]+1)
    if (toString(state2) %in% problem$left_collumn) return (FALSE)
    
  }
  
  # ABAJO
  if (action == "down"){
    #Borde
    if (problem$size[1]<state[1]) return (FALSE)
    #Pies
    if (problem$table[state[2]][state[1]-1] == state) return (FALSE)
    #Barreras
    if (toString(state) %in% problem$down_collumn) return (FALSE)
    state2 <- c(state[2]+1, state[1])
    if (toString(state2) %in% problem$top_collumn) return (FALSE)
  }
  
  #ARRIBA
  if (action == "up"){
    #Borde
    if (1 > state[2]) return (FALSE) #Preguntar Error en ejecucion
    #Pies
    if (problem$table[state[2]][state[1]-1] == state) return (FALSE)
    #Barreras
    if (toString(state) %in% problem$top_collumn) return (FALSE)
    state2 <- c(state[2]-1, state[1])
    if (toString(state2) %in% problem$down_collumn) return (FALSE)
  }
  
  return(result)
}


# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  if (action == "left") return (result <- c(state[2], state[1]-1))
  
  if (action == "right") return (result <- c(state[2], state[1]+1))
  
  if (action == "down") return (result <- c(state[2]+1, state[1]))
  
  if (action == "up") return (result <- c(state[2]-1, state[1]))
}
# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  # result <- FALSE # Default value is FALSE.
  result <- FALSE
  if (state[1] == problem$final_state[1] && state[2] == problem$final_state[2]) return (TRUE)
  
  return(result)
}


# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(problem$cost)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
	return(1) # Default value is 1.
}

#Devuelve una L o R
#Preguntar

getState <- function(coordinate, problem){ 
  state <- c(coordinate[2], coordinate[1])     
  return (state) #Devolver en integer!
}

#Devuelve una L o R
#Preguntar
getFeet <- function(state, problem){ 
  feet <- (problem$table[state[2], state[1]])
  return (feet)
}


# El state qué es en sí? La coordenada x,y o si la coordenada es L/R
# Preguntar por problem$actions: se pueden omitir? cómo estructurarlas concretamente?
     
# Falta alguna condición? (isApplicacble)
# Revisar formato de datos
# Preguntar por error en ejecución
# Cómo ejecutarlo?
# En los métodos hay que usaar todos los parámetros que hay?


