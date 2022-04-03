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
  
  problem$name                <- paste0("Feet Maze - ", file)
  problem$size                <- c(as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[1]),as.integer(read.csv(file, sep=";", header = FALSE, nrows=1)[2])) #y,x
  problem$table               <- read.csv(file, sep=";", header = FALSE, skip=1, nrows=problem$size[1]) #y,x
  problem$state_initial       <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[2])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=1+problem$size[1], nrows=1)[1]+1)) #y,x We look for the position of the row where the state is, and add one to the corresponding number, since vectors in R start with 1. 
  problem$state_final         <- c(as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[2])+1, as.integer(read.csv(file, sep=",", header = FALSE, skip=2+problem$size[1], nrows=1)[1]+1)) #y,x 
  problem$actions_possible    <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  problem$left                <- read.csv(file, sep=";", header = FALSE, skip=problem$size[1]+3, nrows = 1) # Although the index of the problem starts at 0, we have not added 1. This is done during the operations.
  problem$right               <- read.csv(file, sep=";", header = FALSE, skip=problem$size [1]+4, nrows = 1)
  problem$down                <- read.csv(file, sep=";", header = FALSE, skip=problem$size [1]+5, nrows = 1)
  problem$top                 <- read.csv(file, sep=";", header = FALSE, skip=problem$size[1]+6, nrows = 1)
  
  return(problem)
}


# Transforms a state into a string
to.string <- function (state) {
  actualState<- c(state[1]-1, state[2]-1)
  stateString <- toString (actualState) # Vector to string transformation. c(x,y) -> "x, y"
  finalState <-gsub(" ", "", stateString) # Removing space between characters. "x, y" -> "x,y"
  return (finalState)
}


# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE. 
  
  # LEFT
  if (action == "Left"){ 
    # Edge
    l1 <- 1 >= state[2]
    # Feet
    l2 <- problem$table[state[1], state[2]-1] == problem$table[state[1], state[2]]
    # Barriers
    l3 <- to.string(state) %in% problem$left
    if (state[2] != 1){ # We check that it is not on the edge of the board so that at the next check it is not outside the board.
      state2 <- c(state[1], state[2]-1)
      l4 <- to.string(state2) %in% problem$right
    }else{l4 <- FALSE}
    
    if (l1 == FALSE && l2 == FALSE && l3 == FALSE && l4 == FALSE) {return(TRUE)} else{return(FALSE)}
    }

  # RIGHT
  if (action == "Right"){
    # Edge
    r1 <- problem$size[2] <= state[2]
    # Feet
    r2 <- problem$table[state[1], state[2]+1] == problem$table[state[1], state[2]]
    # Barriers
    r3 <- to.string(state) %in% problem$right
    if (problem$size[2] != state[2]){
      state2 <- c(state[1], state[2]+1)
      r4 <- to.string(state2) %in% problem$left
    }else{r4 <- FALSE}
    
    if (r1 == FALSE && r2 == FALSE && r3 == FALSE && r4 == FALSE) {return (TRUE)} else{return(FALSE)}
  }
  
  # DOWN
  if (action == "Down"){
    # Edge
    d1 <- 1 >= state[1]
    # Feet
    d2 <- problem$table[state[1]-1, state[2]] == problem$table[state[1], state[2]]
    # Barriers
    d3 <- to.string(state) %in% problem$down
    if (1 != state[1]){
      state2 <- c(state[1]-1, state[2])
      d4 <- to.string(state2) %in% problem$top
    }else{d4 <- FALSE}

    if (d1 == FALSE && d2 == FALSE && d3 == FALSE && d4 == FALSE) {return (TRUE)} else{return(FALSE)}
  }
 
  # UP 
  if (action == "Up"){
    # Edge
    u1 <- problem$size[1] <= state[1]
    # Feet
    u2 <- problem$table[state[1]+1, state[2]] == problem$table[state[1], state[2]]
    # Barriers
    u3 <- to.string(state) %in% problem$top
    if (problem$size[1] != state[1]){
      state2 <- c(state[1]+1, state[2])
      u4 <- to.string(state2) %in% problem$down
    }else{u4 <- FALSE}
    
    if (u1 == FALSE && u2 == FALSE && u3 == FALSE && u4 == FALSE) {return(TRUE)} else{return(FALSE)}
    }

  return(result)
}


# Returns the state resulting on applying the action over the state.
# Note that the origin of coordinates is in the upper left corner!
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  if (action == "Left") (result <- c(state[1], state[2]-1))
  if (action == "Right") (result <- c(state[1], state[2]+1))
  if (action == "Down") (result <- c(state[1]-1, state[2]))
  if (action == "Up") (result <- c(state[1]+1, state[2]))
  return (result)
}


# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) { 
  result <- FALSE # Default value is FALSE.
  if (state[1] == final_state[1] && state[2] == final_state[2]) result <- TRUE
  return (result)
}


# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1)
}


# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
	return(1) # Default value is 1.
}