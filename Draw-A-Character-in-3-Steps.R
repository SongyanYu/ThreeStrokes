#---
# try to finish drawing a character in three steps
# author: songyan Yu
# date created: 07/04/2022
#---

# twelve sides of the character
side01 <- c('A1', 'A2')
side02 <- c('A2', 'A3')
side03 <- c('A3', 'A4')
side04 <- c('A1', 'A4')

side05 <- c('B1', 'B2')
side06 <- c('B2', 'B3')
side07 <- c('B3', 'B4')
side08 <- c('B1', 'B4')

side09 <- c('A1', 'B1')
side10 <- c('A2', 'B2')
side11 <- c('A3', 'B3')
side12 <- c('A4', 'B4')

side.lst <- list(side01, side02,
                 side03, side04,
                 side05, side06,
                 side07, side08,
                 side09, side10,
                 side11, side12)

# load customised functions
which_side <- function(step){
  which(sapply(side.lst, FUN = function(x){
    step %in% x
  }))
}

forward <- function(x){
  if(substr(x, 2, 2) == '4'){
    forward.point <- 1
    forward.step <- paste0(substr(x, 1, 1), forward.point)
  }else{
    forward.point <- as.numeric(substr(x, 2, 2)) + 1
    forward.step <- paste0(substr(x, 1, 1), forward.point)
  }
  return(forward.step)
}

backward <- function(x) {
  if(substr(x, 2, 2) == '1'){
    backward.point <- 4
    backward.step <- paste0(substr(x, 1, 1), backward.point)
  }else{
    backward.point <- as.numeric(substr(x, 2, 2)) - 1
    backward.step <- paste0(substr(x, 1, 1), backward.point)
  }
}

leap <- function(x){
  new.circle <- ifelse(substr(x, 1, 1) == 'A', 'B', 'A')
  new.step <- paste0(new.circle,substr(x, 2, 2))
  return(new.step)
}

# try 10,000 times to find a solution
i = 1
steps.lst <- list()
sides.lst <- list()
while(i <= 10000){
  start <- sample(c('A1','A2', 'A3', 'A4', 'B1', 'B2', 'B3', 'B4'), 1)
  steps <- start
  sides <- 0
  
  # first stroke
  n <- 0
  while(n <= 1000){
    next.step <- sample(c(forward(start), leap(start), backward(start)), 1)
    side <- intersect(which_side(start), which_side(next.step))
    
    if(!(side %in% sides)){
      sides <- append(sides, side)
      steps <- append(steps, next.step)
      
      start <- next.step
    }else{
      n <- n + 1
    }
  }
  
  # second stroke
  rest.sides <- setdiff(c(1:12), sides)
  rest.steps <- unique(unlist(side.lst[rest.sides]))
  
  start <- sample(rest.steps, 1)
  steps <- append(steps, start)
  
    while(n <= 2000){
    next.step <- sample(c(forward(start), leap(start), backward(start)), 1)
    side <- intersect(which_side(start), which_side(next.step))
    
    if(!(side %in% sides)){
      sides <- append(sides, side)
      steps <- append(steps, next.step)
      
      start <- next.step
    }else{
      n <- n + 1
    }
  }
  
  # third stroke
  rest.sides <- setdiff(c(1:12), sides)
  rest.steps <- unique(unlist(side.lst[rest.sides]))
  
  start <- sample(rest.steps, 1)
  steps <- append(steps, start)
  
  while(n <= 3000){
    next.step <- sample(c(forward(start), leap(start), backward(start)), 1)
    side <- intersect(which_side(start), which_side(next.step))
    
    if(!(side %in% sides)){
      sides <- append(sides, side)
      steps <- append(steps, next.step)
      
      start <- next.step
    }else{
      n <- n + 1
    }
  }
  
  sides.lst[[i]] <- sides
  steps.lst[[i]] <- steps
  
  i <- i + 1
  cat(i, '\n')
}

summary(lengths(sides.lst))
