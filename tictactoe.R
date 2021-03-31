winning_combos <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

display <- function(state) {
  state.position <- rep(NA,9)
  i= 1
  while (i <=9) {
    if(is.na(state[i])) {
      state.position[i] <- i
    }

    else {
    state.position[i] <- state[i]
    }
    i <- i + 1
  }

  symbol <- "---+---+---"

  cat(state.position[1], "  | ", state.position[2], " |  ", state.position[3], "\n", symbol,
      "\n", state.position[4], "  | ", state.position[5], " |  ", state.position[6], "\n",
      symbol, "\n", state.position[7], "  | ", state.position[8], " |  ", state.position[9],
      "\n", "\n", sep = "")
}

update <- function(state, who, pos) {

  state[pos] <- who
  return(state)

}

computer_turn <- function(state) {
  x.pos <- which(state == "X")
  o.pos <- which(state == "O")
  if(length(x.pos) <= length(o.pos)) { #The computer is X
    pos <- BestSpot(my.pos = x.pos, o.pos, state)
  }
  else{
    pos <- BestSpot(my.pos = o.pos, x.pos, state)
  }
  return(as.integer(pos))
}

#computer needs to determine the best spot

BestSpot <- function(my.pos, opp.pos, state){
  i <- 1
  while(i < 9) {
    x <- winning_combos[[i]]
    my.match <- na.omit(match(my.pos, x))
    opp.match <- na.omit(match(opp.pos,x))
    if((length(my.match) == 2) && (length(opp.match) == 0)) #Can win in one step
    {return(x[-my.match])}
    if((length(opp.match) == 2) && (length(my.match) == 0)) #Need to block opponent
    {return(x[-opp.match])}
  i <- i + 1
  }
  open.spot <- which(is.na(state))
  return(sample(open.spot, 1))

}

check_winner <- function(state) {

  x.pos <- as.numeric(which(state == "X"))
  o.pos <- as.numeric(which(state == "O"))

  for(i in 1:8) {
    if(sum(winning_combos[[i]] %in% x.pos) == 3) {
      print("Yaay! X wins!")
      return(FALSE)
    }
  if(sum(winning_combos[[i]] %in% o.pos) == 3) {
    print("Yaay! O wins!")
    return(FALSE)
  }
  if(!(NA %in% state)) {
    print("Its a draw!")
    return(FALSE)
  }
  }
  return(TRUE)

}


valid.position <- function(position) {
  if(grepl("^[1-9]$", position)) {
  return(TRUE)
  }
  return(FALSE)
}

valid.input <- function(prompt, allowed_input, error){
  input = " "
  while(!(input %in% allowed_input)) {
    if(interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }

    cat(prompt, " ")

    input <- readLines(con = con, n = 1)

    if(input %in% c("q", "quit")){
      quit()
    }
    if(!(input %in% allowed_input)){
      cat(error, "\n")
      input <- readLines(con = con, n = 1)
    }
    return(input)
  }
}

initial_input <- function(){
  choices <- c("x", "X", "o", "O")
  input <- valid.input("X or O?", choices, "You can chose either X or O")
  return(toupper(input))
}


######## The game begins ##########
play <- function(){
  state <- rep(NA, 9)

  who <- initial_input()

  if(who == "X"){
                    player2 <- "computer"
                    if(interactive()){
                                      con <- stdin()
                                      }else{
                                      con <- "stdin"
                                            }
  cat("Please enter your name: ")
  player1 <- readLines(con = con, n = 1)
  }

if(who == "O"){
  player1 <- "computer"
   if(interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  cat("Please enter your name: ")
  player2 <- readLines(con = con, n = 1)}

repeat{

    display(state)
    if (player1 == "computer") {
      posx <- computer_turn(state)
      state <- update(state, "X", posx)
    } else {
      if(interactive()) {
        con <- stdin()
      } else {
        con <- "stdin"
      }
      cat(player1, "Which position do you want to play ")
      posx <- readLines(con = con, n = 1)

      while(!valid.position(posx)){
        print("Please enter a valid input from 1-9")
        if(interactive()) {
          con <- stdin()
        } else {
          con <- "stdin"
        }
        cat(player1, "Which position do you want to play ", sep = ' ')
        posx <- readLines(con = con, n = 1)
      }

        posx <- as.integer(posx)

        while(!is.na(state[posx])){
          print("Oops! This position seems to be taken. Try again!")
          if(interactive()) {
            con <- stdin()
          } else {
            con <- "stdin"
          }
          cat(player1, "Which position do you want to play ")
          posx <- readLines(con = con, n = 1)
          posx <- as.integer(posx)
        }

        state <- update(state, "X", posx)
     }
    if (!check_winner(state)) {
      break
    }
    display(state)
    if (player2 == "computer") {
      poso <- computer_turn(state)
      state <- update(state, "O", poso)
    } else {
      if(interactive()) {
        con <- stdin()
      } else {
        con <- "stdin"
      }
      cat(player2, "Which position do you want to play ")
      poso <- readLines(con = con, n = 1)
      while(!valid.position(poso)){
        print("Please enter a valid input from 1-9")
        if(interactive()) {
          con <- stdin()
        } else {
          con <- "stdin"
        }
        cat(player2, "Which position do you want to play ", sep = ' ')
        poso <- readLines(con = con, n = 1)
      }
        poso <- as.integer(poso)

        while(!is.na(state[poso])){
          print("Oops! This position seems to be taken. Try again!")
          if(interactive()) {
            con <- stdin()
          } else {
            con <- "stdin"
          }
          cat(player2, "Which position do you want to play ")
          poso <- readLines(con = con, n = 1)
          poso <- as.integer(poso)
        }

        }
        state <- update(state, "O", poso) # update board

    if (!check_winner(state)) {
      break
    }
}
display(state)

}


play()

