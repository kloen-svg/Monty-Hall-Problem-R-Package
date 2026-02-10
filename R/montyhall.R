#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Randomly select a door for the contestant.
#' 
#' @description
#' `select_door()` automatically and randomly samples a door from 1-3 as a character. 
#' 
#' @details
#' #' In the original Monty Hall premise the contestant is given the option to select a door.
#' This sets the original door for the game, and allows for comparison against the strategy
#' of picking a different door.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a number between 1 and 3 indicating the initial door selection,
#' specifically a double type numeric.
#' 
#' @examples
#' select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Reveal a goat door.
#' 
#' @description
#' `open_goat_door()` reveals a door that the contestant has NOT selected and
#' contains a goat behind it.
#' 
#' @details
#' In the secondary stage of the original Monty Hall problem, the host reveals
#' one of the remaining choices that is a losing door.
#' 
#' @param 
#' The `game` parameter passes the configured game. `a.pick` passes the initial 
#' door selection.
#' 
#' @return 
#' The function returns a single number between 1 and 3. Also a double type numeric.
#' 
#' @examples
#' this.game <- create_game()
#' my.pick <- 3
#' open_goat_door(this.game, my.pick)
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change selection to the other door.
#' 
#' @description
#' `change_door()` is a function that either keeps the initial pick or changes
#' the door selection.
#' 
#' @details 
#' #' This is the stage in the Monty Hall setup where the contestant is given the
#' opporunity to change their selection to the other unopened door.
#' 
#' @param 
#' `stay` a TRUE/FALSE condition indicating whether the contestant wants to 
#' switch options. TRUE is the default condition.
#' `opened.door` passes a number through that was the revealed door.
#' `a.pick` is the number indicating the door the contestant initially selected.
#' 
#' @return 
#' Returns a number between 1 and 3 as a numeric double type.
#' 
#' @examples
#' #' this.game <- create_game()
#' my.pick <- 3
#' open.sesame <- open_goat_door(this.game, my.pick)
#' change_door(stay=T, open.sesame, my.pick)
#' If the contestant decides to switch to the other door:
#' change_door(stay= F, open.sesame, my.pick)
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine win or loss of the game.
#' 
#' @description
#' Returns whether the game is won or lost.
#' 
#' @details
#' Determines whether the player has successfully identified the door with the car,
#' or has revealed the other goat.
#' 
#' @param 
#' `final.pick` is the final selection of the contestant as a number.
#' `game` is a vector that indicates the position of each prize.
#' 
#' @return 
#' Returns a vector character. 
#' 
#' @examples
#' test.game <- create_game()
#' first.pick <- select_door()
#' door.reveal <- open_goat_door(test.game, first.pick)
#' final.decision <- change_door(stay = T, door.reveal, first.pick)
#' outcome.stay <- determine_winner(final.decision, test.game)
#' outcome.stay
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Nested Monty Hall Problem function.
#' 
#' @description
#' Automates the Monty Hall problem setup and returns the outcomes for both scenarios.
#' 
#' @details
#' This function automates and returns the outcomes for staying with the initial decision
#' as well as the strategy to change doors. This allows the user to quickly iterate
#' and analyze numerous games without manually entering each individual game.
#' 
#' @param ... no arguments are used by the function
#' 
#' @return 
#' The function returns a list with a 2x2 data frame.
#' 
#' @examples
#' outcome <- play_game()
#' outcome
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Iterates and stores simulation results for the Monty Hall problems.
#' 
#' @description
#' `play_n_games()` allows users to loop the `play_game()` function for a defined
#' number of loops and then returns the summary of all outcomes.
#' 
#' @details
#' Users can define a number of times to run the simulation. Each loop contains
#' the outcome for stay and changing door strategies and returns it into a list.
#' This allows for easy statistical analysis.
#' 
#' @param 
#' `n=100` n is the number of simulations to be run. The default value is 100.
#' 
#' @return 
#' Returns the results as a data frame. Has dplyr dependencies.
#' 
#' @examples
#' play_n_games()
#' play_n_games( n = 5000)
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}

# USER-DEFINED GAME

#' @title
#' Create a user defined Monty Hall setup.
#' 
#' @description
#' `create_game.playerchoice()` allows users to create their own bespoke Monty Hall setup.
#' 
#' @details
#' #' This function presents the user the opportunity to create a Monty Hall problem
#' with user defined numbers of goats and cars. The function also randomizes the order of
#' goats and cars u8sing the sample function.
#' 
#' @param
#' `goats` the number of goats in a game.
#' `cars` the number of cars in a game. Both parameters must be greater than 0.
#' Ideally there are a minimum of 2 goats to allow for the reveal logic to work for
#' the fully wrapped function.
#' 
#' @return 
#' The function returns variable length character vector indicating the
#' positions of goats and the car. The length of the vector is determined by the number
#' of goats and cars.
#' 
#' @examples
#' create_game.playerchoice(8,1)
#' create_game.playerchoice(4, 17)
#' 
#' @export
create_game.playerchoice <- function(goats, cars)
{
  x <- rep("goat", goats)
  y <- rep("car", cars)
  sample_list <- c(x,y)
  sample_size <- length(sample_list)
  a.game <- sample( c(sample_list), size=sample_size, replace=F )
  return( a.game )
} 

#' @title
#' Door list generator.

#' @description
#' Creates a vector of numbers based on user input.

#' @details
#' Creates a vector with the appropriate number of doors as defined by the user-defined
#' scenario. This is largely a background function to make assigning and revealing
#' doors easier, especially in the fully nested function.

#' @param 
#' `a.game` is the user defined vector representing the 
 
#' @return
#' Returns a vector with the number of doors. 

#' @examples
#' new.game <- create_game.playerchoice(8, 5)
#' new.doors <- door_list.playerchoice(new.game)

#' @export
door_list.playerchoice <- function(a.game)
{  doors <-seq(from =1, to =length(a.game), by=1)
return( doors )
}

#' @title
#' Door selection for user-defined game.

#' @description
#' `select_door.playerchoice` randomly samples a number from the list of doors and returns one value.

#' @details
#' Since in this version of the simulation, it is unknown what number of doors there will be,
#' the code allows for any number of doors to be presented for selection. Only one door
#' will be selected.

#' @param 
#' `door.order` is a vector with the number of doors in the user-defined game scenario.

#' @return 
#' Returns a number representing the initial door choice.

#' @examples
#' new.game <- create_game.playerchoice(8, 5)
#' new.doors <- door_list.playerchoice(new.game)
#' first.pick <- select_door.playerchoice(new.doors)

#' @export
select_door.playerchoice<- function(door.order )
{
  doors <- door.order 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  
}

#' @title
#' Open one goat door for the user defined game.

#' @description
#' Returns an unopened goat door from a user-definied configuration as a numeric double.

#' @details
#' Using the user-defined prize scenario, reveal a single goat door.

#' @param
#' `game` - the vector defining the prize arrangement.
#' `a.pick` - the initial choice of door.
#' `doors` - the vector for the list of doors.

#' @return 
#' The function returns a single number as a double type numeric between
#' one and *n* doors from the `doors` parameter.

#' @examples
#' new.game <- create_game.playerchoice(8, 5)
#' new.doors <- door_list.playerchoice(new.game)
#' first.pick <- select_door.playerchoice(new.doors)
#' goat.reveal <- open_goat_door.playerchoice(new.game, first.pick, new.doors)

#' @export
open_goat_door.playerchoice<- function( game, a.pick, doors )
{
  
  
  
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ]
    car.doors <- doors[game == "car"]
    goat.open <- sample( goat.doors, size=1 )
    opened.doors <- c(goat.open)
  }
  
  if( game[ a.pick ] == "goat" )
  { 
    goat.doors <- doors[ game != "car" & doors != a.pick]
    car.doors <- doors[game == "car"]
    goat.open <- sample( goat.doors, size=1 )
    opened.doors <- c(goat.open)
    
  }
  return( opened.doors ) 
}

#' @title
#' Change Door Function User Defined Scenario

#' @description
#' `change_door.playerchoice()` allows a user to opt into the stay strategy, or select
#' a previously unselected door.

#' @details
#' This is the same as the base function, only using the user-defined scenario.
#' If a user switches doors, an unopened door is randomly selected from the remaining choices.

#' @param 
#' `stay` a true/false operator which determins if the user stays with the same initial selection,
#' or if a different door will be revealed for the final prize determination. Default setting is TRUE
#' for sticking with the initial option.
#' `opened.doors` passes all opened door information.
#' `a.pick` the initial pick the user made.
#' `doors` the vector determining how many doors there are in a given game.

#' @return 
#' Returns the final door decision as a numeric double.
#' 
#' @examples
#' new.game <- create_game.playerchoice(8, 5)
#' new.doors <- door_list.playerchoice(new.game)
#' first.pick <- select_door.playerchoice(new.doors)
#' goat.reveal <- open_goat_door.playerchoice(new.game, first.pick, new.doors)
#' final.choice <- change_door.playerchoice(stay=F, new.doors, first.pick, new.doors)

#' @export
change_door.playerchoice <- function( stay=T, opened.doors, a.pick,doors)
{
  
  invalid.doors <- c(a.pick, opened.doors)
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- sample(doors[! doors %in% invalid.doors], size=1)
  }
  
  return( final.pick )  # number between 1 and 5
}

#' @title
#' Determine winner in player defined game.

#' @description
#' Determines if the player has won or lost.

#' @details
#' Returns the outcome of the user-definied Monty Hall Problem.

#' @param 
#' `final.pick` the prize door to be revealed.
#' `game` the game setup previously generated by the `create_gameplayerchoice()` function

#' @return
#' Returns a character type value. 

#' @examples
#' new.game <- create_game.playerchoice(8, 5)
#' new.doors <- door_list.playerchoice(new.game)
#' first.pick <- select_door.playerchoice(new.doors)
#' goat.reveal <- open_goat_door.playerchoice(new.game, first.pick, new.doors)
#' final.choice <- change_door.playerchoice(stay=T, new.doors, first.pick, new.doors)
#' final.outcome <- determine_winner.playerchoice(final.choice, new.game)

#' @export
determine_winner.playerchoice <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}


#' @title
#' Automated user-defined Monty Hall Game

#' @description
#' This is the same as the base Monty Hall simulator function, but with user defined parameters
#' with regards to the number of goats and cars.

#' @details
#' A fully nested and self-contained function that returns outcomes in a Monty Hall style problem.
#' The function returns the results for staying and changing doors.

#' @param 
#' `goats` the number of goats in a game.
#' `cars` the number of cars in a game. Both parameters must be greater than 0.
#' Ideally there are a minimum of 2 goats to allow for the reveal logic to work.
#' The function may misbehave otherwise if there are not at least 1 car and 2 goats, which is
#' equivalent to the original Monty Hall Problem setup.
 
#' @return 
#' Returns a character list of both outcomes--stay or switch--in response to a user defined game. 

#' @examples
#' play_game.playerchoice(2, 3) %>% pander()  # pander is for pretty printing

#' @export
play_game.playerchoice <- function( goats,cars )
{
  new.game          <- create_game.playerchoice(goats,cars)
  door.setup        <- door_list.playerchoice(new.game) 
  first.pick        <- select_door.playerchoice(door.setup)
  opened.doors      <- open_goat_door.playerchoice( new.game, first.pick, door.setup)
  final.pick.stay   <- change_door.playerchoice( stay=T, opened.doors, first.pick, door.setup )
  final.pick.switch <- change_door.playerchoice( stay=F, opened.doors, first.pick, door.setup )
  outcome.stay      <- determine_winner.playerchoice( final.pick.stay, new.game  )
  outcome.switch    <- determine_winner.playerchoice( final.pick.switch, new.game )
  
  strategy     <- c( "stay", "switch" )
  outcome      <- c( outcome.stay, outcome.switch )
  game.results.playerchoice <- data.frame( strategy, outcome ) 
  
  return( game.results.playerchoice )
}

#' @title
#' Iterates and stores simulation results for the Monty Hall problems.
#' 
#' @description
#' `play_n_games.choice()` allows users to loop the `play_game.playerchoice()` function for a defined
#' number of loops and then returns the summary of all outcomes.
#' 
#' @details
#' Users can define a number of times to run the simulation. Each loop contains
#' the outcome for stay and changing door strategies and returns it into a list.
#' This allows for easy statistical analysis.
#' 
#' @param 
#' `n=100` n is the number of simulations to be run. The default value is 100.
#' 
#' @return 
#' Returns the results as a data frame. Has dplyr dependencies.
#' 
#' @examples
#' play_n_games.choice( 5,8 )
#' play_n_games.choice( n = 5000, 3, 4)
#' 
#' @export
play_n_games.choice <- function( n=100, goats, cars )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game.playerchoice(goats, cars)
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}