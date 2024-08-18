library(tidyverse)

# Helper Functions
# --------------------------------------------------------------------------------------

validate_ship_input <- function(input, max_size) {
  if (!grepl("^[A-Za-z]+-[0-9]+$", input)) {
    return(FALSE)
  }
  
  parts <- strsplit(input, "-")[[1]]
  size <- as.numeric(parts[2])
  
  if (is.na(size) || size < 1 || size > max_size) {
    return(FALSE)
  }
  
  return(TRUE)
}


# Function to set up ships based on user input
setup_ships <- function(max_size, max_ships) {
  ship_list <- list()
  
  for (i in 1:max_ships) {
    repeat {
      ships <- readline(prompt = paste("What would you like to name ship", i, "? Please answer in the format: Name-Size (e.g. Battleship-4) \n"))
      if (validate_ship_input(ships, max_size)) {
        break
      } else {
        cat("Invalid input. Please answer in the format: Name-Size (e.g. Battleship-4) and ensure size is between 1 and", max_size, "\n")
      }
    }
    
    parts <- strsplit(ships, "-")[[1]]
    ship_name <- parts[1]
    ship_size <- as.numeric(parts[2])
    
    # Create ship object and add to list
    ship_obj <- ship(name = ship_name, size = ship_size)
    ship_list <- c(ship_list, list(ship_obj))
    
    if (i < max_ships) {
      while (TRUE) {
        done <- readline(prompt = "Do you want to add another ship? Answer 'yes' or 'no': \n")
        done <- tolower(done)
        
        if (done == "yes" || done == "no") {
          break
        } else {
          cat("Invalid input. Please answer either 'yes' or 'no'.\n")
        }
      }
      
      if (done == 'no') {
        break
      }
    }
  }
  
  return(ship_list)
}

valid_position_input <- function(input, ocean_dim) { # this one is for position in general
  input <- toupper(input)  # Ensure input is uppercase
  
  # Check if the input matches the required format
  if (!grepl("^[A-Z]-[0-9]+$", input)) {
    return(FALSE)
  }
  
  parts <- strsplit(input, "-")[[1]]
  row <- parts[1]
  col <- parts[2]
  
  row <- match(row, LETTERS)
  col <- as.numeric(col)

  # Check for NA values which indicate invalid row or column values
  if (is.na(row) || is.na(col)) {
    
    return(FALSE)
  }
  
  # Check if the start and end positions are within the ocean dimensions
  if (row < 1 || row > ocean_dim[1] || col < 1 || col > ocean_dim[2]) {
    
    return(FALSE)
  }
  
  return(TRUE)
}

validate_position_input <- function(input, ocean_dim, ship_size) {
  input <- toupper(input)  # Ensure input is uppercase
  
  # Check if the input matches the required format
  if (!grepl("^[A-Z]-[0-9]+,[A-Z]-[0-9]+$", input)) {
    return(FALSE)
  }
  
  parts <- strsplit(input, ",")[[1]]
  start <- strsplit(parts[1], "-")[[1]]
  end <- strsplit(parts[2], "-")[[1]]
  
  # Check if the split results in valid components
  if (length(start) != 2 || length(end) != 2) {
    return(FALSE)
  }
  
  start_row <- match(start[1], LETTERS)
  start_col <- as.numeric(start[2])
  end_row <- match(end[1], LETTERS)
  end_col <- as.numeric(end[2])
  
  # Check for NA values which indicate invalid row or column values
  if (is.na(start_row) || is.na(start_col) || is.na(end_row) || is.na(end_col)) {
    return(FALSE)
  }
  
  # Check if the start and end positions are within the ocean dimensions
  if (start_row < 1 || start_row > ocean_dim[1] || start_col < 1 || start_col > ocean_dim[2] ||
      end_row < 1 || end_row > ocean_dim[1] || end_col < 1 || end_col > ocean_dim[2]) {
    return(FALSE)
  }
  
  # Ensure the ship is placed either horizontally or vertically
  if (start_row != end_row && start_col != end_col) {
    return(FALSE)
  }
  
  # Check if the size of the ship matches the specified ship size
  if (start_row == end_row) {  # Horizontal placement
    if (abs(end_col - start_col) + 1 != ship_size) {
      return(FALSE)
    }
  } else {  # Vertical placement
    if (abs(end_row - start_row) + 1 != ship_size) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

generate_random_position <- function(ship_size, ocean_dim, occupied_positions) {
  repeat {
    orientation <- sample(c("horizontal", "vertical"), 1)
    if (orientation == "horizontal") {
      start_row <- sample(1:ocean_dim[1], 1)
      start_col <- sample(1:(ocean_dim[2] - ship_size + 1), 1)
      end_row <- start_row
      end_col <- start_col + ship_size - 1
    } else {
      start_row <- sample(1:(ocean_dim[1] - ship_size + 1), 1)
      start_col <- sample(1:ocean_dim[2], 1)
      end_row <- start_row + ship_size - 1
      end_col <- start_col
    }
    
    positions <- if (orientation == "horizontal") {
      paste0(LETTERS[start_row], "-", start_col:end_col)
    } else {
      paste0(LETTERS[start_row:end_row], "-", start_col)
    }
    
    if (!any(positions %in% occupied_positions)) {
      return(c(start_row, start_col, end_row, end_col))
    }
  }
}

place_ships <- function(ships, ocean_dim, gameboard) {
  occupied_positions <- character()  # Initialize an empty character vector to keep track of occupied positions
  
  for (i in seq_along(ships)) {  # Iterate over indices to ensure updates in the list
    ship <- ships[[i]]  # Extract the ship object
    cat("Processing ship:", ship$name, "\n")
    
    # Ask the user if they want to place the ship manually or randomly, with validation
    while (TRUE) {
      choice <- readline(prompt = paste("Do you want to choose the position for the", ship$name, "of size", ship$size, "? (yes/no): "))
      choice <- tolower(choice)
      if (choice %in% c("yes", "no")) break
      cat("Invalid input. Please answer either 'yes' or 'no'.\n")
    }
    
    if (choice == "no") {
      # Generate a random position for the ship
      repeat {
        pos <- generate_random_position(ship$size, ocean_dim, occupied_positions)
        positions <- if (pos[1] == pos[3]) {  # Check if the ship is placed horizontally
          paste0(LETTERS[pos[1]], "-", pos[2]:pos[4])
        } else {  # If the ship is placed vertically
          paste0(LETTERS[pos[1]:pos[3]], "-", pos[2])
        }
        
        if (!any(positions %in% occupied_positions)) {
          occupied_positions <- c(occupied_positions, positions)  # Update occupied positions with the new ship positions
          ship$position <- list(paste0(LETTERS[pos[1]], "-", pos[2]), paste0(LETTERS[pos[3]], "-", pos[4]))  # Assign the generated position to the ship
          break
        }
      }
    } else {
      # If the ship has a position assigned
      repeat {
        pos_input <- readline(prompt = paste("Enter position for", ship$name, "of size", ship$size, "in format StartRow-StartCol,EndRow-EndCol (e.g., A-1,A-4):\n"))
        if (validate_position_input(pos_input, ocean_dim, ship$size)) {  # Validate the user input
          pos_input <- toupper(pos_input)
          parts <- strsplit(pos_input, ",")[[1]]  # Split the input into start and end parts
          start <- strsplit(parts[1], "-")[[1]]  # Further split the start part into row and column
          end <- strsplit(parts[2], "-")[[1]]  # Further split the end part into row and column
          positions <- if (start[1] == end[1]) {  # Check if the ship is placed horizontally
            paste0(start[1], "-", as.numeric(start[2]):as.numeric(end[2]))  # Create position strings for horizontal placement
          } else {  # If the ship is placed vertically
            paste0(LETTERS[match(start[1], LETTERS):match(end[1], LETTERS)], "-", start[2])  # Create position strings for vertical placement
          }
          
          if (!any(positions %in% occupied_positions)) {  # Check if the positions do not overlap with existing ships
            occupied_positions <- c(occupied_positions, positions)  # Update occupied positions with the new ship positions
            ship$position <- list(paste0(start[1], "-", start[2]), paste0(end[1], "-", end[2]))
            break
          } else {
            cat("Positions overlap with another ship. Try again.\n")
          }
        } else {
          cat("Invalid input. Please follow the format and ensure the ship fits within the ocean dimensions and the ship's size.\n")
        }
      }
    }
    
    # Update the ship object in the list
    # print(occupied_positions)
    ships[[i]] <- ship
  }
  
  # Update the gameboard
  for (ship in ships) {
    start_pos <- ship$position[[1]]
    end_pos <- ship$position[[2]]
    start_row <- match(substr(start_pos, 1, 1), LETTERS)
    start_col <- as.numeric(substr(start_pos, 3, nchar(start_pos)))
    end_row <- match(substr(end_pos, 1, 1), LETTERS)
    end_col <- as.numeric(substr(end_pos, 3, nchar(end_pos)))
    
    if (start_row == end_row) {  # Horizontal placement
      cols <- start_col:end_col
      gameboard[start_row, cols] <- "S"
    } else {  # Vertical placement
      rows <- start_row:end_row
      gameboard[rows, start_col] <- "S"
    }
  }
  
  return(list(ships = ships, gameboard = gameboard))
}


initialize_game <- function(player) {
  gameboard <- NULL
  player_fleet <- c()
  
  board_size <- readline(prompt = "What size board are we playing with? (e.g., 10,10) \n")
  valid_input <- grepl("^(2[0-5]|1[0-9]|[5-9]),(2[0-5]|1[0-9]|[5-9])$", board_size)
  
  while (!valid_input) {
    cat("Invalid input. Please enter board size in the format X,Y where X and Y are between 5 and 25.\n")
    board_size <- readline(prompt = "What size board are we playing with? (e.g., 10,10) \n")
    valid_input <- grepl("^(2[0-5]|1[0-9]|[5-9]),(2[0-5]|1[0-9]|[5-9])$", board_size)
  }
  
  parts <- strsplit(board_size, ",")[[1]]
  row <- as.integer(parts[1])
  col <- as.integer(parts[2])
  
  
  max_size <- max(row,col)
  
  ocean_dim <- c(row,col)
  
  column_names <- seq_len(col)
  row_names <- LETTERS[1:row]
  
  gameboard <- matrix("-", nrow = row, ncol = col)
  
  rownames(gameboard) <- row_names
  colnames(gameboard) <- column_names
  
  
  good_inputs <- c("Default", "Choose")
  ship_setup <- readline(prompt = "Would you like to choose your own ships or use the default ships? Please answer 'Default' or 'Choose': \n")
  
  while (!(ship_setup %in% good_inputs)) {
    cat("Invalid input. Please answer 'Default' or 'Choose'.\n")
    ship_setup <- readline(prompt = "Would you like to choose your own ships or use the default ships? Please answer 'Default' or 'Choose': \n")
  }
  
  if (ship_setup == "Default") {
    player_fleet <- fleet(admiral = player, ocean = ocean_dim)
  } else {
    
    player_ships <- setup_ships(max_size, max_size)
    
    player_fleet <- fleet(admiral = player, ships = player_ships, ocean = ocean_dim)
  }
  
  player_result <- place_ships(player_fleet$ships, ocean_dim, gameboard)
  player_fleet$ships <- player_result$ships
  gameboard <- player_result$gameboard
  
  return(list(fleet = player_fleet, gameboard = gameboard))
  
}

update_history <- function(history, from, to, target, hit) {
  history <- add_row(history, 
                     from = from, 
                     to = to, 
                     target = target, 
                     hit = hit)
  return(history)
}



# Create several class of objects and create the class constructors
# --------------------------------------------------------------------------------------

ship <- function(name, size) {
  # making sure that the name is a character string
  stopifnot(is.character(name), length(name) == 1)
  # making sure that the size is a number
  stopifnot(is.numeric(size), length(size) == 1)

  structure(list(
    name = name,
    size = size,
    position = list(NULL,NULL),
    hits = rep(FALSE, size),
    sunk = FALSE),
    class = "ship"
    )
}

fleet <- function(admiral, ocean = c(10, 10), ships = NULL) {

  # makes sure that ocean is a length 2 numeric vector
  stopifnot(is.numeric(ocean), length(ocean) == 2)

  if(is.null(ships)) {
    ships = default_ships()
  }

  structure(list(
    admiral = admiral,
    ocean = ocean,
    ships = ships),
    class = "fleet"
    )
}

battleship <- function(fleets = list()) {

  if(length(fleets) < 2) {
    if(length(fleets) == 0) {
      fleets <- list(
        fleet("Player 1"),
        fleet("Player 2")
      )
    }

    else if(length(fleets) == 1) {
      existing_admiral <- fleet$admiral[[1]]
      new_admiral <- ifelse(existing_admiral == "Player 1", "Player 2", "Player 1")
      fleets <- c(fleets, list(fleet(new_admiral)))
    }
  }

  structure(list(
    fleets = fleets,
    history = tibble::tibble(from = character(), to = character(), target = character(), hit = logical())),
    class = "battleship"
  )
}


# Gameplay
# --------------------------------------------------------------------------------------


play_bs <- function(players = c("human", "ai_305721672"), strengths = c(9, 9), verbose, plot_before_turn = "none", oceans = list(c(10,10), c(10,10)), ships = list(default_ships(), default_ships())) {
  
  if (length(players) != 2) {
    if (length(players) == 1) {
      # If only one player is provided, add a default second player
      players <- c(players, "ai_305721672")
      strengths <- c(strengths, 9)
    } else {
      stop("players must be a character vector of length 1 or 2.")
    }
  }
  
  game <- battleship()
  
  result <- list()
  lose_pb <- FALSE
  
  ai <- FALSE
  if(players[1] == "self_ai_305721672" && players[2] == "self_ai_305721672") {

    ai <- TRUE
    player_1 <- "Player 1"
    player_2 <- "Player 2"

  } else{

    player_1 <- players[1]
    player_2 <- players[2]
  }
  
  
  player1_fleet <- game$fleets[[1]]
  player2_fleet <- game$fleets[[2]]
  
  player1_fleet$admiral <- player_1
  player2_fleet$admiral <- player_2
  
  player1_fleet$ocean <- oceans[[1]]
  player2_fleet$ocean <- oceans[[2]]
  
  player1_fleet$ships <- ships[[1]]
  player2_fleet$ships <- ships[[2]]
  
  
  player1_fleet <- position_fleet(player1_fleet)
  player2_fleet <- position_fleet(player2_fleet)
  
  
  game$fleets <- list(player1_fleet, player2_fleet)
  
  if(verbose == TRUE) {
    cat("Players have setup their boards. Game Start! \n")
  }
  
  sink_order <- c("Hold")
  last_sink <- "hold"
  winner <- "undecided"
  turn <- 1
  turn_official <- 1 # this is so the real "turn" is counted because the other one controls whose turn it is 
  memory <- list(list(), list())
  ship_num <- c(length(player1_fleet$ships), length(player2_fleet$ships))
  
  while(winner == "undecided") {
    
    if(verbose == TRUE) {
      cat(paste0("Turn ", turn_official, ":\n"))
    }
    
    if(turn %% 2 == 1) {
      current_player <- player_1
    } else {
      current_player <- player_2
    }
    
    if(turn %% 2 == 1) {
      opponent <- player_2
    } else {
      opponent <- player_1
    }
    
    if(turn %% 2 == 1) {
      current_fleet <- player1_fleet
    } else {
      current_fleet <- player2_fleet
    }
    
    if(turn %% 2 == 1) {
      opponent_fleet <- player2_fleet
    } else {
      opponent_fleet <- player1_fleet
    }
    
    if(current_player == player_1){
      
      if(plot_before_turn == "player 1" || plot_before_turn == "both") {
        plot.battleship(game, which = "player 1")
      }
      
    } else {
      
      if(plot_before_turn == "player 2" || plot_before_turn == "both") {
        plot.battleship(game, which = "player 2")
      }
      
    }
    if (verbose == TRUE) {
      cat(paste0(current_player, " is playing... \n"))
    }

    
    
    
    player_index <- ifelse(turn %% 2 == 1, 1, 2) 
    opponent_index <- ifelse(turn %% 2 == 1, 2, 1)
    
    target_history <- game$history %>%
      filter(from == current_player) %>%
      select(target) %>%
      pull(target)
    
    current_player_history <- game$history %>%
      filter(from == current_player)
    
    opp_shipsize <- c()
    
    for(i in seq_along(opponent_fleet$ships)) {
      opp_shipsize <- c(opp_shipsize, opponent_fleet$ships[[i]]$size)
    }
    
    # maybe there's a way to input only the current players history into the ai so in the actual game itself
    
    if (ai == TRUE || current_player == "ai_305721672") {
      ai_result <- self_ai_305721672(battleship = game, history = current_player_history, ocean = opponent_fleet$ocean, size = opp_shipsize, strength = strengths[player_index], memory = memory[[player_index]])
      target <- ai_result$target
      memory[[player_index]] <- ai_result$memory
      
      
    } else {
      target <- human()
      target_valid <- valid_position_input(target, opponent_fleet$ocean) 
      target <- toupper(target)
      
      # we know the form is fine
      while(target_valid != TRUE) {
        target <- readline(prompt = paste0("Invalid input, please enter a value in ", opponent, "'s board dimensions in the correct format (e.g. A-1)"))
        target_valid <- valid_position_input(target, opponent_fleet$ocean) 
        target <- toupper(target)
      }
      
      # we check if we've said it before
      while(target %in% target_history) {
        target <- readline(prompt = ("Invalid input, this position has already been marked. Try again."))
        target_valid <- valid_position_input(target, opponent_fleet$ocean)
        while(target_valid != TRUE) {
          
          target <- readline(prompt = paste0("Invalid input, please enter a value in ", opponent, "'s board dimensions in the correct format (e.g. A-1)"))
          target_valid <- valid_position_input(target, opponent_fleet$ocean) 
        }
        target <- toupper(target)
      }
    }
    
    if(verbose == TRUE) {
      cat(paste0(current_player, " chooses: ", target, "\n"))
    }
    
    
    hit <- FALSE
    sink_count <- c(0, 0)
    
    for (i in seq_along(opponent_fleet$ships)) {
      
      
      ship_position <- opponent_fleet$ships[[i]]$position
      
      start <- strsplit(ship_position[[1]], "-")[[1]]
      end <- strsplit(ship_position[[2]], "-")[[1]]
      
      start_row <- match(start[1], LETTERS)
      start_col <- start[2]
      end_row <- match(end[1], LETTERS)
      end_col <- end[2]
      
      positions <- if (start_row == end_row) {  # Check if the ship is placed horizontally
        paste0(start[1], "-", as.numeric(start[2]):as.numeric(end[2]))  # Create position strings for horizontal placement
      } else {  # If the ship is placed vertically
        paste0(LETTERS[start_row:end_row], "-", start_col)  # Create position strings for vertical placement
      }
      
      if(target %in% positions) {
        
        hit <- TRUE
        index <- which(target == positions)
        opponent_fleet$ships[[i]]$hits[[index]] <- TRUE
        opponent_fleet$ships[[i]] <- opponent_fleet$ships[[i]]
        
        game$history <- add_row(game$history, 
                                from = current_player, 
                                to = opponent, 
                                target = target, 
                                hit = TRUE)
        
        # this is when we sunk a ship
        if(all(opponent_fleet$ships[[i]]$hits) == TRUE) {

          opponent_fleet$ships[[i]]$sunk <- TRUE
          sink_order <- c(sink_order, opponent_fleet$ships[[i]]$name)
          
        }
        break
      } 
      
    }
    
    for (j in seq_along(opponent_fleet$ships)) {
      if(opponent_fleet$ships[[j]]$sunk == TRUE) {
        sink_count[opponent_index] <- sink_count[opponent_index] + 1
        
      }
    }
    
    if(hit == TRUE) {
      
      if(verbose == TRUE) {
        cat("Hit!\n")
        
        if(sink_count[opponent_index] != ship_num[opponent_index]) {
          cat(paste0(current_player, "'s turn again! \n"))
        }
      }
      
      turn <- turn + 1
    }
    
    if(hit == FALSE) {
      
      if(verbose == TRUE) {
        cat("Miss! \n")
      }
      
      game$history <- add_row(game$history, 
                              from = current_player, 
                              to = opponent, 
                              target = target, 
                              hit = FALSE)
    }
    
    if(sink_count[opponent_index] == ship_num[opponent_index]) {
      winner <- "decided"
      
      if(verbose == TRUE) {
        cat(paste0(current_player, " has won! \n All of ", opponent, "'s ships have been sunk\n"))
        last_sink <- tail(sink_order, n = 1)
        
      }
      

    }
  
    
    if(current_player == player_1) {
      player2_fleet$ships <- opponent_fleet$ships
      
      
    } else {
      player1_fleet$ships <- opponent_fleet$ships
    }
    
    game$fleets <- list(player1_fleet, player2_fleet)
    turn <- turn + 1
    turn_official <- turn_official + 1
    
    win <- current_player
    
    if(last_sink == "Patrol Boat") {
      lose_pb <- TRUE
    }
    
    win_turn_count <- game$history %>%
      filter(from == current_player) %>%
      nrow()
    
    
    lose_hits <- game$history %>%
      filter(from == opponent & hit == TRUE) %>%
      nrow()
    
    win_unsunk <- 0
    
    for (j in seq_along(current_fleet$ships)) {
      if(current_fleet$ships[[j]]$sunk == FALSE) {
        win_unsunk <- win_unsunk + 1
        
      }
    }
    
    win_pb <- FALSE
    
    for (ship in current_fleet$ships) {
      if(ship$name == "Patrol Boat" & ship$sunk == TRUE) {
        win_pb <- TRUE
      }
    }
    
    result <- list(win = win, turns = win_turn_count, win_unsunk = win_unsunk, lose_hits = lose_hits, win_pb = win_pb, lose_pb = lose_pb)
    
    
  }
  
  
  return(result)

}

# this one is for not ai v ai 
play_bs2 <- function(players = c("human", "ai_305721672"), strengths = c(9, 9), verbose, plot_before_turn = "none", oceans = list(c(10,10), c(10,10)), ships = list(default_ships(), default_ships())) {

  if (length(players) != 2) {
    if (length(players) == 1) {
      # If only one player is provided, add a default second player
      players <- c(players, "ai_305721672")
      strengths <- c(strengths, 9)
    } else {
      stop("players must be a character vector of length 1 or 2.")
    }
  }
  
  game <- battleship()
  
  player_1 <- players[1]
  player_2 <- players[2]
  
  player1_fleet <- game$fleets[[1]]
  player2_fleet <- game$fleets[[2]]
  
  player1_fleet$admiral <- player_1
  player2_fleet$admiral <- player_2
  
  player1_fleet$ocean <- oceans[[1]]
  player2_fleet$ocean <- oceans[[2]]
  
  player1_fleet$ships <- ships[[1]]
  player2_fleet$ships <- ships[[2]]
  
  
  player1_fleet <- position_fleet(player1_fleet)
  player2_fleet <- position_fleet(player2_fleet)
  
  
  game$fleets <- list(player1_fleet, player2_fleet)
  
  cat("Players have setup their boards. Game Start! \n")
  
  winner <- "undecided"
  turn <- 1
  turn_official <- 1 # this is so the real "turn" is counted because the other one controls whose turn it is 
  memory <- list(list(), list())
  ship_num <- c(length(player1_fleet$ships), length(player2_fleet$ships))
  
  while(winner == "undecided") {
    
    
    if(verbose == TRUE) {
      cat(paste0("Turn ", turn_official, ":\n"))
    }
    
    if(turn %% 2 == 1) {
      current_player <- player_1
    } else {
      current_player <- player_2
    }
    
    if(turn %% 2 == 1) {
      opponent <- player_2
    } else {
      opponent <- player_1
    }
    
    if(turn %% 2 == 1) {
      current_fleet <- player1_fleet
    } else {
      current_fleet <- player2_fleet
    }
    
    if(turn %% 2 == 1) {
      opponent_fleet <- player2_fleet
    } else {
      opponent_fleet <- player1_fleet
    }
    
    if(current_player == player_1){
      
      if(plot_before_turn == "player 1" || plot_before_turn == "both") {
        plot.battleship(game, which = "player 1")
      }
      
    } else {
      
      if(plot_before_turn == "player 2" || plot_before_turn == "both") {
        plot.battleship(game, which = "player 2")
      }
      
    }

    cat(paste0(current_player, " is playing... \n"))

    player_index <- ifelse(turn %% 2 == 1, 1, 2) 
    opponent_index <- ifelse(turn %% 2 == 1, 2, 1)
  
    target_history <- game$history %>%
      filter(from == current_player) %>%
      select(target) %>%
      pull(target)
    
    opp_shipsize <- c()
    
    for(i in seq_along(opponent_fleet$ships)) {
      opp_shipsize <- c(opp_shipsize, opponent_fleet$ships[[i]]$size)
    }
    
    if (current_player == "ai_305721672") {
      ai_result <- ai_305721672(battleship = game, history = game$history, ocean = current_fleet$ocean, size = opp_shipsize, strength = strengths[player_index], memory = memory[[player_index]])
      target <- ai_result$target
      memory[[player_index]] <- ai_result$memory
      
      
    } else {
      target <- human()
      target_valid <- valid_position_input(target, opponent_fleet$ocean) 
      target <- toupper(target)
      
      # we know the form is fine
      while(target_valid != TRUE) {
        target <- readline(prompt = paste0("Invalid input, please enter a value in ", opponent, "'s board dimensions in the correct format (e.g. A-1)"))
        target_valid <- valid_position_input(target, opponent_fleet$ocean) 
        target <- toupper(target)
      }
      
      # we check if we've said it before
      while(target %in% target_history) {
        target <- readline(prompt = ("Invalid input, this position has already been marked. Try again."))
        target_valid <- valid_position_input(target, opponent_fleet$ocean)
        while(target_valid != TRUE) {
          
          target <- readline(prompt = paste0("Invalid input, please enter a value in ", opponent, "'s board dimensions in the correct format (e.g. A-1)"))
          target_valid <- valid_position_input(target, opponent_fleet$ocean) 
        }
        target <- toupper(target)
      }
    }
    
    if(verbose == TRUE) {
      cat(paste0(current_player, " chooses: ", target, "\n"))
    }
    
    
    hit <- FALSE
    sink_count <- c(0, 0)
    
    for (i in seq_along(opponent_fleet$ships)) {
      
      
      ship_position <- opponent_fleet$ships[[i]]$position
      
      start <- strsplit(ship_position[[1]], "-")[[1]]
      end <- strsplit(ship_position[[2]], "-")[[1]]
      
      start_row <- match(start[1], LETTERS)
      start_col <- start[2]
      end_row <- match(end[1], LETTERS)
      end_col <- end[2]
      
      positions <- if (start_row == end_row) {  # Check if the ship is placed horizontally
        paste0(start[1], "-", as.numeric(start[2]):as.numeric(end[2]))  # Create position strings for horizontal placement
      } else {  # If the ship is placed vertically
        paste0(LETTERS[start_row:end_row], "-", start_col)  # Create position strings for vertical placement
      }
      
      if(target %in% positions) {
        
        hit <- TRUE
        index <- which(target == positions)
        opponent_fleet$ships[[i]]$hits[[index]] <- TRUE
        opponent_fleet$ships[[i]] <- opponent_fleet$ships[[i]]
        
        game$history <- add_row(game$history, 
                                from = current_player, 
                                to = opponent, 
                                target = target, 
                                hit = TRUE)
        
        
        if(all(opponent_fleet$ships[[i]]$hits) == TRUE) {
          opponent_fleet$ships[[i]]$sunk <- TRUE

        }
        break
      }
      
    }
    
    for (j in seq_along(opponent_fleet$ships)) {
      if(opponent_fleet$ships[[j]]$sunk == TRUE) {
        sink_count[opponent_index] <- sink_count[opponent_index] + 1
        
      }
    }
    
    
    if(hit == TRUE) {
      
      if(verbose == TRUE) {
        cat("Hit!\n")
        
        if(sink_count[opponent_index] != ship_num[opponent_index]) {
          cat(paste0(current_player, "'s turn again! \n"))
        }
      }
      
      turn <- turn + 1
    }
    
    if(hit == FALSE) {
      
      if(verbose == TRUE) {
        cat("Miss! \n")
      }
      
      game$history <- add_row(game$history, 
                              from = current_player, 
                              to = opponent, 
                              target = target, 
                              hit = FALSE)
    }
    
    if(sink_count[opponent_index] == ship_num[opponent_index]) {
      winner <- "decided"
      
      cat(paste0(current_player, " has won! \n All of ", opponent, "'s ships have been sunk"))

    }
    
    if(current_player == player_1) {
      player2_fleet$ships <- opponent_fleet$ships
    } else {
      player1_fleet$ships <- opponent_fleet$ships
    }
    
    game$fleets <- list(player1_fleet, player2_fleet)
    turn <- turn + 1
    turn_official <- turn_official + 1
  }
}

# this one requires user input to initialize the board basically a functioning game of battleship between two people
play_bs1 <- function(players = c("human", "ai_305721672"), strengths = c(9, 9), verbose, plot_before_turn = "none") {
  
  if (length(players) != 2) {
    if (length(players) == 1) {
      # If only one player is provided, add a default second player
      players <- c(players, "ai_305721672")
      strengths <- c(strengths, 9)
    } else {
      stop("players must be a character vector of length 1 or 2.")
    }
  }
  
  game <- battleship()
  
  player_1 <- players[1]
  player_2 <- players[2]
  
  cat(paste0("Setting up ", player_1, "'s board... \n"))
  
  # Initialization for Player 1
  #--------------------------------------------------
  
  results1 <- initialize_game(player_1)
  gameboard1 <- results1$gameboard
  player1_fleet <- results1$fleet
  
  cat(paste0("Now, setting up ", player_2, "'s board...\n "))
  
  # Initialization for Player 2
  #--------------------------------------------------
  
  results2 <- initialize_game(player_2)
  gameboard2 <- results2$gameboard
  player2_fleet <- results2$fleet
  
  #--------------------------------------------------
  
  game$fleets <- list(player1_fleet, player2_fleet)
  
  cat("Players have setup their boards. Game Start! \n")
  
  winner <- "undecided"
  turn <- 1
  turn_official <- 1 # this is so the real "turn" is counted because the other one controls whose turn it is 
  memory <- list(list(), list())
  ship_num <- c(length(player1_fleet$ships), length(player2_fleet$ships))
  
  while(winner == "undecided") {
    
    
    if(verbose == TRUE) {
      cat(paste0("Turn ", turn_official, ":\n"))
    }
    
    if(turn %% 2 == 1) {
      current_player <- player_1
    } else {
      current_player <- player_2
    }
    
    if(turn %% 2 == 1) {
      opponent <- player_2
    } else {
      opponent <- player_1
    }
    
    if(turn %% 2 == 1) {
      current_fleet <- player1_fleet
    } else {
      current_fleet <- player2_fleet
    }
    
    if(turn %% 2 == 1) {
      opponent_fleet <- player2_fleet
    } else {
      opponent_fleet <- player1_fleet
    }
    
    if(current_player == player_1){
      
      if(plot_before_turn == "player 1" || plot_before_turn == "both") {
        plot.battleship(game, which = "player 1")
      }
      
    } else {
      
      if(plot_before_turn == "player 2" || plot_before_turn == "both") {
        plot.battleship(game, which = "player 2")
      }
      
    }
    
    cat(paste0(current_player, " is playing... \n"))
    
    player_index <- ifelse(turn %% 2 == 1, 1, 2) 
    opponent_index <- ifelse(turn %% 2 == 1, 2, 1)
    
    target_history <- game$history %>%
      filter(from == current_player) %>%
      select(target) %>%
      pull(target)
    
    opp_shipsize <- c()
    
    for(i in seq_along(opponent_fleet$ships)) {
      opp_shipsize <- c(opp_shipsize, opponent_fleet$ships[[i]]$size)
    }
    
    if (current_player == "ai_305721672") {
      ai_result <- ai_305721672(battleship = game, history = game$history, ocean = current_fleet$ocean, size = opp_shipsize, strength = strengths[player_index], memory = memory[[player_index]])
      target <- ai_result$target
      memory[[player_index]] <- ai_result$memory
      
      
    } else {
      target <- human()
      target_valid <- valid_position_input(target, opponent_fleet$ocean) 
      target <- toupper(target)
      
      # we know the form is fine
      while(target_valid != TRUE) {
        target <- readline(prompt = paste0("Invalid input, please enter a value in ", opponent, "'s board dimensions in the correct format (e.g. A-1)"))
        target_valid <- valid_position_input(target, opponent_fleet$ocean) 
        target <- toupper(target)
      }
      
      # we check if we've said it before
      while(target %in% target_history) {
        target <- readline(prompt = ("Invalid input, this position has already been marked. Try again."))
        target_valid <- valid_position_input(target, opponent_fleet$ocean)
        while(target_valid != TRUE) {
          
          target <- readline(prompt = paste0("Invalid input, please enter a value in ", opponent, "'s board dimensions in the correct format (e.g. A-1)"))
          target_valid <- valid_position_input(target, opponent_fleet$ocean) 
        }
        target <- toupper(target)
      }
    }
    
    if(verbose == TRUE) {
      cat(paste0(current_player, " chooses: ", target, "\n"))
    }
    
    
    hit <- FALSE
    sink_count <- c(0, 0)
    
    for (i in seq_along(opponent_fleet$ships)) {
      
      
      ship_position <- opponent_fleet$ships[[i]]$position
      
      start <- strsplit(ship_position[[1]], "-")[[1]]
      end <- strsplit(ship_position[[2]], "-")[[1]]
      
      start_row <- match(start[1], LETTERS)
      start_col <- start[2]
      end_row <- match(end[1], LETTERS)
      end_col <- end[2]
      
      positions <- if (start_row == end_row) {  # Check if the ship is placed horizontally
        paste0(start[1], "-", as.numeric(start[2]):as.numeric(end[2]))  # Create position strings for horizontal placement
      } else {  # If the ship is placed vertically
        paste0(LETTERS[start_row:end_row], "-", start_col)  # Create position strings for vertical placement
      }
      
      if(target %in% positions) {
        
        hit <- TRUE
        index <- which(target == positions)
        opponent_fleet$ships[[i]]$hits[[index]] <- TRUE
        opponent_fleet$ships[[i]] <- opponent_fleet$ships[[i]]
        
        game$history <- add_row(game$history, 
                                from = current_player, 
                                to = opponent, 
                                target = target, 
                                hit = TRUE)
        
        
        if(all(opponent_fleet$ships[[i]]$hits) == TRUE) {
          opponent_fleet$ships[[i]]$sunk <- TRUE
          
        }
        break
      }
      
    }
    
    for (j in seq_along(opponent_fleet$ships)) {
      if(opponent_fleet$ships[[j]]$sunk == TRUE) {
        sink_count[opponent_index] <- sink_count[opponent_index] + 1
        
      }
    }
    
    
    if(hit == TRUE) {
      
      if(verbose == TRUE) {
        cat("Hit!\n")
        
        if(sink_count[opponent_index] != ship_num[opponent_index]) {
          cat(paste0(current_player, "'s turn again! \n"))
        }
      }
      
      turn <- turn + 1
    }
    
    if(hit == FALSE) {
      
      if(verbose == TRUE) {
        cat("Miss! \n")
      }
      
      game$history <- add_row(game$history, 
                              from = current_player, 
                              to = opponent, 
                              target = target, 
                              hit = FALSE)
    }
    
    if(sink_count[opponent_index] == ship_num[opponent_index]) {
      winner <- "decided"
      cat(paste0(current_player, " has won! \n All of ", opponent, "'s ships have been sunk"))
      
    }
    
    if(current_player == player_1) {
      player2_fleet$ships <- opponent_fleet$ships
    } else {
      player1_fleet$ships <- opponent_fleet$ships
    }
    
    game$fleets <- list(player1_fleet, player2_fleet)
    turn <- turn + 1
    turn_official <- turn_official + 1
  }
}


human <- function() {
  repeat {
    target <- readline(prompt = "Enter target (e.g., A-3): ")

    # Normalize the input by converting it to uppercase
    target <- toupper(target)

    # Validate the input format using regular expression
    if (!grepl("^[A-Z]-([1-9]|1[0-9]|2[0-5])$", target)) {
      cat("Invalid input. Please enter a valid target (e.g., A-3).\n")
      next
    }

    # Extract row and column
    parts <- strsplit(target, "-")[[1]]
    row <- parts[1]
    col <- as.integer(parts[2])

    # Ensure the input is within valid range
    if (!row %in% LETTERS[1:26] || is.na(col) || col < 1) {
      cat("Invalid input. Please enter a valid target (e.g., A-3).\n")
      next
    }

    return(paste(row, col, sep = "-"))
  }
}


# i duplicated this function so i could modify it to go against itself to answer the questions 
self_ai_305721672 <- function(battleship, history, ocean, size, strength = 9, memory = list()) {
  
  make_probability_grid <- function(ocean_dim, size, misses) {
    # matrix of zeros
    base <- matrix(0, nrow = ocean_dim[1], ncol = ocean_dim[2], 
                   dimnames = list(LETTERS[1:ocean_dim[1]], 1:ocean_dim[2]))
    
    # add off-limit spots (misses)
    if (length(misses) != 0) {
      for (miss in misses) {
        parts <- strsplit(miss, "-")[[1]]
        row <- match(parts[1], LETTERS)
        col <- as.numeric(parts[2])
        base[row, col] <- NA
      }
    }
    
    if (length(hits) != 0) {
      for (hit in hits) {
        parts <- strsplit(hit, "-")[[1]]
        row <- match(parts[1], LETTERS)
        col <- as.numeric(parts[2])
        base[row, col] <- NA
      }
    }
    
    # add probability density for a given ship length
    add_probability_density <- function(base, ship_length) {
      for (row in 1:nrow(base)) {
        for (col in 1:(ncol(base) - ship_length + 1)) {
          if (all(is.na(base[row, col:(col + ship_length - 1)]) == FALSE)) {
            base[row, col:(col + ship_length - 1)] <- base[row, col:(col + ship_length - 1)] + 1
          }
        }
      }
      
      for (col in 1:ncol(base)) {
        for (row in 1:(nrow(base) - ship_length + 1)) {
          if (all(is.na(base[row:(row + ship_length - 1), col]) == FALSE)) {
            base[row:(row + ship_length - 1), col] <- base[row:(row + ship_length - 1), col] + 1
          }
        }
      }
      
      return(base)
    }
    
    # Add probability density for each ship
    for (ship in size) {
      base <- add_probability_density(base, ship)
    }
    
    return(base)
  }
  
  # Function to update the probability grid based on hits and misses 
  
  update_probability_grid <- function(prob_grid, hits, ocean_dim) {
    hit_increment <- 3
    
    for (hit in hits) {
      parts <- strsplit(hit, "-")[[1]]
      row <- match(parts[1], LETTERS)
      col <- as.numeric(parts[2])
      
      if (row > 1) prob_grid[row - 1, col] <- prob_grid[row - 1, col] + hit_increment
      if (row < ocean_dim[1]) prob_grid[row + 1, col] <- prob_grid[row + 1, col] + hit_increment
      if (col > 1) prob_grid[row, col - 1] <- prob_grid[row, col - 1] + hit_increment
      if (col < ocean_dim[2]) prob_grid[row, col + 1] <- prob_grid[row, col + 1] + hit_increment
    }
    return(prob_grid)
  }
  
  # Choose the target with the highest probability
  choose_target <- function(prob_grid, fired_shots) {
    
    rownames(prob_grid) <- LETTERS[1:nrow(prob_grid)]
    colnames(prob_grid) <- 1:ncol(prob_grid)
    
    # Create all possible targets
    rows <- 1:nrow(prob_grid)
    cols <- 1:ncol(prob_grid)
    possible_targets <- expand.grid(rows,cols)
    possible_targets <- apply(possible_targets, 1, function(x) paste(LETTERS[x[1]], x[2], sep = "-"))
    
    possible_targets <- setdiff(possible_targets, fired_shots)
    
    available_indices <- which(prob_grid == max(prob_grid, na.rm = TRUE), arr.ind = TRUE)
    
    target_choices <- c()
    
    for (i in 1:nrow(available_indices)) {
      row_name <- rownames(prob_grid)[available_indices[i, 1]]
      col_value <- available_indices[i, 2]
      target <- paste(row_name, col_value, sep = "-")
      target_choices <- c(target_choices, target)
    }
    
    final_targets <- c()
    
    for(choice in target_choices) {
      if (choice %in% possible_targets) {
        final_targets <- c(choice,final_targets)
      }
    }
    
    random_number <- sample(1:length(final_targets), 1)
    
    target <- final_targets[random_number]
    
    return(target)
  }
  
  if (strength == 8) {
    
    # Initialize memory
    if (is.null(memory$previous_shots)) {
      memory$previous_shots <- c()
      memory$turn_count <- 1
    }
    
    if (memory$turn_count < 3 || memory$turn_count %% 8 == 0) {
      
      rows <- 1:ocean[1]
      cols <- 1:ocean[2] 
      possible_targets <- expand.grid(rows, cols)
      possible_targets <- apply(possible_targets, 1, function(x) paste(LETTERS[x[1]], x[2], sep = "-"))
      possible_targets <- setdiff(possible_targets, memory$attempts)
      
      target <- sample(possible_targets, 1)
      memory$previous_shots <- c(memory$previos_shots, target)
      
      list(target = target, memory = memory)
      
    } else {
      
      hits <- history %>%
        # i removed from == "ai_305721672" & 
        filter(hit == TRUE ) %>%
        select(target) %>%
        pull(target)
      
      misses <- history %>%
        filter(hit == FALSE) %>%
        select(target) %>%
        pull(target)
      
      prob_grid <- make_probability_grid(ocean_dim = ocean, size = size, misses = misses)
      
      # Update the probability grid
      prob_grid <- update_probability_grid(prob_grid, hits, ocean)
      
      # Choose the next target
      target <- choose_target(prob_grid, memory$previous_shots)
      
      # Update fired shots
      memory$previous_shots <- c(memory$previous_shots, target)
      
    }

    
  } else if (strength == 9){
    
    # Initialize memory
    if (is.null(memory$previous_shots)) {
      memory$previous_shots <- c()
    }
    
    hits <- history %>%
      # i removed from == "ai_305721672" & 
      filter(hit == TRUE ) %>%
      select(target) %>%
      pull(target)
    
    misses <- history %>%
      filter(hit == FALSE) %>%
      select(target) %>%
      pull(target)
    
    prob_grid <- make_probability_grid(ocean_dim = ocean, size = size, misses = misses)
    
    # Update the probability grid
    prob_grid <- update_probability_grid(prob_grid, hits, ocean)
    
    # Choose the next target
    target <- choose_target(prob_grid, memory$previous_shots)
    
    # Update fired shots
    memory$previous_shots <- c(memory$previous_shots, target)
    
  } else {
    if (length(memory) == 0) {
      memory$attempts <- list()
    }
    
    # Simple strategy: Randomly choose a target not already attempted
    rows <- 1:ocean[1]
    cols <- 1:ocean[2] 
    possible_targets <- expand.grid(rows, cols)
    possible_targets <- apply(possible_targets, 1, function(x) paste(LETTERS[x[1]], x[2], sep = "-"))
    possible_targets <- setdiff(possible_targets, memory$attempts)
    target <- sample(possible_targets, 1)
    memory$attempts <- c(memory$attempts, target)
    
    list(target = target, memory = memory)
  }
  
  memory$turn_count <- memory$turn_count + 1
  return(list(target = target, memory = memory))
}

# for the tournament
ai_305721672 <- function(battleship, history, ocean, size, strength = 9, memory = list()) {

  make_probability_grid <- function(ocean_dim, size, misses) {
    # matrix of zeros
    base <- matrix(0, nrow = ocean_dim[1], ncol = ocean_dim[2], 
                   dimnames = list(LETTERS[1:ocean_dim[1]], 1:ocean_dim[2]))
    
    # add off-limit spots (misses and hits)
    if (length(misses) != 0) {
      for (miss in misses) {
        parts <- strsplit(miss, "-")[[1]]
        row <- match(parts[1], LETTERS)
        col <- as.numeric(parts[2])
        base[row, col] <- NA
      }
    }
    
    if (length(hits) != 0) {
      for (hit in hits) {
        parts <- strsplit(hit, "-")[[1]]
        row <- match(parts[1], LETTERS)
        col <- as.numeric(parts[2])
        base[row, col] <- NA
      }
    }
    
    # add probability density for a given ship length
    add_probability_density <- function(base, ship_length) {
      for (row in 1:nrow(base)) {
        for (col in 1:(ncol(base) - ship_length + 1)) {
          if (all(is.na(base[row, col:(col + ship_length - 1)]) == FALSE)) {
            base[row, col:(col + ship_length - 1)] <- base[row, col:(col + ship_length - 1)] + 1
          }
        }
      }
      
      for (col in 1:ncol(base)) {
        for (row in 1:(nrow(base) - ship_length + 1)) {
          if (all(is.na(base[row:(row + ship_length - 1), col]) == FALSE)) {
            base[row:(row + ship_length - 1), col] <- base[row:(row + ship_length - 1), col] + 1
          }
        }
      }
      
      return(base)
    }
    
    # add probability density for each ship
    for (ship in size) {
      base <- add_probability_density(base, ship)
    }
    
    return(base)
  }
  
  # update the probability grid based on hits and misses 
  update_probability_grid <- function(prob_grid, hits, ocean_dim) {
    hit_increment <- 3 # how much to add to the score of nearby squares when theres a hit 
    
    for (hit in hits) {
      parts <- strsplit(hit, "-")[[1]]
      row <- match(parts[1], LETTERS)
      col <- as.numeric(parts[2])
      
      if (row > 1) prob_grid[row - 1, col] <- prob_grid[row - 1, col] + hit_increment
      if (row < ocean_dim[1]) prob_grid[row + 1, col] <- prob_grid[row + 1, col] + hit_increment
      if (col > 1) prob_grid[row, col - 1] <- prob_grid[row, col - 1] + hit_increment
      if (col < ocean_dim[2]) prob_grid[row, col + 1] <- prob_grid[row, col + 1] + hit_increment
    }
    return(prob_grid)
  }
  
  # choose the target with the highest probability
  choose_target <- function(prob_grid, fired_shots) {
    
    rownames(prob_grid) <- LETTERS[1:nrow(prob_grid)]
    colnames(prob_grid) <- 1:ncol(prob_grid)
    
    # create all possible targets
    rows <- 1:nrow(prob_grid)
    cols <- 1:ncol(prob_grid)
    possible_targets <- expand.grid(rows,cols)
    possible_targets <- apply(possible_targets, 1, function(x) paste(LETTERS[x[1]], x[2], sep = "-"))
    
    possible_targets <- setdiff(possible_targets, fired_shots)
    
    available_indices <- which(prob_grid == max(prob_grid, na.rm = TRUE), arr.ind = TRUE)
    
    target_choices <- c()
    
    for (i in 1:nrow(available_indices)) {
      row_name <- rownames(prob_grid)[available_indices[i, 1]]
      col_value <- available_indices[i, 2]
      target <- paste(row_name, col_value, sep = "-")
      target_choices <- c(target_choices, target)
    }
    
    final_targets <- c()
    
    for(choice in target_choices) {
      if (choice %in% possible_targets) {
        final_targets <- c(choice,final_targets)
      }
    }
    
    random_number <- sample(1:length(final_targets), 1)
    
    target <- final_targets[random_number]
    
    return(target)
  }
  
  if (strength == 9) {
    
    # Initialize memory
    if (is.null(memory$previous_shots)) {
      memory$previous_shots <- c()
    }
    
    hits <- history %>%
      filter(from == "ai_305721672" & hit == TRUE ) %>%
      select(target) %>%
      pull(target)

    misses <- history %>%
      filter(from == "ai_305721672" & hit == FALSE) %>%
      select(target) %>%
      pull(target)
    
    
    prob_grid <- make_probability_grid(ocean_dim = ocean, size = size, misses = misses)

    # update the probability grid
    prob_grid <- update_probability_grid(prob_grid, hits, ocean)

    # choose the next target
    target <- choose_target(prob_grid, memory$previous_shots)

    # update fired shots
    memory$previous_shots <- c(memory$previous_shots, target)

  } else {
    if (length(memory) == 0) {
      memory$attempts <- list()
    }
    
    # randomly choose a target not already attempted
    rows <- 1:ocean[1]
    cols <- 1:ocean[2] 
    possible_targets <- expand.grid(rows, cols)
    possible_targets <- apply(possible_targets, 1, function(x) paste(LETTERS[x[1]], x[2], sep = "-"))
    possible_targets <- setdiff(possible_targets, memory$attempts)
    
    target <- sample(possible_targets, 1)
    memory$attempts <- c(memory$attempts, target)

    list(target = target, memory = memory)
  }
  
  return(list(target = target, memory = memory))
}

attributes(ai_305721672) = list(alt = "cxy")

default_ships <- function() {
  list(
    ship("Aircraft Carrier", 5),
    ship("Battleship", 4),
    ship("Destroyer", 3),
    ship("Submarine", 3),
    ship("Patrol Boat", 2)
  )
}

position_fleet <- function(fleet, positions = NULL) {
  occupied_positions <- c()
  
  if (is.null(positions)) {
    positions <- list()
    
    for (ship in fleet$ships) {
      repeat {
        pos <- generate_random_position(ship$size, fleet$ocean, occupied_positions)
        new_positions <- if (pos[1] == pos[3]) { # Check if the ship is placed horizontally
          paste0(LETTERS[pos[1]], "-", pos[2]:pos[4])
        } else { # If the ship is placed vertically
          paste0(LETTERS[pos[1]:pos[3]], "-", pos[2])
        }
        
        if (!any(new_positions %in% occupied_positions)) {
          occupied_positions <- c(occupied_positions, new_positions) # Update occupied positions with the new ship positions
          positions <- append(positions, list(c(paste(LETTERS[pos[1]], pos[2], sep = "-"), paste(LETTERS[pos[3]], pos[4], sep = "-")))) # Assign the generated position to the ship
          break
        }
      }
    }
  }
  
  for (i in seq_along(fleet$ships)) {
    fleet$ships[[i]]$position <- positions[[i]]
  }
  return(fleet)
}

# Create Methods
# --------------------------------------------------------------------------------------

print.ship <- function(x) {
  cat("Ship Information: \n")
  cat("------------------------- \n")
  cat("Name:     ", x$name, "\n")
  cat("Size:     ", x$size, "\n")
  cat("Position: ", paste(x$position, collapse = " to "), "\n")
  cat("Hits:     ", paste(ifelse(x$hits, "Hit", "Miss"), collapse = ", "), "\n")
  cat("Sunk?:    ", if (x$sunk == TRUE) "Yes" else "No", "\n")
  cat("-------------------------")
}

print.fleet <- function(x) {
  cat("Fleet Details:\n")
  cat("--------------\n")
  cat(" Admiral    :", x$admiral, "\n")
  cat(" Ocean size :", paste(x$ocean, collapse = "x"), "\n")
  cat(" Ships:\n")
  cat("~~~~~~~~~~~\n")
  for (ship in x$ships) {
    cat(paste0(ship$name, ", ", "Sunk? ",if (ship$sunk == TRUE) "Yes" else "No", "\n"))
  }
  cat("--------------\n")
}

print.battleship <- function(x) {
  cat("Battleship Game Details:\n")
  cat("------------------------\n")
  for (fleet in x$fleets) {
    print(fleet)
    cat("\n")
  }
  cat(" History of Shots:\n")
  print(x$history)
  cat("------------------------\n")
}


plot.fleet <- function(x) {

  ocean_grid <- matrix("-", nrow = x$ocean[1], ncol = x$ocean[2],
                       dimnames = list(LETTERS[1:x$ocean[1]], 1:x$ocean[2]))

  for (ship in x$ships) {
    if (length(ship$position) == 2) {
      start_pos <- ship$position[[1]]
      end_pos <- ship$position[[2]]
      
      s_parts <- strsplit(start_pos, "-")[[1]]

      start_row <- match(s_parts[1], LETTERS)
      start_col <- as.numeric(s_parts[2])

      e_parts <- strsplit(end_pos, "-")[[1]]
      end_row <- match(e_parts[1], LETTERS)
      end_col <- as.numeric(e_parts[2])
            
      if (start_row == end_row) {
        for (col in start_col:end_col) {
          ocean_grid[start_row, col] <- ifelse(ship$hits[col - start_col + 1], "X", "S")
        }
      } else if (start_col == end_col) {
        for (row in start_row:end_row) {
          ocean_grid[row, start_col] <- ifelse(ship$hits[row - start_row + 1], "X", "S")
        }
      }
    }
  }

  return(ocean_grid)
}

plot.battleship <- function(x, which = "both") {

  # board of player 1 no ship
  
  ocean_grid1 <- matrix("-", nrow = x$fleets[[1]]$ocean[1], ncol = x$fleets[[1]]$ocean[2],
                       dimnames = list(LETTERS[1:x$fleets[[1]]$ocean[2]], 1:x$fleets[[1]]$ocean[1]))

  # hit vector
  player1_hits <- x$history %>%
    filter(hit == TRUE & to == x$fleets[[1]]$admiral) %>%
    select(target) %>% 
    pull(target)
  
  # miss vector
  player1_miss <- x$history %>%
    filter(hit == FALSE & to == x$fleets[[1]]$admiral) %>%
    select(target) %>% 
    pull(target)
  
  
  for (hit in player1_hits) {
      
      parts <- strsplit(hit, "-")[[1]]
      
      row <- match(parts[1], LETTERS)
      col <- as.numeric(parts[2])
    
      ocean_grid1[row, col] <- "X"

  }
  
  for (miss in player1_miss) {
    
    parts <- strsplit(miss, "-")[[1]]
    
    row <- match(parts[1], LETTERS)
    col <- as.numeric(parts[2])
    
    ocean_grid1[row, col] <- "O"
    
  }
  
  # board of player 2 no ship
  
  ocean_grid2 <- matrix("-", nrow = x$fleets[[2]]$ocean[1], ncol = x$fleets[[2]]$ocean[2],
                        dimnames = list(LETTERS[1:x$fleets[[2]]$ocean[2]], 1:x$fleets[[2]]$ocean[1]))
  
  # hit vector
  player2_hits <- x$history %>%
    filter(hit == TRUE & to == x$fleets[[2]]$admiral) %>%
    select(target) %>% 
    pull(target)
  
  # miss vector
  player2_miss <- x$history %>%
    filter(hit == FALSE & to == x$fleets[[2]]$admiral) %>%
    select(target) %>% 
    pull(target)
  
  
  for (hit in player2_hits) {
    
    parts <- strsplit(hit, "-")[[1]]
    
    row <- match(parts[1], LETTERS)
    col <- as.numeric(parts[2])
    
    ocean_grid2[row, col] <- "X"
    
  }
  
  for (miss in player2_miss) {
    
    parts <- strsplit(miss, "-")[[1]]
    
    row <- match(parts[1], LETTERS)
    col <- as.numeric(parts[2])
    
    ocean_grid2[row, col] <- "O"
    
  }
  
  
  # board of player 1 with ship
  
  p1_grid <- plot.fleet(x$fleets[[1]])
  
  player1_hits <- x$history %>%
    filter(hit == TRUE & to == x$fleets[[1]]$admiral) %>%
    select(target) %>% 
    pull(target)
  
  # miss vector
  player1_miss <- x$history %>%
    filter(hit == FALSE & to == x$fleets[[1]]$admiral) %>%
    select(target) %>% 
    pull(target)
  
  
  for (hit in player1_hits) {
    
    parts <- strsplit(hit, "-")[[1]]
    
    row <- match(parts[1], LETTERS)
    col <- as.numeric(parts[2])
    
    p1_grid[row, col] <- "X"
    
  }
  
  for (miss in player1_miss) {
    
    parts <- strsplit(miss, "-")[[1]]
    
    row <- match(parts[1], LETTERS)
    col <- as.numeric(parts[2])
    
    p1_grid[row, col] <- "O"
    
  }
  
  
  # board of player 2 with ship
  
  p2_grid <- plot.fleet(x$fleets[[2]])
  
  player2_hits <- x$history %>%
    filter(hit == TRUE & to == x$fleets[[2]]$admiral) %>%
    select(target) %>% 
    pull(target)
  

  player2_miss <- x$history %>%
    filter(hit == FALSE & to == x$fleets[[2]]$admiral) %>%
    select(target) %>% 
    pull(target)
  
  
  for (hit in player2_hits) {
    
    parts <- strsplit(hit, "-")[[1]]
    
    row <- match(parts[1], LETTERS)
    col <- as.numeric(parts[2])
    
    p2_grid[row, col] <- "X"
    
  }
  
  for (miss in player2_miss) {
    
    parts <- strsplit(miss, "-")[[1]]
    
    row <- match(parts[1], LETTERS)
    col <- as.numeric(parts[2])
    
    p2_grid[row, col] <- "O"
    
  }
  
  if (which == "player 1") {
    cat("Current Player's Board:\n")
    print(p1_grid)
    
    cat("Opponent's Board:\n")
    print(ocean_grid2)
  }
  if (which == "player 2") {
    cat("Current Player's Board:\n")
    print(p2_grid)
    
    cat("Opponent's Board:\n")
    print(ocean_grid1)
  }
  if (which == "both") {
    cat("Current Player's Board:\n")
    print(p1_grid)
    
    cat("Opponent's Board:\n")
    print(p2_grid)
  }
  
}

summary.fleet <- function(object, ...) {
  cat("Summary of Fleet for Admiral:", object$admiral, "\n")
  cat("Number of Ships:", length(object$ships), "\n")
  cat("Ships:\n")
  for (ship in object$ships) {
    cat(" ", ship$name, "- Size:", ship$size, "- Sunk:", ship$sunk, "\n")
  }
}

summary.battleship <- function(object, ...) {
  cat("Summary of Battleship Game:\n")
  for (fleet in object$fleets) {
    summary(fleet)
    cat("\n")
  }
  cat(" History of Shots:\n")
  print(object$history)
}
