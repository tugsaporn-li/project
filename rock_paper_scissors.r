game <- function() {
  user_score <- 0
  comp_score <- 0
  hands <- c("rock", "paper", "scissors")
  
  while(TRUE) {
    # Get user input
    user_hand <- readline("Choose your hand: ")
    
    # Check for quit condition
    if (user_hand == "quit") {
      if (user_score > comp_score) cat("You win!\n")
      else if (user_score < comp_score) cat("Computer wins!\n")
      else cat("It's a tie!\n")
      
      cat(paste0("--- Final Score ---\n",
                 "Your score : ", user_score, "\n", 
                 "Computer score : ", comp_score, "\n",
                 "Thanks for playing!"))
      break
    } 
    
    # Check for invalid input
    if (!(user_hand %in% hands)) {
      cat("Invalid choice. Please enter 'rock', 'paper', 'scissors', or 'quit'.\n")
      next # Skip the rest of the current iteration and go to the next round
    }
    
    # Computer chooses its hand
    comp_hand <- sample(hands, 1)
    cat(paste0("Computer chose: ", comp_hand, "\n"))
    
    # It's a Tie
    if (comp_hand == user_hand) {
      cat("It's a tie!\n")
    }
    
    # Computer Wins
    else if ((comp_hand == "rock" & user_hand == "scissors") |
             (comp_hand == "scissors" & user_hand == "paper") |
             (comp_hand == "paper" & user_hand == "rock")) {
      comp_score <- comp_score + 1
      cat("Computer wins this round!\n")
    }
    
    # User Wins
    else {
      user_score <- user_score + 1
      cat("You win this round! Congratulations!\n")
    }
    
    # Print current scores after each round to show the game is progressing
    cat(paste0("Current Scores: You = ", user_score, ", Computer = ", comp_score, "\n\n"))
  }
}
