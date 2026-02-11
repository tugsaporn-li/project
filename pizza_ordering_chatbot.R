order_pizza <- function() {
  
  # --- 0. SETTINGS & PARAMETERS ---
  max_pizza_qty <- 32  
  max_drink_qty <- 50 
  
  vat_rate <- 0.07
  delivery_fees <- list(Standard = 1.00, Express = 3.00) 
  
  pizzas <- c(Cheese = 6.00, Sausage = 7.00, Ricotta = 8.00, 'Buffalo chicken' = 11.00)
  drinks <- c(Coke = 3.00, Sprite = 3.00, Fanta = 3.00, Lemonade = 7.00)
  
  order_summary <- list()
  subtotal <- 0.00
  
  # --- 1. GREETING ---
  cat("Welcome to Pizzeria Online! 🍕\n")
  cat("(Type 'quit' at any time to cancel your order)\n")
  user_name <- readline("What is your name?: ")
  if (tolower(user_name) == "quit") return(cat("Order cancelled. Goodbye!\n"))
  cat(paste0("Hello, ", user_name, "! Let's get your order started.\n"))
  
  # --- 2. PIZZA LOOP ---
  ordering_pizza <- TRUE
  while (ordering_pizza) {
    cat("\n----- Pizza Menu -----\n")
    for (i in seq_along(pizzas)) {
      cat(sprintf("%d. %-15s: $%.2f\n", i, names(pizzas)[i], pizzas[i]))
    }
    
    valid_choice <- FALSE
    while (!valid_choice) {
      choice_input <- readline(sprintf("Select pizza (1-%d) or 'quit': ", length(pizzas)))
      if (tolower(choice_input) == "quit") return(cat("Order cancelled. Goodbye!\n"))
      
      choice_index <- suppressWarnings(as.integer(choice_input))
      if (!is.na(choice_index) && choice_index >= 1 && choice_index <= length(pizzas)) {
        valid_choice <- TRUE
        item_name <- names(pizzas)[choice_index]
        item_price <- pizzas[choice_index]
        
        valid_qty <- FALSE
        while (!valid_qty) {
          qty_input <- readline(paste0("How many '", item_name, "' pizza do you want? (Max ", max_pizza_qty, "): "))
          if (tolower(qty_input) == "quit") return(cat("Order cancelled. Goodbye!\n"))
          
          qty <- suppressWarnings(as.integer(qty_input))
          if (!is.na(qty) && qty >= 1) {
            if (qty > max_pizza_qty) {
              cat(sprintf("⚠️ Too many items! For orders larger than %d, please contact our store directly.", max_pizza_qty))
            } else {
              valid_qty <- TRUE
              line_total <- qty * item_price
              subtotal <- subtotal + line_total
              order_summary[[length(order_summary)+1]] <- list(type="Pizza", name=item_name, quantity=qty, price=item_price, total=line_total)
              cat(sprintf("Added %d x %s pizza. Current subtotal: $%.2f\n", qty, item_name, subtotal))
            }
          } else { cat("❌ Invalid quantity. Please try again.\n") }
        }
      } else { cat("❌ Invalid choice. Please try again.\n") }
    }
    
    valid_p_input <- FALSE
    while(!valid_p_input) {
      continue_p <- readline("Order another pizza? (Y/N) or 'quit': ")
      if (tolower(continue_p) == "quit") return(cat("Order cancelled. Goodbye!\n"))
      
      continue_p <- toupper(continue_p)
      if (continue_p == "Y") { valid_p_input <- TRUE }
      else if (continue_p == "N") { ordering_pizza <- FALSE; valid_p_input <- TRUE }
      else { cat("Please enter 'Y', 'N', or 'quit'.\n") }
    }
  }
  
  # --- 3. DRINKS LOOP ---
  ordering_drinks <- TRUE
  while (ordering_drinks) {
    cat("\n----- Drinks Menu -----\n")
    for (i in seq_along(drinks)) { 
      cat(sprintf("%d. %-15s: $%.2f\n", i, names(drinks)[i], drinks[i])) 
    }
    
    valid_d_choice <- FALSE
    while(!valid_d_choice) {
      choice_input <- readline(sprintf("Select drink (1-%d), [Enter] to skip, or 'quit': ", length(drinks)))
      if (tolower(choice_input) == "quit") return(cat("Order cancelled. Goodbye!\n"))
      if (nchar(choice_input) == 0) { ordering_drinks <- FALSE; valid_d_choice <- TRUE; break }
      
      choice_index <- suppressWarnings(as.integer(choice_input))
      if (!is.na(choice_index) && choice_index >= 1 && choice_index <= length(drinks)) {
        valid_d_choice <- TRUE
        item_name <- names(drinks)[choice_index]
        item_price <- drinks[choice_index]
        
        valid_qty <- FALSE
        while (!valid_qty) {
          qty_input <- readline(paste0("How many '", item_name, "'? (Max ", max_drink_qty, "): "))
          if (tolower(qty_input) == "quit") return(cat("Order cancelled. Goodbye!\n"))
          
          qty <- suppressWarnings(as.integer(qty_input))
          if (!is.na(qty) && qty >= 1) {
            if (qty > max_drink_qty) {
              cat(sprintf("⚠️ Too many items! For orders larger than %d, please contact our store directly.\n", max_drink_qty))
            } else {
              valid_qty <- TRUE
              subtotal <- subtotal + (qty * item_price)
              order_summary[[length(order_summary)+1]] <- list(type="Drinks", name=item_name, quantity=qty, price=item_price, total=qty*item_price)
              cat(sprintf("Added %d x %s. Current subtotal: $%.2f\n", qty, item_name, subtotal))
            }
          } else { cat("❌ Invalid quantity. Please try again.\n") }
        }
      } else { cat("❌ Invalid choice. Please try again.\n") }
    }
    
    if (!ordering_drinks) break
    
    valid_d_input <- FALSE
    while(!valid_d_input) {
      continue_d <- readline("Order another drink? (Y/N) or 'quit': ")
      if (tolower(continue_d) == "quit") return(cat("Order cancelled. Goodbye!\n"))
      
      continue_d <- toupper(continue_d)
      if (continue_d == "Y") { valid_d_input <- TRUE }
      else if (continue_d == "N") { ordering_drinks <- FALSE; valid_d_input <- TRUE }
      else { cat("Please enter 'Y', 'N', or 'quit'.\n") }
    }
  }
  
  # --- 4. DELIVERY ---
  delivery_choice <- NULL
  delivery_fee <- 0.00
  while (is.null(delivery_choice)) {
    cat("\nHow would you like your order delivered?\n")
    cat(sprintf("1. Standard ($%.2f)\n2. Express ($%.2f)\n", delivery_fees$Standard, delivery_fees$Express))
    delivery_input <- readline("Choice (1/2) or 'quit': ")
    if (tolower(delivery_input) == "quit") return(cat("Order cancelled. Goodbye!\n"))
    
    if (delivery_input == "1") {
      delivery_choice <- "Standard"; delivery_fee <- delivery_fees$Standard
    } else if (delivery_input == "2") {
      delivery_choice <- "Express"; delivery_fee <- delivery_fees$Express
    } else { cat("❌ Invalid option.\n") }
  }
  
  # --- 5. SUMMARY CALCULATION ---
  vat_amount <- subtotal * vat_rate
  final_total <- subtotal + vat_amount + delivery_fee
  
  cat("\n=================================================\n")
  cat("✨ Order Summary for ", user_name, " ✨\n")
  cat("=================================================\n")
  for (item in order_summary) {
    name_display <- paste(item$name, item$type)
    cat(sprintf(" %2d x %-22s @ $%.2f = $%.2f\n", item$quantity, name_display, item$price, item$total))
  }
  cat("-------------------------------------------------\n")
  cat(sprintf("%-28s: $%.2f\n", "Subtotal", subtotal))
  cat(sprintf("%-28s: $%.2f (%.0f%%)\n", "VAT", vat_amount, vat_rate * 100))
  cat(sprintf("%-28s: $%.2f (%s)\n", "Delivery Fee", delivery_fee, delivery_choice))
  cat("-------------------------------------------------\n")
  cat(sprintf("%-28s: $%.2f\n", "FINAL TOTAL", final_total))
  cat("=================================================\n")
  cat(paste0("🎉 Thank you for your order, ", user_name, "! Your order is being processed.\nGoodbye!\n"))
}
