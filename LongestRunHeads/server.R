# --------------------------
#  App Title: Longest Run
#     Author: Jimmy Doi
# --------------------------

library(shiny)
library(ggplot2)
# source("Coin_Flips_Runs.R")

options(shiny.error=browser)

# Streak bias function to calculate streak probabilities
calculate_streak_probs <- function(coords, max_streak = 5) {
  streak_counts <- numeric(max_streak)
  head_after_streak <- numeric(max_streak)
  
  streak_length <- 0  # Track the current streak length of heads
  
  for (i in 2:nrow(coords)) {
    if (coords[i - 1, 1] == 1) {  # Previous flip was heads
      streak_length <- streak_length + 1
    } else {
      streak_length <- 0  # Reset streak if previous flip wasn't heads
    }
    
    if (streak_length >= 1 && streak_length <= max_streak) {
      streak_counts[streak_length] <- streak_counts[streak_length] + 1
      if (coords[i, 1] == 1) {  # Current flip is heads
        head_after_streak[streak_length] <- head_after_streak[streak_length] + 1
      }
    }
  }
  
  observed_probs <- head_after_streak / streak_counts
  observed_probs[is.nan(observed_probs)] <- 0  # Replace NaNs with 0s
  return(observed_probs)
}

shinyServer(function(input, output) {
  
  phh_history <- reactiveValues(data = numeric())
  
  dataInput <- reactive({
    flip.gen(input$trials*(input$save>-1),input$H.prob)
  })
  
  observeEvent(input$save, {
    phh_values <- numeric()  # Store P(H|H) values
    last_coords <- NULL      # Store coin flip results
    max_streak <- 5          # Maximum streak length to calculate
    
    streak_sums <- numeric(max_streak)  # Sum streak probabilities across iterations
    streak_counts <- numeric(max_streak)  # Track the number of streaks observed
    
    # List to store Figure 1 data for each iteration
    figure1_data <- list()
    
    # Generate a new P(H|H) value for each iteration
    for (i in 1:input$iterations) {
      coords <- flip.gen(input$trials, input$H.prob)
      phh <- round(sum(coords[, 4]) / sum(coords[, 1][1:nrow(coords) - 1]), 4)
      phh_values <- c(phh_values, phh)  # Append each P(H|H) value
      last_coords <- coords             # Store the last run's coin flip results
      
      # Calculate streak probabilities for the current run
      streak_probs <- calculate_streak_probs(coords, max_streak)
      
      # Accumulate the streak probabilities and counts
      streak_sums <- streak_sums + streak_probs
      streak_counts <- streak_counts + as.numeric(!is.na(streak_probs))
    }
    
    # Store the results in reactive values
    phh_history$data <- c(phh_history$data, phh_values)
    phh_history$last_coords <- last_coords  # Store last iteration results for plotting
    phh_history$streak_probs <- streak_sums / streak_counts  # Average streak probabilities
  })
  
  # Fill in the spot we created for a plot
  output$coinPlot <- renderPlot({
    
    plot.flips(input$minrun,dataInput(),input$my.cex,input$checkbox)
    
    
    ####################################################################
    
  })
  
  output$phhPlot <- renderPlot({
    if (length(phh_history$data) > 0 & !input$checkbox2) {
      phh_df <- data.frame(
        trial = seq_along(phh_history$data),
        phh_value = phh_history$data
      )

      avg_phh <- mean(phh_history$data)

      # ggplot for P(H|H) history
      ggplot(phh_df, aes(x = trial, y = phh_value)) +
        geom_line(color = "cornflowerblue", size = 1) +
        geom_point(color = "dodgerblue4", size = 2) +
        geom_hline(yintercept = avg_phh, linetype = "dashed", color = "firebrick", size = 1) +  # Average line
        annotate("text", x = max(phh_df$trial), y = avg_phh, label = paste("Avg:", round(avg_phh, 4)),
                 vjust = -1, hjust = 1.1, color = "firebrick", size = 5) +  # Label for average line
        labs(x = "Trial Run", y = "P(H|H)",
             title = expression("History of P(H|H) across Trials")) +
        theme_bw(base_size = 15)
    } else if (!input$checkbox2) {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No data yet.", size = 6) +
        theme_void()
    } else if (!is.null(phh_history$streak_probs)) {
        streak_df <- data.frame(
          Streak_Length = 1:length(phh_history$streak_probs),
          Observed_Prob = phh_history$streak_probs
        )
        
        expected_prob <- input$H.prob  # Expected probability of heads
        
        ggplot(streak_df, aes(x = Streak_Length, y = Observed_Prob)) +
          geom_bar(stat = "identity", fill = "cornflowerblue", color = "dodgerblue4") +
          geom_hline(yintercept = expected_prob, linetype = "dashed", color = "firebrick", size = 1.2) +
          geom_text(aes(label = round(Observed_Prob, 3)), vjust = -0.5, size = 5) +
          labs(title = "Conditional Probability of Heads after Streaks of k Heads",
               x = "Streak Length (Number of Consecutive Heads)",
               y = "P(H|H^k)") +
          theme_bw() +
          annotate("text", x = max(streak_df$Streak_Length), y = expected_prob, 
                   label = paste("Expected P(H) =", expected_prob), color = "firebrick", vjust = -1)
      } else {
        
      }
  })
  # Dot plot for P(H|H) history resembling stacked dots
  output$phhPlot2 <- renderPlot({
    if (length(phh_history$data) > 0 & !input$checkbox2) {
      # Create a data frame with counts of each unique P(H|H) value
      phh_df <- data.frame(P_H_H = phh_history$data)
      avg_phh <- mean(phh_history$data)
      phh_count <- as.data.frame(table(phh_df$P_H_H))
      colnames(phh_count) <- c("P_H_H", "Count")
      phh_count$P_H_H <- as.numeric(as.character(phh_count$P_H_H))
      
      # Create the stacked dot plot
      ggplot(phh_count, aes(x = P_H_H, y = Count)) +
        geom_col(fill = "cornflowerblue", color = "dodgerblue4") +  # Bars for counts
        geom_vline(xintercept = avg_phh, linetype = "dashed", color = "firebrick", size = 1) +  # Average line
              annotate("text", x = avg_phh, y = max(phh_count$Count)/2, label = paste("Avg:", round(avg_phh, 4)),
                       vjust = -1, hjust = 1.1, color = "firebrick", size = 5) +  # Label for average line
        labs(title = "P(H|H) Values Over Iterations",
             x = "P(H|H)",
             y = "Count") +
        theme_bw() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Expand y-axis for better visibility
    } else if (!input$checkbox2){
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No data yet.", size = 6) +
        theme_void()
    } else {
      
    }
  })
  
  # # Plot for streak probabilities similar to Figure 1
  # output$streakPlot <- renderPlot({
  #   if (!is.null(phh_history$streak_probs) & input$checkbox2) {
  #     streak_df <- data.frame(
  #       Streak_Length = 1:length(phh_history$streak_probs),
  #       Observed_Prob = phh_history$streak_probs
  #     )
  #     
  #     expected_prob <- input$H.prob  # Expected probability of heads
  #     
  #     ggplot(streak_df, aes(x = Streak_Length, y = Observed_Prob)) +
  #       geom_bar(stat = "identity", fill = "cornflowerblue", color = "dodgerblue4") +
  #       geom_hline(yintercept = expected_prob, linetype = "dashed", color = "firebrick", size = 1.2) +
  #       geom_text(aes(label = round(Observed_Prob, 3)), vjust = -0.5, size = 5) +
  #       labs(title = "Conditional Probability of Heads after Streaks of k Heads",
  #            x = "Streak Length (Number of Consecutive Heads)",
  #            y = "P(H|H^k)") +
  #       theme_bw() +
  #       annotate("text", x = max(streak_df$Streak_Length), y = expected_prob, 
  #                label = paste("Expected P(H) =", expected_prob), color = "firebrick", vjust = -1)
  #   } else if (input$checkbox2) {
  #     ggplot() +
  #       annotate("text", x = 1, y = 1, label = "No data yet.", size = 6) +
  #       theme_void()
  #   } else {
  # 
  #   }
  # })
  
})