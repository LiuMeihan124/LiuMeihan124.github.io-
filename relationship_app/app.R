library(shiny)
library(ggplot2)
library(plotly)

##JUNE
june_data <- read.csv("june.csv", stringsAsFactors = FALSE)

replace_k_m <- function(x) {
  for (i in 1:length(x)) {
    # 将k替换为*1000
    x[i] <- gsub("([0-9]+\\.?[0-9]*)k", "\\1*1000", x[i], ignore.case = TRUE, perl = TRUE)
    # 将m替换为*1000000
    x[i] <- gsub("([0-9]+\\.?[0-9]*)m", "\\1*1000000", x[i], ignore.case = TRUE, perl = TRUE)
    # 将字符串计算结果转为数值
    x[i] <- eval(parse(text = x[i]))
  }
  return(x)
}


cols_to_replace <- c(3:7)
june_data[, cols_to_replace] <- lapply(june_data[, cols_to_replace], replace_k_m)


library(dplyr)

# Assuming june_data is your original data frame
June <- june_data %>%
  mutate_at(vars(3:7), as.numeric)

June[, c("Subscribers.count", "Views.avg.", "Likes.avg")] <- 
  June[, c("Subscribers.count", "Views.avg.", "Likes.avg")] / 1000000

June[, c("Shares.avg", "Comments.avg.")] <- 
  June[, c("Shares.avg", "Comments.avg.")] / 1000

June <- June %>% 
  sample_n(1000, replace = FALSE)


June <- data.frame(
  Influencer = June$influencer.name,
  Subscribers = June$Subscribers.count,
  Views = June$Views.avg.,
  Likes = June$Likes.avg,
  Shares = June$Shares.avg,
  Comments = June$Comments.avg.
)

##SEPTEMBER
library(tidyverse)

sep_data <- read.csv("sep.csv", stringsAsFactors = FALSE)

replace_k_m <- function(x) {
  for (i in 1:length(x)) {
    # 将k替换为*1000
    x[i] <- gsub("([0-9]+\\.?[0-9]*)k", "\\1*1000", x[i], ignore.case = TRUE, perl = TRUE)
    # 将m替换为*1000000
    x[i] <- gsub("([0-9]+\\.?[0-9]*)m", "\\1*1000000", x[i], ignore.case = TRUE, perl = TRUE)
    # 将字符串计算结果转为数值
    x[i] <- eval(parse(text = x[i]))
  }
  return(x)
}

cols_to_replace <- c(4:8)
sep_data[, cols_to_replace] <- lapply(sep_data[, cols_to_replace], replace_k_m)

September <- sep_data %>%
  mutate_at(vars(4:8), as.numeric)

September <- September[, -1]

September <- data.frame(
  Influencers = September$Tiktok.name,
  Subscribers = September$Subscribers/1000000,
  Views = September$Views.avg./1000000,
  Likes = September$Likes.avg./1000000,
  Comments = September$Comments.avg./1000,
  Shares = September$Shares.avg./1000
)

##DECEMBER
dec_data <-read.csv("dec.csv", stringsAsFactors = FALSE)

replace_k_m <- function(x) {
  for (i in 1:length(x)) {
    # 将k替换为*1000
    x[i] <- gsub("([0-9]+\\.?[0-9]*)k", "\\1*1000", x[i], ignore.case = TRUE, perl = TRUE)
    # 将m替换为*1000000
    x[i] <- gsub("([0-9]+\\.?[0-9]*)m", "\\1*1000000", x[i], ignore.case = TRUE, perl = TRUE)
    # 将字符串计算结果转为数值
    x[i] <- eval(parse(text = x[i]))
  }
  return(x)
}

cols_to_replace <- c(4:8)
dec_data[, cols_to_replace] <- lapply(dec_data[, cols_to_replace], replace_k_m)

December <- dec_data %>%
  mutate_at(vars(4:8), as.numeric)

December <- December[, -1]

December <- data.frame(
  Influencers = December$Tiktok.name,
  Subscribers = December$followers/1000000,
  Views = December$views.avg./1000000,
  Likes = December$likes.avg../1000000,
  Comments = December$comments.avg../1000,
  Shares = December$shares.avg../1000
)

# Define UI
ui <- fluidPage(
  
  titlePanel("Types of Influencers: Views vs Engagement Metrics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Month:",
                  choices = c("June", "September", "December"),
                  selected = "June"),
      selectInput("engagement_metric", "Select Engagement Metric:",
                  choices = c("Likes", "Shares", "Comments"),
                  selected = "Likes")
    ),
    
    mainPanel(
      plotlyOutput("scatterplot"),
      textOutput("correlation_text")
    )
  )
)

# Function to categorize influencers
categorize_influencers <- function(Subscribers) {
  ifelse(Subscribers > 1, "Mega",
         ifelse(Subscribers >= 0.1, "Macro",
                ifelse(Subscribers >= 0.01, "Micro", "Nano")))
}

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to select dataset based on user input
  dataset <- reactive({
    data <- switch(input$dataset,
                   "June" = June,
                   "September" = September,
                   "December" = December)
    
    # Apply categorization function to the dataset
    data$Influencer_Type <- factor(categorize_influencers(data$Subscribers),
                                   levels = c("Mega", "Macro", "Micro", "Nano"))
    data
  })
  
  # Generate scatter plot based on selected dataset and engagement metric
  output$scatterplot <- renderPlotly({
    # Filter data based on selected dataset
    selected_data <- dataset()
    
    # Create scatter plot based on selected engagement metric
    p <- ggplot(selected_data, aes_string(x = input$engagement_metric, y = "Views", color = "Influencer_Type")) +
      geom_point(alpha = 0.5) +
      labs(title = paste("Views vs", input$engagement_metric, "by Influencer Type -", input$dataset),
           x = paste(input$engagement_metric, "(millions)"), y = "Views (millions)") +
      theme_minimal()
    
    # Convert ggplot to plotly
    plotly_obj <- ggplotly(p)
    
    plotly_obj
  })
  
  # Calculate and display correlation coefficient
  output$correlation_text <- renderText({
    # Filter data based on selected dataset
    selected_data <- dataset()
    
    # Calculate correlation coefficient
    correlation_coefficient <- round(cor(selected_data[[input$engagement_metric]], selected_data$Views), 2)
    
    # Construct the text to display
    correlation_text <- paste("Correlation coefficient:", correlation_coefficient)
    
    correlation_text
  })
}

# Run the application
shinyApp(ui = ui, server = server)
