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
  mutate_at(vars(3:7), as.numeric) %>%
  arrange(desc(Subscribers.count)) %>%  # Arrange by the most subscribers to the least
  slice(1:100)  # Select the first 100 rows

June[, c("Subscribers.count", "Views.avg.", "Likes.avg")] <- 
  June[, c("Subscribers.count", "Views.avg.", "Likes.avg")] / 1000000

June[, c("Shares.avg", "Comments.avg.")] <- 
  June[, c("Shares.avg", "Comments.avg.")] / 1000


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

September <- September %>% arrange(desc(Subscribers)) %>% slice(1:100)

September <- September[, -1]

September <- data.frame(
  Influencer = September$Tiktok.name,
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

December <- December %>% arrange(desc(followers)) %>% slice(1:100)

December <- December[, -1]

December <- data.frame(
  Influencer = December$Tiktok.name,
  Subscribers = December$followers/1000000,
  Views = December$views.avg./1000000,
  Likes = December$likes.avg../1000000,
  Comments = December$comments.avg../1000,
  Shares = December$shares.avg../1000
)

# Define UI
ui <- fluidPage(
  titlePanel("Top TikTok Influencers"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:",
                  choices = c("June", "September", "December"),
                  selected = "June"),
      selectInput("metric", "Engagement Metric:",
                  choices = c("Subscribers", "Views", "Likes", "Shares", "Comments"),
                  selected = "Subscribers"),
      selectInput("num_influencers", "Select Number of Influencers:",
                  choices = c(10, 20, 50),
                  selected = 10)  # Default to top 10 influencers
    ),
    mainPanel(
      plotlyOutput("barplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to select dataset based on user-selected month
  selected_month_data <- reactive({
    switch(input$month,
           "June" = June,
           "September" = September,
           "December" = December)
  })
  
  # Function to generate barplot based on selected metric, month, and number of influencers
  generate_barplot <- function() {
    data <- selected_month_data()
    num_influencers <- input$num_influencers
    month_color <- switch(input$month,
                          "June" = "skyblue",
                          "September" = "black",
                          "December" = "purple")
    p <- ggplot(data[1:num_influencers, ], aes_string(x = "Influencer", y = input$metric)) +
      geom_bar(stat = "identity", fill = month_color) +  # Use month-specific color
      labs(title = paste("Top", num_influencers, "TikTok Influencers in", input$month, "-", input$metric),
           x = "Influencer (name)", y = NULL) +  # Remove y-axis label here
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Modify y-axis label to include unit
    if (input$metric %in% c("Subscribers", "Views", "Likes")) {
      p <- p + ylab(paste0(input$metric, " (per millions)"))
    } else if (input$metric %in% c("Shares", "Comments")) {
      p <- p + ylab(paste0(input$metric, " (per thousand)"))
    }
    
    ggplotly(p)
  }
  
  # Render plotly barplot
  output$barplot <- renderPlotly({
    generate_barplot()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

