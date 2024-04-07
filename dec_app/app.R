library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

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
  Influencers = December$Tiktok.name,
  Subscribers = December$followers/1000000,
  Views = December$views.avg./1000000,
  Likes = December$likes.avg../1000000,
  Comments = December$comments.avg../1000,
  Shares = December$shares.avg../1000
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Top 100 TikTok Influencers in December"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Engagement Metric:",
                  choices = c("Subscribers", "Views", "Likes", "Shares", "Comments"),
                  selected = "Subscribers")
    ),
    mainPanel(
      plotlyOutput("barplot")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Function to generate barplot based on selected metric
  generate_barplot <- function(metric) {
    p <- ggplot(December, aes_string(x = "Influencers", y = metric)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = paste("Top 100 TikTok Influencers in December -", metric),
           x = "Influencers (name)", y = metric) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (metric %in% c("Subscribers", "Views", "Likes")) {
      p <- p + ylab(paste0(metric, " (per million)"))
    } else if (metric %in% c("Shares", "Comments")) {
      p <- p + ylab(paste0(metric, " (per thousand)"))
    }
    
    ggplotly(p)
  }
  
  # Render plotly barplot
  output$barplot <- renderPlotly({
    generate_barplot(input$metric)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
