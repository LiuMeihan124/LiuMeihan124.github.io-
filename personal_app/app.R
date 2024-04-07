library(shiny)
library(ggplot2)
library(plotly)

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

#Define ui
ui <- fluidPage(
  titlePanel("Top 100 TikTok Influencers in June"),
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
    p <- ggplot(June, aes_string(x = "Influencer", y = metric)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = paste("Top 100 TikTok Influencers in June -", metric),
           x = "Influencer (name)", y = NULL) +  # Remove y-axis label here
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Modify y-axis label to include unit
    if (metric %in% c("Subscribers", "Views", "Likes")) {
      p <- p + ylab(paste0(metric, " (per millions)"))
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
