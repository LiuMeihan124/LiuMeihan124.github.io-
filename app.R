library(shiny)
library(ggplot2)
library(plotly)

June <- data.frame(
  Influencer = June$Influencer,
  Subscribers = June$Subscribers,
  Views = June$Views,
  Likes = June$Likes,
  Shares = June$Shares,
  Comments = June$Comments
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
           x = "Influencer", y = metric) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  }
  
  # Render plotly barplot
  output$barplot <- renderPlotly({
    generate_barplot(input$metric)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
