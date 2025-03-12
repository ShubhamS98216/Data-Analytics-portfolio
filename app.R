#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(sf)
library(plotly)

# Read your data here

data <- read.csv("Global_YouTube_Statistics.csv")

# Define UI
ui <- fluidPage(
  titlePanel("YouTube Statistics Dashboard"),
  
  # Tab layout with three tabs
  tabsetPanel(
    tabPanel("Category Revenue", 
             plotOutput("categoryRevenuePlot")),
    
    tabPanel("Country Views",
             plotOutput("countryViewsPlot")),
    
    tabPanel("Subscribers vs Views",
             plotOutput("subscribersViewsPlot"))
  )
)

# Define server logic
server <- function(input, output) {
  
  # Category Revenue Plot
  output$categoryRevenuePlot <- renderPlot({
    
    categ_rev.df = data %>%
      group_by(category) %>%
      summarise(monthly_revenue = mean(highest_monthly_earnings)) %>%
      arrange(desc(monthly_revenue))
    
    categ_rev.df = categ_rev.df %>%
      arrange(desc(monthly_revenue)) %>%
      mutate(rank = row_number())
    
    
    categ_rev.df$revenue <- round(categ_rev.df$monthly_revenue/1e6, digits = 2)
    
    ggplot(categ_rev.df, aes(x = reorder(category, revenue), y = revenue, fill = revenue)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = ifelse(rank <= 4, format(round(revenue, digits = 2), nsmall = 2), "")),
        position = position_dodge(width = 1),
        size = 3,
        vjust = 0.5
      ) +
      scale_fill_gradient(low = "#FFDDE1", high = "#EE9CA7") +
      theme_minimal() +
      theme(
        legend.position = "left", legend.title = element_blank()
      ) +
      labs(title = "Average Monthly Revenue by Channel Category (in Millions)", x = "Category", y = "Average Monthly Revenue", color = "Revenue") +
      coord_flip()
  })
  
  # Country Views Plot
  output$countryViewsPlot <- renderPlot({
    country_view.df = data %>%
      group_by(Country) %>%
      summarise(views = mean(video.views), revenue = mean(highest_monthly_earnings)) %>%
      arrange(desc(views))
    
    country_view.df = country_view.df %>%
      arrange(desc(views), desc(revenue)) %>%
      slice_head(n = 15) %>%
      mutate(rank = row_number())
    
    country_view.df$views_billion <- round(country_view.df$views / 1e9, digits = 1)
    
    ggplot(country_view.df, aes(x = reorder(Country, views_billion), y = views_billion, fill = views_billion)) +
      geom_col() +
      geom_text(
        aes(label = format(round(views_billion, digits = 1), nsmall = 1)),
        position = position_stack(vjust = 0.5),
        size = 3
      ) +
      scale_fill_gradient(low = "#bdc3c7", high = "#2c3e50") +
      theme_minimal() +
      theme(
        legend.position = "left", 
        legend.title = element_blank()
      ) +
      labs(title = "Views by each country (Billions)", x = "Country", y = "Views (in Billions)") + 
      coord_flip()
  })
  
  # Subscribers vs Views Plot
  output$subscribersViewsPlot <- renderPlot({
    subscribers.df <- data %>%
      group_by(Country) %>%
      summarise(subscribers = mean(subscribers), views = mean(video.views), revenue = mean(highest_monthly_earnings)) %>%
      arrange(desc(subscribers))
    
    
    model = lm(subscribers.df$views ~ subscribers.df$subscribers)
    summary(model)
    
    subscribers.df$views_scaled <- subscribers.df$views/1e9
    subscribers.df$subscribers_scaled <- subscribers.df$subscribers/1e6
    
    ggplot(subscribers.df, aes(x = subscribers_scaled, y = views_scaled, label = Country)) +
      geom_point(aes(color = views_scaled), size = 4) +
      geom_text_repel() +
      geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "dashed") +
      scale_color_gradient(low = "lightgreen", high = "darkgreen") +
      labs(title = "Scatter Plot of Views vs Subscribers", x = "Subscribers(Millions)", y = "Views (Billion)", color = "Views (Billions)") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
