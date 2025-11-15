#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(ggridges)
library(ggdist)
library(dplyr)

# Ensure flat_model exists
top10_resale$flat_model <- as.factor(top10_resale$flat_model)

# Add "All" as an option
flat_choices <- c("All", levels(top10_resale$flat_model))

ui <- fluidPage(
  titlePanel("Top 10 Subzones Resale Prices by Flat Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "flat_sel",
        label = "Select Flat Model:",
        choices = flat_choices,
        selected = "All"
      )
    ),
    mainPanel(
      plotOutput("resalePlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    if(input$flat_sel == "All"){
      top10_resale
    } else {
      top10_resale %>% filter(flat_model == input$flat_sel)
    }
  })
  
  # Render ggplot
  output$resalePlot <- renderPlot({
    ggplot(filtered_data(), aes(
      x = resale_price,
      y = reorder(SUBZONE_N, resale_price, FUN = mean),
      fill = if(input$flat_sel == "All") flat_model else NULL  # color by flat_model only if "All"
    )) +
      stat_halfeye(
        adjust = 0.5,
        fill = if(input$flat_sel == "All") NULL else "#e1b588",
        justification = -0.2,
        .width = 0,
        point_colour = NA
      ) +
      geom_boxplot(width = 0.1, fill = "white", outlier.size = 1) +
      labs(
        title = paste("Top 10 Subzones by Average Resale Price -", input$flat_sel),
        x = "Resale Price (SGD)",
        y = "Subzone",
        fill = if(input$flat_sel == "All") "Flat Model" else NULL
      ) +
      theme_ridges() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5,
                                  margin = ggplot2::margin(b = 15)),
        panel.background = element_rect(fill = "#f6f6f6", color = NA),
        plot.background = element_rect(fill = "#f6f6f6", color = NA),
        axis.line.x = element_line(color = "grey30"),
        axis.ticks = element_line(color = "grey30"),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(hjust = 0.5, size = 8),
        axis.text.x = element_text(hjust = 0.5, size = 8,
                                   margin = ggplot2::margin(t = 10)),
        axis.title.x = element_text(size = 10,
                                    margin = ggplot2::margin(t = 10))
      )
  })
  
}

shinyApp(ui, server)

