MATH 534 HW 6 Q1
================
David Zheng
2025-10-19

- [Q1](#q1)

# Q1

``` r
# app.R
library(shiny)
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
ui <- fluidPage(
  titlePanel("Interactive Car Mileage Explorer"),

  sidebarLayout(
    sidebarPanel(
      # 1. Text input for title
      textInput("plot_title", "Plot title:", "My MPG Plot"),

      # 2. Select input for x variable
      selectInput("xvar", "Choose X-axis variable:",
                  choices = names(mtcars),
                  selected = "hp"),

      # 3. Slider to filter by mpg
      sliderInput("mpg_filter", "Minimum MPG:",
                  min = min(mtcars$mpg),
                  max = max(mtcars$mpg),
                  value = 15),

      # 4. Checkbox group to choose cylinders
      checkboxGroupInput("cyl_filter", "Select cylinder types:",
                         choices = sort(unique(mtcars$cyl)),
                         selected = unique(mtcars$cyl)),

      # 5. Radio buttons for color
      radioButtons("color_choice", "Point color:",
                   choices = c("blue", "red", "darkgreen", "purple"),
                   selected = "blue")
    ),

    mainPanel(
      plotOutput("scatter_plot"),
      tableOutput("filtered_table")
    )
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    mtcars %>%
      filter(mpg >= input$mpg_filter,
             cyl %in% input$cyl_filter)
  })

  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$xvar, y = "mpg")) +
      geom_point(color = input$color_choice, size = 3) +
      labs(
        x = input$xvar,
        y = "Miles per Gallon (mpg)",
        title = input$plot_title
      ) +
      theme_minimal(base_size = 14)
  })

  output$filtered_table <- renderTable({
    head(filtered_data(), 10)
  })
}

shinyApp(ui = ui, server = server)
```

    ## 
    ## Listening on http://127.0.0.1:6776

    ## Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ## ℹ Please use tidy evaluation idioms with `aes()`.
    ## ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](David-Zheng-MATH-534-HW-6-Q1_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
