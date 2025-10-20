MATH 534 HW 6 Q2
================
David Zheng
2025-10-19

- [Q2](#q2)

# Q2

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
library(palmerpenguins)
```

    ## 
    ## Attaching package: 'palmerpenguins'

    ## The following objects are masked from 'package:datasets':
    ## 
    ##     penguins, penguins_raw

``` r
# Convert factor columns to character safely
species_choices <- c("All", sort(unique(na.omit(as.character(penguins$species)))))
# Add "NA" label explicitly for missing sex values
sex_choices <- c("All",
                 sort(unique(na.omit(as.character(penguins$sex)))),
                 "NA")

ui <- fluidPage(
  titlePanel("Palmer Penguins Explorer"),

  sidebarLayout(
    sidebarPanel(
      # Species dropdown
      selectInput("species", "Select Species:",
                  choices = species_choices,
                  selected = "All"),

      # Sex dropdown (now includes NA option)
      selectInput("sex", "Select Sex:",
                  choices = sex_choices,
                  selected = "All"),

      # X-axis
      selectInput("xvar", "X-axis Variable:",
                  choices = c("Bill Length (mm)"   = "bill_length_mm",
                              "Bill Depth (mm)"    = "bill_depth_mm",
                              "Flipper Length (mm)"= "flipper_length_mm",
                              "Body Mass (g)"      = "body_mass_g"),
                  selected = "bill_length_mm"),

      # Y-axis
      selectInput("yvar", "Y-axis Variable:",
                  choices = c("Bill Length (mm)"   = "bill_length_mm",
                              "Bill Depth (mm)"    = "bill_depth_mm",
                              "Flipper Length (mm)"= "flipper_length_mm",
                              "Body Mass (g)"      = "body_mass_g"),
                  selected = "flipper_length_mm"),

      # Color by
      radioButtons("color_by", "Color By:",
                   choices = c("Species", "Sex"),
                   selected = "Species")
    ),

    mainPanel(
      plotOutput("penguin_plot", height = "500px"),
      tableOutput("penguin_table")
    )
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    data <- penguins

    # Filter by species
    if (input$species != "All") {
      data <- data %>% filter(as.character(species) == input$species)
    }

    # Filter by sex, including NA
    if (input$sex != "All") {
      if (input$sex == "NA") {
        data <- data %>% filter(is.na(sex))
      } else {
        data <- data %>% filter(as.character(sex) == input$sex)
      }
    }

    data
  })

  # Scatterplot
  output$penguin_plot <- renderPlot({
    df <- filtered_data()
    color_var <- if (tolower(input$color_by) == "species") "species" else "sex"

    ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], color = .data[[color_var]])) +
      geom_point(size = 3, alpha = 0.85, na.rm = TRUE) +
      labs(
        x = input$xvar,
        y = input$yvar,
        color = input$color_by,
        title = "Palmer Penguins Measurements"
      ) +
      theme_minimal(base_size = 14)
  })

  # Table output
  output$penguin_table <- renderTable({
    filtered_data() %>%
      select(species, sex, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
      head(10)
  })
}

shinyApp(ui = ui, server = server)
```

    ## 
    ## Listening on http://127.0.0.1:4488

![](David-Zheng-MATH-534-HW-6-Q2_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
