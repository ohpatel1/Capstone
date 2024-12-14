library(shiny)
library(ggplot2)
library(randomForest)
library(DT)
library(shinyjs)
library(shinythemes)
library(caret)

# Load dataset
used_cars <- read.csv("used_cars_fully_encoded_10000.csv")# Update to your dataset's file path
head(used_cars)

# Select relevant columns for the model
model_data <- used_cars[, c("make_name", "year", "mileage", "horsepower", "transmission", "price")]
model_data <- model_data[1:10000, ]  # Use the first 40,000 rows

# Reduce high-cardinality variables by combining rare levels
make_name_freq <- table(model_data$make_name)
model_data$make_name <- as.factor(ifelse(
  model_data$make_name %in% names(make_name_freq[make_name_freq < 18]),
  "Other",
  as.character(model_data$make_name)
))

transmission_freq <- table(model_data$transmission)
model_data$transmission <- as.factor(ifelse(
  model_data$transmission %in% names(transmission_freq[transmission_freq < 10]),
  "Other",
  as.character(model_data$transmission)
))

# Ensure categorical variables are factors
model_data$make_name <- as.factor(model_data$make_name)
model_data$transmission <- as.factor(model_data$transmission)

# Split the data into training (70%) and testing (30%) sets
set.seed(123)
train_index <- createDataPartition(model_data$price, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train the random forest model on the training data
library(randomForest)
set.seed(123)
rf_model <- randomForest(price ~ ., data = train_data, importance = TRUE)

# Predict prices on the testing set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate model performance
mse <- mean((test_data$price - predictions)^2)  # Mean Squared Error
rmse <- sqrt(mse)  # Root Mean Squared Error
r_squared <- 1 - sum((test_data$price - predictions)^2) / sum((test_data$price - mean(test_data$price))^2)  # R²

print(mse)
print(rmse)
print(r_squared)

# Define UI
library(shinyjs)  # Ensure this is loaded

library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)

# Define custom CSS for styling
custom_css <- "
body {
  background-color: #f5f5f5; 
  font-family: 'Helvetica Neue', Arial, sans-serif;
}

#header {
  text-align: center; 
  padding: 20px; 
  background-color: #ff914d; 
  color: white; 
  border-bottom: 3px solid #f3762f;
}

.sidebar {
  background-color: #ffffff; 
  border-radius: 10px; 
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
  padding: 20px;
}

.sidebar h3 {
  color: #f3762f;
}

.action-button {
  background-color: #ff914d; 
  color: white; 
  border: none; 
  padding: 10px 20px; 
  border-radius: 5px;
  font-size: 14px;
}

.action-button:hover {
  background-color: #f3762f;
  color: white;
}

.main-panel {
  background-color: #ffffff; 
  border-radius: 10px; 
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
  padding: 20px;
}

.main-panel h3 {
  color: #f3762f;
}

.data-table {
  padding: 10px; 
  font-size: 14px;
}
"

ui <- fluidPage(
  useShinyjs(),  # For toggling filter visibility
  inlineCSS("
    #header {
      text-align: center; 
      padding: 20px; 
      background-color: #ff914d; 
      color: white; 
      border-bottom: 3px solid #f3762f;
    }
    .sidebar {
      background-color: #ffffff; 
      border-radius: 10px; 
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
      padding: 20px;
      margin-bottom: 20px;
    }
    .main-panel {
      background-color: #ffffff; 
      border-radius: 10px; 
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
      padding: 20px;
    }
    .action-button {
      background-color: #ff914d; 
      color: white; 
      border: none; 
      padding: 10px 20px; 
      border-radius: 5px;
      font-size: 14px;
    }
    .action-button:hover {
      background-color: #f3762f;
    }
  "),  # Custom CSS for styling
  
  # App title
  div(id = "header", h1("Used Car Price Predictor and Finder")),
  
  # Main layout
  fluidRow(
    column(
      4,  # Price predictor on the left
      div(
        class = "sidebar",
        h3("Predict Car Price"),
        selectInput("make_name", "Make:", levels(model_data$make_name)),
        numericInput("year", "Year:", value = 2015, min = 1980, max = 2024),
        numericInput("mileage", "Mileage (in miles):", value = 50000, min = 0),
        numericInput("horsepower", "Horsepower:", value = 200, min = 50),
        selectInput("transmission", "Transmission:", levels(model_data$transmission)),
        actionButton("predict", "Predict Price", class = "action-button"),
        br(),
        h3("Predicted Price:"),
        verbatimTextOutput("predicted_price")
      )
    ),
    column(
      8,  # Filters and results on the right
      div(
        class = "sidebar",
        h3("Car Finder Filters"),
        actionButton("toggle_filters", "Show/Hide Filters", class = "action-button"),
        br(), br(),
        hidden(div(
          id = "filters_section",
          selectInput("filter_make", "Make:", c("All", levels(used_cars$make_name)), selected = "All"),
          sliderInput("filter_year", "Year Range:", 
                      min = min(used_cars$year), max = max(used_cars$year), 
                      value = c(2010, 2024), step = 1),
          selectInput("filter_transmission", "Transmission:", 
                      c("All", levels(used_cars$transmission)), selected = "All"),
          sliderInput("filter_mileage", "Mileage (in miles):", 
                      min = min(used_cars$mileage), max = max(used_cars$mileage), 
                      value = c(0, 100000), step = 1000),
          sliderInput("filter_price", "Price Range ($):", 
                      min = min(used_cars$price), max = max(used_cars$price), 
                      value = c(0, 50000), step = 500)
        ))
      ),
      div(
        class = "main-panel",
        h3("Car Finder Results"),
        div(dataTableOutput("data_table"))
      )
    )
  )
)




server <- function(input, output, session) {
  
  # Toggle visibility of filters
  observeEvent(input$toggle_filters, {
    toggle("filters_section")
  })
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    data <- used_cars
    
    # Apply filters
    if (input$filter_make != "All") {
      data <- data[data$make_name == input$filter_make, ]
    }
    data <- data[data$year >= input$filter_year[1] & data$year <= input$filter_year[2], ]
    if (input$filter_transmission != "All") {
      data <- data[data$transmission == input$filter_transmission, ]
    }
    data <- data[data$mileage >= input$filter_mileage[1] & data$mileage <= input$filter_mileage[2], ]
    data <- data[data$price >= input$filter_price[1] & data$price <= input$filter_price[2], ]
    
    # Select only variables used in prediction and trim_name
    data <- data[, c("make_name", "trim_name", "year", "mileage", "horsepower", "transmission", "price")]
    
    data
  })
  
  # Render filtered dataset
  output$data_table <- renderDataTable({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,  
        scrollX = TRUE,   
        rowCallback = JS(
          "function(row, data) {",
          "  $('td', row).css({'padding': '6px'});",
          "}"
        )
      )
    )
  })
  
  # Update dropdowns dynamically
  observe({
    updateSelectInput(session, "filter_make", 
                      choices = c("All", unique(as.character(used_cars$make_name))))
    updateSelectInput(session, "filter_transmission", 
                      choices = c("All", unique(as.character(used_cars$transmission))))
  })
  
  # Predict price logic
  predicted_price <- eventReactive(input$predict, {
    new_data <- data.frame(
      make_name = factor(input$make_name, levels = levels(model_data$make_name)),
      year = input$year,
      mileage = input$mileage,
      horsepower = input$horsepower,
      transmission = factor(input$transmission, levels = levels(model_data$transmission))
    )
    predict(rf_model, new_data)
  })
  
  
  # Output predicted price
  output$predicted_price <- renderText({
    req(predicted_price())
    paste("$", round(predicted_price(), 2))
  })
}



# Run the application
shinyApp(ui = ui, server = server)

