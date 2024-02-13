library(shiny)
library(shinythemes)

# BMI Categories
bmi_categories <- c("Underweight", "Normal Weight", "Overweight", "Obese")

# BMI Category Thresholds
bmi_thresholds <- c(18.5, 24.9, 29.9)

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("BMI Calculator:",
                           
                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      sliderInput("height", 
                                                  label = "Height (cm)", 
                                                  value = 175, 
                                                  min = 40, 
                                                  max = 250),
                                      sliderInput("weight", 
                                                  label = "Weight (kg)", 
                                                  value = 70, 
                                                  min = 20, 
                                                  max = 300),
                                      actionButton("submitbutton", 
                                                   "Calculate BMI", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata'), # Results table
                                      textOutput("category"),
                                      plotOutput("bmi_plot")
                                    ) # mainPanel()
                                    
                           ), #tabPanel(), Home
                           
                           tabPanel("About", 
                                    titlePanel("About"), 
                                    div(
                                      align="justify",
                                      p("Welcome to the BMI Calculator app!"),
                                      p("This application helps you calculate your Body Mass Index (BMI), a measure of body fat based on your height and weight."),
                                      p("BMI is commonly used to categorize individuals into different weight status categories, such as underweight, normal weight, overweight, and obese."),
                                      p("This app provides real-time BMI calculation as you adjust your height and weight using the sliders."),
                                      p("Explore the BMI categories and visualize the distribution of BMI categories using the plots."),
                                      p("For any questions or feedback, please reach out to us at hgaur8@gatech.edu"),
                                      p("Thank you for using the BMI Calculator app!")
                                    )
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    bmi <- input$weight / ((input$height/100) ^ 2)
    bmi <- round(bmi, 1)
    bmi_df <- data.frame(BMI = bmi)
    bmi_df$Category <- cut(bmi, breaks = c(-Inf, bmi_thresholds, Inf), labels = bmi_categories)
    bmi_df
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(datasetInput()) 
    } 
  })
  
  # BMI Category
  output$category <- renderText({
    if (input$submitbutton > 0) {
      bmi <- isolate(datasetInput()$BMI)
      category <- isolate(datasetInput()$Category)
      paste("Your BMI is", bmi, "which falls into the category of", category)
    } else {
      ""
    }
  })
  
  # BMI Plot
  output$bmi_plot <- renderPlot({
    if (input$submitbutton > 0) {
      bmi <- isolate(datasetInput()$BMI)
      category <- isolate(datasetInput()$Category)
      barplot(table(category), col = rainbow(length(bmi_categories)), 
              main = "BMI Categories", xlab = "Category", ylab = "Frequency")
    }
  })
}

####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)

# Deploy the app
# rsconnect::deployApp(appDir = "/Users/hinagaur/Desktop/R/bmi_app/")
