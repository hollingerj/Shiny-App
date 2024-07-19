library(shiny)


ui <- fluidPage(

    
    titlePanel("Shiny LM Data"),

 
    sidebarLayout(
        sidebarPanel(

            
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
           
            tags$hr(),
            
            
            checkboxInput("header", "Header", TRUE),
            
           
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
      
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
           
            tags$hr(),
            
            
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            actionButton("go", "Plot Linear Model")
        ),

       
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)


server <- function(input, output) {

    lmdata <- reactiveValues()

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })

    observeEvent(input$go,{
        update_lm()
    })

    update_lm <- function(){
        lmdata$model <- lm(y ~ x, data = dataInput())
        lmdata$rsq <- summary(lmdata$model)$r.squared
        lmdata$coef <- summary(lmdata$model)$coefficients
        
        # Debugging print statements
        print("Linear model updated:")
        print(lmdata$model)
        print("Coefficients:")
        print(lmdata$coef)
    }

    output$distPlot <- renderPlot({
        print("Rendering scatter plot...")
        print(dataInput())
        plot(dataInput()$x, dataInput()$y, xlab = "X Axis", ylab = "Y Axis")
    })

    output$lmPlot <- renderPlot({
        req(dataInput())
        print("Rendering lm plot...")
        print(lmdata$model)
        
        # Plot scatter plot
        plot(dataInput()$x, dataInput()$y, xlab = "X Axis", ylab = "Y Axis")
        
        # Check if lmdata$model exists
        if (!is.null(lmdata$model)) {
            # Add linear model line
            abline(lmdata$model, col = "blue")
            
            # Extract coefficients
            slope <- round(lmdata$coef[2], 2)
            intercept <- round(lmdata$coef[1], 2)
            correlation <- round(cor(dataInput()$x, dataInput()$y), 2)
            
            # Display coefficients as text on the plot
            legend("topright", legend = c(paste("Slope:", slope), paste("Intercept:", intercept), paste("Correlation:", correlation)), 
                   bty = "n", cex = 0.8, text.col = "black")
        }
    })

    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(dataInput()))
        } else {
            return(dataInput())
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
