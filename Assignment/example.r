library(shiny)
library(shinythemes)


# Define UI ----
ui <- fluidPage(theme = shinytheme("journal"),
                
                navbarPage(
                  "Principal Component Analysis",
                  tabsetPanel(
                    tabPanel("PCA",sidebarPanel(
                      fileInput('files',"Dataset"),
                      #radioButtons('sep', 'Separator:',c(Comma=',',Semicolon=';',Tab='\t'))
                      
                    ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Table View", tableOutput("table")),
                        
                        tabPanel(" plot",plotOutput("plot"))
                        
                      )
                    )
                    )
                    
                    
                    
                    
                  ))
                
)

# Define server logic ----
server <- function(input, output) {
  
  table_out <- reactive({
    inFile <- input$files
    if (is.null(inFile)) return(NULL)
    na.omit(inFile)
    data <-read.csv(inFile$datapath, header = TRUE,
                    sep = ',',stringsAsFactors = FALSE)
    data
  })
  
  
  
  
  output$table <- renderTable({
    table_out()
  })
  
  output$plot <- renderPlot({
    data <- table_out()
    scaled_data <- scale(data[,2:14])
    cov <- cov(scaled_data)
    eigen <- eigen(cov)
    Variance <- eigen$values
    plot_data <- qplot(x=c(1:13), y=Variance) + 
      geom_line() + 
      xlab("Principal Component") + 
      ylab("Variance") +
      ggtitle("Scree Plot") 
    #ylim(0, 1)
    plot_data
  })
  
  
  
}

shinyApp(ui = ui, server = server)
