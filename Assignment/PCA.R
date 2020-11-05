library(shinydashboard)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage("PCA: Principal Component Analysis",
                  tabsetPanel(tabPanel("PCA",
                    sidebarPanel(fileInput('files',"Upload your Dataset (Numeric)"),),
                      mainPanel(
                        tabsetPanel(tabPanel("Dataset Table View", tableOutput("table")),
                          tabPanel("Plot",plotOutput("plot")),
                          tabPanel("Eigen Values", tableOutput("eigval_table")),
                          tabPanel("Eigen Vectors", tableOutput("eigvec_table")),
                          tabPanel("PCA", tableOutput("pca_table"))
                          )
                        )
                      )
                    )
                  )
)


# Define server logic ----
server <- function(input, output) {
  op_table <- reactive({
    inputFile <- input$files
    if (is.null(inputFile)) return(NULL)
    na.omit(inputFile)
    data <-read.csv(inputFile$datapath, header = TRUE,
                    sep = ',',stringsAsFactors = FALSE)
    data
  })
  
#To Show Dataset Table View
  output$table <- renderTable({
    op_table()
  })
  
#To show the plot
  output$plot <- renderPlot({
    data <- op_table()
    n_row <- dim(data)[1]
    n_col  <- dim(data)[2]
    scaled_data <- scale(data[,])
    cov <- cov(scaled_data)
    eigen <- eigen(cov)
    Variance <- eigen$values
    plot_data <- qplot(x=c(1:n_col), y=Variance) + 
      geom_line() + 
      xlab("Principal Component") + 
      ylab("Variance") +
      ggtitle("Plot") 
    plot_data
  })
  
#To Show The Eigen Values
  output$eigval_table <- renderTable({
    data <- op_table()
    scaled_data <- scale(data[,])
    cov <- cov(scaled_data)
    eigen <- eigen(cov)
    Variance <- eigen$values
  })
  
#To Show The Eigen Vectors
  output$eigvec_table <- renderTable({
    data <- op_table()
    scaled_data <- scale(data[,])
    cov <- cov(scaled_data)
    eigen <- eigen(cov)
    EigVec <- eigen$vectors
  })
  
#To Show The PCA
  output$pca_table <- renderTable({
    data <- op_table()
    scaled_data <- scale(data[,])
    cov <- cov(scaled_data)
    eigen <- eigen(cov)
    EigVec <- eigen$vectors
    pca_val <- prcomp(data)
    pca_val$x
  })
}

shinyApp(ui, server)


