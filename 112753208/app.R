library(shiny)
library(shinydashboard)
library(ggbiplot)
library(FactoMineR)
library(factoextra)

data(iris)
log.iris <- log(iris[, 1:4])
ir.pca <- prcomp(log.iris, center = TRUE, scale = TRUE)

irisCA_dum <- iris
irisCA_dum$Species <- as.factor(irisCA_dum$Species)
irisCA_dum <- cbind(irisCA_dum, model.matrix(~Species-1, data = irisCA_dum))
res.ca <- CA(irisCA_dum[,-5], graph = FALSE)

pc_choices <- c(PC1 = 1, PC2 = 2, PC3 = 3, PC4 = 4)

# Define the UI interface for the Shiny dashboard

ui <- shinyUI(
  navbarPage(
    "羅鈺涵的hw4",
    tabPanel(
      "pca",
      fluidRow(
        column(
          3,
          sliderInput('input_size', 'Input data size', min = 6, max = nrow(iris), value = min(150, nrow(iris)), round = 0),
          selectInput("x_axis", label = "X-Axis:", choices = names(pc_choices), selected = names(pc_choices)[1]),
          selectInput("y_axis", label = "Y-Axis:", choices = names(pc_choices), selected = names(pc_choices)[2])
        ),
        column(
          9,
          tabsetPanel(
            tabPanel("pca plot", plotOutput("pca_plot"),plotOutput("ir.pca")),
            tabPanel("result data", tableOutput('pca_data')),
            tabPanel("input data", tableOutput('input_data'))
          )
        )
      )
    ),
    tabPanel(
      "ca",
      fluidRow(
        column(
          3,
          sliderInput('inputsize', 'Input data size', min = 6, max = nrow(iris), value = min(150, nrow(iris)), round = 0)
        ),
        column(
          9,
          plotOutput("ca_plot")
        )
      )
    ),
    tabPanel("iris data", fluidRow(
      column(
        3,
        h4("iris data"),
        textInput("txtInput","The Iris dataset was used in R.A. Fisher's classic 1936 paper, The Use of Multiple Measurements in Taxonomic Problems, and can also be found on the UCI Machine Learning Repository."
        ), 
        tableOutput('iris_data')
      )
    ))
  )
)



server <- function(input, output, session) {
  
  #PCA繪圖處理
  observeEvent(input$input_size, {
    log.ir <- log(iris[1:input$input_size, 1:4])
    ir.species <- iris[1:input$input_size, 5]
    ir.pca <- prcomp(log.ir, center = TRUE, scale = TRUE)
    
    output$pca_plot <- renderPlot({
      g <- ggbiplot(ir.pca, choices = c(pc_choices[[input$x_axis]], pc_choices[[input$y_axis]]),obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE)
      g <- g + scale_color_discrete(name = '')
      g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
      print(g)
    })
    
    observeEvent(input$x_axis, {
      available_choices <- names(pc_choices)[-grep(input$x_axis, names(pc_choices))]
      updateSelectInput(session, "y_axis", choices = available_choices, selected = available_choices[1])
    })
    
    output$ir.pca <- renderPlot({
      plot(ir.pca, type = 'l')
    })
    
    observeEvent(input$inputsize,{
      iris_dum <- iris[1:input$inputsize, ]
      iris_dum$Species <- as.factor(iris_dum$Species)
      iris_dum <- cbind(iris_dum, model.matrix(~Species-1, data = iris_dum))
      res.ca <- CA(iris_dum[,-5], graph = FALSE)
      iris_dum[,5:ncol(iris_dum)] <- lapply(iris_dum[,5:ncol(iris_dum)], factor)
      output$ca_plot <- renderPlot({
        fviz_ca_row(res.ca, col.row = iris_dum$Species, repel = TRUE, nbelements = input$inputsize)
      })
    })
    
    
    
    pca_data <- as.data.frame(ir.pca$x)
    output$pca_data <- renderTable({
      pca_data
    })
    
    output$input_data <- renderTable({
      log.ir
    })
    
    output$iris_data <- renderTable({
      print(iris)
    })
    
  })
  
}


# Run the Shiny application
shinyApp(ui, server)




