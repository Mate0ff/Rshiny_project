library(shinydashboard)
library(corrplot)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Final Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Presentation", tabName = "Data", icon = icon("book", lib = "glyphicon")),
      menuItem("Description", tabName = "Description", icon = icon("book", lib = "glyphicon")),
      menuItem("Correlation", icon = icon("stats", lib = "glyphicon"), tabName = "Correlation"),
      menuItem("Histogram", icon = icon("stats", lib = "glyphicon"), tabName = "Histogram"),
      menuItem("Boxplot", icon = icon("stats", lib = "glyphicon"), tabName = "Boxplot"),
      menuItem("Scatterplot", icon = icon("stats", lib = "glyphicon"), tabName = "Scatterplot"),
      menuItem("Pie Chart", icon = icon("stats", lib = "glyphicon"), tabName = "PieChart")
    ),
    selectInput(
      'vector', h3("Select data"),
      choices = c("mpg"=2,"cyl"=3,"disp"=4,"hp"=5, "drat"=6, "wt"=7,"qsec"=8,"vs"=9,"am"=10,"gear"=11,"carb"=12), 
      multiple=FALSE, selected=2
    ),
    sliderInput("slider", "Number of cars:", 1, 32, 32)
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "Data",
                h2("Data Presentation"),
                box(status = "warning", solidHeader = T, title = "mtcars dataset",
                    tableOutput("data"), width = 5)
        ),
        tabItem(tabName = "Description",
                h2("Description"),
                box(title = "Descriptive Statistics", status = "warning", collapsible = TRUE, solidHeader = T,
                    tableOutput("stats"),
                    h3(verbatimTextOutput('print1'))
                ),
                box(status = "warning", solidHeader = T,
                    h3(verbatimTextOutput('print2'))),textOutput("text")
        ),
        tabItem(tabName = "Correlation",
                h2("Correlation"),box(status = "warning", solidHeader = T,plotOutput("plot8"))
        ),
        tabItem(tabName = "Histogram",
                h2("Initial plots"),
                box(title="Histogram", status = "warning", solidHeader = T, plotOutput("plot1", height = 550, width=650)),
                box(title="Line Chart", status = "warning", solidHeader = T, plotOutput("plot3", height = 450, width = 650))
        ),
        tabItem(tabName = "Boxplot",selectInput(
          'vector2', h3("Select data"),
          choices = c("mpg"=2,"disp"=4,"hp"=5, "drat"=6, "wt"=7,"qsec"=8), 
          multiple=FALSE, selected=2
        ),h2("Boxplot"),
        box(status = "warning",solidHeader = T, plotOutput("plot2", height =450,width = 650))
        ),
        tabItem(tabName = "Scatterplot",h2("Scatterplot"),
                box(status = "warning",solidHeader = T, plotOutput("plot7"))
        ),
        tabItem(tabName = "PieChart",h2("Pie Chart"),
                box(status = "warning",solidHeader = T,plotOutput("plot4", height = 450, width = 400),),
                box(status = "warning",solidHeader = T,plotOutput("plot5", height = 450, width = 400),width = 5),
                plotOutput("plot6"))
      )
    )
  )
)

server <- function(input, output) {
  d <- read.csv(file="https://gist.githubusercontent.com/seankross/a412dfbd88b3db70b74b/raw/5f23f993cd87c283ce766e7ac6b329ee7cc2e1d1/mtcars.csv", 
                head=T)
  output$stats <- renderTable({ 
    data.frame(cbind(c("Vector Length","Minimum","Maximum","Mean","Standard Deviation",
                       "Median"),
                     c(length(d[seq_len(input$slider),as.numeric(input$vector)]),
                       min(d[seq_len(input$slider),as.numeric(input$vector)]),
                       max(d[seq_len(input$slider),as.numeric(input$vector)]),
                       mean(d[seq_len(input$slider),as.numeric(input$vector)]),
                       sd(d[seq_len(input$slider),as.numeric(input$vector)]),
                       median(d[seq_len(input$slider),as.numeric(input$vector)]))))})
  
  output$print2 <- renderPrint({
    print("Data")
    str(d)
  })
  
  output$data <- renderTable({d})
  
  output$text <- renderText({"Data about 32 cars from the Motor Trend US magazine from 1974, showing their specifications"})
  
  output$print1 <- renderPrint({
    print("Quantiles")
    quantile(d[seq_len(input$slider),as.numeric(input$vector)])
  })
  
  output$plot1 <- renderPlot({
    data <- d[seq_len(input$slider),as.numeric(input$vector)]
    hist(data, col="light blue", border = "darkblue", xlab = "Cars", ylab = "Count")
    grid()
  })
  
  output$plot2 <- renderPlot({
    data <- d[,as.numeric(input$vector2)]
    boxplot(data ~ mtcars$cyl, main = "Cylinders vs Selected Data", col = "Darkgreen", xlab = "Number of cylinders", ylab = "Data")
  })
  
  output$plot3 <- renderPlot({
    data <- d[seq_len(input$slider),as.numeric(input$vector)]
    plot(data, col='brown2', type="b", xlab = "Cars", ylab = "Data")
  })
  
  gears <- c(sum(d$gear==3), sum(d$gear==4), sum(d$gear==5))
  cylinders <- c(sum(d$cyl==4), sum(d$cyl==6), sum(d$cyl==8))
  automatic <- c(sum(d$am==0), sum(d$am==1))
  
  gears_percent <- round(100 * gears / 32, 1)
  cylinders_percent <- round(100 * cylinders / 32, 1)
  automatic_percent <- round(100 * automatic / 32, 1)
  
  output$plot4 <- renderPlot({
    pie(gears,
        labels = gears_percent,
        radius = 1, col = c("brown2","light green","cadetblue1"), main = "Distribution of cars based on gears (C-3,Z-4,N-5)")
  })
  
  output$plot5 <- renderPlot({pie(cylinders, 
                                  labels = cylinders_percent,adius = 1,col = c("brown2","light green","cadetblue1"), main = "Distribution of Cars based on Cylinders(4-4,6-6,8-8)" )
  })
  
  output$plot6 <- renderPlot({pie(automatic,
                                  labels = automatic_percent,
                                  radius = 1,col = c("light green","cadetblue1"), main = "Proportion(Manual-Automatic)" )
  })
  
  output$plot7 <- renderPlot({
    data <- d[,as.numeric(input$vector)]
    plot(d$wt,data, main ="scatter plot",col='darkslateblue', pch=4,xlab = "Weight", ylab = "Data", lwd=1, cex=2)
    
  })
  M = cor(mtcars)
  
  output$plot8 <- renderPlot({
    corrplot(M)
    
  })
}

shinyApp(ui, server)