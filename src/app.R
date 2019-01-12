################################################################
#**************************************************************#
#--------------------------------------------------------------#
######### Dissertation Artifact: Nishad Abdul Latheef ##########
##-------------------- Shiny Application ---------------------##
#--------------------------------------------------------------#
#**************************************************************#
################################################################

#Command used to extract and add readxml
#install.packages("readxml")
library(readxl)

#Command used to extract and add tidyverse
#install.packages("tidyverse")
library(tidyverse)

#Command used to extract and add knitr
#install.packages("knitr")
library(knitr)

#Command used to add ggplot2
library(ggplot2)

#Command used to extract and add lubridate
#install.packages("lubridate")
library(lubridate)

#Command used to extract and add plyr
#install.packages("plyr")
library(plyr)

#Command used add dplyr 
library(dplyr)

#Command used to extract and add package arules
#install.packages("arules")
library(arules)

#Command used to extract and add arulesViz
#install.packages("arulesViz")
library(arulesViz)

#Command used to extract and add shiny
#install.packages("shiny")
library(shiny)

#Command used to extract and add shinythemes
#install.packages("shinythemes")
library(shinythemes)

#The csv file is loaded into a variable Tran
Tran <- read.transactions('transactions_output.csv', 
                          format = 'basket', sep=',')

#Defining the UI of the app
ui = tagList( 
  
  #Defining the Navigation Page
  navbarPage(
    #mentioning the theme
    theme = shinytheme("united"),
    
    "Market Basket Analysis",
    
    #defining Home page
    tabPanel("Home",
             icon = icon("glyphicon glyphicon-home",
                         lib = "glyphicon"),
             #Defining the side bar panel
             sidebarPanel(
               #br() gives required space
               br(),
               br(),
               br(),
               strong(h3("Master of Science",
                         align = "center")),
               h3("in",align = "center"),
               h3("Data Analytics",align = "center"),
               #Align allows the centre the paragraph
               br(),
               br(),
               br(),
               br(),
               br(),
               h2("DUBLIN BUSINESS SCHOOL",
                  align = "center"),
               h4("Dublin, Ireland",align = "center"),
               br(),
               br(),
               br(),
               #Inserting the image
               HTML('<center><img src="DBS.png"
                    height = "150" width="400" ></center>'),
               br(),
               br()
               
             ),
             #Defining the main panel
             mainPanel(
               
               br(),
               br(),
               br(),
               br(),
               h2("A Shiny Application", align = "center"),
               br(),
               h4("as a part of research on", align = "center"),
               br(),
               br(),
               strong( h1("Developing A Recommender System 
                          for Customers Using 
                          Apriori Algorithm in Market Basket Analysis"
                          ,align = "center")),
               br(),
               
               br(),
               HTML('<center><img src="Rs.png" height = "150" 
                    width="200" ></center>'),
               
               br(),
               br(),
               
               h3("Nishad Abdul Latheef",align = "right"),
               h3("10382242",align = "right")
               
               
               )
             
    ),
    #Defining the model page
    tabPanel("The Model", 
             icon = icon("glyphicon glyphicon-shopping-cart"
                         ,lib = "glyphicon"),
             #Defining the side bar panel
             sidebarPanel(
               #input for selecting the method of graphs
               radioButtons("meth", "Method Type:",
                            c("Scatter Plot" = "SPlot",
                              "Two-Key Plot" = "TKPlot",
                              "Grouped Plot" = "GPPlot",
                              "Graph Plot" = "GPlot",
                              "Parallel Coordinate Plot"= "PCPlot")),
               
               br(),
               
               #Slider inputs for confidence and support
               sliderInput(inputId = "conf",
                           label = "Confidence:",
                           min = 0,
                           max = 1,
                           value = 0.8),
               sliderInput(inputId = "supp",
                           label = "Support:",
                           min = 0,
                           max = 0.0025,
                           value = 0.001, step = 0.00001)
               
             ),
             #Defining the main panel
             mainPanel(
               
               tabsetPanel(type = "tabs",
                           tabPanel("Association Rules", 
                                    tableOutput("table"),
                                    icon = icon("table")),
                           tabPanel("Summary", 
                                    verbatimTextOutput("summary"),
                                    icon = icon("list-alt")),
                           tabPanel("Visualisations", 
                                    plotOutput("plot"),
                                    icon = icon("bar-chart-o"))
               )
             )
    ),
    
    #defining the About page
    tabPanel("About", 
             icon = icon("glyphicon glyphicon-user"
                         ,lib = "glyphicon"),
             #Defining the side bar panel
             sidebarPanel(
               br(),
               br(),
               br(),
               strong(h3("Master of Science"
                         ,align = "center")),
               h3("in",align = "center"),
               h3("Data Analytics",align = "center"),
               br(),
               br(),
               br(),
               br(),
               br(),
               h2("DUBLIN BUSINESS SCHOOL"
                  ,align = "center"),
               h4("Dublin, Ireland",align = "center"),
               br(),
               br(),
               br(),
               HTML('<center><img src="DBS.png"
                    height = "150" width="400" ></center>'),
               br(),
               br()
               
               
             ),
             #Defining the main panel
             mainPanel(
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               h3("The main aim of this research deals
                    with building a recommender system 
                  that understand the purchase behaviour 
                  of the customers using transactional
                  data in a scan and go retail store."
                  , align = "center"),
               br(),
               br(),
               br(),
               br(),
               br(),
               h2(tags$a(
                 href="https://www.linkedin.com/in/nishad-abdul-latheef-8136ab142/", 
                         "Nishad Abdul Latheef"), align = "center"),
               
               br(),
               br(),
               h2("Email: 10382242@mydbs.ie",align = "center"),
               br(),
               br(),
               br(),
               br(),
               br(),
               h3("THANK YOU FOR VISITING..!!",align = "center")
               )
             )
  )
)

#Defining the server
server<- function(input, output) {
  
  m <- reactive({
                  #If-else ladder to retrive the value of radio button
                  if(input$meth == "SPlot")
                  {meth <- "scatterplot"}
    
                  else if(input$meth == "TKPlot")
                  { meth <- "two-key plot"}
    
                  else if(input$meth == "GPPlot")
                  { meth <- "grouped"}
    
                  else if(input$meth == "GPlot")
                  { meth <- "graph"}
    
                  else if(input$meth == "PCPlot")
                  { meth <- "paracoord"}
    
                  else { meth <- "scatterplot"}
    
                  #return the value of meth
                  return(meth)
               })

  #Generate top rules
  output$table <- renderTable({
                  
                  #Retiving slider input values from UI to c and s
                  c <- input$conf
                  s <- input$supp
              
                  #The association rules are generated 
                  #using apriori function
                  Generated_Rules <- apriori(Tran, 
                                             parameter = list(supp=s,
                                                              conf=c,
                                                             maxlen=5))
                  #Converting the rules into dataframe
                  gr <- as( Generated_Rules, "data.frame") 
                  #Getting the top 40 rows
                  head(gr,40)
  })
  
  
  # Generate the summary of the generated rules
  output$summary <- renderPrint({
    
                    #Retiving slider input values from UI to c and s
                    c <- input$conf
                    s <- input$supp
                    
                    #The association rules are generated 
                    #using apriori function
                    Generated_Rules <- apriori(Tran, 
                                               parameter = list(supp=s,
                                                                conf=c,
                                                              maxlen=5))
                    #Getting the summary of the rules
                    summary( Generated_Rules)
  })
  
 
  
  
  #Generate visualisations
  output$plot <- renderPlot({
                
                  #Retiving slider input values from UI to c and s
                  c <- input$conf
                  s <- input$supp
                  
                  #The association rules are generated 
                  #using apriori function 
                  Generated_Rules <- apriori(Tran, 
                                             parameter = list(supp=s,
                                                              conf=c,
                                                              maxlen=5))
                  #Calling the function m to variable mf
                  mf <- m()
                  
                  #If-else ladder for different menthods
                  if(mf == "graph")
                  { Generated_Rules <- head(Generated_Rules,
                                            n=10, by="lift")}
                  
                  else  if(mf == "paracoord")
                  { Generated_Rules  <- head(Generated_Rules,
                                             n=30, by="lift")}
                  
                  #Plotting the graphs with relavent methods
                  plot(Generated_Rules, method= m())
    
  })
  
  
}


#Command to run the shiny app
shinyApp(ui, server)



################################################################
#----------------------------The End---------------------------#
################################################################
