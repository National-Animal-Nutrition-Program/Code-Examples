# lessons https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
# https://cfss.uchicago.edu/shiny.html
# https://yvyys.shinyapps.io/BirthdayApp/
  
library(shiny)
library(rhandsontable)
library(ggplot2)
library(plotly)
library(DT)
library(shinyjs)



jscode <- "shinyjs.refresh = function() { history.go(0); }"

title <-  tags$a(
           href = 'https://animalnutrition.org/',
           tags$img(src="NANPlogo.png"), height = "0.5", width ="0.5",
           strong("National Animal Nutrition Program (NANP)") ) #style = "color:blue"



  shinyUI(fluidPage(
    
    
    # Application title 1
   
    headerPanel(title= title), 
    
    pageWithSidebar(mainPanel(h1(strong("Predictions of amino acid (AA) requirements for Broilers"), align = "left"),
                              h4(strong("Why should I use this Web App?"), align = "left", style = "color:#239023"),
                              h5("1. Estimate AA requirements of broiler chickens.", align = "left", style = "color:#000000"),
                              h5("2. Compare empirical and mechanistic models.", align = "left", style = "color:#000000"),
                              h5("3. Visualize AA requirements according to sex and line.", align = "left", style = "color:#000000"),
                              h5("4. Download AA requirements in a CSV file for use in ration formulation programs.", align = "left", style = "color:#000000"),
                              h5(strong("Reference: Sakomura, N.K. et al., 2015. The Journal of Applied Poultry Research, 24, 267-282. Contact: sakomura@unesp.br"), align = "left", style = "color:#000000"),
                              h5("Web application developed by V.L. Daley, M.P. Reis, L.V.F.M. Carvalho et al. (2019). Contact: veridi7@vt.edu", align = "left", style = "color:#000000")
                              ),
                    
                    
                   
   sidebarPanel(
  
   
   h5(strong("Required Information"), style = "color:#e39d19", align = "center"),  
   
   
   selectInput("text1", label =  "Choose a Line:",
               choices = list("Ross", "Cobb", "Hubbard"), selected = "Ross"), 
   
   selectInput("text8", label =  "Choose an USA Category:",
               choices = list("Early-developing", "Late-developing"), selected = "Late-developing"), 
   
   selectInput("text2", label =  "Choose a Sex:",
               choices = list("Female", "Male"), selected = "Male"), 
   
   #sliderInput("text3", label = ("Choose an Age (day)"), min = 1, 
   #             max = 55, value = 1), #c(40, 60)
   
   # test 
   
   sliderInput(inputId = "text3",
               label = "Choose a Range of Age",
               min = 1,
               max = 55,
               value = c(1, 55),
               pre = "Day"),
  
   # sliderInput("slider1", "Choose Range of Percentage", 
   #            min = a_temp[1], 
   #            max = a_temp[10],
   #             value = c(a_temp[1], a_temp[10]),
   #             step = 100
   # ),
   # test 
   sliderInput("text4", label = ("Choose an Ambient Temperature (Celsius)"), min = 0, 
               max = 35, value = 32, step = 0.1), #c(40, 60)
   
   sliderInput("text5", label = ("Choose a Dietary Metabolizable Energy (Mcal/kg)"), min = 1.00, 
               max = 4.00, value = 3.04, step = 0.01), #c(40, 60)
   

   #textInput("text1", "Line (Insert: Ross, Cobb, or Hubbard)", "Cobb"),
   #textInput("text2", "Sex (Insert: female or male)", "male"),
   #textInput("text3", "Age (day)", 1),
   #textInput("text4", "Insert a Ambient Temperature (Celsius)", "32"),
   #textInput("text5", "Insert a Dietary Metabolizable Energy (Mcal/kg)", "3.00"),
   
   h5(strong("Optional Information"), style = "color:#e39d19", align = "center"),
   textInput("text6", "Insert Body Weight (kg)", ""),
   textInput("text7", "Insert Feed Intake (g/day)", ""),
   
   actionButton("update", "Update Table"),
   actionButton("reset", "Clear"), 
   
   h5("Model developed by N.K. Sakomura et al. (2018), FCAV/UNESP, BR",style = "color:#239023")
   
   
   
   ),
  
   #mainPanel(tableOutput("table1"))

    mainPanel(
      
     tabsetPanel(type = "tabs",
                  
     tabPanel("Input values",
              dataTableOutput("table1")
              ),
     tabPanel("Output values",
              rHandsontableOutput("table2"), actionButton("saveBtn", "Save"), downloadButton("BroilersData.csv", "Save Predictions as csv")
     ),
    
     tabPanel("Lys",
               plotlyOutput("plot1a"),
               plotlyOutput("plot1b"),
               plotlyOutput("plot1c"),
               plotlyOutput("plot1d")),
    
     tabPanel("Met+Cys",
               plotlyOutput("plot2a"),
               plotlyOutput("plot2b"),
               plotlyOutput("plot2c"),
               plotlyOutput("plot2d")
              ),
     
     tabPanel("Thr",
               plotlyOutput("plot3a"),
              plotlyOutput("plot3b"),
              plotlyOutput("plot3c"),
              plotlyOutput("plot3d")
              ), 
     tabPanel("Val",
               plotlyOutput("plot4a"),
               plotlyOutput("plot4b"),
               plotlyOutput("plot4c"),
               plotlyOutput("plot4d")
              
              ),
    tabPanel("Ile",
               plotlyOutput("plot5a"),
               plotlyOutput("plot5b"),
               plotlyOutput("plot5c"),
               plotlyOutput("plot5d")
             ),
    tabPanel("Trp",
               plotlyOutput("plot6a"),
               plotlyOutput("plot6b"),
               plotlyOutput("plot6c"),
               plotlyOutput("plot6d")
             ),
    tabPanel("Growth Curve",
             plotlyOutput("plot7"),
             plotlyOutput("plot7a"),
             plotlyOutput("plot7b")
    )    
               
               )
     # verbatimTextOutput("event")
                           
                  
     # rHandsontableOutput("table2"), actionButton("saveBtn", "Save"), downloadButton("BroilersData.csv", "Save Predictions as csv"),
    #  plotOutput("plot1")
                  
    )
  
   
   
  )))
