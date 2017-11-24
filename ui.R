library(shiny)
library(plotly)
library(rhandsontable)



load('laeknar.Rdata')
load('legudeild.Rdata')
load('gjorgaesla.Rdata')
load('op_count.Rdata')
instruments <- readRDS('instruments.rds', refhook = NULL)
print(instruments)


choice = vector("list",length(surgerycode))
names(choice) <- surgerycode
choice[1:length(surgerycode)] = c(1:length(surgerycode))

shinyUI(fluidPage(
  
  includeCSS("style.css"),
  
  titlePanel(h3("Almenna")),
  
  sidebarLayout(
    
    sidebarPanel(

      selectInput("select", label = h4("Velja aðgerð"), 
                  choices = choice, 
                  selected = 1), 
      
     
      
      htmlOutput("surgeryselected"),
      
      selectInput("aldur", label = h4("Aldur"), 
                  choices = list('Allir' = 1,'[0-25)' = 2,'[25-50)' = 3, '[50-74)' = 4, '[75-)' = 5), 
                  selected = 1),
      htmlOutput("click"), br(), br(), 
      wellPanel(
        h4("Aðgerðir fyrir röðun"), 
        actionButton("save2","Vista lækna"),
        actionButton("simulate", "Herma")
      ),
      width = 2
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        #tabPanel("Læknar", plotlyOutput("laekPlot",height = 700)), 
        #tabPanel("Legutímar", plotOutput("leguPlot"),plotOutput("legutimePlot")), 
        #tabPanel("Gjörgæslutími", plotOutput("gjorPlot"),plotOutput("gjortimePlot")),
        #tabPanel("Acute/Elective", plotOutput("acuteElectivePlot"),plotOutput("fjoldiadgerdaPlot")),
        #tabPanel("Skurðstofutímar", plotOutput("skurdPlot")), 
        #tabPanel("Skurðstofur", plotOutput("skurdStofur")),
        tabPanel("Forsendur röðunar", rHandsontableOutput("hot"),actionButton("save", "Vista forsendur röðunar",style='padding:5px; font-size:100%')),
        #tabPanel("Um aðgerðir", plotOutput("GjorPlot")),
        tabPanel("Skráð aðföng fyrir aðgerðir",rHandsontableOutput('Adfong'),actionButton("SaveInstru", "Vista aðföng")),
        tabPanel("Vaktir lækna",rHandsontableOutput("hotsd"),rHandsontableOutput("hotsd2")),
        tabPanel("Skurðstofuskipan", rHandsontableOutput("Stofur"),actionButton("SaveMaster", "Vista skurðstofuskipan",style='padding:4px; font-size:100%')),
        tabPanel("Tölfræði hermunar",plotOutput("GjorPlot"))
        #tabPanel("Skurðstofuskema"), rHandsontableOutput('hotsd'))
      )
    )
  )
))