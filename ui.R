library(shiny)
library(shinydashboard)
library(shinythemes)


header <- dashboardHeader(
  title = "Zombie Outbreak Timeline",
  titleWidth = 450
)

body<-dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  #menu items
  box(title="Graph Options",
      status="info",
      solidHeader = TRUE,
      width=NULL,
      collapsible = TRUE,
      collapsed = TRUE,
      div(
        HTML("<em><strong>Usage Tip:</strong> You can manipulate the appearance of the graphic below through the menu above</em>")
      ),
      br(),
      tabsetPanel(
        tabPanel(title="Add data",
                 hr(),
                 div(HTML("You can <em>add</em> data to the basetimeline graphic. Select what data to add using the check boxes.")),
                 checkboxGroupInput("addData",choices=c("Symptom Onset","Patient Relationships"),label=NULL)
                 ),
        tabPanel(title ="Colour by",
                 hr(),
                 div(HTML("You can <em>change the colour</em> of data points. You can only colour data using one other data type. Use the checkboxes below to select which data point the colour by.")),
                 selectInput("colorData",choices=c("Status"="status","Sex" = "Sex","Occupation"="Occupation"),label=NULL,multiple=FALSE)),
        tabPanel(title= "Filter by"),
        type="pills"
      )
  ),
  fluidRow(
    plotOutput("timeline"),
    br(),
    div(HTML("<strong>Description of the plot:</strong>")),
    textOutput("textDesc")
  )
)
dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body,
  skin="black"
)
  
