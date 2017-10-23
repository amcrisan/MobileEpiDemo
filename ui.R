library(shiny)
library(shinydashboard)
library(shinythemes)


header <- dashboardHeader(
  title = "Zombie Outbreak",
  titleWidth = 450
)

body<-dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  #menu items
  box(title="Graph Options",
      status="warning",
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
        tabPanel(title= "Filter by",
                 hr(),
                 div(HTML("You can <em> emphasize data </em> by checking off items below. For example, if you check off 'Male' then male patients will be emphasized in the data set.")),
                checkboxGroupInput("filterSex",label="Sex",choices=c("Male"="M","Female" = "F")),
                checkboxGroupInput("filterStatus",label="Infection Status",choices=c("Sick"="Sick","Exposed but well" = "ExposedButWell")),
                checkboxGroupInput("filterExposure",label="Exposures",choices=c("Bite"="Bite",
                                                                                 "Droplet" = "Droplet",
                                                                                 "Poop"="Faeces",
                                                                                 "Household Contact" = "Household",
                                                                                 "Intercourse"="Sex",
                                                                                 "Shared Meal" = "Shared Meal")),
                sliderInput("filterAge", label ="Age Range", min = 0,max = 55, value = c(0, 56))
                 ),
        type="pills"
      )
  ),
  fluidRow(
    div(HTML("<strong>Description of the plot:</strong>")),
    textOutput("textDesc"),
    plotOutput("timeline",height="450px",hover = hoverOpts("plot_click")),
    uiOutput("click_info"),
    #uiOutput("hover_info")
    br(),
    plotOutput("exposureDat",height="250px")
  )
)
dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body,
  skin="black"
)
  
