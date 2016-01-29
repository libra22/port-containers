library(shiny)
library(rCharts)
library(markdown)
library(rmarkdown)

kargo <- read.csv("kargo2.csv")
kargo$PORT <- as.character(kargo$PORT)
kargo$TONNAGETYPE <- as.character((kargo$TONNAGETYPE))
portlist <- sort(unique(kargo$PORT),decreasing = FALSE)
typelist <- sort(unique(kargo$TONNAGETYPE),decreasing = FALSE)
yearlist <- sort(unique(kargo$YEAR),decreasing = FALSE)


fluidPage(
  titlePanel("Port Container Information"),
  hr(),

  tabsetPanel(
    tabPanel("Introduction",
       includeMarkdown("usermanual.md")
    ),
    tabPanel("Tonnage Type",
       br(),
       sidebarLayout(
          sidebarPanel(
             selectInput(inputId = "ttport",label = "Select Port", choices = portlist, selected=NULL)
          ),
          mainPanel(
              showOutput("ttplot","polycharts"),
              showOutput("tt2plot","polycharts")
          )
       )
    ), #tabpanel1

    tabPanel("Compare Port",
       br(),
       sidebarLayout(
          sidebarPanel(
             selectInput(inputId = "cpport1",label = "Select Port", choices = portlist, selected=portlist[1]),
             selectizeInput(inputId = "cpport2",label = "Select Port", choices = portlist, selected=portlist[2]),
             selectInput(inputId = "cptype",label = "Select Type", choices = typelist, selected=typelist[1])
          ),
          mainPanel(
             showOutput("cpplot","polycharts")
          )
       )
    ), #tabpanel1

    tabPanel("Compare Type by Year",
       br(),
       sidebarLayout(
          sidebarPanel(
             selectInput(inputId = "ctyyear",label = "Select Year", choices = yearlist, selected=yearlist[1]),
             selectInput(inputId = "ctytype",label = "Select Type", choices = typelist, selected=typelist[1]),
             checkboxInput(inputId="chkalltype",label="Show All Types",value=FALSE),
             checkboxInput(inputId="chkfacet",label="Show As Separated Graph",value=FALSE)
          ),
          mainPanel(
             showOutput("ctyplot","polycharts")
          )
       )
    ), #tabpanel1

    tabPanel("View Total and Average Data",
       br(),
       sidebarPanel(
         selectInput(inputId = "agtype",label = "Select Type", choices = typelist,selected = typelist[1]),
         radioButtons(inputId="rbag", label = "Select Aggregate Type", choices = c("Total","Average"), selected = NULL, inline = FALSE, width = NULL)
       ),
       mainPanel(
         showOutput("aggplot","polycharts")
       )
    )
  ) #navlistpanel
) #fluidpage
