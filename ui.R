#IMPREX Scoreboard v0.2
# setwd("~/R/shinysb1/sbdtest1")
readRenviron("~/R/.Renviron")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

library(shiny)
library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)

db <- src_postgres(
  dbname = REdbname,
  host = REhost,
  port = REport,
  user = REuser,
  password = REpassword
)

tbl.scores <- tbl(db, "tblScores")
tbl.dataload <- tbl(db, "tblDataLoad")
tbl.interface <- tbl(db, "tblInterface")

shinyUI(
  fluidPage(
    div(style="float:right",
        paste("Connected to ", db$info$host, " as ", db$info$user)
        
        # # checkboxInput("hideme", "Hide: ", value = FALSE),
        # # conditionalPanel("hideme==='FALSE'",
        #                  observe({
        #                    # check for session$onSessionEnded perhaps
        #                    if (!is.null(db$con)){
        #                      paste("Connected to ", db$info$host, " as ", db$info$user)
        #                    } else {
        #                      "database not connected, loading localdefault RDS file"
        #                    }
        #                    # selectInput("rtnConnec","Connected to: ", choices = c("database", "RDS file"), multiple=F)
        #                  })
        #                # )
        
    ),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel("Scoreboard"),
        img(src = "imprex.png", height = 100)
    ),
    # div(style="float:left",
    # ),
    
    fluidRow(
      column(
      4,
      wellPanel(
        uiOutput("CaseStudy"),
        uiOutput("System"),
        uiOutput("Setup")

      ),
      
      wellPanel(
        h4("Filter Criteria"),
        uiOutput("ModelVariable"),
        uiOutput("ForecastType"),
        uiOutput("Locations")
        # uiOutput("ScoreType")
      )
    ),

    column(
      8,
      tabsetPanel(id = "inTabset",
        type = "tabs",

        tabPanel(
          "Plot",
          # h4("Plot a score over lead times"),
          wellPanel(
            column(5,
                   uiOutput("ScoreTypeSingle")
                   ),
            column(3,
                   checkboxInput('save', 'Save Plot?', FALSE),
                   conditionalPanel(
                     condition = "input.save == true",
                     # br(),
                     downloadButton('downloadMainPlot')
                     )
                   ),
              hr()
            ),
          plotOutput("seriesPlot")
          
        ),
        tabPanel(
          "Panel plots",
          # h4("Select and filter data to create "),
          # explore pauses: invalidateLater(2000, session)  # 2 seconds
          #output pdf
          wellPanel(
            column(6,
                   uiOutput("ScoreTypes")

            ),
            column(2,
              checkboxInput('savePP', 'Save Plots?', FALSE),
              conditionalPanel(
                condition = "input.savePP == true",
                textInput("pngname", "Filename", "my.png"),
                downloadButton("downloadPanelPlot", "Download File")
              )
            ),
            hr()
          ),
          
          # invalidateLater(2000, session),
          plotOutput("facetPlot")
          
        ),
        
        tabPanel(
          "Summary",
          h4("Summary of selected values"),
          p(""),
          verbatimTextOutput("summary")
        ),
        
        tabPanel(
          "Compare Skill Scores",
          # h4("Compare only skill scores"),
          # p("Something to add here, controls buggy?")
          wellPanel(
            column(4,
                   p("repeated from leftbar")
                   # uiOutput("System"),
                   # uiOutput("Setup")
                   # uiOutput("System"),
                   # uiOutput("Setup"),
                   # uiOutput("ScoreTypes"), # mutiple = T
                   # uiOutput("Locations")
            ),
            column(4,
                   p("some more static stuff")
                   # uiOutput("System")
                   # uiOutput("Setup")
            ),
          br()
          )
        ),
        
        # TODO define and test RDS, possibly CSV/TXT file uploads
        tabPanel(
          "Upload",
          h4("Add score data to the IMPREX database"),
          p("")
        )
      )
    )
    )
)
)
