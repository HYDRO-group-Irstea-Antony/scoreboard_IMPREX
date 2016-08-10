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
        uiOutput("Location")
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
          # p("Plot a score over lead times for one or more locations"),
          plotOutput("seriesPlot") ,
          
          uiOutput("ScoreTypeSingle"),
          #output pdf
          wellPanel(
            h4("Save Plot") ,
            # sidebarPanel(
            checkboxInput('save', 'save your Plot?', FALSE),
            conditionalPanel(
            condition = "input.save == true",
            br(),
              downloadButton('downloadMainPlot')
            )
          )
        ),
        tabPanel(
          "Panel plots",
          # h4("Select and filter data to create "),
          # p("Plot scores by selected location(s)"),
          plotOutput("facetPlot"),

          uiOutput("AllScoreTypes"),
          uiOutput("ScoreType"),
          
          #output pdf
          wellPanel(
            h4("Save Plot") ,
            checkboxInput('savePP', 'save your Panel Plot?', FALSE),
            conditionalPanel(
              condition = "input.savePP == true",
              br(),
              textInput("pngname", "Filename", "my.png"),
              downloadButton("downloadPanelPlot", "Download File")
            )
          )
        ),
        
        tabPanel(
          "Summary",
          h4("Summary of selected values"),
          p(""),
          verbatimTextOutput("summary")
        ),
        
        # TODO define and test RDS, possibly CSV/TXT file uploads
        tabPanel(
          "Compare Skill Scores",
          h4("Compare only skill scores"),
          p("")
          
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
