#IMPREX Scoreboard v0.1
setwd("~/R/shinysb1/sbdtest1")
readRenviron("~/R/shinysb1/.Renviron")
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

# TODO move to server; clean up DB and 3NF tables; speed up init
tmpCaseStudy <-
  filter(tbl(db, "tblInterface"),
         ObjectName == "Case Study" & LanguageID == RElanguage)
ctlCaseStudy <- collect(tmpCaseStudy)

tmpForecastType <-
  select(tbl.scores, forecastType)
ctlForecastType <- arrange_(distinct(collect(tmpForecastType, n=Inf)))

shinyUI(
  fluidPage(
    img(src = "imprex.png", height = 100),
    titlePanel("Scoreboard"),
    
    fluidRow(
      column(
      4,
      wellPanel(
        selectInput(
        "rtnCaseStudy",
        "Case Study:",
        c(
          "Central European Rivers" = 1,
          "South-East French Catchments" = 2,
          "Júcar River Basin (Spain)" = 3,
          "Lake Como Basin (Italy)" = 4,
          "Upper Umeälven River (Sweden)" = 5,
          "Segura River Basin (Iberian Peninsula)" = 6,
          "The Llobregat River Basin (Spain)" = 7,
          "The Messara Valley (Crete)" = 8,
          "Test Case Study (LC)" = 9
        ),
        selected = 1
        ),
        selectInput(
          "rtnForecastSystem",
          "System:",
          c("ECMWF EFAS" = 1,
            "E-HYPE" = 2,
            "System 3" = 3,
            "ECMWF LS Seasonal month" = 4,
            "ECMWF EDMD Seasonal month" = 5,
            "ECMWF LS Seasonal week" = 6,
            "ECMWF EDMD Seasonal week" = 7
          ),
          selected = 2
        )
      ),
      
      wellPanel(
        h4("Filter Criteria"),
        uiOutput("Location"),
        uiOutput("ModelVariable"),
        uiOutput("ForecastType"),
        uiOutput("ScoreType")
      )
    ),

    column(
      8,
      tabsetPanel(
        type = "tabs",

        tabPanel(
          "Plot",
          h4("Plot"),
          p("Plot a score over lead times for one or more locations"),
          plotOutput("seriesPlot") ,
          
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
          h4("Select and filter data to create "),
          p("Plot scores by selected location(s)"),
          plotOutput("facetPlot"),
          uiOutput("AllScoreTypes"),
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
          "Upload",
          h4("Add score data to the IMPREX database"),
          p("")

        )
      )
    )
    )
)
)
