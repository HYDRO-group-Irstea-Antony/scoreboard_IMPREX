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
library(DBI)

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
          #note js nomenclature input.inTabset is equiv to input$inTabset
          conditionalPanel("input.inTabset != 'CompareSkillScores'",
                           uiOutput("Locations")
          )
          # uiOutput("ScoreType")
        )
      ),
      
      column(
        8,
        tabsetPanel(id = "inTabset",
                    type = "tabs",
                    
                    tabPanel(
                      "Plot",
                      value="Plot",
                      # h4("Plot a score over lead times"),
                      wellPanel(
                        column(5,
                               uiOutput("ScoreTypeSingle")
                        ),
                        column(3,
                               p("note")
                               # checkboxInput('save', 'Save Plot?', FALSE),
                               # conditionalPanel(
                               #   condition = "input.save == true",
                               #   # br(),
                               #   downloadButton('downloadMainPlot') #download series
                               #   )
                        ),
                        hr()
                      ),
                      plotOutput("seriesPlot")
                      
                    ),
                    tabPanel(
                      "Panel plots",
                      value="PanelPlots",
                      # h4("Select and filter data to create "),
                      # explore pauses: invalidateLater(2000, session)  # 2 seconds
                      #output pdf
                      wellPanel(
                        column(6,
                               uiOutput("ScoreTypes")
                               
                        ),
                        column(2,
                               p("note")
                               # checkboxInput('savePP', 'Save Plots?', FALSE),
                               # conditionalPanel(
                               #   condition = "input.savePP == true",
                               #   textInput("pngname", "Filename", "my.png"),
                               #   downloadButton("downloadPanelPlot", "Download File")
                               # )
                        ),
                        hr()
                      ),
                      # invalidateLater(2000, session),
                      plotOutput("facetPlot")
                      
                    ),
                    
                    tabPanel(
                      "Summary",
                      value="Summary",
                      h4("Summary of selected values"),
                      p(""),
                      verbatimTextOutput("summary")
                    ),
                    
                    tabPanel(
                      "Compare Skill Scores",
                      value="CompareSkillScores",
                      # h2("Renders comparative plot of scores for two Systems / Setups"),
                      wellPanel(
                        column(1,
                               p(" ") # baseline
                        ),

                        column(4,
                               div(strong("Reference System:"), style = "color:blue"),
                               strong(uiOutput("ReferenceSystem"))
                               # p("select another System to compare"),
                        ),
                        column(4,
                               div(strong("Reference Forecast Setup:"), style = "color:blue"),
                               strong(uiOutput("ReferenceSetup"))
                        ),
                        column(2,
                               p("note: Baseline or \"Reference\" selections are changed in the menu on the left")
                               # ,
                               # br()
                               )
                      ),
                      wellPanel(
                        column(2,
                               p(" ") # comparison
                        ),
                        column(4,
                               # p("select another Setup to compare"),
                               uiOutput("SystemToCompare"),
                               uiOutput("ScoreTypesInBoth") #,
                               # uiOutput("LocationsAll")
                        ),
                        column(3,
                               # uiOutput("System")
                               uiOutput("SetupToCompare"),
                               uiOutput("LocationsAll")
                        ),
                        br()
                        
                      ),
                      # p("The plot below will refresh as you update reference values at left and / or comparison values above"),
                      
                      br(),
                      plotOutput("compareSkillScorePlot")
                      
                    ), # end tabPanel
                    
                    # TODO define and test RDS, possibly CSV/TXT file uploads
                    ###########################
                    ### Upload Data
                    tabPanel(
                      "Upload",
                      h4("Add unrecognized score type to IMPREX database"),
                      p(""),
                      # h3("Using renderUI and uiOutput"),
                      uiOutput("my_output_UI"),
                      textInput("mytext", ""),
                      actionButton("mybutton", "Click to add to Selections")
                    ) 
                    
        ) # end tabSet
      )
    )
  )
)
