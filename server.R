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
# library(DT)

db <- src_postgres(
  dbname = REdbname,
  host = REhost,
  port = REport,
  user = REuser,
  password = REpassword
)

tbl.scores <- tbl(db, "tblScores")
tbl.interface <- tbl(db, "tblInterface")
tbl.forecastsetup <- tbl(db, "tblForecastSetup")
# totes <- cbind(tbl.scores$forecastSetup, tbl.forecastsetup)

# enc2utf8
# defined within the interface table
tmpCaseStudy <-
    filter(tbl.interface,
         ObjectName == "Case Study" & LanguageID == RElanguage)
ctlCaseStudy <- collect(tmpCaseStudy)

tmpSystem <-
  filter(tbl.interface,
         ObjectName == "System" & LanguageID == RElanguage)
ctlSystem <- collect(tmpSystem)

# t.forecast.setup <- collect(db, tbl.forecastsetup)

tmpSetup <- select(tbl.forecastsetup, ID, forecastSetup)
ctlSetup <- collect(tmpSetup)

# tmpForecastSetup <-
#   select(tbl.scores, forecastSystem)
# ctlForecastSetup <- arrange_(distinct(collect(tmpForecastSetup, n=Inf)))

# directly from the score table
tmpScoreType <-
  select(tbl.scores, scoreType)
ctlScoreType <- arrange_(distinct(collect(tmpScoreType, n=Inf)))

tmpModelVariable <-
  select(tbl.scores, modelVariable)
ctlModelVariable <- arrange_(distinct(collect(tmpModelVariable, n=Inf)))

# was filtering by multiple datapackageGUIDs before, not necessary now?
tmpLocationName <- 
  distinct(select(tbl.scores, locationID, caseStudy))
ctlLocationName <- collect(tmpLocationName)
ctlLocationName <-
  arrange_(ctlLocationName, "caseStudy", "locationID")

# # pdf-generating function:
# makePdf <- function(filename, plotObject){
#   pdf(file = filename)
#   g <- plotObject
#   
#   # plot(cars)
#   dev.off()
# }
# 
# makePng <- function(filename, pngObject){
#   png::writePNG(pngObject)
#   # pdf(file = filename)
#   g <- pngObject
#   # plot(cars)
#   dev.off()
# }

shinyServer(function(input, output, session) {
  
# trying out deps on caseStudy
  observe({
    tmpLocationName <- 
      distinct(select(tbl.scores, locationID, caseStudy))
    ctlLocationName <- collect(tmpLocationName)
    ctlLocationName <-
      arrange_(ctlLocationName, "caseStudy", "locationID")
  })
  
  # define Filters
  output$CaseStudy <- renderUI({
    if(is.null(ctlCaseStudy))
      return()
    CaseStudy <- setNames(ctlCaseStudy$ObjectInteger, ctlCaseStudy$ObjectItemName)
    # CaseStudy = paste(ctlCaseStudy$ObjectItemName, "\"=\"", ctlCaseStudy$ObjectInteger ) # not the way
    selectInput("rtnCaseStudy", "Case Study (DB):", choices = CaseStudy, multiple = F)
  })

  output$System <- renderUI({
    if(is.null(ctlSystem))
      return()
    System <- ctlSystem$ObjectItemName
    selectInput("rtnForecastSystem", "System:", choices = System, multiple = F, selected = "E-HYPE")
  })
  
  # note - set choices=character(0) to reset selections 
  
  output$Location <- renderUI({
    if (is.null(ctlLocationName))
      return()
    # if(!is.null(output$CaseStudy))
    # {
    #   Location <- filter(ctlLocationName, caseStudy == output$CaseStudy)
    #   Location <- ctlLocationName$locationID # hiding dataPackageGUID, can use on filter
    # }
    # else {
      Location <- ctlLocationName$locationID # hiding dataPackageGUID, can use on filter
    # }
    selectInput("rtnLocid","Location: ", choices = structure(Location), multiple=T)
  })
  
# setup ex Bias Corr 1 (was forecastType)
  output$Setup <- renderUI({
    if(!is.null(ctlSetup)) {
      # Setup <- setNames(ctlSetup$ID, ctlSetup$forecastSetup) # ID, value
      Setup <- c(ctlSetup$forecastSetup) #value only
      selectInput("rtnForecastType","Forecast Setup: ", 
                  choices = structure(Setup), 
                  multiple=F)
    }
  })

  output$ScoreType <- renderUI({
    if(!is.null(ctlScoreType)) {
      # ScoreType <- structure(ctlScoreType)
      ScoreType <- structure(ctlScoreType$scoreType)
    }
    selectInput("rtnAllScoreTypes",
                "Score Type(s)", 
                choices = ScoreType, 
                multiple=T,
                selected = c("RMSE Skill Score",
                             "Brier Skill Score",
                             "CRPS Skill Score"))
  })
  
  output$ScoreTypeSingle <- renderUI({
    if(!is.null(ctlScoreType)) {
      ScoreType <- structure(ctlScoreType$scoreType)
    }
    selectInput("rtnScoreType","Score Type", choices = ScoreType, multiple=F)
  })

  output$ModelVariable <- renderUI({
    if (is.null(ctlModelVariable))
      return()
    ModelVariable <- ctlModelVariable$modelVariable
    selectInput("rtnModelVariable","Variable: ", choices = structure(ModelVariable), multiple=F)
  })
  

# >>>>>>> b1e392d... uiOutput / renderUI partout
  filtInput <- reactive({
    validate(
      need(input$rtnLocid != "", "Please select at least one location")
    )
    
    if (length(input$rtnLocid) == 1) {
      remote <- filter(tbl.scores,
                       locationID == input$rtnLocid)
    }
    else if (length(input$rtnLocid) > 1){
      remote <- filter(tbl.scores,
                       locationID %in% input$rtnLocid)
    }
    remote <- filter(remote,
      caseStudy == input$rtnCaseStudy &
      forecastSystem == input$rtnForecastSystem  & # ehype
      # scoreNA == FALSE & #more like "bad data" now, contains -Infinity too
      modelVariable == input$rtnModelVariable &
      forecastType == input$rtnForecastType & # Bias Corr 1
      scoreType == input$rtnScoreType
    )
    getit <- structure(collect(remote)) #database hit
  }) #end reactive
  
  filtSkillScores <- reactive({
    validate(
      need(input$rtnLocid != "", "Please select at least one location and one or more scores (below)")
    )
    
    list.skill.scores <- input$rtnAllScoreTypes

    if (length(input$rtnLocid) == 1) {
      remote <- filter(tbl.scores,
                       locationID == input$rtnLocid)
    }
    else if (length(input$rtnLocid) > 1){
      remote <- filter(tbl.scores,
                       locationID %in% input$rtnLocid)
    }
    remote <- filter(remote,
                     caseStudy == input$rtnCaseStudy &
                       forecastSystem == input$rtnForecastSystem  &
                       # scoreNA == FALSE & #more like "bad data" now, contains -Infinity too
                       modelVariable == input$rtnModelVariable &
                       forecastType == input$rtnForecastType &
                       scoreType %in% list.skill.scores
    )
    getit <- structure(collect(remote)) #database hit
  }) #end reactive
  
  output$summary <- renderPrint({
    dataset <- filtInput()
    dataset <- within(
      dataset, rm("row.names", "datePartUnit", "forecastSystem", "forecastRange", "caseStudy",  "leadtimeUnit", "leadtimeValue")
      )
    summary(dataset)
  })
  
  output$seriesPlot <- renderPlot({
    if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      plot(1, 1, col = "white")
      text(1, 1,"Select one or more data elements from the Filter to begin")
    }
    else if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      text(1, 1, "filtInput() was empty, try a different combo")
    } else {
      # have data
      filtered.input <- filtInput() # debug rename in summarySE
      loc.sum <- summarySE(
          filtered.input,
          measurevar = "scoreValue",
          groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"), 
          na.rm = TRUE
        )
      loc.sum$locationID <- as.factor(loc.sum$locationID)
      na.count <- sum(filtered.input$scoreNA) 
    } # end else
    
    if (nrow(filtInput()) == 0) {
      plot(1, 1, col = "white")
      text(1, 1, "The database doesn't have information on this combination of variables (yet)")
    } else {
      pd <- position_dodge(0.2)
      ggplot(loc.sum,
             aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
        geom_line() +
        geom_point(aes(color = locationID)) + 
        xlab("Lead Times") + 
        ylab(paste("SCORE: ", input$rtnScoreType))
    } 
  }) 
  
  output$facetPlot <- renderPlot({
    if (nrow(filtSkillScores()) == 0 || length(filtSkillScores()) == 0) {
      plot(1, 1, col = "white")
      text(1,1,"Select one or more data elements from the Filter to begin")
    }
    else if (nrow(filtSkillScores()) == 0 || length(filtSkillScores()) == 0) {
      text(1, 1, "filtInput() was empty, try a different combo")
    } else {
      filtered.input <- filtSkillScores() # debug rename in summarySE
      loc.sum <- summarySE(
          filtered.input,
          measurevar = "scoreValue",
          groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"), # "locationID", "leadtimeValue"
          na.rm = TRUE
        )
      loc.sum$locationID <- as.factor(loc.sum$locationID)
      na.count <- sum(filtered.input$scoreNA) # should report to user since value hidden by summarySE()
    }

    if (nrow(filtSkillScores()) == 0) {
      plot(1, 1, col = "white")
      text(1, 1, "The database doesn't have information on this combination of variables (yet)")
    } else {

      # pd <- position_dodge(0.2)
      loc.count <- length(loc.sum$locationID)

      ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
        geom_point(aes(color = locationID)) +
        # facet_wrap(scoreType ~ locationID, nrow = loc.count) +
        facet_grid(scoreType ~ locationID) + #margin = TRUE
        geom_hline(aes(yintercept=0), colour="grey", linetype="dashed") +
        xlab("Lead Times") +
        ylab("Scores")
    }
    
  })
  
# # main plot
#   output$downloadMainPlot <- downloadHandler(
#     
#     filename = function() { paste(input$dataset, '.png', sep='') },
#     content = function(file) {
#       ggsave(file, plot = "facetPlot", device = "png")
#     }
#   )  
  
}) # end shinyServer

