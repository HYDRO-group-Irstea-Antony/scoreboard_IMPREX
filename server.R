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

tmpModelVariable <-
  filter(tbl(db, "tblInterface"),
         ObjectName == "Model Variable" & LanguageID == RElanguage)
ctlModelVariable <- collect(tmpModelVariable)

tmpScoreType <-
  select(tbl.scores, scoreType)
ctlScoreType <- arrange_(distinct(collect(tmpScoreType, n=Inf)))

tmpLocationName <-
  distinct(select(tbl.scores, locationID, dataPackageGUID))
ctlLocationName <- collect(tmpLocationName)
ctlLocationName <-
  arrange_(ctlLocationName, "dataPackageGUID", "locationID")


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
  
  # define Filters
  output$ScoreType <- renderUI({
    if(!is.null(ctlScoreType)) {
      # ScoreType <- firstFitler()$scoreType
      ScoreType <- structure(ctlModelVariable)
      # Location=unique(initial.query()$locationID)
    }
    selectInput("scoreType","Score Type", choices = structure(ScoreType), multiple=T)
  })
  
  output$AllScoreTypes <- renderUI({
    if(!is.null(ctlScoreType)) {
      ScoreType <- structure(ctlModelVariable)
      # Location=unique(initial.query()$locationID)
    }
    selectInput("scoreType","Score Type", 
                choices = structure(ScoreType), 
                multiple=TRUE,
                selected = c("RMSE Skill Score",
                           "Brier Skill Score",
                           "CRPS Skill Score")
    )
  })
  # selectInput("rtnAllScoreTypes",
  #             multiple = TRUE,
  #             "Score(s):",
  #             c(sort.int(ctlScoreType$scoreType)),
  #             selected = c("RMSE Skill Score",
  #                          "Brier Skill Score",
  #                          "CRPS Skill Score"
  #             )
              
  # selectInput("rtnLocid",
  #             multiple = TRUE,
  #             "Location(s):",
  #             c(ctlLocationName$locationID
  #             )),
  
  # selectInput("rtnModelVariable",
  #             "Variable:",
  #             c(sort.int(ctlModelVariable$ObjectItemName)),
  #             selected = "Streamflow"
  # ),
  # selectInput("rtnForecastType",
  #             "Forecast System:",
  #             c(ctlForecastType$forecastType)
  # ),
  
  
  
  
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
      forecastSystem == input$rtnForecastSystem  &
      # scoreNA == FALSE & #more like "bad data" now, contains -Infinity too
      modelVariable == input$rtnModelVariable &
      forecastType == input$rtnForecastType &
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

