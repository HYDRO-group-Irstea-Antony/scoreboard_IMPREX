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
# library(DT)

db <- src_postgres(
  dbname = REdbname,
  host = REhost,
  port = REport,
  user = REuser,
  password = REpassword
  # encoding="utf8"
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

shinyServer(function(input, output, session) {
  
# trying out deps on caseStudy
  # location.list <- reactive({
  #   if(is.null(input$rtnCaseStudy))
  #     return()
  #   remote <- filter(remote,
  #                    caseStudy == input$rtnCaseStudy
  #   )
  #   getit <- structure(collect(remote)) #database hit
  #   # browser()
  #   return(getit$locationID)
  # })
  
  # define heirarchical Filters
  output$CaseStudy <- renderUI({
    if(is.null(ctlCaseStudy))
      return()
    CaseStudy <- setNames(ctlCaseStudy$ObjectInteger, iconv(ctlCaseStudy$ObjectItemName, "UTF-8")) # to get the Swedish / spanish chars
    selectInput("rtnCaseStudy", 
                paste("Case Study: (",length(unique(ctlCaseStudy$ObjectItemName)), ")"), choices = CaseStudy, multiple = F)
  })

  output$System <- renderUI({
    x <- input$rtnCaseStudy
    browser()
    if (any(
      is.null(x)
    )) 
      return("System:")
    # select ctlSystem$ObjectItemName, 
    System <- ctlSystem$ObjectItemName
    selectInput("rtnForecastSystem", "System:", choices = System, multiple = F, selected = "E-HYPE")

    # if(is.null(ctlSystem))
    #   return()
    # System <- ctlSystem$ObjectItemName
    # selectInput("rtnForecastSystem", "System:", choices = System, multiple = F, selected = "E-HYPE")
  })
  
  #used in Comp Skill Score only
  output$System2 <- renderUI({
    if(is.null(ctlSystem))
      return()
    System <- ctlSystem$ObjectItemName
    selectInput("rtnForecastSystem", "System:", choices = System, multiple = F, selected = "E-HYPE")
  })
  
    # note - set choices=character(0) to reset selections 
  output$Locations <- renderUI({
    if (is.null(ctlLocationName))
      return()
    # if(!is.null(output$CaseStudy))
    # {
    #   Location <- filter(ctlLocationName, caseStudy == output$CaseStudy)
    #   Location <- ctlLocationName$locationID 
    # }
    # else {
      Locations <- ctlLocationName$locationID
    # }
    selectInput("rtnLocid","Location(s): ", choices = structure(Locations), multiple=T)
  })
  
  output$LocationsAll <- renderUI({
    if (is.null(ctlLocationName))
      return()
    # if(!is.null(output$CaseStudy))
    # {
    #   Location <- filter(ctlLocationName, caseStudy == output$CaseStudy)
    #   Location <- ctlLocationName$locationID # hiding dataPackageGUID, can use on filter
    # }
    # else {
    Locations <- c("All", ctlLocationName$locationID)
    # }
    selectInput("rtnLocid","Location(s): ", choices = structure(Locations), multiple=T)
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

  output$Setup2 <- renderUI({
    if(!is.null(ctlSetup)) {
      Setup <- c(ctlSetup$forecastSetup)
      selectInput("rtnForecastType","Forecast Setup: ", 
                  choices = structure(Setup), 
                  multiple=F)
    }
  })
  
    output$ScoreTypes <- renderUI({
    if(!is.null(ctlScoreType)) {
      # ScoreType <- structure(ctlScoreType)
      ScoreTypes <- structure(ctlScoreType$scoreType)
    }
    selectInput("rtnAllScoreTypes",
                "Score Type(s)", 
                choices = ScoreTypes, 
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
  
  filtInput <- reactive({
    validate(
      need(input$rtnLocid != "", "Please select at least one location")
    )
    
    if (length(input$rtnLocid) == 1) {
      remote <- filter(tbl.scores,
                       locationID == input$rtnLocid)
    }
    else if (length(input$rtnLocid) > 1) {
      remote <- filter(tbl.scores,
                       locationID %in% input$rtnLocid)
    }
    
    remote <- filter(remote,
      caseStudy == input$rtnCaseStudy &
      forecastSystem == input$rtnForecastSystem  & # ex ehype
      modelVariable == input$rtnModelVariable &
      forecastType == input$rtnForecastType & # ex Bias Corr 1
      scoreType == input$rtnScoreType
    )
    getit <- structure(collect(remote))
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
    else if (length(input$rtnLocid) > 1) {
      remote <- filter(tbl.scores,
                       locationID %in% input$rtnLocid)
    }
    remote <- filter(remote,
                     caseStudy == input$rtnCaseStudy &
                       forecastSystem == input$rtnForecastSystem  &
                       # scoreNA == FALSE & #more like "bad data" now, contains -Infinity too
                       modelVariable == input$rtnModelVariable &
                       forecastType == input$rtnForecastType &
                       scoreType %in% list.skill.scores # TODO if only 1 selected?? (do as compareSkillScores below)
    )
    getit <- structure(collect(remote)) 
  }) #end reactive

  compareSkillScores <- reactive({
    validate(
      need(input$rtnLocid != "", "Please select at least one location and one or more Forecast Setups to plot")
    )
    
    if (length(input$rtnLocid) == 1) {
      remote <- filter(tbl.scores,
                       locationID == input$rtnLocid)
    }
    else if (length(input$rtnLocid) > 1) {
      remote <- filter(tbl.scores,
                       locationID %in% input$rtnLocid)
    }

    if (length(input$scoreType) == 1) {
      remote <- filter(tbl.scores,
                      scoreType == input$scoreType)
    } else {
      remote <- filter(tbl.scores,
                       scoreType %in% input$scoreType)
    }
    
    remote <- filter(remote,
                     caseStudy == input$rtnCaseStudy &
                       forecastSystem == input$rtnForecastSystem  &
                       modelVariable == input$rtnModelVariable &
                       forecastType == input$rtnForecastType
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
    validate(
      need(!is.null(filtInput()), "Select one or more data elements from the Filter to begin")
    )
    if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      plot(1, 1, col = "white")
      text(1, 1,"Select one or more data elements from the Filter to begin")
    }
    else if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      text(1, 1, "filtInput() was empty, try a different combo")
    } else {      # have data
      filtered.input <- filtInput() # debug rename in summarySE
      loc.sum <- summarySE(filtered.input,
        measurevar = "scoreValue",
        groupvars = c("locationID", "datePartUnit", "leadtimeValue", "scoreType", "forecastType"),  # GT
        na.rm = TRUE)
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
        geom_line(size = 1) +
        geom_point(aes(color = locationID)) + 
        xlab(paste("Lead Times (", loc.sum$datePartUnit,")", sep="")) + 
        ylab(paste("SCORE: ", loc.sum$scoreType)) +
        theme_bw() + 
        theme(panel.grid.major = element_line(colour = NA)) +
        theme(axis.text = element_text(size=14, vjust=0.5)) +
        theme(legend.text = element_text(size=14, vjust=0.5)) +
        theme(title = element_text(size = 14))
    } 
  })
  
  output$facetPlot <- renderPlot({
    validate(
      need(!is.null(filtSkillScores()), "Select one or more data elements from the Filter to begin")
    )
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
          groupvars = c("locationID", "datePartUnit", 
                        "leadtimeValue", "scoreType", "forecastType"), # GT
          na.rm = TRUE
        )
      loc.sum$locationID <- as.factor(loc.sum$locationID)
      na.count <- sum(filtered.input$scoreNA) # should report to user since value hidden by summarySE()
    }

    if (nrow(filtSkillScores()) == 0) {
      plot(1, 1, col = "white")
      text(1, 1, "The database doesn't have information on this combination of variables (yet)")
    } else {
          # loc.count <- length(loc.sum$locationID)
        # plotInput <- 
          ggplot(loc.sum, aes(color = locationID, x = leadtimeValue, y = scoreValue ) ) +
            geom_line(size = 1) +
            geom_point(aes(color = locationID)) +
            facet_grid(scoreType ~ locationID, scales = "free_y") + #margin = TRUE
            geom_hline(aes(yintercept=0), colour="grey", linetype="dashed") +
            xlab(paste("Lead Times (",loc.sum$datePartUnit,")", sep="")) + 
            ylab("Scores") +
            theme_bw() + 
            theme(panel.grid.major = element_line(colour = NA)) +
            theme(axis.text = element_text(size=14, vjust=0.5)) +
            theme(legend.text = element_text(size=14, vjust=0.5)) +
            theme(title = element_text(size = 14)) + 
            scale_x_discrete(limits = loc.sum$leadtimeValue) + 
            theme(panel.margin.x = unit(2 / (length(unique(loc.sum$locationID)) - 1), "lines")) +
            theme(panel.margin.y = unit(2 / (length(unique(loc.sum$scoreType)) - 1), "lines")) +
            theme(strip.text = element_text(size=14, vjust=0.5))    
      }
  }) #renderPlot
  
# main plot
  output$downloadMainPlot <- downloadHandler(
    filename = 'series.plot.png',
    content = function(file){
      device <- function(..., width=width, height=height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
                }
        ggsave(file, plot = plotInput(), device = device)
        }
    )
  
    
    # filename = function() { paste(input$dataset, '.png', sep='') },
    # content = function(file) {
    #   ggsave(file, plot = "seriesPlot", device = "png")
    #}
  # )
  
  ###########################
  ### Upload Data
  
  # return a list of UI elements
  output$my_output_UI <- renderUI({
    list(
      h4(style = "color:blue;", "Add something missing from the database"),
      selectInput(inputId = "myselect", label="", choices = selections)
    )
  })
  # static
  selections <- c("Brier Score", "CRPS", "CRPS Skill Score")
  # update the selection list. Note the double assignment <<-
  observeEvent(input$mybutton,{
    selections <<- c(input$mytext, selections)
    updateSelectInput(session, "myselect", choices = selections, selected = selections[1])
  })
  
  
  
}) # end shinyServer

