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
# test <- cbind(tbl.scores$forecastSetup, tbl.forecastsetup)

# defined within the interface table
tmpCaseStudy <-
    filter(tbl.interface,
         ObjectName == "Case Study" & LanguageID == RElanguage)
ctlCaseStudy <- collect(tmpCaseStudy)
#ctlCaseStudy <- ctlCaseStudy[,c(4,2)] #ID, CaseStudy
#names(ctlCaseStudy) <- c("CaseStudyID", "CaseStudy")

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
                paste("Case Study:"),  # (",length(unique(ctlCaseStudy$ObjectItemName)), ")
                choices = CaseStudy, multiple = F)
  })

  output$System <- renderUI({
    x <- input$rtnCaseStudy
    # browser()
    if (any(
      is.null(x)
    )) 
      return()
    #input of case study should be integer
    System <- select(tbl.scores, c(caseStudy, forecastSystem) )
    System <- filter(System, caseStudy==x)
    System <- unique(collect(System, n=Inf))
    selectInput("rtnForecastSystem", "System:", choices = structure(System$forecastSystem), multiple = F, selected = 2) # "E-HYPE" 
    # ? 'selected' must be the values instead of names of 'choices' for the input 'rtnForecastSystem'
  })
  
  
  # (Forecast) Setup ex Bias Corr 1 (formerly forecastType)
  output$Setup <- renderUI({
    x <- input$rtnForecastSystem
    y <- input$rtnCaseStudy
    if (any(
      is.null(x),
      is.null(y)
    )) 
      return() # hits this each time there's a null in first two selections TAKE CARE for endless loop

    # browser()
    
    Setup <- select(tbl.scores, c(caseStudy, forecastSystem, forecastSetup, forecastType))
    Setup <- filter(Setup, forecastSystem==x & caseStudy==y)
    Setup <- unique(collect(Setup, n=Inf))
    # print(paste("Setup$forecastSetup is: ", Setup$forecastSetup))
    Setup <- Setup[Setup$forecastSetup == Setup$forecastSetup]
    
    # Setup <- filter(tbl.forecastsetup, ID==Setup$forecastSetup)
    
    selectInput("rtnForecastType","Forecast Setup: ", 
                choices = structure(Setup$forecastType), 
                multiple=F)

  })
  
  
  output$Locations <- renderUI({
    x <- input$rtnCaseStudy
    y <- input$rtnForecastSystem
    z <- input$rtnForecastType #ex Bias Corr 
    # tab <- input$inTabset # using this resets all Locations
      
    if (any(
      is.null(x),
      is.null(y),
      is.null(z)
    )) 
      return() # hits this each time there's a null in first two selections TAKE CARE for endless loop

    # if (tab=="CompareSkillScores"){
    #   # note - set choices=character(0) to reset selections 
    #   # this isn't right because it clears the Location field anytime user clicks on this tab ... need condPanel
    #   return()
    # }
    
    Locations <- select(tbl.scores, c(locationID, caseStudy, forecastSystem, forecastType)) # forecastSetup
    Locations <- filter(Locations, caseStudy == x & forecastSystem == y & forecastType == z)
    Locations <- select(Locations, locationID)
    Locations <- distinct(Locations)
    Locations <- collect(Locations, n=Inf)
    Locations <- sort(Locations$locationID)
    # print(paste("locations to control: ", Locations))
    selectInput("rtnLocid","Location(s): ", choices = Locations, multiple=T) # order(Locations)
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
  
  ########################" TAB 3 Compare Skill Scores
  
  output$ReferenceSystem <- renderUI({
    paste("> ", input$rtnForecastSystem)    #"ReferenceSystem", System
  })
  
  output$ReferenceSetup <- renderUI({
    paste("> ", input$rtnForecastType)    #"ReferenceSystem", Setup
  })
  
  output$SystemToCompare <- renderUI({
    if(is.null(ctlSystem))
      return()
    System <- ctlSystem$ObjectItemName
    selectInput("rtnSystemToCompare", "System:", choices = System, multiple = F) #selected = none?
  })
  
  output$SetupToCompare <- renderUI({
    z <- input$rtnSystemToCompare
    if (any(
      is.null(z)
    )) 
      return()
    SetupCompare <- select(tbl.scores, c(caseStudy, forecastSystem, forecastSetup, forecastType))
    SetupCompare <- filter(SetupCompare, forecastSystem==z) #  & caseStudy==y
    SetupCompare <- unique(collect(SetupCompare, n=Inf))
    # print(paste("SetupCompare$forecastSetup is: ", SetupCompare$forecastSetup))
    SetupCompare <- SetupCompare[SetupCompare$forecastSetup == SetupCompare$forecastSetup]
    selectInput("rtnSetupToCompare","Forecast Setup: ", 
                choices = structure(SetupCompare$forecastType), 
                multiple=F)
  })

  get.overlapping.locs <- reactive({
    if(!is.null(input$rtnForecastSystem) & # right?
       !is.null(input$rtnForecastType) &
       !is.null(input$rtnSystemToCompare) &
       !is.null(input$rtnSetupToCompare) ){
      # print("entering SQL query")
      sGetAllLocations <- paste0("select distinct(\"locationID\") from \"tblScores\" where \"forecastSystem\" = '", input$rtnForecastSystem, "' and \"forecastType\" = '", input$rtnForecastType , 
                                "' and \"locationID\" in (select distinct(\"locationID\") from \"tblScores\" where \"forecastSystem\" = '", input$rtnSystemToCompare, "' and \"forecastType\" = '", input$rtnSetupToCompare ,"');")
      # cleaner, faster param query
      # sGetAllLocations <- paste("select distinct(\"locationID\") from \"tblScores\" where \"forecastSystem\" = 'E-HYPE' and \"forecastType\" = 'Bias Correction 1' ",
      #                           "and \"locationID\" in (select distinct(\"locationID\") from \"tblScores\" where \"forecastSystem\" = 'EFAS SYS4' and \"forecastType\" = 'Bias Correction 2');")
      # print(paste("query: ", sGetAllLocations))
      rs <- dbSendQuery(db$con, sGetAllLocations) # use existing conn, seems to work w difft API
      # cleanres <- sqlInterpolate(db$con, 
      #                            sGetAllLocsClean,
      #                            rs1 = input$rtnForecastSystem,
      #                            su1 = input$rtnForecastType,
      #                            rs2 = input$rtnSystemToCompare,
      #                            su2 = input$rtnSetupToCompare)
      df <- dbFetch(rs)
      dbClearResult(rs)
      return(df)
    }
    else {
      return()
      }
    })

  # DONE query valid scoreTypes for selected params
  # output$OverlappingScoreTypes
  get.overlapping.scoretypes <- reactive({
    a <- input$rtnSystemToCompare
    b <- input$rtnForecastType
    c <- input$rtnSystemToCompare
    d <- input$rtnSetupToCompare
    
    if (any(
      is.null(a),
      is.null(b),
      is.null(c),
      is.null(d)
    )) 
      return()
    
    # print("here comes sql query")
    sqlOverlappingScoreTypes <- paste0("select distinct(\"scoreType\") from \"tblScores\" where \"forecastSystem\" = '", input$rtnForecastSystem, 
                                       "' and \"forecastType\" = '", input$rtnForecastType , 
                                       "' and \"scoreType\" in ",
                                       "(select distinct(\"scoreType\") from \"tblScores\" where \"forecastSystem\" = '", input$rtnSystemToCompare, 
                                       "' and \"forecastType\" = '", input$rtnSetupToCompare ,"');")
    # print(paste("query: ", sqlOverlappingScoreTypes))
    rs <- dbSendQuery(db$con, sqlOverlappingScoreTypes ) 
    df <- dbFetch(rs)
    # browser()
    dbClearResult(rs)
    return(df)
  })

    
  output$LocationsAll <- renderUI({
    if (is.null(ctlLocationName))
      return()
    # browser()   
  
    # if(!is.null(get.overlapping.locs())){
    #   print(paste("get.overlapping.locs not null: ", as.vector(get.overlapping.locs())))
    # }
    # if(length(get.overlapping.locs())>0){
    #   print(paste("get.overlapping.locs len>0: ", as.vector(get.overlapping.locs())))
    # }
    
    if(!is.null(get.overlapping.locs())){
      LocationsAll <- as.vector(get.overlapping.locs())
    }
    else {
      return()
      # Locations <- c("No Locations in selection") 
    }
    selectInput("rtnLocationsMeetingCrit","Location(s): ", choices = structure(LocationsAll), multiple=T)
  })
    
  output$ScoreTypesInBoth <- renderUI({

      # if(!is.null(get.overlapping.locs())){
      #   print(paste("get.overlapping.locs not null: ", as.vector(get.overlapping.locs())))
      # }
      # if(length(get.overlapping.locs())>0){
      #   print(paste("get.overlapping.locs len>0: ", as.vector(get.overlapping.locs())))
      # }
      
      if(!is.null(get.overlapping.scoretypes())){
        ScoreTypesInBoth <- as.vector(get.overlapping.scoretypes())
        # browser()
      }
      else {
        return()
      }
    selectInput("rtnScoreTypesMeetingCrit","Score Type(s): ", choices = structure(ScoreTypesInBoth$scoreType), multiple=T)
  })

  #for first plot  
  filtInput <- reactive({
    validate(
      need(input$rtnLocid != "", "Please select at least one location from Filter Criteria")
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
      # TODO add validation points
      # need(input$)
    )
    list.skill.scores <- input$rtnAllScoreTypes
    if (length(list.skill.scores) == 1) {
      remote <- filter(tbl.scores,
                       scoreType == list.skill.scores)
    }
    else if (length(list.skill.scores) > 1) {
      remote <- filter(tbl.scores,
                       scoreType %in% list.skill.scores)
    }
    
    
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
                       forecastType == input$rtnForecastType
                       # scoreType %in% list.skill.scores # TODO if only 1 selected?? (do as compareSkillScores below)
    )
    getit <- structure(collect(remote)) 
  }) #end reactive

############ this builds the dataset needed for compareSkillScorePlot
##
  compareSkillScores <- reactive({
    validate(
      need(input$LocationsAll != "", "Please select at least one location and one or more Forecast Setups to plot"),
      need(input$ScoreTypesInBoth != "", "Please select at least one location and one or more Forecast Setups to plot")
    )
    
    #what goes in
    #Location
    if (length(input$rtnLocationsMeetingCrit) == 1) {
      remote <- filter(tbl.scores,
                       locationID == input$rtnLocationsMeetingCrit)
    }
    else if (length(input$rtnLocationsMeetingCrit) > 1) {
      remote <- filter(tbl.scores,
                       locationID %in% input$rtnLocationsMeetingCrit)
    }

    #ScoreType
    if (length(input$rtnScoreTypesMeetingCrit) == 1) {
      remote <- filter(tbl.scores,
                       scoreType == input$rtnScoreTypesMeetingCrit)
    }
    else if (length(input$rtnScoreTypesMeetingCrit) > 1) {
      remote <- filter(tbl.scores,
                       scoreType %in% input$rtnScoreTypesMeetingCrit)
    }
    
    remote <- filter(remote,
                       caseStudy == input$rtnCaseStudy &
                       # forecastSystem == input$rtnForecastSystem  &
                       modelVariable == input$rtnModelVariable
    )
    getit <- structure(collect(remote))
    browser()
    
  }) #end reactive

  output$summary <- renderPrint({
    dataset <- filtInput()
    dataset <- within(
      dataset, rm("row.names", "datePartUnit", "forecastSystem", "forecastRange", "caseStudy",  "leadtimeUnit", "leadtimeValue")
      )
    summary(dataset)
  })
  
  output$compareSkillScorePlot <- renderPlot({
    validate(
      # compareSkillScores
      need(!is.null(input$rtnLocationsMeetingCrit), "Select one or more Location(s) to begin"),
      need(!is.null(input$rtnScoreTypesMeetingCrit), "Select one or more ScoreType(s) to begin")
    )
    
    df <- compareSkillScores()
    if (length(df)<1){
      return()
    }
    
    #TODO need smart filter for lead times
    df <- filter(df, leadtimeValue %in% c(1,2,3,4,5,6))
    
    #TODO replace w smart filter for _______
    print(paste("debug, ref is:", input$rtnForecastType))
    print(paste("debug, new is:", input$rtnSetupToCompare))
    df$ref = NA
    df$ref[df$forecastType==input$rtnForecastType] = "ref"
    df$ref[df$forecastType!=input$rtnSetupToCompare] = "new"
    
    #step 1, aggregate dataet
    agg <- c("forecastSetup", "forecastSystem", "forecastType", "locationID", "leadtimeValue", "scoreType", "ref")
    df.sum <- summarySE(data = df, "scoreValue", agg, na.rm = T)
    
    #step 2
    df3 <- skillScore(df.sum)


    # step 3
    #3, run df3 thru something like this to frame up
    df.plot = data.frame(LocationID = rep(unique(df.sum$locationID), each = length(unique(df.sum$leadtimeValue))), 
                       leadtimeValue = rep(unique(df.sum$leadtimeValue), times = length(unique(df.sum$locationID))), 
                       ScoreValue = df3[2:length(df3)])
    

    plotInput <- 
      ggplot(df.plot,  aes(color = locationID, x = leadtimeValue, y = scoreValue ))
      # ggplot(loc.sum, aes(color = locationID, x = leadtimeValue, y = scoreValue ) ) +
      #   geom_line(size = 1) +
      #   geom_point(aes(color = locationID)) +
      #   facet_grid(scoreType ~ locationID, scales = "free_y") + #margin = TRUE
      #   geom_hline(aes(yintercept=0), colour="grey", linetype="dashed") +
      #   xlab(paste("Lead Times (",loc.sum$datePartUnit,")", sep="")) + 
      #   ylab("Scores") +
      #   theme_bw() + 
      #   theme(panel.grid.major = element_line(colour = NA)) +
      #   theme(axis.text = element_text(size=14, vjust=0.5)) +
      #   theme(legend.text = element_text(size=14, vjust=0.5)) +
      #   theme(title = element_text(size = 14)) + 
      #   scale_x_discrete(limits = loc.sum$leadtimeValue) + 
      #   theme(panel.margin.x = unit(2 / (length(unique(loc.sum$locationID)) - 1), "lines")) +
      #   theme(panel.margin.y = unit(2 / (length(unique(loc.sum$scoreType)) - 1), "lines")) +
      #   theme(strip.text = element_text(size=14, vjust=0.5))    

  })
  
  output$seriesPlot <- renderPlot({
    validate(
      need(!is.null(filtInput()), "Select one or more data elements from the Filter to begin")
    )
    # TODO NOT SURE IF VALIDATE WAS ENOUGH ... NEW ERRORS??
    if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      plot(1, 1, col = "white")
      text(1, 1,"Select one or more data elements from the Filter to begin")
    }
    else if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      text(1, 1, "filtInput() was empty, try a different combo")
    } else {      # have data
      filtered.input <- filtInput() # debug rename in summarySE
      loc.sum <- NULL # DEBUG ?
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

