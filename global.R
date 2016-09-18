# global function file in IMPREX ScoreBoard

# sse <- function(x, series){
#   sum((series - x)^2)
# }
# 
# mse <- function(x, series){
#   mean((series - x)^2)
# }

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
# borrowed from http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # library(plyr); library(dplyr)
  # library(reshape2)
  library(dplyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )
  
  # Rename the "mean" column
  # datac <- rename(datac, c("mean" = measurevar)) # causes issues w plyr?
  names(datac)[names(datac)=="mean"] <- measurevar # simpler jbn
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# expects to see col named "ref" with values "new" or "ref"

skillScore <- function(dl) {
  df_out = NULL
  # print(unique(dl$scoreType))
  for (my_score_type in unique(dl$scoreType)){
    df = NULL
    data <- as.list(split(dl[ dl$scoreType == my_score_type, c("reference", "scoreValue")], f=as.factor(dl$locationID)) )
    list.out  <- lapply(data, function(x){  
      ss   = 1 - (x[x$reference == "new", "scoreValue"] / x[x$reference == "ref", "scoreValue"])
    })
    df <- as.data.frame(list.out)
    # print(summary(df))
    #xformed to factors, drop the leading X
    names(df) <- sub(pattern = "X", replacement = "", colnames(df))
    df <- stack(df)
    colnames(df) <- c("scoreValue", "locationID")
    df$leadtimeValue <- rep(unique(dl$leadtimeValue[dl$scoreType == my_score_type]), 
                            times = length(unique(dl$locationID[dl$scoreType == my_score_type])))
    df$scoreType = my_score_type
    if(is.null(df_out)) {df_out = df} else {df_out = rbind(df_out, df)}
  }
  return(df_out)
}