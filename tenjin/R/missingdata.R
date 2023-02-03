# MISSING DATA DETECTION
Interpolate <- function(id, data, type = "person_id"){
  ##Positions where the mean == NaN means there were no values to average,
  ## these are the points that are going to be interpolated. Store their positions
  ID = id

  if(type == "person_id"){
    dp <- data %>% filter(person_id == ID) %>%
      mutate(difference = ymd_hms(timestamp) - lag(ymd_hms(timestamp))) %>%
      replace_na(list("difference" = dseconds(x = 0))) %>%
      mutate(timelapse = cumsum(as.double(difference))) %>%
      mutate(period = timelapse %/% 0.5)

    dp$difference[1]

  }else{
    dp <- data %>% filter(tag_id == ID) %>%
      mutate(difference = ymd_hms(timestamp) - lag(ymd_hms(timestamp))) %>%
      replace_na(list("difference" = dseconds(x = 0))) %>%
      mutate(timelapse = cumsum(as.double(difference))) %>%
      mutate(period = timelapse %/% 0.5)
  }


  #Creating empty vectors to store calculated means
  xx <- numeric()
  yy <- numeric()

  if(sum(dp$difference > 0.5)){
    index <- which(dp$difference > 0.5)

    for (i in 1:length(index)){
      if(dp$difference[index[i]] < 2){
        #Select position before and after na, store, and take the average
        da <- dp %>% slice(index[i] -1 , index)
        dt <- as.double((mean(ymd_hms(da$timestamp)) - ymd_hms(da$timestamp[1]))) / as.double((ymd_hms(da$timestamp[2]) - ymd_hms(da$timestamp[1])))
        xx[i] <- dt * (da$x[2] - da$x[1]) + da$x[1]
        yy[i] <- dt * (da$y[2] - da$y[1]) + da$y[1]
        dp <- dp %>% add_row(tag_id = da$tag_id[1] ,
                             experiment_id = da$experiment_id[1],
                             person_id = da$person_id[1],
                             timestamp = as.character(ymd_hms(da$timestamp[2]) + (seconds(da$difference[2]) / 2)) ,
                             x = xx[i],
                             y = yy[i],
                             .before = index[i])
        return(dp)
      }else{
        dp <- dp %>% add_row(tag_id = dp$tag_id[1] ,
                             experiment_id = dp$experiment_id[1],
                             person_id = dp$person_id[1],
                             timestamp = NA ,
                             x = NA,
                             y = NA,
                             .before = index[i])
        return(dp)
      }
    }
  }else{
    return(dp)
  }
}

CompleteData <- function(vectors, data, task_df, type = "person_id", verbose = F){
  FinalDataframe <- data.frame()
  maxjumps <- numeric()
  meanjumps <- numeric()
  for (i in 1:length(vectors)){

    PartialData <- Interpolate(vectors[i],data,type)
    maxjumps[i] <- max(PartialData$difference)
    meanjumps[i] <- mean(PartialData$difference)
    # compute deviance within periods
    PartialData %>% group_by(person_id, period) %>%
      suppressMessages(summarise(n_data = n(),
                                 time = first(timestamp),
                                 meanx = mean(x) ,
                                 meany = mean(y),
                                 x_maxdev = max(mean(x) -x),
                                 y_maxdev = max(mean(y) -y)))
    #Selection for person_id or tag_id
    if(type == "person_id"){
      PartialData$person_id <- vectors[i]
      PartialData <- AddGoals(vectors[i], PartialData, task_df)
    }
    else{
      PartialData$tag_id <- vectors[i]
    }
    FinalDataframe <- rbind(FinalDataframe,PartialData)
    print(paste("ID", i , "of", length(vectors)))
    if(verbose == T){
      print(paste("with maximum timejump:", maxjumps[i],"and mean timejump:", meanjumps[i]))
    }
  }
  return(FinalDataframe)
}

AddGoals <- function(id, data, task_df, type = "person_id"){
  ID = id
  #Replace interpolate with your own function.
  #IMPORTANT check what the time column is called.
  #Data for one given person
  TheData <- data
  #Data for one given tag
  Goals <- task_df %>% filter(, person_id == ID)


  #Important to add these columns to the data
  TheData$tablet_to_id <- NA
  TheData$tablet_from_id <- NA

  #ContainsTime at the end of the loop will contains a list of vectors.
  #Each vector is a comparison of whether each row in TheData$time is within
  #A given timespan of a goals that is in the Goals table
  ContainsTime <- list()
  #I have seperate columns for the time and the adjusted time
  #Make sure naming of the time column is consistent between data & function
  #
  for (j in 1:length(Goals$start_time)){
    ContainsTime[[j]] <- (ymd_hms(TheData$timestamp) >= ymd_hms(Goals$start_time[j]) &
                            ymd_hms(TheData$timestamp) <= ymd_hms(Goals$end_time[j]))
  }

  #We loop over each entry in ContainsTime, use which to get the positions that are
  #TRUE than replace the positions of the TRUE's in position k in the list with the goals
  #in position from the subsetted task table
  for (k in 1:length(ContainsTime)){
    #
    TheData$tablet_to_id[which(ContainsTime[[k]])] <- Goals$tablet_to_id[k]
    TheData$tablet_from_id[which(ContainsTime[[k]])] <- Goals$tablet_from_id[k]
  }
  return(TheData)
}
