# Generate a detail box for a task
# Expects a dataframe with tasks and row of the task that we want to have the details of
getDetails <- function(tasks, index = 1) {
  rowIndex <- index
  
  # details and calculations
  wordsDone <- tasks$wordsDone[rowIndex]
  wordsLeft <- tasks$wordGoal[rowIndex] - wordsDone
  percentage <- format(round((wordsDone / tasks$wordGoal[rowIndex]) * 100, 2), nsmall = 2)
  startDay <- as.Date(tasks$startDate[rowIndex])
  today <- Sys.Date()
  finalDay <- as.Date(tasks$finishDate[rowIndex])
  lang <- "DE"
  
  # Calculation of weekdays between today and due date
  # (only if finalDay is in the future)
  if(as.numeric(finalDay - today) > 0) {
    if(sum(!weekdays(seq(as.Date("1988-02-01"), as.Date("1988-02-08"), "days")) %in% c("Saturday", "Sunday")) == 8)
      daysLeft <- as.numeric(sum(!weekdays(seq(today, finalDay, "days")) %in% c("Samstag", "Sonntag")))
    else {
      daysLeft <- as.numeric(sum(!weekdays(seq(today, finalDay, "days")) %in% c("Saturday", "Sunday")))
      lang <- "EN"
    }
  }
  else
    daysLeft <- -1
  
  # Weekdays between start date and today
  if(lang == "DE")
    daysSinceStart <- as.numeric(sum(!weekdays(seq(startDay, today, "days")) %in% c("Samstag", "Sonntag")))
  else
    daysSinceStart <- as.numeric(sum(!weekdays(seq(startDay, today, "days")) %in% c("Saturday", "Sunday")))
  
  # Weekdays between start date and finish date
  if(lang == "DE")
    projectTime <- as.numeric(sum(!weekdays(seq(startDay, finalDay, "days")) %in% c("Samstag", "Sonntag")))
  else
    projectTime <- as.numeric(sum(!weekdays(seq(startDay, finalDay, "days")) %in% c("Saturday", "Sunday")))
  
  wpd <- wordsDone / daysSinceStart
  wpdGoal <- wordsLeft / daysLeft
  
  # Show details of currently selected task
  if(length(rowIndex)) {
    
    #trackRecord <- strong("On track!", style="color:green")
    trackRecord <- "On track!"
    
    if(wpd < wpdGoal)
      #trackRecord <- strong("You need to pick up the pace, friend.", style="color:red")
      trackRecord <- "You need to pick up the pace, friend."
    
    # Pretty checks
    if(daysLeft < 0) {
      daysLeft <- 0
      wpd <- wordsDone / projectTime
      print(paste("Project had", projectTime, "workdays."))
      #trackRecord <- strong("Finished.")
      trackRecord <- "Finished."
    }
    
    data.frame(taskname = tasks$taskname[rowIndex], wordsleft = wordsLeft, daysleft = daysLeft, percdone = percentage,
               wordsperday = format(round(wpd, 2), nsmall = 2), wpdgoal = format(round(wpdGoal, 2), nsmall = 2), record = trackRecord)
    
    # fluidRow(
    #   box(
    #     h4(tasks$taskname[rowIndex]),
    #     p("Words left:", wordsLeft),
    #     p("Work days left:", daysLeft),
    #     p("Percentage done:", percentage, "%"),
    #     p("Average words done per day:", format(round(wpd, 2), nsmall = 2)),
    #     p("Words per day needed to finish on time:", format(round(wpdGoal, 2), nsmall = 2)),
    #     trackRecord
    #   ))
  }
}

# Returns a barplot of the words per day for a given project
# Expects a dataframe with tasks, row of the task and an optional ylimit for the barplot
getWpdPlot <- function(tasks, index, ylimit = NULL) {
  # Construct a df for plot containing only the dates and wordcounts
  count <- 0
  theDf <- tibble()
  theDf <- theDf %>% add_column(date = as.Date(character()), words = numeric())
  
  for(name in colnames(tasks)) {
    count <- count+1
    
    if(startsWith(name, "day")) {
      theDate <- as.Date(tasks[[index, count]])
      
      # print(paste0("Date in column ", count, ": "))
      # print(theDate)
    }
    
    if(startsWith(name, "wordsDay")) {
      theWords <- tasks[[index, count]]
      
      # print(paste0("Count in column ", count, ": ", theWords))
      
      # As soon as we have a word column, we've had the corresponding date immediately before
      theDf <- theDf %>% add_row(date = theDate, words = theWords)
    }
  }
  
  barplot(height = theDf$words, main = "Words per day", xlab = "Days", ylab = "Words", names.arg = theDf$date, ylim = ylimit)
}