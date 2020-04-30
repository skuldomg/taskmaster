# Generate a detail box for a task
getDetails <- function(tasks, index = 1) {
  rowIndex <- index
  
  # details and calculations
  wordsDone <- tasks$wordsDone[rowIndex]
  wordsLeft <- tasks$wordGoal[rowIndex] - wordsDone
  percentage <- format(round((wordsDone / tasks$wordGoal[rowIndex]) * 100, 2), nsmall = 2)
  startDay <- as.Date(tasks$startDate[rowIndex])
  today <- Sys.Date()
  finalDay <- as.Date(tasks$finishDate[rowIndex])
  
  # Calculation of weekdays between today and due date
  if(sum(!weekdays(seq(as.Date("1988-02-01"), as.Date("1988-02-08"), "days")) %in% c("Saturday", "Sunday")) == 8)
    daysLeft <- as.numeric(sum(!weekdays(seq(today, finalDay, "days")) %in% c("Samstag", "Sonntag")))
  else
    daysLeft <- as.numeric(sum(!weekdays(seq(today, finalDay, "days")) %in% c("Saturday", "Sunday")))
  
  daysSinceStart <- as.numeric(today - startDay, units="days")
  wpd <- wordsDone / daysSinceStart
  wpdGoal <- wordsLeft / daysLeft
  
  # Show details of currently selected task
  if(length(rowIndex)) {
    
    trackRecord <- strong("On track!", style="color:green")
    
    if(wpd < wpdGoal)
      trackRecord <- strong("You need to pick up the pace, friend.", style="color:red")
    
    # Pretty checks
    if(daysLeft < 0) {
      daysLeft <- 0
      trackRecord <- strong("Finished.")
    }
    
    fluidRow(
      box(
        h4(tasks$taskname[rowIndex]),
        p("Words left:", wordsLeft),
        p("Work days left:", daysLeft),
        p("Percentage done:", percentage, "%"),
        p("Average words done per day:", format(round(wpd, 2), nsmall = 2)),
        p("Words per day needed to finish on time:", format(round(wpdGoal, 2), nsmall = 2)),
        trackRecord
      ))
  }
}