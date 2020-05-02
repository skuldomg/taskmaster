#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# TODO: More than one current task possible
# TODO: Make UI better
# TODO: Fix double save bug

library(shiny)
library(shinythemes)
library(shinydashboard)
library(googlesheets4)
library(DT)
library(tibble)
library(stringr)
library(dplyr)
library(lubridate)

source("helpFunctions.R")

# the sheet ID
ss <- "1HwdbcGaXf1kUz6Ot0ufT_y4l4Yp76QDGzk6aeXDuOgo"

# Read task data from Googlesheet
taskdata <- range_read(ss)

# Transform to proper date format
taskdata <- as_tibble(transform(taskdata, startDate = as.Date(startDate)))
taskdata <- as_tibble(transform(taskdata, finishDate = as.Date(finishDate)))

for(c in 1:ncol(taskdata)) {
    if(startsWith(colnames(taskdata[, c]), "day")) {
       taskdata[[colnames(taskdata[, c])]] <- as.Date(taskdata[[colnames(taskdata[, c])]])
    }
}

# Write back to Googlesheets when closing
onStop(function() {
    print("Saving...")
    write_sheet(taskdata, ss, sheet = 1)
    print("... done.")
})

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # Application title
    dashboardHeader(
        title="Taskmaster"),
    
    # Sidebar with a slider input for number of bins 
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("dashboard")),
            menuItem("Details", tabName = "details", icon = icon("th"))
        )
    ),
     
    dashboardBody(
        tabItems(
            tabItem(tabName = "home",
                    h2("Home"),
                    hr(),
                    uiOutput("homeUi"),
                    fluidRow(
                        splitLayout(
                            cellWidths = c("33%", "33%"),
                            plotOutput("plot1"),
                            plotOutput("plot2"),
                            style = "width:75%"
                        )
                    )
                    ),
            
            tabItem(tabName = "details",
                    h2("Details"),
                    hr(),
                    uiOutput("newTaskUi"),
                    hr(),
                    uiOutput("addWordsUi"),
                    hr(),
                    DT::dataTableOutput("tasktable"),
                    hr(),
                    uiOutput("myStatus")
            ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Home tab
    # TODO: Make it update properly after entering new data
    # TODO: Make it work for more than one concurrent task
    output$homeUi <- renderUI({
        
        input$saveTask
        input$saveWords
        
        activeTasks <- with(taskdata, taskdata[(as.Date(finishDate) >= Sys.Date()), ])
        theDetails <- getDetails(activeTasks)
        
        detls <- reactiveVal(theDetails)
        
            tagList(
                h3("Current tasks"),
                fluidRow(
                    box(
                        h4(theDetails$taskname),
                        p("Words left:", isolate(detls()$wordsleft)),#theDetails$wordsleft),
                        p("Work days left:", isolate(detls()$daysleft)),#theDetails$daysleft),
                        p("Percentage done:", isolate(detls()$percdone), "%"),#theDetails$percdone, "%"),
                        p("Average words done per day:", isolate(detls()$wordsperday)),#theDetails$wordsperday),
                        p("Words per day needed to finish on time:", isolate(detls()$wpdgoal)),#theDetails$wpdgoal),
                        strong(isolate(detls()$record))#theDetails$record)
                    )
                )#,
                #plotOutput("wpdPlot", width = "50%")
            )
        
    })
    
    # TODO: Actually get it by corresponding index
    output$plot1 <- renderPlot({
        
        input$saveTask
        input$saveWords
        
        tasks <- taskdata
        makeReactiveBinding("tasks")
        
        # Get the plot of the task
        getWpdPlot(isolate(tasks <- taskdata), 2)
    })
    
    output$plot2 <- renderPlot({
        
        input$saveTask
        input$saveWords
        
        tasks <- taskdata
        makeReactiveBinding("tasks")
        
        getCumWordsPlot(isolate(tasks <- taskdata), 2)
    })
    
    # Detail tab
    # Add new task
    output$newTaskUi <- renderUI({
        
        tagList(
            h3("Enter new task"),
            textInput(inputId = "taskInput", label = "Task name"),
            textInput(inputId = "startDate", label = "Start date"),
            textInput(inputId = "finishDate", label = "Due date"),
            textInput(inputId = "wordsDone", label = "Words already done"),
            textInput(inputId = "wordGoal", label = "Total words"),
            actionButton("saveTask", label = "Save")#,
            #actionButton("reload", "Reload")
        )
    })
    
    # Add words to existing project
    output$addWordsUi <- renderUI({
        
        tagList(
            textInput(inputId = "addWords", label = "Words done today"),
            actionButton("saveWords", label = "Save")
        )
        
    })
    
    output$tasktable <- DT::renderDataTable({
        input$saveTask
        input$saveWords
        input$reload

        outtable <- reactiveVal(taskdata)
        DT::datatable(isolate(outtable()), selection = "single", rownames = FALSE)
    })
    
    output$myStatus <- renderUI({
        rowIndex <- input$tasktable_rows_selected

        if(!is.null(rowIndex)) {
            theDetails <- getDetails(taskdata, rowIndex)
            
            tagList(
                h3("Current tasks"),
                fluidRow(
                    box(
                        h4(theDetails$taskname),
                        p("Words left:", theDetails$wordsleft),
                        p("Work days left:", theDetails$daysleft),
                        p("Percentage done:", theDetails$percdone, "%"),
                        p("Average words done per day:", theDetails$wordsperday),
                        p("Words per day needed to finish on time:", theDetails$wpdgoal),
                        strong(theDetails$record)
                    )
                ))
        }
    })
    
    # Save words
    observeEvent(input$saveWords, {
        rowIndex <- input$tasktable_rows_selected
        
        if(!is.null(rowIndex)) {
            today <- Sys.Date()
            todaysWords <- as.numeric(input$addWords)
            
            # Find last date in the row
            lastDate <- NA
            lastDateIndex <- -1
            for(c in 1:ncol(taskdata)) {
                if(is.Date(taskdata[[rowIndex, c]])) {
                    lastDate <- taskdata[[rowIndex, c]]
                    lastDateIndex <- as.numeric(c)
                }
            }
            
            # If we have one, and it is the same as today, don't add a new day
            if(!is.na(lastDate) && (today - lastDate == 0)) {
                print("We already have data for today. Adding..")
                taskdata[rowIndex, ] <<- replace(taskdata[rowIndex, ], c(lastDateIndex+1), c(taskdata[[rowIndex, lastDateIndex+1]]+todaysWords))
            }
            else {
            
                # Find the first day column that doesn't have any data and put the data there
                # If all are full, add a new day column
                firstNa <- first(which(is.na(taskdata[rowIndex, ])))
                
                if(!is.na(firstNa)) {
                    # Write date and word count
                    taskdata[rowIndex, ] <<- replace(taskdata[rowIndex, ], c(firstNa), c(today))
                    taskdata[rowIndex, ] <<- replace(taskdata[rowIndex, ], c(firstNa+1), c(todaysWords))
                }
                else {
                    # Get number of last day entered
                    lastColName <- last(colnames(taskdata))
                    
                    if(str_length(lastColName) == 9)
                        dayNo <- as.numeric(substr(lastColName, 9, 9))+1
                    else
                        dayNo <- as.numeric(substr(lastColName, 9, 10))+1
                    
                    # Problem: We fill all new rows with the info that way
                    taskdata <<- taskdata %>% add_column(!!(paste0("day", dayNo)) := today, !!(paste0("wordsDay", dayNo)) := todaysWords)
                    
                    newCol1Index <- ncol(taskdata)-1
                    newCol2Index <- ncol(taskdata)
                    
                    # Delete values of all rows that are not our current one
                    
                    for(row in rownames(taskdata)) {
                        if(as.numeric(row) != as.numeric(rowIndex)) {
                            taskdata[as.numeric(row), newCol1Index] <- NA
                            taskdata[as.numeric(row), newCol2Index] <- NA
                        }
                    }
                    # Write to global
                    taskdata <<- taskdata
                }
            }
            
            # Update total wordcount
            taskdata[rowIndex, ]$wordsDone <<- taskdata[rowIndex, ]$wordsDone + todaysWords
            
            updateTextInput(session, "addWords", value = "")
            
        }
    })
    
    # Save a new task
    observeEvent(input$saveTask, {
        
        # Check if the task already exists, if yes, update the task
        existingRow <- as.numeric(rownames(taskdata)[taskdata$taskname == input$taskInput])
        
        if(length(existingRow) > 0) {
            
            # If the input field is empty, take the old value and don't overwrite it
            taskname <- input$taskInput
            startDate <- as.Date(input$startDate, "%d.%m.%y")
            dueDate <- as.Date(input$finishDate, "%d.%m.%y")
            wordsDone <- as.numeric(input$wordsDone)
            wordGoal <- as.numeric(input$wordGoal)
            
            if(str_length(input$startDate) == 0 || is.null(input$startDate) || is.na(input$startDate))
                startDate <- as.Date(taskdata$startDate[existingRow])
            
            if(str_length(input$finishDate) == 0 || is.null(input$finishDate) || is.na(input$finishDate))
                dueDate <- as.Date(taskdata$finishDate[existingRow])
            
            if(str_length(input$wordsDone) == 0 || is.null(input$wordsDone))
                wordsDone <- as.numeric(taskdata$wordsDone[existingRow])
            
            if(str_length(input$wordGoal) == 0 || is.null(input$wordGoal))
                wordGoal <- as.numeric(taskdata$wordGoal[existingRow])
            
            # Update the row with new data
            taskdata[existingRow, ] <<- tribble(
                ~taskname, ~startDate, ~finishDate, ~wordsDone, ~wordGoal,
                taskname, startDate, dueDate, wordsDone, wordGoal
                )
            
        }
        else 
            taskdata <<- taskdata %>% add_row(taskname = input$taskInput, finishDate = as.Date(input$finishDate, "%d.%m.%y"), wordsDone = as.numeric(input$wordsDone), wordGoal = as.numeric(input$wordGoal))
        
        # empty text input fields
        updateTextInput(session, "taskInput", value = "")
        updateTextInput(session, "startDate", value = "")
        updateTextInput(session, "finishDate", value = "")
        updateTextInput(session, "wordsDone", value = "")
        updateTextInput(session, "wordGoal", value = "")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)