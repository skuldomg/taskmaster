#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# TODO: More than one current task possible
# TODO: Separate thingies for create new task and add words

library(shiny)
library(shinythemes)
library(shinydashboard)
library(googlesheets4)
library(DT)
library(tibble)
library(stringr)

source("getDetails.R")

# the sheet ID
ss <- "1HwdbcGaXf1kUz6Ot0ufT_y4l4Yp76QDGzk6aeXDuOgo"

# Read task data from Googlesheet
taskdata <- range_read(ss)

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
    
    # Show a plot of the generated distribution
    dashboardBody(
        tabItems(
            tabItem(tabName = "home",
                    h2("Home"),
                    hr(),
                    uiOutput("homeUi")
                    ),
            
            tabItem(tabName = "details",
                    h2("Details"),
                    hr(),
                    uiOutput("myUi"),
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
    output$homeUi <- renderUI({
        
        activeTasks <- with(taskdata, taskdata[(as.Date(finishDate) >= Sys.Date()), ])
        
        
            tagList(
                h3("Current tasks"),
                getDetails(activeTasks)
            )
        
    })
    
    # Detail tab
    output$myUi <- renderUI({
        
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
    
    output$tasktable <- DT::renderDataTable({
        input$saveTask
        input$reload
        
        outtable <- reactiveVal(taskdata)
        DT::datatable(isolate(outtable()), selection = "single", rownames = FALSE)
    })
    
    output$myStatus <- renderUI({
        rowIndex <- input$tasktable_rows_selected
        
        getDetails(taskdata, rowIndex)
    })
    
    # Prefill input fields with currently selected row
    observeEvent(input$tasktable_rows_selected, {
        rowIndex <- input$tasktable_rows_selected
        updateTextInput(session, "taskInput", value = taskdata$taskname[rowIndex])
        updateTextInput(session, "wordsDone", value = taskdata$wordsDone[rowIndex])
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
        else {
            print("Wait what")
            taskdata <<- taskdata %>% add_row(taskname = input$taskInput, finishDate = as.Date(input$finishDate, "%d.%m.%y"), wordsDone = as.numeric(input$wordsDone), wordGoal = as.numeric(input$wordGoal))
        }
        
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
