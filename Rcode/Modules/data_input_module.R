#'==============================================================================
#  dataInput: Upload data file to Shiny  ---------------
#'==============================================================================
#  Usage: UI section 
#  dataInputUI("ns.name", "User data (.csv format)")
#  Usage: Server section: dataInputServer("datain")
#'==============================================================================
#' UI  ------------------------------------------------
dataInputUI <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Choose File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Input: Checkbox if file has header ----
    checkboxInput(ns("header"), "Header", TRUE),
    # Input: Select separator ----
    radioButtons(ns("sep"), "Separator",
                 choices = c(Comma = ",", Tab = "\t"), selected = ",")
  ) # End taglist
} # End dataInputUI
#' Server ---------------------------------------------------------------------
dataInputServer <- function(id){
  moduleServer(id,
               function(input, output, session) {
                 # The selected file, if any
                 userFile <- reactive({
                   # If no file is selected, don't do anything
                   validate(need(input$file, message = FALSE))
                   input$file
                 }) # End userfile
                 
                 # The user's data, parsed into a data frame
                 df <- reactive({
                   df <- read.csv(userFile()$datapath,
                                  header = input$header,
                                  sep = input$sep,
                                  stringsAsFactors = FALSE)
                 })
                 # We can run observers in here if we want to
                 observe({
                   msg <- sprintf("File %s was uploaded", userFile()$name)
                   cat(msg, "\n")
                 }) # End observe
                 file_name <- reactive(userFile()$name)
                 return(list(df=df,fn=file_name))
               } # End function
  ) # End moduleServer
} # End dataInputServer
