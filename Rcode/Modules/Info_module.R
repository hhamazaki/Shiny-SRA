#'==============================================================================
# Info: add show info section  ----
#'==============================================================================
#'  UI ----------------------- 
InfoUI <- function(id,info.name) {
  ns <- NS(id)
#  p(strong(
#    HTML(
#    paste0(info.name,
          p(strong(info.name),
          tags$span(actionLink(ns('info'),"", icon = icon("circle-info",lib = "font-awesome")))
          )  
#          as.character(actionLink(ns('info'),"test", icon = icon("circle-info",lib = "font-awesome")))
#         actionLink(ns('info'),"", icon = icon("circle-info",lib = "font-awesome"))
#   ) # paste 0
#  )
#  )
#  )
}
#'  Server  ----------------------- 
InfoServer <- function(id,info.name,info.texts){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(input$info, {
        showModal(modalDialog(
          title = info.name,
          footer = NULL, info.texts
          ,
          easyClose = TRUE
        ) # End modalDialog
        ) # End showModal
      }
      )# End ObserverEvent
    } # End function 
  ) # End moduleServer
} # End InfoServer
