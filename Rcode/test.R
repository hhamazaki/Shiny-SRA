  shinyApp(
    ui <- fluidPage(
      actionButton("reset", "RESET", style="simple", size="sm", color = "warning"),
      verbatimTextOutput(outputId = "text"),
      textOutput('range')
    ),
    server = function(input, output, session) {
      l <- reactiveValues()
      observeEvent(input$reset, {
        # display a modal dialog with a header, textinput and action buttons
        showModal(modalDialog(
          tags$h2('Please enter your information'),
  sliderInput(inputId="lnalpha","lnalpha", value=c(0,10),min=0,max=5,step=0.1),
  sliderInput(inputId="beta","beta", value=c(0,5),min=0,max=5,step=0.1),          textInput('name', 'Name'),
          textInput('state', 'State'),
          footer=tagList(
            actionButton('submit', 'Submit'),
            modalButton('cancel')
          )
        ))
      })
      
      # only store the information if the user clicks submit
      observeEvent(input$submit, {
        removeModal()
        l$name <- input$name
        l$state <- input$state
        l$lnalpha <- input$lnalpha
        l$beta <- input$beta
      })
    observeEvent(input$submit,{
        if(is.null(input$submit)){
            updateSliderInput(session,'lnalpha',value = c(0,5))
            updateSliderInput(session,'beta',value = c(0,5))
        }
    })
      
      # display whatever is listed in l
      output$text <- renderPrint({
        if (is.null(l$name)) return(NULL)
        paste('Name:', l$name, 'and state:', l$state)
      })
      output$range <- renderPrint({
        paste('lnalpha', input$lnalpha, 'beta', input$beta)
      })
      
      
    }
      )
  