#===============================================================================    
#  SR_model_Bayes.R  Shiny app for Bayesian Spawner Recruit Analyses 
#  This code read data, create brood table, conduct SR model and 
#  escapement goal analyses 
#  Author:  Toshihide "Hamachan" Hamazaki 
#      
# Naming conventions 
# Data  : Dat_xxxx 
# Shiny Output Plot  : Plt_xxxx
# Markdown Output Plot : Fig_xxxx
# Table : Tbl_xxxx
# Text  : Txt_xxxx 
# where xxx is a name of reactive object.  Reactive object is downloadable. 
#===============================================================================
#initialize
library(shiny)        # used for creating Shiny 
library(shinythemes)  # used to specify themes
library(shinyhelper)  # used to add quick help
library(bslib)
#library(markdown)     # used to read markdown file
library(rmarkdown)     # used to get rmarkdown file
library(knitr)
library(reshape2)     # used for data transpose 
library(datasets)   
library(lmtest)       # used for dwtest 
library(mgcv)         # used for spline 
library(coda)         # used to read MCMC data 
library(R2jags)       # used to run JAGS
library(openxlsx)     # used for creating EXCEL output table  
library(officedown)   # used to create Word doc
#library(flextable)    # used to make tables
library(ggplot2)      #ggplot
options(scipen=999)   # Do not show Scientific notation
source("Rcode/pointLabelBase.R")   # 
source("Rcode/addNonOverlappingTestLabelsOrPoints.R")   # 
source("Rcode/Shiny_modules.R")   #  Module codes 
source("Rcode/Shiny_SR_functions.R")   #  All functions created and used in this app 
source("Rcode/Shiny_Bayes_modules.R")  #  Modules related to Bayesian model 
source("Rcode/ggplot_theme.r")  # Include ggplot

#===============================================================================    
#  UI:  
#===============================================================================
ui<-fluidPage(
 navbarPage(
    theme =  bs_theme(version = 3, bootswatch = 'cerulean'), id = "tabs",
    title = div(
        img(src="Picture2.png",height=40, width=40)
        , "Pacific Salmon Escapement Goal Analyses"),

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 1:  Data Input and Submit 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#navbarMenu("Data Input & Bayes model",
  tabPanel("Data Input",
   sidebarPanel(width = 3,
#-------------------------------------------------------------------------------    
#   Data file Input
#-------------------------------------------------------------------------------
# Create brood and SR module
selectInput(inputId="dataType","Data Type", choices = c('Run','S-R','Escapement Only')),
# Sample data input 
checkboxInput(inputId="Sample", "Inport Sample Data", FALSE), 

# File Input module 
  dataInputUI("datain", "User data (.csv format)"),

# Show Age range (Only appear when data type is "Run")
  uiOutput('agerange'),

# Whether to combine or eliminate Ages (Only appear when data type is "Run")
  uiOutput('agecomb'),

# Limit Year range 
  uiOutput('yrange'),
#-------------------------------------------------------------------------------
  hr(),
# Default axis point UI 
 checkboxInput(inputId="autoui", "Defalut axis unit", TRUE),
 conditionalPanel(condition="input.autoui== false",
    selectInput(inputId="ui","Figure Axis Dislpay Unit", choices = c('1','1000','million'))
      )
   ), # End sidebarPanel (Data Input)

#=========================MainPanel=============================================
  mainPanel(
    tabsetPanel(id="subTab",
#------------------ Show Input data --------------------------------------------      
      tabPanel("Table",
               strong(htmlOutput('note')),
               h2(uiOutput('note2')),
               dataTableOutput('Tbl_data')),
#------------------ Brood Table ------------------------------------------------      
      tabPanel("Brood Table",
                dataTableOutput("Tbl_data.brood")
                  ), # End tabPanel
#------------------ Time Series ------------------------------------------------
      tabPanel("Time Series",
              plotOutput("Plt_srt"),
              plotOutput('Plt_runesc')  
              # Activates only when run is available
                ),#End tabPanel
#------------------ Data summary  ----------------------------------------------
      tabPanel("Summary",
               verbatimTextOutput('Txt_sum.data'),
               plotOutput('Plt_hist.sry')
             ), # End tabPanel
      tabPanel("Help", (includeMarkdown("documents/Input_help.md"))
          )  # End tabPanel
        )  # End tabsetPanel
      )  # End mainPanel
     ), # End Data Input tabPanel

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2 Escapement Only Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
tabPanel("Escapement Only Analyses",
    sidebarPanel(width = 3,
      conditionalPanel(condition="input.ePanel == 'Percentile Analyses'",  
        p(strong("Percentile Analyses")),
        PercentileUI("prcnt"),   # Percentile Analyses Module
        (htmlOutput("Txt_Tier")),
              hr(),
        (htmlOutput("Txt_Note"))
                ), # End conditionalPanel
      conditionalPanel(condition="input.ePanel == 'Risk Analyses'",                        
        p(strong("Risk Analyses")), 
        strong(textOutput("Txt_Risk_Model")),
              hr(),
        RiskUI("risk"),   # Risk Analyses Module
#        strong(htmlOutput('Txt_Risk'))
                 ) # End conditionalPanel
          ), #End SidbarPanel
#============= Main Panel ======================================================    
  mainPanel(
    tabsetPanel(
#------------------ Percentile Analyses ----------------------------------------   
      tabPanel("Percentile Analyses",
        plotOutput("Plt_prcnt")   
            ), #End tabPanel:Percentile
#------------------ Risk Analyses  ---------------------------------------------   
      tabPanel("Risk Analyses",
        plotOutput(height = '400px', "Plt_risk"),
        plotOutput(height = '300px', "Plt_risk2"),
        tableOutput('Tbl_risk'),
        tableOutput('Tbl_riskp'),
          p(strong("Durbin-Watson Serial Correlation Analyses")),
          verbatimTextOutput('Txt_dwtest')
            ), #End tabPanel: Risk
#        tabPanel("Risk Table",
#                 ),
        tabPanel("Help",
          withMathJax(               
            includeMarkdown("documents/ESC_Analyses_help.md")
              )
            ), #End tabPanel: Help
          id = "ePanel"
           )#End tabsetPanel
         )#End main Panel 
  ),#End tabPanel Escapement only Analysis 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3  Data Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("SR Model",
    sidebarPanel(width = 3,
#---------  Bayes Model Control Sidebar ----------------------------------------                 
      conditionalPanel(condition="input.Panel == 'Bayes Model'",
        p(strong("Bayesian Model Setting")),
           selectInput('Model',"Select SR Model",choices=c('Ricker','Beverton-Holt'),
                    selected ='Ricker'),
           radioButtons(inputId="add","Model Addition",
            choices=c("None"="none","AR(1) Error"="ar1","Time varying alpha"="kf"),
            selected = NULL),
#-------------------------------------------------------------------------------    
# UI:  State-Space Model Control UI
#-------------------------------------------------------------------------------
conditionalPanel(condition="input.dataType == 'Run'",  
# checkboxInput(inputId="SS", ("State-Space Modeling"), FALSE), 
       conditionalPanel(condition="input.SS == true",
       p("Select Column name from data file"),
    # Input: Select what to display
       selectInput(inputId="cv_N","Run CV", choices = ""),
       selectInput(inputId="cv_E","Escapement CV", choices = ""),
       selectInput(inputId="cv_H","Harvest CV", choices = ""), 
       selectInput(inputId="efn","Effective Age sample size", choices = "")
        )
     ),              

#-------------------------------------------------------------------------------    
# UI:  Bayes Model Control UI
#-------------------------------------------------------------------------------
 hr(),
 p(strong("Bayes Model Setting")),
  checkboxInput(inputId="BayesMCMC", strong("Import MCMC"), FALSE), 
   conditionalPanel(condition="input.BayesMCMC == false",
      BayesInputUI('Bayes'),
      hr(),
      downloadButton('downloaddMCMC', label='MCMC download')
      ),
   conditionalPanel(condition="input.BayesMCMC == true",
      dataInputUI("mcmc.in", "User data (.csv format)"),
                    )              
                 
          ),  # End conditional Panel
#---------  SR Model Sidebar ----------------------------------------
    conditionalPanel(condition="input.Panel == 'SR Plot'|| input.Panel == 'Yield Plot'", 
        radioButtons(inputId="target","Management Target Option",
          choices=c("Median Recruit"="md","Mean Recruit"="me"),
              selected = 'md'), 
        uiOutput('astar'),  
        p(strong("Plot options")),               
        checkboxInput(inputId="show.points", "show Years", TRUE), 
        checkboxInput(inputId="show.smsy", "show Smsy", FALSE),
        checkboxInput(inputId="show.smax", "show Smax", FALSE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
        sliderInput("CI", "% Interval", value=90,min=0,max=100,step=5),
        selectInput(inputId="Li","Interval Type", choices = c('credible','prediction'))
        ),
       conditionalPanel(condition="input.Panel == 'MCMC'", 
       sliderInput(inputId="CIB", "% Interval", value=90,min=0,max=100,step=5)
       )   
# End Conditional panel  
       ), # End sidebarPanel
       
#=========================MainPanel=============================================       
  mainPanel(tabsetPanel(
#------------------ Bayes Model ------------------------------------------------        
    tabPanel("Bayes Model",
        p(strong("Trace Plots")),
        plotOutput("Plt_trace"),
        p(strong("Model summary")), 
        verbatimTextOutput('BayesSum')  
          ),#End tabPanel:Bayes
#------------------ SR Plot-----------------------------------------------------        
    tabPanel("SR Plot",
        plotOutput(height='500px','Plt_SR')
            ),#End tabPanel: SR 
#------------------ Yield Plot--------------------------------------------------        
    tabPanel("Yield Plot",
        plotOutput(height='500px','Plt_yield')
            ),#End tabPanel: Yield
#------------------ Residuals  ------------------------------------------------- 
    tabPanel("Diagnoses", 
        plotOutput(height='300px',"Plt_predict"),     
        plotOutput(height='300px',"Plt_residual"),
        plotOutput(height='300px',"Plt_lnalphai")
            ),#End tabPanel: Diagnoses
#------------------ MCMC ------------------------------------------------------- 
    tabPanel("MCMC",
        p(strong("Outliers Removed")),     
        verbatimTextOutput('sumpost'),
        plotOutput("Plt_hist.mc")
            ),#End tabPanel: MCMC
#------------------ Bayes Model Code ------------------------------------------- 
    tabPanel("Model Codes",
        verbatimTextOutput('modelcode')
            ),#End tabPanel: Model Code
#    tabPanel("MCMC data",
#      dataTableOutput('Tbl_mcmcdata')
#            ),#End tabPanel: Model Code
     tabPanel("Help",
        withMathJax(includeMarkdown("documents/JAGS_help.md"))
            ), # End tabPanel: Help
      tabPanel("SR Help",
         withMathJax(includeMarkdown("documents/SR_help.md"))
        ), # End tabPanel: Help
id = "Panel"
           )#End tabsetPanel
        )#End mainPanel
      ),#End SR Model tabPanel


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 4: Escapement Goal Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

navbarMenu("Escapement Goal Analyses",
#===============================================================================    
#  tabPanel: Smsy-Smax Goal Analyses 
#===============================================================================    
  tabPanel("Smsy & Smax Goal Analyses",      
    sidebarPanel(width = 3,
       ProfileUI('smsy','MSY'),
       hr(),
       ProfileUI('smax','Rmax'),
          ),#End sidebarPanel
#=========================MainPanel=============================================       
    mainPanel(
      tabsetPanel(
#------------------ Smsy Profile ----------------------------------------------- 
        tabPanel("Profile",
           plotOutput(height='400px','Plt_Smsy_prof'),  
           plotOutput(height='400px','Plt_Smax_prof') 
            ), #End tabPanel: Profile
#------------------ Smsy Yield Profile -----------------------------------------
        tabPanel("Yield & Recruit",
          plotOutput(height='300px','Plt_yield.pg'),
          plotOutput(height='300px','Plt_rec.pg')
              ), #End tabPanel: YieldRec
        tabPanel("Goal Ranges",
                p(strong("Smsy Goal Range")),        
#          htmlOutput("Txt_Srange.smsy"),
                p(strong("Smax Goal Range")), 
#          htmlOutput("Txt_Srange.smax")
              ), #End tabPanel: Goal 
        tabPanel("Help",
          withMathJax(includeMarkdown("documents/Profile_help.md"))
                ) #End tabPanel: Help  
            )#End tabsetPanel
        )#End mainPanel
      ),#End tabPanel: Smsy Goal Analyses 
 
#===============================================================================    
#  tabPanel Recruit & Yield Goal Analyses 
#===============================================================================    
  tabPanel("Yield & Recruit Goal Analyses",
    sidebarPanel(width = 3,
      conditionalPanel(condition="input.cPanel == 'Recruit Goal Analyses'",  
        p(strong("Recruit Goal Analyses")), 
        uiOutput('minRec'),
        sliderInput("r1p", "Min % Achieve", value=90,min=0, max=100,step=5)
        ), # End conditionalPanel
      conditionalPanel(condition="input.cPanel == 'Yield Goal Analyses'",                        
        p(strong("Yield Goal Analyses")),
        uiOutput('minYield'),
        sliderInput("y1p", "Min % Achieve", value=90,min=0, max=100,step=5)
        ) # End conditionalPanel
      ),  # End sidebarPanel

#=========================MainPanel=============================================       
    mainPanel(
      tabsetPanel(
#------------------ Yield Goal  Profile ----------------------------------------                
        tabPanel("Yield Goal Analyses",
            plotOutput(height='300px','Plt_yield.gl'),
            plotOutput(height='300px','Plt_yield.prof'),
            verbatimTextOutput("Txt_Yield_gl")
        ),
#------------------ Recruit Goal Profile ---------------------------------------   
        tabPanel("Recruit Goal Analyses",
          plotOutput(height='300px','Plt_rec.gl'),
          plotOutput(height='300px','Plt_rec.prof'),
          verbatimTextOutput("Txt_Rec_gl")
          ),# End tabPanel
        tabPanel("Help",
          withMathJax(includeMarkdown("documents/Yield_Recruit_help.md"))        
        ),# End tabPanel
        id = "cPanel"
          )#End tabsetPanel
        )#End maiPanel
      ),#End tabPanel Recruit & Yield Goal Analyses 
              
#-------------------------------------------------------------------------------    
# tabPanel Custom Escapement Goal Evaluation 
#-------------------------------------------------------------------------------    
  tabPanel("Custom Escapment Goal Range",
    sidebarPanel(width = 3,
      p(strong("Select Lower and Upper Escapement Goal")),
      fluidRow(
      column(6,uiOutput('minEG')),  
      column(6,uiOutput('maxEG'))
       ),
      p("Submit Goal Range for  Analyses"),
      actionButton("Run","Run"),
# Horizontal line ----
      hr(),
      uiOutput('cyg'),
      uiOutput('crg')
                       ), #End Sidepar Panel
#============= Main Panel ======================================================    
    mainPanel(
      tabsetPanel(
#------------------ Profile ----------------------------------------------------   
        tabPanel("Profile Analyses",
          plotOutput(height='400px','plt_msyprof_c'),
          plotOutput(height='400px','plt_maxprof_c')
        ), #End tab Panel
                 
#------------------ Expected Mean Recruit and Yields ---------------------------   
        tabPanel("Expected Yield",
          plotOutput(height = '300px', 'Plt_yield.cg'),      
          plotOutput(height='300px',"Plt_Yield_EG"),
            splitLayout(cellWidths = c("50%", "50%"),
              p(strong("Yields Summary")),
              p(strong("Probability of Meeting Target"))),
            splitLayout(cellWidths = c("50%", "50%"),
              verbatimTextOutput("Txt_Yield_cg"),
              verbatimTextOutput("Txt_Yield_pb_cg"))
          ), #End tab Panel
#------------------ Recruit Annual Recruit and Yields --------------------------   
      tabPanel("Expected Recruit",
          plotOutput(height = '300px', 'Plt_rec.cg'),         
          plotOutput(height ='300px',"Plt_Rec_EG"),
            splitLayout(cellWidths = c("50%", "50%"),
              p(strong("Recruit Summary")),
              p(strong("Probability of Meeting Target"))),
            splitLayout(cellWidths = c("50%", "50%"),
              verbatimTextOutput("Txt_Rec_cg"),
              verbatimTextOutput("Txt_Rec_pb_cg"))
          ), #End tab Panel
      tabPanel("Goal Comparison",
               tableOutput('Tbl_sim')
        ),# End tabPanel
      tabPanel("Help",
        withMathJax(includeMarkdown("documents/Custom_Escapement_help.md"))     
          )# End tabPanel
        )#End tabsetPanel
      )#End main Panel 
    ) #End tabPanel 
    ),#End Escapement Goal Analyses navbarMenu
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 4  Management Strategy Evaluation 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
navbarMenu("MSE Analyses",
#-------------------------------------------------------------------------------    
#  Simulation model UI
#-------------------------------------------------------------------------------  
 tabPanel("Simulation Model",
  sidebarPanel(width = 3,        
   conditionalPanel(condition="input.MSEPanel == 'Simulation Run'",           
    p(strong("Escapement Goal")), 
     fluidRow(
      column(6,uiOutput('LEG')),  
      column(6,uiOutput('UEG'))
      ),
    p(strong("Fishery Target & Capacity")),  
     fluidRow(
      column(6,uiOutput('minH')),  
      column(6,uiOutput('maxH')),
      ),
    p(strong("Management Option")),  
     fluidRow(
      column(6,
        selectInput(inputId="strType","Priority", 
          choices = c('Escapement','Harvest','Hybrid'),selected = 'Escapement')
             ),
      column(6,
        selectInput(inputId="cmode","Target ESC", 
          choices = c('Lower','Middle','Upper'),selected = 'Middle')
             ),  
          ),
#    numericInput(inputId="EG.rev", "Escapement Goal Review Frequency", value=0,,min=0,max=20,step=1),
     fluidRow(
       column(6,uiOutput('nsim')),  
         ),
    actionButton("SimRun","Simulate"),
    actionButton("show", "Help"),   
#                     ),
  p(strong("Management % Error")),  
    fluidRow(
     column(6,numericInput(inputId="spred", "Run Assessment", value=15,min=0,max=100,step=5),
      ),
     column(6,numericInput(inputId="simpH", "Inplementation", value=10,min=0,max=100,step=5),
      ),
    ),
    
  #    sliderInput(inputId="sobsH", "Harvest Observation %E", value=10,min=0,max=100,step=5),
  #    sliderInput(inputId="sobsE", "Escapement Observation %E", value=30,min=0,max=100,step=5),
    
  sliderInput(inputId="simy", "Management Years", value=50,min=0,max=100,step=5,round=0)
      ),
  conditionalPanel(condition="input.MSEPanel == 'Sim Summary'",
    p(strong("Consecutive Years not meeting target")),
    numericInput(inputId="conLEsc", "Escapement", value=3,min=1,max=10),
    numericInput(inputId="conminH", "Minimum Harvest", value=3,min=1,max=10),
    numericInput(inputId="con0H", "No Fishery", value=3,min=1,max=10),
      ),
conditionalPanel(condition="input.MSEPanel == 'Sim replicates'",
  uiOutput('sim.rep')
 )
      ), #End sidebarPanel
                    
#---------------- mainPanel ----------------------------------------------------
 mainPanel(
  tabsetPanel(
#-------------------------------------------------------------------------------    
#  Simulation Run 
#-------------------------------------------------------------------------------        
    tabPanel("Simulation Run",
      fluidRow(  
        selectInput(inputId="pltmse","Plot", 
                    choices = c('Run','Escapement','Harvest'),selected = 'Run'),
        plotOutput(height='500px',"Plt_mse")
           ),
#      fluidRow( 
#          column(3, actionButton("InitRun","Initialize")),
#          column(3, actionButton("SimRun","Simulate")),
#          column(3,actionButton("SimClear","Clear Results"))
#                  ),
        verbatimTextOutput("Txt_sum.mse"),
        plotOutput("Plt_sum.mse")
                  ), #End tabPanel
    tabPanel("Sim Summary",
      verbatimTextOutput('sims.out2') ,
       verbatimTextOutput("Txt_HE_mse"),
       plotOutput('Plt_freq_mse')
                  ), #End tabPanel
                        
    tabPanel("Sim replicates",
           plotOutput(height='500px','Plt_mse_rep'),  
           verbatimTextOutput("Txt_rep.mse"),
           plotOutput("Plt_rep.mse"),
           verbatimTextOutput('sims.out'),
           verbatimTextOutput('sims.out.rep') ,  
#          dataTableOutput('Tbl_mse')
#          plotOutput(height='600px',"altsim.N"), 
          downloadButton("simdownload", "Download")
                  ),  #End tabPanel
                        
#------------------ Model Parameters ---------------------------------   
    tabPanel("Model Parameters",
      fluidPage(
            title = 'Set MSE Simulaiton Initization Parameters',
            hr(),
      fluidRow(
        column(4,
          p(strong("Simulation Years")),           
#            sliderInput(inputId="burnin", "Burnin", value=25,min=0,max=100,step=5,round=0),
#            sliderInput(inputId="train", "Training", value=25,min=0,max=100,step=5,round=0),
#            sliderInput(inputId="simy", "Management", value=50,min=0,max=100,step=5,round=0)
                  ),
        column(4,
          p(strong("Errors")),       
#           sliderInput(inputId="spred", "Preseason Run prediction %E", value=20,min=0,max=100,step=5),
#           sliderInput(inputId="simpH", "Management Implementation %E", value=10,min=0,max=100,step=5),
#           sliderInput(inputId="sobsH", "Harvest Observation %E", value=10,min=0,max=100,step=5),
#           sliderInput(inputId="sobsE", "Escapement Observation %E", value=30,min=0,max=100,step=5),
#           sliderInput(inputId="Nobage", "Age Comp Sample size", value=100,min=10,max=500,step=10)
                  ),
        column(4,
          p(strong("Population Errors")), 
#           sliderInput(inputId="phi", "AR1 correlation", value=0.6,min=0,max=1,step=.1),
#           sliderInput(inputId="D", "Drishelet D", value=50,min=0,max=200,step=10)
                )
              ) # End fluidRow
            )# End fluidOPage
          ), # End tabPanel
         id = "MSEPanel"
        )#End tabsetPanel
      )#End mainPanel
    ),#End tabPanel Simulation Model 
           
#------------------ Model Descriptions -----------------------------------------   
    tabPanel("Help",
      tabsetPanel(
#------------------ Model Structure  -------------------------------------------             
      tabPanel("Model description",
                       includeMarkdown("documents/MSE_help.md")
                      ),  #End tabPanel
#------------------ Parameters Description ---------------------------------             
      tabPanel("Base model Parameters",
                    h3("Simulation length"),
                    p("- Burnin: Years to make modle stabilize "),
                    p("- Training: Years SR data are collected befre setting active management"),
                    p("- Managmenet: Years active mangement is conducted"),
                    h3("Management Errors"),
                    p("Fishery management takes following steps: 1) predict preseason run size, 
              2) determin harves target, 3) execute harvests, and 
              4) observe harvest and escapetnt to set and escapement goal. The model incorporates errors
                  associated with each step.  Errors were modeled as independent log-normal"),
                    p("- Preseaon Run prediction: Accuracy +/-  x%"),
                    p("- Management Implementation: Accuracy +/-  x%"),
                    p("- Harvest Observation: Accuracy +/-  x%"),
                    p("- Escapement Observation: Accuracy +/-  x%"),
                    p("Observed postseason run size is Observed harvest + escapement"),
                    h3("Poplation Dynamic"),
                    p("- AR1 corelation: Recuruitment error was modeled as AR1 with sigma and phi (correlation),
              sigma is derived from SR model fit."),
                    p("- Drishelet D. Brood age proportion is modeled as Drishelet distrribution
              D determines level of variation. Lower D indicate higher variation")
                      ) # End tabPanel
                    )# End tabsetPanel
           )#End tabPanel 
    ),#End MSE navMenue Panel

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 5  Report Output section 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("Report Output",   
        sidebarPanel(
        downloadButton('downloadReport')
        ),
   mainPanel(
    h1("Under Construction")
#    withMathJax(               
#      includeMarkdown("report.Rmd")
#    )
  )
),  # End (Report Output panel)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 6  Version History 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("Version History",  
  )
 
 ),#End nabVarPage
#-------------------------------------------------------------------------------
# Citation Disclaimer  
#-------------------------------------------------------------------------------
hr(),
h5("Disclaimer"),
print(strong("This App is developed by Toshihide Hamachan Hamazaki, Alaska Department of Fish and Game Division of Commercial Fisheries")),
h5("Contact about this applicaiton"), 
print(strong("Questions and improvement suggestions? Please contact",
a(href="mailto:toshihide.hamazaki@alaska.gov", "Hamachan"))),
h5("Suggested Citation"),
print(strong(paste("Hamazaki, T.",format(Sys.Date(), "%Y"),". Pacific salmon escapement goal analyses (source: https://hamachan.shinyapps.io/Spawner_Recruit_Bayes/)"))),
h5("Other Models"),
p(strong("Missed escapement passage estimation:",
         a(href="https://hamachan.shinyapps.io/Missed_Run/", "Missed Passage")))
) #End fluidPage

#===============================================================================    
#  Server:  
#===============================================================================
server<-shinyServer(function(input, output, session){
#-------------------------------------------------------------------------------
# ggplot theme set 
#theme_set(theme_simple()) 
#-------------------------------------------------------------------------------
  output$note <- reactive({
   HTML(
   if(input$dataType== "S-R"){
      paste("S-R Data file column orders:", "Year, Spawner (Escapement), Recruit",sep = '<br/>')
    } else if(input$dataType== "Run")  {
      paste("Run Data file column orders: Year, Escapement, Run, Run by age (or proportion), cv_N, cv_E, cv_H,efn", 
            "Age name MUST be written as 'A + run age' (A3) or 'a+European scale age' (a1.1) ",
            "cv_N, cv_E, cv_H: cv estimate of run, escapement, harvest", 
            "efn:  Effective sample size (must be integer)",
            sep = '<br/>')
    } else if(input$dataType== "Escapement Only") {
      paste("Escapement only Data file column orders: Year, Escapement")
   }
    ) # End HTML
  })
output$note2 <- renderUI({
  if(input$dataType== "Run" & is.null(age.out(data()))){
    paste("Incorrect age column names: Please correct.")
  } 
})
#-------------------------------------------------------------------------------
#  Tab Control  
#-------------------------------------------------------------------------------
observe({
  if(input$dataType=="Run") {
    hideTab(inputId = "tabs", target = "Escapement Only Analyses")
    showTab(inputId = "tabs", target = "Escapement Goal Analyses")
    showTab(inputId = "tabs", target = "MSE Analyses")
    showTab(inputId = "tabs", target = "SR Model")
    showTab(inputId = "subTab", target = "Brood Table")
  } else if (input$dataType== "S-R") {
    hideTab(inputId = "tabs", target = "Escapement Only Analyses")
    showTab(inputId = "tabs", target = "Escapement Goal Analyses")
    hideTab(inputId = "tabs", target = "MSE Analyses")
    showTab(inputId = "tabs", target = "SR Model")
    hideTab(inputId = "subTab", target = "Brood Table")
  } else {
    showTab(inputId = "tabs", target = "Escapement Only Analyses")
    hideTab(inputId = "tabs", target = "Escapement Goal Analyses")
    hideTab(inputId = "tabs", target = "MSE Analyses")
    hideTab(inputId = "tabs", target = "SR Model")
    hideTab(inputId = "subTab", target = "Brood Table")
  }
 })  

# Observe Year, Spawner, Recruit Column name
observe({
  var.opts<-names(data())
  updateSelectInput(session, inputId="cv_N", choices = var.opts)
  updateSelectInput(session, inputId="cv_E", choices = var.opts)
  updateSelectInput(session, inputId="cv_H", choices = var.opts)
  updateSelectInput(session, inputId="efn", choices = var.opts)
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Data upload and output 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Convert unit to numbers. 
#---- UI Output Minimum Yield --------------------------------------------------
  unit <- reactive({
    if(input$autoui==TRUE){
      d <-  floor(log10(max(e.data.0()$S,na.rm=TRUE)))
      u <- ifelse(d>=6,10^6,ifelse(d>3,10^3,1))
      } else {
      u <- ifelse(input$ui=='million',1000000,as.numeric(input$ui))
      }
    return(u)
     })
  
# Data file reading module  
data1 <-  dataInputServer("datain")

# Choose data 
  
data <- reactive({
  if(isTRUE(input$Sample)){
  if(input$dataType== "Run"){out <- read.csv('Sample_data/Sample_Run_data.csv',header=T)} 
  else if(input$dataType== "S-R"){out <- read.csv('Sample_data/Sample_SR_data.csv',header=T)} 
  else if(input$dataType== "Escapement Only"){out <- read.csv('Sample_data/Sample_Esc_data.csv',header=T)} 
    }
  else {
   out <- data1()
  }
  return(out)
})
      

# table ---- Uploaded data table output ----------------------------------------
  output$Tbl_data <- renderDataTable({data()})  

# UI ---- age range ------------------------------------------------------------
#  observe({
#    if(input$dataType== "Run" & is.nul(age.out(data())))
#      showModal(modalDialog(
 #      title = "Somewhat important message",
#        "This is a somewhat important message.",
 #       easyClose = TRUE,
#        footer = NULL
#      ))
#  })
  
  output$agerange <- renderUI({
    if(input$dataType== "Run"){
     age <-  age.out(data())
     if(is.null(age)){
       renderText({
         paste("Wrong age data format")
       })
     }else{
     fage <- min(age)       # First brood year 
     lage <- max(age)       # Last brood year
    #  Slider input UI 
    sliderInput("rage", label = paste("Select","Run Age Range"), min = fage, max = lage, value = c(fage, lage),step=1,sep = "")
     }
    }
    })
  
  output$agecomb <- renderUI({
    if(input$dataType== "Run" & length(input$rage[1])>0){
      #  Slider input UI 
      checkboxInput("combage", label = "Pool Ages", value = TRUE)
        } 
  })
  
  
# brood.table--- Construct brood table (when dataType is "Run") ----------------   
  brood.out <-  reactive({
    if(input$dataType== "Run"){
      agedata <- make.age(data(),input$rage[1], input$rage[2],input$combage)
      data <- cbind(data()[,1:3],agedata)
      brood <- make.brood(data,input$rage[1])
      return(brood)
    } else{NA}
  })
  
# Tbl_data.brood ----- show brood table ----------------------------------------
output$Tbl_data.brood <- renderDataTable({
    if(input$dataType== "Run"){
    round(brood.out()$brood,0)
    }  
  }) 
  
# brood age comp----------------------------------------------------------------
  brood.p <- reactive({
    if(input$dataType== "Run"){
     brood <- brood.out()$brood
     p.brood <- brood[complete.cases(brood),]
     ncol <- dim(p.brood)[2]
     pbrood <- p.brood[,-c(1,2,ncol)]/p.brood$Recruit
     return(pbrood)
     }
   })
   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Create SR data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Original SR data -------------------------------------------------------------  
 sr.data.0 <- reactive({
  req(input$dataType)
    if(input$dataType== "Run"){
      x <- brood.out()$SR
    } else if (input$dataType== "S-R"){
      x <- data()
      x <- x[complete.cases(x),]
    }
    names(x) <- c('Yr','S','R')
    return(x)   
  })

# Original Escapement data -----------------------------------------------------  
 e.data.0 <- reactive({
      x <- data()[,c(1:2)]
    names(x) <- c('Yr','S')
    return(x)     
  })  

# yrange --- UI output to determine data year range -----------------------------
output$yrange <- renderUI({
  if(input$dataType== "Escapement Only"){
    name <- 'Run'
    year <- e.data.0()$Yr    # Extract brood year data range 
  }else{
    name <- 'Brood'
    year <- sr.data.0()$Yr   # Extract brood year data range     
  }
    fyear <- min(year)       # First brood year 
    lyear <- max(year)       # Last brood year
    #  Slider input UI 
    sliderInput("sryears", label = paste("Select",name,"Year Range"), min = fyear, max = lyear, value = c(fyear, lyear),step=1,sep = "")
  })
  
# sr.data --- final dataset used for SR analyses -------------------------------
  sr.data <- reactive({ cut.data(sr.data.0(),input$sryears) })

# e.data --- final dataset used for percentile risk -----------------------------
  e.data <- reactive({ cut.data(e.data.0(),input$sryears) })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Plot SR data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 plot_runesc <- function(dat,u){
     par(yaxs='i',bty='l')
     plot(R/u~Yr,data=dat,type='l',ylim=c(0,with(dat,max(R,S,na.rm =TRUE)/u)),xlab='',ylab='')
     lines(S/u~Yr,data=dat,lty=2)
 }
#plot_runesc <- function(dat,u){
# ggplot() + ylim(0, 100)+c(0,with(dat,max(R,S,na.rm =TRUE)/u))+     
# geom_line(data = dat, aes(x=Yr,y=R/u))+geom_line(data = dat, aes(x=Yr,y=S/u),linetype = "dashed")
#}

# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") --------------
# Plot object: runesc 
# runesc <- reactive({
#   if(input$dataType== "Run"){
#      x <- data()[,c(1:3)]
#      names(x) <-c('Yr','S','R')
      # Calculate Harvest rate 
#      x$hrate <- with(x,(R-S)/R)
#      u <- unit()
#      p1 <- plot_runesc(x,u)
#      p1 <- p1 + 

#      plot(hrate~Yr, data=x,type = "l", xaxt = "n",yaxt = "n",xlab='',ylab='',ylim=c(0,1),col=2)
#      axis(side = 4)
#      mtext("Harvest rate",side=4,line=2.5,las=0)
#      title("Run and Escapement", xlab="Year",
#            ylab=paste('Run / Escapement',mult(u))) 
#      add_legend("topright",legend=c('Run','Esc','H rate'),lty=c(1,2,1),
#             col=c(1,1,2), box.lty=0,xpd=TRUE)  
#----  Plots Output ------------------------------------------------------------  
#      out <-recordPlot()  
#      return(out)    
#     }
#   })

 runesc <- reactive({
   if(input$dataType== "Run"){
     layout(matrix(1:2, ncol=2), widths=c(4,1))
     x <- data()[,c(1:3)]
     names(x) <-c('Yr','S','R')
     u <- unit()
     par(mar=c(4,4,4,4),las=1)
     plot_runesc(x,u)
     # Calculate Harvest rate 
     x$hrate <- with(x,(R-S)/R)
     par(new = TRUE)
     plot(hrate~Yr, data=x,type = "l", xaxt = "n",yaxt = "n",xlab='',ylab='',ylim=c(0,1),col=2)
     axis(side = 4)
     mtext("Harvest rate",side=4,line=2.5,las=0)
     title("Run and Escapement", xlab="Year",
           ylab=paste('Run / Escapement',mult(u))) 
     add_legend("topleft",legend=c('Run','Esc','H rate'),lty=c(1,2,1),
                col=c(1,1,2), box.lty=0,xpd=TRUE)  
     #----  Plots Output ------------------------------------------------------------  
     out <-recordPlot()  
     return(out)    
   }
 })
 
output$Plt_runesc <- renderPlot({runesc()})

# Plt_agecompr --- Plot Rum Age comp Time series (when data is "Run") ----------
output$Plt_agecompr <- renderPlot({
  if(input$dataType== "Run"){
    x <- data()
    names(x) <-c('Yr','S','R')
    u <- unit()
    plot_runesc(x,u)
    legend('topright',c('Run','Escapement'),lty=c(1,2),box.lty=0)  
    title("Run and Escapement", xlab="Year",
          ylab=paste('Run / Escapement',mult(u)))  
  }
})


# Plt_srt ---------- Plot SR time series ---------------------------------------
srt <- reactive({
  if(input$dataType != 'Escapement Only'){
  layout(matrix(1:2, ncol=2), widths=c(4,1))  
    x <- sr.data.0()
    u <- unit()
  par(yaxs='i',bty='u',las=1,mar=c(4,4,4,4))
   plot(R/u~Yr,data=x,type='l',ylim=c(0,with(x,max(R,S,na.rm=TRUE)/u)),xlab='',ylab='')
   lines(S/u~Yr,data=x,lty=2)
  par(new = TRUE)
   plot((R/S)~Yr, data=x,type = "l", ylim=c(0,with(x,max(R/S,na.rm=TRUE))),xaxt = "n",yaxt = "n",xlab='',ylab='',col=4)
   axis(side = 4)
   mtext("R/S",side=4,line=2.5,las=0)
   title("Spawner and Recruit", xlab="Brood Year", ylab=paste('Spawner / Recruit',mult(u))) 
   # Add Cutting data 
   if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
#     with(x,polygon(c(min(Yr),min(input$sryears),min(input$sryears),min(Yr)),c(0,0,max(R/u),max(R/u)),col=tcol('grey',50),border=NA))
#     with(x,polygon(c(max(Yr),max(input$sryears),max(input$sryears),max(Yr)),c(0,0,max(R/u),max(R/u)),col=tcol('grey',50),border=NA))
     abline(v=input$sryears,col=2)
     } 
   add_legend('topleft',c('Spawner','Recruit','R/S'),lty=c(2,1,1),col=c(1,1,4),box.lty=0,xpd=TRUE)  
    } else {
    x <- e.data.0()
    u <- unit()
    par(yaxs='i',bty='l',las=1)
    plot(S/u~Yr,data=x,type='l',ylim=c(0,with(x,max(S/u,na.rm=TRUE))),xlab='',ylab='')
    title("Escapement", xlab="Year",
          ylab=paste('Escapement',mult(u))) 
    # Add Cutting data 
    if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
      abline(v=input$sryears,col=2)
   }

  }
#----  Plots Output ------------------------------------------------------------  
  out <-recordPlot()  
  return(out)      
})

output$Plt_srt <- renderPlot({srt()})

# Plt_lnsrt ---------- Plot SR time series -------------------------------------
output$Plt_lnsrt <- renderPlot({
  x <- sr.data.0()
  u <- unit()
  plot_runesc(x,u)
  legend('topright',c('Spawner','Recruit'),lty=c(2,1),box.lty=0)  
  title("Spawner and Recruit", xlab="Brood Year",
        ylab=paste('Spawner / Recruit',mult(u)))  
  # Add Escapement Goal range  
  if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
    abline(v=input$sryears,col=2)
  }
})


# Plt_srtime ---------- Plot SR time series ------------------------------------
output$Plt_srtime <- renderPlot({
  x <- sr.data.0()
  RS <- with(x,log(R/S))
  names(RS) <- x$Yr
  stars <- stars(RS, L=10, p=0.05,h=2, AR1red="est", prewhitening = F)  
  test <- as.data.frame(stars$starsResult)
  par(yaxs='i',bty='l')
  plot(log(R/S)~Yr,data=x,type='l',xlab='',ylab='',laa=1)
  lines(x$Yr,test$mean,col=4,lwd=2)
  legend('topright',c('R/S'),lty=c(2,1),box.lty=0,xpd=TRUE)  
  title("ln(Recruit/Spawner)", xlab="Brood Year",
        ylab=paste('ln(Spawner/Recruit)'))  
  # Add Escapement Goal range  
  if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
    abline(v=input$sryears,col=2)
  }
})


# summary -------- sr data summary Output --------------------------------------
output$Txt_sum.data <- renderPrint({
  if(input$dataType != "Escapement Only"){
  dat <- sr.data()
  dat$Y <- dat$R-dat$S
  }else{
  dat <- e.data()    
  }
  summary(dat[,-1])  # Remove year 
  })

# Plt_hist.sry ---------- sr data histgranm ------------------------------------------- 
output$Plt_hist.sry <- renderPlot({
  if(input$dataType != "Escapement Only"){
  par(mfrow=c(1,3))
  x <- sr.data()
  x$Y <- x$R-x$S
  hist(x$S,main='',xlab='Spawnter')
  hist(x$R,,main='',xlab='Recruit')
  hist(x$Y,,main='',xlab='Yield') 
  } else {
    x <- e.data()  
    hist(x$S,main='',xlab='Escapement')    
  }
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2: Bayesian Model:  Create JAG data and model   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bayesedata --- Create data set for Bayesian modeling -------------------------
Bayesdata <- reactive({
  #  Import SR data 
  x <- sr.data()
  # nyrs is the number of brood years (i.e. number of rows) 
  nyrs <- dim(x)[1]
  R <- x$R
  S <- x$S
  # d is S multiplier
  d <- floor(log10(mean(S)))
  #  ar1: 1 if ar1 is included, 0 if not 
  if(input$add =='ar1'){ar1 <- 1} else {ar1 <- 0}
  if(input$add =='kf'){kf <- 1} else {kf <- 0}
  out <-list(nyrs=nyrs, S=S, R=R,d=d,ar1=ar1,kf=kf)
  return(out)
  })


# Select Bayes model 
Bayesmodel <- reactive({model_select(input$Model,input$add,input$SS)})

# Model Name 
model.name <- reactive({
  xp <- sr.data()
  add <- ifelse(input$add=="ar1","AR1",ifelse(input$add=="kf","TVA","ST"))
  ss <- ifelse(isTRUE(input$SS),'SS','')
  yrs <- paste(min(xp$Yr),'-',max(xp$Yr))
  model <- paste(input$Model,ss,add,yrs,sep='_')
  return(model)
 })


# Show model code
output$modelcode <- renderPrint({ Bayesmodel() })

#===============================================================================
#  Run JAG Model Module
#===============================================================================
#  Bayesmodel  Model section for JAG Models 
#  The function outputs: 
#  jagmodel: Selected JAG model
#  parameters: Model output parameters
#  model:  SR model used for simulation 
#===============================================================================
# Run Bayesian Model 
sim <- BayesInputServer('Bayes', Bayesdata, Bayesmodel)

# Model output 
run.JAGS <- reactive({sim()})

#-------------------------------------------------------------------------------
#  Extract JAG results 
#-------------------------------------------------------------------------------
# BayesSum ------------ Output MCMC sumaary ------------------------------------

output$BayesSum <- renderPrint({ print(run.JAGS()) })

# Plt_trace --------- Trace and density plot -----------------------------------
output$Plt_trace <- renderPlot({
  mcmc <- as.mcmc(run.JAGS())
#  mcmc <- run.JAGS()$Samples
  pars <- c('lnalpha','beta')
  if(input$add=='ar1') {pars <- c(pars,'phi')}
  par(mfrow=c(2,4))
  plot(mcmc[,pars],auto.layout=FALSE)
  })
#===============================================================================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3: SR Model Analyses   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data file reading module  
 mcmcdata <- dataInputServer("mcmc.in")
# SR.post --- Remove outliers
# Create SR parameters: alpha, beta, Seq, Smsy, Umsy, Smax ---------------------
 SR.post <- reactive({
  D <- as.numeric(Bayesdata()$d)
# Read mcmc data
  if(input$BayesMCMC==TRUE){
   post <- mcmcdata()
   } else {
  mcmc <- as.matrix(as.mcmc(run.JAGS()))
  post <- sim.out(mcmc,d=D,add=input$add,model=input$Model,model.br=Bayesmodel()$model.br)
   }
  return(post)
  })

# output mcmc data -------------------------------------------------------------
output$Tbl_mcmcdata <- renderDataTable({SR.post()}) 

# sumpost --- SR Parameter Summaries output------------------------------------- 
sumbayes <- reactive({
  ci <- input$CIB
  if(input$target =='me'){
  parname <-c('alpha.c','lnalpha.c','beta','Seq.c','Smsy.c','Umsy.c')
  if(input$add=='ar1'){parname <-c('alpha.c','lnalpha.c','beta','phi','Seq.c','Smsy.c','Umsy.c')}
  } else {
  parname <-c('alpha','lnalpha','beta','Seq','Smsy','Umsy')    
  if(input$add=='ar1'){parname <-c('alpha','lnalpha','beta','phi','Seq','Smsy','Umsy')}
  }
  if(input$Model == 'Ricker'){parname <- c(parname,'Smax')}
  if(input$add=='ar1'){r <- c(3,3,3,3,0,0,3,0)} else {r <- c(3,3,3,0,0,3,0)}
  t(round(t(sum.fun(SR.post()[,parname],ci)),r))
 })
# print out sumpost 
output$sumpost <- renderPrint({sumbayes()})

# Plt_hist.mc --- SR Parameters Density plots ----------------------------------
hist.mc <- reactive({
  D <- Bayesdata()$d
  if(input$add=='ar1'){ar1<- TRUE} else {ar1<- FALSE}
  plot_density(SR.post(),D,ar1,model=input$Model,target=input$target)
#--- Plot output----------------------------------------------------------------  
  out <-recordPlot()
  return(out)  
 })

output$Plt_hist.mc <- renderPlot({hist.mc()})

#===============================================================================
#  Time Variant allpha Analyses  
#===============================================================================
# lnalphais  Extract Time_variant alpha-----------------------------------------
lnalphais <-reactive({
 if(input$add=='kf'){
  nyrs <- Bayesdata()$nyrs
  year <- sr.data()$Yr
# Bayesian simulation out parameters -------------------------------------------    
 if(input$BayesMCMC==TRUE){
  parname <- paste0('lnalphai.',1:nyrs,'.')  # Reading CSV data 
   } else {
  parname <- paste0('lnalphai[',1:nyrs,']')  # Reading directly 
   }
# Extract lnalphai from posterior   
  lnalphai <- SR.post()[,parname]
# Mean lnalphai  
  lnalphai.mm <- apply(lnalphai,2,mean)
# Get 95% CI  
  cil <- apply(lnalphai,2,function(x) quantile(x, 0.025))
  ciu <- apply(lnalphai,2,function(x) quantile(x, 0.975))
# Calculate STARS using STARS function 
   names(lnalphai.mm) <- year
   stars <- stars(lnalphai.mm, L=10, p=0.05,  h=2, AR1red="est", prewhitening = F)  
   test <- as.data.frame(stars$starsResult)
# Combine data 
   lnalphai.m <- data.frame(cbind(year,lnalphai.mm,cil,ciu,test$mean))
   names(lnalphai.m) <- c('year','lnalphai.m','cil','ciu','star')
# Get unique year
   miny <- aggregate(year~star,data=lnalphai.m,min)
# order by small to large  
   miny <- miny[order(miny$year),]
# replace first one to first year
   miny$year[1] <- min(year)
   maxy <- aggregate(year~star,data=lnalphai.m,max)
# merge miny and maxy   
   cuty <- merge(miny,maxy,by='star')
   names(cuty) <- c('star','miny','maxy')
# Order from first year to last year    
   cuty <- cuty[order(cuty$miny),] 
# Add text range 
   cuty$txt <- paste0(cuty$miny,'-',cuty$maxy)
# Add number of years for each period
   cuty$ny <- cuty$maxy-cuty$miny+1
# Add cum  years for each period
   cuty$cy <- cumsum(cuty$ny)
# Add cum  years begin 
   cuty$cby <- cuty$cy-cuty$ny+1
  out <- list(lnalphai=lnalphai.m, stars=stars,cuty=cuty)
      } 
  return(out)
  })


output$Plt_lnalphai <- renderPlot({
  if(input$add=='kf'){
# Extract mean lnalpha for each year 
  xa <- lnalphais()$lnalphai
#  lnalphai.m <- xa$lnalphai.m
# Mean overall lnalpha  
  lnalpha <- mean(xa$lnalphai.m)
# Calculate STARS
# Plot figures   
  par(bty='l')
  with(xa,plot(lnalphai.m~year,type='l',lwd=2,main='time-varying lnalpha',
       xlab='Year',ylab='lnalpha',ylim=c(min(cil),max(ciu))))
  with(xa,polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)) 
  abline(h=lnalpha,lwd=2,col=2)
  lines(star~year,data=xa,col=4,lwd=2)
  } 
 })

# yrange --- UI output to determine data year range -----------------------------
output$astar <- renderUI({
 if(input$add=='kf'){
# Extract lnalhpha Star 
 cuty <- lnalphais()$cuty
 radioButtons(inputId="alphai","Time Variant alpha Select Periods",
               choices=c("None",cuty$txt),
               selected = "None")
  }
 })

# SR.pred.i ---- Time variant Bayesian Model Prediction ------------------------
SR.post.i <- reactive({
  req(input$alphai)
# Read mcmc data
  if(input$add=='kf' & input$alphai != 'None'){
    post <- SR.post()
    cuty <- lnalphais()$cuty
    period <- cuty[cuty$txt==input$alphai,c('cby','cy')] 
    if(input$BayesMCMC==TRUE){
      parname <- paste0('lnalphai.',period$cby:period$cy,'.')
    } else{
      parname <- paste0('lnalphai[',period$cby:period$cy,']')
    }
    
    # Extract lnalphai  
    lnalphai <- as.matrix(SR.post()[,parname])
    post$lnalpha <- apply(lnalphai,1, FUN=mean, na.rm=TRUE)
    post$sigma.lnalpha <- apply(lnalphai,1, FUN=sd, na.rm=TRUE) 
    post$lnalpha.c <- post$lnalpha+0.5*SR.post()$sigma^2
    return(post)
    }
})

#===============================================================================
#  Create SR Model Predictions    
#===============================================================================
# SR.resid ----- Model Residuals -----------------------------------------------
SR.resid <-reactive({
#---------- Select SR Model ----------------------------------------------------  
  srmodel <- Bayesmodel()$model
#---------- Extract MCMC SR Model Parameters -----------------------------------
  lnalpha <-SR.post()$lnalpha
  beta <- SR.post()$beta
  phi <- SR.post()$phi
  S <- Bayesdata()$S
  D <- Bayesdata()$d
  R <- Bayesdata()$R
  nyrs <- Bayesdata()$nyrs
#---------  Set up empty matrix ------------------------------------------------  
  ncol <- length(S) 
  nrow <- length(lnalpha)  #  Extract number of MCMC sample 
# Create Residuals  MCMC matrix    
  RD <- matrix(NA,nrow=nrow,ncol=ncol) 
  RD2 <- matrix(NA,nrow=nrow,ncol=ncol) 

#-------- Time variant alpha residual ------------------------------------------
if(input$add=='kf'){
  if(input$BayesMCMC==TRUE){
    parname <- paste0('lnalphai.',1:nyrs,'.')
  } else{
    parname <- paste0('lnalphai[',1:nyrs,']')
  }
    # Extract lnalphai  
  lnalphai <- as.matrix(SR.post()[,parname])
  for(i in 1:nrow){
      # Calculated expected Returns form each MCMC SR model parameters   
      Ey <- srmodel(lnalphai[i,],beta[i],S,D)
      RD[i,] <- log(R)-log(Ey) 
     }  
    } # End if 
#------ Others -----------------------------------------------------------------
       else {
  for(i in 1:nrow){
    # Calculated expected Returns form each MCMC SR model parameters   
    Ey <- srmodel(lnalpha[i],beta[i],S,D)
    RD[i,] <- log(R)-log(Ey)
         }  
       }
#------ Residuals for AR1 Remove AR1 correlation -------------------------------
  if(input$add=='ar1'){
      for(i in 1:nrow){
        RD2[i,2:ncol] <- RD[i,2:ncol] - phi[i]*RD[i,1:(ncol-1)]
        RD2[i,1] <- RD[i,1]-phi[i]*SR.post()$e0[i]
         }  
      } # End else
# Create residuals -----------------------------------------------------------  
  out <- list(RD=RD,RD2=RD2)
  return(out)
  })  

#-------------------------------------------------------------------------------
# SR.pred ---- Bayesian Model Prediction ---------------------------------------
#-------------------------------------------------------------------------------
SR.pred1 <-reactive({
  srmodel <- Bayesmodel()$model
#---------- Extract MCMC SR Model Parameters -----------------------------------
  D <- Bayesdata()$d
#---------- Determine model S length -------------------------------------------
  Seq <- quantile(SR.post()$Seq,0.9)   # Extract 90 percentile Seq
  max.s <- max(Seq,max(sr.data.0()$S)) # Extract max spawner 
  if(input$add == 'kf') {
    req(input$alphai)
    if(input$alphai == 'None'){
    out <- SR.pred.sim(SR.post(),D,max.s,srmodel,input$add)
    } else {
    out <- SR.pred.sim(SR.post.i(),D,max.s,srmodel,input$add)     
    }
  } else {
    out <- SR.pred.sim(SR.post(),D,max.s,srmodel,input$add)     
   }
  return(out)
 })  

SR.pred <-reactive({
  pred <-SR.pred1()
  out <- list()
  out$S <- pred$S
  if(input$target =='me'){
    out$R <- pred$R.c
    out$Y <- pred$Y.c
  } else {
    out$R <- pred$R
    out$Y <- pred$Y
  }
  out$R.p <- pred$R.p
  out$Y.p <- pred$Y.p
  return(out)
})  


# SRp ------ Model predicted mean, CI, PI  SR range ----------------------------
SRp <- reactive({
       pred_CI(SR.pred(),input$CI)
  })  


# downloadData ---- Results download -------------------------------------------
output$downloaddMCMC <- downloadHandler(
  filename = function() {
    paste0('MCMCdata_', model.name(),'_', Sys.Date(),'.csv')
  },
  content = function(file) {
    write.csv(as.data.frame(SR.post()), file,row.names = FALSE)
  }
)

#===============================================================================
#  Standard SR and Yield Plots and Tables Outputs  
#=============================================================================== 
base.pl <- reactive({
  u <- as.numeric(unit())
  x <- sr.data.0()
  xp <- x/u
  SRp <- SRp()/u
#---  Basic SR plot ------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l',las=1)
  plot(R~S,data=xp,pch=19,col='gray',cex=1.5, 
       xlab=paste("Escapement",mult(u)),ylab=paste('Recruit',mult(u)),
       xlim=c(0,max(SRp$S)),ylim=c(0,1.25*max(xp$R)))
  x2 <- sr.data()
  xp2 <- x2/u
  points(R~S,data=xp2,pch=19,col=1,cex=1.5)
  # Plot 1:1 line 
  abline(0,1)
  # Add Predicted   
  lines(RS.md~S,data=SRp,col=1,lw=2)
  lines(RS.me~S,data=SRp,col=1,lw=2,lty=2)
  out1 <-recordPlot()
#-------------------------------------------------------------------------------
#---  Basic Yield plot ---------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot((R-S)~S,data=xp,pch=19,col='gray', cex=1.5,
       xlab=paste("Escapement",mult(u)),ylab=paste('Yield',mult(u)),
       xlim=c(0,max(SRp$S)),ylim=c(min(SRp$Rl-SRp$S),1.25*max(xp$R-xp$S)))
  points((R-S)~S,data=xp2,pch=19,col=1,cex=1.5)
  lines((RS.md-S)~S,data=SRp,col=1,lw=2)
  lines((RS.me-S)~S,data=SRp,col=1,lw=2,lty=2)
  abline(h=0)
#-------------------------------------------------------------------------------
#----  Plots Output ------------------------------------------------------------  
  out2 <-recordPlot()  
  return(list(base.p=out1,base.py=out2))
})


base.p <- reactive({base.pl()$base.p})

base.py <- reactive({base.pl()$base.py})

#===============================================================================
#  Standard SR and Yield Plots with CI-PI
#=============================================================================== 
# srplot.g : plot goal range in SR plot-----------------------------------------
base.sr <- reactive({
  u <- as.numeric(unit())
  SRp <- SRp()/u
# Plot base recruit Plot  
  replayPlot(base.p())
  # SR CI range 
  with(SRp,polygon(c(S,rev(S)),c(Ru,rev(Rl)),col=tcol('grey',50),border=NA))
  # SR PI range 
  with(SRp,lines(S,Ru.p,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p,lty=2,col='grey'))
  out1 <-recordPlot()
# Plot base Yield Plot
  replayPlot(base.py())
  with(SRp,polygon(c(S,rev(S)),c(Ru-S,rev(Rl-S)),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,Ru.p-S,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p-S,lty=2,col='grey'))  
  out2 <-recordPlot()
  return(list(base.r=out1,base.y=out2))  
})

base.r <- reactive({base.sr()$base.r})

base.y <- reactive({base.sr()$base.y})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 4: SR Model Analyses plots   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# srplot ------- SR plot function ----------------------------------------------
srplot <- reactive({
  u <- as.numeric(unit())
  SRp <- SRp()/u
  xp <- sr.data()
  dyear <- floor(xp$Yr/10)*10
  colp <- (dyear-min(dyear))/10+1
    # Draw Base SR Plot
  replayPlot(base.p())
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  yrs <- paste('Brood Year:',min(xp$Yr),'-',max(xp$Yr))
  model <- paste0('SR Model: ',input$Model,' ',add)
#  texmodel <- c(model,yrs)   
  title(main = paste(model,yrs)
        #        , sub = '',
        #        xlab = "X axis", ylab = "Y axis",
        #        cex.main = 1,   font.main= 4, col.main= "black",
        #        cex.sub = 0.75, font.sub = 3, col.sub = "black",
        #        col.lab ="darkblue"
  )  

#-------- Time Varying Alpha ---------------------------------------------------  
  if(input$add=='kf'){
    srmodel <- Bayesmodel()$model
    model.br <- Bayesmodel()$model.br
#---------- Extract MCMC SR Model Parameters -----------------------------------
    star <- lnalphais()$cuty
#    star <- data.frame(yr = sr.data()$Yr,star=xa$star)
    beta <- mean(SR.post()$beta)
    sigma <- mean(SR.post()$sigma)
    S <- SRp()$S
    D <- Bayesdata()$d
# Get unique alpha 
    alpha.star <- star$star
    alpha.star.c <- alpha.star + 0.5*sigma^2
    br <- model.br(alpha.star,beta,D)
    br.c <- model.br(alpha.star.c,beta,D)
    nstar <- length(alpha.star)
    if(input$target=='me'){
      for (i in 1:nstar){
        R <- srmodel(alpha.star.c[i],beta,S,D)
        lines(S/u,R/u,lty=2,lwd=2,col=1+i)
        }
      }else{
    for (i in 1:nstar){
     R <- srmodel(alpha.star[i],beta,S,D)
     lines(S/u,R/u,lty=2,lwd=2,col=1+i)
      }
    }
    tex <- star$txt 
    if(input$show.smsy==FALSE){
    legend('right',col=c(1:nstar+1),lwd=3,lty=2,legend=tex,box.lty=0)
    }
  }
  # credible Interval 
  if (input$Li =='credible') {
    lwr <- SRp$Rl
    upr <- SRp$Ru
    }
  else {
    # Prediction Interval
    lwr <- SRp$Rl.p
    upr <- SRp$Ru.p
   }
  
  # Add CI    
  if(input$show.int==TRUE){
      polygon(c(SRp$S,rev(SRp$S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA)
    }
  # Add Years
  if(input$show.points==TRUE) {
    points(xp$S/u,xp$R/u,pch=19,cex=1.5,col=colp)
#    addTextLabels(xp$S/u,xp$R/u, labels=as.character(xp$Yr), cex.label= 1,
#                  col.label = 'black')
    pointLabel(xp$S/u,xp$R/u, labels=as.character(xp$Yr), cex = 1)
    legend('topleft',col=unique(colp),legend=unique(dyear), pt.cex = 1.2,
           pch=19,box.lty=0)
    }
  # Add Smsy
  t1 <- ''
  l1 <- 0
  Smsy <- ifelse(input$target=='me',mean(SR.post()$Smsy.c),mean(SR.post()$Smsy))
  if(input$show.smsy==TRUE) {
    if(input$add=='kf'){
      if(input$target=='me'){
        kfSmsy <- c(mean(SR.post()$Smsy.c),br.c$Smsy)
      }else{
        kfSmsy <- c(mean(SR.post()$Smsy),br$Smsy)   
      }
      abline(v=kfSmsy/u,col=c(1:(nstar+1)),lty=2) 
      t1 <- paste('Smsy:',c('overall',star$txt),':',paste(round(kfSmsy)))
      } else {
    abline(v=Smsy/u,lty=2)
    t1 <- paste('Smsy:',round(Smsy,0))
      }
    l1 <- 2
    }
  # Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE & input$Model == 'Ricker') {
    abline(v=mean(SR.post()$Smax)/u,col=1,lty=3)
    t2 <- paste('Smax:',round(mean(SR.post()$Smax),0))
    l2 <- 3
    }
  legend('topright',c(t1,t2),lty=c(rep(l1,length(t1)),l2),
         lwd=2, col=c(1:(length(t1)),1),box.lty=0)  
  out <-recordPlot()
  return(out)  
  })

# Plt_SR ------ SR plot -------------------------------------------------------
output$Plt_SR <- renderPlot({srplot()})


# Plt_yield -------- Yield plot ----------------------------------------------- 
yldplot <- reactive({
  u <- as.numeric(unit())
  SRp <- SRp()/u
  xp <- sr.data()
  dyear <- floor(xp$Yr/10)*10
  colp <- (dyear-min(dyear))/10+1
  # Plot base Yield plot
  replayPlot(base.py())
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  yrs <- paste('Brood Year:',min(xp$Yr),'-',max(xp$Yr))
  model <- paste0('SR Model: ',input$Model,' ',add)
  title(main = paste(model,yrs)
#        , sub = '',
#        xlab = "X axis", ylab = "Y axis",
#        cex.main = 1,   font.main= 4, col.main= "black",
#        cex.sub = 0.75, font.sub = 3, col.sub = "black",
#        col.lab ="darkblue"
  )
#-------- Time Varying Alpha ---------------------------------------------------  
  if(input$add=='kf'){
    srmodel <- Bayesmodel()$model
    model.br <- Bayesmodel()$model.br
    #---------- Extract MCMC SR Model Parameters -----------------------------------
    star <- lnalphais()$cuty
    #    star <- data.frame(yr = sr.data()$Yr,star=xa$star)
    beta <- mean(SR.post()$beta)
    sigma <- mean(SR.post()$sigma)
    S <- SRp()$S
    D <- Bayesdata()$d
    # Get unique alpha 
    alpha.star <- star$star
    alpha.star.c <- alpha.star + 0.5*sigma^2
    br <- model.br(alpha.star,beta,D)
    br.c <- model.br(alpha.star.c,beta,D)
    nstar <- length(alpha.star)
      for (i in 1:nstar){
        if(input$target=='me'){
          R <- srmodel(alpha.star.c[i],beta,S,D)
        } else {
          R <- srmodel(alpha.star[i],beta,S,D)          
        }
        lines(S/u,(R-S)/u,lty=2,lwd=2,col=1+i)
      }
    tex <- star$txt 
    if(input$show.smsy==FALSE){
      legend('right',col=c(1:nstar+1),lwd=3,lty=2,legend=tex,box.lty=0)
    }
  }
  # credible Interval 
  if (input$Li =='credible') {
    lwr <- SRp$Rl-SRp$S
    upr <- SRp$Ru-SRp$S
  }
  else {
    # Prediction Interval
    lwr <- SRp$Rl.p-SRp$S
    upr <- SRp$Ru.p-SRp$S
  }
  # Add CI    
  if(input$show.int==TRUE){
    polygon(c(SRp$S,rev(SRp$S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA)
  }
  # Add Years
  if(input$show.points==TRUE) {
    points(xp$S/u,(xp$R-xp$S)/u,pch=19,cex=1.5,col=colp)
    legend('topleft',col=unique(colp),legend=unique(dyear), pt.cex = 1.5,
           pch=19,box.lty=0)
    pointLabel(xp$S/u,(xp$R-xp$S)/u, labels=as.character(xp$Yr), cex= 1,col=1)
  }
  
  # Add Smsy
  t1 <- ''
  l1 <- 0
  Smsy <- ifelse(input$target=='me',mean(SR.post()$Smsy.c),mean(SR.post()$Smsy))
  if(input$show.smsy==TRUE) {
    if(input$add=='kf'){
     if(input$target=='me'){
        kfSmsy <- c(mean(SR.post()$Smsy.c),br.c$Smsy)
      } else {
        kfSmsy <- c(mean(SR.post()$Smsy),br$Smsy)   
      }  # End of if me: 
      abline(v=kfSmsy/u,col=c(1:(nstar+1)),lty=2) 
      t1 <- paste('Smsy:',c('overall',star$txt),':',paste(round(kfSmsy)))
      } else {
      abline(v=Smsy/u,lty=2)
      t1 <- paste('Smsy:',round(Smsy,0))
    } # End of if kf 
    l1 <- 2
  }  # End if show Smsy       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE & input$Model == 'Ricker') {
    abline(v=mean(SR.post()$Smax)/u,col=1,lty=3)
    t2 <- paste('Smax:',round(mean(SR.post()$Smax),0))
    l2 <- 3
  }
  legend('topright',c(t1,t2),lty=c(rep(l1,length(t1)),l2),
         lwd=2, col=c(1:length(t1),1),box.lty=0)    
  out <-recordPlot()
  return(out)  
  })

output$Plt_yield <- renderPlot({yldplot()})

# Output Model name   
#output$bypt <-renderText({
#  BEG.p <- round(b.YApg())
#  paste('Escapement Goal Range:',BEG.p[1],'-',BEG.p[2])
#})
# Plt_residual --- Plot Residual Plot ------------------------------------------

output$Plt_residual <- renderPlot({
  year <- sr.data()$Yr
  if(input$add=='ar1'){
    resid <-SR.resid()$RD2  
   } else {
    resid <- SR.resid()$RD
   } # End if ar1
  resid.m <- apply(resid,2,mean)
  cil <- apply(resid,2,function(x) quantile(x, 0.025))
  ciu <- apply(resid,2,function(x) quantile(x, 0.975))
  par(bty='l')
  plot(resid.m~year,xlab='Year',type='l',ylab='Residuals',lwd=2, main='Residual',
       ylim =c(min(cil),max(ciu)))
  abline(h=0)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  })


# Plt_predict --- Plot Predicted Plot ------------------------------------------
output$Plt_predict <- renderPlot({
  year <- sr.data()$Yr
  resid <-SR.resid()$RD
  R <- log(Bayesdata()$R)
  R.m <- R-apply(resid,2,mean)
  cil <- R - apply(resid,2,function(x) quantile(x, 0.025))
  ciu <- R - apply(resid,2,function(x) quantile(x, 0.975))
  par(bty='l')
  plot(R.m~year,xlab='Year',type='l',lwd=2,ylab='ln(Recruit)',main='Recruit',
       ylim =c(min(cil,R),max(ciu,R)))
  points(R~year,pch=19,col=2)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  legend('topright',legend=c('Observed','Predicted '),pch=c(19,NA),lty=c(NA,1), col=c(2,1),box.lty=0)
 })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 5: Escapement Goal Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#===============================================================================
#  Smsy Goal Analyses: This produces 
#  EG.Smsy, EG.Smsy.st, SA.BEG, p.msy, p.msy.t
#===============================================================================
  smsyprof <- ProfileServer("smsy",SR.pred,'MSY',unit)
  EG.Smsy <- reactive({smsyprof$EG()})
  EG.Smsy.st <- reactive({smsyprof$EG.st()})
  SA.BEG  <- reactive({smsyprof$BEG()})
  p.msy <- reactive({smsyprof$p.min()})
  p.msy.t <- reactive({smsyprof$p.t()})
# Basic smsy profile plot
  plt.msy.prof <- reactive({smsyprof$plt.profile()})  
# Smsy profile plot with bounds  
  plt.msy.prof.fig <- reactive({smsyprof$plt.prof.fig()})

# Output Smsy profile plot   
  output$Plt_Smsy_prof <- renderPlot({plt.msy.prof.fig()})

#===============================================================================
#  Smax Goal Analyses 
#===============================================================================
smaxprof <- ProfileServer("smax",SR.pred,'Rmax',unit)  
  EG.Smax <- reactive({smaxprof$EG()})
  EG.Smax.st <- reactive({smaxprof$EG.st()})
  SM.BEG  <- reactive({smaxprof$BEG()})   # Smax based goal range
  p.max <- reactive({smaxprof$p.min()})
  p.max.t <- reactive({smaxprof$p.t()})
# Basic smax profile plot  
  plt.max.prof <- reactive({smaxprof$plt.profile()})
# Smax profile plot with bounds   
  plt.max.prof.fig <- reactive({smaxprof$plt.prof.fig()})

# Output Smax profile plot    
  output$Plt_Smax_prof <- renderPlot({plt.max.prof.fig()})  

#===============================================================================
#  Smsy-Smax Goal Analyses Output 
#===============================================================================
plot_range <- function(out,baseplot,sr.data,SRp,Srange1,Srange2=c(NA,NA),goal=NA,u)
  {
  x <- sr.data
  xp <- x/u
  SRp <- SRp()/u
  replayPlot(baseplot)
  if(out=='r'){
  ymin <-c(0,0)
  ymax <-c(1.1*max(xp$R),1.1*max(xp$R))
  }
  if(out=='y'){
    ymin <-c(min(SRp$Rl-SRp$S),min(SRp$Rl-SRp$S))
    ymax <-c(1.1*max(xp$R-xp$S),1.1*max(xp$R-xp$S))
  }
  polygon(c(Srange1/u,rev(Srange1/u)),c(ymin,ymax),col=tcol(3,80),border=NA)
  # Plot escapement goal range 
  if(!is.na(sum(Srange2))) {
    polygon(c(Srange2/u,rev(Srange2/u)),c(ymin,ymax),col=tcol(4,80),border=NA)
  }
  if(!is.na(goal)) {
  abline(h=goal/u,lwd=2,col=2)
  }  
  }

# Yield and Recruit Plot -------------------------------------------------------
output$Plt_rec.pg <- renderPlot({
  plot_range('r',base.r(),sr.data(),SRp(),Srange1=EG.Smsy()$S.Range,Srange2=EG.Smax()$S.Range,u=as.numeric(unit()))
  })
  
output$Plt_yield.pg <- renderPlot({
  plot_range('y',base.y(),sr.data(),SRp(),Srange1=EG.Smsy()$S.Range,Srange2=EG.Smax()$S.Range,u=as.numeric(unit()))
  })
  
# Txt_Srange.smsy -------- Smsy goal output ------------------------------------
  output$Txt_Srange.smsy <-renderUI({ SA.BEG() })
# Txt_Srange.smax -------- Smax Goal range output table ------------------------
  output$Txt_Srange.smax <-renderUI({ SM.BEG() })
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Yield and Recruit based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#---- UI Output Initial Value generating function ------------------------------
numinput <- function(dat,p=0.5){
  s <- quantile(dat,p,na.rm=TRUE)
  D <- floor(log10(s))
  # This makes largest numbers into integer (e.g. 100000)
  imr <- round(s,-D)  
  step <- 10^(D-1)
  out <- c(imr,step)
  return(out)
}

#---- UI Output-----------------------------------------------------------------
output$minYield = renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(sr.data()$R-sr.data()$S,0.5)/u
  numericInput("y1", paste("Min Yield",mult), value=v[1],min=0, step=v[2])
})


#----- Yield Goal Simulation ---------------------------------------------------
Yield_gl_sim <- reactive({
  u<- as.numeric(unit())
  mc.Ypm <- Prob.calc(SR.pred()$Y,input$y1*u)
  mc.Ypa <- Prob.calc(SR.pred()$Y.p,input$y1*u)
  out <- list(mc.Yp=mc.Ypm, mc.Ypa=mc.Ypa)
  return(out)
  })


#---- Optimum Mean and annual Yield Profile Plot -------------------------------
output$Plt_yield.prof <- renderPlot({
  u <- as.numeric(unit())
  mult <- mult(u)
  yg <- input$y1*u
  ypg <- input$y1p/100
  S <- SR.pred()$S/u
  mc.Yp <- Yield_gl_sim()$mc.Yp
  mc.Ypa <- Yield_gl_sim()$mc.Ypa
  # Create a plot 
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,mc.Yp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',yg,'Yield probability plot')) 
  lines(S,mc.Ypa,lty=2)
  abline(h = ypg,lwd=2,col=2)
  BEG.p <- Yield_gl()/u 
  polygon(c(BEG.p[1,],rev(BEG.p[1,])),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
  polygon(c(BEG.p[2,],rev(BEG.p[2,])),c(c(0,0),c(1,1)),col=tcol(4,80),border=NA)
    tex <- ifelse(input$target =='me','Mean','Median')
  legend('topright',legend=c(tex,'Annual'),lty=c(1,2), box.lty=0)
  }) 

# Find Yield Target Intersection 
Yield_gl <- reactive({
  ypg <- input$y1p/100
  S <- SR.pred()$S 
  mc.Yp <- Yield_gl_sim()$mc.Yp
  mc.Ypa <- Yield_gl_sim()$mc.Ypa
# Find Intersections 
  b.p <- S[mc.Yp > ypg]
  b.pa <- S[mc.Ypa > ypg]
  BEG.p <- c(NA,NA)
  if(sum(b.p) > 0){BEG.p <- c(min(b.p),max(b.p))}
  BEG.pa <- c(NA,NA)
  if(sum(b.pa) > 0){BEG.pa <- c(min(b.pa),max(b.pa))}
  out <- rbind(BEG.p,BEG.pa)
  return(out)
  })

# Print Optimum Yield Profile Goal Range  
output$Txt_Yield_gl <-renderText({
  BEG.p <- Yield_gl()
  tex <- ifelse(input$target =='me','Mean','Median')
  paste(
  paste(tex,'target range:',BEG.p[1,1],'-',BEG.p[1,2]),
  paste('Annual target range:',BEG.p[2,1],'-',BEG.p[2,2]),
  sep='\n')
  })

# Yield Plot 
output$Plt_yield.gl <- renderPlot({
  u <- as.numeric(unit())
  BEG.p <- Yield_gl()
  plot_range('y',base.y(),sr.data(),SRp(),BEG.p[1,],BEG.p[2,],input$y1*u,u)
    }) 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Recruitment  based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#---- UI Output----------------------------------------------------------------------
output$minRec <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(sr.data()$R,0.5)/u
  numericInput("r1", paste("Min Recruit",mult), value=v[1],min=0, step=v[2])
})

Rec_gl_sim <- reactive({
  u <- as.numeric(unit())
  Rp <- Prob.calc(SR.pred()$R,input$r1*u)
  Rpa <- Prob.calc(SR.pred()$R.p,input$r1*u)
  out <- list(Rp=Rp, Rpa = Rpa)
  return(out)
})


#------------------------------------------------------------------------------
#  Calculate probability that intercepts profile 
#------------------------------------------------------------------------------
Rec_gl <- reactive({
  rpg <- input$r1p/100
  S <- SR.pred()$S 
  Rp <- Rec_gl_sim()$Rp
  Rpa <- Rec_gl_sim()$Rpa  
  # Find Intersections 
  b.p <- S[Rp > rpg]
  b.pa <- S[Rpa > rpg]
  BEG.p <- c(NA,NA)
  if(sum(b.p) > 0){BEG.p <- c(min(b.p),max(b.p))}
  BEG.pa <- c(NA,NA)
  if(sum(b.pa) > 0){BEG.pa <- c(min(b.pa),max(b.pa))}
  out <- rbind(BEG.p,BEG.pa)
  return(out)
})

# Recruit Plot 
output$Plt_rec.gl <- renderPlot({
  u <- as.numeric(unit())
  BEG.p <- Rec_gl()
  plot_range('r',base.r(),sr.data(),SRp(),BEG.p[1,],BEG.p[2,],input$r1*u,u)
  }) 

#  Minimum Recruit Profile Plot 
output$Plt_rec.prof <- renderPlot({
  u <- as.numeric(unit())
  mult <- mult(u)
  rg <- input$r1*u
  rpg <- input$r1p/100
  S <- SR.pred()$S/u
  Rp <- Rec_gl_sim()$Rp
  Rpa <- Rec_gl_sim()$Rpa  
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,Rp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',rg,'Recruit probability Plot')) 
  lines(S,Rpa,lty=2)
  BEG.p <- Rec_gl()/u 
  polygon(c(BEG.p[1,],rev(BEG.p[1,])),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
  polygon(c(BEG.p[2,],rev(BEG.p[2,])),c(c(0,0),c(1,1)),col=tcol(4,80),border=NA)
  abline(h = rpg,lwd=2,col=2)
  tex <- ifelse(input$target =='me','Mean','Median')
  legend('topright',legend=c(tex,'Annual'),lty=c(1,2), box.lty=0)
  })  


# Optimum Recruit Profile Escapement Goal 
output$Txt_Rec_gl <-renderText({
  BEG.p <- Rec_gl()
  tex <- ifelse(input$target =='me','Mean','Median')
  paste(
    paste(tex,'target range:',BEG.p[1,1],'-',BEG.p[1,2]),
    paste('Annual target range:',BEG.p[2,1],'-',BEG.p[2,2]),
    sep='\n')
  })

#===============================================================================
#  Custom escapement goal analyses 
#===============================================================================
#---- UI Output Lower Goal -----------------------------------------------------
output$minEG <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,2],0.1)/u
  numericInput("lg", paste("Lower",mult), value=v[1],min=0, step=v[2])
 })
#---- UI Output Upper Goal -----------------------------------------------------
output$maxEG <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,2],0.75)/u
  numericInput("ug", paste("Upper",mult), value=v[1],min=0, step=v[2])
})

#---- UI Output Minimum Yield --------------------------------------------------
output$cyg = renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  h <- (data()[,3]-data()[,2])
  v <- numinput(h,0.25)/u
  numericInput("yg", paste("Min Target Yield",mult), value=v[1],min=0, step=v[2])
})
#---- UI Output Minimum Recruit ------------------------------------------------
output$crg = renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,3],0.25)/u
  numericInput("rg", paste("Min Target Recruit",mult), value=v[1],min=0, step=v[2])
})

#-------------------------------------------------------------------------------
# plt_msyprof:  Plot  MSY prof in given escapement 
#-------------------------------------------------------------------------------
plt_msyprof_c1 <- reactive({
  u <- as.numeric(unit())
  layout(matrix(1:2, ncol=2),widths=c(2,1))
  #   Plot profile 
  par(mar=c(4,4,4,1))
  replayPlot(plt.msy.prof())
  SS <- c(input$lg,input$ug)
  if(SS[1]==SS[2]){lines(SS,c(0,1),col=3,lwd=3)
    } else{
  polygon(c(SS,rev(SS)),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
  }
  #  Add legends 
  percent <- c(90,80,70,as.numeric(p.msy()))
  EG.pf <- (data.frame(S=EG.Smsy()$S,t(EG.Smsy.st()$S.prof.st),EG.Smsy()$S.prof))
  EG.p <- EG.pf[EG.pf$S>=input$lg*u,]
  pl <- round(EG.p[1,-1]*100,0)
  EG.p <- EG.pf[EG.pf$S>=input$ug*u,]
  pu <- round(EG.p[1,-1]*100,0)
  txt <- c(paste(percent,'%','MSY','acheiving',pl,' - ',pu,'%')) 
  add_legend("left", legend= txt, lwd=c(1,1,1,2), lty=c(1,2,4,1),
         col=c(1,1,1,6),text.font = c(1,1,1,2), box.lty=0)
}) 

#----- Profile output ----------------------------------------------------------
output$plt_msyprof_c <- renderPlot({plt_msyprof_c1()})

#-------------------------------------------------------------------------------
# plt_maxprof:  Plot  Smax prof in given escapement 
#-------------------------------------------------------------------------------
plt_maxprof_c1 <- reactive({
  u <- as.numeric(unit())
  layout(matrix(1:2, ncol=2),widths=c(2,1)) 
  #   Plot profile 
  par(mar=c(4,4,4,1))
  replayPlot(plt.max.prof())
  SS <- c(input$lg,input$ug)
  if(SS[1]==SS[2]){lines(SS,c(0,1),col=4,lwd=3)
  } else {
  polygon(c(SS,rev(SS)),c(c(0,0),c(1,1)),col=tcol(4,80),border=NA)
  }
  #  Add legends 
  percent <- c(90,80,70,as.numeric(p.max()))
  EG.pf <- (data.frame(S=EG.Smax()$S,t(EG.Smax.st()$S.prof.st),EG.Smax()$S.prof))
  EG.p <- EG.pf[EG.pf$S>=input$lg*u,]
  pl <- round(EG.p[1,-1]*100,0)
  EG.p <- EG.pf[EG.pf$S>=input$ug*u,]
  pu <- round(EG.p[1,-1]*100,0)
  txt <- c(paste(percent,'%','RMAX','acheiving',pl,' - ',pu,'%'))
  add_legend("left",legend= txt, lwd=c(1,1,1,2), lty=c(1,2,4,1),
         col=c(1,1,1,4),text.font = c(1,1,1,2),box.lty=0)  
})

#----- Profile output ----------------------------------------------------------
output$plt_maxprof_c <- renderPlot({plt_maxprof_c1()})

#-------------------------------------------------------------------------------
# CG_sim:  Calculate Predicted Rec Yield in given EG range
#-------------------------------------------------------------------------------
CG_sim <- eventReactive(input$Run,{
#-------------------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'MCMC calculationin progress',
               detail = 'This may take a while...')
  for (i in 1:15) {progress$set(value = i)}
#----------==-------------------------------------------------------------------   
  u <- as.numeric(unit())
  #  Import user defined lower and upper goal 
  lg <- input$lg*u
  ug <- input$ug*u
  # create goal range 
  S <- seq(lg,ug,length.out=201)   
  D <- Bayesdata()$d
  srmodel <- Bayesmodel()$model
#---------- Extract MCMC SR Model Parameters -----------------------------------
  lnalpha <-SR.post()$lnalpha
  lnalpha.c <-SR.post()$lnalpha.c
  beta <- SR.post()$beta
  sigma <- SR.post()$sigma
  if(input$add=='kf')
  {
    if(input$alphai != 'None') {
      lnalpha <- SR.post.i()$lnalpha 
      lnalpha.c <- SR.post.i()$lnalpha.c 
      beta <- SR.post.i()$beta
      sigma <- SR.post.i()$sigma      
    } 
  } 
  nrow <- length(lnalpha)  #  Extract number of MCMC sample 
  # Create Expected mean and observed Recruit MCMC matrix    
  R <- matrix(NA,nrow=nrow,ncol=201) 
  R.c <- matrix(NA,nrow=nrow,ncol=201) 
  R.p <- matrix(NA,nrow=nrow,ncol=201)
  
  for(i in 1:nrow){
    # Calculated expected Returns form each MCMC SR model paramters   
    Ey <- srmodel(lnalpha[i],beta[i],S,D)
    R[i,] <- Ey
    R.c[i,] <- srmodel(lnalpha.c[i],beta[i],S,D) 
    # mc.R.p adds observation error (sigma)  
    R.p[i,] <- exp(rnorm(201,log(Ey),sigma[i]))
  } 

# Create expected mean and observed Yield matrix
  Y.c <- t(t(R.c)-S)
  Y <-  t(t(R)-S) 
  Y.p <-  t(t(R.p)-S) 
#------  Create Output list files ---------------------------------------------  
  # Outputs are mean and annual Yields and recruits within proposed S range.    
  out <- list(S = S, R = R, R.c=R.c,Y = Y, Y.c = Y.c,R.p = R.p, Y.p = Y.p)
  return(out) 
  })

#-------------------------------------------------------------------------------
# CG_pct:  Calculate % achieving target yield and recruit 
#-------------------------------------------------------------------------------
CG_pct <- reactive({
  u <- as.numeric(unit())
  rg <- input$rg*u
  # Probability of meeting long-term Mean/Median Recruit Target    
  prg <- mean(ifelse(CG_sim()$R>rg,1,0))
  # Probability of meeting annual Mean/Median Recruit Target  
  prgp <- mean(ifelse(CG_sim()$R.p>rg,1,0))
  yg <- input$yg*u
  # Probability of meeting long-term Mean/Median Yield Target  
  pyg <- mean(ifelse(CG_sim()$Y>yg,1,0))
  # Probability of meeting annual Mean/Median Yield Target   
  pygp <- mean(ifelse(CG_sim()$Y.p>yg,1,0))
  # Probability of getting Zero Yield at given escapement.    
  py0 <- mean(ifelse(CG_sim()$Y.p<0,1,0))  
  out <- data.frame(prg=prg, pyg = pyg, prgp=prgp, pygp = pygp,py0=py0)   
  return(out)
})

#-----------------------------------------------------------------------
#  Plot distribution of Mean and Annual  Yield at Given Escapement Range
#-----------------------------------------------------------------------
Yield_EG <- reactive({
  par(mfrow=c(1,1),xaxs='i',yaxs='i',bty='l',cex=1)
  u <- as.numeric(unit())
  mult <- mult(u)
  yg <- input$yg
  Y.p <-CG_sim()$Y.p/u
  Y.p <- Y.p[Y.p < quantile(Y.p,0.995)]
    if (input$target =='me'){
    Y.m <-CG_sim()$Y.c/u
    m <- mean(Y.p)
     } else {
    Y.m <-CG_sim()$Y/u
    m <- median(Y.p)
    }
  # plot density
  leg.tx <- c('Annual',ifelse(input$target =='me','Mean','Median'))
  mult.den.plt(Y.p,Y.m,'Expected Yield',paste("Yield",mult),leg.tx) 
  abline(v=yg,col=2)
  abline(v=m,col=4)
 })

output$Plt_Yield_EG <- renderPlot({Yield_EG()}) 

#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#  SR model based CI and PI
#-----------------------------------------------------------------------
Rec_EG <- reactive({
  par(mfrow=c(1,1),xaxs='i',yaxs='i',bty='l', cex=1)
  u <- as.numeric(unit())
  mult <- mult(u)
  rg <- input$rg
  # Annual estimate   
  R.p <-CG_sim()$R.p/u
  R.p <- R.p[R.p < quantile(R.p,0.995)]  
  if (input$target =='me'){
    R.m <-CG_sim()$R.c/u
    m <- mean(R.p)
  } else {
    R.m <-CG_sim()$R/u
    m <- median(R.p)
  }
  # plot density
  leg.tx <- c('Annual',ifelse(input$target =='me','Mean','Median'))
  mult.den.plt(R.p,R.m,'Expected Recruit',paste("Recruit",mult),leg.tx)  
  abline(v=rg,col=2)
  abline(v=m,col=4)
  }) 

output$Plt_Rec_EG <- renderPlot({Rec_EG()}) 


output$Txt_Rec_cg <- renderPrint({
  if(input$target=='me'){ 
    R  <- as.vector(CG_sim()$R.c)
  } else {
    R <- as.vector(CG_sim()$R)
  }
    R.p <- as.vector(CG_sim()$R.p)
    max_length <- max(c(length(R), length(R.p)))
   dat <- data.frame(c(R,rep(NA, max_length - length(R))),c(R.p, rep(NA, max_length - length(R.p))))
  tex <- ifelse(input$target =='me','Mean','Median')
  names(dat) <- c(tex,'Annual')
  print(summary(dat),digits=1)
  })
  

output$Txt_Yield_cg <- renderPrint({
  if(input$target=='me'){ 
  Y <- as.vector(CG_sim()$Y.c)
  } else {
  Y <- as.vector(CG_sim()$Y)
  }
  Y.p <- as.vector(CG_sim()$Y.p)
  max_length <- max(c(length(Y), length(Y.p)))
  dat <- data.frame(c(Y, rep(NA, max_length - length(Y))),c(Y.p, rep(NA, max_length - length(Y.p))))
  tex <- ifelse(input$target =='me','Mean','Median')
  names(dat) <- c(tex,'Annual')
  print(summary(dat),digits=1)
  })

# Calculate Probability meeting target  
output$Txt_Rec_pb_cg <- renderText({
  tex <- ifelse(input$target =='me','Mean','Median')
  t.prg <- paste('Meeting',tex,'Recruit Target:',round(100*CG_pct()$prg,0),'%')
  t.pyg <- paste('Meeting Annual Recruit Target:',round(100*CG_pct()$prgp,0),'%')
  paste(t.prg,t.pyg,sep='\n')
})

# Calculate Probability meeting target  
output$Txt_Yield_pb_cg <- renderText({
  tex <- ifelse(input$target =='me','Mean','Median')  
  t.prg <- paste('Meeting', tex,'Yield Target:',round(100*CG_pct()$pyg,0),'%')
  t.pyg <- paste('Meeting Annual Yield Target:',round(100*CG_pct()$pygp,0),'%')
  t.py0 <- paste('Annual Zero Yield:',round(100*CG_pct()$py0,0),'%')
  paste(t.prg,t.pyg,t.py0,sep='\n')
})

# Recruit Plot 
output$Plt_rec.cg <- renderPlot({
  u <-as.numeric(unit())
  plot_range('r',base.r(),sr.data(),SRp(),c(input$lg,input$ug)*u,u=u)
  if(input$lg==input$ug){abline(v=input$lg,col=3,lwd=2)}
  abline(h=input$rg,col=2,lwd=2)
}) 

# Yield Plot 
output$Plt_yield.cg <- renderPlot({
  u <-as.numeric(unit())
  plot_range('y',base.y(),sr.data(),SRp(),c(input$lg,input$ug)*u,u=u)
  if(input$lg==input$ug){abline(v=input$lg,col=3,lwd=2)}
  abline(h=input$yg,col=2,lwd=2)
    }) 

#-------------------------------------------------------------------------------
#  Save multiple Simulation Results
#-------------------------------------------------------------------------------
# Set temporary memory: M
# gls: custom EG range 
gls <- reactiveVal(data.frame())
ys <- reactiveVal(data.frame())
meds <- reactiveVal(data.frame())
# Retrieve simulation results data 

# Data update 
observeEvent(input$Run,{
  # Put old value hwere 
  old_gl <- gls()
  old_y <- ys()
  old_meds  <- meds()
  # create new data  
  glt <- paste(input$lg,'-',input$ug)
  pct <- round(100*CG_pct(),0)
  gl <- data.frame(glt, pct)
  names(gl) <- c('Esc Goal Range', '% Recruit Lng','% Yield Lng',
                 '% Recruit Anl','% Yeild Anl','% Zero Yield Anl' )
  y <- data.frame(Y = as.vector(CG_sim()$Y), Y.p = as.vector(CG_sim()$Y.p), 
                  R = as.vector(CG_sim()$R), R.p = as.vector(CG_sim()$R.p))
  med <-  data.frame(round(t(c(apply(y,2,mean),apply(y,2,median)))))
  
  # attach the new line to the old data frame here:
  new_gl <- rbind(old_gl, gl)
  new_meds <- rbind(old_meds, med)
  # Attach data frame     
  if(dim(old_y)[1]==0){
    new_y <- y
  } else {
    new_y <- data.frame(old_y,y)
  }
  # Save updated table  
  gls(new_gl)
  ys(new_y)
  meds(new_meds)
})

#cg_sums <- reactive({data.frame(gls(),meds(),pcts())})
#output$Tbl_sim <- renderDataTable({})

output$Tbl_sim <- renderTable(gls(),digits=0)

output$altsim.R <- renderPrint({
  #  par(mfrow=c(1,4),mar = c(2,2,2,2),cex=1.1)
  # Total Simulation Years 
  x <- ys()
  alts <- dim(x)[2]
  Y.alt <- data.frame(x[,seq(1,alts,4)])
  Yp.alt <- data.frame(x[,seq(2,alts,4)])
  R.alt <- data.frame(x[,seq(3,alts,4)])
  Rp.alt <- data.frame(x[,seq(4,alts,4)])
  #  tY.alt <- melt(Y.alt)
  #  tYp.alt <- melt(Yp.alt)
  #  tR.alt <- melt(R.alt)
  # tRp.alt <- melt(Rp.alt)
  out <- list(Y=summary(Y.alt),Yp=summary(Yp.alt),R=summary(R.alt),Rp=summary(Rp.alt))
  return(out)
})


#===============================================================================
#  Percentile Analyses 
#===============================================================================
# Call Percentile Analyses module Server 
prcntout <- PercentileServer("prcnt",e.data,as.numeric(unit()))

# Txt_Tier : Tier Definition output  
  txt <- reactive({prcntout$Txt_Tier()})
output$Txt_Tier <- renderUI({ txt() })

# Txt_Note:  Tier based goal range  
  txt2 <- reactive({prcntout$Txt_Note()})

output$Txt_Note <- renderUI({ txt2() })
  
  EGS <- reactive({prcntout$EGS()})
  
  Tier <- reactive({prcntout$Tier()})

  plt_prcnt <- reactive({prcntout$Plt_prcnt()})
  
  output$Plt_prcnt <- renderPlot({plt_prcnt()})

#===============================================================================
#  Risk Analyses 
#===============================================================================
# Call Risk Analyses module Server 
riskout <- RiskServer("risk",e.data,as.numeric(unit()))

#---- UI Output----------------------------------------------------------------------

Risk_sim_base <- reactive({riskout$Risk_sim_base()})
Risk_sim <- reactive({riskout$Risk_sim()})
Risk_custom <- reactive({riskout$Risk_custom()})   

output$Tbl_risk <- renderTable({
  Risk_custom()$EG.p
  },caption= 'Risk based on escapement')

output$Tbl_riskp <- renderTable({
  (Risk_custom()$Sp)
},caption='Escapement based on acceptable risk')

# Risk output
plt_risk <- reactive({riskout$Plt_risk()})
output$Plt_risk <- renderPlot({plt_risk()}) 

# Txt_dwtest: Durbin-Watson test results ----------------------------------------
output$Txt_dwtest <- renderPrint({ Risk_sim_base()$dw})

# Risk Model  ---------------------------------------------
output$Txt_Risk_Model <-renderText({Risk_sim_base()$md})

# Risk Target: Print out Target ------------------------------------------------
output$Txt_Risk <-renderUI({ Risk_sim()$txt })

# Plt_risk2 --------------------------------------------------------------------
Plt_risk2 <- reactive({
   riskout$Plt_risk2()
   })
output$Plt_risk2 <- renderPlot({Plt_risk2()})

#===============================================================================
#  Panel 4: Management Strategy Evaluation    
#===============================================================================
#-------------------------------------------------------------------------------
# MSE simulation 
# This simulation evaluate effects of an escapement goal and fishery management 
# strategy for achieving management objectives in more realistic situations
# The model creates fish population dynamics based on data.
# Step 1: Obtain escapement data from the last calendar year escapement to the 
# last complete brood year escapement. 
# Step 2: Use the escapement data to generate expected recruit and run 
# Step 3: Run is subject to fishery  (N)
# Step 3a: Run is predicted with (prediction) error (Ne)
# Step 3b: Fishery Harvested (Ht) target is set based on management target and predicted run 
# Step 3c: Fishery is executed with (implementation) error (H)
# Step 3d: Escapement (E) is Run (N) minus harvest (H)
# Step 4: Based on escapement (E) future recruitment (R) is determined with (process) variation
# Step 5: Go back to step 2

#---- UI Output-----------------------------------------------------------------
output$LEG <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,2],0.25)/u
  numericInput("LEG", paste("Lower",mult), value=v[1],step = v[2])
})
#---- UI Output-----------------------------------------------------------------
output$UEG <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,2],0.75)/u
  numericInput("UEG", paste("Upper",mult), value=v[1],step = v[2])
})

#---- UI Output-----------------------------------------------------------------
output$maxH <- renderUI({
  h <- (data()[,3]-data()[,2])
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(h,0.75)/u
  numericInput(inputId="maxH", paste("Max",mult), value=v[1],step=v[2])
})

#---- UI Output-----------------------------------------------------------------
output$minH <- renderUI({
  h <- (data()[,3]-data()[,2])
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(h,0.1)/u
  numericInput(inputId="minH", paste("Min",mult), value=v[1],step=v[2])
})
 
#Bayesian figure page
output$nsim = renderUI({
  lnalpha <-SR.post()$lnalpha
  mn <- length(lnalpha)
  numericInput("nsim", "N Simulation", value=500,min=1,max=mn,step=100)
})

output$sim.rep = renderUI({
  numericInput('sim.rep', 'Simulation rep', value=1,min=1,max=input$nsim,step=1)
 })

observeEvent(input$show, {
  showModal(modalDialog(
    title = "Management Option",
    HTML("
          <b>Harvest Strategy:</b> Max fishery harvest regardless run size <br/><br/>
          <b>Escapement Strategy</b><br/>
          Predicted run < target escapement -> <b>Fishery is closed</b><br/>
          Predicted run > target escapement -> <b>Up to max fishery harvest</b><br/><br/>
          <b>Hybrid Strategy</b><br/> 
          Predicted run < target escapement -> <b>Minimum fishery harvest.</b><br/>
          Predicted run > target escapement -> <b>Up to max fishery harvest.</b><br/>
         ")
          ,
        easyClose = TRUE
  ))
})

#-------------------------------------------------------------------------------  
#  Function sets:
#-------------------------------------------------------------------------------
# e.freq summarize consecutive frequencies -------------------------------------  
e.freq <- function(data,c,crit){
  temp1 <- rle(data)  
  # function rle produces two data: values: entries,
  # length: consecutive frequency
  temp2 <- data.frame(value=temp1$values,length=temp1$length,crit=crit)
  # temp2 creates data frame
  temp3 <- temp2[temp2$value==c,]
  # temp3 selects particular entities. 
  # if entity is 0, add 0.     
  if(dim(temp3)[1]==0){
    temp3 <- rbind(temp3,data.frame(value=c,length=0,crit=crit))
  }
  return(temp3)
}

# SumMSE:  Mean, Median, lci, Uci ----------------------------------------------  
sumMSE <- function(x,pci){
  md <- apply(x,1,median,na.rm=TRUE)
  me <- apply(x,1,mean,na.rm=TRUE)
  lci <- apply(x,1,function(x) quantile(x, pci,na.rm=TRUE))
  uci <-  apply(x,1,function(x) quantile(x, 1-pci,na.rm=TRUE)) 
  out <- data.frame(md,me,lci,uci)
  return(out)
}  


#-------------------------------------------------------------------------------  
#  MSE.int: Create initial modeling conditions
#-------------------------------------------------------------------------------  
MSE.int <-  eventReactive(input$SimRun,{
#-----------------------------------------------------------------------  
# Extract S0 from brood data   
  brood <- brood.out()$brood
# Extract S0
  S0 <- brood[is.na(brood$Recruit),2]
# S0 is last lage years of escapement 
  S0 <- S0[!is.na(S0)]
# Extract lage   
  lage <- length(S0)
# Extract number of simulation years
  nyrs <- input$simy
# import brood table
  brood.p <- brood.p()
# Maturity schedule is a random sample of observed brood proportion 
  e.p  <- brood.p[sample(dim(brood.p)[1],nyrs+lage,replace = TRUE),]    
# Prediction and Implementation Error are normal   
  e.pred <- (rnorm(nyrs,1,input$spred/100))
  e.imp <- (rnorm(nyrs,1,input$simpH/100))
# Determine Fishery opening target: FT  
  out <- list(S0=S0,e.pred = e.pred, e.imp = e.imp, e.p = e.p)
  return(out) 
})

#-------------------------------------------------------------------------------
# Txt_Strategy:  Explanation of Strategy 
#-------------------------------------------------------------------------------  
Txt_Strategy <- reactive({
  txt<- HTML(
    if(input$strgy =="Harvest"){
      paste("Constant Harvest Strategy",
            "Target harvest is taken regardless run size",
             sep = '<br/>')
    } else if(input$strgy == "Escapement")  {
      paste("Escapement Goal Strategy",
            "Predicted run is below target escapement",
            "     Fishery is closed.",
            "Predicted run is above target escapement",
            "     Fihsery harvest is up to max harvest rate or max harvest.",
            sep = '<br/>')
    } else {
      paste("Hybrid Strategy",
            "Predicted run is below target escapement",
            "     Fishery harvest is minimum.",
            "Predicted run is above target escapement",
            "     Fihsery harvest is up to max harvest rate or max harvest.",
            sep = '<br/>')
    } 
  )
  return(txt)
})

#-------------------------------------------------------------------------------
# msesim:  Run simulation and output 
#-------------------------------------------------------------------------------  
msesim <- eventReactive(input$SimRun,{
#-------------------------------------------------------------------------------
#  Import Error Data 
#-------------------------------------------------------------------------------  

# Initial Spawners  
  S0 <- MSE.int()$S0
  lage <- length(S0)
# Prediction and Implementation Error
  e.imp <- as.vector(MSE.int()$e.imp)
  e.pred <- as.vector(MSE.int()$e.pred)
  nyrs <- length(e.pred)
  bt <- nyrs+lage
# Brood age comp 
  e.p <- as.matrix(MSE.int()$e.p)
#---------- Extract MCMC SR Model Parameters -----------------------------------
# SR model   
  srmodel <- Bayesmodel()$model
# D  
  D <- Bayesdata()$d
# SR parameters
  lnalpha <-SR.post()$lnalpha
  beta <- SR.post()$beta
  sigma <- SR.post()$sigma 
  if(input$add=='kf'){
      if(input$alphai != 'None') {
        lnalpha <- SR.post.i()$lnalpha 
        beta <- SR.post.i()$beta
        sigma <- SR.post.i()$sigma      
     } 
    }
  
# Error is AR1 process
    if(input$add=='ar1'){
    phi <- SR.post()$phi
# e0 is the residuals of the last year    
    e0 <- SR.resid()$RD[,Bayesdata()$nyrs]
    }
# lnalpha is time variant  
#  if (input$add=='kf'){
#    sigmaw <- SR.post()$sigmaw
#    ny <- Bayesdata()$nyrs
#    parname <- paste0('lnalphai[',ny,']')
    # Extract the last year's lnalpha   
#    lnalphai <- SR.post()[,parname]
#  }
  u <- as.numeric(unit())
# Fishery   
  FT <- FTA(input$cmode,input$LEG*u,input$UEG*u)
  if(input$strType=='Escapement'){
    mH <- c(0,input$maxH*u)
  } else if(input$strType=='Harvest'){
    mH <- c(input$maxH*u,input$maxH*u)
  } else if(input$strType =='Hybrid'){
    mH <- c(input$minH*u,input$maxH*u)
  }
#-------------------------------------------------------------------------------
# Select Parameters 
#-------------------------------------------------------------------------------  
nsims <-sample(1:length(lnalpha),input$nsim)
# The number of bootstrap replicates
  boot.n <- input$nsim
#-----------------------------------------------------------------------  
  progress <- Progress$new()
  on.exit(progress$close())
  progress$set(message = paste(boot.n,'MSE Calculation in progress'),
               detail = 'This will take a while. Be patient please....')
  
# Set outpit 
sim.out <- list()

for(k in 1:input$nsim){
  j <- nsims[k]
#-------------------------------------------------------------------------------
# Set SR simulation parameters
#-------------------------------------------------------------------------------  
# Set lnalpha.i  (random walk when alpha is time varying) ----------------------
# Constant alpha
  lnalpha.i <- rep(lnalpha[j],bt)
# Time Varying alpha  
#  if (input$add=='kf'){
#    w <- rnorm(bt,0,sigmaw[j])
#    lnalpha.i <- numeric(bt)
#    lnalpha.i[1] <- lnalphai[j]
#    for(i in 2:bt){lnalpha.i[i] <- lnalpha.i[i-1]+w[i]}
#  }

# Set Error (Random, AR1) ------------------------------------------------------
# Random Error   
  e.Rec <- rnorm(bt,0,sigma[j])
# Create AR1 Error 
  if(input$add=='ar1'){
    e.Rec.0 <- e.Rec
    e.Rec[1] <- e.Rec.0[1]+phi[j]*e0[j]
    for(i in 2:bt){e.Rec[i] <- phi[j]*e.Rec[i-1]+e.Rec.0[i]}
  }         
# Set SR beta: Constant --------------------------------------------------------
  sbeta <- beta[j]
# Simulate MSE    
  sim.out[[k]] <- MSE.sim(srmodel,lnalpha.i,sbeta,S0,D,e.Rec,e.p,e.pred,e.imp,FT,mH)  
  progress$set(value = k/boot.n)
  # Increment the progress bar, and update the detail text.
  progress$inc(1/boot.n, detail = paste("Completed", round(100*k/boot.n,0),"%"))
  # sim.out produces: 'N','S','H','R' for each 
  }
  return(sim.out)   
})

#-------------------------------------------------------------------------------
# MSE.sum: Summarize MSE results as data.frame 
#-------------------------------------------------------------------------------  
MSE.sum <- reactive({
  # Set output as data.frame   
  out <- data.frame()
  u <- as.numeric(unit())  
  for(i in 1:input$nsim){
    dat <- msesim()[[i]][,c('N','S','H')]
    # Frequency of fishery closure  
    dat$H0 <- ifelse(dat$H==0,1,0)
    # Frequency of not meeting escapement goal  
    dat$EG <- ifelse(dat$S < input$LEG*u,1,0)
    # Frequency of Harvest below target      
    dat$Hmin <- ifelse(dat$H < input$minH*u,1,0)
    # Frequency of Run below 10      
    dat$ND <- ifelse(dat$N > 10,1,0)    
    f.H0 <- e.freq(dat$H0,1,'H0')
    # Consecutive fishery below minimum
    f.Hmin <- e.freq(dat$Hmin,1,'Hm')
    # Consecutive escapement failure
    f.EG <- e.freq(dat$EG,1,'EG')  
    # Consecutive escapement failure
    f.ND <- e.freq(dat$ND,0,'ND')     
    # Combine 
    f <- rbind(f.H0,f.Hmin,f.EG,f.ND)
    f$rep <- i
    out <- rbind(out,f)
  }
  return(out) 
})

#-------------------------------------------------------------------------------
# Plt_mse:  MSE summary plot
#---------------------------''----------------------------------------------------  
output$Plt_mse <- renderPlot({
  u <- unit()
  mult <- mult(u)
  lyear <- max(data()[,1])
  nyrs <- input$simy
  years <- c(1:nyrs)+lyear
# Spread out list data 
  simout <- do.call("cbind", msesim())
  N <- simout[,names(simout)=='N']
  H <- simout[,names(simout)=='H']
  S <- simout[,names(simout)=='S']
  N.sum <- sumMSE(N,0.025)
  H.sum <- sumMSE(H,0.025)
  S.sum <- sumMSE(S,0.025)
  
  par(yaxs='i',bty='l')
  
# Plot N  
if(input$pltmse=='Run')
 {
  plot(data()[,1],data()[,3]/u,type='l',lwd=2,xlim=c(min(data()[,1]),max(years)),ylim=c(0,max(N.sum$uci,data()[,3])/u), main='', xlab = '',ylab=paste('Run',mult))
  polygon(c(years,rev(years)),c(N.sum$uci/u,rev(N.sum$lci/u)),col=tcol('grey',50),border=NA)
  lines(years,N.sum$md/u,lwd=2,lty=1)
  lines(years,N.sum$me/u,lwd=2,lty=2)
}
  if(input$pltmse=='Escapement')
  {  
# Plot S
  plot(data()[,1],data()[,2]/u,type='l',lwd=2,xlim=c(min(data()[,1]),max(years)),ylim=c(0,max(S.sum$uci,data()[,2])/u), main='', xlab = '',ylab=paste('Escapement',mult))
  polygon(c(years,rev(years)),c(S.sum$uci/u,rev(S.sum$lci/u)),col=tcol('grey',50),border=NA)
  lines(years,S.sum$md/u,lwd=2,lty=1)
  lines(years,S.sum$me/u,lwd=2,lty=2)
  polygon(c(lyear-1,max(years)+2,max(years)+2,lyear-1),c(input$LEG,input$LEG,input$UEG,input$UEG),col=tcol(5,80),border=NA)
  }
  if(input$pltmse=='Harvest')
  {   
  # Plot S
  plot(data()[,1],(data()[,3]-data()[,2])/u,type='l',lwd=2,xlim=c(min(data()[,1]),max(years)),ylim=c(0,max(H.sum$uci,(data()[,3]-data()[,2]))/u), main='', xlab = '',ylab=paste('Harvest',mult))
  polygon(c(years,rev(years)),c(H.sum$uci/u,rev(H.sum$lci/u)),col=tcol('grey',50),border=NA)
  lines(years,H.sum$md/u,lwd=2,lty=1)
  lines(years,H.sum$me/u,lwd=2,lty=2)
  abline(h=input$minH, col=2,lwd=2)
  }
  legend('topright',legend=c('Median','Mean'),lwd=2, lty=c(1,2),box.lty=0)
})

#-------------------------------------------------------------------------------
# Plt_mse_rep:  MSE plot for each simulation 
#-------------------------------------------------------------------------------  
output$Plt_mse_rep <- renderPlot({
  u <- unit()
  mult <- mult(u)
  lyear <- max(data()[,1])
  nyrs <- input$simy
  years <- c(1:nyrs)+lyear
  simout <- msesim()
  j <- as.numeric(input$sim.rep)
  N <- simout[[j]]$N
  H <- simout[[j]]$H
  S <- simout[[j]]$S
  par(yaxs='i',bty='l')
# Run size 
  plot(data()[,1],data()[,3]/u,type='l',lwd=2,xlim=c(min(data()[,1]),max(years)),ylim=c(0,max(max(N),max(data()[,3]))/u), main='', xlab = '',ylab='')
  lines(years,N/u)
# Harvest
  lines (data()[,1],(data()[,3]-data()[,2])/u,type='l',lwd=2, col=2)
  lines(years,H/u,col=2)
# Escapement 
  lines (data()[,1],(data()[,2])/u,type='l',lwd=2, col=3)
  lines(years,S/u, col=3,lwd=1)
  polygon(c(lyear-1,max(years)+2,max(years)+2,lyear-1),c(input$LEG,input$LEG,input$UEG,input$UEG),col=tcol(5,80),border=NA)
  abline(h=input$minH, col=2,lwd=2)
  title("MSE Simulation", xlab="Year", ylab=paste('Run / Escapement/Harvest',mult))
  legend('topright',legend=c('Run','Escapement','Harvest'),col = c(1,3,2),lwd=c(1,2,1), box.lty=0)
})

output$sim.lnalphai <- renderPlot({
  if (input$add=='kf'){
    
  }
  })


#output$Tbl_mse <- renderDataTable({aggregate(length~rep+crit,FUN=sum, data=MSE.sum())})  
#output$Tbl_mse <- renderDataTable({MSE.sum()}) 

#output$sims.out <- renderPrint({
#  fail.sum <- aggregate(length~rep+crit,FUN=sum, data=MSE.sum())
#  fail.sum.w <- dcast(fail.sum,rep~crit)
#  fail.sum.w[is.na(fail.sum.w)]<- 0
#  return(summary(fail.sum.w[,-1]))
# })

#-------------------------------------------------------------------------------
#  Estimate frequencies of 
#-------------------------------------------------------------------------------
output$sims.out2 <- renderText({
  dat<- MSE.sum()
  fail.Esc <- dim(dat[which(dat$crit=='EG' & dat$length>=input$conLEsc),])[1]/input$nsim
  fail.Hm <- dim(dat[which(dat$crit=='Hm' & dat$length>=input$conminH),])[1]/input$nsim
  fail.H0 <- dim(dat[which(dat$crit=='H0' & dat$length>=input$con0H),])[1]/input$nsim
  fail.Run <- dim(dat[which(dat$crit=='ND' & dat$length>0),])[1]/input$nsim
  head <- paste('During',input$simy,'years')
  t.ND <- paste('Run Extinct',round(100*fail.Run,0),'%')
  t.H0 <- paste('Complete fishery closure more than',input$con0H,'consecutive years',round(100*fail.H0,0),'%')
  t.Hmin <- paste('Below minimum harvest more than',input$conminH,'consequtive years',round(100*fail.Hm,0),'%')
  t.EG <- paste('Below escapement goal more than',input$conLEsc,'consequtive years',round(100*fail.Esc,0),'%')
  out <- paste(head, t.ND,t.H0,t.Hmin,t.EG,sep='\n')
  return(out)
})

output$sims.out.rep <- renderText({
  j <- as.numeric(input$sim.rep)
  dat<- MSE.sum()
  dat.j <- dat[which(dat$rep==j),]
  fail.Esc.max <- max(dat.j[which(dat.j$crit=='EG'),'length'])  
  fail.Esc <- dim(dat.j[which(dat.j$crit=='EG' & dat.j$length>=input$conLEsc),])[1]
  fail.Esc.max <- max(dat.j[which(dat.j$crit=='EG'),'length'])
  fail.Hm <- dim(dat.j[which(dat.j$crit=='Hm' & dat.j$length>=input$conminH),])[1]
  fail.Hm.max <- max(dat.j[which(dat.j$crit=='Hm'),'length'])
  fail.H0 <- dim(dat.j[which(dat.j$crit=='H0' & dat.j$length>=input$con0H),])[1]
  fail.H0.max <- max(dat.j[which(dat.j$crit=='H0'),'length'])
  
  head <- paste('During',input$simy,'years','frequency of')
  t.H0 <- paste('Complete fishery closure more than',input$con0H,'consecutive years',fail.H0,'maximum years',fail.H0.max)
  t.Hmin <- paste('Below minimum harvest more than',input$conminH,'consequtive years',fail.Hm,'maximum years',fail.Hm.max)
  t.EG <- paste('Below escapement goal more than',input$conLEsc,'consequtive years',fail.Esc,'maximum years',fail.Esc.max)
  
  out <- paste(head, t.H0,t.Hmin, t.EG,sep='\n')
  return(out)
  
})

# downloadData ---- Results download -------------------------------------------
output$simdownload <- downloadHandler(
  filename = function() {
    paste0('MSEdata_', model.name(),'_', Sys.Date(),'.csv')
  },
  content = function(file) {
    write.csv(as.data.frame(t(do.call("cbind", msesim()))), file,row.names = FALSE)
  }
)

output$Txt_sum.mse <- renderPrint({
  # Spread out list data 
  simout <- do.call("cbind", msesim())
  # convert N,S,H to vector  
   N <- c(as.matrix(simout[,names(simout)=='N']))
   H <- c(as.matrix(simout[,names(simout)=='H']))
   S <- c(as.matrix(simout[,names(simout)=='S']))
  # x combine to a data.frame 
   x <- data.frame(N,S,H)
   min <- sapply(x,min,na.rm=TRUE)
   max <- sapply(x,max,na.rm=TRUE) 
   mean <- sapply(x,mean,na.rm=TRUE)
   median <- sapply(x,median,na.rm=TRUE)
   sd <- sapply(x,sd,na.rm=TRUE)
   cv  <- sd/mean
   out <- data.frame(round(min,0),round(mean,0),round(median,0),round(sd,0),round(cv,3),
                      round(max,0))
   names(out) <- c('Min','Mean','Median','SD','CV','Max')
   row.names(out) <- c('Run','Escapement','Harvest')
   return(out)
  })

output$Plt_sum.mse <- renderPlot({
  # Spread out list data 
  simout <- do.call("cbind", msesim())
  # convert N,S,H to vector  
  N <- c(as.matrix(simout[,names(simout)=='N']))
  H <- c(as.matrix(simout[,names(simout)=='H']))
  S <- c(as.matrix(simout[,names(simout)=='S']))
  # x combine to a data.frame 
  x <- data.frame(N,S,H)
  boxplot(x,horizontal = TRUE, outline=FALSE,names = c('Harvest','Escapement','Run'))
})

output$Plt_rep.mse <- renderPlot({
  j <- as.numeric(input$sim.rep)
  x <- msesim()[[j]][,c('H','S','N')]
  boxplot(x,horizontal = TRUE, outline=FALSE,names = c('Harvest','Escapement','Run'))
})

output$Txt_HE_mse <- renderText({
# Calculate the number of years by each rep 
  dat.sum <- aggregate(length~rep+crit,FUN=sum, data=MSE.sum())
#  nyrs <- input$simy: number of simulation years
  H0 <- dat.sum[dat.sum$crit =='H0','length']
  Hm <- dat.sum[dat.sum$crit =='Hm','length']
  EG <- dat.sum[dat.sum$crit =='EG','length']  
  # Frequency of fishery closure  
  t.H0 <- paste('Complete fishery closure:',round(100*mean(H0)/input$simy,0),'%',
                'Min',round(100*min(H0)/input$simy,0),'%','-','Max',round(100*max(H0)/input$simy,0),'%')
  t.Hmin <- paste('Below minimum harvest target:',round(100*mean(Hm)/input$simy,0),'%',
                  'Min',round(100*min(Hm)/input$simy,0),'%','-','Max',round(100*max(Hm)/input$simy,0),'%')
  t.EG <- paste('Below escapement goal:',round(100*mean(EG)/input$simy,0),'%',
                'Min',round(100*min(EG)/input$simy,0),'%','-','Max',round(100*max(EG)/input$simy,0),'%')
  out <- paste(t.H0,t.Hmin, t.EG,sep='\n')
  return(out)
})

output$Txt_rep.mse <- renderPrint({
  j <- as.numeric(input$sim.rep)
  x <- msesim()[[j]][,c('N','S','H')]
  min <- sapply(x,min, na.rm=TRUE)
  max <- sapply(x,max, na.rm=TRUE)
  mean <- sapply(x,mean, na.rm=TRUE)
  median <- sapply(x,median, na.rm=TRUE)
  sd <- sapply(x,sd,na.rm=TRUE)
  cv  <- sd/mean
  out <- data.frame(round(min,0),round(mean,0),round(median,0),round(sd,0),round(cv,3),
                    round(max,0))
  names(out) <- c('Min','Mean','Median','SD','CV','Max')
  row.names(out) <- c('Run','Escapement','Harvest')
  return(out)
})

output$Plt_freq_mse <- renderPlot({
  par(mfrow=c(1,3))
  dat <- MSE.sum()
  barplot(table(dat[dat$crit=='H0','length']),main='Frequency of consecutive fishery closure',xlab='Years')
  barplot(table(dat[dat$crit=='Hm','length']),main='Frequency of consecutive harvest below minimum',xlab='Years')
  barplot(table(dat[dat$crit=='EG','length']),main='Frequency of consecutive escapenent below lower bound',xlab='Years')
})


#===============================================================================
# Reporting Section 
#===============================================================================
# Create and download report 
output$downloadReport <- downloadHandler(
  filename = function(){paste0('SR_Report_',model.name(),'_',Sys.Date(),'.docx')
    },
  content = function(file) {
    tempReport <- file.path(tempdir(), "report_officedown.Rmd")
    tempTemplate <- file.path(tempdir(), "template.docx")
    file.copy('report_officedown.Rmd',tempReport,overwrite = TRUE) 
    file.copy('template.docx',tempTemplate,overwrite = TRUE) 
    # SEE HERE
# temporarily switch to the temp dir, in case you do not have write
# permission to the current working directory
#    src <- normalizePath('report.Rmd')
#    owd <- setwd(tempdir())
#    on.exit(setwd(owd))
#    file.copy(src, c('report.Rmd','template.docx'), overwrite = TRUE) # SEE HERE
    params <- list(
  #    Tbl_sum = sumbayes(),
  #    srplot = srplot()
      )
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    progress$set(message = 'Generating a report',
                 detail = 'This may take a while...')
    for (i in 1:150) {progress$set(value = i)}
  
    rmarkdown::render(tempReport, output_file = file, output_format = 'word_document',
                      params = params,
                      envir = new.env(), intermediates_dir = tempdir()
  )
  }
 ) # End downloadReport

})# End of Server 

# Create Shiny app ----
shinyApp(ui, server)
