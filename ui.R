#'==============================================================================
#'  Shiny Escapement Goal Analyses App
#'  File name: ui.R
#'  Author:  Toshihide "Hamachan" Hamazaki 
#'  Date: 
#'  Description
#'  This program is conducts Escapement Goal Analyses using Bayesian SR models
#'   
#'
#'==============================================================================
# Include required packages 
library(shiny)
library(bslib)
library(rmarkdown)     # used to get rmarkdown file
library(markdown)     # used to get rmarkdown file
library(knitr)         # used to produce word report 
library(reshape2)     # used for data transpose 
library(gsl)       # used for dwtest 
library(car)          # used for dwtest 
library(mgcv)         # used for spline 
library(R2jags)       # used to run JAGS
options(scipen=999)   # Do not show Scientific notation
#'--- Required Source code------------------------------------------------------
source("Rcode/Shiny_modules.R")   #  Module codes 
source("Rcode/Help_Info.R")  # Help doc info

#options(shiny.legacy.datatable = TRUE)
#'==============================================================================    
#' UI Section----  
#'==============================================================================
ui<-fluidPage(
  div(style = "background-color: #f8f9fa; padding: 15px; text-align: center;",
      img(src="Picture2.png",height=50, width=50)
      ,HTML('<span style="font-size:32px;">Pacific Salmon SR Escapement Goal Analyses</span>')),
 navbarPage(
    theme =  bs_theme(version = 3, bootswatch = 'cerulean'), 
     id = "tabs",
#     title = div(
#        img(src="Picture2.png",height=40, width=40)
#        , "Pacific Salmon SR Escapement Goal Analyses"),
title='',
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Panel 1  Data Input and Submit ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("Data Input",
### Sidebar Panel ================================================================           
   sidebarPanel(width = 3,
#'------------------------------------------------------------------------------ 
#'###  Data file Input ----
#'------------------------------------------------------------------------------
#' Data type selection ---------------------------------------------------------
selectInput(inputId="dataType",
#' Add popup information mark---------------------------------------------------        
 InfoUI('info1','Input Data Type'), 
choices = c('Run','S-R','Escapement Only')),

#' Sample data input -----------------------------------------------------------
checkboxInput(inputId="Sample", "Import Sample Data", FALSE), 
# File input UI: shows when sample data input is false
conditionalPanel(condition="input.Sample== false",
# File Input module 
  dataInputUI("datain", "User data (.csv format)"),
 ),

# Show Age range (Only appear when data type is "Run")
  uiOutput('agerange'),
# Whether to combine or eliminate Ages (Only appear when data type is "Run")
  uiOutput('agecomb'),
# Limit Year range 
  uiOutput('yrange'),
#  textOutput('test'),
#'------------------------------------------------------------------------------
  hr(),
# input_switch('gg','ggplot'),
#' Default axis point UI --------------------------------------------------------
 checkboxInput(inputId="autoui", "Defalut axis unit", TRUE),
 conditionalPanel(condition="input.autoui== false",
    selectInput(inputId="ui","Figure Axis Dislpay Unit", choices = c('1','1000','million'))
     )
   ), # End sidebarPanel (Data Input)

### Main Panel ========================================================
  mainPanel(
    tabsetPanel(id="subTab",
#'------------------ Show Input data -------------------------------------------      
      tabPanel("Input Data",
               DT::DTOutput('Tbl_data')
               ),
#'------------------ Corrected Run Table ---------------------------------------      
      tabPanel("Run Table",
          DT::DTOutput("Tbl_data.run")
       
           ), # End tabPanel

#'------------------ Brood Table -----------------------------------------------  
      tabPanel("Brood Table",
                DT::DTOutput("Tbl_data.brood")
                  ), # End tabPanel
#'------------------ Time Series -----------------------------------------------
      tabPanel("Time Series",
              plotOutput("Plt_srt"),
              plotOutput('Plt_runesc')  
              # Activates only when run is available
                ),#End tabPanel
#'------------------ Data summary  ---------------------------------------------
      tabPanel("Summary",
               tableOutput('Tbl_sum_sr.data'),
               plotOutput('Plt_hist.sry',height='300px'),
               tableOutput('Tbl_sum_run.data'),
               plotOutput('Plt_hist.run',height='300px')
             ) # End tabPanel
        )  # End tabsetPanel
      )  # End mainPanel
     ), # End Data Input tabPanel

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  Panel 2 Escapement Only Analyses---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
tabPanel("Escapement Only Analyses",
### Sidebar Panel ==============================================================         
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
        strong(htmlOutput('Txt_Risk'))
                 ) # End conditionalPanel
          ), #End SidbarPanel
### Main Panel ======================================================    
  mainPanel(
    tabsetPanel(id = "ePanel",
#'------------------ Percentile Analyses ---------------------------------------   
      tabPanel("Percentile Analyses",
# Parcentile figure                
        plotOutput("Plt_prcnt"),  
# Histogram
        plotOutput("Plt_prcnt_hist") 
            ), #End tabPanel:Percentile
#'------------------ Risk Analyses  --------------------------------------------  
      tabPanel("Risk Analyses",
#  Main Risk Analyses                
        plotOutput(height = '400px', "Plt_risk"),
#  Time series 
        plotOutput(height = '300px', "Plt_risk2"),
# Risk Table 
        tableOutput('Tbl_risk'),
        tableOutput('Tbl_riskp'),
# Durbin-Watson Statistics 
          p(strong("Durbin-Watson Serial Correlation Analyses")),
          verbatimTextOutput('Txt_dwtest')
            ), #End tabPanel: Risk
#        tabPanel("Risk Table",
#                 ),
           )#End tabsetPanel
         )#End main Panel 
  ),#End tabPanel Escapement only Analysis 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  Panel 3  SR Model Data Analyses ---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
navbarMenu("SR Model",
  tabPanel("Bayes Model",
           
    sidebarPanel(width = 3,
#'--- ConditionalPanel Bayes Model Control -------------------------------------      
      conditionalPanel(condition="input.Panel == 'Bayes Model'|| input.Panel =='Model Code'||input.Panel =='Model data'",
        p(strong("Bayesian Model Setting")),
          selectInput('Model',"Select SR Model",choices=c('Ricker','Beverton-Holt'),
                    selected ='Ricker'),
#'--------  Conditional RE option ----------------------------------------------      
        uiOutput('re'),
#'--------  Conditional SS option ----------------------------------------------      
        uiOutput('ss'),
#'--------- Model addition  ----------------------------------------------------
        radioButtons(inputId="add","Model Addition",
            choices=c("None"="none","AR(1) Error"="ar1","Time varying alpha"="kf"),
            selected = NULL),
hr(),
checkboxInput(inputId="BayesMCMC", strong("Import MCMC"), FALSE), 
conditionalPanel(condition="input.BayesMCMC == true",
                 dataInputUI("mcmc.in", "User data (.csv format)"),
),  # End conditional Panel true
conditionalPanel(condition="input.BayesMCMC == false",
    BayesInputUI('Bayes'),
checkboxInput(inputId="Priors", "Modify Priors", FALSE), 
    conditionalPanel(condition="input.Priors== true",
                     # Change Priors 
      sliderInput(inputId="lnalpha","lnalpha", value=c(-1,4),min=-5,max=10,step=0.1),
      sliderInput(inputId="beta","beta", value=c(-1,5),min=-5,max=10,step=0.1)
    ),
    p(strong('Download MCMC')),
    downloadButton("download.mc","Download")                    
   ), # End conditional panel: false

 ),  # End conditionalPanel for Bayes Model 



#'------------------------------------------------------------------------------    
# UI:  Bayes Model Control UI
#'------------------------------------------------------------------------------

#' Conditional Panel SR and Yield Plot -----------------------------------------
conditionalPanel(condition="input.Panel != 'Bayes Model'& input.Panel !='Model Code'& input.Panel !='Model data'",
  radioButtons(inputId="target",
      InfoUI('info4','Management Target Option'),                  
  choices=c("Median Recruit"="md","Mean Recruit"="me"), selected = 'md'), 

#' UI output astar: Segmented time series Only show when TVA model is selected    
        uiOutput('astar'),  # Only show when TVA model is selected 
   ),
  conditionalPanel(condition="input.Panel == 'SR Plot'|| input.Panel == 'Yield Plot'|| input.Panel == 'ln(R/S) Plot'|| input.Panel=='SR Dist Plot'",
#' Choose between Median or Mean recruit target option  
#' Plot Options ----------------------------------------------    
        p(strong("Plot options")),               
        checkboxInput(inputId="show.points", "show Years", TRUE),
        checkboxInput(inputId="show.seq", "show Seq", FALSE),
        checkboxInput(inputId="show.smsy", "show Smsy", FALSE),
        checkboxInput(inputId="show.smax", "show Smax", FALSE),
        checkboxInput(inputId="show.sgen", "show Sgen", FALSE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
#' Conditional Panel for SS output ---------------------------------------------- 
      conditionalPanel(condition="input.SS == 'State-Space Model'",
        p(strong("SS Model Plot options")),
        checkboxInput(inputId="show.ob.se", "Ob errors", FALSE),
        checkboxInput(inputId="show.ss.point", "Predicted SR", TRUE), 
        checkboxInput(inputId="show.arrows", "Shrinkage", TRUE), 
        ),
#' CI selection -----------------------------------------------------------------
        sliderInput("CI", "% Interval", value=95,min=0,max=100,step=5),
        selectInput(inputId="Li","Interval Type", choices = c('credible','prediction')),
#' S Axis selection -------------------------------------------------------------
 checkboxInput(inputId="axis", "Select Axis Range", FALSE),
 conditionalPanel(condition="input.axis== true",
        uiOutput('maxS'),
        uiOutput('maxR'),
        uiOutput('Yrange'),
         ),        
      ), # End conditionalPanel forSR and Yield Plot 

#' Summary  ------------------------------------------------------
  conditionalPanel(condition="input.Panel == 'Summary'", 
        sliderInput(inputId="CIB", "% Interval", value=95,min=0,max=100,step=5),
        checkboxInput(inputId="Remove_out", 
                      InfoUI('info3','Remove Outliers'), FALSE),
        p(strong('Download summary data')),
        downloadButton("download.sum", "Download")
        ) # End Conditional panel MCMC 
      ), # End sidebarPanel
       
#### MainPanel -----------------------------------------------------------------      
  mainPanel(tabsetPanel(id = "Panel",
##### Bayes Model -----------------------------------------------        
    tabPanel("Bayes Model",
        p(strong("Trace Plots")),
        plotOutput("Plt_trace"),
        p(strong("Model summary")), 
        verbatimTextOutput('BayesSum')  
          ),#End tabPanel:Bayes
##### SR Plot-----------------------------------------------------        
    tabPanel("SR Plot",
        p(strong("Click to show data point")),
        strong(textOutput("SRinfo")),
        plotOutput(height='500px','Plt_SR',click ='SR_click'
                   ),
         textOutput("Txt_SR")
            ),#End tabPanel: SR Plot
##### Yield Plot--------------------------------------------------        
    tabPanel("Yield Plot",
        plotOutput(height='500px','Plt_yield'),
        (textOutput("Txt_YD")),
            ),#End tabPanel: Yield Plot
##### ln(R/S) Plot--------------------------------------------------        
  tabPanel("ln(R/S) Plot",
        plotOutput(height='500px','Plt_lnRS'),
        textOutput("Txt_lnRS"),
            ),#End tabPanel: lnRS
#####  SR Refrence Plot  -------
  tabPanel("SR Dist Plot",
        plotOutput(height='500px',"Plt_MSY.mc"),
  p("Probability distribution of reference parameters. The vertical line indicates mean."),
            ),#End tabPanel: Model Code
##### 3.1.6  SR Status plot  -------
  tabPanel("Kobe Plot",
          plotOutput(height='500px',"Plt_kobe"),
          p("Kobe Plot: Escapement vs Harvest Rate by calenar year. Vertical line is Smsy and hoizontal line is Umsy. 
            Generally, The upper left (>Umsy and <Smsy) is considered at Unsustainable state, 
            and the lower right (<Umsy and >Smsy) is Perfectly sustainable state."),
            ),#End tabPanel: Model Code

##### Summary ------------------------------------------------------- 
  tabPanel("Summary",
         tableOutput('Tbl_sumpost'),
         plotOutput(height='500px',"Plt_hist.mc"),
         hr(),
      p("Probability distribution of the model and biological reference parameters. The vertical line indicates mean (solid) and median (hash)."),
         hr(),
  conditionalPanel(condition="input.Remove_out == true", 
         p(strong('Outlier summary')),
         tableOutput('Tbl_Ref_sum')
            )
    ),#End tabPanel: Summary
##### Model code ---------------------------------------------------------------
  tabPanel("Model Code",
         verbatimTextOutput('modelcode')
    ),#End tabPanel: Model Code
##### Priors  --------------------------------------------------------
##### Model data ---------------------------------------------------
 tabPanel("Model data",
         verbatimTextOutput('modeldata')
     )#End tabPanel: Model data
           )#End tabsetPanel
         ) #End mainPanel
      ),#End SR Model tabPanel

tabPanel("Model Diagnoses",
         sidebarPanel(width = 3,
        ), # End sidebarPanel
#### MainPanel -----------------------------------------------------------------       
 mainPanel(
    tabsetPanel(id="ssTab",
##### Bayes Model Code ------------------------------------------- 
##### Residuals  ------------------------------------------------- 
  tabPanel("Residuals", 
         plotOutput(height='400px',"Plt_predict"), 
  p("Predicted vs. Observed ln(Recruit). The gray shade indicates 95% credible interval."),
         plotOutput(height='400px',"Plt_residual"),
  p("Resicdaual plot between predicted and observed ln(Recruit). The red line indicated mean and gray shade indicates 95% credible interval."),
         strong(textOutput('Txt_dwtest.title')),
         verbatimTextOutput('Txt_dwtest.resid'),
         plotOutput(height='400px',"Plt_lnalphai"),
    ),#End tabPanel: Diagnoses
 tabPanel("Run Size", 
         plotOutput(height='900px',"Plt_SS"),     
  p("Predicted vs. Observed run size. The gray shade indicates 95% credible interval."),
                   ),#End tabPanel: Diagnoses
 tabPanel("Run Age Comp", 
         plotOutput(height='900px',"Plt_SS_Age"),
  p("Predicted vs. Observed run age compositon. The gray shade indicates 95% credible interval."),
 ),#End tabPanel: Diagnoses
 tabPanel("Brood Age Comp", 
         plotOutput(height='900px',"Plt_SS_BAge"),
  p("Predicted vs. Observed brood recruit age compositon. The gray shade indicates 95% credible interval."),
 ),#End tabPanel: Diagnoses
##### MCMC data --------------------------------------------------------------------
  tabPanel("MCMC data",
     DT::DTOutput('Tbl_mcmcdata')
#     dataTableOutput('Tbl_mcmcdata')
            ),#End tabPanel: Model Code
    ) # End tabsetPanel
  ) # End MainPanel
  ) # End TabPanel Model Diagnoses
),  # End navbarMenue

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Panel 4: Escapement Goal Analyses---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
navbarMenu("Escapement Goal Analyses",
#'===============================================================================
#'### Smsy-Smax Goal Analyses ----
#'===============================================================================    
tabPanel("Smsy & Smax Goal Analyses",      
   sidebarPanel(width = 3,
      strong(htmlOutput("model.1")),        
      conditionalPanel(condition="input.ProfPanel != 'Custom Range'",           
       ProfileUI('smsy','MSY'),
       hr(),
       ProfileUI('smax','Rmax'),
       hr(),
#       downloadUI('Prof','Download Profile Data'),
      downloadButton('download.prof','Download'),
#       checkboxInput("xrange", label = "Customize", value = FALSE),
#   conditionalPanel(condition="input.xrange == true",
#     sliderInput(inputId="lensim","S segments", value=200,min=100,max=1000,step=1),
       uiOutput('prof.range'),
#        )
       ), # End conditionalPanel
  conditionalPanel(condition="input.ProfPanel == 'Custom Range'",
        p(strong('Custom Profile Range Panel')),
        p('Select Min % range and Target % range to create escapement goal range'),
        sliderInput(inputId="ProfM","Min % Range", min=0,max=95, value = c(70,90),step =5),
        sliderInput(inputId="ProfP","Target % Range", min=0,max=95, value = c(70,90),step =5),
        p(strong('Download Profile Summary')),
 downloadButton('download.profSum',label='Download'),
#        downloadUI('ProfSum','Download Profile Summary'),

        )
       ),#End sidebarPanel
       
#'=========================MainPanel=============================================
mainPanel(
      tabsetPanel(id = "ProfPanel",
#### Smsy Profile ----------------------------------------------- 
        tabPanel("Profile",
            fluidRow(
      column(10,plotOutput(height='400px','Plt_Smsy_prof')),  
      column(2,tableOutput('Tbl_MSY_gl'))
                      ),
      p("MSY Profile Analyses Plot. 
        The profile line indiate probability of yield exceeding x% of MSY at given escapement.
        Intersecion of the probablity profile and target probability (red line) shows 
        the range of escapement meeting the criteria."),
      fluidRow(
      column(10,plotOutput(height='400px','Plt_Smax_prof')),  
      column(2,tableOutput('Tbl_Rmax_gl'))
                      ),
      p("RMAX Profile Analyses Plot. 
        The profile line indiate probability of yield exceeding x% of RMAX at given escapement.
        Intersecion of the probablity profile and target probability (red line) shows 
        the range of escapement meeting the criteria.")
            ), #End tabPanel: Profile
#### Profile Analyses ----------------------------------------------------   
        tabPanel("Optimal",
              splitLayout(cellWidths = c("50%", "50%"),
              plotOutput(height='600px','Plt_msyprof_r'),
              plotOutput(height='600px','Plt_maxprof_r')
              ),
          splitLayout(cellWidths = c("50%", "50%"),
          p("MSY Optimal profile plot. Each ridge indicates a range of Escapement in which the yield exceeding 
             x % of MSY. The profile line indicates the proportion of ridges that intersected at given Escapement."),
          p("Rmax Optimal profile plot. Each ridge indicates a range of Escapement in which the yield exceeding 
             x % of Rmax. The profile line indicates the proportion of ridges that intersected at given Escapement.")
          )
                       
        ), #End tab Panel

#### Smsy Yield Profile -----------------------------------------
        tabPanel("Yield & Recruit",
          plotOutput(height='300px','Plt_yield.pg'),
          plotOutput(height='300px','Plt_rec.pg')
              ), #End tabPanel: YieldRec
#        tabPanel("Custom Range",
#                 DTOutput('Tbl_prof'),
#                 tableOutput('Tbl_prof'),
#                p(strong("Smsy Goal Range")),        
#          htmlOutput("Txt_Srange.smsy"),
#                p(strong("Smax Goal Range")), 
#          htmlOutput("Txt_Srange.smax")
#              ), #End tabPanel: Custom Range  
#        tabPanel("Help",
#         withMathJax(includeMarkdown("documents/Profile_help.md"))
#                ) #End tabPanel: Help 
            )#End tabsetPanel
        )#End mainPanel
      ),#End tabPanel: Smsy Goal Analyses 
 
#'===============================================================================    
### Yield & Recruit Goal Analyses ----
#'===============================================================================    
  tabPanel("Yield & Recruit Goal Analyses",
    sidebarPanel(width = 3,
      conditionalPanel(condition="input.cPanel == 'Recruit Goal Analyses'||input.cPanel == 'Optimal'",  
        p(strong("Recruit Goal Analyses")), 
        uiOutput('minRec'),
        sliderInput("r1p", "Min % Achieve", value=90,min=0, max=100,step=5)
        ), # End conditionalPanel
      conditionalPanel(condition="input.cPanel == 'Yield Goal Analyses'||input.cPanel == 'Optimal'",             
        p(strong("Yield Goal Analyses")),
        uiOutput('minYield'),
        sliderInput("y1p", "Min % Achieve", value=90,min=0, max=100,step=5)
        ) # End conditionalPanel
      ),  # End sidebarPanel

#'=========================MainPanel=============================================       
    mainPanel(
      tabsetPanel(id = "cPanel",
#### Yield Goal  Profile ----------------------------------------                
        tabPanel("Yield Goal Analyses",
            plotOutput(height='300px','Plt_yield.gl'),
            plotOutput(height='300px','Plt_yield.prof'),
            verbatimTextOutput("Txt_Yield_gl")
        ),
#### Recruit Goal Profile ---------------------------------------   
        tabPanel("Recruit Goal Analyses",
          plotOutput(height='300px','Plt_rec.gl'),
          plotOutput(height='300px','Plt_rec.prof'),
          verbatimTextOutput("Txt_Rec_gl")
         ),# End tabPanel
#### Profile Analyses ----------------------------------------------------   
        tabPanel("Optimal",
              splitLayout(cellWidths = c("50%", "50%"),
              plotOutput(height='600px','Plt_yield_r'),
              plotOutput(height='600px','Plt_rec_r')
              )
        ), #End tab Panel
          )#End tabsetPanel
        )#End maiPanel
      ),#End tabPanel Recruit & Yield Goal Analyses 
              
#'------------------------------------------------------------------------------    
### Custom Escapement Goal Evaluation ---- 
#'------------------------------------------------------------------------------
tabPanel("Custom Escapment Goal Range Analyses",
    sidebarPanel(width = 3,
      p(strong("Select Lower and Upper Escapement Goal")),
      fluidRow(
      column(6,uiOutput('minEG')),  
      column(6,uiOutput('maxEG'))
       ),
      p("Submit Goal Range for  Analyses"),
      actionButton("Run","Run"),
# Horizontal line
      hr(),
      uiOutput('cyg'),
      uiOutput('crg')
                       ), #End Sidepar Panel
#'============= Main Panel =====================================================    
    mainPanel(
      tabsetPanel(
#### Profile Analyses ----------------------------------------------------   
        tabPanel("Profile Analyses",
            fluidRow(
      column(10,plotOutput(height='400px','Plt_msyprof_c')),  
      column(2,tableOutput('Tbl_msyprof_c'))
                      ), 
      fluidRow(
      column(10,plotOutput(height='400px','Plt_maxprof_c')),  
      column(2,tableOutput('Tbl_maxprof_c'))
                      )       
        ), #End tab Panel

#### Expected Yields ---------------------------   
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
#### Expected Recruit  --------------------------   
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
#      tabPanel("Help",
#        withMathJax(includeMarkdown("documents/Custom_Escapement_help.md"))     
#         )# End tabPanel
        )#End tabsetPanel
      )#End main Panel 
     ), #End tabPanel: Custom Escapement Goal Analyses 

    ),#End Escapement Goal Analyses navbarMenu
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Panel 5  Management Strategy Evaluation ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
navbarMenu("MSE Analyses",
#conditionalPanel(condition ="input.dataType=='Run'",       
#'-------------------------------------------------------------------------------    
### Simulation model UI ----
#'-------------------------------------------------------------------------------  
#-------------------------------------------------------------------------------
# Management Strategy Evaluation ----
#-------------------------------------------------------------------------------
 tabPanel("Management Starategy Evaluation",
  sidebarPanel(width = 3,        
 conditionalPanel(condition="input.MSEPanel == 'Simulation Run'",           
  fluidRow(
        selectInput(inputId="strType", InfoUI('info5','Management Strategies') ,
          choices = c('Escapement','Harvest','Hybrid'),selected = 'Escapement'),
conditionalPanel(condition="input.strType != 'Harvest'",          
  p(strong("Escapement Goal")), 
     fluidRow(
      column(6,uiOutput('LEG')),  
      column(6,uiOutput('UEG')),
      ),
     ),
    p(strong("Fishery Target & Capacity")),  
     fluidRow(
      column(6,uiOutput('minH')),  
      column(6,uiOutput('maxH')),
      ),

#      ),
  conditionalPanel(condition="input.strType == 'Harvest'", 
    numericInput(inputId="Hrate", "Target Harvest Rate %", value=50,min=0,max=90,step=5)                   
  ),
#    numericInput(inputId="EG.rev", "Escapement Goal Review Frequency", value=0,min=0,max=20,step=1),
    ),
     fluidRow(
       column(6,uiOutput('nsim')),  
       column(6,
  selectInput(inputId="cmode","Target Eecapement", 
       choices = c('Lower','Middle','Upper'),selected = 'Middle')
              ) 
         ),  # End fluidRow
    actionButton("SimRun","Simulate"),
#                     ),
  p(strong("Management % Error")),  
    fluidRow(
     column(6,numericInput(inputId="spred", "Assessment", value=15,min=0,max=100,step=5),
      ),
     column(6,numericInput(inputId="simpH", "Inplementation", value=10,min=0,max=100,step=5),
      ),
    ),
  sliderInput(inputId="simy", "Management Years", value=50,min=0,max=100,step=5,round=0),
      ),
  conditionalPanel(condition="input.MSEPanel == 'Sim Summary'",
    p(strong("Consecutive Years not meeting target")),
    numericInput(inputId="conLEsc", "Escapement", value=3,min=1,max=10),
    numericInput(inputId="conminH", "Minimum Harvest", value=3,min=1,max=10),
    numericInput(inputId="con0H", "No Fishery", value=3,min=1,max=10),
      ),
conditionalPanel(condition="input.MSEPanel == 'Sim rep Data'|input.MSEPanel == 'Sim replicates'",
  uiOutput('sim.rep')
 )
      ), #End sidebarPanel
                    
#'---------------- mainPanel ----------------------------------------------------
 mainPanel(
  tabsetPanel(id = "MSEPanel",
#'-------------------------------------------------------------------------------    
###  Simulation Run ----
#'-------------------------------------------------------------------------------        
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
#        DTOutput('foo2')
       verbatimTextOutput('sims.out2') ,
       verbatimTextOutput("Txt_HE_mse"),

       plotOutput('Plt_freq_mse')
                  ), #End tabPanel
#'------------------------------------------------------------------------------
#'  #***  Simulation Replicate ----
#'------------------------------------------------------------------------------
    tabPanel("Sim replicates",
           plotOutput(height='500px','Plt_mse_rep'),  
           verbatimTextOutput("Txt_rep.mse"),
           plotOutput("Plt_rep.mse"),
           verbatimTextOutput('sims.out'),
           verbatimTextOutput('sims.out.rep') ,  
#          DTOutput('Tbl_mse')
#          plotOutput(height='600px',"altsim.N"),
           p(strong('Download Simulation Data')),
           downloadButton("download.mse.sim", "Download")
#            downloadUI('MSE_Sim','Download Simulation Data')
                  ),  #End tabPanel
tabPanel("Sim rep Data",
#    p('Total Frequency of H0: Fishery Closure, Hmin: Bellow minimum Harvest Target,
#      EG: Bellow Lower EG, ND: Fish extinct'),     
    DT::DTOutput('Tbl_MSE_sum')
),  #End tabPanel


#'------------------ Model Parameters ---------------------------------   
    tabPanel("Model Parameters",
      fluidPage(
            title = 'Set MSE Simulaiton Initization Parameters',
            hr(),
      fluidRow(
        column(4,
#          p(strong("Simulation Years")),           
            sliderInput(inputId="maxHR", "Maximum Harvest Rate", value=90,min=0,max=100,step=5,round=0),
#            sliderInput(inputId="train", "Training", value=25,min=0,max=100,step=5,round=0),
                    ),
        column(4,
          p(strong("Errors")),       
           sliderInput(inputId="sobsH", "Observation Error Harvest", value=10,min=0,max=100,step=5),
           sliderInput(inputId="sobsE", "Observation Error Escapement", value=30,min=0,max=100,step=5),
           sliderInput(inputId="Nobage", "Observation Error Age Comp (smaple size)", value=100,min=10,max=500,step=10)
                  ),
        column(4,
          p(strong("Population Errors")), 
                )
              ) # End fluidRow
            )# End fluidOPage
          ) # End tabPanel
        )#End tabsetPanel
        )#End mainPanel
        ) # End MSE panel 
# )
           
#'------------------ Model Descriptions -----------------------------------------   
#    tabPanel("Help",
#      tabsetPanel(
#'------------------ Model Structure  -------------------------------------------             
#      tabPanel("Model description",
#                       includeMarkdown("documents/MSE_help.md")
#                      ),  #End tabPanel
#'------------------ Parameters Description ---------------------------------             
#      tabPanel("Base model Parameters",
#                    h3("Simulation length"),
#                    p("- Burnin: Years to make modle stabilize "),
#                    p("- Training: Years SR data are collected befre setting active management"),
#                    p("- Managmenet: Years active mangement is conducted"),
#                    h3("Management Errors"),
#                    p("Fishery management takes following steps: 1) predict preseason run size, 
#              2) determin harves target, 3) execute harvests, and 
#              4) observe harvest and escapetnt to set and escapement goal. The model incorporates errors
#                  associated with each step.  Errors were modeled as independent log-normal"),
#                    p("- Preseaon Run prediction: Accuracy +/-  x%"),
#                    p("- Management Implementation: Accuracy +/-  x%"),
#                    p("- Harvest Observation: Accuracy +/-  x%"),
#                    p("- Escapement Observation: Accuracy +/-  x%"),
#                    p("Observed postseason run size is Observed harvest + escapement"),
#                    h3("Poplation Dynamic"),
#                    p("- AR1 corelation: Recuruitment error was modeled as AR1 with sigma and phi (correlation),
#              sigma is derived from SR model fit."),
#                    p("- Drishelet D. Brood age proportion is modeled as Drishelet distrribution
#              D determines level of variation. Lower D indicate higher variation")
#                      ) # End tabPanel
#                    )# End tabsetPanel
#           )#End tabPanel 
    ),#End MSE navMenue Panel

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Panel 6  Report Output section -----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("Report Output",   
        sidebarPanel(
          p('Report '), 
          downloadButton('downloadReport',label='Download'),
          p('Table'),
          downloadButton('downloadTable',label='Download'),      
        ),
   mainPanel(
    h1("Under Construction"),
      textInput("txt_title", label = "Stock Name", value = ""),
      textAreaInput("txt_free", label="Project Description", value="",width='auto'),
      fluidRow( 
        column(3, 
          h3("Data Input"),
            checkboxInput("Brood_plt", label = "SR plot", value = TRUE),
      conditionalPanel(condition="input.dataType=='Run'",  
            checkboxInput("Run_plt", label = "Run plot", value = TRUE),
            checkboxInput("Run_t", label = "Run Table", value = TRUE),
            checkboxInput("Brood_t", label = "Brood Table", value = TRUE),
                   ) # End conditionalPanel
                 ), 
        column(3, 
          h3("SR Model"),
           checkboxInput("SR_plt", label = "SR Plot", value = TRUE),
           checkboxInput("YLD_plt", label = "Yield Plot", value = FALSE),
           checkboxInput("SREQ", label = "SR Equations", value = TRUE),
          h3("Diagnosess"),
           checkboxInput("lSR_plt", label = "ln(R/S) Plot", value = FALSE),
           checkboxInput("Res_plt", label = "Residual Plot", value = TRUE),
            conditionalPanel(condition="input.SS==true",  
            checkboxInput("SREQ", label = "SR Equations", value = TRUE),
              ), # End conditional Panel
             ), #End of Column
        column(3, 
          h3("Escapement Goal Analyses"),     
            checkboxInput("MSY_prof", label = "MSY Profile Analyses", value = TRUE),
            checkboxInput("MSR_prof", label = "RMAX profile Analyses", value = FALSE),
            checkboxInput("T_Yld_prof", label = "Target Yield Analyses", value = FALSE),
            checkboxInput("T_Rec_prof", label = "Target Recruit Analyses", value = FALSE),
          #h3("Custom Goal"),
           # checkboxInput("C_Yld_prof", label = "Target Yield profile", value = FALSE),
            #checkboxInput("C_Rct_prof", label = "Target Recurit Profile", value = FALSE),
                 ), #End of Column
        column(3,
          conditionalPanel(condition="input.dataType=='Run'",  
          #h3("MSE Analyses"),     
                      
                  ) # End of Conditional Panel
                 ) #End of Column
                  ) # End fluidRow
    
                  ) # End main panel 
  ),  # End (Report Output panel)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Panel 7  Help Section ----  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("Help",
    navlistPanel(widths = c(3,9),
    "Escapement Goals Introduction",
      tabPanel("Definition",
        withMathJax(includeMarkdown("documents/Esc_Goal/Escapement_Goal_help.Rmd"))
             ),  
  "Data Input",
    tabPanel("Run Data",
      tabsetPanel(
        tabPanel("Run Data",
          withMathJax(includeMarkdown("documents/Data_Input/Input_Run_data_help.Rmd"))
             ),
        tabPanel("SS Data",
          withMathJax(includeMarkdown("documents/Data_Input/Input_Run_data_SS.Rmd"))
             ),
          tabPanel("Age Data",
          withMathJax(includeMarkdown("documents/Data_Input/Input_Run_data_brood.Rmd"))
             ),
        tabPanel("Missing Data",
          withMathJax(includeMarkdown("documents/Data_Input/Input_Missing_data_help.Rmd"))       
             ), 
          ),
      ),
     tabPanel("SR and Escapement Only Data",
      tabsetPanel(
        tabPanel("SR and Escapement Data",     
        withMathJax(includeMarkdown("documents/Data_Input/Input_SR_data_help.Rmd")),
           ),
      tabPanel("Missing Data handling",
          withMathJax(includeMarkdown("documents/Data_Input/Input_SR_data_Missing_help.Rmd"))       
             ), 
          ),
         ),
  "Escapement Only Data Analyses",
    tabPanel("Percentile and Risk Analyses",
      tabsetPanel(
        tabPanel("Escapement Only Data",       
          withMathJax(includeMarkdown("documents/Escapement_Only/ESC_Analyses.Rmd"))
            ),
        tabPanel("Percentile Analyses",       
          withMathJax(includeMarkdown("documents/Escapement_Only/Percentile_Analyses.Rmd"))
            ),
        tabPanel("Risk Analyses",       
          withMathJax(includeMarkdown("documents/Escapement_Only/Risk_Analyses.Rmd"))
          )
        )
      ),
  "SR Model",
    tabPanel("Running SR Model",
      tabsetPanel(    
        tabPanel("Running Step",
          withMathJax(includeMarkdown("documents/SR_Model_JAGS/JAGS_1.Rmd"))
          ),
        tabPanel("Model Setting",
          withMathJax(includeMarkdown("documents/SR_Model_JAGS/JAGS_2.Rmd"))
          ),
        tabPanel("Model Choice",
          withMathJax(includeMarkdown("documents/SR_Model_JAGS/JAGS_3.Rmd"))
          ),
        ),  
      ),
    tabPanel("Model Outupts",
       withMathJax(includeMarkdown("documents/SR_Model_JAGS/JAGS_help_output.Rmd"))
             ),    
    tabPanel("Model Diagnoses",
       withMathJax(includeMarkdown("documents/SR_Model_JAGS/JAGS_diagnoses.Rmd"))    
            ),
    tabPanel("SR Equations",
      tabsetPanel(  
        tabPanel("SR model",
        withMathJax(includeMarkdown("documents/SR_Model_EQ/SR_help.Rmd"))
             ),
        tabPanel("Model Prediction",
        withMathJax(includeMarkdown("documents/SR_Model_EQ/SR_help_Pred.Rmd"))
             ),
        tabPanel("Reference Points",
        withMathJax(includeMarkdown("documents/SR_Model_EQ/SR_help_2.Rmd"))
             ),
      tabPanel("State-Space Model Equations",
        withMathJax(includeMarkdown("documents/SR_Model_EQ/State-Space.Rmd"))
             ),
            )
          ),
    "Escapement Goal Analyses",
      tabPanel("Smsy-Smax Profile Analyses",
              withMathJax(includeMarkdown("documents/Esc_Goal/Profile_help.Rmd"))
                 ),
      tabPanel("Yield-Recruit Analyses",
          withMathJax(includeMarkdown("documents/Esc_Goal/Yield_Recruit_help.Rmd"))   
                 ),
      tabPanel("Custom Escapement Goal Analyses",
          withMathJax(includeMarkdown("documents/Esc_Goal/Custom_Escapement_help.Rmd"))
                 ),
    "MSE Analyses",
        tabPanel("Model Description",
          includeMarkdown("documents/MSE_Analyses/MSE_help.Rmd")    
                 ),
        tabPanel("Model Analyses",
#          includeMarkdown("documents/MSE_Analyses/MSE_help.Rmd")    
                 ),
     )
#    )     
  )# End tabPanel Help
 ),#End nabVarPage: Beginning of the ui 

#'------------------------------------------------------------------------------
## Footnote Citation Disclaimer ----  
#'------------------------------------------------------------------------------
hr(),
h5("Disclaimer"),
strong('This App is developed by Toshihide Hamachan Hamazaki, Alaska Department of Fish and Game Division of Commercial Fisheries'),

h5("Contact about this applicaiton"), 
strong("Questions and improvement suggestions? Please contact",
a(href="mailto:toshihide.hamazaki@alaska.gov", "Hamachan")),

h5("Suggested Citation"),
strong(paste("Hamazaki, T.",format(Sys.Date(), "%Y"),". Pacific salmon escapement goal analyses (source: https://hamachan.shinyapps.io/Spawner_Recruit_Bayes/)")),
h5("Other Models"),
strong("Missed escapement passage estimation:",
         a(href="https://hamachan.shinyapps.io/Missed_Run/", "Missed Passage"))

# End of Foot note  
) #End fluidPage:  End of UI 
