#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Modules 
#  Shiny Modules consists of nameUI and nameServer functions 
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
source("Rcode/Modules/Bayes_JAGS_module.R")
source("Rcode/Modules/data_input_module.R")
source("Rcode/Modules/Risk_Analysis_module.R")
source("Rcode/Modules/Percentile_Analysis_module.R")
source("Rcode/Modules/Profile_module.R")
source("Rcode/Modules/Info_module.R")

#'===============================================================================
#  4.0 MSE Analyses Module 
#'===============================================================================
#'===============================================================================
#  MSEUI Module: Produce Risk analyses and plot results  
#  Usage: 
#  UI section 
#  MSEUI("ns.name")
#  Server section
#  MSEUIServer("ns.name",e.data,u)
#'===============================================================================
MSEUI <- function(id){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    p(strong("Modeling Parameters")),  
    p(strong("Lower and Upper Escapement Goal Range")),  
    numericInput("LEG", "Lower Goal", value=0,min=0, step=1000),  
    numericInput("UEG", "Upper Goal", value=0,min=0, step=1000),
    selectInput(inputId="cmode","Fishery Opening above Escapement Goal", choices = c('Lower','Middle','Upper')),              
    p(strong("Fishery limits")),  
    numericInput(inputId="maxH", "Maximum Harvest", value=0,min=0,step=10000),
    sliderInput(inputId="maxHr", "Maximum Surplus Harvest Rate", value=0.5,min=0,max=1,step=0.1),
    p(strong("Management Error")),       
    sliderInput(inputId="spred", "Run Assessment Error", value=20,min=0,max=100,step=5),
    sliderInput(inputId="simpH", "Management Error", value=10,min=0,max=100,step=5),
#    sliderInput(inputId="sobsH", "Harvest Observation %E", value=10,min=0,max=100,step=5),
#    sliderInput(inputId="sobsE", "Escapement Observation %E", value=30,min=0,max=100,step=5),
    actionButton("InitRun","Initialize"),
    actionButton("SimRun","Simulate"),
    actionButton("SimClear","Clear Results")
    ) # End tagList 
  }

# Output Module ----------------------------------------------------------------
MSEServer <- function(id,p.brood,fage,SR.post,model,d){
  moduleServer(
    id,
    function(input, output, session){
#-----------------------------------------------------------------------
#  Initialize 
#-----------------------------------------------------------------------
  MSE.int <- eventReactive(input$InitRun,{
#-----------------------------------------------------------------------  
# import brood table
  # Calculate mean age recruit
        # first age
        fage <- fage
        # number of age groups  
        nages <- dim(x)[2]
        # last age 
        lage <- fage + nages-1
        years <- input$simy
        burnin <-input$burnin
        # Total Simulation Years 
        nyrs <- burnin+train+years
        ar1 <- function(n,alpha,sigma, e0){
          ar1 <- numeric(n)
          ar1[1] <- rnorm(1,sigma)+alpha*e0
          for(i in 2:n){
            ar1[i] <- alpha*ar1[i-1]+rnorm(1,sigma)
          }
          ar1
        } 
#          rw <- function(n,sigma){
#          e <- numeric(n)
#          e[1] <- rnorm(1,sigma)
#          for(i in 2:n){
#          e[i] <- e[i-1]+rnorm(1,sigma)
#          }
#          e
#        } 
        if(input$add=='ar1'){
          e.Rec <- ar1(nyrs,SR.out()$sigma,phi)  
        } else {
          e.Rec <- ar1(nyrs,SR.out()$sigma,0)  
        }
        if(input$add =='kf')  ai <-  ar1(nyrs,SR.out()$sigmaw,0)

        # output data  
        e.pred <- exp(rnorm(nyrs,0,input$spred/100))
        e.imp <- exp(rnorm(nyrs,0,input$simpH/100))
        out <- list(nages=nages,e.pred = e.pred, e.imp = e.imp, e.Rec = e.Rec, e.p = e.p)
        return(out) 
      })
      
#-------------------------------------------------------------------------------
#  MSE Simulation Rutine 
#-------------------------------------------------------------------------------
sim <- eventReactive(input$SimRun,{
#-----------------------------------------------------------------------
#  Import Error Data 
#-----------------------------------------------------------------------  
    Init <- MSE.int()
    nages <- MSE.int()$nages
    e.imp <- as.vector(MSE.int()$e.imp)
    e.Rec <- as.vector(MSE.int()$e.Rec)
    e.p <- as.matrix(MSE.int()$e.p)
    srmodel <- model
    
# Initial Run size   
    R0 <- median(sr.data()$R)
# first age
    fage <- input$fage
# last age 
    lage <- fage + nages-1
# Simulation     
    years <- input$simy
    burnin <- input$burnin
# Total Simulation Years 
    nyrs <- burnin+years
#-------------------------------------------------------------------------------
#  Create Empty vector  
#-------------------------------------------------------------------------------  
# Recruit
  R <- numeric(nyrs)
#  R.obs <- numeric(nyrs)
# Annual Run
  N <- numeric(nyrs)
# Annual Escapement
  S <- numeric(nyrs)
#  S.obs <- numeric(nyrs)
# Annual Harvest 
  H <- numeric(nyrs)
#  H.obs <- numeric(nyrs)
# Annual Run by age 
  N.ta <- matrix(0,ncol=nages, nrow=nyrs+lage+2)
#  N.ta.obs <- matrix(0,ncol=nages, nrow=nyrs+lage+2)
#---------------------------------------------------------------------------
#   Start simulation 
#---------------------------------------------------------------------------
  for (y in 1:nyrs){
# First generation is constant   
    if(y<=lage) {
        N.ta[y,] <- R0*exp(e.Rec[y])*e.p[y,]
          }  
# Anunual Run is sum of all ages
    N[y] <- sum(N.ta[y,])
# Predicted Run
    N.pred <- N[y]*e.pred[y]
# Determine target harvest criteria	
    EG.l <- ifelse(
      input$cmode =='Middle',mean(input$LEG,input$UEG), # Fishery target: mid EG range 
      ifelse(input$cmode =='Upper',input$UEG, # Fishery target: Upper EG range 
             input$LEG # Fishery target lower EG range
             ))
# Management based on Escapement goal 
    H.target <- ifelse(
      N.pred < EG.l,0,  # Fishery closed if it does not meet target   
      min(input$maxH,(N.pred-EG.l)*input$maxHr))
# Actual Harvest
  H[y] <- min(H.target*e.imp[y],0.99*N[y])
# Actual Escapement 
  S[y] <- N[y] - H[y]
# Calculate Future Recruits based on SR 
  R[y] <- alpha*S[y]*exp(-beta*S[y]+e.Rec[y])
# Fill Future Return by age
    for (a in 1:nages){ N.ta[y+fage+a-1,a] <- R[y]*e.p[y,a] }
# Observed Escapement 
#  S.obs[y] <- S[y]*e.obsS[y]
#Observed Harvest
#  H.obs[y] <- H[y]*e.obsH[y]
#Age comp
#  p.age <- N.ta[y,]/N[y]
#Observed age comp 
#  p.age.ob <-rmultinom(1,input$Nobage,p.age)/input$Nobage
#Observed Run by age (Assume age comp est is accurate)
#  N.ta.obs[y,] <- sum(S.obs[y],H.obs[y])*p.age.ob
        } # End simulation
        })
outdata <- list(N =N, S = S, H=H)
return(outdata)  
    } # End function 
  ) # End moduleServer
} # End MSEServer


