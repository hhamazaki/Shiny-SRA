#'==============================================================================
#'  Shiny Escapement Goal Analyses App
#'  File name: server.R
#'  Author:  Toshihide "Hamachan" Hamazaki 
#'  Date: 
#'  Description
#'  This program is conducts Escapement Goal Analyses using Bayesian SR models
#'   
#'  Naming convention 
#'  Plt_xxx  (Plot Output)
#'  plt_xxx  (plot object for reporting)
#'  Tbl_xxx  (Table output)
#'  tbl_xxs  (Table object for reporting)
#'  Txt_xxx  (Text Output)
#'==============================================================================
#'============================================================================== 
#'#  Server ----  
#'==============================================================================
server<-shinyServer(function(input, output, session){
#'------------------------------------------------------------------------------
#palette("Okabe-Ito")  
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
options(DT.options=list(pageLength=25,scrollX=TRUE, scrollY="400px", scrollcollapse = TRUE,
       columnDefs=list(list(width='50px',targets="_all")
  )))
options(shiny.maxRequestSize = 100 * 1024^2)
#'------------------------------------------------------------------------------
#'  Include Source codes
#'------------------------------------------------------------------------------
source("Rcode/Functions/Data_Assemby.R")  # Include functions related to data assembly
source("Rcode/Functions/Shiny_SR_functions.R")  # Include functions related to data assembly
source("Rcode/Functions/MSE_functions.R")  # Include functions related to data assembly


#'------------------------------------------------------------------------------
#'  plt: Determine output figure is ggplot or base plot 
#'------------------------------------------------------------------------------

plt <-'gg'

if(plt=='base'){
source("Rcode/plots/baseplot/base_plot_functions.R")  
source("Rcode/plots/baseplot/plots_base.R", local = TRUE)
}
if(plt=='gg'){
source("Rcode/plots/ggplot/ggplot_functions.R")
source("Rcode/plots/ggplot/plots_gg.R", local = TRUE)
}

#'-------------------------------------------------------------------------------
# Tab Control ---- 
#'-------------------------------------------------------------------------------
observe({
  if(input$dataType=="Run") {
    hideTab(inputId = "tabs", target = "Escapement Only Analyses")
    showTab(inputId = "tabs", target = "Escapement Goal Analyses")
    showTab(inputId = "tabs", target = "MSE Analyses")
    showTab(inputId = "tabs", target = "SR Model")
    showTab(inputId = "Panel", target =  "Kobe Plot")
    showTab(inputId = "subTab", target = "Run Table")
    showTab(inputId = "subTab", target = "Brood Table")
    hideTab(inputId = "ssTab", target = "Run Size")
    hideTab(inputId = "ssTab", target = "Run Age Comp")
    hideTab(inputId = "ssTab", target = "Brood Age Comp") 
    hideTab(inputId = "Panel", target = "Priors") 
    if(input$Priors){
    showTab(inputId = "Panel", target = "Priors")  
    }
  } else if (input$dataType== "S-R") {
    hideTab(inputId = "tabs", target = "Escapement Only Analyses")
    showTab(inputId = "tabs", target = "Escapement Goal Analyses")
    hideTab(inputId = "tabs", target = "MSE Analyses")
    hideTab(inputId = "Panel", target =  "Kobe Plot")
    showTab(inputId = "tabs", target = "SR Model")
    hideTab(inputId = "subTab", target = "Run Table")
    hideTab(inputId = "subTab", target = "Brood Table")
    hideTab(inputId = "ssTab", target = "Run Size")
    hideTab(inputId = "ssTab", target = "Run Age Comp")
    hideTab(inputId = "ssTab", target = "Brood Age Comp")
    hideTab(inputId = "Panel", target = "Priors") 
    if(input$Priors){
    showTab(inputId = "Panel", target = "Priors")  
    }
      } else {
    showTab(inputId = "tabs", target = "Escapement Only Analyses")
    hideTab(inputId = "tabs", target = "Escapement Goal Analyses")
    hideTab(inputId = "tabs", target = "MSE Analyses")
    hideTab(inputId = "tabs", target = "SR Model")
    hideTab(inputId = "subTab", target = "Run Table")
    hideTab(inputId = "subTab", target = "Brood Table")
    hideTab(inputId = "ssTab", target = "Run Size")
    hideTab(inputId = "ssTab", target = "Run Age Comp")
    hideTab(inputId = "ssTab", target = "Brood Age Comp") 
  }
 })  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Panel 1  Data Input and Submit ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'==============================================================================
# 1. Data input ----
#' final input data object is data()
#'==============================================================================
#' User choses dataType first (input$DataType)
##  Input data file reading module ---- 
data.get <-  dataInputServer("datain")
data.in <- reactive(data.get$df())
output$file.n <- renderText(paste("Uploaded File:",data.get$fn()))

##  check input file has errors ------
observe({
      if(datacheck(data.in())>0){
      showModal(modalDialog(
        title = Info_data_input_Error_title,
        Info_data_input_Error,
        easyClose = TRUE,
        footer = NULL
      ))}else if(input$dataType== "Run"&is.null(age.out(data.in()))){
      showModal(modalDialog(
        title = Info_data_Age_Error_title,
        Info_data_Age_Error,
        easyClose = TRUE,
        footer = NULL
      ))}
    })


## data() input data ---- 
data <- reactive({
#' In put sample data ----------------------------------------------------------
  if(input$Sample){
#'Run data 
  if(input$dataType== "Run"){
    out <- read.csv('Sample_data/Sample_Run_data.csv',header=T)} 
#' SR data 
  else if(input$dataType== "S-R"){
    out <- read.csv('Sample_data/Sample_SR_data.csv',header=T)} 
#' Escapement data 
  else if(input$dataType== "Escapement Only"){
    out <- read.csv('Sample_data/Sample_Esc_data.csv',header=T)} 
      } # End if Sample 
#' Input data file check -----------------------------------------------------
  else {
    # Check if input file are numbers
    validate(need(datacheck(data.in())==0,"Please check input data format."))
    if(input$dataType =="Run"){
    # Check if age column names are correct   
    validate(need(!is.null(age.out(data.in())),"Please check Age data column names."))      
    }
    out <- data.in()
   }
  return(out)
})

#' Create Unit assembly object unit(): uint can be by 1000, million-------------
unit <- reactive({
    if(input$autoui==TRUE){
      d <-  floor(log10(max(data()[,2],na.rm=TRUE)))  #data()[,2] is Escapement/spawner
      u <- ifelse(d>=6,10^6,ifelse(d>3,10^3,1))
      } else {
      u <- ifelse(input$ui=='million',1000000,as.numeric(input$ui))
      }
    return(u)
     })

## Tbl_data: input data Output ----------------------------------------
 output$Tbl_data <- DT::renderDT(data(),rownames = FALSE)
 

#'==============================================================================
# 2. Data Modification -----
#'   Create Brood Table 
#'==============================================================================
o.age <- reactive({if(input$dataType== "Run"){
  age.out(data())}
})
#'------------------------------------------------------------------------------
#'  Create Run Data Modifying UI 
#'------------------------------------------------------------------------------
##### UI agerange: Select Age range  -------------------------------------------
output$agerange <- renderUI({
    if(input$dataType== "Run"){
     age <-  age.out(data())
     fage <- min(age)       # First age
     lage <- max(age)       # Last age
    #  Slider input UI 
    sliderInput("rage", label = paste("Select","Run Age Range"), 
        min = fage, max = lage, value = c(fage, lage),step=1,sep = "")
     }
    })

##### UI agecomb  Set Age combining method (pooling vs. omitting) --------------
output$agecomb <- renderUI({
    if(input$dataType== "Run" & (!is.null(input$rage[1]))){
      #  Slider input UI 
    checkboxInput("combage", label = InfoUI('info2','Pool Ages')
                    , value = TRUE)
        } 
  })

#'------------------------------------------------------------------------------
#'  Data modifying assembly:  In Data_Assembly.R
#'------------------------------------------------------------------------------
#' Create Escapement, Run, Run by age proportion table 
tbl_run <-reactive({
     if(input$dataType== "Run"){
      agedata <- make.age(data(),input$rage[1], input$rage[2],input$combage)
      data <- cbind(data()[,1:3],agedata)
      names(data)[1:3] <- c('Year','S','N')
      return(data)
     }
    })


#' Create CV data table when CV columns  exist  
tbl_run.cv <-  reactive({
# Check if all cv columns exist 
  if(input$dataType== "Run"){
    name <- names(data())
    if(length(name[name %in% c('cv_N','cv_E','cv_H','efn')])==4){
    run_cv(data())}
     }
    })   
# Create SR data with ci
sr.data.ci <- reactive({
    if(!is.null(tbl_run.cv())){
      make_sr_var(tbl_run(),tbl_run.cv(),sr.data())
    }
  })

# create brood table
tbl_brood <-  reactive({
     if(input$dataType== "Run"){
      make.brood(tbl_run(),TRUE)
      }
    })

#' Create brood by age proportion table ------------------------------
brood.p <- reactive({
    if(input$dataType== "Run"){
     brood <- tbl_brood()$brood[,-2] # Remove Spawner data
     brood <- brood[complete.cases(brood),]
     ncol <- dim(brood)[2]
     brood[,2:ncol] <- brood[,2:ncol]/brood$Recruit
     return(brood[,-ncol])
    }
   })

##### Tbl_data.run ----- show run table ----------------------------------------
output$Tbl_data.run <- DT::renderDT(round(tbl_run(),2),rownames = FALSE) 


##### Tbl_data.brood ----- show brood table ------------------------------------
output$Tbl_data.brood <- DT::renderDT(round(tbl_brood()$brood,0),rownames = FALSE) 
#'==============================================================================
# 3. Create SR or data ---- 
# Select analysis year range 
# This create final data sr.data or e.data (Escapement only data)
#'==============================================================================
#' sr.data.0  Original SR data -------------------------------------------------  
sr.data.1 <- reactive({
  # SR data created from Run data     
  if(input$dataType== "Run"){
    x <- tbl_brood()$SR
    h <- brood.H(tbl_run())
    x <- merge(x,h,by=c('b.Year','Spawner'))
    names(x) <- c('Yr','S','R','H')
    return(x)
  }
    })


#' sr.data.0  Original SR data -------------------------------------------------  
sr.data.0 <- reactive({
# SR data created from Run data     
    if(input$dataType== "Run"){
      x <- tbl_brood()$SR
# Add cv_E when it exists (used for measurement error model)      
    if(!is.null(tbl_run.cv())){
      y <- tbl_run.cv()
      x <- merge(x,y[,c('Year','cv_E')],by.x='b.Year',by.y = 'Year',all.x = TRUE)
      }
     } else if (input$dataType== "S-R"){
# SR data directly imported       
      x <- data()
      x <- x[complete.cases(x),]
     }
# Rename   
    names(x)[1:3] <- c('Yr','S','R')
# Add Yield    
    x$Y <- with(x,R-S)
# Add ln(R/S)    
    x$lnRS <- with(x,log(R/S))
    return(x)   
  })

### e.data.0 Original Escapement Only data   -------------------------------  
 e.data.0 <- reactive({
   if(input$dataType== "Escapement Only"){
      x <- data()[,c(1:2)]
    names(x) <- c('Yr','S')
    return(x) }     
  })  


#### UI yrange UI output to determine data year range --------------------------
output$yrange <- renderUI({
  if(input$dataType== "Escapement Only"){
    name <- 'Calendar'
    year <- e.data.0()$Yr    # Extract Calendar year data range 
      }else{
    name <- 'Brood'
    year <- sr.data.0()$Yr   # Extract brood year data range     
         }
    fyear <- min(year)       # First year 
    lyear <- max(year)       # Last year
    #  Slider input UI 
    sliderInput("sryears", label = paste("Select",name,"Year Range"), min = fyear, max = lyear, value = c(fyear, lyear),step=1,sep = "")
  })

### e.data --- final dataset used for percentile risk -----------------
  e.data <- reactive({ cut.data(e.data.0(),input$sryears) })
  
### sr.data --- final dataset used for SR analyses --------------------
  sr.data <- reactive({cut.data(sr.data.0(),input$sryears) })

### Reactive ss.data --- final dataset used for percentile risk ----------------
ss.year <- reactive({ 
  min <- input$sryears[1]
  max <- input$sryears[2]+input$rage[2]
  ssyear <- c(min,max)
  return(ssyear)
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Plot SR data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") ----------

  output$Plt_runesc <- renderPlot({plt_runesc()})

## Plt_srt Plot SR time series ---------------------------------------

  output$Plt_srt <- renderPlot({plt_srt()})
  output$Plt_srt2 <- renderPlot({plt_srt2()})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Input data Summary Statistics 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
tbl_sum_sr.data <- reactive({
  if(input$dataType == "Escapement Only"){
  dat <- e.data()  
 x <- ((sum.ci(dat,95)))
   names(x) <- c('Min',paste0(2.5,'%'),'Mean','Median',paste0(97.5,'%'),'Max','SD','CV')
#      x[2,]  <- as.integer(x[2,])
      x <- x[2,]} else {   
  dat <- sr.data()
  dat$Y <- dat$R-dat$S
  x <- data.frame(t(t(sum.fun(dat[,c('S','R','Y','lnRS')],95))))  
#for(i in 1:3){x[,i] <- as.integer(x[,i])}
  x[,4] <- round(x[,4],3)
  names(x) <- c('Spawner','Recruit','Yield','ln(R/S)')
  }
  return(x)
})

output$Tbl_sum_sr.data <- renderTable({tbl_sum_sr.data()},rownames=TRUE)

#### Plt_hist.sry:  sr data histogram ------------------------------------------- 

output$Plt_hist.sry <- renderPlot({plt_hist.sry()})

##### Txt_sum_run.data: Run data summary Output ----------------------------
tbl_sum_run.data <- reactive({
  if(input$dataType == "Run"){
  df <- data()[,c(1:3)]
  names(df) <-c('Yr','S','R')
  df$H <- with(df,(R-S))
  df$HR <- with(df,H/R)
  x <- data.frame(t(t(sum.fun(df[,c('R','S','H','HR')],95))))
#for(i in 1:3){x[,i] <- as.integer(x[,i])}  
  x[,4] <- round(x[,4],3)
  names(x) <- c('Run','Escapement','Harvest','Harvest Rate')
  return(x)
  }
})

output$Tbl_sum_run.data <- renderTable({tbl_sum_run.data()},rownames=TRUE)

#### Plt_hist.run: run data histogram ------------------------------------------ 
output$Plt_hist.run <- renderPlot({plt_hist.run()})


#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Panel 2: Bayesian Model:  Create JAG data and model-----   
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'==============================================================================
# 1. Model Selection -----   
#'==============================================================================
###  UI output re: Measurement Error model option Show only when data are SR and CV_E exists -------------------  
output$re <- renderUI({  
  if(input$dataType== "S-R" & sum(names(sr.data()) %in% 'cv_E')==1){
  checkboxInput(inputId="RE", "Measurement Error Model", FALSE)
  } 
 })

##  UI output ss: State-Space model otion Only show when data are Run ----------  
output$ss <- renderUI({  
if(input$dataType=='Run' & (!is.null(tbl_run.cv()))){
  radioButtons(inputId="SS","Model Options",
               choices=c("Standard","Measurement Error Model","State-Space Model"),
               selected = "Standard")
    } 
  })

# RE: Measurement Error Model ---------------------  
RE <- reactive({
      if(input$dataType=='Run'& (!is.null(tbl_run.cv()))){
      out <- ifelse(input$SS=="Measurement Error Model",TRUE,FALSE)
      } else if(input$dataType=='S-R'& sum(names(sr.data()) %in% 'cv_E')==1){
        out <- input$RE   
      } else {
      out <- FALSE  
      }
      return(out)
      })
# SS: State-Space Model ---------------------  
SS <- reactive({
  if(input$dataType=='Run'& (!is.null(tbl_run.cv()))){
  out <- ifelse(input$SS=="State-Space Model",TRUE,FALSE)
  } else {out <- FALSE}
  return(out)
  })
observe({
if(isTRUE(SS())){
  showTab(inputId = "ssTab", target = "Run Size")
  showTab(inputId = "ssTab", target = "Run Age Comp")
  showTab(inputId = "ssTab", target = "Brood Age Comp")   
}
})

# Model Name 
model.name <- reactive({
  st <- ifelse(input$add=="kf",paste0(input$alphai),'')
  add <- ifelse(input$add=="ar1","AR1",ifelse(input$add=="kf","TVA","ST"))
  re <- ifelse(RE(),'RE','')
  ss <- ifelse(SS(),'SS','')
  yrs <- paste0(input$sryears[1],'-',input$sryears[2])
  model <- paste(input$Model,ss,re,add,st,yrs,sep='_')
  return(model)
 })

# Model.tile used for display 
model.title <- reactive({
  st <- ifelse(input$add=="kf",paste('TVA selected periods',input$alphai),'')
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  txt <- HTML(paste(paste('Model:',input$Model,add),
                  paste('Brood Year:',input$sryears[1],'-',input$sryears[2]),
                  paste(ifelse(SS(),'State-Space Model','')),
                  paste(ifelse(RE(),'Measurement Error Model','')),
                  paste(st),
                  sep = '<br/>'))
    return(txt)
 })


sr.title <- reactive({
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  yrs <- paste('Brood Year:',min(sr.data()$Yr),'-',max(sr.data()$Yr))
  model <- paste0('SR Model: ',input$Model,' ',add)
  title <- paste(model,yrs)
  return(title)
})

output$model.1 <- renderText(model.title())

#'==============================================================================
## 2.0: Bayesian Model Data Assembly -----   
#'==============================================================================
## PR --- Create data set for Priors ----------------------------------------
observeEvent(input$Priors,{
        if(isFALSE(input$Priors)){
            updateSliderInput(session,'lnalpha',value = c(-1,4))
            updateSliderInput(session,'beta',value = c(-1,5))
          }
        })

hyper <- reactive({
  hyper <- list(
  min.a = input$lnalpha[1],
  max.a = input$lnalpha[2],
  min.b = input$beta[1],
  max.b = input$beta[2]
  ) 
  return(hyper)
})


## Bayesedata --- Create data set for Bayesian modeling -------------------------
Bayesdata <- reactive({
model <- list(
  rk = ifelse(input$Model=='Ricker',1,0),
  bh = ifelse(input$Model=='Beverton-Holt',1,0),
  ar1 = ifelse(input$add =='ar1',1,0),
  kf = ifelse(input$add =='kf',1,0)
 )
  # Model setting 

# Create data-------------------------------------------------------------------
# Condition: 1  State-Space model 
if(SS()){
# Extract data     
  dat <- cbind(tbl_run(),tbl_run.cv())
  #' Create data range for State-Space Modeling   
# Create trimmed year   
  trimyear<- seq(input$sryears[1],input$sryears[2]+input$rage[2])
# Extract Year matches trimyear  
  dat <- dat[dat$Year %in% trimyear,]
# d is S multiplier
  d <- floor(log10(mean(dat$S,na.rm=TRUE)))
# Create age proportion  
  page <- as.matrix(dat[,substr(names(dat),1,1) =='A'])
  page[is.nan(page)] <- 0
  page[is.na(page)] <- 0
# multiply with effective sample size and round, so that all numbers are integer
  multage<-round(page*dat$efn,0) 
  out<-list(nyrs = dim(dat)[1], 
          nages=(input$rage[2]-input$rage[1]+1), 
          fage=input$rage[1], lage=input$rage[2],
          p_age=multage, efn=rowSums(multage),
          H.obs=dat$N-dat$S, N.obs=dat$N, S.obs= dat$S,
          tau.log.N=1/(log(dat$cv_N^2+1)),
          tau.log.H=1/(log(dat$cv_H^2+1)),
          tau.log.S=1/(log(dat$cv_E^2+1)),
          d=d)
   } else {
# Analysis is not State-Space model   
  #  Import SR data 
  x <- sr.data()
  # nyrs is the number of brood years (i.e. number of rows) 
  nyrs <- dim(x)[1]
  R <- x$R
  S <- x$S
  # d is S multiplier
  d <- floor(log10(mean(S,na.rm=TRUE)))
  #  ar1: 1 if ar1 is included, 0 if not 
 out <-list(nyrs=nyrs,S=S,R=R,d=d)
# Analysis is Measurement Error Model   
 if(RE()){
    out$lnSm = mean(log(S))
    out$tau.lnSm = 1/var(log(S))
    out$tau.log.S=1/(log(x$cv_E^2+1))
   } 
   }
  out <- c(out,model,hyper())
  return(out)  
  })
 
# Select Bayes model 
Bayesmodel <- reactive({model_select(input$Model,input$add,SS(),RE())})
# Show model code
output$modelcode <- renderPrint({ Bayesmodel()})
output$modeldata <- renderPrint({ Bayesdata()})

#'==============================================================================
##  Run JAG Model Module ----
#'==============================================================================
#  Bayaesmodel  Model section for JAG Models 
#  The function outputs: 
#  jagmodel: Selected JAG model
#  parameters: Model output parameters
#  model:  SR model used for simulation 
#'==============================================================================
# Run Bayesian Model
sim <- BayesInputServer('Bayes', Bayesdata, Bayesmodel)
# Model output 
JAGS.In <-  reactive(({sim()$input}))
MCMC <- reactive({as.matrix(as.mcmc(sim()$output))})
# Summary output
JAGS.sum <- reactive({
             data <- sim()$output$BUGSoutput$summary
             parameters <- row.names(data)
             data <- data.frame(cbind(parameters,data))
             return(data)
             })

output$download.mc <- downloadHandler(
  filename = function(){
    paste0('MCMCdata_', model.name(),'_', Sys.Date(),'.csv')  
        },
    content = function(file) {
      write.csv(data.frame(MCMC()), file,row.names = FALSE,na='')  
          }
  )

output$download.JAG <- downloadHandler(
  filename = function(){
    paste0('MCMCdata_', model.name(),'_', Sys.Date(),'.txt')  
        },
    content = function(file) {
       dput(sim(), file)  
          }
  )

# MCMC data file reading module  
mcmcdata <- dataInputServer("mcmc.in")

#'------------------------------------------------------------------------------
## Extract JAG results ----
#'------------------------------------------------------------------------------
### BayesSum ------------ Output MCMC summary ----------------------------------
output$BayesSum <- renderPrint({print(sim()$output) })

### Plt_trace --------- Trace and density plot ---------------------------------
output$Plt_trace <- renderPlot({
  if(isTRUE(input$BayesMCMC)){
   mcmc <- mcmcdata()
   } else {
   mcmc <- as.mcmc(sim()$output)
   }
  pars <- c('lnalpha','beta')
  if(input$add=='ar1') {pars <- c(pars,'phi')}
  par(mfrow=c(2,4))
  plot(mcmc[,pars],auto.layout=FALSE)
  })

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  EXTRACT RE data ----
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### RE.post Posterior for Measurement Error model ----
RE.post <- reactive({
  if(RE()){
    nyrs <- Bayesdata()$nyrs 
    if(isTRUE(input$BayesMCMC)){
    mcmc <- mcmcdata()
    } else {
    mcmc <- MCMC()
    }
# Extract data 
    lnS <- mcmc[,substr(names(mcmc),1,5)=='log.S']
# sort by year     
    lnS <- lnS[,paste0('log.S.mu[',1:nyrs,']')]
    lnSm <- sum.ci(lnS,90)
    return(lnSm)    
  }
})

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  EXTRACT SS data ----
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### SS.post Posterior for SS ----
SS.post <- reactive({
  if(SS()){
    nyrs <- Bayesdata()$nyrs 
    nages <- Bayesdata()$nages 
    fage <- Bayesdata()$fage 
    if(isTRUE(input$BayesMCMC)){
    mcmc <- mcmcdata()
    } else {
    mcmc <- as.data.frame(MCMC())
    }
# Extract data 
    N <- mcmc[,substr(names(mcmc),1,2)=='N[']
    S <- mcmc[,substr(names(mcmc),1,2)=='S[']
    R <- mcmc[,substr(names(mcmc),1,2)=='R[']
    H <- mcmc[,substr(names(mcmc),1,2)=='H[']
#    rwe <- mcmc[,substr(names(mcmc),1,4)=='rwe[']
#    rwk <- mcmc[,substr(names(mcmc),1,4)=='rwk[']
     p <- mcmc[,substr(names(mcmc),1,2)=='p[']
     q <- mcmc[,substr(names(mcmc),1,2)=='q[']
    p.age <-list()
    for(i in 1:nages){
      d <- nchar(i)+1   # find the number of digits 
      temp <- p[,substrRight(names(p),d)==paste0(i,']')]
      p.age[[i]] <- temp[,paste0('p[',1:nyrs,',',i,']')]
    }
#  Brood  age comp     
      q.age <-list()
    for(i in 1:nages){
      d <- nchar(i)+1   # find the number of digits 
      temp <- q[,substrRight(names(q),d)==paste0(i,']')]
      q.age[[i]] <- temp[,paste0('q[',1:(nyrs+nages-1),',',i,']')]
    }  
# sort by year     
    N <- N[,paste0('N[',1:nyrs,']')]
    S <- S[,paste0('S[',1:nyrs,']')]
    H <- H[,paste0('H[',1:nyrs,']')]
    R <- R[,paste0('R[',1:(nyrs+nages-1),']')]
    
#    rwe <- rwe[,paste0('rwe[',1:(nyrs+nages-1),']')]
#    rwk <- rwk[,paste0('rwk[',1:(nyrs+nages-1),']')]
# output  
#    out <- list(N=N,S=S,R=R,rwe=rwe,rwk=rwk,p.age = p.age)
    out <- list(N=N,S=S,H=H,R=R,p.age = p.age,q.age=q.age)
  }
})

#### SS.post.sum ----
SS.post.sum <- reactive({
  if(SS()){  
    nyrs <- Bayesdata()$nyrs 
    nages <- Bayesdata()$nages 
    fage <- Bayesdata()$fage
    lage <- Bayesdata()$lage
    dat <- SS.post()
    trimyear <- seq(ss.year()[1],ss.year()[2])
    Ryear <- seq(ss.year()[1]-lage,ss.year()[2]-fage)
# model predicted S     
    S <- sum.ci(dat$S,95)
    S$Year <- trimyear
# model predicted N
    N <- sum.ci(dat$N,95)
    N$Year <- trimyear
# model predicted H    
    H <- sum.ci(dat$N-dat$S,95)
    H$Year <- trimyear
# model predicted R        
    R <- sum.ci(dat$R,95)
    R$Year <- Ryear
# model predicted run age comp    
    p <-list()
    for(i in 1:nages){
        p[[i]] <- sum.ci(dat$p.age[[i]],95)
        p[[i]]$Year <- trimyear
        p[[i]]$Age <- paste('Age',fage+i-1)
    }   
# model predicted brood age comp    
    q <-list()
    for(i in 1:nages){
        q[[i]] <- sum.ci(dat$q.age[[i]],95)
        q[[i]]$Year <- Ryear
        q[[i]]$Age <- paste('Age',fage+i-1)
              } 
    out <- list(N=N,S=S,H=H,R=R,p.age = p,q.age=q) 
  }
 })


#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3: SR Model Analyses ----  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'------------------------------------------------------------------------------
#' This section calculates posteriors of Biological reference parameters
#' Seq, Seq.c Smsy, Smsy.c Umsy, Umsy.c, Smax, Sgen, Sgen.c
#' The posteriors are saved in the object SR.post and SR.post.i (for TVA)
#'------------------------------------------------------------------------------
### SR.post: Create SR parameters: alpha, beta, Seq, Smsy, Umsy, Smax, Sgen-------
 SR.post <- reactive({
  D <- as.numeric(Bayesdata()$d)
# Read mcmc data
  if(isTRUE(input$BayesMCMC)){
   mcmc <- mcmcdata()
   } else {
   mcmc <- as.matrix(MCMC())
   }
   post <- sim.out(mcmc,d=D,add=input$add,model=input$Model,model.br=Bayesmodel()$model.br)
   post <-  Id.outliers(post,input$target) 
   outlier <- outlier_sum(post)
   
if(isTRUE(input$Remove_out)){post <- post[which(is.na(post$Remove)),]}
  out <- list(post=post,outlier=outlier)
      return(out)
  })

#'===============================================================================
##  Time Variant alpha Analyses ---- 
#'===============================================================================
### lnalphais  Extract Time_variant alpha----------------------------------------
lnalphais <-reactive({
 if(input$add=='kf'){
  nyrs <- Bayesdata()$nyrs
  if(isTRUE(SS())) {nyrs <- Bayesdata()$nyrs - Bayesdata()$lage}
   year <- sr.data()$Yr
#' Bayesian simulation out parameters -------------------------------------------    
 if(input$BayesMCMC==TRUE){
  parname <- paste0('lnalphai.',1:nyrs,'.')  # Reading CSV data 
   } else {
  parname <- paste0('lnalphai[',1:nyrs,']')  # Reading directly 
   }
# Extract lnalphai from posterior   
# Read mcmc data
  if(isTRUE(input$BayesMCMC)){
   mcmc <- mcmcdata()
   } else {
   mcmc <- MCMC()
   }
  lnalphai <- mcmc[,parname]
# Mean lnalphai  
  lnalphai.mm <- apply(lnalphai,2,mean)
# Get 95% CI  
  cil <- apply(lnalphai,2,function(x) quantile(x, 0.025))
  ciu <- apply(lnalphai,2,function(x) quantile(x, 0.975))
# Calculate STARS using STARS function 
   names(lnalphai.mm) <- year
   stars <- stars(lnalphai.mm, L=5, p=0.05,  h=2, AR1red="est", prewhitening = F)  
   test <- data.frame(stars$starsResult)
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


output$Plt_lnalphai <- renderPlot({plt_lnalphai()})

###  astar UI output to determine data year range  -----------------------------
output$astar <- renderUI({
 if(input$add=='kf'){
# Extract lnalhpha Star 
 cuty <- lnalphais()$cuty
 radioButtons(inputId="alphai","Time Variant alpha Select Periods",
               choices=c("None",cuty$txt),
               selected = "None")
  }
 })

### SR.post.i ---- Time variant alpha Model data out  ------------------------
SR.post.i <- reactive({
  req(input$alphai)
# Read mcmc data
  if(input$add=='kf' & input$alphai != 'None'){
    cuty <- lnalphais()$cuty
    period <- cuty[cuty$txt==input$alphai,c('cby','cy')] 
    if(input$BayesMCMC==TRUE){
      parname <- paste0('lnalphai.',period$cby:period$cy,'.')
    } else{
      parname <- paste0('lnalphai[',period$cby:period$cy,']')
    }
    # Extract lnalphai  
    lnalphai <- MCMC()[,parname]
    post <- SR.post()$post
    post$lnalpha <- apply(lnalphai,1, FUN=mean, na.rm=TRUE)
    post$lnalpha.sigma <- apply(lnalphai,1, FUN=sd, na.rm=TRUE) 
    post$lnalpha.c <- post$lnalpha+0.5*post$sigma^2
    post$alpha <- exp(post$lnalpha)
    post$alpha.c <- exp(post$lnalpha.c)
    D <- as.numeric(Bayesdata()$d)
    br <- with(post,Bayesmodel()$model.br(lnalpha,beta,D))
    br.c <- with(post,Bayesmodel()$model.br(lnalpha.c,beta,D))
    post$Seq <- br$Seq
    post$Smsy <- br$Smsy
   post$Umsy <- br$Umsy
   post$Sgen <- br$Sgen
   post$Seq.c <- br.c$Seq
   post$Smsy.c <- br.c$Smsy
   post$Umsy.c <- br.c$Umsy
   post$Sgen.c <- br.c$Sgen
   post$Smax <- br$Smax
   post <-  Id.outliers(post,input$target)
   outlier <- outlier_sum(post)
  if(isTRUE(input$Remove_out)){post <- post[which(is.na(post$Remove)),]}
  out <- list(post=post,outlier=outlier)
  return(out)
    }
 })
### Tbl_mcmcdata output mcmc data ----------------------------------------------
#output$Tbl_mcmcdata <- DT::renderDT({datatable(SS.post.sum()$S,rownames = FALSE)}) 
#output$Tbl_mcmcdata <- renderDataTable({SR.post()$post}) 

#'------------------------------------------------------------------------------
#' Posterior Summaries 
#' -----------------------------------------------------------------------------
# tbl_sumpost:  Creates SR Parameters Standard Summary statistics ----------------- 
tbl_sumpost<- reactive({
  ci <- input$CIB
  alpha <- c('alpha','lnalpha')
  br <- c('Seq','Smsy','Umsy','Sgen')
  if(input$target =='me'){
  parname <- c(paste0(alpha,'.c'),'beta',paste0(br,'.c'))
  } else {
  parname <-c(alpha,'beta',br)
  }
  if(input$add=='ar1'){parname <-c(parname[1:3],'phi',parname[4:7])}
  if(input$Model == 'Ricker'){parname <- c(parname,'Smax')}
  if(input$add =='kf') {
    req(input$alphai)
    if(input$alphai != 'None'){
    out <- t(t(sum.fun(SR.post.i()$post[,parname],ci)))  
    } else{
    out <- t(t(sum.fun(SR.post()$post[,parname],ci)))
    } 
    } else {
    out <- t(t(sum.fun(SR.post()$post[,parname],ci)))
    }   
   out <- data.frame(out)
  if(input$add=='ar1'){
#    out[-8,c(5,6,8)] <- sapply(out[-8,c(5,6,8)],as.integer)
  } else{
#    out[-8,c(4,5,7)] <- sapply(out[-8,c(4,5,7)],as.integer)
  }
 if(input$Model == 'Ricker'){
#    out[-8,length(parname)] <- as.integer(out[-8,length(parname)])
  }
 # Rename beta 
  names(out)[3] <- paste0('beta x10^(-',Bayesdata()$d,')')
  return(out)
 })

# print out sumpost 
output$Tbl_sumpost <- renderTable(tbl_sumpost(),rownames = TRUE,digits=3)
#-------------------------------------------------------------------------------
#  Histogram 
output$Plt_hist.mc <- renderPlot({plt_hist.mc()})
#'------------------------------------------------------------------------------
output$Txt_hist.mc <- renderText({
"Probability distribution of the model and biological reference parameters. The vertical line indicates mean (solid) and median (hash)."
  })


output$Tbl_Ref_sum <- renderTable({
  if(input$add=='kf'){
    if( input$alphai != 'None'){
  SR.post.i()$outlier
    } 
  else {SR.post()$outlier}  
   }
  else {SR.post()$outlier}
 },na="") 

#downloadServer('Ref',Ref_data(), paste0('Sumdata_', model.name(),'_', Sys.Date()))

#output$download.sum <- downloadHandler(
#  filename = function(){
#    paste0('Sumdata_', model.name(),'_', Sys.Date(),'.csv')  
#    },
#    content = function(file) {
#      write.csv(as.data.frame(Ref_data()), file,row.names = FALSE,na='')  
#    }
#)


#'==============================================================================
#  SR Model Prediction Data Assembly -----    
#'==============================================================================
#'------------------------------------------------------------------------------
## Determine Max S  ---------------------------------------
#'------------------------------------------------------------------------------
#'---- UI Output Initial Value generating function ------------------------------
numinput <- function(dat,p=0.5){
  s <- quantile(dat,p,na.rm=TRUE)
  D <- floor(log10(s))
  # This makes largest numbers into integer (e.g. 100000)
  imr <- round(s,-D)  
  step <- 10^(D-1)
  out <- c(imr,step)
  return(out)
}

#'------------------------------------------------------------------------------
## SR.pred1 Bayesian Model Prediction ---------------------------------------
#'------------------------------------------------------------------------------
SR.pred1 <-reactive({
  srmodel <- Bayesmodel()$model
#'--------- Extract MCMC SR Model Parameters -----------------------------------
  D <- Bayesdata()$d
  u <- unit()
#'--------- Determine model S length -------------------------------------------
  Seq <- quantile(SR.post()$post$Seq,0.95,na.rm=TRUE)   # Extract 90 percentile Seq
  # Extract max spawner
  max.s <- ceiling(max(Seq,max(sr.data.0()$S))/(10^D))*(10^D)
  unit.list <- c(10^(0:10),5*10^(0:9))
  picker <-  function(x, viable_numbers) {
  max(viable_numbers[viable_numbers < x])
  }
  S <- seq(0,max.s, picker(max.s/200,unit.list)) 
  if(input$add == 'kf') {
   if(input$alphai == 'None'){
    out <- SR.pred.sim(SR.post()$post,D,S,srmodel,input$add)
    } else {
    out <- SR.pred.sim(SR.post.i()$post,D,S,srmodel,input$add)     
    }
  } else {
    out <- SR.pred.sim(SR.post()$post,D,S,srmodel,input$add)     
   }
  return(out)
 })  

#' SR.pred Determine Median or Mean recruitment option -----
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
  out$lnRS <- pred$lnRS
  out$lnRS.p <- pred$lnRS.p
  return(out)
})  

#' SRp ------ Model predicted mean, CI, PI  SR range ----------------------------
SRp <- reactive({
       pred_CI(SR.pred(),input$CI)
  })  

# sr.cut TVA SR predictions ----------------------------------------------------  
sr.cut <- reactive({
# Get the model 
  if(input$add=='kf'){
    # Call Bayes model 
    srmodel <- Bayesmodel()$model
    model.br <- Bayesmodel()$model.br
#  Extract MCMC SR Model Parameters 
    star <- lnalphais()$cuty
    beta <- mean(SR.post()$post$beta)
    sigma <- mean(SR.post()$post$sigma)
    S <- SR.pred()$S
    D <- Bayesdata()$d
# Get unique alpha 
    alpha.star <- star$star
    alpha.star.c <- alpha.star + 0.5*sigma^2
    br <- model.br(alpha.star,beta,D)
    br.c <- model.br(alpha.star.c,beta,D)
    nstar <- length(alpha.star)
    R <- data.frame()
    R.c <- data.frame()
    for(i in 1:nstar){
    Ri <- srmodel(alpha.star[i],beta,S,D)
    Rci <- srmodel(alpha.star.c[i],beta,S,D)
    if(i==1){
    R <- Ri
    R.c <- Rci
    } else {
    R <- cbind(R,Ri)
    R.c <- cbind(R.c,Rci)
    }
    }
   out <- list(nstar=nstar, br=br,br.c =br.c, R = R, R.c=R.c)
  }
 })


#' downloadData ---- Results download 
#downloadServer('MCMC',
#  as.data.frame(as.matrix(MCMC())),
#  paste0('MCMCdata_', model.name(),'_', Sys.Date()))

#'==============================================================================
#  Standard SR and Yield Plots and Tables Outputs  
#'============================================================================== 
#'------------------------------------------------------------------------------
# DATA plots functions----
#'------------------------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## UI Output Lower Goal -----------------------------------------------------

output$maxS <- renderUI({
  u <- unit()
  # Extract max spawner
  max.s <- max(SRp()$S)/u
  step = 10^(floor(log10(max.s))-1)
  sliderInput("maxS", paste("Max S",mult(u)),min=0,max=max.s,value=max.s,step=step)
 })
output$maxR <- renderUI({
  u <- unit()
  maxR <- round(1.25*max(sr.data.0()$R/u,na.rm=TRUE))
  step = 10^(floor(log10(maxR))-1)
  sliderInput("maxR", paste("Max R",mult(u)),min=0,max=maxR,value=maxR,step=step)
 })
output$Yrange <- renderUI({
  u <- unit()
  minY <- round(min(SRp()$Rl-SRp()$S,na.rm=TRUE)/u)
  maxY <- round(1.25*max(sr.data.0()$Y,na.rm=TRUE)/u)
  step = 10^(floor(log10(maxY))-1)
  sliderInput("Yrange", paste("Yield Range",mult(u)), value=c(minY,maxY),min=minY,max=maxY,step=step)
 })


base.r <- reactive({base.sr()$base.r})
base.y <- reactive({base.sr()$base.y})
base.ln <- reactive({base.sr()$base.ln})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Panel 4: SR Model Analyses plots ----  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# br.data : SEQ, SMSY, SMAX, SGEN, UMSY  +++++++++++++++++++++++++++++++++++++++
br.data <- reactive({
  # SMSY Calc 
  srmodel <- Bayesmodel()$model
  model.br <- Bayesmodel()$model.br
  lnalpha <- mean(SR.post()$post$lnalpha)
  lnalpha.c <- mean(SR.post()$post$lnalpha.c)
  beta <- mean(SR.post()$post$beta)
  D <- Bayesdata()$d
  if(input$add=='kf')
  {
    if(input$alphai != 'None') {
      lnalpha <- mean(SR.post.i()$post$lnalpha) 
      lnalpha.c <- mean(SR.post.i()$post$lnalpha.c) 
      beta <- mean(SR.post.i()$post$beta)
    } 
  } 
  # Get unique alpha 
  bref <- model.br(lnalpha,beta,D)
  bref.c <- model.br(lnalpha.c,beta,D)
  Smsy <- ifelse(input$target=='me',bref.c$Smsy,bref$Smsy)
  Sgen <- ifelse(input$target=='me',bref.c$Sgen,bref$Sgen)
  Seq  <- ifelse(input$target=='me',bref.c$Seq,bref$Seq)
  Umsy <- ifelse(input$target=='me',bref.c$Umsy,bref$Umsy)
  Smax <- bref$Smax
  
  out  <- data.frame(x = c(Seq,Smsy, Smax, Sgen,Umsy), 
                     label = c(
                       paste("Seq:", format(round(Seq, 0))),
                       paste("Smsy:", format(round(Smsy, 0))),
                       paste("Smax:", format(round(Smax, 0))),
                       paste("Sgen:", format(round(Sgen, 0))),
                       paste("Umsy:",format(round(Umsy,2)))
                     ),
                     linetype = c("solid","dashed","dotted","dotdash","dashed")) 
  return(out)  
})
#### TVA data  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
kf.data <- reactive({
 if(input$add=='kf'){
    star <- lnalphais()$cuty
    br <- sr.cut()$br
    br.c <- sr.cut()$br.c
    nstar <- sr.cut()$nstar
    ny <- star$ny
   if(input$target=='me'){
      df <- data.frame(sr.cut()$R.c)
   } else {
      df <- data.frame(sr.cut()$R)
   }  
    names(df) <- c(1:nstar)
      df$S <- SRp()$S
      df <- melt(df, id.vars='S',variable.name='star',value.name='R' )
      out <- list(df=df,star=star,nstar=nstar,ny=ny)
    return(out)
      }
  })


## Plt_SR ------ SR plot -------------------------------------------------------
plt_SR <- reactive({plt_SRY()$pltSR})
output$Plt_SR <- renderPlot({add_title(plt_SR(),sr.title())})

output$Txt_SR <- renderText({
paste0("Spawner-recruit curve (solid: median, dash: mean). Gray shade and dashed line indicate ", 
       input$CI,"% Bayesian ",input$Li," and Prediction interval.")
  })

output$SRinfo <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("")
      paste0("S:", round(e$x, 2)," R:",round(e$y, 2))
    }
   xy_str(input$SR_click)
  })


## Plt_yield -------- Yield plot ----------------------------------------------- 
plt_yield <- reactive({plt_SRY()$pltYD})
output$Plt_yield <- renderPlot({add_title(plt_yield(),sr.title())})

output$Txt_YD <- renderText({
paste0("Spawner-Yield curve (Solid:median, dash:mean). Gray shade and dashed line indicate ", 
       input$CI,"% Bayesian ",input$Li," and Prediction interval.")
  })

## Plt_lnRS --- Plot ln(R/S) vs S ------------------------------------------
plt_lnRS <- reactive({plt_SRY()$pltLN})

output$Plt_lnRS <- renderPlot({add_title(plt_lnRS(),sr.title())})

output$Txt_lnRS <- renderText({
paste0("ln(R/S) plot (Solid:median, dash:mean). Gray shade and dashed line indicate ", 
       input$CI,"% Bayesian ",input$Li," and Prediction interval.")
  })


## Plt_MSY.mc --- Overlay SEQ, SMSY, SMAX, SGEN distribution over SR Plot ------
output$Plt_MSY.mc <- renderPlot({
  add_title(plt_SRY()$pltBRp,sr.title())
  })

output$Txt_MSY.mc <- renderText({
"SR plot overlayed with probability distribution of Seq, Smsy, Smax, and Sgen. 
  Vertical line indicate median estimates."
  })
## Plt_kobe --- Kobe Plot ------
output$Plt_kobe <- renderPlot({plt_kobe()})


#'------------------------------------------------------------------------------
# SR Model Diagnoses plots--------------------------
#'------------------------------------------------------------------------------
#'==============================================================================
#  Model Diagnoses -----    
#'==============================================================================
## SR.resid ----- Model Residuals ----------------------------------------------
SR.resid <-reactive({
#'---------- Select SR Model ---------------------------------------------------  
  srmodel <- Bayesmodel()$model
#'---------- Extract MCMC SR Model Parameters ----------------------------------
  lnalpha <-SR.post()$post$lnalpha
  beta <- SR.post()$post$beta
  phi <- SR.post()$post$phi
  S <- sr.data()$S
  D <- Bayesdata()$d
  R <- sr.data()$R
#'---------  Set up empty matrix -----------------------------------------------  
  ncol <- length(S) 
  nrow <- length(lnalpha)  #  Extract number of MCMC sample 
# Create Residuals  MCMC matrix    
  RD <- matrix(NA,nrow=nrow,ncol=ncol) 
  RD2 <- matrix(NA,nrow=nrow,ncol=ncol) 
#'-------- Time variant alpha residual -----------------------------------------
if(input$add=='kf'){
  if(input$BayesMCMC==TRUE){
    parname <- paste0('lnalphai.',1:ncol,'.')
  } else{
    parname <- paste0('lnalphai[',1:ncol,']')
  }
    # Extract lnalphai  
  lnalphai <- as.matrix(SR.post()$post[,parname])
  for(i in 1:nrow){
      # Calculated expected Returns form each MCMC SR model parameters   
      Ey <- srmodel(lnalphai[i,],beta[i],S,D)
      RD[i,] <- try(log(R)-log(Ey)) 
     }  
    } # End if 
#'------ Others ----------------------------------------------------------------
       else {
  for(i in 1:nrow){
    # Calculated expected Returns form each MCMC SR model parameters   
    Ey <- srmodel(lnalpha[i],beta[i],S,D)
    RD[i,] <- try(log(R)-log(Ey))
         }  
       }
#'------ Residuals for AR1 Remove AR1 correlation ------------------------------
  if(input$add=='ar1'){
      for(i in 1:nrow){
        RD2[i,2:ncol] <- RD[i,2:ncol] - phi[i]*RD[i,1:(ncol-1)]
        RD2[i,1] <- RD[i,1]-phi[i]*SR.post()$post$e0[i]
         }  
      } # End else
#' Create residuals ------------------------------------------------------------  
  out <- list(RD=RD,RD2=RD2)
  return(out)
  })  

output$Txt_dwtest.title <- renderText(if(input$add !='ar1'){'Durbin-Watson Autocrrelation Statisstics'})
output$Txt_dwtest.resid <- renderPrint(if(input$add !='ar1'){
  durbinWatsonTest(lm(apply(SR.resid()$RD,1,mean)~1))
  })


output$Plt_predict <- renderPlot({plt_predict()})

output$Plt_residual <- renderPlot({plt_residual()})

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  State-Space Model diagnoses ----    
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

output$Plt_SS_Age <- renderPlot({plt_SS_Age()})


output$Plt_SS <- renderPlot({plt_SS()})


output$Plt_SS_BAge <- renderPlot({plt_SS_BAge()})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Panel 5: Escapement Goal Analyses---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'===============================================================================
### Profile Analyses---- 
#'===============================================================================
#'-------------------------------------------------------------------------------
###  Smsy Goal Analyses: This produces 
#  EG.Smsy, EG.Smsy.st, SA.BEG, p.msy, p.msy.t
#'-------------------------------------------------------------------------------
smsyprof <- ProfileServer("smsy",SR.pred,'MSY',unit,plt)
  EG.Smsy <- reactive({smsyprof$EG()})
  EG.Smsy.st <- reactive({smsyprof$EG.st()})
  SA.BEG  <- reactive({(smsyprof$BEG())})
  p.msy <- reactive({smsyprof$p.min()})
  p.msy.t <- reactive({smsyprof$p.t()})
# Basic smsy profile plot
  plt.msy.prof <- reactive({smsyprof$plt.profile()})  
# Smsy profile plot with bounds  
  plt.msy.prof.fig <- reactive({smsyprof$plt.prof.fig()})
# Output Smsy profile plot   
  output$Plt_Smsy_prof <- renderPlot({plt.msy.prof.fig()})
  output$Tbl_MSY_gl <- renderTable(SA.BEG(),spacing="xs",digits=0)
#'-------------------------------------------------------------------------------
#  Smax Goal Analyses 
#'-------------------------------------------------------------------------------
  maxS <- reactive({ifelse(isFALSE(input$PlotMore),round(max(SRp$S)),input$maxS)})
  smaxprof <- ProfileServer("smax",SR.pred,'Rmax',unit,plt)  
  EG.Smax <- reactive({smaxprof$EG()})
  EG.Smax.st <- reactive({smaxprof$EG.st()})
  SM.BEG  <- reactive({(smaxprof$BEG())})   # Smax based goal range
  p.max <- reactive({smaxprof$p.min()})
  p.max.t <- reactive({smaxprof$p.t()})
# Basic smax profile plot  
  plt.max.prof <- reactive({smaxprof$plt.profile()})
# Smax profile plot with bounds   
  plt.max.prof.fig <- reactive({smaxprof$plt.prof.fig()})
  output$Plt_Smax_prof <- renderPlot({plt.max.prof.fig()})  
  output$Tbl_Rmax_gl <- renderTable(SM.BEG(),spacing="xs",digits=0)
  
#'-------------------------------------------------------------------------------
###  Custom Profile Range Analyses----   
#'-------------------------------------------------------------------------------
# Create profile Table  
  T.Prof <- reactive({
    out <- data.frame(EG.Smsy()$S,t(EG.Smsy.st()$S.prof.st),EG.Smsy()$S.prof,
                      t(EG.Smax.st()$S.prof.st),EG.Smax()$S.prof)
    names(out) <- c('S','MSY90','MSY80','MSY70',paste0('MSY',p.msy()),
                    'MAX90','MAX80','MAX70',paste0('MAX',p.max()))
    return(out)
  })
  
  # Create profile Table  
  Table.Prof <- reactive({
     # Generate profile 
      S <- EG.Smax()$S
      Y.prof <- EG.Smax()$S.prof
      Y.prof.st <- data.frame(t(EG.Smax.st()$S.prof.st))
      names(Y.prof.st) <- c('p90','p80','p70')
      df <- data.frame(S=S,Med=Y.prof,Y.prof.st )
      out <- melt(df,id.vars ='S',variable.name='prof.type',value.name='prob') 
    return(out)
  })
# Create Summary Table  
    
  
Prof.sum <- reactive({
    tprof <- T.Prof()
      tp <- c(0.7,0.8,0.9)
      tm <- c(2:4,6:8)
      tr <- length(tp)*length(tm)
      target <-vector('character',tr)
      pt <- vector('integer',tr)
      pm <- vector('integer',tr)
      minEG <- vector('integer',tr)
      maxEG <- vector('integer',tr)
        for(j in 1:6){
          for(i in 1:3){
        S.prof <- tprof$S[tprof[,tm[j]] >= tp[i]]
        target[i+3*(j-1)] <- substring(names(tprof)[tm[j]],1,3)
        pt[i+3*(j-1)] <- as.numeric(substring(names(tprof)[tm[j]],4,5))
        pm[i+3*(j-1)] <- 100*tp[i]
        minEG[i+3*(j-1)] <- min(S.prof)
        maxEG[i+3*(j-1)] <- max(S.prof)
          }
        }
       out <- data.frame(Target=target,PercOfTarget=pt,ProbOfAchieving=pm, 
                         minEG=minEG, maxEG=maxEG)
       return(out)
  })  


output$Tbl_prof <- renderTable(Prof.sum(),digits=0)  
#output$Tbl_prof <- DT::renderDT(datatable(data.frame(S=SR.pred()$S,Yield_gl_sim()),rownames = FALSE))    
#'------------------------------------------------------------------------------
#'  Ridge plot
#'------------------------------------------------------------------------------
output$Plt_msyprof_r <- renderPlot({plt_msyprof_r()})

output$Plt_maxprof_r <- renderPlot({plt_maxprof_r()})


# downloadData ---- profile download -------------------------------------------
#  downloadServer('Prof',T.Prof(),paste0('Profile_', model.name(),'_', Sys.Date()))
output$download.prof <- downloadHandler(
  filename = function(){
     paste0('Profile_', model.name(),'_', Sys.Date(),'.csv')
    },
    content = function(file) {
      write.csv(data.frame(T.Prof()), file,row.names = FALSE,na='')  
    }
)

# downloadData ---- Profsummary download -------------------------------------------
#  downloadServer('ProfSum',Prof.sum(),paste0('Profile_summary', model.name(),'_', Sys.Date()))
output$download.profSum <- downloadHandler(
  filename = function(){
     paste0('Profile_summary_', model.name(),'_', Sys.Date(),'.csv')
    },
    content = function(file) {
      write.csv(data.frame(Prof.sum()), file,row.names = FALSE,na='')  
    }
)
  
#'===============================================================================
#  Smsy-Smax Goal Analyses Output 
#'===============================================================================
### Yield and Recruit Plot -------------------------------------------------------
output$Plt_rec.pg <- renderPlot({
  plot_range(base.r(),EG.Smsy()$S.Range,EG.Smax()$S.Range,unit(),NA)
  })
  
output$Plt_yield.pg <- renderPlot({
  plot_range(base.y(),EG.Smsy()$S.Range,EG.Smax()$S.Range,unit(),NA)
  })
  
### Txt_Srange.smsy -------- Smsy goal output ------------------------------------
  output$Txt_Srange.smsy <-renderUI({ SA.BEG() })
### Txt_Srange.smax -------- Smax Goal range output table ------------------------
  output$Txt_Srange.smax <-renderUI({ SM.BEG() })


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Target Yield and Recruit based Escapement Goal ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#'---- UI Output-----------------------------------------------------------------
output$minYield = renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(sr.data()$R-sr.data()$S,0.5)/u
  numericInput("y1", paste("Target Yield",mult), value=v[1],min=0, step=v[2])
})

Yield_gl <- reactive({
  prof_sim(SR.pred()$S,SR.pred()$Y,SR.pred()$Y.p,input$y1*unit (),input$y1p)
  })

output$Plt_yield.prof <- renderPlot({plt_yield.prof()})

# Print Optimum Yield Profile Goal Range  
output$Txt_Yield_gl <-renderText({
  BEG.p <- Yield_gl()$range$b.p
  tex <- ifelse(input$target =='me','Mean','Median')
  paste(tex,'target range:',BEG.p[1],'-',BEG.p[2])
  })

# Yield Plot 
output$Plt_yield.gl <- renderPlot({
  u <- unit()
  BEG.p <- Yield_gl()$range$b.p
  plot_range(base.y(),BEG.p,NA,u,input$y1*u)
    }) 

Yield_gl_sim <- reactive({
  u <- unit()
  mc.Ypm <- Prob.calc(SR.pred()$Y,input$y1*u)$Ypm
  mc.Ypa <- Prob.calc(SR.pred()$Y.p,input$y1*u)$Ypm
  out <- data.frame(mc.Yp=mc.Ypm, mc.Ypa=mc.Ypa)
  return(out)
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Recruitment  based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#---- UI Output-----------------------------------------------------------------
output$minRec <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(sr.data()$R,0.5)/u
  numericInput("r1", paste("Target Recruit",mult), value=v[1],min=0,step=v[2])
})

#'------------------------------------------------------------------------------
#  Calculate probability that intercepts profile 
#'------------------------------------------------------------------------------
Rec_gl <- reactive({
  prof_sim(SR.pred()$S,SR.pred()$R,SR.pred()$R.p,input$r1*unit (),input$r1p)
})

# Recruit Plot 
output$Plt_rec.gl <- renderPlot({
  u <- unit()
  BEG.p <- Rec_gl()$range$b.p
  plot_range(base.r(),BEG.p,NA,u,input$r1*u)
  }) 


output$Plt_rec.prof <- renderPlot({plt_rec.prof()})  

# Optimum Recruit Profile Escapement Goal 
output$Txt_Rec_gl <-renderText({
  BEG.p <- Rec_gl()$range$b.p
  tex <- ifelse(input$target =='me','Mean','Median')
  paste(tex,'target range:',BEG.p[1],'-',BEG.p[2])
  })
#'------------------------------------------------------------------------------
#'  Ridge plot
#'------------------------------------------------------------------------------
output$Plt_yield_r <- renderPlot({plt_yield_r()})

output$Plt_rec_r <- renderPlot({plt_rec_r()})

#'===============================================================================
# Custom escapement goal analyses ----
#'===============================================================================

## UI Output Lower Goal -----------------------------------------------------
output$minEG <- renderUI({
  u <- unit()
  mult <- mult(u)
  v <- numinput(data()[,2],0.1)/u
  numericInput("lg", paste("Lower",mult), value=v[1],min=0, step=v[2])
 })
## UI Output Upper Goal -----------------------------------------------------
output$maxEG <- renderUI({
  u <- unit()
  mult <- mult(u)
  v <- numinput(data()[,2],0.75)/u
  numericInput("ug", paste("Upper",mult), value=v[1],min=0, step=v[2])
})

## UI Output Minimum Yield --------------------------------------------------
output$cyg = renderUI({
  u <- unit()
  mult <- mult(u)
  h <- (data()[,3]-data()[,2])
  v <- numinput(h,0.25)/u
  numericInput("yg", paste("Min Target Yield",mult), value=input$y1,min=0, step=v[2])
})
## UI Output Minimum Recruit ------------------------------------------------
output$crg = renderUI({
  u <- unit()
  mult <- mult(u)
  v <- numinput(data()[,3],0.25)/u
  numericInput("rg", paste("Min Target Recruit",mult), value=input$r1,min=0, step=v[2])
})



#'-------------------------------------------------------------------------------
## plt_cg_prof: Plot  MSY and Rmax prof at given escapement---- 
#'-------------------------------------------------------------------------------


#'-------------------------------------------------------------------------------
## plt_cg_prof: Calculate MSY and Rmax prof probability at given escapement---- 
#'-------------------------------------------------------------------------------
tbl_cg_prof <- reactive({
  SS <- c(input$lg,input$ug)*unit()
  p.msy <- c(as.numeric(p.msy()),70,80,90)
  p.max <- c(as.numeric(p.max()),70,80,90)
  EG.msy.pf <- data.frame(S=EG.Smsy()$S,EG.Smsy()$S.prof,t(EG.Smsy.st()$S.prof.st))
  EG.max.pf <- data.frame(S=EG.Smax()$S,EG.Smax()$S.prof,t(EG.Smax.st()$S.prof.st))
  
  prof.cut <- function(prof, SS){
  EG.pl <- prof[prof$S>=SS[1],]
  pl <- as.vector(100*EG.pl[1,-1])
  EG.pu <- prof[prof$S>=SS[2],]
  pu <- as.vector(100*EG.pu[1,-1])
  out <- cbind(pl,pu)
  return(out)
  }
  t.msy <- data.frame(p.msy, prof.cut(EG.msy.pf,SS))
    names(t.msy) <- c('MSY %','Lower','Upper')
  t.max <- data.frame(p.max, prof.cut(EG.max.pf,SS))
    names(t.max) <- c('Rmax %','Lower','Upper')
  return(list(msy=t.msy,max=t.max))
  })

#'----- Profile output ----------------------------------------------------------
# MSY prof
output$Plt_msyprof_c <- renderPlot({plt_cg_prof()$msy})
output$Tbl_msyprof_c <- renderTable(tbl_cg_prof()$msy,spacing="xs",digits=0)
# Rmax prof
output$Plt_maxprof_c <- renderPlot({plt_cg_prof()$max})
output$Tbl_maxprof_c <- renderTable(tbl_cg_prof()$max,spacing="xs",digits=0)  

#'-------------------------------------------------------------------------------
# CG_sim:  Calculate Predicted Rec Yield in given EG range----
#'-------------------------------------------------------------------------------
CG_sim <- eventReactive(input$Run,{
#'-------------------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'MCMC calculationin progress',
               detail = 'This may take a while...')
  for (i in 1:15) {progress$set(value = i)}
#'----------==-------------------------------------------------------------------   
  u <- unit()
  #  Import user defined lower and upper goal 
  lg <- input$lg*u
  ug <- input$ug*u
  # create goal range 
  S <- seq(lg,ug,length.out=201)   
  D <- Bayesdata()$d
  srmodel <- Bayesmodel()$model
#'---------- Extract MCMC SR Model Parameters -----------------------------------
  lnalpha <-SR.post()$post$lnalpha
  lnalpha.c <-SR.post()$post$lnalpha.c
  beta <- SR.post()$post$beta
  sigma <- SR.post()$post$sigma
  if(input$add=='kf')
  {
    if(input$alphai != 'None') {
      lnalpha <- SR.post.i()$post$lnalpha 
      lnalpha.c <- SR.post.i()$post$lnalpha.c 
      beta <- SR.post.i()$post$beta
      sigma <- SR.post.i()$post$sigma      
    } 
  } 
  nrow <- length(lnalpha)  #  Extract number of MCMC sample 
  # Create Expected mean and observed Recruit MCMC matrix    
  R <- matrix(NA,nrow=nrow,ncol=201) 
  R.c <- matrix(NA,nrow=nrow,ncol=201) 
  R.p <- matrix(NA,nrow=nrow,ncol=201)
  
  for(i in 1:nrow){
    # Calculated expected Returns form each MCMC SR model parameters   
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
#'------  Create Output list files ---------------------------------------------  
  # Outputs are mean and annual Yields and recruits within proposed S range.    
  out <- list(S = S, R = R, R.c=R.c,Y = Y, Y.c = Y.c,R.p = R.p, Y.p = Y.p)
  return(out) 
  })

#'------------------------------------------------------------------------------
# CG_pct:  Calculate % achieving target yield and recruit 
#'------------------------------------------------------------------------------
CG_pct <- reactive({
  u <- unit()
# Recruit   
  rg <- input$rg*u
  # Probability of meeting long-term Mean/Median Recruit Target    
  prg <- mean(ifelse(CG_sim()$R>rg,1,0))
  # Probability of meeting annual Mean/Median Recruit Target  
  prgp <- mean(ifelse(CG_sim()$R.p>rg,1,0))
# Yield   
  yg <- input$yg*u
  # Probability of meeting long-term Mean/Median Yield Target  
  pyg <- mean(ifelse(CG_sim()$Y>yg,1,0))
  # Probability of meeting annual Mean/Median Yield Target   
  pygp <- mean(ifelse(CG_sim()$Y.p>yg,1,0))
  # Probability of getting Zero Yield at given escapement.    
  py0 <- mean(ifelse(CG_sim()$Y.p<0,1,0))  
  out <- data.frame(prg=prg, pyg = pyg, prgp=prgp, pygp = pygp,py0=py0) 
  
# Table output  Recruit
  tex <- ifelse(input$target =='me','Mean','Median')
  t.prg <- paste('Meeting Recruit Target',tex,':',round(100*prg,0),'%')
 t.prgp <- paste('Meeting Recruit Target Annual:',round(100*prgp,0),'%')
  Rec.txt <- paste(t.prg,t.prgp,sep='\n')
  
# Table output Yield  
  t.pyg <- paste('Meeting Yield Target', tex,':',round(100*pyg,0),'%')
  t.pygp <- paste('Meeting Yield Target Annual :',round(100*pygp,0),'%')
  t.py0 <- paste('Zero Yield Annual:',round(100*py0,0),'%')
  Yld.txt <- paste(t.pyg,t.pygp,t.py0,sep='\n')
  return(list(data=out, Rec.txt = Rec.txt,Yld.txt=Yld.txt))
})

#'-----------------------------------------------------------------------
#  Plot distribution of Mean and Annual  Yield at Given Escapement Range
#'-----------------------------------------------------------------------
output$Plt_Yield_EG <- renderPlot({plt_Yield_EG()}) 

#'-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#  SR model based CI and PI
#'-----------------------------------------------------------------------
output$Plt_Rec_EG <- renderPlot({plt_Rec_EG()}) 

# Recruit goal output -----------------------------------------------------------
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
  print(sum.fun(dat,90),digits=1)
  })
  
# Yield goal output -----------------------------------------------------------
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
  print(sum.fun(dat,90),digits=1)
  })

# Calculate Probability meeting target  
output$Txt_Rec_pb_cg <- renderText({CG_pct()$Rec.txt
  })

# Calculate Probability meeting target  
output$Txt_Yield_pb_cg <- renderText({CG_pct()$Yld.txt
  })

# Recruit Plot 
output$Plt_rec.cg <- renderPlot({
  u <-unit()
  plot_range(base.r(),c(input$lg,input$ug)*u,NA,u,input$rg*u)
}) 

# Yield Plot 
output$Plt_yield.cg <- renderPlot({
  u <-unit()
  plot_range(base.y(),c(input$lg,input$ug)*u,NA,u,input$yg*u)
    }) 

#'-------------------------------------------------------------------------------
#  Save multiple Simulation Results
#'-------------------------------------------------------------------------------
# Set temporary memory: M
# gls: custom EG range 
gls <- reactiveVal(data.frame())
ys <- reactiveVal(data.frame())
meds <- reactiveVal(data.frame())
# Retrieve simulation results data 

# Data update 
observeEvent(input$Run,{
  # Put old value here 
  old_gl <- gls()
  old_y <- ys()
  old_meds  <- meds()
  # create new data  
  glt <- paste(input$lg,'-',input$ug)
  pct <- round(100*CG_pct()$data,0)
  gl <- data.frame(glt, pct)
  names(gl) <- c('Esc Goal Range', '% Recruit Lng','% Yield Lng',
                 '% Recruit Anl','% Yeild Anl','% Zero Yield Anl' )
  y <- data.frame(Y = as.vector(CG_sim()$Y), Y.p = as.vector(CG_sim()$Y.p),
                  R = as.vector(CG_sim()$R), R.p = as.vector(CG_sim()$R.p))
  med <-  data.frame(EG_Range=as.vector(glt), 
                     round(t(c(apply(y,2,mean),apply(y,2,median)))))
  
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


output$Tbl_sim <- renderTable(meds(),digits=0)

output$altsim.R <- renderPrint({

  x <- ys()
  alts <- dim(x)[2]
  Y.alt <- data.frame(x[,seq(1,alts,4)])
  Yp.alt <- data.frame(x[,seq(2,alts,4)])
  R.alt <- data.frame(x[,seq(3,alts,4)])
  Rp.alt <- data.frame(x[,seq(4,alts,4)])
  out <- list(Y=summary(Y.alt),Yp=summary(Yp.alt),R=summary(R.alt),Rp=summary(Rp.alt))
  return(out)
})


#'===============================================================================
#  Percentile Analyses ----
#'===============================================================================
# Call Percentile Analyses module Server 
prcntout <- PercentileServer("prcnt",e.data,as.numeric(unit()),plt)

# Txt_Tier : Tier Definition output  
  txt <- reactive({prcntout$Txt_Tier()})
  output$Txt_Tier <- renderUI({ txt() })

# Txt_Note:  Tier based goal range  
  txt2 <- reactive({prcntout$Txt_Note()})

  output$Txt_Note <- renderUI({ txt2() })
  
  EGS <- reactive({prcntout$EGS()})
  
  Tier <- reactive({prcntout$Tier()})

  plt_prcnt <- reactive({prcntout$Plt_prcnt()})
  
#  plt_prcnt_hist <- reactive({prcntout$Plt_prcnt_hist()})
  
  output$Plt_prcnt <- renderPlot({plt_prcnt()})
#  output$Plt_prcnt_hist <- renderPlot({plt_prcnt_hist()})

#'===============================================================================
#  Risk Analyses ----
#'===============================================================================
# Call Risk Analyses module Server 
riskout <- RiskServer("risk",e.data,as.numeric(unit()),plt)

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

#'===============================================================================
#  Panel 4: Management Strategy Evaluation  ----  
#'===============================================================================
#'-------------------------------------------------------------------------------
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

#'---- UI Output-----------------------------------------------------------------
output$LEG <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,2],0.25)/u
  numericInput("LEG", paste("Lower",mult), value=v[1],step = v[2])
})
#'---- UI Output-----------------------------------------------------------------
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
  lnalpha <-SR.post()$post$lnalpha
  mn <- length(lnalpha)
  numericInput("nsim", "N Simulation", value=500,min=1,max=mn,step=100)
})

output$sim.rep = renderUI({
  numericInput('sim.rep', 'Simulation rep', value=1,min=1,max=10000,step=1)
 })


#'------------------------------------------------------------------------------  
#  Function sets:
#'------------------------------------------------------------------------------
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


#'------------------------------------------------------------------------------  
###  MSE.int: Create initial modeling conditions  ----
#'------------------------------------------------------------------------------  
MSE.int <-  eventReactive(input$SimRun,{
#'------------------------------------------------------------------------------  
# Extract S0 from brood data   
  brood <- tbl_brood()$brood
  S0 <- brood[is.na(brood$Recruit),2]
# S0 is last lage years of escapement 
  S0 <- S0[!is.na(S0)]
# lage is the maximum age    
  lage <- length(S0)
# Extract number of simulation years
  nyrs <- input$simy
# import brood table
  brood.p <- brood.p()[,-1]
# Maturity schedule is a random sample of observed brood proportion 
  e.p  <- brood.p[sample(dim(brood.p)[1],nyrs+lage,replace = TRUE),]    
# Prediction and Implementation Error are normal with mean 1.0   
  e.pred <- (rnorm(nyrs,1,input$spred/100))
  e.imp <- (rnorm(nyrs,1,input$simpH/100))
# Determine Fishery opening target: FT  
  out <- list(S0=S0,e.pred = e.pred, e.imp = e.imp, e.p = e.p)
  return(out) 
})

#output$foo2 <- DT::renderDT({data.frame(MSE.int()$e.p)})  

#'-------------------------------------------------------------------------------
# Txt_Strategy:  Explanation of Strategy 
#'-------------------------------------------------------------------------------  

#'-------------------------------------------------------------------------------
# msesim:  Run simulation and output 
#'-------------------------------------------------------------------------------  
msesim <- eventReactive(input$SimRun,{
#'-------------------------------------------------------------------------------
#  Import Error Data 
#'------------------------------------------------------------------------------- 
#'# Initial Spawners  
  S0 <- MSE.int()$S0
  lage <- length(S0)
# Prediction and Implementation Error
  e.imp <- as.vector(MSE.int()$e.imp)
  e.pred <- as.vector(MSE.int()$e.pred)
  nyrs <- length(e.pred)
  bt <- nyrs+lage  # Brood years.
# Brood age comp 
  e.p <- as.matrix(MSE.int()$e.p) # Maturity Schedule 
#---------- Extract MCMC SR Model Parameters -----------------------------------
# SR model   
  srmodel <- Bayesmodel()$model
# D  
  D <- Bayesdata()$d
# Extract SR parameters from MCMC data 
  lnalpha <-SR.post()$post$lnalpha
  beta <- SR.post()$post$beta
  sigma <- SR.post()$post$sigma 
  if(input$add=='kf'){
      if(input$alphai != 'None') {
        lnalpha <- SR.post.i()$post$lnalpha 
        beta <- SR.post.i()$post$beta
        sigma <- SR.post.i()$post$sigma      
     } 
    }
  
# When Error is AR1 process
    if(input$add=='ar1'){
    phi <- SR.post()$post$phi
# e0 is the residuals of the last year    
    e0 <- SR.resid()$RD[,Bayesdata()$nyrs]
    }
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
#'-------------------------------------------------------------------------------
# Select Parameters 
#'-------------------------------------------------------------------------------  
# Randomly select nsims numbers from MCMC 
  nsims <-sample(1:length(lnalpha),input$nsim)
# The number of bootstrap replicates
   boot.n <- input$nsim
#'-----------------------------------------------------------------------  
  progress <- Progress$new()
  on.exit(progress$close())
  progress$set(message = paste(boot.n,'MSE Calculation in progress'),
               detail = 'This will take a while. Be patient please....')
  
# Set output 
sim.out <- list()

for(k in 1:input$nsim){
  j <- nsims[k]  # select MCMC data 
#'-------------------------------------------------------------------------------
# Populate simulation for all years. 
#'-------------------------------------------------------------------------------  
# Extract lnalpha
  lnalpha.i <- rep(lnalpha[j],bt)
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
  sim.out[[k]] <- MSE.sim(srmodel,lnalpha.i,sbeta,S0,D,e.Rec,e.p,e.pred,e.imp,FT,mH,input$maxHR)  
  progress$set(value = k/boot.n)
  # Increment the progress bar, and update the detail text.
  progress$inc(1/boot.n, detail = paste("Completed", round(100*k/boot.n,0),"%"))
  # sim.out produces: 'N','S','H','R' for each 
  }
  return(sim.out)   
})


  
#'-------------------------------------------------------------------------------
# MSE.sum: Summarize MSE results as data.frame 
#'-------------------------------------------------------------------------------  
MSE.sum <- reactive({
  # Set output as data.frame   
  out <- data.frame()
  u <- as.numeric(unit())  
  for(i in 1:input$nsim){
    dat <- msesim()[[i]][,c('N','S','H')]
# identify whether fishing closure occurred 
    dat$H0 <- ifelse(dat$H==0,1,0)
# identify whether not meeting lower escapement goal   
    dat$EG <- ifelse(dat$S < input$LEG*u,1,0)
# identify whether not harvest meeting minimum target    
    dat$Hmin <- ifelse(dat$H < input$minH*u,1,0)
# identify  Run below 10      
    dat$ND <- ifelse(dat$N < 10,1,0)    
    f.H0 <- e.freq(dat$H0,1,'H0')
    # Consecutive fishery below minimum
    f.Hmin <- e.freq(dat$Hmin,1,'Hm')
    # Consecutive escapement failure
    f.EG <- e.freq(dat$EG,1,'EG')  
    # Consecutive escapement failure
    f.ND <- e.freq(dat$ND,1,'ND')     
    # Combine 
    f <- rbind(f.H0,f.Hmin,f.EG,f.ND)
    f$rep <- i
    out <- rbind(out,f)
  }
  return(out) 
})

MSE.sum2 <- reactive({
  dat <- MSE.sum()
  sumdata <- aggregate(length~crit+rep,sum,data=dat)
  out <- dcast(sumdata, rep~crit,value.var='length')
  return(out)
})
output$Tbl_MSE_sum <- DT::renderDT(MSE.sum2(),,rownames=FALSE)
#              colnames=c('Fishery Closure','Below Min Harvest','Below Lower EG','Stock Collapse')E)
#'-------------------------------------------------------------------------------
# Plt_mse:  MSE summary plot
#'-------------------------------------------------------------------------------  
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
  polygon(c(lyear-1,max(years)+2,max(years)+2,lyear-1),c(input$LEG,input$LEG,input$UEG,input$UEG),col=tcol(8,80),border=NA)
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

#'-------------------------------------------------------------------------------
# Plt_mse_rep:  MSE plot for each simulation 
#'-------------------------------------------------------------------------------  
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
  polygon(c(lyear-1,max(years)+2,max(years)+2,lyear-1),c(input$LEG,input$LEG,input$UEG,input$UEG),col=tcol(8,80),border=NA)
  abline(h=input$minH, col=2,lwd=2)
  title("MSE Simulation", xlab="Year", ylab=paste('Run / Escapement/Harvest',mult))
  legend('topright',legend=c('Run','Escapement','Harvest'),col = c(1,3,2),lwd=c(1,2,1), box.lty=0)
})




#output$Tbl_mse <- DT::renderDT({aggregate(length~rep+crit,FUN=sum, data=MSE.sum())})  
#output$Tbl_mse <- DT::renderDT({MSE.sum()}) 

#output$sims.out <- renderPrint({
#  fail.sum <- aggregate(length~rep+crit,FUN=sum, data=MSE.sum())
#  fail.sum.w <- dcast(fail.sum,rep~crit)
#  fail.sum.w[is.na(fail.sum.w)]<- 0
#  return(summary(fail.sum.w[,-1]))
# })

#'-------------------------------------------------------------------------------
#  Estimate frequencies of 
#'-------------------------------------------------------------------------------
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

output$download.mse.sim <- downloadHandler(
  filename = function(){
     paste0('MSEdata_', model.name(),'_', Sys.Date(),'.csv')
    },
    content = function(file) {
      write.csv(data.frame(t(do.call("cbind", msesim()))), file,row.names = FALSE,na='')  
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


#'==============================================================================
# Reporting Section ---- 
#'==============================================================================
# Create and download report 
output$downloadReport <- downloadHandler(
  filename = function(){paste0('SR_Report_',model.name(),'_',Sys.Date(),'.docx')
    },
  content = function(file) {
    tempReport <- file.path(tempdir(),"report_officedown.Rmd")
    tempTemplate <- file.path(tempdir(),"template.docx")
    tempTable <- file.path(tempdir(),"run_table.Rmd")  
    tempEQ <- file.path(tempdir(),"SR_Equations.Rmd")
    tempEQR <- file.path(tempdir(),"SR_Equations_Ricker.Rmd")
    tempEQB <- file.path(tempdir(),"SR_Equations_BH.Rmd")
    file.copy('Report/report_officedown.Rmd',tempReport,overwrite = TRUE)
    file.copy('Report/template.docx',tempTemplate,overwrite = TRUE) 
    file.copy('Report/run_table.Rmd',tempTable,overwrite = TRUE) 
    file.copy('Report/SR_Equations.Rmd',tempEQ,overwrite = TRUE) 
    file.copy('Report/SR_Equations_Ricker.Rmd',tempEQR,overwrite = TRUE) 
    file.copy('Report/SR_Equations_BH.Rmd',tempEQB,overwrite = TRUE) 
    params <- list(
	    title = input$txt_title,   # Text title
      texts = input$txt_free,    # Free entry 
	    file = data.get$fn(),      # Input file name 
      add = input$add,          # Model type
      data = input$dataType,
      JAGS = sim()$output$BUGSoutput,
      tbl_sumpost = tbl_sumpost(),
	    tbl_brood = tbl_brood()$brood,
	    tbl_run = tbl_run(),
      plt_runesc = plt_runesc(),
      plt_srt = plt_srt(),
      plt_SR = plt_SR(),
      plt_yield = plt_yield(),
      plt_predict = plt_predict(),
      plt_lnRS = plt_lnRS(),
	    plt_lnalphai =plt_lnalphai(),
      plt_residual = plt_residual(),
      plt_msy_prof =plt.msy.prof.fig(),
      plt_max_prof =plt.max.prof.fig()
      )
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    progress$set(message = 'Generating a report',
                 detail = 'This may take a while...')
    for (i in 1:150) {progress$set(value = i)}
    render(tempReport, output_file = file, 
                       output_format = 'word_document',
                       params = params,
                       envir = new.env(), intermediates_dir = tempdir()
  )
  }
 ) # End downloadReport


#'==============================================================================
# Help Information Section ----
#'==============================================================================
## Data Input Help ----
InfoServer('info1',Info_data_input_title,Info_data_input)

## Pool Age  Help ----
InfoServer('info2',Info_pool_title,Info_pool)

## Outlier Removed Help ------
InfoServer('info3',Info_Outlier_title,Info_Outlier)

## Mean vs. Median -----
InfoServer('info4',Info_md_title,Info_md)

## MSE Management Option -----------------
InfoServer('info5',Info_MSE_manage_title,Info_MSE_manage)


})# End of Server 
