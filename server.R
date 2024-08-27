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
#'  plt_xxx  (plot output for reporting)
#'==============================================================================
#'===============================================================================    
#  Server ----  
#'===============================================================================
server<-shinyServer(function(input, output, session){
#'-------------------------------------------------------------------------------
# ggplot theme set 
theme_set(theme_simple())
palette("Okabe-Ito")  
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
options(list(ggplot2.discrete.colour = okabe,ggplot2.discrete.fill = okabe))
#theme_set(theme_adfg())  
options(DT.options=list(pageLength=25))
#'-------------------------------------------------------------------------------
##  Tab Control ---- 
#'-------------------------------------------------------------------------------
observe({
  if(input$dataType=="Run") {
    hideTab(inputId = "tabs", target = "Escapement Only Analyses")
    showTab(inputId = "tabs", target = "Escapement Goal Analyses")
    showTab(inputId = "tabs", target = "MSE Analyses")
    showTab(inputId = "tabs", target = "SR Model")
    showTab(inputId = "subTab", target = "Run Table")
    showTab(inputId = "subTab", target = "Brood Table")
    hideTab(inputId = "ssTab", target = "Run Size")
    hideTab(inputId = "ssTab", target = "Run Age Comp")
    hideTab(inputId = "ssTab", target = "Brood Age Comp") 
    hideTab(inputId = "Panel", target = "Priors") 
    if(isTRUE(SS())){
    showTab(inputId = "ssTab", target = "Run Size")
    showTab(inputId = "ssTab", target = "Run Age Comp")
    showTab(inputId = "ssTab", target = "Brood Age Comp")   
    }
    if(input$Priors){
    showTab(inputId = "Panel", target = "Priors")  
    }
  } else if (input$dataType== "S-R") {
    hideTab(inputId = "tabs", target = "Escapement Only Analyses")
    showTab(inputId = "tabs", target = "Escapement Goal Analyses")
    hideTab(inputId = "tabs", target = "MSE Analyses")
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
## Reactive unit: Convert unit to numbers -----
 unit <- reactive({
    if(input$autoui==TRUE){
      d <-  floor(log10(max(e.data.0()$S,na.rm=TRUE)))
      u <- ifelse(d>=6,10^6,ifelse(d>3,10^3,1))
      } else {
      u <- ifelse(input$ui=='million',1000000,as.numeric(input$ui))
      }
    return(u)
     })
  
### data1 Data file reading module ---- 
data1 <-  dataInputServer("datain")

#' data:  Sample data input  ---- 
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
      
### Tbl_data: Uploaded data table output ----------------------------------------
  output$Tbl_data <- renderDT(datatable(data(),rownames = FALSE))  

##### UI agerange: Set Age range ------------------------------------------------------------
  output$agerange <- renderUI({
    if(input$dataType== "Run"){
     age <-  age.out(data())
     if(is.null(age)){
       renderText({
         paste("Wrong age data format")
       })
     }else{
     fage <- min(age)       # First age
     lage <- max(age)       # Last age
    #  Slider input UI 
    sliderInput("rage", label = paste("Select","Run Age Range"), min = fage, max = lage, value = c(fage, lage),step=1,sep = "")
     }
    }
    })
o.age <- reactive({if(input$dataType== "Run"){
     age.out(data())}
     })
  
##### UI agecomb  Set Age comp method ------------------------------------------
  output$agecomb <- renderUI({
    if(input$dataType== "Run" & (!is.null(input$rage[1]))){
      #  Slider input UI 
    checkboxInput("combage", label = InfoUI('info2','Pool Ages')
                    , value = TRUE)
        } 
  })


##### run.out construct run table (when dataType is "Run")    
  run.out <-reactive({
     if(input$dataType== "Run"){
      agedata <- make.age(data(),input$rage[1], input$rage[2],input$combage)
      data <- cbind(data()[,1:3],agedata)
      names(data)[1:3] <- c('Year','S','N')
      return(data)
       }
    })
##### Tbl_data.run ----- show run table ----------------------------------------
output$Tbl_data.run <- renderDT({datatable(round(run.out(),2),rownames = FALSE)}) 
  
 
##### run.cv Construct CV table (when dataType is "Run") ----   
  run.cv <-  reactive({
    if(input$dataType== "Run"){
# Check if all cv column exist 
      dat <- data()
      name <- names(dat)
      names(dat)[1:3] <- c('Year','S','N')
    if(length(name[name %in% c('cv_N','cv_E','cv_H','efn')])==4){
# Extract cv columns if they exist 
      H <- dat$N-dat$S
      dat$cv_N <- with(dat,ifelse(is.na(cv_N),
                     sqrt((S*cv_E)^2+(H*cv_H)^2)/N,cv_N))
      dat$cv_N[is.na(dat$cv_N)] <- max(dat$cv_N,na.rm=TRUE)
      dat$cv_E <- with(dat,ifelse(is.na(cv_E),
                      sqrt((N*cv_N)^2-(H*cv_H)^2)/S,cv_E))
      dat$cv_E[is.na(dat$cv_E)] <- max(dat$cv_E,na.rm=TRUE)
      dat$cv_H <- with(dat,ifelse(is.na(cv_H),
                      sqrt((N*dat$cv_N)^2-(E*cv_E)^2)/H,cv_H))
      dat$cv_H[is.na(dat$cv_H)] <- max(dat$cv_H,na.rm=TRUE)
      dat.cv <- dat[,c('Year','cv_N','cv_E','cv_H','efn')]
      return(dat.cv)
         }
      }
    })   

##### Reactive run.age.var Construct variance by run age  ----   
run.age.var <-  reactive({
    if(!is.null(run.cv())){
# Read run and cv data       
      run <- run.out()
      cv  <- run.cv()
# Read age data. 
      p.age <- run[,-c(1:3)]
      row <- dim(p.age)[1]
      col <- dim(p.age)[2]
      N.var <- (run$N*cv$cv_N)^2
      S.var <- (run$S*cv$cv_E)^2
      cv$efn <- ifelse(cv$efn==0,10,cv$efn)
# Calculate age prop variance as binomial
      if(col ==1){
        age.var <- data.frame(cbind(run$Year,S.var,N.var,N.var))
      } else {
      p.age.var <- matrix(0,row,col)
    for(i in 1:col){
      p.age.var[,i] <-p.age[,i]*(1-p.age[,i])/cv$efn
        }
# Calculate variance by age using Goodman's formula     
    age.var <- matrix(0,row,col)
    for(i in 1:col){
      age.var[,i] <-(run$N^2)*p.age.var[,i]+(p.age[,i]^2)*N.var-N.var*p.age.var[,i]
     }
      age.var <- data.frame(cbind(run$Year,S.var,N.var,age.var))
      } 
    names(age.var) <- names(run)  
    return(age.var)
    }
  })   

##### Reactive brood.out Construct brood table (when dataType is "Run")----   
  brood.out <-  reactive({
     if(input$dataType== "Run"){
      make.brood(run.out(),TRUE)
      }
    })
  
 brood.var.out <-  reactive({
    if(!is.null(run.cv())){
      make.brood(run.age.var(),FALSE)
      }
    })

  

##### Tbl_data.brood ----- show brood table ------------------------------------
output$Tbl_data.brood <- renderDT({datatable(round(brood.out()$brood,0),rownames = FALSE)}) 
#output$Tbl_data.brood <- DT::renderDT({round(brood.var.out()$brood,0)})   

##### Reactive brood.p brood age comp ------------------------------------------
  brood.p <- reactive({
    if(input$dataType== "Run"){
     brood <- brood.out()$brood[,-2] # Remove Spawner data
     brood <- brood[complete.cases(brood),]
     ncol <- dim(brood)[2]
     brood[,2:ncol] <- brood[,2:ncol]/brood$Recruit
     return(brood[,-ncol])
    }
   })
   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Create SR data ---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Reactive sr.data.0  Original SR data -------------------------------------------------------------  
 sr.data.0 <- reactive({
    if(input$dataType== "Run"){
      x <- brood.out()$SR
    if(!is.null(run.cv())){
      y <- run.cv()
      x <- merge(x,y[,c('Year','cv_E')],by.x='b.Year',by.y = 'Year',all.x = TRUE)
      }
    } else if (input$dataType== "S-R"){
      x <- data()
      x <- x[complete.cases(x),]
    }
    names(x)[1:3] <- c('Yr','S','R')
    x$Y <- with(x,R-S)
    x$lnRS <- with(x,log(R/S))
    return(x)   
  })

 
### Reactive e.data.0 Original Escapement data -----------------------------------------------------  
 e.data.0 <- reactive({
      x <- data()[,c(1:2)]
    names(x) <- c('Yr','S')
    return(x)     
  })  

 
#### UI yrange UI output to determine data year range --------------------------
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
  
### Reactive sr.data --- final dataset used for SR analyses --------------------
  sr.data <- reactive({cut.data(sr.data.0(),input$sryears) })
  
  sr.data.ci <- reactive({
    if(!is.null(run.cv())){
    sr <- sr.data()
    sr.v <- brood.var.out()$SR
    names(sr.v) <- c('Yr','Sv','Rv')
    temp <- merge(sr,sr.v,by='Yr')
# Calculate CV
    temp$Svl <- with(temp,sqrt(log(Sv/(S^2)+1))) 
    temp$Slci <- with(temp,exp(log(S)-2*Svl)) 
    temp$Suci <- with(temp,exp(log(S)+2*Svl)) 
    temp$Rvl <- with(temp,sqrt(log(Rv/(R^2)+1))) 
    temp$Rlci <- with(temp,exp(log(R)-2*Rvl)) 
    temp$Ruci <- with(temp,exp(log(R)+2*Rvl))
    out <- temp[,c('Yr','S','R','Slci','Suci','Rlci','Ruci')]
    return(out)
     }
  })
  
### Reactive ss.data --- final dataset used for percentile risk ----------------
  ss.year <- reactive({ 
    min <- input$sryears[1]
    max <- input$sryears[2]+input$rage[2]
    ssyear <- c(min,max)
    return(ssyear)
    })

### Reactive e.data --- final dataset used for percentile risk -----------------
  e.data <- reactive({ cut.data(e.data.0(),input$sryears) })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Plot SR data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") ----------
plt_runesc <- reactive({
   if(input$dataType== "Run"){
     x <- data()[,c(1:3)]
    names(x) <-c('Yr','S','R')
     x$ex <- with(x,(R-S)/R)
     x$H <- with(x,(R-S))
     u <- unit()
     p1 <- ggplot(data=x)+
       geom_line(aes(x=Yr,y=R,color='Run'),linetype=1)+
       geom_line(aes(x=Yr,y=S,color ='Escapement'),linetype=2)+
       geom_line(aes(x=Yr,y=H,color= 'Harvest'),linetype=3)+
# Second y axisis        
       geom_line(aes(x=Yr,y= inv_scale_function(R,ex,0,0),color='Harvest Rate'),linetype=4)+
    scale_x_continuous(expand=expansion(add = c(.5, .5)),n.breaks = 10,oob=oob_keep) +
    scale_y_continuous(expand=expansion(mult = c(0, .25)), limits=c(0, max(x$R)),
                      sec.axis = sec_axis(~scale_function(.,x$R,x$ex,0,0),name='Harvest Rate'),
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
      scale_color_manual(values=c(1,2,3,4),breaks=c('Run','Escapement','Harvest','Harvest Rate'),
        guide=guide_legend(override.aes=list(linetype=c(1,2,3,4))))+
        xlab(paste('Year')) + ylab(paste('Abundance',mult(u)))
      return(p1)                      
    }
 })
 
 
output$Plt_runesc <- renderPlot({plt_runesc()})

##### Plt_srt Plot SR time series ---------------------------------------
plt_srt <- reactive({
  if(input$dataType != 'Escapement Only'){
    x <- sr.data.0()
    u <- unit()
     x$ex <- with(x,log(R/S))
     u <- unit()
     p1 <- ggplot(data=x)+
       geom_line(aes(x=Yr,y=R,color='Recruit'),linetype=1)+
       geom_line(aes(x=Yr,y=S,color ='Spawner'),linetype=5)+
       geom_line(aes(x=Yr,y= inv_scale_function(R,ex,0,1),color='ln(R/S)'),linetype=1)+
        scale_color_manual(values=c(1,1,4),breaks=c('Recruit','Spawner','ln(R/S)'),
        guide=guide_legend(override.aes=list(linetype=c(1,5,1))))+
    scale_x_continuous(expand=c(0, 0.5),n.breaks = 10,oob=oob_keep) +
    scale_y_continuous(expand=expansion(mult = c(0, .25)), limits=c(0, max(x$R)),
                       sec.axis = sec_axis(~scale_function(.,x$R,x$ex,0,1),name='ln(R/S)'),
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+                       labs(x=paste('Year'),y=paste('Abundance',mult(u)))                   
    } else {
    x <- e.data.0()
    u <- unit()
     p1 <- ggplot(data=x)+
       geom_line(aes(x=Yr,y=S,color='Escapement'),linetype=1)+
    scale_x_continuous(expand=c(0, 0.5),n.breaks = 10,oob=oob_keep) +
    scale_y_continuous(expand=expansion(mult = c(0, .25)), limits=c(0, max(x$S)),
                       labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
             labs(x=paste('Year'),y=paste('Escapement',mult(u)))
    }
    # Add Cutting data 
    if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
      p1 <- p1+ 
      geom_vline(xintercept = input$sryears, color =2) 
    }
  return(p1)      
 })

output$Plt_srt <- renderPlot({plt_srt()})


##### Txt_sum.data:  summary sr data summary Output ----------------------------
output$Tbl_sum.data <- renderTable({
  if(input$dataType != "Escapement Only"){
  dat <- sr.data()
  dat$Y <- dat$R-dat$S
  x <- as.data.frame(t(t(sum.fun(dat[,c('S','R','Y','lnRS')],90))))  
for(i in 1:3){x[,i] <- as.integer(x[,i])}
  x[,4] <- round(x[,4],3)
  names(x) <- c('Spawner','Recruit','Yield','ln(R/S)')
  }else{
  dat <- e.data()  
  x <- as.data.frame(t(t(sum.fun(dat[,c('S')],90))))  
  x  <- as.integer(x)
  }
  return(x)
  },rownames=TRUE)

#### Plt_hist.sry:  sr data histogranm ------------------------------------------- 
plt_hist.sry <- reactive({
  u <- as.numeric(unit())
  if(input$dataType != "Escapement Only"){
  df <- sr.data()
  p1 <- gg_hist(df,S,u,df$S)+xlab(paste('Spawner',mult(u)))
  p2 <- gg_hist(df,R,u,df$R)+xlab(paste('Recruit',mult(u)))
  p3 <- gg_hist(df,Y,u,df$Y)+xlab(paste('Yield',mult(u)))
  p4 <- gg_hist(df,lnRS,1,df$lnRS)+xlab(paste('ln(R/S)',mult(1)))
  p5 <- plot_grid(p1,p2,p3,p4,nrow=1)
  return(p5)
  } else {
  df <- e.data()  
  p1 <- gg_hist(df,S,u,df$S)+xlab(paste('Spawner',mult(u)))
  return(p1)
  }
})

output$Plt_hist.sry <- renderPlot({plt_hist.sry()})

##### Txt_sum.data:  summary sr data summary Output ----------------------------
output$Tbl_sum_run.data <- renderTable({
  if(input$dataType == "Run"){
  df <- data()[,c(1:3)]
  names(df) <-c('Yr','S','R')
  df$H <- with(df,(R-S))
  df$HR <- with(df,H/R)
  x <- as.data.frame(t(t(sum.fun(df[,c('R','S','H','HR')],95))))
for(i in 1:3){x[,i] <- as.integer(x[,i])}  
  x[,4] <- round(x[,4],3)
  names(x) <- c('Run','Escapement','Harvest','Harvest Rate')
#  x <- summary(df[,c('R','S','H','HR')])  # Remove year 
#  colnames(x) <- c('Run','Escapement','Harvest','Harvest Rate')
  return(x)
  }
  },rownames=TRUE)

plt_hist.run <- reactive({
  u <- unit()
  if(input$dataType == "Run"){
  df <- data()[,c(1:3)]
  names(df) <-c('Yr','S','R')
  df$H <- with(df,(R-S))
  df$HR <- with(df,H/R)
  p1 <- gg_hist(df,S,u,df$S)+xlab(paste('Escapement',mult(u)))
  p2 <- gg_hist(df,R,u,df$R)+xlab(paste('Run',mult(u)))
  p3 <- gg_hist(df,H,u,df$H)+xlab(paste('Harvest',mult(u)))
  p4 <- gg_hist(df,HR,1,df$HR)+xlab(paste('Harvest Rate',mult(1)))
  p4 <- plot_grid(p2,p1,p3,p4,nrow=1)
  return(p4)
  }
})

output$Plt_hist.run <- renderPlot({plt_hist.run()})



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Panel 2: Bayesian Model:  Create JAG data and model-----   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  UI output RE -------------------------------------------------------------  
output$re <- renderUI({  
  if(input$dataType== "S-R" & sum(names(sr.data()) %in% 'cv_E')==1){
  checkboxInput(inputId="RE", "Measurement Error Model", FALSE)
  } 
 })

RE <- reactive({
      if(input$dataType=='Run'& (!is.null(run.cv()))){
      out <- ifelse(input$SS=="Measurement Error Model",TRUE,FALSE)
      } else if(input$dataType=='S-R'& sum(names(sr.data()) %in% 'cv_E')==1){
        out <- input$RE   
      } else {
      out <- FALSE  
      }
      return(out)
      })

##  UI output SS -------------------------------------------------------------  
output$ss <- renderUI({  
if(input$dataType=='Run' & (!is.null(run.cv()))){
  radioButtons(inputId="SS","Model Options",
               choices=c("Standard","Measurement Error Model","State-Space Model"),
               selected = "Standard")
    } 
  })

SS <- reactive({
  if(input$dataType=='Run'& (!is.null(run.cv()))){
  out <- ifelse(input$SS=="State-Space Model",TRUE,FALSE)
  } else {out <- FALSE}
  return(out)
  })

## hyper --- Create data set for Priors -------------------------
PR <- reactive({ifelse(isTRUE(input$Priors),TRUE,FALSE)})

observeEvent(input$Priors,{
        if(isFALSE(input$Priors)){
            updateSliderInput(session,'lnalpha',value = c(0,5))
            updateSliderInput(session,'beta',value = c(0,5))
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
  if(input$Model=='Ricker'){rk<-1} else {rk<-0}
  if(input$Model=='Beverton-Holt'){bh<-1} else {bh<-0}
  if(input$add =='ar1'){ar1 <- 1} else {ar1 <- 0}
  if(input$add =='kf'){kf <- 1} else {kf <- 0}

# Condition: 1  State-Space model 
if(SS()){
# Extract data     
  dat <- cbind(run.out(),run.cv())
# trim data 
# Create trimmed year   
  trimyear <- seq(ss.year()[1],ss.year()[2])    
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
               fage=input$rage[1], 
               lage=input$rage[2],
               p_age=multage,
               efn=rowSums(multage),
               H.obs=dat$N-dat$S,
               N.obs=dat$N,
               S.obs= dat$S,
               tau.log.N=1/(log(dat$cv_N^2+1)),
               tau.log.H=1/(log(dat$cv_H^2+1)),
               tau.log.S=1/(log(dat$cv_E^2+1)),
               rk = rk,
               bh = bh,
               ar1 = ar1,
               kf = kf,
               d=d
          )
   } else{
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
 out <-list(nyrs=nyrs, S=S, R=R,d=d,rk=rk,bh=bh,ar1=ar1,kf=kf)
# Analysis is Measurement Error Model   
 if(RE()){
    lnSm <- mean(log(S))
    tau.lnSm <- 1/var(log(S))
    out <-list(nyrs=nyrs,S=S,tau.log.S=1/(log(x$cv_E^2+1)),lnSm=lnSm, tau.lnSm = tau.lnSm,R=R,d=d,rk=rk,bh=bh,ar1=ar1,kf=kf)} 
  } 
  
  out <- append(out,hyper()) 
  return(out)  
  })
 

# Select Bayes model 
Bayesmodel <- reactive({model_select(input$Model,input$add,SS(),RE())})

# Model Name 
model.name <- reactive({
  st <- ifelse(input$add=="kf",paste0(input$alphai),'')
  add <- ifelse(input$add=="ar1","AR1",ifelse(input$add=="kf","TVA","ST"))
  ss <- ifelse(SS(),'SS','')
  yrs <- paste0(input$sryears[1],'-',input$sryears[2])
  model <- paste(input$Model,ss,add,st,yrs,sep='_')
  return(model)
 })


model.title <- reactive({
  st <- ifelse(input$add=="kf",paste('TVA selected periods',input$alphai),'')
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  txt <- HTML(paste(paste('Model:',input$Model,add),
                  paste('Brood Year:',input$sryears[1],'-',input$sryears[2]),
                  paste(ifelse(SS(),'State-Space Model','')),
                  paste(st),
                  sep = '<br/>'))
    return(txt)
 })

output$model.1 <- renderText(model.title())


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
run.JAGS <- reactive({sim()$output})
JAGS.In <-  reactive(({sim()$input}))

# Summary output
JAGS.sum <- reactive({
             data <- run.JAGS()$BUGSoutput$summary
             parameters <- row.names(data)
             data <- data.frame(cbind(parameters,data))
             return(data)
             })


# Posterior  output 
MCMC <- reactive({as.mcmc(run.JAGS())})
mcdata <- reactive(as.data.frame(as.matrix(MCMC())))

output$download.mc <- downloadHandler(
  filename = function(){
    paste0('MCMCdata_', model.name(),'_', Sys.Date(),'.csv')  
        },
    content = function(file) {
      write.csv(mcdata(), file,row.names = FALSE,na='')  
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

output$BayesSum <- renderPrint({ print(run.JAGS()) })

### Plt_trace --------- Trace and density plot ---------------------------------
output$Plt_trace <- renderPlot({
  if(isTRUE(input$BayesMCMC)){
   mcmc <- mcmcdata()
   } else {
    mcmc <- MCMC()
   }
  pars <- c('lnalpha','beta')
  if(input$add=='ar1') {pars <- c(pars,'phi')}
  par(mfrow=c(2,4))
  plot(mcmc[,pars],auto.layout=FALSE)
  })



#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  EXTRACT RE data ----
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### RE.post Posterior for Measuremen Error model ----
RE.post <- reactive({
  if(RE()){
    nyrs <- Bayesdata()$nyrs 
    if(isTRUE(input$BayesMCMC)){
    mcmc <- mcmcdata()
    } else {
    mcmc <- as.data.frame(as.matrix(MCMC()))
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
    mcmc <- as.data.frame(as.matrix(MCMC()))
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
  if(isTRUE(SS())){  
    nyrs <- Bayesdata()$nyrs 
    nages <- Bayesdata()$nages 
    fage <- Bayesdata()$fage
    lage <- Bayesdata()$lage
    dat <- SS.post()
    trimyear <- seq(ss.year()[1],ss.year()[2])
    Ryear <- seq(ss.year()[1]-lage,ss.year()[2]-fage)
# model predicted S     
    S <- sum.ci(dat$S,90)
    S$Year <- trimyear
# model predicted N
    N <- sum.ci(dat$N,90)
    N$Year <- trimyear
# model predicted H    
    H <- sum.ci(dat$N-dat$S,90)
    H$Year <- trimyear
# model predicted R        
    R <- sum.ci(dat$R,90)
    R$Year <- Ryear
# model predicted run age comp    
    p <-list()
    for(i in 1:nages){
        p[[i]] <- sum.ci(dat$p.age[[i]],90)
        p[[i]]$Year <- trimyear
        p[[i]]$Age <- paste('Age',fage+i-1)
    }   
# model predicted brood age comp    
    q <-list()
    for(i in 1:nages){
        q[[i]] <- sum.ci(dat$q.age[[i]],90)
        q[[i]]$Year <- Ryear
        q[[i]]$Age <- paste('Age',fage+i-1)
              } 
    out <- list(N=N,S=S,H=H,R=R,p.age = p,q.age=q) 
  }
 })



#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3: SR Model Analyses ----  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### SR.post.0: Create SR parameters: alpha, beta, Seq, Smsy, Umsy, Smax, Sgen-------
 SR.post.0 <- reactive({
  D <- as.numeric(Bayesdata()$d)
# Read mcmc data
  if(isTRUE(input$BayesMCMC)){
   mcmc <- mcmcdata()
   } else {
   mcmc <- (as.matrix(MCMC()))
   }
   post <- sim.out(mcmc,d=D,add=input$add,model=input$Model,model.br=Bayesmodel()$model.br)
   post <-  Id.outliers(post,input$target)
  return(post)
  })

### SR.post: Outlier removed --------
 SR.post <- reactive({
   if(isTRUE(input$Remove_out)){
   SR.post.0()[which(is.na(SR.post.0()$Remove)),]
   } else {SR.post.0()}
  })

### Tbl_mcmcdata output mcmc data ----------------------------------------------
output$Tbl_mcmcdata <- renderDT({datatable(SR.post.0(),rownames = FALSE)}) 

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
  lnalphai <- SR.post()[,parname]
# Mean lnalphai  
  lnalphai.mm <- apply(lnalphai,2,mean)
# Get 95% CI  
  cil <- apply(lnalphai,2,function(x) quantile(x, 0.025))
  ciu <- apply(lnalphai,2,function(x) quantile(x, 0.975))
# Calculate STARS using STARS function 
   names(lnalphai.mm) <- year
   stars <- stars(lnalphai.mm, L=5, p=0.05,  h=2, AR1red="est", prewhitening = F)  
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


## Plt_lnalphai  Plot time variant lnalpha--------------------------------------
plt_lnalphai <- reactive({
  if(input$add=='kf'){
# Extract mean lnalpha for each year 
   xa <- lnalphais()$lnalphai
# Mean overall lnalpha  
  lnalpha <- mean(xa$lnalphai.m)
# Calculate STARS
# Plot figures   
  out <- ggplot(data=xa)+ggtitle('time-varying lnalpha')+theme(legend.title = element_blank())+
# CI bounds   
  geom_ribbon(aes(x = year, ymin = cil, ymax = ciu), 
              linetype = 0, color = 'grey',alpha = 0.1) + 
  # Mean line    
  geom_line(aes(x = year, y = lnalphai.m)) +
  # Star    
  geom_line(aes(x = year, y = star),color= 4) +
  #
  geom_hline(yintercept = lnalpha, linetype = "solid",color='red',linewidth=1.25) +
  # Scale 
  scale_y_continuous(expand=c(0.05,0),oob=oob_keep)+
  scale_x_continuous(expand=c(0,0.5),n.breaks = 10)+
  xlab('Year') + ylab('lnalpha')
  return(out)   
  } 
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
SR.post.i0 <- reactive({
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
    lnalphai <- as.matrix(SR.post.0()[,parname])
    post <- SR.post.0()
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
  return(post)
    }
 })

### SR.post.i  Outlier removed -------------------------------------------------
SR.post.i <- reactive({
   if(isTRUE(input$Remove_out)){
 SR.post.i0()[which(is.na(SR.post.i0()$Remove)),]
   } else {SR.post.i0()}
})

# sumpost  SR Parameter Summaries output------------------------------------- 
sumbayes <- reactive({
  ci <- input$CIB
  if(input$target =='me'){
  parname <-c('alpha.c','lnalpha.c','beta','Seq.c','Smsy.c','Umsy.c','Sgen.c')
  if(input$add=='ar1'){parname <-c('alpha.c','lnalpha.c','beta','phi','Seq.c','Smsy.c','Umsy.c','Sgen.c')}
  } else {
  parname <-c('alpha','lnalpha','beta','Seq','Smsy','Umsy','Sgen')    
  if(input$add=='ar1'){parname <-c('alpha','lnalpha','beta','phi','Seq','Smsy','Umsy','Sgen')}
  }
  if(input$Model == 'Ricker'){parname <- c(parname,'Smax')}
  if(input$add =='kf') {
    req(input$alphai)
    if(input$alphai != 'None'){
    out <- t(t(sum.fun(SR.post.i()[,parname],ci)))  
    } else{
    out <- t(t(sum.fun(SR.post()[,parname],ci)))
    } 
    } else {
    out <- t(t(sum.fun(SR.post()[,parname],ci)))
    }   
   out <- as.data.frame(out)
  if(input$add=='ar1'){
   out[,c(1:4,7)] <- round(out[,c(1:4,7)],3)
   out[,c(5,6,8)] <- round(out[,c(5,6,8)],0)
#   out[,c(5)] <- as.integer(out[,c(5)])
#   out[,c(6)] <- as.integer(out[,c(6)])
#   out[,c(8)] <- as.integer(out[,c(8)])
  } else{
   out[,c(1:3,6)] <- round(out[,c(1:3,6)],3)
#   out[,c(4,5,7)] <- as.integer(out[,c(4,5,7)])
   out[,c(4)] <- as.integer(out[,c(4)])
   out[,c(5)] <- as.integer(out[,c(5)])
   out[,c(7)] <- as.integer(out[,c(7)])
  }
 if(input$Model == 'Ricker'){
    out[,length(parname)] <- as.integer(out[,length(parname)])
  }
 # Rename beta 
  names(out)[3] <- paste0('beta x10^(-',Bayesdata()$d,')')
  return(out)
 })

# print out sumpost 
output$sumpost <- renderTable({sumbayes()},rownames = TRUE,digits=3)


#' -------------------------------------------
Ref_data <- reactive({
  req(input$add)
  par1 <- c('alpha','lnalpha')
  par2 <- c('Seq','Smsy','Umsy','Sgen')
  if(input$target =='me'){
  parname <- c(paste0(par1,'.c'),'beta',paste0(par2,'.c'))
  if(input$add=='ar1'){parname <-c(paste0(par1,'.c'),'beta','phi',paste0(par2,'.c'))}
  } else {
  parname <- c(par1,'beta',par2)   
  if(input$add=='ar1'){parname <-c(par1,'beta','phi',par2)}
 }
  if(input$Model == 'Ricker'){parname <- c(parname,'Smax')}
  
  if(input$add =='kf'){
     if(input$alphai != 'None'){
    out <-  as.data.frame(SR.post.i0()[,c(parname,'Remove')])
      }
     else {
      out <- SR.post.0()[,c(parname,'Remove')] 
     }
  } else {
        out <- SR.post.0()[,c(parname,'Remove')]      
  } 
  return(out)
  })

#output$Tbl_Ref <- DT::renderDT({Ref_data()}) 
output$Tbl_Ref <- renderDT({
  ob.page <- melt(brood.p(),id.vars='b.Year',variable.name='Age',value.name='ob.p')
  ob.page$Age <- paste('Age',substr(ob.page$Age,6,7))
  return(datatable(ob.page,rownames = FALSE))
  }) 


Ref_data_sum <- reactive ({
 dat <-data.frame(table(Ref_data()$Remove,useNA='ifany')) 
 names(dat) <- c('code','n')
 dat$Pct <- round(100*dat$n/sum(dat$n),1)
 dat$code <- as.integer(as.character(dat$code))
 note <- c('beta Negative','Outlier beta','Outlier lnalpha')
 code <- c(1,2,3)
 note <- data.frame(code,note)
 dat <- merge(dat,note, by =c('code'),all=TRUE)
 dat$code <- as.integer(dat$code)
 return(dat)
})

output$Tbl_Ref_sum <- renderTable({
   Ref_data_sum()
   },na="") 

         
#downloadServer('Ref',Ref_data(), paste0('Sumdata_', model.name(),'_', Sys.Date()))

output$download.sum <- downloadHandler(
  filename = function(){
    paste0('Sumdata_', model.name(),'_', Sys.Date(),'.csv')  
    },
    content = function(file) {
      write.csv(as.data.frame(Ref_data()), file,row.names = FALSE,na='')  
    }
)


# Plt_hist.mc --- SR Parameters Density plots ----------------------------------
plt_hist.mc <- reactive({
  D <- Bayesdata()$d
  if(input$add=='ar1'){ar1<- TRUE} else {ar1<- FALSE}
  if(input$add =='kf') {
     req(input$alphai)
    if(input$alphai != 'None'){
   p1 <- plot_density_gg(SR.post.i(),D,ar1,model=input$Model,target=input$target)
  } else {
   p1 <- plot_density_gg(SR.post(),D,ar1,model=input$Model,target=input$target)    
  }
  }else{
   p1 <- plot_density_gg(SR.post(),D,ar1,model=input$Model,target=input$target)      
  }
#--- Plot output----------------------------------------------------------------  
  return(p1)  
 })

output$Plt_hist.mc <- renderPlot({plt_hist.mc()})

#'==============================================================================
#  Create SR Model Predictions -----    
#'==============================================================================
#'------------------------------------------------------------------------------
## SR.pred1 Bayesian Model Prediction ---------------------------------------
#'------------------------------------------------------------------------------
SR.pred1 <-reactive({
  srmodel <- Bayesmodel()$model
#'--------- Extract MCMC SR Model Parameters -----------------------------------
  D <- Bayesdata()$d
#'--------- Determine model S length -------------------------------------------
  Seq <- quantile(SR.post()$Seq,0.9)   # Extract 90 percentile Seq
  # Extract max spawner
  max.s <- ceiling(max(Seq,max(sr.data.0()$S))/(10^D))*(10^D)
  unit.list <- c(10^(0:10),5*10^(0:9))
  picker <-  function(x, viable_numbers) {
  max(viable_numbers[viable_numbers < x])
  }
  S <- seq(0,max.s, picker(max.s/200,unit.list)) 
  if(input$add == 'kf') {
    req(input$alphai)
    if(input$alphai == 'None'){
    out <- SR.pred.sim(SR.post(),D,S,srmodel,input$add)
    } else {
    out <- SR.pred.sim(SR.post.i(),D,S,srmodel,input$add)     
    }
  } else {
    out <- SR.pred.sim(SR.post(),D,S,srmodel,input$add)     
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
    beta <- mean(SR.post()$beta)
    sigma <- mean(SR.post()$sigma)
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

base.pl <- reactive({
  u <- as.numeric(unit())
  xp <- sr.data.0()
  xp2 <- sr.data()
  SRp <- SRp()
#  maxS <- round(max(SRp$S))
  maxR <- round(1.25*max(xp$R,na.rm=TRUE))
  minY <- round(min(SRp$Rl-SRp$S,na.rm=TRUE))
  maxY <- round(1.25*max(xp$R-xp$S,na.rm=TRUE))
#  maxlnRS  <- (1.25*max(log(xp$R/xp$S)))
#  minlnRS  <- ifelse(min(log(xp$R/xp$S))<0,1.25*min(log(xp$R/xp$S)),0.75*min(log(xp$R/xp$S)))
#---  Basic SR plot ------------------------------------------------------------
  gsr <- ggplot()+
# Original data    
  geom_point(data = xp, aes(x = S, y = R), color = "gray", size = 3) +
# Trimmed data
  geom_point(data = xp2, aes(x = S, y = R), color = "black", size = 3) +
# 1:1 line
  geom_abline(intercept = 0, slope = 1, linetype = "solid",color='red',linewidth=0.8) +
# predicted Median
  geom_line(data = SRp, aes(x = S, y = RS.md), color = "black", linewidth = 0.8) +
# predicted Mean    
  geom_line(data = SRp, aes(x = S, y = RS.me), color = "black", linewidth = 0.8,linetype = 2) +
  scale_x_continuous(expand=c(0, 0), limits=c(0, NA), 
                     labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, maxR), 
                     labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
  labs(x=paste('Spawner',mult(u)),y=paste('Recruit',mult(u)))
  
#'-------------------------------------------------------------------------------
#---  Basic Yield plot ---------------------------------------------------------
  gsy <- ggplot()+
# Original data    
  geom_point(data = xp, aes(x = S, y = (R-S)), color = "gray", size = 3) +
# Trimmed data
  geom_point(data = xp2, aes(x = S, y = (R-S)), color = "black", size = 3) +
# predicted Median
  geom_line(data = SRp, aes(x = S, y = (RS.md-S)), color = "black", linewidth = 0.8) +
# predicted Mean    
  geom_line(data = SRp, aes(x = S, y = (RS.me-S)), color = "black", linewidth = 0.8,linetype = 2) +
  geom_hline(yintercept = 0, linetype = "solid",color=1,size=1.0) +
  scale_x_continuous(expand=c(0, 0), limits=c(0, NA), 
                     labels = label_number(scale = 1 /u), n.breaks = 10,oob=oob_keep) +
  scale_y_continuous(expand=c(0, 0), limits=c(minY, maxY),
                     labels = label_number(scale = 1 /u), n.breaks = 10,oob=oob_keep)+
   labs(x=paste('Spawner',mult(u)),y=paste('',mult(u)))
  
#---  Basic lnSR plot ---------------------------------------------------------
  glnRS <- ggplot()+
# Original data    
  geom_point(data = xp, aes(x = S, y = log(R/S)), color = "gray", size = 3) +
# Trimmed data
  geom_point(data = xp2, aes(x = S, y = log(R/S)), color = "black", size = 3) +
# predicted Median
  geom_line(data = SRp, aes(x = S, y = log(RS.md/S)), color = "black", linewidth = 0.8) +
# predicted Mean    
  geom_line(data = SRp, aes(x = S, y = log(RS.me/S)), color = "black", linewidth = 0.8,linetype = 2) +
  geom_hline(yintercept = 0, linetype = "solid",color=1,size=1.0) +
  scale_x_continuous(expand=c(0, 0), limits=c(0, NA), 
                     labels = label_number(scale = 1 /u), n.breaks = 10,oob=oob_keep) +
  scale_y_continuous(expand=c(0, 0), 
                     n.breaks = 10,oob=oob_keep)+
   labs(x=paste('Spawner',mult(u)),y=paste('ln(R/S'))
  
#'-------------------------------------------------------------------------------
#----  Plots Output ------------------------------------------------------------  
  return(list(base.r=gsr,base.y=gsy,base.ln = glnRS))
})


#'===============================================================================
#  Standard SR and Yield Plots with CI-PI
#'=============================================================================== 
# srplot.g : plot goal range in SR plot-----------------------------------------
base.sr <- reactive({
  SRp <- SRp()
# Plot base SR Plot with CI  ---------------------------------------------------  
  p1 <- base.pl()$base.r
  p1 <- p1 +
  geom_ribbon(data = SRp, aes(x = S, ymin = Rl, ymax = Ru), linetype = 0, 
              color = 'grey',alpha = 0.1) +  # Add PI ranbe 
  geom_line(data = SRp, aes(x = S, y = Ru.p), color = "grey", linetype = 2) +    
  geom_line(data = SRp, aes(x = S, y = Rl.p), color = "grey", linetype = 2) 
  
# Plot base Yield Plot with CI --------------------------------------------------
  p2 <- base.pl()$base.y
  p2 <- p2 +
  geom_ribbon(data = SRp, aes(x = S, ymin = (Rl-S), ymax = (Ru-S)), 
              linetype = 0,color = 'grey',alpha=0.1) +  
  geom_line(data = SRp, aes(x = S, y = (Ru.p-S)), color = "grey", linetype = 2) +    
  geom_line(data = SRp, aes(x = S, y = (Rl.p-S)), color = "grey", linetype = 2) 
  
# Plot base lnRS Plot with CI --------------------------------------------------
  p3 <- base.pl()$base.ln
  p3 <- p3 +
  geom_ribbon(data = SRp, aes(x = S, ymin = log(Rl/S), ymax = log(Ru/S)), 
              linetype = 0,color = 'grey',alpha=0.1) +  
  geom_line(data = SRp, aes(x = S, y = log(Ru.p/S)), color = "grey", linetype = 2) +    
  geom_line(data = SRp, aes(x = S, y = log(Rl.p/S)), color = "grey", linetype = 2)   
  
  return(list(base.r=p1,base.y=p2,base.ln = p3))  
})

base.r <- reactive({base.sr()$base.r})
base.y <- reactive({base.sr()$base.y})
base.ln <- reactive({base.sr()$base.ln})


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Panel 4: SR Model Analyses plots ----  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sr.title <- reactive({
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  yrs <- paste('Brood Year:',min(sr.data()$Yr),'-',max(sr.data()$Yr))
  model <- paste0('SR Model: ',input$Model,' ',add)
  title <- paste(model,yrs)
  return(title)
})
# br.data : SEQ, SMSY, SMAX, SGEN ++++++++++++++++++++++++++++++++++++++++++++++
br.data <- reactive({
    # SMSY Calc 
    srmodel <- Bayesmodel()$model
    model.br <- Bayesmodel()$model.br
    lnalpha <- mean(SR.post()$lnalpha)
    lnalpha.c <- mean(SR.post()$lnalpha.c)
    beta <- mean(SR.post()$beta)
    D <- Bayesdata()$d
  if(input$add=='kf')
     {
    if(input$alphai != 'None') {
      lnalpha <- mean(SR.post.i()$lnalpha) 
      lnalpha.c <- mean(SR.post.i()$lnalpha.c) 
      beta <- mean(SR.post.i()$beta)
     } 
   } 
# Get unique alpha 
    bref <- model.br(lnalpha,beta,D)
    bref.c <- model.br(lnalpha.c,beta,D)
    Smsy <- ifelse(input$target=='me',bref.c$Smsy,bref$Smsy)
    Sgen <- ifelse(input$target=='me',bref.c$Sgen,bref$Sgen)
    Smax <- bref$Smax
    
out  <- data.frame(x = c(Smsy, Smax, Sgen), 
                          label = c(
                            paste("Smsy:", format(round(Smsy, 0))),
                            paste("Smax:", format(round(Smax, 0))),
                            paste("Sgen:", format(round(Sgen, 0)))
                          ),
                          linetype = c("dashed","dotted","dotdash")) 
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

## plt_SRY  ------- SR Yield plots -----------------------------------------------------
plt_SRY <- reactive({
  SRp <- SRp()   # Predicted 
  xp <- sr.data()  # SR plot datat
  dyear <- floor(xp$Yr/10)*10   # Decades 
  labels <- unique(dyear)
  colp <- 1:length(labels)+1 # Decades color pallette    
  
# Create data for TVA model 
if(input$add=='kf'){
    df.tva <- kf.data()$df
    nstar <- kf.data()$nstar
    ny <- kf.data()$ny
    star <- kf.data()$star    
    dyear <- rep(c(1:nstar),ny)     
    colp <- 1:nstar+1
    labels <- star$txt
   }   

# CI or PI data ----------------------------------------------------------------
    if (input$Li =='credible') {
    lwr <- SRp$Rl
    upr <- SRp$Ru
    }
  else {
    # Prediction Interval
    lwr <- SRp$Rl.p
    upr <- SRp$Ru.p
   }
  legend_data <- br.data()
  
#' Draw Base SR   and YLD Plot -------------------------------------------------
  p1 <- base.pl()$base.r+ ggtitle(sr.title())+theme(legend.title = element_blank())
  p2 <- base.pl()$base.y+ ggtitle(sr.title())+theme(legend.title = element_blank())
  p3 <- base.pl()$base.ln+ ggtitle(sr.title())+theme(legend.title = element_blank())
  
#'---------- TVA lines  ---------------------------------------------------------  
if(input$add=='kf'){
  p1 <- p1+
    geom_point(data = xp, aes(x = S, y = R, color=as.factor(dyear)), size = 3)+    
    geom_line(data = df.tva, aes(x = S, y = R,color=star), 
              linetype = "dashed", linewidth = 0.7) +
    scale_color_manual(values=c(1:nstar+1),labels=star$txt)  
#'..............................................................................    
  p2 <- p2+
    geom_point(data = xp, aes(x = S, y = R-S, color=as.factor(dyear)), size = 3)+    
        geom_line(data = df.tva, aes(x = S, y = R-S,color=star), 
              linetype = "dashed", linewidth = 0.7) +
    scale_color_manual(values=c(1:nstar+1),labels=star$txt)  
#'..............................................................................  
  p3 <- p3+
    geom_point(data = xp, aes(x = S, y = log(R/S), color=as.factor(dyear)), size = 3)+    
    geom_line(data = df.tva, aes(x = S, y = log(R/S),color=star), 
              linetype = "dashed", linewidth = 0.7) +
    scale_color_manual(values=c(1:nstar+1),labels=star$txt) 
  }  

#'-------- Add Interval -------------------------------------------------------- 
  if(isTRUE(input$show.int)){
      # credible Interval 
  p1 <- p1 +
    geom_ribbon(data = SRp, aes(x = S, ymin = lwr, ymax = upr), 
                  linetype = 0, color = 'grey',alpha = 0.1 )+
    geom_line(data = SRp, aes(x = S, y = Ru.p), color = "grey", linetype = 2) +    
    geom_line(data = SRp, aes(x = S, y = Rl.p), color = "grey", linetype = 2)  
  p2 <- p2 +
    geom_ribbon(data = SRp, aes(x = S, ymin = (lwr-S), ymax = (upr-S)), 
                  linetype = 0, color = 'grey',alpha = 0.1 )+
    geom_line(data = SRp, aes(x = S, y = Ru.p-S), color = "grey", linetype = 2) +    
    geom_line(data = SRp, aes(x = S, y = Rl.p-S), color = "grey", linetype = 2)
  p3 <- p3 +
    geom_ribbon(data = SRp, aes(x = S, ymin = log(lwr/S), ymax = log(upr/S)), 
                  linetype = 0, color = 'grey',alpha = 0.1 )+
    geom_line(data = SRp, aes(x = S, y = log(Ru.p/S)), color = "grey", linetype = 2) +    
    geom_line(data = SRp, aes(x = S, y = log(Rl.p/S)), color = "grey", linetype = 2)
    } # End Interval
#'------------------------------------------------------------------------------

#'------ Add Years -------------------------------------------------------------
if(isTRUE(input$show.points)) {
  p1 <- p1 + 
    geom_point(data = xp, aes(x = S, y = R, color = as.factor(dyear)), size = 3) + 
    scale_color_manual(values = colp,labels=labels)+
    geom_text_repel(data = xp, aes(x = S, y = R, label = as.character(Yr)), 
                    size = 5, nudge_x = 0.5, nudge_y = -0.1, max.overlaps = Inf)
#'..............................................................................    
  p2 <- p2 + 
    geom_point(data = xp, aes(x = S, y = R-S, color = as.factor(dyear)), size = 3) +  
    geom_text_repel(data = xp, aes(x = S, y = R-S, label = as.character(Yr)), 
                    size = 5, nudge_x = 0.5, nudge_y = -0.1, max.overlaps = Inf)+
    scale_color_manual(values = colp,labels=labels)    
#'..............................................................................    
  p3 <- p3 + 
    geom_point(data = xp, aes(x = S, y = log(R/S), color = as.factor(dyear)), size = 3) +
    geom_text_repel(data = xp, aes(x = S, y = log(R/S), label = as.character(Yr)), 
                    size = 5, nudge_x = 0.5, nudge_y = -0.1, max.overlaps = Inf)+
    scale_color_manual(values = colp,labels=labels)  
    
        }  # End show.points  
#'------------------------------------------------------------------------------  

  if(RE()){
    S <- RE.post()
    S <- data.frame(S,xp)
    cols <- as.factor(as.numeric(as.factor(dyear))+1)
  p1 <- p1 + 
    geom_point(data = S, aes(x = exp(mean), y = R), color = cols, size = 3) + 
    geom_segment(data=S,aes(x=exp(uci),y=R,yend=R,xend =exp(lci)),
                 color=cols,alpha=0.6,linewidth=0.5)    
#    }
#  if(isTRUE(input$show.arrows)){     
#    df2a <- data.frame(S2=exp(S$mean), S1 = S,R2=R, R1 = R)
#    p1 <- p1+
#      geom_segment(data=S,aes(x=S,y=R,xend =exp(mean), yend = R),
#                 arrow=arrow(angle = 15, type = "closed"), size= 0.2)
#    }
  }  
  
  
#'-- Add SS Elements  ----------------------------------------------------------
if(SS()){
    cidata <- data.frame(sr.data.ci())
    cidata <- cidata[which(cidata$Yr %in% xp$Yr),]
    S <- SS.post.sum()$S
    S <- S[which(S$Year %in% xp$Yr),]
    R <- SS.post.sum()$R
    R <- R[which(R$Year %in% xp$Yr),] 
# Color Palette    
    
    if(isTRUE(input$show.points)|input$add=='kf'){
    cols <- as.factor(as.numeric(as.factor(dyear))+1)
    } else {cols <- 'gray30'}

    
    df.SS <- data.frame(S=S$mean, Suci=S$uci,Slci=S$lci,R=R$mean,Ruci=R$uci,Rlci=R$lci)
# Observed CI   
  if(isTRUE(input$show.ob.se)){    
    p1 <- p1 +
      geom_segment(data=cidata,aes(x=S,y=Ruci, xend=S,yend = Rlci),
                 color=cols,alpha=0.6,linewidth=0.5) +
      geom_segment(data=cidata,aes(x=Suci,y=R,yend=R,xend =Slci),
                 color=cols,alpha=0.6,linewidth=0.5)
#................................................................................   
    p2 <- p2 +
      geom_segment(data=cidata,aes(x=S,y=(Ruci-S), xend=S,yend = (Rlci-S)),
                 color=cols,alpha=0.6,linewidth=0.5) +
      geom_segment(data=cidata,aes(x=Suci,y=R-S,yend=R-S,xend =Slci),
                 color=cols,alpha=0.6,linewidth=0.5)   
#................................................................................   
    p3 <- p3 +
      geom_segment(data=cidata,aes(x=S,y=log(Ruci/S), xend=S,yend = log(Rlci/S)),
                 color=cols,alpha=0.6,linewidth=0.5) +
      geom_segment(data=cidata,aes(x=Suci,y=log(R/S),yend=log(R/S),xend =Slci),
                 color=cols,alpha=0.6,linewidth=0.5)      
  }
    
# Model predicted Point and CI   
  if(isTRUE(input$show.ss.point)){  
    p1 <- p1 +
      geom_point(data=df.SS,aes(x=S,y=R),color=cols,alpha=0.6,size=3)+
      geom_segment(data=df.SS,aes(x=S,y=Ruci, xend=S,yend = Rlci),
                 color=cols,alpha=0.6,linewidth=0.5) +
      geom_segment(data=df.SS,aes(x=Suci,y=R,yend=R,xend =Slci),
                 color=cols,alpha=0.6,linewidth=0.5)
#...............................................................................  
    p2 <- p2 +
      geom_point(data=df.SS,aes(x=S,y=(R-S)),color=cols,alpha=0.6,size=3)+
      geom_segment(data=df.SS,aes(x=S,y=(Ruci-S), xend=S,yend =(Rlci-S)),
                 color=cols,alpha=0.6,linewidth=0.5) +
      geom_segment(data=df.SS,aes(x=Suci,y=R-S,yend=R-S,xend =Slci),
                 color=cols,alpha=0.6,linewidth=0.5)
#...............................................................................  
    p3 <- p3 +
      geom_point(data=df.SS,aes(x=S,y=log(R/S)),color=cols,alpha=0.6,size=3)+
      geom_segment(data=df.SS,aes(x=S,y=log(Ruci/S), xend=S,yend =log(Rlci/S)),
                 color=cols,alpha=0.6,linewidth=0.5) +
      geom_segment(data=df.SS,aes(x=Suci,y=log(R/S),yend=log(R/S),xend =Slci),
                 color=cols,alpha=0.6,linewidth=0.5)    
    }
  if(isTRUE(input$show.arrows)){  
    df2a <- data.frame(S2=S$mean, S1 = xp$S,R2=R$mean, R1 = xp$R)
    p1 <- p1+ geom_point(data=df2a,aes(x=S2,y=R2),size=4,color=cols)+
      geom_segment(data=df2a,aes(x=S1,y=R1,xend =S2, yend = R2),
                 arrow=arrow(angle = 15, type = "closed"), size= 0.5)
#...............................................................................
    p2 <- p2+ geom_point(data=df2a,aes(x=S2,y=R2-S2),size=4,color=cols)+
      geom_segment(data=df2a,aes(x=S1,y=(R1-S1),xend=S2, yend = (R2-S2)),
                 arrow=arrow(angle = 15, type = "closed"), size= 0.5)
#...............................................................................
    p3 <- p3+ geom_point(data=df2a,aes(x=S2,y=log(R2/S2)),size=4,color=cols)+
      geom_segment(data=df2a,aes(x=S1,y=log(R1/S1),xend=S2, yend = log(R2/S2)),
                 arrow=arrow(angle = 15, type = "closed"), size= 0.5)    
    }
  }
  
#'---------- Add Smsy ----------------------------------------------------------
  if(input$show.smsy==TRUE){
    p1 <- p1 +
      geom_vline(data = legend_data[grepl("Smsy", legend_data$label),], 
               aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")  
#...............................................................................
    p2 <- p2 +
      geom_vline(data = legend_data[grepl("Smsy", legend_data$label),], 
               aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")  
  }
  
# Add Smax       
  if(input$show.smax==TRUE & input$Model == 'Ricker') {
    p1 <- p1 +
      geom_vline(data = legend_data[grepl("Smax", legend_data$label),], 
               aes(xintercept = x, linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")
#...............................................................................  
    p2 <- p2 +
      geom_vline(data = legend_data[grepl("Smax", legend_data$label),], 
               aes(xintercept = x, linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")
   }
#  Add Sgen  
    if(input$show.sgen==TRUE){
    p1 <- p1 +
      geom_vline(data = legend_data[grepl("Sgen", legend_data$label),], 
               aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")
#...............................................................................  
    p2<- p2 +
      geom_vline(data = legend_data[grepl("Sgen", legend_data$label),], 
               aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")
     }
  return(list(pltSR=p1, pltYD=p2,pltLN = p3))  
 })

## Plt_SR ------ SR plot -------------------------------------------------------
plt_SR <- reactive({plt_SRY()$pltSR})
output$Plt_SR <- renderPlot({plt_SR()})

output$Txt_SR <- renderText({
paste0("Spawner-recruit curve (solid: median, dash: mean).  Gray shade and dashed line indicate ", input$CI,"% Bayesian ",input$Li,"and Prediction interval.")
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
output$Plt_yield <- renderPlot({plt_yield()})

output$Txt_YD <- renderText({
paste0("Spawner-Yield curve (Solid:median, dash:mean).  Gray shade and dashed line indicate ", input$CI,"% Bayesian ",input$Li,"and Prediction interval.")
  })

## Plt_lnRS --- Plot ln(R/S) vs S ------------------------------------------
plt_lnRS <- reactive({plt_SRY()$pltLN})
output$Plt_lnRS <- renderPlot({plt_lnRS()})

output$Txt_lnRS <- renderText({
paste0("ln(R/S) plot (Solid:median, dash:mean). Gray shade and dashed line indicate ", input$CI,"% Bayesian ",input$Li,"and Prediction interval.")
  })


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
  lnalpha <-SR.post()$lnalpha
  beta <- SR.post()$beta
  phi <- SR.post()$phi
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
  lnalphai <- as.matrix(SR.post()[,parname])
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
        RD2[i,1] <- RD[i,1]-phi[i]*SR.post()$e0[i]
         }  
      } # End else
#' Create residuals ------------------------------------------------------------  
  out <- list(RD=RD,RD2=RD2)
  return(out)
  })  

### Plt_residual --- Plot Residual Plot ----------------------------------------
plt_residual <- reactive({
  year <- sr.data()$Yr
  if(input$add=='ar1'){
    resid <-SR.resid()$RD2  
   } else {
    resid <- SR.resid()$RD
   } # End if ar1
  resid.m <- apply(resid,2,mean)
  cil <- apply(resid,2,function(x) quantile(x, 0.025))
  ciu <- apply(resid,2,function(x) quantile(x, 0.975))
# Create data frmae
  df<- data.frame(year,resid.m,cil,ciu)
    
p1 <- ggplot(data=df)+ 
# CI bounds   
  geom_ribbon(aes(x = year, ymin = cil, ymax = ciu), 
              linetype = 0, color = 'grey',alpha = 0.1) + 
# horizontal line
  geom_hline(yintercept = 0, linetype = "solid",color=1,linewidth =1.0) +
  # Residual    
  geom_line(aes(x = year, y = resid.m), color = "red", linewidth = 0.8) +
# Scale y
  scale_y_continuous(expand=c(0.05, 0), limits=c(min(cil), max(ciu)))+
  scale_x_continuous(expand=c(0, 0.5),n.breaks = 10)+
  xlab('Year') + ylab('Residual')
 return(p1)   
  })

output$Plt_residual <- renderPlot({plt_residual()})

## Plt_predict --- Plot Predicted Plot ------------------------------------------
plt_predict <- reactive({
  year <- sr.data()$Yr
  resid <-SR.resid()$RD
  R <- log(sr.data()$R)
  R.m <- R - apply(resid,2,mean)
  cil <- R - apply(resid,2,function(x) quantile(x, 0.025))
  ciu <- R - apply(resid,2,function(x) quantile(x, 0.975))
# Create data frmae
    df <- data.frame(year,R,R.m,cil,ciu)
p1 <- ggplot(data=df)+  theme(legend.title = element_blank())+
# CI bounds   
  geom_ribbon(aes(x = year, ymin = cil, ymax = ciu), 
              linetype = 0, color = 'grey',alpha = 0.1) + 
  # Mean Recruit    
#  geom_line(aes(x = year, y = R.m,color='Predicted'), size = 0.8) +
  # Mean Recruit    
#  geom_point(aes(x = year, y = R,color = "Observed"), size = 3) +
#  scale_color_manual(values=c('red','black'),
#        guide=guide_legend(override.aes=list(linetype=c('blank','solid'),shape=c(19,NA))))+
  # Mean Recruit    
  geom_line(aes(x = year, y = R.m,linetype='Predicted'), linewidth = 0.8) +
  # Mean Recruit    
  geom_point(aes(x = year, y = R,fill = "Observed"),color='red', size = 3) +
  # Scale 
  scale_y_continuous(expand=c(0.15,0), limits=c(min(cil,R), max(ciu,R)),oob=oob_keep)+
  scale_x_continuous(expand=c(0,0.5),n.breaks = 10)+
  xlab('Year') + ylab('ln(Recruit)')
 return(p1)   
})

output$Plt_predict <- renderPlot({plt_predict()})


#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  State-Space Model diagnoses ----    
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Plt_SS Age  plot Age Comp  -----------------------------------------
plt_SS_Age <- eventReactive(isTRUE(SS()),{
# Import run data 
  run <- run.out()
# trim data 
  # Create trimmed year   
  trimyear <- seq(ss.year()[1],ss.year()[2])    
# Extract Year matches trimyear  
  run <- run[run$Year %in% trimyear,]
# Extract observed run age comp   
#  page <- proportions(as.matrix(run[,substr(names(run),1,1) =='A']),margin = 1) 
  ob.page <- melt(run[,-(2:3)],id.vars='Year',variable.name='Age',value.name='ob.p')
  ob.page$Age <- paste('Age',substr(ob.page$Age,2,3))
# Import predicted age comp   
  dat.ssp <- data.frame(do.call(rbind,SS.post.sum()$p.age))
  dat.ssp <- merge(dat.ssp,ob.page,by =c('Age','Year'))
  nages <- Bayesdata()$nages 
  fage <- Bayesdata()$fage 

  
# Plot  
# Create data frmae

p1 <- ggplot(dat.ssp)+theme(legend.title = element_blank())+
# CI bounds   
  geom_ribbon(aes(x = Year, ymin = lci, ymax = uci), 
              linetype = 0, color = 'grey',alpha = 0.1) + 
  geom_line(aes(x = Year, y = median),linetype=1, linewidth = 0.8) +
  geom_line(aes(x = Year, y = mean),linetype=2, linewidth = 0.8) +
  geom_point(aes(x = Year, y = ob.p),color='red', size = 3) +
  facet_rep_wrap(~Age,scales='free_y',ncol=1)+
#  scale_y_continuous(expand=c(0.05,0), limits=~c(with(dat.ssp, min(lci,ob.p), max(uci,ob.p))))+
#  scale_x_continuous(expand=c(0,0.5), limits=~c(with(dat.ssp,min(Year), max(Year))),n.breaks = 10)+
  xlab('Year') + ylab('Proportion')
  return(p1)  
 })

output$Plt_SS_Age <- renderPlot({plt_SS_Age()})

#### Plt_SS plot Run, Escapement, harvest--------------------------------------  
plt_SS <- eventReactive(isTRUE(SS()),{
    u <- unit()
    run <- run.out()
    # Create trimmed year   
    trimyear <- seq(ss.year()[1],ss.year()[2])    
    # Extract Year matches trimyear  
    run <- run[run$Year %in% trimyear,]
    N <- SS.post.sum()$N
    S <- SS.post.sum()$S
    H <- SS.post.sum()$H
    
pred.plot <- function(df) {
    p1 <- ggplot(df)+ theme(legend.title = element_blank())+  
# CI bounds   
  geom_ribbon(aes(x = Year, ymin = lci, ymax = uci), 
              linetype = 0, color = 'grey',alpha = 0.1) + 
  geom_line(aes(x = Year, y = median),linetype='solid', linewidth = 0.8) +
  geom_line(aes(x = Year, y = mean),linetype='dashed', linewidth = 0.8) +
  scale_x_continuous(expand=c(0,0.5), limits=with(df,c(min(Year), max(Year))),n.breaks = 10)+
  xlab('Year')      
  return(p1)
  }  
      
#' plot Run 
  p1 <- pred.plot(N)+
   geom_point(data=run,aes(x = Year, y = N),color='red', size = 3)+
   scale_y_continuous(expand=c(0,0), limits=c(0, max(N$uci,run$N)),
                      labels = label_number(scale = 1 /u))+
   ylab(paste('Run x',mult(u)))
#'  Plot S  
  p2 <- pred.plot(S)+
   geom_point(data=run,aes(x = Year, y = S),color='red', size = 3)+
   scale_y_continuous(expand=c(0,0), limits=c(0, max(S$uci,run$S)),
                      labels = label_number(scale = 1 /u))+
   ylab(paste('Spawner x',mult(u)))

#' plot harvest   
  p3 <- pred.plot(H)+
   geom_point(data=run,aes(x = Year, y = N-S),color='red', size = 3)+
   scale_y_continuous(expand=c(0,0), limits=c(0, max(H$uci,run$N-run$S)),
                      labels = label_number(scale = 1 /u))+
   ylab(paste('Harvest x',mult(u)))
   return(plot_grid(p1, p2, p3,ncol = 1, align = "v")) 
 })

output$Plt_SS <- renderPlot({plt_SS()})

### Plt_SS_BAge Maturity plot Brood Age comp  ----------------------------------
plt_SS_BAge <- eventReactive(isTRUE(SS()),{
  # Extract Year matches trimyear  
  nages <- Bayesdata()$nages 
  fage <- Bayesdata()$fage 
  ob.page <- melt(brood.p(),id.vars='b.Year',variable.name='Age',value.name='ob.p')
  ob.page$Age <- paste('Age',substr(ob.page$Age,6,7))
  dat.ssp <- data.frame(do.call(rbind,SS.post.sum()$q.age))
  dat.ssp <- merge(dat.ssp,ob.page,by.y =c('Age','b.Year'),by.x =c('Age','Year'), all=TRUE)
p1 <- ggplot(dat.ssp)+theme(legend.title = element_blank())+
# CI bounds   
  geom_ribbon(aes(x = Year, ymin = lci, ymax = uci), 
              linetype = 0, color = 'grey',alpha = 0.1) + 
  geom_line(aes(x = Year, y = median),linetype=1, linewidth = 0.8) +
  geom_line(aes(x = Year, y = mean),linetype=2, linewidth = 0.8) +
  geom_point(aes(x = Year, y = ob.p),color='red', size = 3) +
  facet_rep_wrap(~Age,scales='free_y',ncol=1)+
#  scale_y_continuous(expand=c(0.05,0), limits=~c(with(dat.ssp, min(lci,ob.p), max(uci,ob.p))))+
#  scale_x_continuous(expand=c(0,0.5), limits=~c(with(dat.ssp,min(Year), max(Year))),n.breaks = 10)+
  xlab('Brood Year') + ylab('Proportion')
  return(p1)  
})

output$Plt_SS_BAge <- renderPlot({plt_SS_BAge()})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Panel 5: Escapement Goal Analyses---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'===============================================================================
##  Profile Analyses---- 
#'===============================================================================


#'-------------------------------------------------------------------------------
###  Smsy Goal Analyses: This produces 
#  EG.Smsy, EG.Smsy.st, SA.BEG, p.msy, p.msy.t
#'-------------------------------------------------------------------------------
smsyprof <- ProfileServer("smsy",SR.pred,'MSY',unit)
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
  smaxprof <- ProfileServer("smax",SR.pred,'Rmax',unit)  
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
##  Custom Profile Range Analyses----   
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
#output$Tbl_prof <- renderDT(datatable(data.frame(S=SR.pred()$S,Yield_gl_sim()),rownames = FALSE))    


# downloadData ---- profile download -------------------------------------------
#  downloadServer('Prof',T.Prof(),paste0('Profile_', model.name(),'_', Sys.Date()))
output$download.prof <- downloadHandler(
  filename = function(){
     paste0('Profile_', model.name(),'_', Sys.Date(),'.csv')
    },
    content = function(file) {
      write.csv(as.data.frame(T.Prof()), file,row.names = FALSE,na='')  
    }
)

# downloadData ---- Profsummary download -------------------------------------------
#  downloadServer('ProfSum',Prof.sum(),paste0('Profile_summary', model.name(),'_', Sys.Date()))
output$download.profSum <- downloadHandler(
  filename = function(){
     paste0('Profile_summary_', model.name(),'_', Sys.Date(),'.csv')
    },
    content = function(file) {
      write.csv(as.data.frame(Prof.sum()), file,row.names = FALSE,na='')  
    }
)
  
#'===============================================================================
#  Smsy-Smax Goal Analyses Output 
#'===============================================================================
plot_range <- function(out,baseplot,sr.data,SRp,Srange1,Srange2=c(NA,NA),goal=NA)
  {
  xp <- sr.data
  SRp <- SRp()
  p1 <- baseplot
  if(out=='r'){
  ymin <-c(0,0)
  ymax <-c(1.1*max(xp$R),1.1*max(xp$R))
  }
  if(out=='y'){
    ymin <-c(min(SRp$Rl-SRp$S),min(SRp$Rl-SRp$S))
    ymax <-c(1.1*max(xp$R-xp$S),1.1*max(xp$R-xp$S))
  }
  Srange1 <- as.numeric(Srange1)
  Srange2 <- as.numeric(Srange2)
  p1 <- p1+annotate('rect', xmin = Srange1[1], xmax = Srange1[2], ymin = -Inf, ymax = Inf, alpha=0.1, fill=3)
  # Plot escapement goal range 
  if(!is.na(sum(Srange2))) {
  p1 <- p1+ annotate('rect', xmin = Srange2[1], xmax = Srange2[2], ymin = -Inf, ymax = Inf, alpha=0.1, fill=4)
  }
  if(!is.na(goal)) {
  p1 <- p1 + geom_hline(yintercept = goal, color = 2,size =1.2 )
  }  
  return(p1)
  }

### Yield and Recruit Plot -------------------------------------------------------
output$Plt_rec.pg <- renderPlot({
  plot_range('r',base.r(),sr.data(),SRp(),Srange1=EG.Smsy()$S.Range,Srange2=EG.Smax()$S.Range)
  })
  
output$Plt_yield.pg <- renderPlot({
  plot_range('y',base.y(),sr.data(),SRp(),Srange1=EG.Smsy()$S.Range,Srange2=EG.Smax()$S.Range)
  })
  
### Txt_Srange.smsy -------- Smsy goal output ------------------------------------
  output$Txt_Srange.smsy <-renderUI({ SA.BEG() })
### Txt_Srange.smax -------- Smax Goal range output table ------------------------
  output$Txt_Srange.smax <-renderUI({ SM.BEG() })


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Target Yield and Recruit based Escapement Goal ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
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

#'---- UI Output-----------------------------------------------------------------
output$minYield = renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(sr.data()$R-sr.data()$S,0.5)/u
  numericInput("y1", paste("Target Yield",mult), value=v[1],min=0, step=v[2])
})

Yield_gl <- reactive({
  prof_sim(SR.pred()$S,SR.pred()$Y,SR.pred()$Y.p,input$y1,input$y1p/100,unit(),input$target)
  })

##---- Optimum Mean and annual Yield Profile Plot -------------------------------
output$Plt_yield.prof <- renderPlot({
  u <- unit()
   p1 <-Yield_gl()$fig+labs(title = 'Target Yield Profile Analyses')
   return(p1)
    }) 

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
  plot_range('y',base.y(),sr.data(),SRp(),BEG.p,NA,input$y1*u)
    }) 

Yield_gl_sim <- reactive({
  u <- unit()
  mc.Ypm <- Prob.calc(SR.pred()$Y,input$y1*u)
  mc.Ypa <- Prob.calc(SR.pred()$Y.p,input$y1*u)
  out <- data.frame(mc.Yp=mc.Ypm, mc.Ypa=mc.Ypa)
  return(out)
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Recruitment  based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#---- UI Output----------------------------------------------------------------------
output$minRec <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(sr.data()$R,0.5)/u
  numericInput("r1", paste("Target Recruit",mult), value=v[1],min=0, step=v[2])
})

#'------------------------------------------------------------------------------
#  Calculate probability that intercepts profile 
#'------------------------------------------------------------------------------
Rec_gl <- reactive({
  prof_sim(SR.pred()$S,SR.pred()$R,SR.pred()$R.p,input$r1,input$r1p/100,unit(),input$target)

})

# Recruit Plot 
output$Plt_rec.gl <- renderPlot({
  u <- unit()
  BEG.p <- Rec_gl()$range$b.p
  plot_range('r',base.r(),sr.data(),SRp(),BEG.p,NA,input$r1*u)
  }) 

#  Minimum Recruit Profile Plot 
output$Plt_rec.prof <- renderPlot({
   p1 <-Rec_gl()$fig+labs(title = 'Target Recruit Profile Analyses')
   return(p1)
  })  


# Optimum Recruit Profile Escapement Goal 
output$Txt_Rec_gl <-renderText({
  BEG.p <- Rec_gl()$range$b.p
  tex <- ifelse(input$target =='me','Mean','Median')
  paste(tex,'target range:',BEG.p[1],'-',BEG.p[2])
  })

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
plt_cg_prof <- reactive({
  SS <- c(input$lg,input$ug)*unit()
# Smsy prof (p1) and Smax prof(p2)
    # Inport basic profile plot 
  p1 <- plt.msy.prof()
  p2 <- plt.max.prof()
  if((SS[1]==SS[2])){
  p1 <- p1+ geom_vline(xintercept ==SS[1],color=3,linewidth =3)
  p2 <- p2+ geom_vline(xintercept ==SS[1],color=4,linewidth =3)
    } else {
  p1 <- p1+
  annotate('rect', xmin = SS[1], xmax = SS[2], ymin = 0, ymax = 1, alpha=0.1, fill=3) 
  p2 <- p2+
  annotate('rect', xmin = SS[1], xmax = SS[2], ymin = 0, ymax = 1, alpha=0.1, fill=4) 
  }
 return(list(msy=p1,max=p2))
})

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
Yield_EG <- reactive({
  u <- as.numeric(unit())
  yg <- input$yg*u
  Y.p <-CG_sim()$Y.p
  Y.p <- Y.p[Y.p < quantile(Y.p,0.995)]
    if (input$target =='me'){
    Y.m <-CG_sim()$Y.c
    m <- mean(Y.p)
     } else {
    Y.m <-CG_sim()$Y
    m <- median(Y.p)
    }
  # plot density
  leg.tx <- c('Annual',ifelse(input$target =='me','Mean','Median'))
  p1 <- mult.den.plt.gg(Y.p,Y.m,'Yield',leg.tx[2],u)+    
   geom_vline(xintercept = m, color = 4)+
   geom_vline(xintercept = yg, color = 2)
  return(p1)  
 })

output$Plt_Yield_EG <- renderPlot({Yield_EG()}) 

#'-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#  SR model based CI and PI
#'-----------------------------------------------------------------------
Rec_EG <- reactive({
  u <-  unit()
  rg <- input$rg*u
  # Annual estimate   
  R.p <-CG_sim()$R.p
  R.p <- R.p[R.p < quantile(R.p,0.995)]  
  if (input$target =='me'){
    R.m <-CG_sim()$R.c
    m <- mean(R.p)
  } else {
    R.m <-CG_sim()$R
    m <- median(R.p)
  }
  # plot density
  leg.tx <- c('Annual',ifelse(input$target =='me','Mean','Median'))
  p1 <- mult.den.plt.gg(R.p,R.m,'Recruit',leg.tx[2],u)+    
   geom_vline(xintercept = m, color = 4)+
   geom_vline(xintercept = rg, color = 2)
  return(p1)
  }) 

output$Plt_Rec_EG <- renderPlot({Rec_EG()}) 

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
  p1 <- plot_range('r',base.r(),sr.data(),SRp(),NA,c(input$lg,input$ug)*u,NA)
  if(input$lg==input$ug){
    p1 <- p1+geom_vline(xintercept = input$lg*u, color =3, linewidth=3) 
    }
   p1 <- p1+geom_hline(yintercept = input$rg*u, color=2,linewidth=2 )  
  return(p1)
}) 

# Yield Plot 
output$Plt_yield.cg <- renderPlot({
  u <-unit()
  p1 <- plot_range('y',base.y(),sr.data(),SRp(),NA,c(input$lg,input$ug)*u,NA)
  if(input$lg==input$ug){
    p1 <- p1+geom_vline(xintercept = input$lg*u, color =3, linewidth=3) 
    }
   p1 <- p1+geom_hline(yintercept = input$yg*u, color=2,linewidth=2 )  
  return(p1)
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
  # Put old value hwere 
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
#  Percentile Analyses ---
#'===============================================================================
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

#'===============================================================================
#  Risk Analyses ----
#'===============================================================================
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
plt_risk <- reactive({riskout$Plt_risk_gg()})
output$Plt_risk <- renderPlot({plt_risk()}) 

# Txt_dwtest: Durbin-Watson test results ----------------------------------------
output$Txt_dwtest <- renderPrint({ Risk_sim_base()$dw})

# Risk Model  ---------------------------------------------
output$Txt_Risk_Model <-renderText({Risk_sim_base()$md})

# Risk Target: Print out Target ------------------------------------------------
output$Txt_Risk <-renderUI({ Risk_sim()$txt })

# Plt_risk2 --------------------------------------------------------------------
Plt_risk2 <- reactive({
   riskout$Plt_risk2_gg()
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
  lnalpha <-SR.post()$lnalpha
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
  brood.p <- brood.p()[,-1]
# Maturity schedule is a random sample of observed brood proportion 
  e.p  <- brood.p[sample(dim(brood.p)[1],nyrs+lage,replace = TRUE),]    
# Prediction and Implementation Error are normal   
  e.pred <- (rnorm(nyrs,1,input$spred/100))
  e.imp <- (rnorm(nyrs,1,input$simpH/100))
# Determine Fishery opening target: FT  
  out <- list(S0=S0,e.pred = e.pred, e.imp = e.imp, e.p = e.p)
  return(out) 
})

output$foo2 <- renderDT({data.frame(MSE.int()$e.p)})  
#'-------------------------------------------------------------------------------
# Txt_Strategy:  Explanation of Strategy 
#'-------------------------------------------------------------------------------  

#'-------------------------------------------------------------------------------
# msesim:  Run simulation and output 
#'-------------------------------------------------------------------------------  
msesim <- eventReactive(input$SimRun,{
#'-------------------------------------------------------------------------------
#  Import Error Data 
#'------------------------------------------------------------------------------- # Initial Spawners  
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
  j <- nsims[k]
#'-------------------------------------------------------------------------------
# Set SR simulation parameters
#'-------------------------------------------------------------------------------  
# Set lnalpha.i  (random walk when alpha is time varying) ----------------------
# Constant alpha
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
  sim.out[[k]] <- MSE.sim(srmodel,lnalpha.i,sbeta,S0,D,e.Rec,e.p,e.pred,e.imp,FT,mH)  
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

#'-------------------------------------------------------------------------------
# Plt_mse:  MSE summary plot
#'---------------------------''----------------------------------------------------  
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

output$sim.lnalphai <- renderPlot({
  if (input$add=='kf'){
    
  }
  })


#output$Tbl_mse <- renderDT({aggregate(length~rep+crit,FUN=sum, data=MSE.sum())})  
#output$Tbl_mse <- renderDT({MSE.sum()}) 

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
      write.csv(as.data.frame(t(do.call("cbind", msesim()))), file,row.names = FALSE,na='')  
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
    file.copy('report_officedown.Rmd',tempReport,overwrite = TRUE)
    file.copy('template.docx',tempTemplate,overwrite = TRUE) 
    file.copy('run_table.Rmd',tempTable,overwrite = TRUE) 
    file.copy('SR_Equations.Rmd',tempEQ,overwrite = TRUE) 
    params <- list(
      add = input$add,
      data = input$dataType,
      JAGS = run.JAGS()$BUGSoutput,
      Tbl_sum = sumbayes(),
      plt_runesc = plt_runesc(),
      plt_bsr = plt_srt(),
      plt_sr = plt_SR(),
      plt_yld = plt_yield(),
      plt_pred = plt_predict(),
      plt_lnRS = plt_lnRS(),
      plt_resid = plt_residual(),
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
