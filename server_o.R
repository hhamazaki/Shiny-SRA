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
#theme_set(theme_simple())
#palette("Okabe-Ito")  
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
    if(isTRUE(PR())){
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
    if(isTRUE(PR())){
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
  output$Tbl_data <- renderDataTable({data()})  

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
  run.out <-  reactive({
     if(input$dataType== "Run"){
      agedata <- make.age(data(),input$rage[1], input$rage[2],input$combage)
      data <- cbind(data()[,1:3],agedata)
      names(data)[1:3] <- c('Year','S','N')
      return(data)
       }
    })
##### Tbl_data.run ----- show run table ----------------------------------------
output$Tbl_data.run <- renderDataTable({round(run.out(),2)}) 
  
 
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
output$Tbl_data.brood <- renderDataTable({round(brood.out()$brood,0)}) 
#output$Tbl_data.brood <- renderDataTable({round(brood.var.out()$brood,0)})   

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
  req(input$dataType)
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

# output$test <- renderText({ss.year()})

  
### Reactive e.data --- final dataset used for percentile risk -----------------
  e.data <- reactive({ cut.data(e.data.0(),input$sryears) })

  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Plot SR data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
plot_runesc <- function(dat,u){
     par(yaxs='i',bty='l')
     plot(R/u~Yr,data=dat,type='l',ylim=c(0,with(dat,max(R,S,na.rm =TRUE)/u)),xlab='',ylab='')
     lines(S/u~Yr,data=dat,lty=2)
 }


# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") ----------
plt_runesc <- reactive({
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
 
 
output$Plt_runesc <- renderPlot({plt_runesc()})

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


##### Plt_srt Plot SR time series ---------------------------------------
plt_srt <- reactive({
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
  out <-recordPlot()  
  return(out)      
})

output$Plt_srt <- renderPlot({plt_srt()})

##### Plt_lnsrt Plot ln SR time series -------------------------------------
plt_lnsrt <- reactive({
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
  out <-recordPlot()  
  return(out)      
})

output$Plt_lnsrt <- renderPlot({plt_lnsrt})


##### Plt_srtime: Plot SR time series ------------------------------------
plt_srtime <- reactive({
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
  out <-recordPlot()  
  return(out)      
})
output$Plt_srtime <- renderPlot({plt_srtime})


##### Txt_sum.data:  summary sr data summary Output ----------------------------
output$Txt_sum.data <- renderPrint({
  if(input$dataType != "Escapement Only"){
  dat <- sr.data()
  dat$Y <- dat$R-dat$S
  }else{
  dat <- e.data()    
  }
  summary(dat[,c('R','S','Y')])  # Remove year 
  })

#### Plt_hist.sry:  sr data histogranm ------------------------------------------- 
plt_hist.sry <- reactive({
  if(input$dataType != "Escapement Only"){
  par(mfrow=c(1,3))
  x <- sr.data()
  x$Y <- x$R-x$S
  hist(x$S,main='',xlab='Spawnter')
  hist(x$R,main='',xlab='Recruit')
  hist(x$Y,main='',xlab='Yield') 
  } else {
    x <- e.data()  
    hist(x$S,main='',xlab='Escapement')    
  }
  out <-recordPlot()  
  return(out)      
})

output$Plt_hist.sry <- renderPlot({plt_hist.sry()})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Panel 2: Bayesian Model:  Create JAG data and model-----   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  UI output ME -------------------------------------------------------------  
output$re <- renderUI({  
  if(input$dataType== "S-R" & sum(names(sr.data()) %in% 'cv_E')==1){
    radioButtons(inputId="ME","Model Options",
               choices=c("Standard","Measurement Error (ME) Model"),
               selected = "Standard")  
     } 
 })

ME <- reactive({
      if(input$dataType=='Run'){
      out <- ifelse(input$SS=="Measurement Error (ME) Model",TRUE,FALSE)
      } else if(input$dataType=='S-R'){
      out <- ifelse(input$ME=="Measurement Error (ME) Model",TRUE,FALSE)
      } else {
      out <- FALSE  
      }
      return(out)
      })

##  UI output SS -------------------------------------------------------------  
output$ss <- renderUI({  
if(input$dataType=='Run' & (!is.null(run.cv()))){
  radioButtons(inputId="SS","Model Options",
               choices=c("Standard","Measurement Error (ME) Model","State-Space (SS) Model"),
               selected = "Standard")
    } 
  })

SS <- reactive({
  if(input$dataType=='Run'){
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
#' Standard SR Model data input ------------------------------------------------
  if(isFALSE(SS())){
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
#' Measurement Error SR Model data input ---------------------------------------
    if(isTRUE(ME())){
    lnSm <- mean(log(S))
    tau.lnSm <- 1/var(log(S))
    out <-list(nyrs=nyrs,S=S,tau.log.S=1/(log(x$cv_E^2+1)),
               lnSm=lnSm, tau.lnSm = tau.lnSm,R=R,d=d,rk=rk,
               bh=bh,ar1=ar1,kf=kf)} 
  } else {
#' State-Space SR Model data input ---------------------------------------------
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
   }
  out <- append(out,hyper()) 
  return(out)  
  })
# 

# Select Bayes model 
Bayesmodel <- reactive({model_select(input$Model,input$add,SS(),ME())})

# Model Name 
model.name <- reactive({
  st <- ifelse(input$add=="kf",paste0(input$alphai),'')
  add <- ifelse(input$add=="ar1","AR1",ifelse(input$add=="kf","TVA","ST"))
  ss <- ifelse(isTRUE(SS()),'SS','')
  me <- ifelse(ME(),'ME','')
  yrs <- paste0(input$sryears[1],'-',input$sryears[2])
  model <- paste(input$Model,me,ss,add,st,yrs,sep='_')
  return(model)
 })


model.title <- reactive({
  st <- ifelse(input$add=="kf",paste('TVA selected periods',input$alphai),'')
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  txt <- HTML(paste(paste('Model:',input$Model,add),
                  paste('Brood Year:',input$sryears[1],'-',input$sryears[2]),
                  paste(ifelse(ME(),'Measurment Error Model','')),
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
JAGS.In <- reactive(({sim()$input}))

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
#    paste0('MCMCdata_', model.name(),'_', Sys.Date(),'.csv')  
    paste0('MCMCdata_', model.name(),'_', Sys.Date(),'.txt')  
        },
    content = function(file) {
#      write.csv(mcdata(), file,row.names = FALSE,na='')  
       dput(run.JAGS(), file)  
          }
  )


# MCMC data file reading module  
mcmcdata <- dataInputServer("mcmc.in")


#'------------------------------------------------------------------------------
## Extract JAG results ----
#'------------------------------------------------------------------------------
### BayesSum ------------ Output MCMC sumaary ----------------------------------

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
##  EXTRACT SS data ----
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' function extract right 
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# function 
sum.ci <- function(x,ci){
  p <- (100-ci)/200
  lci <- apply(x,2, function(x) quantile(x,prob=p,na.rm=TRUE))
  uci <- apply(x,2, function(x) quantile(x,prob=1-p,na.rm=TRUE))
  mean <- apply(x,2,mean, na.rm=TRUE)
  median <- apply(x,2,median, na.rm=TRUE)
  out <- data.frame(mean=mean,median=median,lci=lci,uci=uci)
  return(out)
 }

### ME.post Posterior for ME model ----
ME.post <- reactive({
  if(ME()){
    nyrs <- Bayesdata()$nyrs 
    nages <- Bayesdata()$nages 
    fage <- Bayesdata()$fage 
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


### SS.post Posterior for SS ----
SS.post <- reactive({
  if(isTRUE(SS())){
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
    return(out)
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
    }   
# model predicted brood age comp    
    q <-list()
    for(i in 1:nages){
        q[[i]] <- sum.ci(dat$q.age[[i]],90)
        q[[i]]$Year <- Ryear
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
output$Tbl_mcmcdata <- renderDataTable({SR.post.0()}) 

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


## Plt_lnalphai  Plot time variant lnalpha--------------------------------------
plt_lnalphai <- reactive({
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
  out <-recordPlot()  
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
#   out[,c(5,6,8)] <- as.integer(out[,c(5,6,8)])
   out[,c(5)] <- as.integer(out[,c(5)])
   out[,c(6)] <- as.integer(out[,c(6)])
   out[,c(8)] <- as.integer(out[,c(8)])
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

output$Tbl_Ref <- renderDataTable({Ref_data()}) 
#output$foo <- renderTable({lnalphais()$cuty})

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
   plot_density(SR.post.i(),D,ar1,model=input$Model,target=input$target)
  } else {
   plot_density(SR.post(),D,ar1,model=input$Model,target=input$target)    
  }
  }else{
   plot_density(SR.post(),D,ar1,model=input$Model,target=input$target)      
  }
#--- Plot output----------------------------------------------------------------  
  out <-recordPlot()
  return(out)  
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
  if(input$add == 'kf') {
    req(input$alphai)
    if(input$alphai == 'None'){
    out <- SR.pred.sim(SR.post(),D,max.s,srmodel,input$add,input$lensim+1)
    } else {
    out <- SR.pred.sim(SR.post.i(),D,max.s,srmodel,input$add,input$lensim+1)     
    }
  } else {
    out <- SR.pred.sim(SR.post(),D,max.s,srmodel,input$add,input$lensim+1)     
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
#' ranges: Brush and get x y cordinate 
#ranges <- reactiveValues(x = NULL, y = NULL)
#observeEvent(input$plot1_dblclick, {
#    brush <- input$plot1_brush
#    if (!is.null(brush)) {
#      ranges$x <- c(brush$xmin, brush$xmax)
#      ranges$y <- c(brush$ymin, brush$ymax)

#    } else {
#      ranges$x <- NULL
#      ranges$y <- NULL
#    }
#  })

base.pl <- reactive({
  u <- as.numeric(unit())
  x <- sr.data.0()
  xp <- x/u
  SRp <- SRp()/u
  maxS <- round(max(SRp$S))
  maxR <- round(1.25*max(xp$R))
  minY <- round(min(SRp$Rl-SRp$S))
  maxY <- round(1.25*max(xp$R-xp$S))
#  if(is.null(ranges$x[1])){
#    ranges$x <- c(0,maxS)
#    ranges$y <- c(0,maxR)
#  }
  
#---  Basic SR plot ------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l',las=1)
  plot(R~S,data=xp,pch=19,col='gray',cex=1.5, 
       xlab=paste("Escapement",mult(u)),ylab=paste('Recruit',mult(u)),
       xlim=c(0,maxS),ylim=c(0,maxR)
#       xlim=ranges$x,ylim=ranges$y
       )
  x2 <- sr.data()
  xp2 <- x2/u
  points(R~S,data=xp2,pch=19,col=1,cex=1.5)
  
  # Plot 1:1 line 
  abline(0,1)
  # Add Predicted   
  lines(RS.md~S,data=SRp,col=1,lw=2)
  lines(RS.me~S,data=SRp,col=1,lw=2,lty=2)
  out1 <-recordPlot()
#'-------------------------------------------------------------------------------
#---  Basic Yield plot ---------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot((R-S)~S,data=xp,pch=19,col='gray', cex=1.5,
       xlab=paste("Escapement",mult(u)),ylab=paste('Yield',mult(u)),
       xlim=c(0,maxS),ylim=c(minY,maxY))
  points((R-S)~S,data=xp2,pch=19,col=1,cex=1.5)
  lines((RS.md-S)~S,data=SRp,col=1,lw=2)
  lines((RS.me-S)~S,data=SRp,col=1,lw=2,lty=2)
  abline(h=0)
#'-------------------------------------------------------------------------------
#----  Plots Output ------------------------------------------------------------  
  out2 <-recordPlot()  
  return(list(base.p=out1,base.py=out2))

})


base.p <- reactive({base.pl()$base.p})

base.py <- reactive({base.pl()$base.py})

#'===============================================================================
#  Standard SR and Yield Plots with CI-PI
#'=============================================================================== 
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
#Panel 4: SR Model Analyses plots ----  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## srplot ------- SR plot function ----------------------------------------------
plt_SR <- reactive({
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
  title(main = paste(model,yrs))  

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
  if(isTRUE(input$show.int)){
      polygon(c(SRp$S,rev(SRp$S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA)
    }  
  
#'-------- Time Varying Alpha ---------------------------------------------------  
  if(input$add=='kf'){
#'---------- Extract MCMC SR Model Parameters -----------------------------------
    star <- lnalphais()$cuty
    br <- sr.cut()$br
    br.c <- sr.cut()$br.c
    nstar <- sr.cut()$nstar
    if(input$target=='me'){
      for (i in 1:nstar){
        R <- sr.cut()$R.c[,i]        
        lines(SRp$S,R/u,lty=2,lwd=2,col=1+i)
        }
      }else{
    for (i in 1:nstar){
      R <- sr.cut()$R[,i]    
     lines(SRp$S,R/u,lty=2,lwd=2,col=1+i)
      }
    }
    tex <- star$txt 
    if(input$show.smsy==FALSE){
    legend('right',col=c(1:nstar+1),lwd=3,lty=2,legend=tex,box.lty=0)
    }
     if(isTRUE(input$show.points)){
    ny <- star$ny
    colp <- rep(c(1:nstar+1),ny)
    points(xp$S/u,xp$R/u,pch=19,cex=1.5,col=colp)
    pointLabel(xp$S/u,xp$R/u, labels=as.character(xp$Yr), cex = 1)
    }
  }
 
  # Add Years
  if(isTRUE(input$show.points)) {
      if(input$add!='kf'){
    points(xp$S/u,xp$R/u,pch=19,cex=1.5,col=colp)
    pointLabel(xp$S/u,xp$R/u, labels=as.character(xp$Yr), cex = 1)
    legend('topleft',col=unique(colp),legend=unique(dyear), pt.cex = 1.2,
           pch=19,box.lty=0)
      }
     }

  if(isTRUE(input$show.points)) { 
    cols <- character()
    for(i in 1:length(colp)){cols[i] <- tcol(colp[i],30) }
    } else {
    cols <- tcol('grey',30) 
    }
    
  if(isTRUE(ME())){
    S <- ME.post()
#    S <- S[which(S$Year %in% xp$Yr),]
#    points(exp(S$mean)/u, xp$R/u,pch=19,cex=1.5,col=cols)
#    arrows(xp$R/u,x0=exp(S$uci)/u,x1=exp(S$lci)/u,code=0,lty=2,lwd=1,col=cols)
    }
#  if(isTRUE(input$show.arrows)){      
#    arrows(xp$S/u,xp$R/u,S$mean/u,R$mean/u,lty=1,lwd=1,col=1,length = 0.1,cex=0.8)
#    }
#  }
  
  if(isTRUE(SS())){
        ci <- sr.data.ci() 
    S <- SS.post.sum()$S
    S <- S[which(S$Year %in% xp$Yr),]
    R <- SS.post.sum()$R
    R <- R[which(R$Year %in% xp$Yr),]        
  if(isTRUE(input$show.ob.se)){      
    arrows(ci$R/u,x0=ci$Suci/u,x1=ci$Slci/u,code=0,lty=2,lwd=1,col=cols)
    arrows(ci$S/u,y0=ci$Ruci/u,y1=ci$Rlci/u,code=0,lty=2,lwd=1,col=cols)
     }
  if(isTRUE(input$show.ss.point)){      
    points(S$mean/u,R$mean/u,pch=19,cex=1.5,col=cols)
    arrows(R$mean/u,x0=S$uci/u,x1=S$lci/u,code=0,lty=2,lwd=1,col=cols)
    arrows(S$mean/u,y0=R$uci/u,y1=R$lci/u,code=0,lty=2,lwd=1,col=cols)
    }
  if(isTRUE(input$show.arrows)){      
    arrows(xp$S/u,xp$R/u,S$mean/u,R$mean/u,lty=1,lwd=1,col=1,length = 0.1,cex=0.8)
    }
  }
  
#'-------------------------------------------------------------------------------
# SMSY Calc 
    srmodel <- Bayesmodel()$model
    model.br <- Bayesmodel()$model.br
    lnalpha <- mean(SR.post()$lnalpha)
    lnalpha.c <- mean(SR.post()$lnalpha.c)
    beta <- mean(SR.post()$beta)
    D <- Bayesdata()$d
# Get unique alpha 
    bref <- model.br(lnalpha,beta,D)
    bref.c <- model.br(lnalpha.c,beta,D)  
  
# Add Smsy
  t1 <- ''
  l1 <- 0
  if(input$show.smsy==TRUE){
    if(input$add=='kf'){
      if(input$target=='me'){
        kfSmsy <- c(bref.c$Smsy,br.c$Smsy)
      } else {
        kfSmsy <- c(bref$Smsy,br$Smsy)   
      }
      abline(v=kfSmsy/u,col=c(1:(nstar+1)),lty=2) 
      t1 <- paste('Smsy:',c('overall',star$txt),':',paste(round(kfSmsy)))
      } else {
    Smsy <- ifelse(input$target=='me',bref.c$Smsy,bref$Smsy)
    abline(v=Smsy/u,lty=2)
    t1 <- paste('Smsy:',round(Smsy,0))
      }
    l1 <- 2
    }
# Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE & input$Model == 'Ricker') {
    abline(v=bref$Smax/u,col=1,lty=3)
    t2 <- paste('Smax:',round(bref$Smax,0))
    l2 <- 3
   }
#  Add Sgen  
  t3 <- ''
  l3 <- 0
   if(input$show.sgen==TRUE){
    if(input$add=='kf'){
      if(input$target=='me'){
        kfSgen <- c(bref.c$Sgen.c,br.c$Sgen)
      }else{
        kfSgen <- c(bref$Sgen,br$Sgen)   
      }
      abline(v=kfSgen/u,col=c(1:(nstar+1)),lty=4) 
      t3 <- paste('Sgen:',c('overall',star$txt),':',paste(round(kfSgen)))
    } else {
      Sgen <- ifelse(input$target=='me',bref.c$Sgen,bref$Sgen)
      abline(v=Sgen/u,lty=4)
      t3 <- paste('Sgen:',round(Sgen,0))
    }
      l3 <- 4
   }
  legend('topright',c(t1,t2,t3),lty=c(rep(l1,length(t1)),l2,l3),
         lwd=2, col=c(1:(length(t1)),1),box.lty=0)  
  out <-recordPlot()
  return(out)  
  })

## Plt_SR ------ SR plot -------------------------------------------------------
output$Plt_SR <- renderPlot({plt_SR()})

output$Txt_SR <- renderText({
paste0("Spawner-recruit curve (solid: median, dash: mean).  Gray shade indicates ", input$CI,"% Bayesian ",input$Li," interval.")
  })

output$SRinfo <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("")
      paste0("S:", round(e$x, 2)," R:",round(e$y, 2))
    }
   xy_str(input$SR_click)
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
  
#    nearPoints(sr.data(), xvar="S",yvar="R", input$SR_click, addDist = TRUE)
  })


## Plt_yield -------- Yield plot ----------------------------------------------- 
plt_yield <- reactive({
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
  title(main = paste(model,yrs))
#'-------- Time Varying Alpha ---------------------------------------------------  
  if(input$add=='kf'){
#'---------- Extract MCMC SR Model Parameters -----------------------------------
    star <- lnalphais()$cuty
    br <- sr.cut()$br
    br.c <- sr.cut()$br.c
    nstar <- sr.cut()$nstar
    if(input$target=='me'){
      for (i in 1:nstar){
        R <- sr.cut()$R.c[,i]        
        lines(SRp$S,(R/u-SRp$S),lty=2,lwd=2,col=1+i)
        }
      }else{
    for (i in 1:nstar){
      R <- sr.cut()$R[,i]    
     lines(SRp$S,(R/u-SRp$S),lty=2,lwd=2,col=1+i)
      }
    }
    tex <- star$txt 
    if(input$show.smsy==FALSE){
    legend('right',col=c(1:nstar+1),lwd=3,lty=2,legend=tex,box.lty=0)
    }
  }
  # credible Interval 
  if (input$Li =='credible') {
    lwr <- with(SRp,Rl-S)
    upr <- with(SRp,Ru-S)
  }
  else {
    # Prediction Interval
    lwr <- with(SRp,Rl.p-S)
    upr <- with(SRp,Ru.p-S)
  }
  # Add CI    
  if(input$show.int==TRUE){
    polygon(c(SRp$S,rev(SRp$S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA)
  }
   
  if(input$show.points==TRUE) {
    with(xp,points(S/u,(R-S)/u,pch=19,cex=1.5,col=colp))
    legend('topleft',col=unique(colp),legend=unique(dyear), pt.cex = 1.5,
           pch=19,box.lty=0)
    with(xp,pointLabel(S/u,(R-S)/u, labels=as.character(Yr), cex= 1,col=1))
  }
  # Add Years
 if(isTRUE(input$show.points)) { 
    cols <- character()
    for(i in 1:length(colp)){cols[i] <- tcol(colp[i],30) }
    } else {
    cols <- tcol('grey',30) 
    }
    
  if(isTRUE(SS())){
    ci <- sr.data.ci() 
    S <- SS.post.sum()$S
    S <- subset(S,Year %in% xp$Yr)
    R <- SS.post.sum()$R
    R <- subset(R,Year %in% xp$Yr)       
 if(isTRUE(input$show.ob.se)){      
    arrows((ci$R-ci$S)/u,x0=ci$Suci/u,x1=ci$Slci/u,code=0,lty=2,lwd=1,col=cols)
    arrows(ci$S/u,y0=(ci$Ruci-ci$S)/u,y1=(ci$Rlci-ci$S)/u,code=0,lty=2,lwd=1,col=cols)
     }
  if(isTRUE(input$show.ss.point)){      
    points(S$mean/u,(R$mean-S$mean)/u,pch=19,cex=1.5,col=cols)
    arrows((R$mean-S$mean)/u,x0=S$uci/u,x1=S$lci/u,code=0,lty=2,lwd=1,col=cols)
    arrows(S$mean/u,y0=(R$uci-S$mean)/u,y1=(R$lci-S$mean)/u,code=0,lty=2,lwd=1,col=cols)
    }
  if(isTRUE(input$show.arrows)){      
    arrows(xp$S/u,(xp$R-xp$S)/u,S$mean/u,(R$mean-S$mean)/u,lty=1,lwd=1,col=1,length = 0.1,cex=0.8)
    }
    }
  
#'-------------------------------------------------------------------------------
# SMSY Calc 
    srmodel <- Bayesmodel()$model
    model.br <- Bayesmodel()$model.br
    lnalpha <- mean(SR.post()$lnalpha)
    lnalpha.c <- mean(SR.post()$lnalpha.c)
    beta <- mean(SR.post()$beta)
    D <- Bayesdata()$d
# Get unique alpha 
    bref <- model.br(lnalpha,beta,D)
    bref.c <- model.br(lnalpha.c,beta,D)  
  
# Add Smsy
  t1 <- ''
  l1 <- 0
  if(input$show.smsy==TRUE){
    if(input$add=='kf'){
      if(input$target=='me'){
        kfSmsy <- c(bref.c$Smsy,br.c$Smsy)
      } else {
        kfSmsy <- c(bref$Smsy,br$Smsy)   
      }
      abline(v=kfSmsy/u,col=c(1:(nstar+1)),lty=2) 
      t1 <- paste('Smsy:',c('overall',star$txt),':',paste(round(kfSmsy)))
      } else {
    Smsy <- ifelse(input$target=='me',bref.c$Smsy,bref$Smsy)
    abline(v=Smsy/u,lty=2)
    t1 <- paste('Smsy:',round(Smsy,0))
      }
    l1 <- 2
    }
# Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE & input$Model == 'Ricker') {
    abline(v=bref$Smax/u,col=1,lty=3)
    t2 <- paste('Smax:',round(bref$Smax,0))
    l2 <- 3
   }
#  Add Sgen  
  t3 <- ''
  l3 <- 0
   if(input$show.sgen==TRUE){
    if(input$add=='kf'){
      if(input$target=='me'){
        kfSgen <- c(bref.c$Sgen,br.c$Sgen)
      }else{
        kfSgen <- c(bref$Sgen,br$Sgen)   
      }
      abline(v=kfSgen/u,col=c(1:(nstar+1)),lty=4) 
      t3 <- paste('Sgen:',c('overall',star$txt),':',paste(round(kfSgen)))
    } else {
      Sgen <- ifelse(input$target=='me',bref.c$Sgen,bref$Sgen)
      abline(v=Sgen/u,lty=4)
      t3 <- paste('Sgen:',round(Sgen,0))
    }
      l3 <- 4
   }
  legend('topright',c(t1,t2,t3),lty=c(rep(l1,length(t1)),l2,l3),
         lwd=2, col=c(1:(length(t1)),1),box.lty=0)  
  out <-recordPlot()
  return(out)  
  })

output$Plt_yield <- renderPlot({plt_yield()})

output$Txt_YD <- renderText({
paste0("Spawner-Yield curve (Solid:median, dash:mean).  Gray shade indicates ", input$CI,"% Bayesian ",input$Li," interval.")
  })

# Output Model name   
#output$bypt <-renderText({
#  BEG.p <- round(b.YApg())
#  paste('Escapement Goal Range:',BEG.p[1],'-',BEG.p[2])
#})

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
  par(bty='l')
  plot(resid.m~year,xlab='Year',type='l',ylab='Residuals',lwd=2, main='Residual',
       ylim =c(min(cil),max(ciu)))
  abline(h=0)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
        out <-recordPlot()  
       return(out)   
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
  par(bty='l')
  plot(R.m~year,xlab='Year',type='l',lwd=2,ylab='ln(Recruit)',main='Recruit',
       ylim =c(min(cil,R),max(ciu,R)))
  points(R~year,pch=19,col=2)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  legend('topright',legend=c('Observed','Predicted '),pch=c(19,NA),lty=c(NA,1), col=c(2,1),box.lty=0)
  out<- recordPlot()
  return(out)
})

output$Plt_predict <- renderPlot({plt_predict()})

## Plt_lnRS --- Plot ln(R/S) vs S ------------------------------------------
plt_lnRS <- reactive({
  u <- as.numeric(unit())
  x <- sr.data.0()
  x2 <- sr.data()
  dyear <- floor(x2$Yr/10)*10
  colp <- (dyear-min(dyear))/10+1
  lnRS <- SR.pred()$lnRS
  lnRS.p <- SR.pred()$lnRS.p
  SA <- SR.pred()$S
  S <- SR.pred()$S/u
  maxS <- max(S)
  lnRS.m <- apply(lnRS,2,mean)
  cil <- apply(lnRS,2,function(x) quantile(x, 0.025,na.rm=TRUE))
  ciu <- apply(lnRS,2,function(x) quantile(x, 0.975,na.rm=TRUE))
  cil.p <- apply(lnRS.p,2,function(x) quantile(x, 0.025,na.rm=TRUE))
  ciu.p <- apply(lnRS.p,2,function(x) quantile(x, 0.975,na.rm=TRUE))

  par(xaxs='i',yaxs='i',bty='l')
  plot(x$S/u,log(x$R/x$S),pch=19,col='gray', cex=1.5,
       xlab=paste("Escapement",mult(u)),ylab=paste('ln(R/S)'),
       xlim=c(0,maxS),ylim=c(0.75*min(cil,na.rm=TRUE), 1.25*max(log(x$R/x$S))))
  points(x2$S/u,log(x2$R/x2$S),pch=19,col=1,cex=1.5)
  lines(S,lnRS.m,col=1,lw=2)
  polygon(c(S,rev(S)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  lines(S,cil.p,col='gray',lty=4,lw=2)
  lines(S,ciu.p,col='gray',lty=4,lw=2)
  if(input$add=='kf'){
#---------- Extract MCMC SR Model Parameters -----------------------------------
    star <- lnalphais()$cuty
    nstar <- sr.cut()$nstar
    for (i in 1:nstar){
      R <- sr.cut()$R[,i]    
     lines(S,log(R/SA),lty=2,lwd=2,col=1+i)
      }
    tex <- star$txt 
    if(input$show.smsy==FALSE){
    legend('topright',col=c(1:nstar+1),lwd=3,lty=2,legend=tex,box.lty=0)
    }
 if(isTRUE(input$show.points)){
        star <- lnalphais()$cuty
        nstar <- sr.cut()$nstar
        ny <- star$ny
    colp <- rep(c(1:nstar+1),ny)
    points(x2$S/u,log(x2$R/x2$S),pch=19,cex=1.5,col=colp)
    pointLabel(x2$S/u,log(x2$R/x2$S), labels=as.character(x2$Yr), cex = 1)
    }
  }
 if(isTRUE(input$show.points)){
      if(input$add!='kf'){
#        star <- lnalphais()$cuty
#        nstar <- sr.cut()$nstar
#        ny <- star$ny
#        colp <- rep(c(1:nstar+1),ny)
#    points(x2$S/u,log(x2$R/x2$S),pch=19,cex=1.5,col=colp)
#    pointLabel(x2$S/u,log(x2$R/x2$S), labels=as.character(x2$Yr), cex = 1)
#      }else{
     points(x2$S/u,log(x2$R/x2$S),pch=19,cex=1.5,col=colp)
     pointLabel(x2$S/u,log(x2$R/x2$S), labels=as.character(x2$Yr), cex = 1)
     legend('topright',col=unique(colp),legend=unique(dyear), pt.cex = 1.2,
           pch=19,box.lty=0)
      }
     } 

  out <-recordPlot()  
  return(out)   
  })

output$Plt_lnRS <- renderPlot({plt_lnRS()})

output$Txt_lnRS <- renderText({
paste0("Linear ln(R/S) plot.  Gray shade and dashed line indicates 95% Bayesian credible and prediction interval.")
  })

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
  page <- proportions(as.matrix(run[,substr(names(run),1,1) =='A']),margin = 1)  
# Import predicted age comp   
  dat <- SS.post.sum()$p.age
  nages <- Bayesdata()$nages 
  fage <- Bayesdata()$fage 
#  nef <- Bayesdata()$nef
  
# Plot  
  par(yaxs='i',bty='l',mar=c(3,3,3,2)+0.1,cex=1.5)
  par(mfrow=c(nages,1),cex=1.2)
  for(i in 1:nages){
    p <- dat[[i]]
  plot(median~Year,type='l',data=p,ylim=c(0,max(p$uci,page[,i])),xlab='Year',ylab='Proportion',
       main =paste('Age',fage+i-1),cex=1.2)
  lines(run$Year,p$mean,lty=2)
  polygon(c(p$Year,rev(p$Year)),c(p$uci,rev(p$lci)),col=tcol('grey',50),border=NA) 
  points(run$Year,page[,i],pch=19,col='red')
#  se <- sqrt(page[,i]*(1-page[,i])/nef)
#  yl <- page[,i]-1.96*se
#  yu <- page[,i]+1.96*se
#  arrows(run$Year,y0=yu,y1=yl,code=0,col='red')
  }
  out <-recordPlot()
  return(out)  
 })

output$Plt_SS_Age <- renderPlot({plt_SS_Age()})

#### Plt_SS plot Run, Escapement, harvest--------------------------------------  
plt_SS <- eventReactive(isTRUE(SS()),{
    run <- run.out()
    # trim data 
    # Create trimmed year   
    trimyear <- seq(ss.year()[1],ss.year()[2])    
    # Extract Year matches trimyear  
    run <- run[run$Year %in% trimyear,]

    N <- SS.post.sum()$N
    S <- SS.post.sum()$S
    H <- SS.post.sum()$H
    u <- unit()

    par(yaxs='i',bty='l',mar=c(4,3,3,2),cex=1.5)
    par(mfrow=c(3,1),cex=1.2)
    plot(N$Year,N$median/u,type='l',ylim=c(0,max(N$uci)/u),xlab='Year',ylab='',main=paste('Run x',mult(u)))
    lines(N$Year,N$mean/u,lty=2)
    polygon(c(N$Year,rev(N$Year)),c(N$uci/u,rev(N$lci/u)),col=tcol('grey',50),border=NA) 
    points(N$Year,run$N/u,pch=19,col='red')
#'  Plot S    
    plot(S$Year,S$median/u,type='l',ylim=c(0,max(S$uci)/u),xlab='Year',ylab='',main=paste('Escapement x',mult(u)))
    lines(run$Year,S$mean/u,lty=2)
    polygon(c(S$Year,rev(S$Year)),c(S$uci/u,rev(S$lci/u)),col=tcol('grey',50),border=NA) 
    points(run$Year,run$S/u,pch=19,col ='red')
#' Fit with harvest   
    plot(H$Year,H$median/u,type='l',ylim=c(0,max(H$uci)/u),xlab='Year',ylab='',main=paste('Harvest x',mult(u)))
    lines(H$Year,H$mean/u,lty=2)
    polygon(c(H$Year,rev(H$Year)),c(H$uci/u,rev(H$lci/u)),col=tcol('grey',50),border=NA) 
    points(run$Year,(run$N-run$S)/u,pch=19, col ='red')
    out <-recordPlot()
    return(out)  
 })

output$Plt_SS <- renderPlot({plt_SS()})

### Plt_SS_BAge Maturity plot Brood Age comp  ----------------------------------
plt_SS_BAge <- eventReactive(isTRUE(SS()),{
  # Extract Year matches trimyear  
  dat <- SS.post.sum()$q.age
  nages <- Bayesdata()$nages 
  fage <- Bayesdata()$fage 
  ob.p <- brood.p() 
  par(yaxs='i',bty='l',mar=c(4,3,3,2)+0.1,cex=1.5)
  par(mfrow=c(nages,1),cex=1.2)
  for(i in 1:nages){
    p <- dat[[i]]
  plot(median~Year,type='l',data=p,ylim=c(0,max(p$uci,ob.p[,i+1],na.rm=TRUE)),xlab='Year',
       ylab='Proportion', main =paste('Age',fage+i-1),cex=1.2)
  lines(mean~Year,data=p,lty=2)
  polygon(c(p$Year,rev(p$Year)),c(p$uci,rev(p$lci)),col=tcol('grey',50),border=NA) 
  points(ob.p$b.Year,ob.p[,i+1],pch=19,col='red')
    }
  out <-recordPlot()
  return(out)  
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
  SA.BEG  <- reactive({smsyprof$BEG()})
  p.msy <- reactive({smsyprof$p.min()})
  p.msy.t <- reactive({smsyprof$p.t()})

# Basic smsy profile plot
  plt.msy.prof <- reactive({smsyprof$plt.profile()})  
# Smsy profile plot with bounds  
  plt.msy.prof.fig <- reactive({smsyprof$plt.prof.fig()})
# Output Smsy profile plot   
  output$Plt_Smsy_prof <- renderPlot({plt.msy.prof.fig()})

#'-------------------------------------------------------------------------------
#  Smax Goal Analyses 
#'-------------------------------------------------------------------------------
  maxS <- reactive({ifelse(isFALSE(input$PlotMore),round(max(SRp$S)),input$maxS)})
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
  output$Plt_Smax_prof <- renderPlot({plt.max.prof.fig()})  
  
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

### Yield and Recruit Plot -------------------------------------------------------
output$Plt_rec.pg <- renderPlot({
  plot_range('r',base.r(),sr.data(),SRp(),Srange1=EG.Smsy()$S.Range,Srange2=EG.Smax()$S.Range,u=as.numeric(unit()))
  })
  
output$Plt_yield.pg <- renderPlot({
  plot_range('y',base.y(),sr.data(),SRp(),Srange1=EG.Smsy()$S.Range,Srange2=EG.Smax()$S.Range,u=as.numeric(unit()))
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


#'----- Yield Goal Simulation ---------------------------------------------------
Yield_gl_sim <- reactive({
  u<- as.numeric(unit())
  mc.Ypm <- Prob.calc(SR.pred()$Y,input$y1*u)
  mc.Ypa <- Prob.calc(SR.pred()$Y.p,input$y1*u)
  out <- list(mc.Yp=mc.Ypm, mc.Ypa=mc.Ypa)
  return(out)
  })


##---- Optimum Mean and annual Yield Profile Plot -------------------------------
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
       main=paste('Target',yg,'Yield probability plot')) 
  lines(S,mc.Ypa,lty=2)
  abline(h = ypg,lwd=2,col=2)
  BEG.p <- Yield_gl()/u 
  polygon(c(BEG.p[1,],rev(BEG.p[1,])),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
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
  paste(tex,'target range:',BEG.p[1,1],'-',BEG.p[1,2])
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
  numericInput("r1", paste("Target Recruit",mult), value=v[1],min=0, step=v[2])
})

Rec_gl_sim <- reactive({
  u <- as.numeric(unit())
  Rp <- Prob.calc(SR.pred()$R,input$r1*u)
  Rpa <- Prob.calc(SR.pred()$R.p,input$r1*u)
  out <- list(Rp=Rp, Rpa = Rpa)
  return(out)
})


#'------------------------------------------------------------------------------
#  Calculate probability that intercepts profile 
#'------------------------------------------------------------------------------
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
       main=paste('Target',rg,'Recruit probability Plot')) 
  lines(S,Rpa,lty=2)
  BEG.p <- Rec_gl()/u 
  polygon(c(BEG.p[1,],rev(BEG.p[1,])),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
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

#'===============================================================================
# Custom escapement goal analyses ----
#'===============================================================================
## UI Output Lower Goal -----------------------------------------------------
output$minEG <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,2],0.1)/u
  numericInput("lg", paste("Lower",mult), value=v[1],min=0, step=v[2])
 })
## UI Output Upper Goal -----------------------------------------------------
output$maxEG <- renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,2],0.75)/u
  numericInput("ug", paste("Upper",mult), value=v[1],min=0, step=v[2])
})

## UI Output Minimum Yield --------------------------------------------------
output$cyg = renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  h <- (data()[,3]-data()[,2])
  v <- numinput(h,0.25)/u
  numericInput("yg", paste("Min Target Yield",mult), value=v[1],min=0, step=v[2])
})
## UI Output Minimum Recruit ------------------------------------------------
output$crg = renderUI({
  u <- as.numeric(unit())
  mult <- mult(u)
  v <- numinput(data()[,3],0.25)/u
  numericInput("rg", paste("Min Target Recruit",mult), value=v[1],min=0, step=v[2])
})

#'-------------------------------------------------------------------------------
## plt_msyprof: Plot  MSY prof in given escapement---- 
#'-------------------------------------------------------------------------------
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

#'----- Profile output ----------------------------------------------------------
output$plt_msyprof_c <- renderPlot({plt_msyprof_c1()})

#'-------------------------------------------------------------------------------
## plt_maxprof:  Plot  Smax prof in given escapement ---- 
#'-------------------------------------------------------------------------------
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

#'----- Profile output ----------------------------------------------------------
output$plt_maxprof_c <- renderPlot({plt_maxprof_c1()})

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
  u <- as.numeric(unit())
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

#'-----------------------------------------------------------------------
#  Plot distribution of Mean and Annual  Yield at Given Escapement Range
#'-----------------------------------------------------------------------
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

#'-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#  SR model based CI and PI
#'-----------------------------------------------------------------------
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
output$Txt_Rec_pb_cg <- renderText({
  tex <- ifelse(input$target =='me','Mean','Median')
  t.prg <- paste('Meeting Recruit Target',tex,':',round(100*CG_pct()$prg,0),'%')
  t.pyg <- paste('Meeting Recruit Target Annual:',round(100*CG_pct()$prgp,0),'%')
  paste(t.prg,t.pyg,sep='\n')
})

# Calculate Probability meeting target  
output$Txt_Yield_pb_cg <- renderText({
  tex <- ifelse(input$target =='me','Mean','Median')  
  t.prg <- paste('Meeting Yield Target', tex,':',round(100*CG_pct()$pyg,0),'%')
  t.pyg <- paste('Meeting Yield Target Annual :',round(100*CG_pct()$pygp,0),'%')
  t.py0 <- paste('Zero Yield Annual:',round(100*CG_pct()$py0,0),'%')
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

output$Tbl_sim <- renderTable(meds(),digits=0)

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

output$foo2 <- renderDataTable({data.frame(MSE.int()$e.p)})  
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


#output$Tbl_mse <- renderDataTable({aggregate(length~rep+crit,FUN=sum, data=MSE.sum())})  
#output$Tbl_mse <- renderDataTable({MSE.sum()}) 

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
