source("Rcode/plots/baseplot/Base_plot_functions.R")   #  Separate year 
#'==============================================================================
# plots_base.R 
# This file includes all graphical output functions used in Shiny Server
# Include this when producing baseplots outputs  
#'==============================================================================
#'------------------------------------------------------------------------------
# DATA plots functions----
#'------------------------------------------------------------------------------
## plt_runesc -------------------------------------------------------------------
#'  Plot run and escapement timeseries 
#'------------------------------------------------------------------------------
plt_runesc <- reactive({
   if(input$dataType== "Run"){
     layout(matrix(1:2, ncol=2), widths=c(4,1))
     x <- data()[,c(1:3)]
     names(x) <-c('Yr','S','R')
     u <- unit()
     par(bty='u',las=1,cex=1.2,cex.lab=1.25,font.lab=2)
     plot_runesc(x,u)
     # Calculate Harvest rate 
     x$hrate <- with(x,(R-S)/R)
     par(new = TRUE)
     plot(hrate~Yr, data=x,type = "l", xaxt = "n",yaxt = "n",xlab='',ylab='',ylim=c(0,1),col='red')
     axis(side = 4)
     mtext("Harvest rate",side=4,line=2.5,las=0,cex=1.5,font=2)
     title("Run and Escapement", xlab="Year",
           ylab=paste('Run / Escapement',mult(u))) 
     add_legend("topleft",legend=c('Run','Esc','H rate'),lty=c(1,2,1),
                col=c(1,1,2), box.lty=0,xpd=TRUE)  
#'----  Plots Output -----------------------------------------------------------  
     out <-recordPlot()  
     return(out)    
    }
 })
 
#'------------------------------------------------------------------------------
## plt_srt -------------------------------------------------------------------
#'  Plot recruit and escapement timeseries 
#'------------------------------------------------------------------------------
plt_srt <- reactive({
  if(input$dataType != 'Escapement Only'){
  layout(matrix(1:2, ncol=2), widths=c(4,1))  
    x <- sr.data.0()
    u <- unit()
  par(yaxs='i',bty='u',las=1,cex=1.2,cex.lab=1.25,font.lab=2)
   plot(R/u~Yr,data=x,type='l',ylim=c(0,with(x,max(R,S,na.rm=TRUE)/u)),xlab='',ylab='')
   lines(S/u~Yr,data=x,lty=2)
  par(new = TRUE)
   plot((R/S)~Yr, data=x,type = "l", axes = FALSE,xlab='',ylab='',col='red')
   axis(side = 4, at =pretty(range(x$R/x$S)))
   mtext("R/S",side=4,line=2.5,las=0,cex=1.5,font=2)
   title("Spawner and Recruit", xlab="Brood Year", ylab=paste('Spawner / Recruit',mult(u))) 
   # Add Cutting data 
   if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
#     with(x,polygon(c(min(Yr),min(input$sryears),min(input$sryears),min(Yr)),c(0,0,max(R/u),max(R/u)),col=tcol('grey',50),border=NA))
#     with(x,polygon(c(max(Yr),max(input$sryears),max(input$sryears),max(Yr)),c(0,0,max(R/u),max(R/u)),col=tcol('grey',50),border=NA))
     abline(v=input$sryears,col=2)
     } 
   add_legend('topleft',c('Spawner','Recruit','R/S'),lty=c(2,1,1),col=c(1,1,2),box.lty=0,xpd=TRUE)  
    } else {
    x <- e.data.0()
    u <- unit()
    par(yaxs='i',bty='l',las=1,cex=1.2,cex.lab=1.25,font.lab=2)
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


plt_srt2 <- reactive({
  if(input$dataType == 'Run'){
    layout(matrix(1:2, ncol=2), widths=c(4,1))  
    x <- sr.data.1()
    u <- unit()
    par(yaxs='i',bty='u',las=1,cex=1.2,cex.lab=1.25,font.lab=2)
    plot(R/u~Yr,data=x,type='l',ylim=c(0,with(x,max(R,S,na.rm=TRUE)/u)),xlab='',ylab='')
    lines(S/u~Yr,data=x,lty=2)
    lines(H/u~Yr,data=x,lty=3)
    par(new = TRUE)
    plot((H/R)~Yr, data=x,type = "l", ylim=c(0,1),axes = FALSE,xlab='',ylab='',col='red')
    axis(side = 4)
    mtext("Recruit Harvest Rate",side=4,line=2.5,las=0,cex=1.5,font=2)
    title("Spawner and Recruit", xlab="Brood Year", ylab=paste('Spawner / Recruit',mult(u))) 
    # Add Cutting data 
    if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
      abline(v=input$sryears,col=2)
    } 
    add_legend('topleft',c('Spawner','Recruit', 'Recruit Harvest','R.H.rate'),lty=c(2,1,3,1),col=c(1,1,1,2),box.lty=0,xpd=TRUE)  
   } 
  out <-recordPlot()  
  return(out)      
})

#'------------------------------------------------------------------------------
## plt_lnsrt -------------------------------------------------------------------
#'  Plot lnRS and escapement timeseries 
#'------------------------------------------------------------------------------
plt_lnsrt <- reactive({
  x <- sr.data.0()
  u <- unit()
  par(cex=1.2,cex.lab=1.25,font.lab=2)
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

#'------------------------------------------------------------------------------
## plt_hist.sry -------------------------------------------------------------------
#'  Plot Spawner and recruitment histogram 
#'------------------------------------------------------------------------------
plt_hist.sry <- reactive({
  u <- as.numeric(unit())  
  if(input$dataType != "Escapement Only"){
  par(mfrow=c(1,4),cex=1.2,mar=c(4,2,1,0))
  df <- sr.data()
  plot_hist(df$S,u,paste('Spawnter',mult(u)))
  plot_hist(df$R,u,paste('Recruit',mult(u)))
  plot_hist(df$Y,u,paste('Yield',mult(u)))
  plot_hist(df$lnRS,u,paste('ln(R/S)'))
  } else {
    x <- e.data()  
  plot_hist(x$S,u,paste('Spawnter',mult(u)))    
  }
  })

#'------------------------------------------------------------------------------
## plt_hist.run -------------------------------------------------------------------
#'  Plot Run and escapement  histogram (Only when datatype is run ) 
#'------------------------------------------------------------------------------
plt_hist.run <- reactive({
  u <- unit()
  if(input$dataType == "Run"){
  df <- data()[,c(1:3)]
  names(df) <-c('Yr','S','R')
  df$H <- with(df,(R-S))
  df$HR <- with(df,H/R)
  par(mfrow=c(1,4),cex=1.2,mar=c(4,2,1,0))
  plot_hist(df$S,u,paste('Escapement',mult(u)))
  plot_hist(df$R,u,paste('Run',mult(u)))
  plot_hist(df$H,u,paste('Harvest',mult(u)))
  plot_hist(df$HR,1,paste('Harvest Rate'))
  }
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Model Diagnoses Plots ---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'------------------------------------------------------------------------------
## plt_hist.mc -------------------------------------------------------------------
#'  Plot posterior density plots
#'------------------------------------------------------------------------------
plt_hist.mc <- reactive({
  u <- unit()
  D <- Bayesdata()$d
  if(input$add=='ar1'){ar1<- TRUE} else {ar1<- FALSE}
  if(input$add =='kf') {
     req(input$alphai)
    if(input$alphai != 'None'){
   plot_density(SR.post.i()$post,D,u,ar1,model=input$Model,target=input$target)
  } else {
   plot_density(SR.post()$post,D,u,ar1,model=input$Model,target=input$target)    
  }
  }else{
   plot_density(SR.post()$post,D,u,ar1,model=input$Model,target=input$target)      
  }
#--- Plot output----------------------------------------------------------------  
  out <-recordPlot()
  return(out)  
 })

#'------------------------------------------------------------------------------
## plt_lnalphai ----------------------------------------------------------------
#'  Plot annual lnalpha timeseries (when TVA model is selected)
#'------------------------------------------------------------------------------
plt_lnalphai <- reactive({
  if(input$add=='kf'){
# Extract mean lnalpha for each year 
  xa <- lnalphais()$lnalphai
#  lnalphai.m <- xa$lnalphai.m
# Mean overall lnalpha  
  lnalpha <- mean(xa$lnalphai.m)
# Calculate STARS
# Plot figures   
  par(bty='l',cex=1.2)
  with(xa,plot(lnalphai.m~year,type='l',lwd=2,main='time-varying lnalpha',
       xlab='Year',ylab='lnalpha',ylim=c(min(cil),max(ciu))))
  with(xa,polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)) 
  abline(h=lnalpha,lwd=2,col='red')
  lines(star~year,data=xa,col=4,lwd=2)
  out <-recordPlot()  
  return(out)   
  } 
 })

#'------------------------------------------------------------------------------
## plt_residual  ----------------------------------------------------------------
#'  Plot residual teme series plot 
#'------------------------------------------------------------------------------
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
  par(bty='l',las=1,cex=1.2,cex.lab=1.25,font.lab=2)
  plot(resid.m~year,xlab='Year',type='l',ylab='Residuals',lwd=2, main='Residual',
       ylim =c(min(cil),max(ciu)))
  abline(h=0)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  out <-recordPlot()  
  return(out)   
  })

#'------------------------------------------------------------------------------
## plt_predict ----------------------------------------------------------------
#'  Plot annual predicted recruitment time series 
#'------------------------------------------------------------------------------
plt_predict <- reactive({
  year <- sr.data()$Yr
  resid <-SR.resid()$RD
  R <- log(sr.data()$R)
  R.m <- R - apply(resid,2,mean)
  cil <- R - apply(resid,2,function(x) quantile(x, 0.025))
  ciu <- R - apply(resid,2,function(x) quantile(x, 0.975))
  RMSE <- sqrt(mean((R-R.m)^2))
  par(bty='l',las=1,cex=1.2,cex.lab=1.25,font.lab=2)
  plot(R.m~year,xlab='Year',type='l',lwd=2,ylab='ln(Recruit)',main='Recruit',
       ylim =c(min(cil,R),max(ciu,R)))
  points(R~year,pch=19,col='red')
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  legend('topright',legend=c('Observed','Predicted'),pch=c(19,NA),lty=c(NA,1), col=c('red',1),bty='n')
  legend('topleft',legend=paste('RMSE', round(RMSE,2)),bty='n')
  out <-recordPlot()  
  return(out)   
   })

#'------------------------------------------------------------------------------
## plt_SS ----------------------------------------------------------------
#'  Plot Run, Harvest, and escapement 
#'------------------------------------------------------------------------------
plt_SS <- eventReactive(SS(),{
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

#'------------------------------------------------------------------------------
## plt_SS_Age  ----------------------------------------------------------------
#'  Plot Run Age composition 
#'------------------------------------------------------------------------------
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

#'------------------------------------------------------------------------------
## plt_SS_BAge ----------------------------------------------------------------
#'  Plot Brood age composition  
#'------------------------------------------------------------------------------
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
  plot(median~Year,type='l',data=p,ylim=c(0,max(p$uci,ob.p[,2+i],na.rm=TRUE)),xlab='Year',
       ylab='Proportion', main =paste('Age',fage+i-1),cex=1.2)
  lines(mean~Year,data=p,lty=2)
  polygon(c(p$Year,rev(p$Year)),c(p$uci,rev(p$lci)),col=tcol('grey',50),border=NA) 
  points(ob.p[,1],ob.p[,2+i],pch=19,col='red')
    }
  out <-recordPlot()
  return(out)  
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SR Plots ---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'------------------------------------------------------------------------------
## base.pl ----------------------------------------------------------------
#'  Plot base SR, SY, ln(R/S) plot  
#'------------------------------------------------------------------------------
base.pl <- reactive({
  u <- unit()
  xp <- sr.data.0()
  xp[,c(2,3)] <- xp[,c(2,3)]/u
  xp$Y <- xp$Y/u
  xp2 <- sr.data()
  xp2[,c(2:3)] <- xp2[,c(2,3)]/u
  xp2$Y <- xp2$Y/u
  SRp <- SRp()/u
  maxS <- ifelse(input$axis,input$maxS,max(SRp$S))
  maxR <- ifelse(input$axis,input$maxR,round(1.25*max(xp$R,na.rm=TRUE)))
  minY <- ifelse(input$axis,input$Yrange[1],round(min(SRp$Rl-SRp$S,na.rm=TRUE)))
  maxY <- ifelse(input$axis,input$Yrange[2],round(1.25*max(xp$Y,na.rm=TRUE)))
  minlnRS <- ifelse(min(xp$lnRS)>0,0.75*min(xp$lnRS),1.25*min(xp$lnRS))

#'---  Basic SR plot ------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l',las=1,cex=1.2,cex.lab=1.5)
  plot(R~S,data=xp,pch=19,col='gray',cex=1.5, 
       xlab=paste("Spawner",mult(u)),ylab=paste('Recruit',mult(u)),
       xlim=c(0,maxS),ylim=c(0,maxR)
       )
  points(R~S,data=xp2,pch=19,col=1,cex=1.5)
  # Plot 1:1 line 
  abline(0,1)
  # Add Predicted   
  lines(RS.md~S,data=SRp,col=1,lw=2)
  lines(RS.me~S,data=SRp,col=1,lw=2,lty=2)
  gsr <-recordPlot()
#'-------------------------------------------------------------------------------
#'---  Basic Yield plot ---------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l',cex=1.2,cex.lab=1.5)
  plot(Y~S,data=xp,pch=19,col='gray', cex=1.5,
       xlab=paste("Spawner",mult(u)),ylab=paste('Yield',mult(u)),
       xlim=c(0,maxS),ylim=c(minY,maxY))
  points(Y~S,data=xp2,pch=19,col=1,cex=1.5)
  lines((RS.md-S)~S,data=SRp,col=1,lw=2)
  lines((RS.me-S)~S,data=SRp,col=1,lw=2,lty=2)
  abline(h=0)
  gsy <-recordPlot()  

#'---  Basic lnSR plot ---------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l',cex=1.2,cex.lab=1.5)
  plot(lnRS~S,data=xp,pch=19,col='gray', cex=1.5,
       xlab=paste("Spawner",mult(u)),ylab=paste('ln(R/S)'),
       xlim=c(0,maxS),ylim=c(minlnRS, 1.25*max(lnRS)))
  points(lnRS~S,data=xp2,pch=19,col=1,cex=1.5)
  lines(log(RS.md/S)~S,data=SRp,col=1,lw=2)
  lines(log(RS.me/S)~S,data=SRp,col=1,lw=2,lty=2)
  abline(h=0)
  glnRS <-recordPlot() 
#'----  Plots Output ------------------------------------------------------------
  return(list(base.r=gsr,base.y=gsy,base.ln = glnRS))
})

#'------------------------------------------------------------------------------
## base.sr  ----------------------------------------------------------------
#' Base plots with 95% CI/PI range  
#'------------------------------------------------------------------------------
base.sr <- reactive({
  u <- unit()
  SRp <- SRp()/u
#' Plot base SR Plot with CI  ---------------------------------------------------
  replayPlot(base.pl()$base.r)
  # SR CI range 
  with(SRp,polygon(c(S,rev(S)),c(Ru,rev(Rl)),col=tcol('grey',50),border=NA))
  # SR PI range 
  with(SRp,lines(S,Ru.p,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p,lty=2,col='grey'))
  p1 <-recordPlot()

#' Plot base Yield Plot --------------------------------------------------
  replayPlot(base.pl()$base.y)
  with(SRp,polygon(c(S,rev(S)),c(Ru-S,rev(Rl-S)),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,Ru.p-S,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p-S,lty=2,col='grey'))  
  p2 <-recordPlot()  

#' Plot base lnRS Plot  --------------------------------------------------
  replayPlot(base.pl()$base.ln)
  with(SRp,polygon(c(S,rev(S)),c(log(Ru/S),rev(log(Rl/S))),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,log(Ru.p/S),lty=2,col='grey'))
  with(SRp,lines(S,log(Rl.p/S),lty=2,col='grey')) 
  p3 <- recordPlot() 
  return(list(base.r=p1,base.y=p2,base.ln = p3))  
})

#'------------------------------------------------------------------------------
## plt_SRY ----------------------------------------------------------------
#'  Plot SR, SY, ln(R/S), SRG:  This goes to Shiny interface 
#'------------------------------------------------------------------------------
plt_SRY <- reactive({
  u <- unit()
  SRp <- SRp()/u
  maxS <- ifelse(input$axis,input$maxS,max(SRp$S))
  xp <- sr.data()  # SR plot datat
  xp[,c(2,3)] <- xp[,c(2,3)]/u
  xp$Y <- xp$Y/u
  dyear <- floor(xp$Yr/10)*10   # Decades 
  labels <- unique(dyear)
  colp <- 1:length(labels)+1 # Decades color palette  
  cols <- (dyear-min(dyear))/10+2
  
# Create data for TVA model 
if(input$add=='kf'){
    df.tva <- kf.data()$df
    df.tva$S <- df.tva$S/u
    df.tva$R <- df.tva$R/u
    nstar <- kf.data()$nstar
    ny <- kf.data()$ny
    star <- kf.data()$star    
    dyear <- rep(c(1:nstar),ny)     
    colp <- 1:nstar+1
    labels <- star$txt
    cols <- dyear+1
       }   

#' CI or PI data ---------------------------------------------------------------
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

#' Density plot----------------------------------------------------------------
  if(input$add == 'kf') {
   if(input$alphai == 'None'){
    out <-SR.post()$post
    } else {
    out <- SR.post.i()$post    
    }
  } else {
    out <- SR.post()$post     
   }
 pcol <- c('Seq','Smsy','Sgen')

 if(input$target=='md'){
 pcol <- pcol
  } else {
  pcol <- paste0(pcol,'.c')    
    } 
if(input$Model == 'Ricker') {pcol <- c(pcol,'Smax')} 
  den <- apply(out[,pcol]/u,2,FUN=function(x) trimden(x))  
#'------------------------------------------------------------------------------  
    
#' Draw Base SR and YLD Plot -------------------------------------------------
p1 <- base.pl()$base.r
p2 <- base.pl()$base.y
p3 <- base.pl()$base.ln
p4 <- base.pl()$base.r

#'---------- TVA lines  ------------------------------------------------------  
if(input$add=='kf'){
    replayPlot(p1)
    points(R~S,data=xp,pch=19,cex=1.5,col=cols)
    for (i in 1:nstar){
        df <- df.tva[which(df.tva$star==i),]      
        lines(R~S,data=df,lty=2,lwd=1,col=1+i)
     }
   legend('topleft',col=colp,legend=labels, pt.cex = 1.2,
           pch=19,bty='n')
    p1 <- recordPlot()
#'.............................................................................
   replayPlot(p2)
    points(Y~S,data=xp,pch=19,cex=1.5,col=cols)
     for (i in 1:nstar){
        df <- df.tva[which(df.tva$star==i),]  
        lines((R-S)~S,data=df,lty=2,lwd=1,col=1+i)
      }
   legend('topleft',col=colp,legend=labels, pt.cex = 1.2,
           pch=19,bty='n')
    p2 <- recordPlot()   
#'..............................................................................
    replayPlot(p3) 
    points(lnRS~S,data=xp,pch=19,cex=1.5,col=cols)
    for (i in 1:nstar){
        df <- df.tva[which(df.tva$star==i),]      
        lines(log(R/S)~S,data=df,lty=2,lwd=1,col=1+i)
     }
    legend('bottomleft',col=colp,legend=labels, pt.cex = 1.2,
           pch=19,bty='n')
    p3 <- recordPlot() 
#'..............................................................................
    replayPlot(p4)
    points(R~S,data=xp,pch=19,cex=1.5,col=cols)
    for (i in 1:nstar){
      df <- df.tva[which(df.tva$star==i),]      
      lines(R~S,data=df,lty=2,lwd=1,col=1+i)
    }
    legend('topleft',col=colp,legend=labels, pt.cex = 1.2,
           pch=19,bty='n')
    p4 <- recordPlot()
      }  
#'-------- Add Interval -------------------------------------------------------- 
  if(isTRUE(input$show.int)){
# SR plot
      replayPlot(p1) 
      with(SRp,polygon(c(S,rev(S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA))
      lines(Ru.p~S,data=SRp, col= 'grey',lty=2)
      lines(Rl.p~S,data=SRp, col= 'grey',lty=2)
      p1 <- recordPlot()
# Yield plot      
      replayPlot(p2)
      with(SRp,polygon(c(S,rev(S)),c(upr-S,rev(lwr-S)),col=tcol('grey',50),border=NA))
      lines((Ru.p-S)~S,data=SRp, col= 'grey',lty=2)
      lines((Rl.p-S)~S,data=SRp, col= 'grey',lty=2)
      p2 <- recordPlot()
# lnRS plot      
      replayPlot(p3)
      with(SRp,polygon(c(S,rev(S)),c(log(upr/S),rev(log(lwr/S))),col=tcol('grey',50),border=NA))
      lines(log(Ru.p/S)~S,data=SRp, col= 'grey',lty=2)
      lines(log(Rl.p/S)~S,data=SRp, col= 'grey',lty=2)
      p3 <- recordPlot()
# SR den plot
      replayPlot(p4) 
      with(SRp,polygon(c(S,rev(S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA))
      lines(Ru.p~S,data=SRp, col= 'grey',lty=2)
      lines(Rl.p~S,data=SRp, col= 'grey',lty=2)
      p4 <- recordPlot()
     } # End Interval
#'------ Add Years -------------------------------------------------------------
if(isTRUE(input$show.points)) {
    replayPlot(p1)
    points(R~S,data=xp,pch=19,cex=1.5,col=cols)
    with(xp,pointLabel(S,R, labels=as.character(Yr), cex = 1))
if(input$add !='kf'){
        legend('topleft',col=colp,legend=labels, pt.cex = 1.2,
           pch=19,,bty='n')
        }
    p1 <- recordPlot()
#'..............................................................................
    replayPlot(p2) 
    points(Y~S,data=xp,pch=19,cex=1.5,col=cols)
    with(xp,pointLabel(S,Y, labels=as.character(Yr), cex = 1))
    if(input$add !='kf'){
      legend('topleft',col=colp,legend=labels, pt.cex = 1.2,
             pch=19,,bty='n')
    }
    p2 <- recordPlot()
#'..............................................................................
    replayPlot(p3)
    points(lnRS~S,data=xp,pch=19,cex=1.5,col=cols)
    with(xp,pointLabel(S,lnRS, labels=as.character(Yr), cex = 1))
    if(input$add !='kf'){
      legend('bottomleft',col=colp,legend=labels, pt.cex = 1.2,
             pch=19,bty='n')
    }
    p3 <- recordPlot()   
#'..............................................................................
    replayPlot(p4)
    points(R~S,data=xp,pch=19,cex=1.5,col=cols)
    if(input$add !='kf'){
      legend('topleft',col=colp,legend=labels, pt.cex = 1.2,
             pch=19,bty='n')
    }
    p4 <- recordPlot()   
#'..............................................................................
        }  # End show.points  
#'------------------------------------------------------------------------------  
#'------------------------------------------------------------------------------
#'  Add Seq, Smsy, Smax, Sgen
#' -----------------------------------------------------------------------------
t1 <- ''
l1 <- 0
#'---------- Add Smsy ----------------------------------------------------------
  if(input$show.seq==TRUE){
    Seq <- legend_data[grepl("Seq", legend_data$label),]
    t1 <- Seq$label
    l1 <- 1    
    replayPlot(p1) 
    abline(v=Seq$x/u,lty=l1)
    p1 <- recordPlot()
#..............................................................................
    replayPlot(p2) 
    abline(v=Seq$x/u,lty=l1)
    p2 <- recordPlot()
#..............................................................................
    replayPlot(p3) 
    abline(v=Seq$x/u,lty=l1)
    p3 <- recordPlot()
#..............................................................................
    replayPlot(p4)
    par(new = TRUE)  
    plot(den[[1]],col=2,xlim = c(0,maxS),axes = FALSE,xlab='',ylab='',main='')
    polygon(den[[1]], col=tcol(2,80),border=NA)    
    abline(v=Seq$x/u,col=2,lwd=2,lty=l1) 
    p4 <- recordPlot()
    }
  t2 <- ''
  l2 <- 0
#'---------- Add Smsy ----------------------------------------------------------
  if(input$show.smsy==TRUE){
    Smsy <- legend_data[grepl("Smsy", legend_data$label),]
    t2 <- Smsy$label
    l2 <- 2    
    replayPlot(p1) 
    abline(v=Smsy$x/u,lty=l2)
    p1 <- recordPlot()
#..............................................................................
    replayPlot(p2) 
    abline(v=Smsy$x/u,lty=l2)
    p2 <- recordPlot()
#..............................................................................
    replayPlot(p3) 
    abline(v=Smsy$x/u,lty=l2)
    p3 <- recordPlot()
#..............................................................................
    replayPlot(p4)
    par(new = TRUE)  
    plot(den[[2]],col=3,xlim = c(0,maxS),axes = FALSE,xlab='',ylab='',main='')
    polygon(den[[2]], col=tcol(3,80),border=NA)    
    abline(v=Smsy$x/u,col=3,lwd=2,lty=l2)
    p4 <- recordPlot()
    }
  t3 <- ''
  l3 <- 0
# Add Smax       
  if(input$show.smax==TRUE & input$Model == 'Ricker') {
    Smax <- legend_data[grepl("Smax", legend_data$label),]
    t3 <- Smax$label
    l3 <- 3
    replayPlot(p1)
    abline(v=Smax$x/u,lty=l3)
    p1 <- recordPlot()
#...............................................................................
    replayPlot(p2)
    abline(v=Smax$x/u,lty=l3) 
    p2 <- recordPlot()   
#...............................................................................
    replayPlot(p3)
      abline(v=Smax$x/u,lty=l3)
      p3 <- recordPlot()     
#..............................................................................
    replayPlot(p4)
    par(new = TRUE)  
    plot(den[[4]],col=4,xlim = c(0,maxS),axes = FALSE,xlab='',ylab='',main='')
    polygon(den[[4]], col=tcol(4,80),border=NA)    
    abline(v=Smax$x/u,col=4,lwd=2,lty=l3)     
    p4 <- recordPlot()
     }
  t4 <- ''
  l4 <- 0
  #  Add Sgen  
  if(input$show.sgen==TRUE){
      Sgen <- legend_data[grepl("Sgen", legend_data$label),]
      t4 <- Sgen$label
      l4 <- 4
      replayPlot(p1)
      abline(v=Sgen$x/u,lty=l4)
      p1 <- recordPlot()
#...............................................................................
      replayPlot(p2)
      abline(v=Sgen$x/u,lty=l4)
      p2 <- recordPlot()
#...............................................................................
      replayPlot(p3)
      abline(v=Sgen$x/u,lty=l4)
      p3 <- recordPlot()      
#..............................................................................
    replayPlot(p4)
    par(new = TRUE)  
    plot(den[[3]],col=5,xlim = c(0,maxS),axes = FALSE,xlab='',ylab='',main='')
    polygon(den[[3]], col=tcol(5,80),border=NA)    
    abline(v=Sgen$x/u,col=5,lwd=2,lty=l4)  
    p4 <- recordPlot()
     }
#...............................................................................
#'----- Add Legends ------------------------------------------------------------
  replayPlot(p1)
  legend('topright',c(t1,t2,t3,t4),lty=c(l1,l2,l3,l4),
         lwd=2, col=c(1:(length(t1)),1),box.lty=0)  
  p1 <-recordPlot()
#'----- Add Legends ------------------------------------------------------------
    replayPlot(p2) 
  legend('topright',c(t1,t2,t3,t4),lty=c(l1,l2,l3,l4),
         lwd=2, col=c(1,1,1,1),box.lty=0)  
  p2 <-recordPlot()
#'----- Add Legends ------------------------------------------------------------
    replayPlot(p3) 
  legend('topright',c(t1,t2,t3,t4),lty=c(l1,l2,l3,l4),
         lwd=2, col=c(1,1,1,1),box.lty=0)  
  p3 <-recordPlot()
#'----- Add Legends ------------------------------------------------------------
  replayPlot(p4)
  legend('topright',c(t1,t2,t3,t4),lty=c(l1,l2,l3,l4),
         lwd=2, col=c(2,3,4,5),box.lty=0)  
  p4 <-recordPlot()
#'------------------------------------------------------------------------------

#' Case RE Elements ------------------------------------------------------------  
 if(RE()){
    S <- RE.post()
    S <- data.frame(S,xp)
    replayPlot(p1)
    points(exp(S$mean)/u,S$R,pch=19,col=cols,cex=1.5)
    with(S,arrows(R,x0=exp(uci)/u,x1=exp(lci)/u,code=0,lty=2,lwd=1,col=cols))
    p1 <- recordPlot()
  }  
#'-- Add SS Elements  ----------------------------------------------------------
if(SS()){
    cidata <- data.frame(sr.data.ci())
    cidata <- cidata[which(cidata$Yr %in% xp$Yr),]
    cidata[,!names(cidata) %in% c('Yr')] <- cidata[,!names(cidata) %in% c('Yr')]/u
    S <- SS.post.sum()$S
    S <- S[which(S$Year %in% xp$Yr),]
    R <- SS.post.sum()$R
    R <- R[which(R$Year %in% xp$Yr),] 

   df.SS <- data.frame(S=S$mean, Suci=S$uci,Slci=S$lci,R=R$mean,Ruci=R$uci,Rlci=R$lci)
   df.SS <- df.SS/u
   df2a <- data.frame(S2=S$mean/u, S1 = xp$S,R2=R$mean/u, R1 = xp$R)
  # Observed CI   
  if(isTRUE(input$show.ob.se)){
    replayPlot(p1) 
    with(cidata,arrows(R,x0=Suci,x1=Slci,code=0,lty=2,lwd=1,col=cols))
    with(cidata,arrows(S,y0=Ruci,y1=Rlci,code=0,lty=2,lwd=1,col=cols))
   p1 <- recordPlot()
#...............................................................................   
    replayPlot(p2)
    with(cidata,arrows((R-S),x0=Suci,x1=Slci,code=0,lty=2,lwd=1,col=cols))
    with(cidata,arrows(S,y0=(Ruci-S),y1=(Rlci-S),code=0,lty=2,lwd=1,col=cols))
    p2 <- recordPlot() 
#...............................................................................   
    replayPlot(p3)
    with(cidata,arrows(log(R/S),x0=Suci,x1=Slci,code=0,lty=2,lwd=1,col=cols))
    with(cidata,arrows(S,y0=log(Ruci/S),y1=log(Rlci/S),code=0,lty=2,lwd=1,col=cols))
    p3 <- recordPlot()   
  }
    
# Model predicted Point and CI   
  if(isTRUE(input$show.ss.point)){  
    replayPlot(p1)
    with(df.SS,points(S,R,pch=19,col=cols,cex=1.5)) 
    with(df.SS,arrows(R,x0=Suci,x1=Slci,code=0,lty=2,lwd=1,col=cols))
    with(df.SS,arrows(S,y0=Ruci,y1=Rlci,code=0,lty=2,lwd=1,col=cols))
    p1 <- recordPlot()
#...............................................................................  
    replayPlot(p2)
    with(df.SS,points(S,(R-S),pch=19,col=cols,cex=1.5))  
    with(df.SS,arrows(R-S,x0=Suci,x1=Slci,code=0,lty=2,lwd=1,col=cols))
    with(df.SS,arrows(S,y0=(Ruci-S),y1=(Rlci-S),code=0,lty=2,lwd=1,col=cols))
    p2 <- recordPlot()
#...............................................................................
    replayPlot(p3)
    with(df.SS,points(S,log(R/S),pch=19,col=cols,cex=1.5))  
    with(df.SS,arrows(log(R/S),x0=Suci,x1=Slci,code=0,lty=2,lwd=1,col=cols))
    with(df.SS,arrows(S,y0=log(Ruci/S),y1=log(Rlci/S),code=0,lty=2,lwd=1,col=cols))
    p3 <- recordPlot()
  }
   
if(isTRUE(input$show.arrows)){  
    replayPlot(p1)
    points(R2~S2,data=df2a,pch=19,col=cols,cex=1.5)
    with(df2a,arrows(S1,R1,S2,R2,lty=1,lwd=1,col=1,length = 0.1,cex=0.8))
    p1 <- recordPlot()
#...............................................................................
    replayPlot(p2)
    points((R2-S2)~S2,data=df2a,pch=19,col=cols,cex=1.5)
    with(df2a,arrows(S1,(R1-S1),S2,(R2-S2),lty=1,lwd=1,col=1,length = 0.1,cex=0.8))
    p2 <- recordPlot()  
#...............................................................................
    replayPlot(p3)
    points(log(R2/S2)~S2,data=df2a,pch=19,col=cols,cex=1.5)
    with(df2a,arrows(S1,log(R1/S1),S2,log(R2/S2),lty=1,lwd=1,col=1,length = 0.1,cex=0.8))
    p3 <- recordPlot()
    }
   }
  return(list(pltSR=p1, pltYD=p2,pltLN = p3,pltBRp=p4))  
 })

#'------------------------------------------------------------------------------
# Profile Analyses     -------------------------
#'------------------------------------------------------------------------------
#'------------------------------------------------------------------------------
## plt_cg_prof ----------------------------------------------------------------
#'  Plot Custom escapement goal profile plor
#'------------------------------------------------------------------------------
plt_cg_prof <- reactive({
  SS <- c(input$lg,input$ug)
# Smsy prof (p1) and Smax prof(p2)
  if((SS[1]==SS[2])){
    replayPlot(plt.msy.prof())
    lines(SS,c(0,1),col=3,lwd=3)
    p1 <-recordPlot()
    replayPlot(plt.max.prof())
    lines(SS,c(0,1),col=4,lwd=3)
    p2 <-recordPlot()
    } else{
    replayPlot(plt.msy.prof())      
    rect(SS[1],0,SS[2],1,col=tcol(3,80),border=NA)  
    p1 <-recordPlot()
    replayPlot(plt.max.prof())      
    rect(SS[1],0,SS[2],1,col=tcol(4,80),border=NA)  
    p2 <-recordPlot()
    }  
 return(list(msy=p1,max=p2))
})
#'------------------------------------------------------------------------------
## plt_Yield_EG ----------------------------------------------------------------
#'  Plot expected yield density 
#'------------------------------------------------------------------------------
plt_Yield_EG <- reactive({
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
  mult.den.plt(Y.p,Y.m,'Yield',leg.tx,u)
  abline(v=input$yg,col=2,lwd=2)
  abline(v=m/u,col=4,lwd=2) 
  out <-recordPlot()  
  return(out)  
 })

#'------------------------------------------------------------------------------
## plt_Rec_EG ----------------------------------------------------------------
#'  Plot expected recruit density 
#'------------------------------------------------------------------------------
plt_Rec_EG <- reactive({
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
  mult.den.plt(R.p,R.m,'Recryut',leg.tx,u)
  abline(v=input$rg,col=2,lwd=2)
  abline(v=m/u,col=4,lwd=2)
  out <-recordPlot()  
  return(out)  
  }) 

#'------------------------------------------------------------------------------
## plt_rec.prof ----------------------------------------------------------------
#'  Plot expected yield density 
#'------------------------------------------------------------------------------
#  Minimum Recruit Profile Plot 
plt_rec.prof <- reactive({
  u <- unit()
  tex <- ifelse(input$target =='me','Mean','Median')
  prof.ma <- Rec_gl()$prof.ma
  tpg <-input$r1p/100
  srange <- as.numeric(Rec_gl()$range$b.p)
  par(xaxs='i',yaxs='i',bty='l',cex=1.2,cex.lab=1.5)
  prof.ma$S<- prof.ma$S/u
  plot(prof.m~S,data=prof.ma,type='l',ylim=c(0,1),main='Target Recruit Profile Analyses',
       ylab ='Probability',xlab=paste("Spawner",mult(u))) 
  lines(prof.a~S,data=prof.ma,lty=2)
  abline(h = tpg,lwd=2,col=2)
  rect(srange[1]/u,par('usr')[3],srange[2]/u,par('usr')[4],col=tcol(3,80),border=NA)
  legend('topright',legend=c(tex,'Annual'),lty=c(1,2), box.lty=0)
  out <-recordPlot()  
  return(out)  
 }) 

#'------------------------------------------------------------------------------
## plt_rec.prof ----------------------------------------------------------------
#'  Plot expected recruit profile density 
#'------------------------------------------------------------------------------
##---- Optimum Mean and annual Yield Profile Plot -------------------------------
plt_yield.prof <- reactive({
  u <- unit()
  tex <- ifelse(input$target =='me','Mean','Median')
  prof.ma <- Yield_gl()$prof.ma
  tpg <-input$y1p/100
  srange <- as.numeric(Yield_gl()$range$b.p)
  par(xaxs='i',yaxs='i',bty='l',cex=1.2,cex.lab=1.5)
  prof.ma$S<- prof.ma$S/u
  plot(prof.m~S,data=prof.ma,type='l',ylim=c(0,1),main='Target Yield Profile Analyses',
       ylab ='Probability',xlab=paste("Spawner",mult(u))) 
  lines(prof.a~S,data=prof.ma,lty=2)
  abline(h = tpg,lwd=2,col=2)
  rect(srange[1]/u,par('usr')[3],srange[2]/u,par('usr')[4],col=tcol(3,80),border=NA)
  legend('topright',legend=c(tex,'Annual'),lty=c(1,2), box.lty=0)
  out <-recordPlot()  
  return(out)  
}) 

#'------------------------------------------------------------------------------
## plt_ridge.prof ----------------------------------------------------------------
#'  Plot Ridge plot  
#'------------------------------------------------------------------------------
plt_msyprof_r <- reactive({
  S <- EG.Smsy()$S
  Y.prof <- EG.Smsy()$Dat.prof
  u <- unit()
  Srange <- EG.Smsy()$S.Range/u
  par(xaxs='i',yaxs='i',bty='l',cex=1.2,cex.lab=1.5)
  plot_ridge (S,Y.prof,u,60)
  abline(v=Srange,col=2,lty=2)
  title(main='MSY Profile')
  par(new = TRUE)
  plot(S/u,EG.Smsy()$S.prof,type='l', ylim=c(0,1),lwd=1.5, col=6,axes=F,xlab='',ylab='',main='',yaxs='i') 
  axis(4)
  out <- recordPlot()
  return(out)
})

plt_maxprof_r <- reactive({
  S <- EG.Smax()$S
  Y.prof <- EG.Smax()$Dat.prof
  u <- unit()
  Srange <- EG.Smax()$S.Range/u
  par(xaxs='i',yaxs='i',bty='l',cex=1.2,cex.lab=1.5)
  plot_ridge (S,Y.prof,u,60)
  abline(v=Srange,col=2,lty=2)
  title(main='Rmax Profile')    
  par(new = TRUE)
  plot(S/u,EG.Smax()$S.prof,type='l', ylim=c(0,1),lwd=1.5, col=6,axes=FALSE,yaxs='i',xlab='',ylab='',main='') 
  axis(4)
  out <- recordPlot()
  return(out)
})

plt_yield_r <- reactive({
  S <- Yield_gl()$prof.ma$S
  Y.prof <- Yield_gl()$Dat.Prof
  u <- unit()
  Srange <- Yield_gl()$range$b.p/u
  par(xaxs='i',yaxs='i',bty='l',cex=1.2,cex.lab=1.5)
  plot_ridge (S,Y.prof,u,60)
  abline(v=Srange,col=2,lty=2)
  title(main='Target Yield Profile')
  par(new = TRUE)
  plot(S/u,Yield_gl()$prof.ma$prof.m,type='l', ylim=c(0,1),lwd=1.5, col=6,axes=FALSE,yaxs='i',xlab='',ylab='',main='') 
  axis(4)
  out <- recordPlot()
  return(out)  
})

plt_rec_r <- reactive({
  S <- Rec_gl()$prof.ma$S
  Y.prof <- Rec_gl()$Dat.Prof
  u <- unit()
  Srange <- Rec_gl()$range$b.p/u
  par(xaxs='i',yaxs='i',bty='l',cex=1.2,cex.lab=1.5)
  plot_ridge (S,Y.prof,u,60)
  abline(v=Srange,col=2,lty=2)
  title(main='Target Recruit Profile')
  par(new = TRUE)
  plot(S/u,Rec_gl()$prof.ma$prof.m,type='l', ylim=c(0,1),lwd=1.5, col=6,axes=FALSE,yaxs='i',xlab='',ylab='',main='') 
  axis(4)
  out <- recordPlot()
  return(out)
})
