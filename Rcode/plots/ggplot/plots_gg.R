#source("Rcode/plots/ggplot/ggplot_functions.R")  # ggplot functions
#'==============================================================================
# plots_gg.R 
# This file includes all graphical output functions used in Shiny Server
# Include this when producing ggplot outputs  
#'==============================================================================
#'------------------------------------------------------------------------------
# DATA plots functions----
#'------------------------------------------------------------------------------
## plt_runesc -------------------------------------------------------------------
#'  Plot run and escapement timeseries 
#'------------------------------------------------------------------------------
# Lines 307&340 are now vestigial and can be removed after testing
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
#'------------------------------------------------------------------------------
## plt_srt -------------------------------------------------------------------
#'  Plot recruit and escapement timeseries 
#'------------------------------------------------------------------------------
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
                         labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
      labs(x=paste('Year'),y=paste('Abundance',mult(u)))                   
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

#'------------------------------------------------------------------------------
## plt_lnsrt -------------------------------------------------------------------
#'  Plot lnRS and escapement timeseries 
#'------------------------------------------------------------------------------
# Under development 


#'------------------------------------------------------------------------------
## plt_hist.sry -------------------------------------------------------------------
#'  Plot Spawner and recruitment histogram 
#'------------------------------------------------------------------------------
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
    p1 <- gg_hist(df,S,u,df$S)+xlab(paste('Escapement',mult(u)))
    p2 <- gg_hist(df,R,u,df$R)+xlab(paste('Run',mult(u)))
    p3 <- gg_hist(df,H,u,df$H)+xlab(paste('Harvest',mult(u)))
  p4 <- gg_hist(df,HR,1,df$HR)+xlab(paste('Harvest Rate',mult(1)))
  p4 <- plot_grid(p2,p1,p3,p4,nrow=1)
  return(p4)
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
      p1 <- plot_density_gg(SR.post.i()$post,D,ar1,input$Model,input$target,u)
    } else {
      p1 <- plot_density_gg(SR.post()$post,D,ar1,input$Model,input$target,u)    
    }
  }else{
    p1 <- plot_density_gg(SR.post()$post,D,ar1,input$Model,input$target,u)      
  }
  #--- Plot output----------------------------------------------------------------  
  return(p1)  
})

#'------------------------------------------------------------------------------
## plt_lnalphai ----------------------------------------------------------------
#'  Plot annual lnalpha timeseries (when TVA model is selected)
#'------------------------------------------------------------------------------
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

#'------------------------------------------------------------------------------
## plt_SS ----------------------------------------------------------------
#'  Plot Run, Harvest, and escapement 
#'------------------------------------------------------------------------------
plt_SS <- eventReactive(isTRUE(SS()),{
    u <- unit()
    run <- tbl_run()
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

#'------------------------------------------------------------------------------
## plt_SS_Age  ----------------------------------------------------------------
#'  Plot Run Age composition 
#'------------------------------------------------------------------------------
plt_SS_Age <- eventReactive(isTRUE(SS()),{
# Import run data 
  run <- tbl_run()
# trim data 
  # Create trimmed year   
  trimyear <- seq(ss.year()[1],ss.year()[2])    
# Extract Year matches trimyear  
  run <- run[run$Year %in% trimyear,]
# Extract observed run age comp   
#  page <- proportions(as.matrix(run[,substr(names(run),1,1) =='A']),margin = 1) 
  #ob.page <- melt(run[,-(2:3)],id.vars='Year',variable.name='Age',value.name='ob.p')
  # R Base reshape: replace reshape2 tidyr
  ob.page <- reshape(run[,-(2:3)],direction='long', idvar='Year',varying = names(run[,-(2:3)])[-1],
                 v.names='ob.p',timevar='Age',times=names(run[,-(2:3)])[-1])
  
#  ob.page <- pivot_longer(run[, -c(2:3)], cols = -Year, names_to = "Age", values_to = "ob.p")
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
# Replace facet_rep_wrap: (package lemon) to ggplot2 syntax  
  facet_wrap(~Age,scales='free_y',ncol=1,axes='all',axis.labels='margins')+
#  scale_y_continuous(expand=c(0.05,0), limits=~c(with(dat.ssp, min(lci,ob.p), max(uci,ob.p))))+
#  scale_x_continuous(expand=c(0,0.5), limits=~c(with(dat.ssp,min(Year), max(Year))),n.breaks = 10)+
  xlab('Year') + ylab('Proportion')
  return(p1)  
 })

#'------------------------------------------------------------------------------
## plt_SS_BAge ----------------------------------------------------------------
#'  Plot Brood age composition  
#'------------------------------------------------------------------------------
plt_SS_BAge <- eventReactive(isTRUE(SS()),{
  # Extract Year matches trimyear  
  nages <- Bayesdata()$nages 
  fage <- Bayesdata()$fage 
  # R Base reshape: replace reshape2 tidyr
  ob.page <- reshape(brood.p(),direction='long', idvar='b.Year',varying = names(brood.p())[-1],
                     v.names='ob.p',timevar='Age',times=names(brood.p())[-1])
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
  facet_wrap(~Age,scales='free_y',ncol=1,axes='all',axis.labels='margins')+
#  scale_y_continuous(expand=c(0.05,0), limits=~c(with(dat.ssp, min(lci,ob.p), max(uci,ob.p))))+
#  scale_x_continuous(expand=c(0,0.5), limits=~c(with(dat.ssp,min(Year), max(Year))),n.breaks = 10)+
  xlab('Brood Year') + ylab('Proportion')
  return(p1)  
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SR Plots ---- 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'------------------------------------------------------------------------------
## base.pl ----------------------------------------------------------------
#'  Plot base SR, SY, ln(R/S) plot  
#'------------------------------------------------------------------------------
base.pl <- reactive({
  u <- as.numeric(unit())
  xp <- sr.data.0()
  xp2 <- sr.data()
  SRp <- SRp()
  maxS <- ifelse(input$axis,input$maxS*u,max(SRp$S))
  maxR <- ifelse(input$axis,input$maxR*u,round(1.25*max(xp$R,na.rm=TRUE)))
  minY <- ifelse(input$axis,input$Yrange[1]*u,round(min(SRp$Rl-SRp$S,na.rm=TRUE)))
  maxY <- ifelse(input$axis,input$Yrange[2]*u,round(1.25*max(xp$Y,na.rm=TRUE)))
  minlnRS <- ifelse(min(xp$lnRS)>0,0.75*min(xp$lnRS),1.25*min(xp$lnRS))
#'---  Basic SR plot ------------------------------------------------------------
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
  scale_x_continuous(expand=c(0, 0), limits=c(0, maxS), 
                     labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, maxR), 
                     labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
  labs(x=paste('Spawner',mult(u)),y=paste('Recruit',mult(u)))
  
#'-------------------------------------------------------------------------------
#'---  Basic Yield plot ---------------------------------------------------------
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
  scale_x_continuous(expand=c(0, 0), limits=c(0, maxS), 
                     labels = label_number(scale = 1 /u), n.breaks = 10,oob=oob_keep) +
  scale_y_continuous(expand=c(0, 0), limits=c(minY, maxY),
                     labels = label_number(scale = 1 /u), n.breaks = 10,oob=oob_keep)+
   labs(x=paste('Spawner',mult(u)),y=paste('Yield',mult(u)))
  
#'---  Basic lnSR plot ---------------------------------------------------------
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
  scale_x_continuous(expand=c(0, 0), limits=c(0, maxS), 
                     labels = label_number(scale = 1 /u), n.breaks = 10,oob=oob_keep) +
  scale_y_continuous(expand=c(0, 0), 
                     n.breaks = 10,oob=oob_keep)+
   labs(x=paste('Spawner',mult(u)),y=paste('ln(R/S)'))
  
#'-------------------------------------------------------------------------------
#'----  Plots Output ------------------------------------------------------------  
  return(list(base.r=gsr,base.y=gsy,base.ln = glnRS))
})

#'------------------------------------------------------------------------------
## base.sr  ----------------------------------------------------------------
#' Base plots with 95% CI/PI range  
#'------------------------------------------------------------------------------
base.sr <- reactive({
  SRp <- SRp()
  p1 <- base.pl()$base.r
  p2 <- base.pl()$base.y
  p3 <- base.pl()$base.ln
#' Plot base SR Plot with CI  ---------------------------------------------------  
  p1 <- p1 +
    geom_ribbon(data = SRp, aes(x = S, ymin = Rl, ymax = Ru), 
                linetype = 0, color = 'grey',alpha = 0.1 )+
    geom_line(data = SRp, aes(x = S, y = Ru.p), color = "grey", linetype = 2) +    
    geom_line(data = SRp, aes(x = S, y = Rl.p), color = "grey", linetype = 2)  
  p2 <- p2 +
    geom_ribbon(data = SRp, aes(x = S, ymin = (Rl-S), ymax = (Ru-S)), 
                linetype = 0, color = 'grey',alpha = 0.1 )+
    geom_line(data = SRp, aes(x = S, y = Ru.p-S), color = "grey", linetype = 2) +    
    geom_line(data = SRp, aes(x = S, y = Rl.p-S), color = "grey", linetype = 2)
  p3 <- p3 +
    geom_ribbon(data = SRp, aes(x = S, ymin = log(Rl/S), ymax = log(Ru/S)), 
                linetype = 0, color = 'grey',alpha = 0.1 )+
    geom_line(data = SRp, aes(x = S, y = log(Ru.p/S)), color = "grey", linetype = 2) +    
    geom_line(data = SRp, aes(x = S, y = log(Rl.p/S)), color = "grey", linetype = 2)
   return(list(base.r=p1,base.y=p2,base.ln = p3))  
})

#'------------------------------------------------------------------------------
## plt_SRY ----------------------------------------------------------------
#'  Plot SR, SY, ln(R/S), SRG:  This goes to Shiny interface 
#'------------------------------------------------------------------------------
plt_SRY <- reactive({
  SRp <- SRp()   # Predicted 
  xp <- sr.data()  # SR plot datat
  dyear <- floor(xp$Yr/10)*10   # Decades 
  ndyear <- (dyear-min(dyear))/10+1
  df.okabe <- data.frame(okabe)
  df.okabe$level <- row.names(df.okabe)
  cdyear <- merge(data.frame(ndyear),df.okabe,by.x='ndyear',by.y='level') 
  labels <- unique(dyear)
  colp <- okabe # Decades color pallette
  maxR <- round(1.25*max(xp$R,na.rm=TRUE))
# Create data for TVA model 
if(input$add=='kf'){
    df.tva <- kf.data()$df
    nstar <- kf.data()$nstar
    ny <- kf.data()$ny
    star <- kf.data()$star    
    dyear <- rep(c(1:nstar),ny)     
#    colp <- 1:nstar+1
    colp <- okabe
    labels <- star$txt
   }   

#' CI or PI data ----------------------------------------------------------------
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
  den <- apply(out[,pcol],2,FUN=function(x) trimden(x))  
#'------------------------------------------------------------------------------  
  
#' Draw Base SR   and YLD Plot -------------------------------------------------
  p1 <- base.pl()$base.r
  p2 <- base.pl()$base.y
  p3 <- base.pl()$base.ln
  p4 <- base.pl()$base.r
  
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
    scale_color_manual(values=colp,labels=star$txt)  
#'..............................................................................  
  p3 <- p3+
    geom_point(data = xp, aes(x = S, y = log(R/S), color=as.factor(dyear)), size = 3)+    
    geom_line(data = df.tva, aes(x = S, y = log(R/S),color=star), 
              linetype = "dashed", linewidth = 0.7) +
    scale_color_manual(values=colp,labels=star$txt) 
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
  p4 <- p4 +
    geom_ribbon(data = SRp, aes(x = S, ymin = lwr, ymax = upr), 
                linetype = 0, color = 'grey',alpha = 0.1 )+
    geom_line(data = SRp, aes(x = S, y = Ru.p), color = "grey", linetype = 2) +    
    geom_line(data = SRp, aes(x = S, y = Rl.p), color = "grey", linetype = 2)    
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
#'..............................................................................    
  p4 <- p4 + 
    geom_point(data = xp, aes(x = S, y = R, color = as.factor(dyear)), size = 3) + 
    scale_color_manual(values = colp,labels=labels)    
        }  # End show.points  

#'------------------------------------------------------------------------------  
#'------------------------------------------------------------------------------
#'  Add Seq, Smsy, Smax, Sgen
#' -----------------------------------------------------------------------------
#'---------- Add Seq ----------------------------------------------------------
  if(input$show.seq==TRUE){
    p1 <- p1 +
      geom_vline(data = legend_data[grepl("Seq", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")  
#...............................................................................
    p2 <- p2 +
      geom_vline(data = legend_data[grepl("Seq", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")  
#...............................................................................
    p3 <- p3 +
      geom_vline(data = legend_data[grepl("Seq", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")  
#...............................................................................  
    df <- data.frame(x = den[[1]]$x, y = den[[1]]$y)
    p4<- p4 + 
      # Under construction
      geom_line(data = df, aes(x = x, y = maxR*y/max(y)), color = okabe[1]) +
      geom_polygon(data = df, aes(x = x, y = maxR*y/max(y)), fill = okabe[1], alpha = 0.2) +
      geom_vline(data = legend_data[grepl("Seq", legend_data$label),], 
                 aes(xintercept = x,linetype = label),color=2,key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")  
  }
  
  
#'---------- Add Smsy ----------------------------------------------------------
  if(input$show.smsy==TRUE){
    p1 <- p1 +
      geom_vline(data = legend_data[grepl("Smsy", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")  
#...............................................................................
    p2 <- p2 +
      geom_vline(data = legend_data[grepl("Smsy", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")  
#...............................................................................
    p3 <- p3 +
      geom_vline(data = legend_data[grepl("Smsy", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")  
#...............................................................................  
    df <- data.frame(x = den[[2]]$x, y = den[[2]]$y)
    p4<- p4 + 
      # Under construction
      geom_line(data = df, aes(x = x, y = maxR*y/max(y)), color = okabe[3]) +
      geom_polygon(data = df, aes(x = x, y = maxR*y/max(y)), fill = okabe[3], alpha = 0.2) +
      geom_vline(data = legend_data[grepl("Smsy", legend_data$label),], 
               aes(xintercept = x,linetype = label),color=3,key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")  
     }
  
#.............. Add Smax       
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
#...............................................................................  
    p3 <- p3 +
      geom_vline(data = legend_data[grepl("Smax", legend_data$label),], 
                 aes(xintercept = x, linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")
#...............................................................................  
    df <- data.frame(x = den[[4]]$x, y = den[[4]]$y)
    p4<- p4 + 
      # Under construction
      geom_line(data = df, aes(x = x, y = maxR*y/max(y)), color = okabe[4]) +
      geom_polygon(data = df, aes(x = x, y = maxR*y/max(y)), fill = okabe[4], alpha = 0.2) +
      geom_vline(data = legend_data[grepl("Smax", legend_data$label),], 
                 aes(xintercept = x,linetype = label),color=4,key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label))+
      guides(orientation ="horizontal")  
  }
#............  Add Sgen  .......................................................
  if(input$show.sgen==TRUE){
    p1 <- p1 +
      geom_vline(data = legend_data[grepl("Sgen", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")
#...............................................................................  
    p2<- p2 +
      geom_vline(data = legend_data[grepl("Sgen", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")
#...............................................................................  
    p3<- p3 +
      geom_vline(data = legend_data[grepl("Sgen", legend_data$label),], 
                 aes(xintercept = x,linetype = label),key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")
#...............................................................................  
    df <- data.frame(x = den[[3]]$x, y = den[[3]]$y)
    p4<- p4 + 
      # Under construction
      geom_line(data = df, aes(x = x, y = maxR*y/max(y)), color = okabe[5]) +
      geom_polygon(data = df, aes(x = x, y = maxR*y/max(y)), fill = okabe[5], alpha = 0.2) +
      geom_vline(data = legend_data[grepl("Sgen", legend_data$label),], 
                 aes(xintercept = x,linetype = label),color=5,key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")  
  }
  
#' Case RE Elements ------------------------------------------------------------  
  if(RE()){
    S <- RE.post()
    S <- data.frame(S,xp)
    if(isTRUE(input$show.points)|input$add=='kf'){
      cols <- cdyear$okabe
    } else {cols <- 'gray'}
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
      cols <- cdyear$okabe
    } else {cols <- 'gray'}

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
  
  return(list(pltSR=p1, pltYD=p2,pltLN = p3,pltBRp=p4))  
 })


## plt_ehrate -------------------------------------------------------------------
#'  Plot harvest rate and escapement 
#'------------------------------------------------------------------------------
plt_kobe <- reactive({
  if(input$dataType== "Run"){
    x <- data()[,c(1:3)]
    names(x) <-c('Yr','S','R')
    x$ex <- with(x,(R-S)/R)
    dyear <- floor(x$Yr/10)*10   # Decades 
    labels <- unique(dyear)
    colp <- okabe # Decades color palette   
    u <- unit()
    legend_data <- br.data()
    Umsy <- legend_data[grepl("Umsy", legend_data$label),]
    Smsy <- legend_data[grepl("Smsy", legend_data$label),]
    p1 <- ggplot()+
      # Add shades
      annotate("rect", xmin = Smsy$x , xmax = Inf, ymin = 0, ymax = Umsy$x, 
               fill = okabe[3], alpha = 0.2) +
      annotate("rect", xmin = 0 , xmax = Smsy$x, ymin = Umsy$x, ymax = 1.0, 
               fill = okabe[6], alpha = 0.2) +
      # Add shades
      annotate("rect", xmin = Smsy$x , xmax = Inf, ymin = Umsy$x, ymax = 1.0, 
               fill = okabe[1], alpha = 0.2) +
      annotate("rect", xmin = 0 , xmax = Smsy$x, ymin = 0, ymax = Umsy$x, 
               fill = okabe[4], alpha = 0.2) +
            geom_point(data=x,aes(x=S,y=ex,color = as.factor(dyear)), size = 3)+
      scale_color_manual(values = colp,labels=labels)+
      geom_text_repel(data = x, aes(x = S, y = ex, label = as.character(Yr)), 
                      size = 5, nudge_x = 0.1, nudge_y = 0.01, max.overlaps = Inf)+
      geom_vline(data = Smsy,aes(xintercept = x,linetype = label),color=1,key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")+
      geom_hline(data = Umsy,aes(yintercept = x,linetype = label),color=1,key_glyph='path') +
      scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
      guides(orientation ="horizontal")+
      # Second y axis       
      scale_y_continuous(expand=c(0,0),limits=c(0,1.0))+
      scale_x_continuous(expand=expansion(mult = c(0, .25)), limits=c(0, max(x$S)),
                         labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+
      ylab(paste('Harvest Rate')) + xlab(paste('Escapement',mult(u)))
    
    return(p1)   
  }
})


#'------------------------------------------------------------------------------
# Profile Analyses     -------------------------
#'------------------------------------------------------------------------------
#'------------------------------------------------------------------------------
## plt_cg_prof ----------------------------------------------------------------
#'  Plot Custom escapement goal profile plor
plt_cg_prof <- reactive({
  SS <- c(input$lg,input$ug)*unit()
# Smsy prof (p1) and Smax prof(p2)
    # Inport basic profile plot 
  p1 <- plt.msy.prof()
  p2 <- plt.max.prof()
  if(SS[1]==SS[2]){
  p1 <- p1+ geom_vline(xintercept =SS[1],color=3,linewidth =2)
  p2 <- p2+ geom_vline(xintercept =SS[1],color=4,linewidth =2)
    } else {
  p1 <- p1+
  annotate('rect', xmin = SS[1], xmax = SS[2], ymin = 0, ymax = 1, alpha=0.1, fill=3) 
  p2 <- p2+
  annotate('rect', xmin = SS[1], xmax = SS[2], ymin = 0, ymax = 1, alpha=0.1, fill=4) 
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
 leg.tx <- c('Annual',ifelse(input$target =='me','Mean','Median'))
   p1 <- mult.den.plt(Y.p,Y.m,'Yield',leg.tx[2],u)+    
   geom_vline(xintercept = m, color = 4)+
   geom_vline(xintercept = yg, color = 2)
   return(p1)
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
   p1 <- mult.den.plt(R.p,R.m,'Recruit',leg.tx[2],u)+    
   geom_vline(xintercept = m, color = 4)+
   geom_vline(xintercept = rg, color = 2)
  return(p1)
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
  p1 <- ggplot(prof.ma)+
    geom_line(aes(x=S,y=prof.m,linetype=tex))+
    geom_line(aes(x=S,y=prof.a,linetype='Annual'))+
    geom_hline(yintercept=tpg, col=2)+
    scale_x_continuous(expand=c(0,0),limits=c(0,NA),labels = label_number(scale = 1/u),n.breaks = 10,oob=oob_keep)+
    scale_y_continuous(expand=c(0,0),limits=c(0,1),n.breaks = 10,oob=oob_keep)+
    labs(x=paste('Escapement',mult(u)),y='Probability')+
    annotate('rect', xmin = srange[1], xmax = srange[2], ymin = 0, ymax =1, alpha=0.1, fill=3,na.rm = TRUE)+
    scale_linetype_manual(name ="", values=c(2,1))+
    labs(title = 'Target Recruit Profile Analyses')
  return(p1)
})

#'------------------------------------------------------------------------------
## plt_yield.prof ----------------------------------------------------------------
#'  Plot expected recruit profile density 
#'------------------------------------------------------------------------------
plt_yield.prof <- reactive({
  tex <- ifelse(input$target =='me','Mean','Median')
  srange <- as.numeric(Yield_gl()$range$b.p)
  prof.ma <- Yield_gl()$prof.ma
  tpg <- input$y1p/100
  u <- unit()
  p1 <- ggplot(prof.ma)+
    geom_line(aes(x=S,y=prof.m,linetype=tex))+
    geom_line(aes(x=S,y=prof.a,linetype='Annual'))+
    geom_hline(yintercept=tpg, col=2)+
    scale_x_continuous(expand=c(0,0),limits=c(0,NA),labels = label_number(scale = 1/u),n.breaks = 10,oob=oob_keep)+
    scale_y_continuous(expand=c(0,0),limits=c(0,1),n.breaks = 10,oob=oob_keep)+
    labs(x=paste('Escapement',mult(u)),y='Probability')+
    annotate('rect', xmin = srange[1], xmax = srange[2], ymin = 0, ymax =1, alpha=0.1, fill=3,na.rm = TRUE)+
    scale_linetype_manual(name ="", values=c(2,1))+
    labs(title = 'Target Yield Profile Analyses')
  return(p1)
})

#'------------------------------------------------------------------------------
#  Plot Ridge plot ---- 
#'------------------------------------------------------------------------------
plt_msyprof_r <- reactive({
  S <- EG.Smsy()$S
  Y.prof <- EG.Smsy()$Dat.prof
  u <- unit()
  Srange <- EG.Smsy()$S.Range
  df <- data.frame(S=EG.Smsy()$S,S.prof=EG.Smsy()$S.prof)
  n <- 60
  inc <- median(apply(Y.prof,1,max,na.rm=TRUE))
  ry <- inc*n
  p1 <- plot_ridge(S,Y.prof,u,n)+
  geom_line(data=df,aes(x=S,y=ry*S.prof),color=6)+ggtitle('MSY Profile')
  if(!is.na(sum(Srange))) p1 <- p1+  geom_vline(xintercept = Srange, color = 2, linetype=2)
return(p1)
})

plt_maxprof_r <- reactive({
  S <- EG.Smax()$S
  Y.prof <- EG.Smax()$Dat.prof
  u <- unit()
  Srange <- EG.Smax()$S.Range
  df <- data.frame(S=EG.Smax()$S,S.prof=EG.Smax()$S.prof)
  n <- 60
  inc <- median(apply(Y.prof,1,max,na.rm=TRUE))
  ry <- inc*n
  p1 <- plot_ridge(S,Y.prof,u,n)+ 
    geom_line(data=df,aes(x=S,y=ry*S.prof),color=6)+ggtitle('Rmax Profile')
if(!is.na(sum(Srange))) p1 <- p1+geom_vline(xintercept = Srange, color = 2, linetype=2)
  return(p1)
  })

plt_yield_r <- reactive({
  S <- Yield_gl()$prof.ma$S
  Y.prof <- Yield_gl()$Dat.Prof
  u <- unit()
  Srange <- Yield_gl()$range$b.p
  df <- data.frame(S=Yield_gl()$prof.ma$S,S.prof=Yield_gl()$prof.ma$prof.m)
  n <- 60
  inc <- median(apply(Y.prof,1,max,na.rm=TRUE))
  ry <- inc*n
  p1 <- plot_ridge(S,Y.prof,u,n)+
    geom_line(data=df,aes(x=S,y=ry*S.prof),color=6)+ggtitle('Target Yield Profile')
  if(!is.na(sum(Srange))) p1 <- p1+  geom_vline(xintercept = Srange, color = 2, linetype=2)
  return(p1)
})

plt_rec_r <- reactive({
  S <- Rec_gl()$prof.ma$S
  Y.prof <- Rec_gl()$Dat.Prof
  u <- unit()
  Srange <- Rec_gl()$range$b.p
  df <- data.frame(S=Rec_gl()$prof.ma$S,S.prof=Rec_gl()$prof.ma$prof.m)
  n <- 60
  inc <- median(apply(Y.prof,1,max,na.rm=TRUE))
  ry <- inc*n
  p1 <- plot_ridge(S,Y.prof,u,n)+
    geom_line(data=df,aes(x=S,y=ry*S.prof),color=6)+ggtitle('Target Recruit Profile')
  if(!is.na(sum(Srange))) p1 <- p1+geom_vline(xintercept = Srange, color = 2, linetype=2)
  return(p1)
})

