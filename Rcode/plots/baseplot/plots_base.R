#'==============================================================================
# plots_base.R 
# This file includes all graphical output functions used in Shiny Server
# Include this when producing baseplots outputs  
#'==============================================================================
# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") ----------
plot_runesc <- function(dat,u){
     par(yaxs='i',bty='l')
     plot(R/u~Yr,data=dat,type='l',ylim=c(0,with(dat,max(R,S,na.rm =TRUE)/u)),xlab='',ylab='')
     lines(S/u~Yr,data=dat,lty=2)
 }


# Plt_runesc -------------------------------------------------------------------
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
#'----  Plots Output -----------------------------------------------------------  
     out <-recordPlot()  
     return(out)    
    }
 })
 

# Plt_srt ----------------------------------------------------------------------
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

# Plt_lnsrt --------------------------------------------------------------------
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
