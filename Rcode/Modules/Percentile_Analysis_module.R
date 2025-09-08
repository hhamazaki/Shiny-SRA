#'===============================================================================
#  3.0 Percentile Based Goal Analyses Module ----
#'===============================================================================
#'===============================================================================
#  PerentileUI Module: Produce percentile analyses and plot results  
#  Usage: 
#  UI section 
#  PerentileUI("ns.name")
#  Server section
#  PerentileUIServer("ns.name",e.data,u)
#'===============================================================================
PercentileUI <- function(id){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    selectInput(ns("Tiers"),"Tiers", 
                choices = c('Tier 1','Tier 2','Tier 3','Other'))
  )  
}

#' Output Module ----------------------------------------------------------------
PercentileServer <- function(id,e.data,u,plt){
  moduleServer(
    id,
    function(input, output, session){
#'-------------------------------------------------------------------------------
# Txt_Tier:  Explanation of Tiers 
#'-------------------------------------------------------------------------------  
  Txt_Tier <- reactive({
   txt<- HTML(
    if(input$Tiers =="Tier 1"){
        paste("Escapement goal criteria",
        "High contrast (> 8)",
        "High measurement error (aerial or foot surveys)",
        "Low to moderate average harvest rate (<40%)",
        "Goal Range: 20th - 60th percentile",sep = '<br/>')
 } else if(input$Tiers == "Tier 2")  {
        paste("Escapement goal criteria",
        "High contrast (> 8)",
        "Low measurement error (weir or tower surveys)",
        "Low to moderate average harvest rate (<40%)",
        "Goal Range: 15th - 65th percentile",sep = '<br/>')
  } else if(input$Tiers == "Tier 3"){
        paste("Escapement goal criteria",
        "Low contrast (< 8)",
        "Low to moderate average harvest rate (<40%)",
        "Goal Range: 5th - 65th percentile",sep = '<br/>')
   } else {
        paste("High average harvest rate (>40%)",
        "Percentile method not applicable but a goal range is needed",
        "Suggested Goal Range: 25th - >75th percentile",sep = '<br/>')  
          }
    )
    return(txt)
  })
#'-------------------------------------------------------------------------------
# EGS:  Perecentile goals   
#'-------------------------------------------------------------------------------  
  EGS <- reactive({
    S <- e.data()$S
  # Percentile Analyses in 3 Tiers 
    e.g.1 <- (quantile(S,c(0.2,0.6),na.rm=TRUE))   #Tier 1
    e.g.2 <- (quantile(S,c(0.15,0.65),na.rm=TRUE)) #Tier 2
    e.g.3 <- (quantile(S,c(0.05,0.65),na.rm=TRUE)) #Tier 3
    e.g.4 <- (quantile(S,c(0.25,0.75),na.rm=TRUE)) #Tier 4
    e.g <- data.frame(rbind(e.g.1,e.g.2,e.g.3,e.g.4))
    names(e.g) <- c('EGL','EGU')
    return(e.g)
  })
      
#,-------------------------------------------------------------------------------
# Plt_prcnt:  Plot Percentile  
#,-------------------------------------------------------------------------------  
  Plt_prcnt <- function(){
    mult <- mult(u)
    EG <- EGS()
    x <- e.data()
     if(input$Tiers == "Tier 1") { e.g <- as.numeric(EG[1,])
       } else if( input$Tiers == "Tier 2") { e.g <- as.numeric(EG[2,])     
       } else if(input$Tiers == "Tier 3") { e.g <- as.numeric(EG[3,])       
      } else {e.g <- as.numeric(EG[4,])}
  # Graphics   
    if(plt=='base') {        
      layout(matrix(1:2, ncol=2), widths=c(3, 1))
       par(yaxs='i',bty='l',las=1,mar=c(4,4,4,4))
        plot(S/u~Yr,data=x,type='l',ylim=c(0,max(x$S,na.rm=TRUE)/u),xlab='',ylab='')
        title("Escapement", xlab="Year",ylab=paste('Escapement',mult))
          # Add Escapement Goal range  
        polygon(with(x,c(min(Yr),max(Yr),max(Yr),min(Yr))),c(e.g[1]/u,e.g[1]/u,e.g[2]/u,e.g[2]/u),col=tcol(7,50),border=NA)
    # Alternative: 
      abline(h=EG[1,]/u,col = ifelse(input$Tiers == "Tier 1",7,2), lty=2,lwd=ifelse(input$Tiers == "Tier 1",2,1))
      abline(h=EG[2,]/u,col = ifelse(input$Tiers == "Tier 2",7,3), lty=2,lwd=ifelse(input$Tiers == "Tier 2",2,1))
      abline(h=EG[3,]/u,col = ifelse(input$Tiers == "Tier 3",7,4), lty=2,lwd=ifelse(input$Tiers == "Tier 3",2,1))
      abline(h=EG[4,]/u,col = ifelse(input$Tiers == "Other",7,4), lty=2,lwd=ifelse(input$Tiers == "Other",2,1))  
    # EG      
      lines(S/u~Yr,data=x)
      txt <- c('Tier 1','Tier 2','Tier 3','Other')
      cols <- c(ifelse(input$Tiers == "Tier 1",7,2),ifelse(input$Tiers == "Tier 2",7,3),ifelse(input$Tiers == "Tier 3",7,ifelse(input$Tiers == "Other",7,4)))
      lwds <- c(ifelse(input$Tiers == "Tier 1",2,1),ifelse(input$Tiers == "Tier 2",2,1),ifelse(input$Tiers == "Tier 3",2,ifelse(input$Tiers == "Other",2,1)))
      fonts <- c(ifelse(input$Tiers == "Tier 1",2,1),ifelse(input$Tiers == "Tier 2",2,1),ifelse(input$Tiers == "Tier 3",2,ifelse(input$Tiers == "Other",7,1)))
      add_legend('left',legend=txt, col=cols, lwd=lwds,lty=2, text.font=fonts, box.lty=0)  
      out <- recordPlot()
      return(out)
  } else {
 # Graphics:  ggplot 
  ggplot()+ geom_line(data=x, aes(x=Yr, y=S))+
  annotate("rect", ymin=(e.g[1]),ymax=e.g[2],xmax=Inf,xmin=-Inf,fill=2,alpha = 0.1)+
   scale_y_continuous(expand=c(0, 0), limits=c(0, NA), 
      labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
   scale_x_continuous(expand=c(0, 0.5),n.breaks = 10)+
      ylab(paste('Escapement',mult(u))) + xlab('Year')      
       }  # End ggplot     
  }
      
 Plt_prcnt_gg <- function(){
   EG <- EGS()
   x <- e.data()
    if(input$Tiers == "Tier 1") { e.g <- as.numeric(EG[1,])
     } else if( input$Tiers == "Tier 2") { e.g <- as.numeric(EG[2,])    
     } else if(input$Tiers == "Tier 3") { e.g <- as.numeric(EG[3,])       
     }  else {e.g <- as.numeric(EG[4,])}
   # Graphics
        ggplot()+
          geom_line(data=x, aes(x=Yr, y=S))+
          annotate("rect", ymin=(e.g[1]),ymax=e.g[2],xmax=Inf,xmin=-Inf,fill=2,alpha = 0.1)+
          scale_y_continuous(expand=c(0, 0), limits=c(0, NA), 
                             labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
          scale_x_continuous(expand=c(0, 0.5),n.breaks = 10)+
          ylab(paste('Escapement',mult(u))) + xlab('Year')      
        
      }
      
#,-------------------------------------------------------------------------------
# Txt_Note:  Explanation of Tiers 
#,-------------------------------------------------------------------------------  
  Txt_Note <- reactive({
    x <- e.data()
    EG <- EGS()
    contrast <- round(max(x$S,na.rm=TRUE)/min(x$S,na.rm=TRUE),1)
        if(input$Tiers == "Tier 1") { e.g <- EG[1,]
        } else if(input$Tiers == "Tier 2") { e.g <- EG[2,]     
        } else if(input$Tiers == "Tier 3") { e.g <- EG[3,]       
        } else {e.g <- EG[4,]}
        
        txt <- HTML(paste(paste("Escapement goal range"),
                          paste0(round(e.g[1],0)," - ",round(e.g[2],0)),
                          paste("Escapement Contrast:",contrast),
                          sep = '<br/>'))
        return(txt)
      })
      
      Tier <- reactive({input$Tiers})
#,-------------------------------------------------------------------------------
# Module Outputs: Txt_Tier, Txt_Note, Plt_prcnt 
#,-------------------------------------------------------------------------------
      outdata <- list(Txt_Tier =Txt_Tier, Tier=Tier, EGS = EGS, 
                      Txt_Note=Txt_Note, Plt_prcnt=Plt_prcnt)
      return(outdata)      
    } # End function 
  ) # End moduleServer
} # End prcntoutServer
