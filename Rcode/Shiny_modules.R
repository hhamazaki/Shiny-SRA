#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Modules 
#  Shiny Modludes consists of nameUI and nameServer functions 
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'==============================================================================
# Info: add show info section  ----
#'==============================================================================
#'  UI ----------------------- 
InfoUI <- function(id,info.name) {
  ns <- NS(id)
    p(strong(HTML(
    paste0(info.name,
      as.character(actionLink(ns('info'),"", icon = icon("circle-info")))
      )
     )
    )
  )
}
#'  Server  ----------------------- 
InfoServer <- function(id,info.name,info.texts){
  moduleServer(
    id,
    function(input, output, session){
observeEvent(input$info, {
  showModal(modalDialog(
    title = info.name,
    footer = NULL, info.texts
          ,
        easyClose = TRUE
      ) # End modalDialog
    ) # End showModal
    }
    )# End ObserverEvent
    } # End function 
  ) # End moduleServer
} # End InfoServer

#'==============================================================================
#  dataInput: Upload data file to Shiny  ---------------
#'==============================================================================
#  Usage: UI section 
#  dataInputUI("ns.name", "User data (.csv format)")
#  Usage: Server section: dataInputServer("datain")
#'==============================================================================
#' UI  ------------------------------------------------
dataInputUI <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Choose File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Input: Checkbox if file has header ----
    checkboxInput(ns("header"), "Header", TRUE),
    # Input: Select separator ----
    radioButtons(ns("sep"), "Separator",
                 choices = c(Comma = ",", Tab = "\t"), selected = ",")
    ) # End taglist
} # End dataInputUI
#' Server ---------------------------------------------------------------------
dataInputServer <- function(id){
  moduleServer(id,
  function(input, output, session) {
    # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  }) # End userfile
  
# The user's data, parsed into a data frame
  df <- reactive({
    read.csv(userFile()$datapath,
             header = input$header,
             sep = input$sep,
             stringsAsFactors = FALSE)
   })
# We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
    }) # End observe
  return(df)
   } # End fundtion
  ) # End moduleServer
} # End dataInputServer

#'===============================================================================
#  Profile MSY-Rmax Profile Based Goal Analyses Module ----- 
#'===============================================================================
#'===============================================================================
#  ProfileUI ---- 
#  UI section: SmsyprofRUI("ns.name")
#  Server section: Smsyprofserver("ns.name",SR.pred)
#  Output:  EG.Smsy, EG.Smsy.st, Srange.st,  
#'===============================================================================
#'  UI -------------------------------------------------------------------
ProfileUI <- function(id,crit){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    p(strong(paste(crit,"Analyses"))),  
    sliderInput(ns("p.min"), paste("Min % of",crit), value=90,min=0,max=100,step=5),
    sliderInput(ns("p.t"), paste("% Meeting",crit,"Target"), value=90,min=0,max=100,step=5)
      )  # End taglist
    
} # End SmsypfofUI
#'  Server ----------------------------------------------------------------------
#   EG: Escapement Goal based on User defined Profile Analyses
#   EG.st: Escapement Goal based on Standard Profile Analyses
#   BEG: Escapement Goal table based on Profile Analyses
#   Plt.profile: Profile plot without intersections 
#   BEG: Escapement Goal table based on Profile Analyses
#'-------------------------------------------------------------------------------
ProfileServer <- function(id,SR.pred,crit,unit){
  moduleServer(
    id,
    function(input, output, session){
## EG:  User defined profile based goal data -------------------
  EG <- reactive({
        # Import MCMC Expected Mean Yield 
        if(crit == 'MSY'){mc.Y <- SR.pred()$Y}
        else if(crit == 'Rmax') {
           mc.Y <- SR.pred()$R
        }
        # Import S 
        S <- SR.pred()$S
        # Import minimum  %
        p.min <- input$p.min/100
        # Import minimum target %   
        p.t <- input$p.t/100
        # Generate profile using profile function 
        S.prof  <- profile(S, mc.Y, p.min, p.t)
        # Rename Output
        names(S.prof) <- c('S','S.prof','S.Range')
        return(S.prof)  
  }) # End EG
      
## EG.st: Standard profile based goal data ------------------------------
  EG.st <- reactive({
        # Import MCMC Expected Mean Yield
        if(crit == 'MSY'){mc.Y <- SR.pred()$Y}
        else if(crit == 'Rmax'){
          mc.Y <- SR.pred()$R
        }     
        p.t <- input$p.t/100
        # Import S 
        S <- SR.pred()$S
        # Standard min Smsy probability 
        st <- c(0.7,0.8,0.9)
        # Create a dummy matrics 
        ncols <- length(S)
        # Create Standard probability matrix 
        prof.st <- matrix(0,nrow=3,ncol=ncols)
        # Create BEG 
        Srange.st <- matrix(NA,nrow=3,ncol=2)
        for(i in 1:3){
          profile <- profile(S,mc.Y, st[i],p.t)
          prof.st[i,] <- profile$M.prof
          Srange.st[i,] <- profile$S.range
        }
    # Find Profile Intersections 
        out <- list(S.prof.st = prof.st, S.Range.st = Srange.st)
        return(out)   
  }) # End EG.st
      
## BEG: Create EG goal table -------------------------------------------
  BEG <- reactive({
        BEG.st <- EG.st()$S.Range.st
        BEG <- EG()$S.Range
        percent <- c(input$p.min,70,80,90)
        apercent <- input$p.t
        lg <- c(BEG[1],BEG.st[,1])
        ug <- c(BEG[2],BEG.st[,2])
#        t.BEG <- c(paste(percent,'%',crit,apercent,'% target'))
        t.BEG <- data.frame(percent,apercent, lg,ug)
        names(t.BEG) <- c(paste(crit,'%'),'Target %','Lower','Upper')
        return(t.BEG)
      }) # SA.BEG
      
## plt.profile:  Basic Profile fig without frills  ------------------------
  plt.profile <- reactive({
     u <- unit()
      p.min <- input$p.min/100
      percent <- c(input$p.min,70,80,90)
      txt <- c(paste(percent,'%',crit))
    # Generate profile 
      S <- (SR.pred()$S)
      Y.prof <- EG()$S.prof
      Y.prof.st <- data.frame(t(EG.st()$S.prof.st))
      names(Y.prof.st) <- c('p90','p80','p70')
      df <- data.frame(S=SR.pred()$S,Med=EG()$S.prof,Y.prof.st )
      df1.melt <- melt(df,id.vars ='S',variable.name='prof.type',value.name='prob')
      p1 <- ggplot()+
      geom_line(data = df1.melt[which(df1.melt$prof.type != 'Med'),],aes(x=S,y=prob,linetype =prof.type),color=1)+
      geom_line(data = df1.melt[which(df1.melt$prof.type == 'Med'),],aes(x=S,y=prob,linetype = prof.type),color=6)+  
      scale_linetype_discrete(name ="",labels=txt,guide=guide_legend(override.aes=list(color=c(6,1,1,1))))+
     scale_x_continuous(expand=c(0,0),limits=c(0,NA),labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep)+  
     scale_y_continuous(expand=c(0,0),limits=c(0,1),n.breaks = 10,oob=oob_keep)+
     labs(title = paste0(crit,'Profile Analyses'), x=paste('Escapement',mult(u)),y='Probability')      
      return(p1)
   })
  
## plt.pro.fig:  Profile fig with intersections  --------------------------
  plt.prof.fig <- reactive({
     txt <- BEG()
     # Import minimum target %   
      p.t <- input$p.t/100
     c.col <- ifelse(crit=='MSY',3,4)
     eg <- as.numeric(EG()$S.Range)
     p1 <- plt.profile()+
     geom_hline(yintercept = p.t,lwd=1,col=2)+     
     annotate('rect', xmin = eg[1], xmax = eg[2], ymin = -Inf, ymax = Inf, alpha=0.1, fill=c.col)
     return(p1)
    })
    
  p.min <- reactive({input$p.min})
  p.t <- reactive({input$p.t})      
    
  outdata <- list(EG =EG, EG.st = EG.st,BEG=BEG,p.min=p.min, p.t=p.t,plt.profile=plt.profile,plt.prof.fig=plt.prof.fig )
    return(outdata) 
    
    } # End function
  ) # End moduleServer
} # End ProfServer


#'===============================================================================
#  2.0 Risk Based Goal Analyses Module---- 
#'===============================================================================
#===============================================================================
#  RiskUI Module: Produce Risk analyses and plot results  
#  Usage: 
#  UI section 
#  RiskUI("ns.name")
#  Server section
#  RiskServer("ns.name",e.data,u)
#===============================================================================
RiskUI <- function(id){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    sliderInput(ns("risk.k"), "Number of Years", value=3,min=1,max=10,step=1),
    sliderInput(ns("risk.p"), "% Drop", value=90,min=0, max=100,step=5),
    checkboxInput(ns("RiskC"), strong("Custom Risk Analyses"), FALSE), 
    uiOutput(ns('Riskp')),
    uiOutput(ns('RiskEG'))
  )  
}

# Output Module ----------------------------------------------------------------
RiskServer <- function(id,e.data,u){
  moduleServer(
    id,
    function(input, output, session){
 
output$RiskEG <- renderUI({
    ns <- session$ns
     if(input$RiskC){
        mult <- mult(u)
        numericInput(ns("riskEG"), paste("Escapement",mult), value=1,min=0, step= 1)
        }
      })
      
output$Riskp <- renderUI({
  ns <- session$ns
  if(input$RiskC){
  sliderInput(ns("riskp"), "Acceptable Risk",value = 0, min=0,max=1,step=0.05)
  }
  })

#-------------------------------------------------------------------------------
# Risk_sim_base: Produce Risk based simulation: 
# DW test determines AR1 vs Standard 
#-------------------------------------------------------------------------------
  Risk_sim_base <- reactive({
      x <- e.data()
    # calculate ln(S)
        x$lnS <- log(x$S)
        n <- length(x$lnS)
        # Conduct dw test and determine to do AR1 or Standard error model 
        dw <- dwtest(lnS~1,data=x)
        if(dw$p.value < 0.05){
          arima_mod <- arima(x$lnS, order=c(1,0,0))    
          phi <- unname(arima_mod$coef[1])
          mu <- unname(arima_mod$coef[2])
          sig <- sqrt(unname(arima_mod$sigma2))
        } else {
          phi <- 0
          mu <- mean(x$lnS,na.rm=TRUE)
          sig <-sd(x$lnS,na.rm=TRUE)
        }
        model.choice <- paste("Simulation:",
                              ifelse(dw$p.value < 0.05,"AR1","Standard"),"Model")
        cc <- mu*(1-phi)
        sigc <- sig*sqrt((n+1)/n)
        # This makes largest numbers into integer (e.g. 100000)
        D <- floor(log10(mean(x$S,na.rm=TRUE)))
        maxb <- ceiling(mean(x$S,na.rm=TRUE)/(10^D))*(10^D)
        # Cut into 501 segments (can be increased) 
        S <- seq(0,maxb, length.out=501)
        # change 0 to 1  
        S[1] <- 1
        M <- 1000
        # Step 1: create rt 
        t.dist <- rt(M,n-2)*sigc
        out <- list(dw=dw,S=S,cc=cc, phi=phi, mu=mu, md = model.choice, sigc=sigc,M=M,t.dist = t.dist)
      })
      
#-------------------------------------------------------------------------------
# Risk_sim: Produce Risk simulation
#-------------------------------------------------------------------------------
    Risk_sim <- reactive({
        k <- input$risk.k
        p <- input$risk.p/100
        st <- c(0.5, 0.80, 0.9)
        delta <- c(p,st)
        phi <- Risk_sim_base()$phi
        mu <- Risk_sim_base()$mu
        cc <- Risk_sim_base()$cc
        sigc <- Risk_sim_base()$sigc
        cprime <- (1-phi)*log(1-delta)+cc     # vector 
        nd <- length(delta)
        S <-Risk_sim_base()$S
        M <- Risk_sim_base()$M
        lngoal <- log(S)
        # Step 1: create rt 
        t.dist <- Risk_sim_base()$t.dist
# --- Simulation! --------------------------------------------------------------
# Create empty matrix----------------------------------------------------------- 
        xsim <- matrix(nrow=M, ncol=nd+1)
        xsimmax <- matrix(nrow=M-k, ncol=nd+1)
        pi <- matrix(nrow=length(S), ncol=nd+1)
        # Simulation
        # looping
        xsim[1,] <- mu  # initial value starting mu
        for(i in 2:M) { # looping over M sims
          xsim[i,] <- xsim[i-1,]*phi + c(cprime,cc)+ t.dist[i]   # simulated x
        }
        
        for(i in 1:(M-k)) { # looping over M-k max values
          xsimmax[i,] <- apply(xsim[(i+1):(i+k),],2,max)     # simulated k-year max x
        }
        
        int.S <- numeric(nd)
        pi[,1] <- colMeans(outer(X=xsimmax[,length(delta)+1], Y=lngoal, FUN="<="))  # simulated probabilities (unneeded action)
        for(j in 1:nd){
          pi[,j+1] <- colMeans(outer(X=xsimmax[,j], Y=lngoal, FUN=">"))  # simulated probabilities (mistaken inaction)
          int.S[j] <- max(S[pi[,j+1]>pi[,1]])  
        }
        
        txt <- HTML(paste0("Mistaken Inaction at ", 100*delta,"% drop : ",int.S,sep = '<br/>'))
        pi <- as.data.frame(pi)
        names(pi) <- c('Uneeded',paste0('Drop',100*delta,'pcnt'))
        out <- list(S = S, pi = pi,delta=delta,int.S = int.S, txt=txt)
        return(out)   
      })

Risk_custom <- reactive({
    if(input$RiskC){
    Spi <- Risk_sim()$pi
    S <- Risk_sim()$S
  # custom EG goal
    EG <- input$riskEG*u  
  # custom Risk 
    p <- input$riskp
  # Find risk probability based on custom escapement 
    EGS <- sum(ifelse(S>EG,0,1))
    EG.p <-data.frame(as.integer(EG),Spi[EGS,])
    names(EG.p)[1] <- 'Escapement'
  # Find S based on custom risk probability
  # 1 find number of columns 
    n <- dim(Spi)[2]
  # 2 Set vector 
  Sp <- EG.p[,]
  names(Sp)[1] <- 'Risk'
  Sp[,1] <- p
  # Loop 
  for(i in 1:n){
    # find the max number of row that are closest to the custom risk
    EGP <- sum(ifelse(Spi[,i]>p,0,1))  
    # find S that correspond to the row number       
    Sp[,i+1] <-as.integer(S[EGP])
   }
  out <- list(Sp=Sp,EG.p=EG.p) 
  return(out)
  }
})

#-------------------------------------------------------------------------------
# Plt_risk:  Risk analyses plot 
#-------------------------------------------------------------------------------
 Plt_risk <- function(){
      layout(matrix(1:2, ncol=2), widths=c(3,1))   
      mult <- mult(u)
      x <-Risk_sim()$S
      pi <- Risk_sim()$pi
      delta <- Risk_sim()$delta
      n <- length(delta)
      e.g <- Risk_sim()$int.S
      par(xaxs='i',yaxs='i',bty='l')
      plot(x/u, pi[,1], las=1, type='l', ylim=0:1, lwd=2, 
           xlab=paste('Escapement',mult), ylab="Estimated Risk")
      for(j in 1:(n-1)) lines(x/u, pi[,j+1], col=j)
      lines(x/u,pi[,n+1], lwd=2,col=6)
      a <- c(e.g[n],e.g[n])
      b <- c(0,pi[which(x==e.g[n]),n+1])
      lines(a/u,b,lwd=2,col=6)
      a <- c(0,e.g[n])
      b <- c(pi[which(x==e.g[n]),n+1],pi[which(x==e.g[n]),n+1])
      lines(a/u,b,lwd=1,lty=2,col=6)
#  Add custom EG      
      abline(v=input$riskEG, col =4, lty=3,lwd=1)
#  Add custom Risk Prob 
      abline(h=input$riskp, col =7, lty=2,lwd=1)      
      pp <- rep(NA,n)
      for(j in 1:n) pp[j] <- round(pi[which(x==e.g[j]),j+1],2)
      txt <- c("Unneeded action", 
               "Inaction at % drop, EG, Risk",
               paste(100*delta,"% drop", e.g,pp )
               )
      add_legend("left", legend= txt, lwd=c(2,0,rep(1,n-1),2),
             col=c(1,1,1:(n-1),6),box.lty=0,text.font=c(1,2,1,1,1,2))
      out <- recordPlot()
      return(out)
    }
 Plt_risk_gg <- function(){
      mult <- mult(u)
      x <-Risk_sim()$S
      pi <- data.frame(Risk_sim()$pi)
      names(pi) <- c('a','c','d','e','b')
      delta <- Risk_sim()$delta
      n <- length(delta)
      e.g <- Risk_sim()$int.S
      txt <- c("Unneeded action", 
                paste(100*delta,"% drop" )
               )
# create a data.frame 
df <- melt(data.frame(S=x,pi),id.vars=('S'),value.name='p')
 p1 <- ggplot()+
  geom_line(data=df, aes(x=S,y=p,linetype=variable,color=variable, linewidth=variable))+
  scale_linetype_manual(labels=txt,values=c(1,1,2,3,4))+   
  scale_linewidth_manual(labels=txt,values=c(1,1,0.5,0.5,0.5))+
  scale_color_manual(labels=txt,values=c(1,2,3,4,5))+
  scale_x_continuous(expand=c(0, 0), limits=c(0, NA), 
          labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, 1),n.breaks = 10)+
  xlab(paste('Escapement',mult(u))) + ylab('Proabbility')
  if(input$RiskC){
    p1 <- p1+
#  Add custom EG      
   geom_vline(xintercept=input$riskEG*u, color =4, linetype=2,linewidth=1)+
#  Add custom Risk Prob 
   geom_hline(yintercept = input$riskp, color =7, linetype=2,linewidth=1)      
   }
    return(p1)
    }
#-------------------------------------------------------------------------------
# Plt_risk2:  Time series  
#-------------------------------------------------------------------------------
  Plt_risk2 <- function(){
      x <- e.data()      
      par(yaxs='i',bty='l',las=1)
      plot(S/u~Yr,data=x,type='l',ylim=c(0,with(x,max(S,na.rm=TRUE)/u)),xlab='',ylab='')
      title("Escapement", xlab="Year",
            ylab=paste('Escapement',mult(u))) 
      # Add Escapement Goal  
      e.g <- Risk_sim()$int.S[4]
      abline(h=e.g/u,col=2)
      # Add custom Escapement Goal 
      abline(h=input$riskEG, col =4, lty=3,lwd=2)
      out <- recordPlot()
      return(out)
  }
   Plt_risk2_gg <- function(){
      x <- e.data()      
      ggplot(x)+
        geom_line(aes(x=Yr,y=S))+
        geom_hline(yintercept = input$riskEG*u, color =4, linetype=2,linewidth=1)+         
  scale_y_continuous(expand=c(0, 0), limits=c(0, NA), 
          labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
  scale_x_continuous(expand=c(0, 0.5),n.breaks = 10)+
  ylab(paste('Escapement',mult(u))) + xlab('Year')      
    }
#-------------------------------------------------------------------------------
# Module Outputs: Risk_sim_base, Risk_sim, Plt_Risk, Plt_Risk2 
#-------------------------------------------------------------------------------
    outdata <- list(Risk_sim_base =Risk_sim_base, Risk_sim = Risk_sim, 
                    Plt_risk=Plt_risk,Plt_risk_gg=Plt_risk_gg,Plt_risk2=Plt_risk2,Plt_risk2_gg=Plt_risk2_gg,Risk_custom=Risk_custom)
    return(outdata)      
    } # End function 
  ) # End moduleServer
} # End prcntoutServer


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
              choices = c('Tier 1','Tier 2','Tier 3'))
    )  
  }

#' Output Module ----------------------------------------------------------------
PercentileServer <- function(id,e.data,u){
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
          "Low to moderate average harvest rates (<40%)",
          "Goal Range: 20th - 60th percentile",sep = '<br/>')
      } else if(input$Tiers == "Tier 2")  {
        paste("Escapement goal criteria",
          "High contrast (> 8)",
          "Low measurement error (weir or tower surveys)",
          "Low to moderate average harvest rates (<40%)",
          "Goal Range: 15th - 65th percentile",sep = '<br/>')
      } else {
        paste("Escapement goal criteria",
          "Low contrast (< 8)",
          "Lowe to moderate average harvest rates (<40%)",
          "Goal Range: 5th - 65th percentile",sep = '<br/>')
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
  e.g <- data.frame(rbind(e.g.1,e.g.2,e.g.3))
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
  if(input$Tiers == "Tier 1") { e.g <- EG[1,]
  } else if( input$Tiers == "Tier 2") { e.g <- EG[2,]     
  } else if(input$Tiers == "Tier 3") { e.g <- EG[3,]       
  }
  # Graphics   
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
  # EG      
  lines(S/u~Yr,data=x)
  txt <- c('Tier 1','Tier 2','Tier 3')
  cols <- c(ifelse(input$Tiers == "Tier 1",7,2),ifelse(input$Tiers == "Tier 2",7,3),ifelse(input$Tiers == "Tier 3",7,4))
  lwds <- c(ifelse(input$Tiers == "Tier 1",2,1),ifelse(input$Tiers == "Tier 2",2,1),ifelse(input$Tiers == "Tier 3",2,1))
  fonts <- c(ifelse(input$Tiers == "Tier 1",2,1),ifelse(input$Tiers == "Tier 2",2,1),ifelse(input$Tiers == "Tier 3",2,1))
  add_legend('left',legend=txt, col=cols, lwd=lwds,lty=2, text.font=fonts, box.lty=0)  
  out <- recordPlot()
  return(out)
  }

Plt_prcnt_gg <- function(){
  EG <- EGS()
  x <- e.data()
  if(input$Tiers == "Tier 1") { e.g <- (as.numeric(EG[1,]))
  } else if( input$Tiers == "Tier 2") { e.g <- (as.numeric(EG[2,]))     
  } else if(input$Tiers == "Tier 3") { e.g <- (as.numeric(EG[3,]))       
  }
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
    }
    
    txt <- HTML(paste(paste(input$Tiers,"Escapement goal range"),
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
                Txt_Note=Txt_Note, Plt_prcnt=Plt_prcnt_gg)
return(outdata)      

    } # End function 
  ) # End moduleServer
} # End prcntoutServer




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
    selectInput(inputId="cmode","Fishery Oepnng above Escapement Goal", choices = c('Lower','Middle','Upper')),              
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



#===============================================================================
#   SR Figure EG, SMSY, SMX, etc Module  
#===============================================================================
SRplotUI <- function(id){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    checkboxInput(inputId=ns("show.points"), "show Years", FALSE), 
    checkboxInput(inputId=ns("show.smsy"), "show Smsy", FALSE),
    checkboxInput(inputId=ns("show.smax"), "show Smax", FALSE),
    checkboxInput(inputId=ns("show.int"), "show Interval", TRUE),
    selectInput(inputId="Li","Interval Type", choices = c('confidence','prediction'))
  )  
}

# Output Module ----------------------------------------------------------------
SRploteServer <- function(id,base,p,base.y,SRp,SR.data.0,u){
  moduleServer(
    id,
    function(input, output, session){
# srplot ------- SR plot function ----------------------------------------------
    srplot <- function(){
        xp <- sr.data.0
        SRp <- SRp/u
        # Draw Base SR Plot
        replayPlot(base.p)
        # Confidence Interval 
        if (input$Li =='confidence') {
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
          pointLabel(xp$S/u,xp$R/u, labels=as.character(xp$Yr), cex= 1,col=4)}
        # Add Smsy
        t1 <- ''
        l1 <- 0
        if(input$show.smsy==TRUE) {
          abline(v=median(SR.post()$Smsy)/u,col=1,lty=2)
          t1 <- 'Smsy'
          l1 <- 2
        }
        # Add Smax       
        t2 <- ''
        l2 <- 0
        if(input$show.smax==TRUE) {
          abline(v=median(SR.post()$Smax)/u,col=1,lty=3)
          t2 <- 'Smax'
          l2 <- 3
        }
        legend('topright',c(t1,t2),lty=c(l1,l2),bty='n')    
      }
# syplot -------- Yield plot ----------------------------------------------- 
    syplot <- function(){
        SRp <- SRp()/u
        xp <- sr.data.0()
        # Plot base Yiled plot
        replayPlot(base.py())
        # Confidence Interval 
        if (input$Li =='confidence') {
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
          pointLabel(xp$S/u,(xp$R-xp$S)/u, labels=as.character(xp$Yr), cex= 1,col=4)
        }
        # Add Smsy 
        t1 <- ''
        l1 <- 0
        if(input$show.smsy==TRUE) {
          abline(v=median(SR.post()$Smsy)/u,col=1,lty=2)
          t1 <- 'Smsy'
          l1 <- 2
        }
        # Add Smax       
        t2 <- ''
        l2 <- 0
        if(input$show.smax==TRUE) {
          abline(v=median(SR.post()$Smax)/u,col=1,lty=3)
          t2 <- 'Smax'
          l2 <- 3
        }
        # Add legend  
        legend('topright',c(t1,t2),lty=c(l1,l2),box.lty=0)    
      }
      
      
      
    } # End function 
  ) # End moduleServer
} # End SRServer




