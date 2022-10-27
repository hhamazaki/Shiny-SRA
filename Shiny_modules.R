#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Module and Helper functions 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
#  Helper Unit functions  
#-------------------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Modules  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#===============================================================================
#  dataInput Module: Upload data file to Shiny  
#===============================================================================
#  Usage: UI section 
#  dataInputUI("ns.name", "User data (.csv format)")
#  Usage: Server section: dataInputServer("datain")
#===============================================================================
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
#-------------------------------------------------------------------------------
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

#===============================================================================

#===============================================================================
#  1.0 MSY-Rmax Profile Based Goal Analyses Module 
#===============================================================================
#===============================================================================
# 1.1  ProfileUI Module: 
#  UI section: SmsyprofRUI("ns.name")
#  Server section: Smsyprofserver("ns.name",SR.pred)
#  Output:  EG.Smsy, EG.Smsy.st, Srange.st,  
#===============================================================================
# UI input Module --------------------------------------------------------------
ProfileUI <- function(id,crit){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    p(strong(paste(crit,"Analyses"))),  
    sliderInput(ns("p.min"), paste("Min % of",crit), value=90,min=0,max=100,step=5),
    sliderInput(ns("p.t"), paste("% Meeting",crit,"Target"), value=90,min=0,max=100,step=5)
      )  # End taglist
} # End SmsypfofUI


# Output Module ----------------------------------------------------------------
ProfileServer <- function(id,SR.pred,crit,u){
  moduleServer(
    id,
    function(input, output, session){
# EG  ----------------  User defined profile based goal data -------------------
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
      
# EG.st  ------- Standard profile based goal data --------------------
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
        st <- c(0.9,0.8,0.7)
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
        # Find Smsy Profile Intersections 
        out <- list(S.prof.st = prof.st, S.Range.st = Srange.st)
        return(out)   
      }) # End EG.st
      
# BEG  -------  goal table ----------------------------------------------
    BEG <- reactive({
        BEG.st <- EG.st()$S.Range.st
        BEG <- EG()$S.Range
        percent <- c(90,80,70,input$p.min)
        apercent <- input$p.t
        lg <- c(BEG.st[,1],BEG[1])
        ug <- c(BEG.st[,2],BEG[2])
        
        t.BEG <- HTML(paste0(percent,'% ',crit,' achieving ',apercent,'% Probability:',lg,' - ',ug,sep = '<br/>'))
        return(t.BEG)
      }) # SA.BEG
      
    p.min <- reactive({input$p.min})
    p.t <- reactive({input$p.t})
      
  plt.profile <- reactive({
      p.min <- as.numeric(p.min())/100
      # Import minimum % achieving 
      p.t <- as.numeric(p.t())/100  
      # Import S,   
      S <- EG()$S
      Y.prof <- EG()$S.prof
      Y.prof.st <- EG.st()$S.prof.st
      plot_profile(crit,Y.prof,Y.prof.st,S,p.min,p.t,as.numeric(u()))
      out1 <-recordPlot()
      return(out1)
    })
    
    outdata <- list(EG =EG, EG.st = EG.st,BEG=BEG,p.min=p.min, p.t=p.t,plt.profile=plt.profile)
    return(outdata)  
    } # End function 
  ) # End moduleServer
} # End ProfServer

#===============================================================================
#  1.2 ProfPlotUI Module: Read Profile output data and create plots
#  UI section: profPlotUI("ns.name",crit)
#  Server section: profPlotServer("ns.name",Prof,crit)
#  Output:  EG.Smsy, EG.Smsy.st, Srange.st,  
#===============================================================================
# UI input Module --------------------------------------------------------------
ProfPlotUI <- function(id,crit){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    plotOutput(height='400px',width='700px',ns('Plt_prof')),
    )  # End taglist
  } # End ProfPlotUI

# Output Module ----------------------------------------------------------------
ProfPlotServer <- function(id,prof,crit,u){
  moduleServer(
    id,
    function(input, output, session){
# -------Extract Profile data  -------------------------------------------------
      EG <- reactive({prof$EG()})
      EG.st <- reactive({prof$EG.st()})
      p.min <- reactive({prof$p.min()})   # Minimum goal 
      p.t <- reactive({prof$p.t()})       # % achievement target
      plt.profile <- reactive({prof$plt.profile()})
# ------- Profile Plot --------------------------------------------------------- 
      output$Plt_prof <- renderPlot({
        # Import minimum Smsy %
        p.min <- as.numeric(p.min())/100
        # Import minimum % achieving 
        p.t <- as.numeric(p.t())/100  
        # Import S,   
        #      S <- EG()$S
        #      Y.prof <- EG()$S.prof
        #      Y.prof.st <- EG.st()$S.prof.st
        #      plot_profile(crit,Y.prof,Y.prof.st,S,p.min,p.t,u)
        replayPlot(plt.profile())
        S <- EG()$S.Range/u
        c.col <- ifelse(crit=='MSY',3,4)
        polygon(c(S,rev(S)),c(c(0,0),c(1,1)),col=tcol(c.col,80),border=NA)
        #  Add legends 
        percent <- c(90,80,70,100*p.min)
        apercent <- 100*p.t
        BEG.st <- EG.st()$S.Range.st
        BEG <- EG()$S.Range
        lg <- c(BEG.st[,1],BEG[1])
        ug <- c(BEG.st[,2],BEG[2])
        txt <- c(paste(percent,'%',crit,apercent,'% target:',lg,' - ',ug))
        legend("right", legend= txt, lwd=c(1,1,1,2), lty=c(1,2,4,1),
               col=c(1,1,1,6),box.lty=0)
      })
    } # End function 
  ) # End moduleServer
} # End ProfPlotServer
#===============================================================================




#===============================================================================
#  2.0 Risk Based Goal Analyses Module 
#===============================================================================
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
    sliderInput(ns("risk.p"), "% Drop", value=90,min=0, max=100,step=5)
  )  
}

# Output Module ----------------------------------------------------------------
RiskServer <- function(id,e.data,u){
  moduleServer(
    id,
    function(input, output, session){
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
        dw <- dwtest(lnS~Yr,data=x)
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
        st <- c(0.5, 0.85, 0.9, 0.95)
        delta <- c(st,p)
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
        pi[,nd+1] <- colMeans(outer(X=xsimmax[,length(delta)+1], Y=lngoal, FUN="<="))  # simulated probabilities (unneeded action)
        for(j in 1:nd){
          pi[,j] <- colMeans(outer(X=xsimmax[,j], Y=lngoal, FUN=">"))  # simulated probabilities (mistaken inaction)
          int.S[j] <- max(S[pi[,j]>pi[,nd+1]])  
        }
        
        txt <- HTML(paste0("Mistaken inaction at ", 100*delta,"% drop : ",int.S,sep = '<br/>'))
        out <- list(S = S, pi = pi,delta=delta,int.S = int.S, txt=txt)
        return(out)   
      })
#-------------------------------------------------------------------------------
# Plot module     
#-------------------------------------------------------------------------------
# Plt_risk:  Risk analyses plot 
#-------------------------------------------------------------------------------
    Plt_risk <- function(){
      mult <- mult(u)
      x <-Risk_sim()$S
      pi <- Risk_sim()$pi
      delta <- Risk_sim()$delta
      e.g <- Risk_sim()$int.S
      par(xaxs='i',yaxs='i',bty='l')
      plot(x/u, pi[,ncol(pi)], las=1, type='l', ylim=0:1, lwd=2, 
           xlab=paste('Escapement',mult), ylab="Estimated Risk")
      for(j in 1:(length(delta)-1)) lines(x/u, pi[,j], col=j)
      lines(x/u,pi[,length(delta)], lwd=2,col=2) 
      txt <- c("Unneeded action", paste0("Mistaken inaction at ", 100*delta,"% drop ", e.g))
      legend("topright", legend= txt, lwd=c(2,rep(1,length(delta)-1),2),
             col=c(1,1:(length(delta)-1),2),box.lty=0)
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
      # Add Escapement Goal range  
      e.g <- Risk_sim()$int.S[5]
      abline(h=e.g/u,col=2)
    }
#-------------------------------------------------------------------------------
# Module Outputs: Risk_sim_base, Risk_sim, Plt_Risk, Plt_Risk2 
#-------------------------------------------------------------------------------
    outdata <- list(Risk_sim_base =Risk_sim_base, Risk_sim = Risk_sim, 
                    Plt_risk=Plt_risk,Plt_risk2=Plt_risk2)
    return(outdata)      
    } # End function 
  ) # End moduleServer
} # End prcntoutServer


#===============================================================================
#  3.0 Percentile Based Goal Analyses Module 
#===============================================================================
#===============================================================================
#  PerentileUI Module: Produce Risk analyses and plot results  
#  Usage: 
#  UI section 
#  PerentileUI("ns.name")
#  Server section
#  PerentileUIServer("ns.name",e.data,u)
#===============================================================================
PercentileUI <- function(id){
  # Display choice of Run vs S-R
  ns <- NS(id)
  tagList( 
    selectInput(ns("Tiers"),"Tiers", 
              choices = c('Tier 1','Tier 2','Tier 3'))
    )  
  }

# Output Module ----------------------------------------------------------------
PercentileServer <- function(id,e.data){
  moduleServer(
    id,
    function(input, output, session){
#-------------------------------------------------------------------------------
# Txt_Tier:  Explanation of Tiers 
#-------------------------------------------------------------------------------  
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
#-------------------------------------------------------------------------------
# EGS:  Perecentile goals   
#-------------------------------------------------------------------------------  
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


#-------------------------------------------------------------------------------
# Plt_prcnt:  Plot Percentile  
#-------------------------------------------------------------------------------  
add_legend <- function(...) {
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n',xlab='',ylab='')
  legend(...)
}

Plt_prcnt <- function(){
  mult <- mult(u)
  EG <- EGS()
  x <- e.data()
  if(input$Tiers == "Tier 1") { e.g <- EG[1,]
  } else if( input$Tiers == "Tier 2") { e.g <- EG[2,]     
  } else if(input$Tiers == "Tier 3") { e.g <- EG[3,]       
  }
  # Graphics   
  layout(matrix(1:2, ncol=2), widths=c(3, 0.5))
  par(yaxs='i',bty='l',las=1,mar=c(4,4,4,4))
  plot(S/u~Yr,data=x,type='l',ylim=c(0,max(x$S,na.rm=TRUE)/u),xlab='',ylab='')
  title("Escapement", xlab="Year",ylab=paste('Escapement',mult(u))) 
  # Add Escapement Goal range  
  polygon(with(x,c(min(Yr),max(Yr),max(Yr),min(Yr))),c(e.g[1]/u,e.g[1]/u,e.g[2]/u,e.g[2]/u),col=tcol(2,50),border=NA)
  # Alternative: 
  abline(h=EG[1,]/u,col = ifelse(input$Tiers == "Tier 1",2,3), lty=2,lwd=ifelse(input$Tiers == "Tier 1",2,1))
  abline(h=EG[2,]/u,col = ifelse(input$Tiers == "Tier 2",2,4), lty=2,lwd=ifelse(input$Tiers == "Tier 2",2,1))
  abline(h=EG[3,]/u,col = ifelse(input$Tiers == "Tier 3",2,5), lty=2,lwd=ifelse(input$Tiers == "Tier 3",2,1))
  # EG      
#  abline(h=e.g/u,col=2,lwd=2,xpd=FALSE)
  lines(S/u~Yr,data=x)
  txt <- c('Tier 1','Tier 2','Tier 3')
  cols <- c(ifelse(input$Tiers == "Tier 1",2,3),ifelse(input$Tiers == "Tier 2",2,4),ifelse(input$Tiers == "Tier 3",2,5))
  lwds <- c(ifelse(input$Tiers == "Tier 1",2,1),ifelse(input$Tiers == "Tier 2",2,1),ifelse(input$Tiers == "Tier 3",2,1))
  add_legend('topright',legend=txt, col=cols, lwd=lwds,lty=2, box.lty=0,xpd=TRUE)  
}

#-------------------------------------------------------------------------------
# Txt_Note:  Explanation of Tiers 
#-------------------------------------------------------------------------------  
Txt_Note <- reactive({
    x <- e.data()
    EG <- EGS()
    contrast <- round(max(x$S)/min(x$S),1)
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
#-------------------------------------------------------------------------------
# Module Outputs: Txt_Tier, Txt_Note, Plt_prcnt 
#-------------------------------------------------------------------------------
outdata <- list(Txt_Tier =Txt_Tier, Tier=Tier, EGS = EGS, 
                Txt_Note=Txt_Note)
return(outdata)      

    } # End function 
  ) # End moduleServer
} # End prcntoutServer




#===============================================================================
#  4.0 MSE Analyses Module 
#===============================================================================
#===============================================================================
#  MSEUI Module: Produce Risk analyses and plot results  
#  Usage: 
#  UI section 
#  MSEUI("ns.name")
#  Server section
#  MSEUIServer("ns.name",e.data,u)
#===============================================================================
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
#  dataType Module: Define Datat Type SR vs Run 
#===============================================================================
#  Usage: 
#  UI section 
#  dataTypeUI("ns.name")
#  Server section
#  callModule(dataType, "ns.name")
#===============================================================================
#dataTypeUI <- function(id){
# Display choice of Run vs S-R
#  ns <- NS(id)
#  tagList( 
#    selectInput(inputId=ns("dataType"),"Data Type", choices = c('S-R','Run')),
# If data is "Run" select first age 
#    conditionalPanel(
#      sprintf("input['%s'] == 'Run'", ns("dataType")),
#      p("Select First age of run"),
#      # Input: Select what to display
#      numericInput(ns("fage"), "First Return Age", value=4,min=1,max=20,step=1)
#    )  
#  )
#}

#dataType <- function(input, output, session, datafile){
# note: output note to screen  -------------------------------------------------
#  note <- reactive({
#    if(input$dataType== "S-R"){
#      paste("S-R Data file column orders: Year, Spawner (Escapement), Recruit")
#    } else if (input$dataType== "Run"){
#      paste("Run Data file column orders: Year, Escapement, Run,
#                  Run by age (or proportion) from youngest to oldest")
#    }
#  })
#}  
#===============================================================================
#broodSR <- function(input, output, session, datafile){
# brood.table create brood table -----------------------------------------------
  
#  brood.table <- reactive({
#    if(input$dataType== "Run"){
#      x <- datafile()
      # sum first run age: if p   
#      p <- round(sum(x[1,-c(1:3)]),0)
#      fage <- input$fage
#      nages <- dim(x)[2]-3
#      lage <- fage+nages-1
#      yr <- c(min(x[,1])-seq(lage,1),x[,1])
#      brood <- matrix(0,ncol=nages+2,nrow = length(yr))
#      brood[,1] <- yr
#      brood[,2] <- c(rep(NA,lage),x[,2])
#      for(i in 1:nages){
#        if(p==1){
#          brood[,i+2] <- c(rep(NA,lage-fage+1-i),x[,3+i]*x[,3],rep(NA,fage+i-1))
#        }
#        else{
#          brood[,i+2] <- c(rep(NA,lage-fage+1-i),x[,3+i],rep(NA,fage+i-1))  
#        }
#      }
#      brood.c <- data.frame(brood)
#      names(brood.c) <- c('b.Year','Spawner',paste0('b.Age',seq(fage,lage)))
#      brood.c$Recruit <- rowSums(brood.c[,-c(1:2)])
#      return(brood.c)
#    } else {NA}
#  }) 
  
# sr.data.0 --- Original sr dataset --------------------------------------------
#  sr.data.0 <- reactive({
#    if(input$dataType== "Run"){
#      x <- brood.Table()
#      x <- x[complete.cases(x),c('b.year','Spawner','Recruit')]
#    } else if (input$dataType== "S-R"){
#      x <- datafile()
#    }
#    names(x) <- c('Yr','S','R')
#    return(x)     
#  })
  
#  outdata <- list(brood.table=brood.table, sr.data.0 = sr.data.0,note=note)
#  return(outdata)
#}


#-------------------------------------------------------------------------------
#  Year selecting module  
#-------------------------------------------------------------------------------
#yrangeOUTUI <- function(id) {
  # Create a namespace function using the provided id
#  ns <- NS(id)
#  tagList(
#    p(strong("Choose brood year range")),
#    uiOutput(ns("yrange"))
#  )
#}

#yrangeOUT <- function(input, output, session,sr.data.0) {
  
#  output$yrange = renderUI({
#    year <- sr.data.0()$Yr   # Extract brood year data range 
#    fyear <- min(year)       # First brood year 
#    lyear <- max(year)       # Last brood year
    #  Slider input UI 
#    sliderInput(("sryears"), label = "year range", min = fyear, max = lyear, value = c(fyear, lyear),step=1,sep = "")
#  })
  
  # sr.data --- final dataset used for SR analyses -------------------------------
#  sr.data <- reactive({
#    x <- sr.data.0()
#    fyear <- input$sryears[1]
#    lyear <- input$sryears[2]    
#    x <- x[x$Yr>=fyear & x$Yr<=lyear,]
#    return(x)     
#  })
  
#  return(sr.data)
#}

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




