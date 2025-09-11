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
RiskServer <- function(id,e.data,u,plt){
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
  #        dw <- dwtest(lnS~1,data=x)
    dw <- durbinWatsonTest(lm(lnS~1,data=x))
    if(dw$p < 0.05){
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
                   ifelse(dw$p < 0.05,"AR1","Standard"),"Model")
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
      if(i==1){
          EGP <- sum(ifelse(Spi[,i]>=p,0,1))  
        } else{
          EGP <- sum(ifelse(Spi[,i]<=p,0,1))      
          }
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
  if(plt=='base'){ 
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
        Plt_risk2 <- function(){
          x <- e.data()      
          par(yaxs='i',bty='l',las=1)
          plot(S/u~Yr,data=x,type='l',ylim=c(0,with(x,max(S,na.rm=TRUE)/u)),xlab='',ylab='')
          title("Escapement", xlab="Year",
                ylab=paste('Escapement',mult(u))) 
          # Add Escapement Goal  
          e.g <- Risk_sim()$int.S[1]
          abline(h=e.g/u,col=2)
          # Add custom Escapement Goal 
          abline(h=input$riskEG, col =4, lty=3,lwd=2)
          out <- recordPlot()
          return(out)
        }   
       }
  if(plt=='gg'){ 
    Plt_risk <- function(){
          mult <- mult(u)
          x <-Risk_sim()$S
          pi <- data.frame(Risk_sim()$pi)
          names(pi) <- c('a','b','c','d','e')
          delta <- Risk_sim()$delta
          n <- length(delta)
          e.g <- Risk_sim()$int.S
          txt <- c("Unneeded action", 
                   paste(100*delta,"% drop" )
          )
          # create a data.frame 
          #df <- melt(data.frame(S=x,pi),id.vars=('S'),value.name='p')
          # R Base reshape: replace reshape2 tidyr
          df <- reshape(data.frame(S=x,pi),direction='long', idvar='S',varying = names(data.frame(S=x,pi))[-1],
                         v.names='p',timevar='variable',times=names(data.frame(S=x,pi))[-1])
#          df <- pivot_longer(data.frame(S = x, pi), cols = -S, names_to = "variable", values_to = "p")
          p1 <- ggplot()+
            geom_line(data=df, aes(x=S,y=p,linetype=variable,color=variable, linewidth=variable))+
            scale_linetype_manual(labels=txt,values=c(1,1,3,4,5))+   
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
          e.g <- Risk_sim()$int.S[1]
          x <- e.data()      
          ggplot(x)+
            geom_line(aes(x=Yr,y=S))+
            geom_hline(yintercept = e.g, color =4, linetype=2,linewidth=1)+     
            scale_y_continuous(expand=c(0, 0), limits=c(0, NA), 
                               labels = label_number(scale = 1 /u),n.breaks = 10,oob=oob_keep) +
            scale_x_continuous(expand=c(0, 0.5),n.breaks = 10)+
            ylab(paste('Escapement',mult(u))) + xlab('Year')      
        }
#-------------------------------------------------------------------------------
# Module Outputs: Risk_sim_base, Risk_sim, Plt_Risk, Plt_Risk2 
#-------------------------------------------------------------------------------
      }    
      outdata <- list(Risk_sim_base =Risk_sim_base, Risk_sim = Risk_sim, 
                      Plt_risk=Plt_risk,Plt_risk2=Plt_risk2,Risk_custom=Risk_custom)
      return(outdata)      
    } # End function 
  ) # End moduleServer
} # End prcntoutServer

