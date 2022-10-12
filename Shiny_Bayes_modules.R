#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Bayes Modules  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#===============================================================================
#  BayesModelUI: 
#  Usage: UI section 
#  BayesModelUI("ns.name", "User data (.csv format)")
#  Usage: Server section
#  callModule(dataInput, "ns.name",stringsAsFactors = FALSE)
#===============================================================================
BayesInputUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
        numericInput(ns('n.burnin'),'Burn-in x 1000',value=5,min=0,step = 1),
        numericInput(ns('n.thin'),'Thinning',value=5,min=0,step = 1)
                     ),  
      column(6,
             numericInput(ns('n.iter'),'Sim x 1000',value=10,min=0,step=1), 
             numericInput(ns('n.chain'),'Chains',value=3,min=1,step = 1)
             )
    ),    
    p("Start Bayesian Analyses"),
    actionButton(ns("RunBayes"),"Run")
    )
  }
#-------------------------------------------------------------------------------
BayesInputServer <- function(id,Bayesdata,Bayesmodel){
  moduleServer(id,
               function(input, output, session) {
   # The selected file, if any
  run.JAGS <- eventReactive(input$RunBayes,{
    #-------------------------------------------------------------------------------  
    progress <- Progress$new(min=1,max=100)
    on.exit(progress$close())
    progress$set(message = paste('JAG Model in progress'),
                 detail = 'This will take a while. Be patient please....')
    for (i in 1:100) {
      progress$set(value = i)
    }
#-------------------------------------------------------------------------------   
#----  Import model data ------------------------------------------------------- 
    datnew <- Bayesdata()   
    niter <- 1000*input$n.iter      
    nburn <- 1000*input$n.burnin
    titer <- nburn+niter
    nthin <- input$n.thin
    nchain <- input$n.chain
#    seed <- 123
#    seed <- sample(1:1000,1)
    #  JAGS model selection 
    jagmodel <- Bayesmodel()$jagmodel
    pars <- Bayesmodel()$parameters
    # Run JAGS 
    output <- jags(data=datnew,parameters.to.save=pars, model.file= jagmodel,
                            n.chains=nchain, n.iter=titer,n.burnin=nburn,n.thin=nthin)
#    output <- jags.parallel(data=datnew,parameters.to.save=pars, model.file= jagmodel,
#                   n.chains=nchain, n.iter=titer,n.burnin=nburn,n.thin=nthin,
#                   jags.seed = seed)
    
    return(output)
  })
    } # End fundtion
  ) # End moduleServer
} # End BayesInputServer


#===============================================================================
#  Bayesmodel  Model Functions  
#===============================================================================
#---------------------------------------------------------------
#  Ricker 
#---------------------------------------------------------------
  jag.model.CR <- function(){
    for(y in 1:nyrs){
      # log normal Likelihood 
      R[y] ~ dlnorm(mu[y],Tau)
# v[i] is used for Kalman filter 
      fit[y] <- log(S[y]) + lnalpha - beta * S[y]/(10^d) 
      e[y] <- log(R[y]) - fit[y]
      w[y] ~dnorm(0,tauw)
     }
# ar1 = 0 and kf =0 in standard analysis   
# ar1 = 1 and kf =0 AR1 error moddel 
# ar1 = 0 and kf =1 time-varying alpha
    mu[1] <-  fit[1] + ar1*phi * e0;
    cw[1] <- w[1] #+ kf*v[y]
  for(y in 2:nyrs){	   
      cw[y] <- cw[y-1] + w[y] #+ v[y]
      mu[y] <- fit[y] + kf*cw[y] + ar1*phi*e[y-1]
    }
    # log normal Likelihood 
#    for(y in 1:nyrs){     
#      R[y] ~ dlnorm(mu[y],Tau)
#    }  
#   Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,20)
    sigma ~ dunif(0,10)  
    sigmaw ~ dunif(0,10)
#    sigmav ~ dunif(0,10)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001) 
    Tau <- 1/(sigma*sigma)
    tauw <- 1/(sigmaw*sigmaw)	
#   tauv <- 1/(sigmav*sigmav)
# Extract time-varyihg alapha
    for(y in 1:nyrs){
     lnalphai[y] <- lnalpha+cw[y] 
    }

  }
  
# SR model function for post processing ---------------------------------------
  SR.CR <- function(lnalpha,beta,S,d){
    s <- S/(10^d)
    lnR <- log(S) + lnalpha - beta*s
    R <- exp(lnR)
    return(R)
  }

# Estimate Biological Reference for post processing ------------------------------
  BR.CR <- function(lnalpha,beta,d){
    Seq <- (10^d)*lnalpha/beta
    Smsy <- Seq*(0.5-0.07*lnalpha)
    Umsy <- lnalpha*(0.5-0.07*lnalpha)
    Smax <- (10^d)/beta
    out <- data.frame(Seq,Smsy,Umsy,Smax)
    return(out)
  }    
  
  
#-------------------------------------------------------------------------------
#  Beverton Holt  
#-------------------------------------------------------------------------------
  jag.model.BH <- function(){
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
      fit[y] <- lnalpha + log(S[y]) -log(1+beta*s[y])
      e[y] <- log(R[y]) - fit[y]
      w[y] ~dnorm(0,tauw)
    }
# ar1 = 0 and kf =0 in standard analysis   
# ar1 = 1 and kf =0 AR1 error moddel 
# ar1 = 0 and kf =1 time-varying alpha
    mu[1] <-  fit[1] + ar1*phi*e0;
    cw[1] <- w[1] #+ v[y]
    for(y in 2:nyrs){	   
      cw[y] <- cw[y-1] + w[y] #+ v[y]
      mu[y] <- fit[y] + kf*cw[y] + ar1*phi*e[y-1]
    }
    # Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,20)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001)     
    sigma ~ dunif(0,10)
    sigmaw ~ dunif(0,10)
#    sigmav ~ dunif(0,10)
    Tau <- 1/(sigma*sigma)
    tauw <- 1/(sigmaw*sigmaw)	
#    tauv <- 1/(sigmav*sigmav)	
# Extract time-varyihg alapha
    for(y in 1:nyrs){
      lnalphai[y] <- lnalpha+cw[y] 
    }
# Likelihood 
    for(y in 1:nyrs){
      R[y] ~ dlnorm(mu[y],Tau)
    }  
  }
# SR model function for post processing ---------------------------------------
  SR.BH <- function(lnalpha,beta,S,d){
    s <- S/(10^d)
    lnR <- lnalpha +log(S) - log(1+beta*s)
    R <- exp(lnR)
    return(R)
  }
# Estimate Biological Reference for post processing ------------------------------
  BR.BH <- function(lnalpha,beta,d){
    alpha <- exp(lnalpha)
    Seq <- (10^d)*(alpha-1)/beta
    Smsy <- (10^d)*(sqrt(alpha)-1)/beta
    Umsy <- 1-sqrt(1/alpha)
    Smax <- NA
    out <- data.frame(Seq,Smsy,Umsy,Smax)
    return(out)
  }
  
#-------------------------------------------------------------------------------
#  Deriso-Shunute  Inactive 
#-------------------------------------------------------------------------------
  jag.model.DS <- function(){
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
      lnS[y] <- log(S[y])
      fit[y] = lnalpha + log(S[y]) - log(1 + beta*c*s[y])/c 
      e[y] = log(R[y]) - fit[y]
    }
    # ar1 = 0 in standard analysis   
    # ar1 = 1 when AR1 error moddel is considered.   
    mu[1] = fit[1] + ar1*phi*e0;	  
    for(y in 2:nyrs){	   
      mu[y] = fit[y] + ar1*phi*e[y-1]    
    } 
    #     Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,20)
    sigma ~ dunif(0,10)
    c ~ dunif(0,1)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001)     
    Tau <- 1/(sigma*sigma)
    # Likelihood 
    for(y in 1:nyrs){     
      R[y] ~ dlnorm(mu[y],Tau)
    }  
  }
# SR model function for poost processing ---------------------------------------  
  SR.DS <- function(lnalpha,beta,c,S,d){
    s <- S/(10^d)
    lnR <- log(S) + lnalpha - log(1 + beta*c*s)/c 
    R <- exp(lnR)
    return(R)
  }


#-------------------------------------------------------------------------------
#  Ricker State-Space Model 
#-------------------------------------------------------------------------------
 jag.model.CR.SS <-function() {
# y: Brood year 
# t: Calendar year t=1 equals to lage+1 for brood year

#-------------------------------------------------------------------------------          	
# SR period:  Before Run-Escapement data are collected
#-------------------------------------------------------------------------------          	
#-------------------------------------------------------------------------------
# Option 1: Estimate Recruit 
#-------------------------------------------------------------------------------                
#    for (y in 1:lage) { 
#    	log.R[y] ~ dnorm(mean.lnR0,tau.R0) 
#    		R[y] <- exp(log.R[y]) 
#    	}	
#    mean.lnS0 ~ dunif(0,15)
#    tau.R0 ~ dgamma(0.001,0.001)     
#-------------------------------------------------------------------------------
# Option 2: Estimate Spawner  
#-------------------------------------------------------------------------------
    resid[1] <- e0
  for (y in 1:lage) { 
      ln.S[y] ~ dnorm(mean.lnS0,tau.S0) 
      SS[y] <- exp(ln.S[y])
      fit[y] <- ln.S[y] + lnalpha - beta * SS[y]/(10^d) + kf*cw[y]
      mu[y] <- fit[y]+ar1*phi*resid[y]  #Note: resid[y] is previous year,
      R[y] ~ dlnorm(mu[y],tau.e)
      resid[y+1] <- log(R[y]) - fit[y]	          
    }	
# priors     
    mean.lnS0 ~ dunif(0,10)
    tau.s ~ dgamma(0.001,0.001) 
#-------------------------------------------------------------------------------          	
# SR period:  After Run-Escapement data are collected
#-------------------------------------------------------------------------------          	
#   resid[lage+1] <- e0  # Use when R [1:lage] were directly estimated
    for (y in (lage+1):(nyrs+lage-fage)) { 
      fit[y] <- log(S[y-lage]) + lnalpha - beta * S[y-lage]/(10^d) + kf*cw[y] 
      mu[y] <- fit[y]+ar1*phi*resid[y] #Note: resid[y] is previous year,
      R[y] ~ dlnorm(mu[y],tau.e)
      resid[y+1] <- log(R[y]) - fit[y]
    }
#--------------------------------------------------------------------------------          	
#  Time variant alpha 
#--------------------------------------------------------------------------------          	
# cw: cumulative variation   
    cw[1] <- w[1] 
  for (y in 2:(nyrs+lage-fage)) {
    cw[y] <-  (cw[y-1] + w[y]) 
     }
  for (y in 1:(nyrs+lage-fage)) { 
      lnalphai[y] <- lnalpha + cw[y]	# sigma for lnalpha
     } 
#  Priors 
  for (y in 1:(nyrs+lage-fage)) {  
      w[y] ~ dnorm(0,tau.w)%_%T(-5,5)  # sigma for lnalpha
      }
#--------------------------------------------------------------------------------          	
#   SR Model Priors     
#--------------------------------------------------------------------------------          	
    lnalpha ~ dnorm(0,1.0E-6)%_%T(0,)
    beta ~ dnorm(0,1.0E-6)%_%T(0,)              
    phi ~ dunif(-1,1)
    tau.e ~ dgamma(0.001,0.001)        	
    e0 ~ dnorm(0,0.001)%_%T(-5,5)
    tau.w ~ dgamma(0.001,0.001) 
#-------------------------------------------------------------------------------             
#  Maturity Schedule 
#-------------------------------------------------------------------------------             
# GENERAL MATURITY SCHEDULE---------------------------------------------------
#	D <- sum(gamma[])
#	for (a in 1:nages) {
#  		gamma[a] ~ dgamma(0.005,0.005)
#  		pi[a] <- gamma[a] / D
#  			for (y in 1:(nyrs+lage-fage)) {                                                    
#      			g[y,a] ~ dgamma(gamma[a],1)
# p: brood year age proportion (i.e. maturity schedule). 	 
#      			p[y,a] <- g[y,a]/sum(g[y,])
#   			}
# 	}
    
#------ Logistic Maturity Schedule --------------------------------------------   	
#  Maturity schedule take random-walk------------------------------------------- 
    rwk[1] = mk
    rwe[1] = me  
#  calculate maturity functions ------------------------------------------------   
 for (y in 1:(nyrs+lage-fage)){
      rwk[y+1] <- mk_b[y]  
      rwe[y+1] <- me_b[y]  
      me_b[y] ~ dnorm(rwe[y],0.01)%_%T(3,6) #Note: rwe[y] is previous year,	 
      mk_b[y] ~ dnorm(rwk[y],0.01)%_%T(0,6) #Note: rwk[y] is previous year,
      
    for (a in 1:(nages-1)){
# cummulative brood age comp is a logistic function	
        cum_brp[y,a] <- 1.0/(1.0+exp(mk_b[y]*(me_b[y]-fage-a+1)))	 
         }
        cum_brp[y,nages] <- 1.0
# p: brood year age proportion (i.e. maturity schedule). 	 
      p[y,1] <- cum_brp[y,1]	 
      p[y,2:nages] <- cum_brp[y,2:nages] - cum_brp[y,1:(nages-1)]	 	
    
    }  # End y
# mk me priors  
    mk ~ dunif(1,6)
    me ~ dunif(4,6)
#-------------------------------------------------------------------------------
#  Calendar Year Population Dynamics 
#-------------------------------------------------------------------------------
 for(t in 1:nyrs){
# N.ta: The number of fish (RUN) returning at Calendar year t by age      
  for(a in 1:nages) { N.ta[t,a]<-R[(t+nages-a)]*p[(t+nages-a),a] }

# N: Total number of fish at calender year 
    N[t] <- sum(N.ta[t,1:nages])
# q: Run age proportion at calender year      
    q[t,1:nages] <- N.ta[t,1:nages] / N[t]
    
#   Calculate Harvest and Escapement 
    H[t] <- mu.H[t] * N[t]    
    S[t] <- N[t]-H.com[t]	
# mu.H Prior
    mu.H[t] ~ dbeta(0.1,0.1)
# Convert N H S to log 
    log.N[t] <- log(N[t])    
    log.H[t] <- log(H[t])
    log.S[t] <- log(S[t])
    
# Calculate Empirical log.Tau for N, H, S 
# N     
    sigma.N[t]<-sqrt(log(pow(cv.N[t],2)+1))
    tau.log.N[t]<-1/pow(sigma.N[t],2)	
# H     
    sigma.H[t]<-sqrt(log(pow(cv.H[t],2)+1))
    tau.log.H[t]<-1/pow(sigma.H[t],2)  
# S     
    sigma.S[t]<-sqrt(log(pow(cv.S[t],2)+1))
    tau.log.S[t]<-1/pow(sigma.S[t],2)	    
#-------------------------------------------------------------------------------
#  Likelihood Calculations: 
#-------------------------------------------------------------------------------
#  Run age comp is mulitinomial Likelihood 
   x[t,1:nages] ~ dmulti(q[t,],n[t])
    
#  Run size  is lognormal distribution 
  N.obs[t] ~ dlnorm(log.N[t],tau.log.N[t]) 
      
# Total Harvest Estimates LIKELIHOOD -------------------------------------------			
  H.obs[t] ~ dlnorm(log.H[t],tau.log.H[t])  

# Spawner is total run minus harvest -------------------------------------------
  S.hat[t] ~ dlnorm(log.S[t],tau.log.S[t]) 		
   
   }  # End t 
  
 }  # End SS.CR model 
  

#--- Function Model select -----------------------------------------------------
model_select <- function(smodel,add,ss=FALSE){
  if(smodel=='Ricker'){
     if(isTRUE(ss)){
       jagmodel <- jag.model.CR.SS
       parameters <- c('lnalpha','alpha','beta','R','S','N','mu.H','mk_b','me_b')    
     }else{
    jagmodel <- jag.model.CR
    parameters <- c('lnalpha','beta','sigma') 
     }
    if(add=='ar1'){ ar1.p <- c('phi','e0')} else {ar1.p <- NULL}
    if(add=='kf'){kf.p <- c('lnalphai','sigmaw')} else {kf.p <- NULL}
    parameters <- c(parameters,ar1.p,kf.p)
    model <- SR.CR
    model.br <- BR.CR
  }
  if(smodel=='Beverton-Holt'){
    jagmodel <- jag.model.BH
    parameters <- c('lnalpha','beta','sigma') 
    if(add=='ar1'){ ar1.p <- c('phi','e0')} else {ar1.p <- NULL}
    if(add=='kf'){kf.p <- c('lnalphai','sigmaw')} else {kf.p <- NULL}
    parameters <- c(parameters,ar1.p,kf.p)
    model <- SR.BH
    model.br <- BR.BH
  } 
  if(smodel=='Deriso-Shunute'){
    jagmodel <- jag.model.DS
    parameters <- c('lnalpha','beta','c','sigma')
    if(add=='ar1'){ ar1.p <- c('phi','e0')} else {ar1.p <- NULL}
    if(add=='kf'){kf.p <- c('lnalphai','sigmaw','sigmav')} else {kf.p <- NULL}
    parameters <- c(parameters,ar1.p,kf.p)      
    model <- SR.DS
  }
  out <- list(jagmodel=jagmodel,parameters=parameters,model=model,model.br = model.br)
  return(out)
}
