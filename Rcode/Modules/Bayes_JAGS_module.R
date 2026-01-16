#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  JAGS Bayes Modules  
#  This module controls JAGS run.
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'==============================================================================
#  BayesModelUI----
#  Usage: UI section 
#  BayesModelUI("ns.name", "User data (.csv format)")
#  Usage: Server section
#  callModule(dataInput, "ns.name",stringsAsFactors = FALSE)
#'==============================================================================
BayesInputUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fluidRow(
      p(strong("Run Model Analyses")),
      actionButton(ns("RunBayes"),"Run"),
      checkboxInput(ns('config'), "Modify Model Running Setting", FALSE),
      conditionalPanel(condition="input.config == true",
      column(6,
        numericInput(ns('n.burnin'),'Burn-in x 1000',value=5,min=0,step = 1),
        numericInput(ns('n.thin'),'Thinning',value=5,min=0,step = 1)
                     ),  
      column(6,
             numericInput(ns('n.iter'),'Sim x 1000',value=10,min=0,step=1), 
             numericInput(ns('n.chain'),'Chains',value=3,min=1,step = 1)
             ),
      checkboxInput(ns('seeds'), "Set Seeds", TRUE),
      ns=NS(id)  # call namespace here!!
      ) # End conditional Panel 
      ) # End fluidRow
    ) # End tagList
  }
#'------------------------------------------------------------------------------
BayesInputServer <- function(id,Bayesdata,Bayesmodel){
  moduleServer(id, function(input, output, session) {
   # The selected file, if any
  run.JAGS <- eventReactive(input$RunBayes,{
#' Progress---------------------------------------------------------------------- 
    progress <- Progress$new(min=1,max=100)
    on.exit(progress$close())
    progress$set(message = paste('JAG Model in progress'),
                 detail = 'This will take a while. Be patient please....')
    for (i in 1:100) {
      progress$set(value = i)
    }
#'-------------------------------------------------------------------------------   
#'----  Import model data ------------------------------------------------------- 
    datnew <- Bayesdata()   
    niter <- 1000*input$n.iter      
    nburn <- 1000*input$n.burnin
    titer <- nburn+niter
    nthin <- input$n.thin
    nchain <- input$n.chain
    if(isTRUE(input$seeds)){seed <- 123} else {seed <- sample(1:10000,1)} 
    #  JAGS model selection 
    jagmodel <- Bayesmodel()$jagmodel
    pars <- Bayesmodel()$parameters
    # Run JAGS 
    output <- jags(data=datnew,parameters.to.save=pars, model.file= jagmodel,
                            n.chains=nchain, n.iter=titer,n.burnin=nburn,n.thin=nthin,jags.seed=seed)
#    output <- jags.parallel(data=datnew,parameters.to.save=pars, model.file= jagmodel,
#                   n.chains=nchain, n.iter=titer,n.burnin=nburn,n.thin=nthin,jags.seed = seed)
    out <- list(output = output, input= c(niter,nburn, titer,nthin,nchain),data=datnew)
    return(out)
  })
    } # End fundtion
  ) # End moduleServer
} # End BayesInputServer


#'==============================================================================
#  Bayesmodel  Model Functions ----  
#'==============================================================================
#'------------------------------------------------------------------------------
#  jag.model.SR:  Run Ricker and Beverton-Holt model ----
#'------------------------------------------------------------------------------
  jag.model.SR <- function(){
    for(y in 1:nyrs){
      # log normal Likelihood 
      R[y] ~ dlnorm(mu[y],Tau)
# rk = 1 and bh = 0:  log-linearlized Rikcer SR Model 
#      fit[y] <- log(S[y]) + lnalpha - rk*(beta * S[y]/(10^d))
# rk = 0 and bh = 1:  log-linearlized Beverton-Holt Model 
#      fit[y] <- log(S[y]) + lnalpha - (log(1+beta*S[y]/(10^d)))   
      fit[y] <- log(S[y]) + lnalpha - rk*(beta * S[y]/(10^d))-bh*(log(1+beta*S[y]/(10^d)))
      e[y] <- log(R[y]) - fit[y]
     }
# ar1 = 0 and kf =0 in standard analysis   
# ar1 = 1 and kf =0 AR1 error moddel 
# ar1 = 0 and kf =1 time-varying alpha
# e0 original residual     
    mu[1] <-  fit[1] + ar1*phi * e0;
    cw[1]  ~ dnorm(0,tauw)
  for(y in 2:nyrs){	   
      cw[y] ~  dnorm(cw[y-1],tauw)
      mu[y] <- fit[y] + kf*cw[y] + ar1*phi*e[y-1]
    }
#   Define Priors
    lnalpha ~ dunif(min.a,max.a)
    beta ~ dunif(min.b,max.b)
    sigma ~ dunif(0,2)  
    sigmaw ~ dunif(0,2)
#    sigmav ~ dunif(0,10)
    phi ~ dnorm(0,5)%_%T(-1,1) 
    e0 ~ dnorm(0,25) 
    Tau <- 1/(sigma*sigma)
    tauw <- 1/(sigmaw*sigmaw)	
#   tauv <- 1/(sigmav*sigmav)
# Extract time-varyihg alapha
    for(y in 1:nyrs){
     lnalphai[y] <- lnalpha+cw[y] 
    }
  }


#'------------------------------------------------------------------------------
#  jag.model.SRe:  Ricker and Beverton-Holt Measurement Error model ----
#'------------------------------------------------------------------------------
  jag.model.SRe <- function(){
    for(y in 1:nyrs){
      # log normal Likelihood for R
      R[y] ~ dlnorm(mu[y],Tau)
      S[y] ~ dlnorm(log.S.mu[y],tau.log.S[y])  
# rk = 1 and bh = 0:  log-linearlized Rikcer SR Model 
#      fit[y] <- log(S[y]) + lnalpha - rk*(beta * S[y]/(10^d))
# rk = 0 and bh = 1:  log-linearlized Beverton-Holt Model 
#      fit[y] <- log(S[y]) + lnalpha - (log(1+beta*S[y]/(10^d)))   
    fit[y] <- log.S.mu[y] + lnalpha - rk*(beta * exp(log.S.mu[y])/(10^d))-bh*(log(1+beta*exp(log.S.mu[y])/(10^d)))
      e[y] <- log(R[y]) - fit[y]
     }
# ar1 = 0 and kf =0 in standard analysis   
# ar1 = 1 and kf =0 AR1 error moddel 
# ar1 = 0 and kf =1 time-varying alpha
# e0 original residual     
    mu[1] <-  fit[1] + ar1*phi * e0;
    cw[1]  ~ dnorm(0,tauw)
  for(y in 2:nyrs){	   
      cw[y] ~  dnorm(cw[y-1],tauw)
      mu[y] <- fit[y] + kf*cw[y] + ar1*phi*e[y-1]
    }
#   Define Priors
    lnalpha ~ dunif(min.a,max.a)
    beta ~ dunif(min.b,max.b)
    sigma ~ dunif(0,2)  
    sigmaw ~ dunif(0,2)
#    sigmav ~ dunif(0,10)
    phi ~ dnorm(0,5)%_%T(-1,1) 
    e0 ~ dnorm(0,25) 
    Tau <- 1/(sigma*sigma)
    tauw <- 1/(sigmaw*sigmaw)	
#    tauv <- 1/(sigmav*sigmav)
#  Priors for S 
    for(y in 1:nyrs){
     log.S.mu[y] ~ dnorm(lnSm,tau.lnSm)
    }
    
# Extract time-varyihg alapha
    for(y in 1:nyrs){
     lnalphai[y] <- lnalpha+cw[y] 
    }
  }

#'------------------------------------------------------------------------------
#  jag.model.SR.SS:  Ricker and Beverton-Holt State-Space Model----
#'------------------------------------------------------------------------------
jag.model.SR.SS <-function() {
# y: Brood year 
# t: Calendar year t=1 equals to lage+1 for brood year
# q: Brood age comp 
# p: Run age comp
#'-------------------------------------------------------------------------------
# Likelihood Calculation: Run, Harvest, Esc, Age comp
#'-------------------------------------------------------------------------------                
#'  t indicates calendar year   
  for(t in 1:nyrs){
     # N.ta: The number of fish (RUN) returning at Calendar year t by age      
    for(a in 1:nages){
      N.ta[t,a]<-R[(t+nages-a)]*q[(t+nages-a),a] 
      }
# N: Total number of fish at calender year 
     N[t] <- sum(N.ta[t,1:nages])
#   Calculate Harvest and Escapement 
     H[t] <- mu.H[t] * N[t]    
     S[t] <- N[t] - H[t]	
     mu.H[t] ~ dbeta(0.1,0.1)

#  Run age comp is multinomial Likelihood 
     p_age[t,1:nages] ~ dmulti(p[t,],efn[t])
         p[t,1:nages] <- N.ta[t,1:nages] / N[t]
# q: Run age proportion at calender year      
# Run size: Lognormal likelihood 
     log.N[t] <- log(N[t]) 
     N.obs[t] ~ dlnorm(log.N[t],tau.log.N[t]) 
     # Harvest:  Lognormal likelihood 
     log.H[t] <- log(H[t])
     H.obs[t] ~ dlnorm(log.H[t],tau.log.H[t])  
     # Spawner:  Lognormal likelihood 
     log.S[t] <- log(S[t])
     S.obs[t] ~ dlnorm(log.S[t],tau.log.S[t]) 		
   }  # End t    

#'-------------------------------------------------------------------------------
# Model: First lage years of recruit is lognormal
#'-------------------------------------------------------------------------------                
  for (y in 1:lage) { 
    	log.R[y] ~ dnorm(mean.log.R0,tau.R0) 
    	R[y] <- exp(log.R[y]) 
    	}	
#'------------------------------------------------------------------------------         	
# SR model:  After Run-Escapement data are collected 
#'------------------------------------------------------------------------------          	
    resid[1] <- e0  # Initial residual 
  for (t in 1:(nyrs-fage)) { 
# fit is SR model prediction       
# rk = 1 and bh = 0:  log-linearlized Rikcer SR Model 
#      fit[y] <- log(S[y]) + lnalpha - rk*(beta * S[y]/(10^d))
# rk = 0 and bh = 1:  log-linearlized Beverton-Holt Model 
#      fit[y] <- log(S[y]) + lnalpha - (log(1+beta*S[y]/(10^d))) 
# kf = 1: time variant alpha (TVA)     
  fit[t] <- log(S[t]) + lnalpha - rk*(beta * S[t]/(10^d))-bh*(log(1+beta*S[t]/(10^d)))+ kf*cw[t]

# mu is SR model with AR1 error 
# ar = 1: AR1 error,  ar=0: standard error   
      mu[t] <- fit[t] +ar1*phi*resid[t] # Note: resid[t] is previous year,
      log.R[t+lage] ~ dnorm(mu[t],tau.e)  # Add lognormal error: t+lage is brood year 
      resid[t+1] <- log.R[t+lage] - fit[t] # resid[t+1] is this year's error 
      R[t+lage] <- exp(log.R[t+lage]) 
    }
#'------------------------------------------------------------------------------         	
###  Time variant alpha 
#'------------------------------------------------------------------------------       	
# cw: cumulative variation   
    cw[1] ~ dnorm(0,tauw)%_%T(-5,5)
  for (t in 2:(nyrs-fage)) {
    cw[t] ~ dnorm(cw[t-1],tauw)%_%T(-5,5)
      }
    
#  TVA lnalapha
  for (t in 1:(nyrs-fage)) {  
      lnalphai[t] <- lnalpha + cw[t]	# sigma for lnalpha
      }

#' GENERAL MATURITY SCHEDULE---------------------------------------------------
  for (a in 1:nages) {
       for (y in 1:(nyrs+lage-fage)) {   
        g[y,a] ~ dgamma(gamma[a],1)
      # q: brood year age proportion (i.e. maturity schedule). 	 
        q[y,a] <- g[y,a]/sum(g[y,])                  }
        gamma[a] ~ dgamma(0.005,0.005)  # Gamma prior   
      }
#'------ Logistic Maturtity Schedule ------------------------------------------- 	
#'  Maturity schedule take random-walk------------------------------------------
#  rwk[1] = mk
#  rwe[1] = me  
#'  calculate maturity functions ----------------------------------------------- 
#  for (y in 1:(nyrs+lage-fage)){
#    q[y,1] <- 1.0/(1.0+exp(rwk[y]*(rwe[y]-fage)))
#    for (a in 2:(nages-1)){
# cumulative brood age comp is a logistic function	
#        q[y,a] <- 1.0/(1.0+exp(rwk[y]*(rwe[y]-fage-a+1))) - q[y,a-1] 
#      } 
#    q[y,nages] <- 1 - 1.0/(1.0+exp(rwk[y]*(rwe[y]-fage-nages+2)))	 	
      # calculate for the next year 
#    rwe[y+1] ~ dnorm(rwe[y],25)%_%T(fage,lage-1) #Note: rwe[y] is previous year,	 
#    rwk[y+1] ~ dnorm(rwk[y],25)%_%T(1,lage) #Note: rwk[y] is previous year,	
#  } # End y

#'------------------------------------------------------------------------------    	
###  Model Priors      
#'------------------------------------------------------------------------------        	
    lnalpha ~ dunif(min.a,max.a)
    beta ~ dunif(min.b,max.b)              
    phi ~ dnorm(0,5)%_%T(-1,1)  
    mean.log.R0 ~ dnorm(0,1.0E-3)%_%T(0,30)  
    sigma.R0 ~ dunif(0,5)
    tau.R0 <- 1/sigma.R0^2
    sigmaw ~ dunif(0,2)
    tauw <- 1/sigmaw^2
    sigma ~ dunif(0,2)	
    tau.e  <- 1/sigma^2       	
    e0 ~ dnorm(0,25)
#    mk ~ dunif(1,lage-1)
#    me ~ dunif(fage,lage)
 }  # End SS.CR model 


#'==============================================================================
# SR Model output calculation functions ----
#'==============================================================================
## SR.CR Ricker Model  -----
  SR.CR <- function(lnalpha,beta,S,d){
    s <- S/(10^d)
    R <- S*exp(lnalpha-beta*s)
    return(R)
  }
## BR.CR Ricker Biological Refrence points ----
  BR.CR <- function(lnalpha,beta,d){
    get_Sgen.bc <- function(m){
      fun_Sgen.bc <- function(S,m) {S*exp(m[1])*exp(-m[2]*S/(10^d)) - m[3]}
       if(m[1]>0 & m[2]>0 & m[3]>0){
         Sgen <- try(uniroot(fun_Sgen.bc, interval=c(0,m[3]),m=m)$root)
       } else {Sgen <- NA}
      return(Sgen)
     }
    Seq <- ifelse(lnalpha>0&beta>0,(10^d)*lnalpha/beta,NA)
#    Smsy <- ifelse(is.na(Seq),NA,Seq*(0.5-0.07*lnalpha))
    Smsy <- ifelse(lnalpha>0&beta>0,(10^d)*(1-lambertW0(exp(1-lnalpha)))/beta,NA)
    Umsy <- ifelse(beta>0,Smsy*beta/(10^d),NA)
    Smax <- ifelse(beta>0,(10^d)/beta,NA)
    mat <- cbind(lnalpha,beta,Smsy)
    Sgen <- as.numeric(apply(mat,1,(get_Sgen.bc)))  
    out <- data.frame(Seq,Smsy,Umsy,Smax,Sgen)
    return(out)
  }   
## SR.BH Beverton-Holt Model  -----  
  SR.BH <- function(lnalpha,beta,S,d){
    s <- S/(10^d)
#    lnR <- lnalpha +log(S) - log(1+beta*s)
    R <- S*exp(lnalpha - log(1+beta*s)) 
      return(R)
  }  

## BR.CR Beverton-Holt Biological Refrence points ---- 
  BR.BH <- function(lnalpha,beta,d){
  get_Sgen.bc <- function(m){
      fun_Sgen.bc <- function(S,m) {S*exp(m[1]-log(1+m[2]*S/(10^d))) - m[3]}
    if(m[1]>0 & m[2] >0 & m[3]>0){
      Sgen <- try(uniroot(fun_Sgen.bc, interval=c(0,m[3]),m=m)$root)
    } else {Sgen <- NA}
      return(Sgen)
    }
    alpha <- exp(lnalpha)
    Seq <- ifelse(alpha>1 & beta>0,(10^d)*(alpha-1)/beta,NA)
    Smsy <- ifelse(alpha>1 & beta>0,(10^d)*(sqrt(alpha)-1)/beta,NA)
    Umsy <- ifelse(alpha>0,1-sqrt(1/alpha),NA)
    Smax <- NA
    mat <- cbind(lnalpha,beta,Smsy)
    Sgen <- as.numeric(apply(mat,1,(get_Sgen.bc))) 
    out <- data.frame(Seq,Smsy,Umsy,Smax,Sgen)
    return(out)
  }
  
#'==============================================================================
#  Model selection functions ----
#'==============================================================================  
## model_select: Select SR models ------------------------------------------
model_select <- function(smodel,add,ss=FALSE,re=FALSE){
base.par <- c('lnalpha','beta','sigma')
#' State-Space model vs standard option
    if(isTRUE(ss)){
       jagmodel <- jag.model.SR.SS
       parameters <- c(base.par,'N','S','H','R','p','q')    
     } else {
    jagmodel <- jag.model.SR
    parameters <- base.par  
  if(isTRUE(re)){
    jagmodel <- jag.model.SRe
    parameters <- c(base.par,'log.S.mu') 
      } 
     }
#' Add ar1 or TVA parameters   
    if(add=='ar1'){ ar1.p <- c('phi','e0')} else {ar1.p <- NULL}
    if(add=='kf'){kf.p <- c('lnalphai','sigmaw')} else {kf.p <- NULL}
    parameters <- c(parameters,ar1.p,kf.p)
#' Select model set   
  if(smodel=='Ricker'){
    model <- SR.CR 
    model.br <- BR.CR
  }
  if(smodel=='Beverton-Holt'){
    model <- SR.BH
    model.br <- BR.BH
  } 
#' Not implemented -------------  
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



#'-------------------------------------------------------------------------------
#  Deriso-Shunute  SR ModelInactive ----
#'-------------------------------------------------------------------------------
jag.model.DS <- function(){
    # Likelihood 
    for(y in 1:nyrs){     
      R[y] ~ dlnorm(mu[y],Tau)
    } 
    # ar1 = 0 in standard analysis   
    # ar1 = 1 when AR1 error moddel is considered.   
    mu[1] = fit[1] + ar1*phi*e0;	  
    for(y in 2:nyrs){	   
      mu[y] = fit[y] + ar1*phi*e[y-1]    
    }     
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
      lnS[y] <- log(S[y])
      fit[y] = lnalpha + log(S[y]) - log(1 + beta*c*s[y])/c 
      e[y] = log(R[y]) - fit[y]
    }

    #     Define Priors
    lnalpha ~ dunif(min.a,max.a)
    beta ~ dunif(min.b,max.b)
    sigma ~ dunif(0,10)
    c ~ dunif(0,1)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001)     
    Tau <- 1/(sigma*sigma)
 
  }
#' SR model function for post processing ---------------------------------------  
  SR.DS <- function(lnalpha,beta,c,S,d){
    s <- S/(10^d)
    lnR <- log(S) + lnalpha - log(1 + beta*c*s)/c 
    R <- exp(lnR)
    return(R)
  }

