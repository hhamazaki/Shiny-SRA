#'==============================================================================
# Shiny_SR_function.R 
# Collections of functions used for Shiny SR model apps
#'==============================================================================
#'==============================================================================
#  General
#'==============================================================================
## Show multiplier axis ---------------------------------------------------- 
mult <- function(u){
  mult <- ifelse(u==1000000,paste0('(x million)'),ifelse(u>1,paste0('(x',u,')'),''))  
  return(mult)  
}
#'==============================================================================
#  1.0 Data Modification ---- 
#'==============================================================================

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.1  cut.data: Cut data based on specified years -----
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 cut.data <- function(data, years){
   x <- data[data$Yr>=years[1] & data$Yr<=years[2],]
   return(x)
 }
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.3  cut.N.data: Cut data based on brood years -----
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
 cut.N.data <- function(data, years,lyear){
   x <- data[data[,1]>=years[1] & data[,1]<=(years[2]+lyear),]
   return(x)
 }
  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.4  substrRight: extract text from right -----  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' function extract right 
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.5  Trim density function  -----  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
trimden <- function(x){density(x[x<quantile(x,0.99,na.rm=TRUE)],na.rm=TRUE)}

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.6  reshapeWL: Reshape from wide to long  -----  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
 reshapeWL <- function(df,idvar,timevar,v.names){
      dfname <- names(df)[names(df)!=idvar]
  out <- reshape(df,direction='long',idvar=idvar,varying=dfname,
          timevar = timevar,v.names=v.names,times=dfname)
     return(out)}  
  
reshapeLW <- function(df,idvar=idvar,v.names=v.names,timevar=timevar){
  dfs <- df[c(idvar,timevar,v.names)]
   w <- reshape(dfs,direction='wide',timevar=timevar,v.names=v.names,idvar=idvar)
   wname <- names(w)[-c(1:length(idvar))]
   vhead <-paste0(v.names,'.')
   wname <- gsub(vhead,'',wname)
   names(w)[-c(1:length(idvar))] <- wname
   return(w)
}
  
#'==============================================================================
#  2.0  Data Summary functions  -----  (used for )
#'==============================================================================
sum.ci.v <- function(x,ci){
  p <- (100-ci)/200
  min <- min(x, na.rm=TRUE)
  max <- max(x,na.rm=TRUE)
  lci <- quantile(x,prob=p,na.rm=TRUE)
  uci <- quantile(x,prob=1-p,na.rm=TRUE)
  mean <- mean(x, na.rm=TRUE)
  median <- median(x,na.rm=TRUE)
  sd <-  sd(x,na.rm=TRUE)
  cv <- sd/mean
  out <- data.frame(min=min,lci=lci,mean=mean,median=median,uci=uci,max=max,sd=sd,vcv=cv)
 }

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  2.1  sum.ci -----  
#   Get summary from MCMC matrix
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sum.ci <- function(x,ci){
  temp <- apply(x,2,function(x) sum.ci.v(x,ci))
  out <-  data.frame(do.call(rbind,temp))
  return(out)
 }
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  2.2  sum.fun -----  
#   Get summary from MCMC data.frame
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sum.fun <- function(x,ci){
  p <- (100-ci)/200
  out <- t(sum.ci(as.matrix(x),ci))
  rownames(out) <- c('Min',paste0(100*p,'%'),'Mean','Median',paste0(100*(1-p),'%'),'Max','SD','CV')
  return(out)
 }

#'==============================================================================
# 3.0  Simulation functions: Read model specification and simulation data, 
#      calculate simulated SEQ, Smsy, Umsy, Smax
#      Clean up simulation results
#'==============================================================================

#'------------------------------------------------------------------------------
# Id.outliers: Identify outliers ---
#'------------------------------------------------------------------------------
outliers <- function(x){
  out <- boxplot(x, plot=FALSE)$out
  r <- which(x %in% out)
  return(r)
}

Id.outliers <- function(dat,target){ 
# Remove obvious outlier data 
# Assign 1 when beta or lnalpha are less than 0   
  dat$Remove <- with(dat, ifelse(beta<=0,1,ifelse(lnalpha<=0,2,NA)))
  pars <- c('beta','lnalpha')
  if(target=='me'){pars <- c('beta','lnalpha.c')}
  for(i in 1:2){
  temp <-dat[,pars[i]]
  temp[which(dat$Remove>=1)] <- NA
  temp <- ifelse(temp>0&temp<1000,log(0.001*temp/(1-0.001*temp)),NA)
    if(length(outliers(temp)>0)) dat$Remove[outliers(temp)] <- (i+2) 
  }
  return(dat)
 }

outlier_sum <- function(dat){
 dat <-data.frame(table(dat$Remove,useNA='ifany')) 
 names(dat) <- c('code','n')
 dat$Pct <- round(100*dat$n/sum(dat$n),1)
 dat$code <- as.integer(as.character(dat$code))
 note <- c('beta Negative','lnalpha Negative','Outlier beta','Outlier lnalpha')
 code <- c(1,2,3,4)
 note <- data.frame(code,note)
 dat <- merge(dat,note, by =c('code'),all=TRUE)
 dat$code <- as.integer(dat$code)
 return(dat)
 }


#'------------------------------------------------------------------------------
# sim.out: Read MCMC data, remove outliers, and calculate biological reference 
# sim:  JAG MCMC 
# d:  multiplier 
# model:  SR model specification 
# add:  model addition (AR1, TVA)
# model.br: biological reference calculation function
#'------------------------------------------------------------------------------
sim.out <- function(sim,d,add,model,model.br){
#D is multiplier.   
  D <- as.numeric(d)
# sim should include followings: 'lnalpha','beta', lnalphai
  post <- as.data.frame(sim)
# 
  if(add=='kf'){
# for TVA model calculate median lnalphai and simga.lnalpha
    post$lnalpha <- apply(post[,grep(pattern='lnalphai',names(post),value=TRUE)],1, FUN=median, na.rm=TRUE)
    post$sigma.lnalpha <- apply(post[,grep(pattern='lnalphai',names(post),value=TRUE)],1, FUN=sd, na.rm=TRUE)
  }
    post$alpha <- exp(post$lnalpha)
# Calculate Mean R adjustment     
  if(add=='ar1'){
    post$lnalpha.c <- post$lnalpha+0.5*(post$sigma^2)/(1-post$phi^2)
    } else {post$lnalpha.c <- post$lnalpha+0.5*post$sigma^2}
    post$alpha.c <- exp(post$lnalpha.c)
  
#'------------------------------------------------------------------------------
# Create Biological reference points
#'------------------------------------------------------------------------------
   br <- with(post,model.br(lnalpha,beta,D))
   br.c <- with(post,model.br(lnalpha.c,beta,D))
   post$Seq <- br$Seq
   post$Smsy <- br$Smsy
   post$Umsy <- br$Umsy
   post$Sgen <- br$Sgen
   post$Seq.c <- br.c$Seq
   post$Smsy.c <- br.c$Smsy
   post$Umsy.c <- br.c$Umsy
   post$Sgen.c <- br.c$Sgen
   post$Smax <- br$Smax   
  return(post)
}  # End sim.out

#'------------------------------------------------------------------------------
# 4.0  SR.pred.sim:  Read simulation data, and create  ----
#   Create predicted Recruit and Yield (CI,PI) at given S
#'------------------------------------------------------------------------------
SR.pred.sim <-function(SRpar,D,S,srmodel,add){
#'---------- Extract MCMC SR Model Parameters ----------------------------------
#' Mean or Median prediction----------------------------------------------------  
  lnalpha.c <- SRpar$lnalpha.c  
  lnalpha <- SRpar$lnalpha 
  beta <- SRpar$beta
#' Extract sigma----------------------------------------------------------------  
  if(add == 'ar1'){
    sigma <- with(SRpar, sqrt(sigma^2/(1-phi^2)))
  } else if(add == 'kf'){
    sigma <- with(SRpar, sqrt(sigma^2+sigma.lnalpha^2))
    #    sigma <- SRpar$sigma
  } else {
    sigma <- SRpar$sigma
  }
#' Cut into 201 segments (can be increased)
# This allows each number be integer
#  S <- seq(0,max.s, length.out=len.sim)
  len.sim <- length(S)
# Get the number of simulation (row)  
  nrow <- length(lnalpha)  
# Create Expected mean and observed Recruit MCMC matrix    
  mc.R <- matrix(NA,nrow=nrow,ncol=len.sim)   # Model expected recruit Median 
  mc.R.c <- matrix(NA,nrow=nrow,ncol=len.sim)   # Model expected recruit Mean 
  mc.R.p <- matrix(NA,nrow=nrow,ncol=len.sim) # Model expected observed recruit
#---------- Calculate expected returns from each MCMC ------------------------ 
  for(i in 1:nrow){
# Calculate expected Returns form each MCMC SR model parameters
# mc.R is Median Recruit when target is Median, and Mean Recruit when target is Mean
    mc.R[i,] <- srmodel(lnalpha[i],beta[i],S,D)
    mc.R.c[i,] <- srmodel(lnalpha.c[i],beta[i],S,D)
# mc.R.p adds observation error (sigma): this case no lognormal correction is needed
    mc.R.p[i,] <- exp(rnorm(len.sim,log(mc.R[i,]),sigma[i]))
  }  
# Create expected mean/median (mc.Y) and observed Yield (mc.Y.p) matrix
#  Expected Median or Mean Yield
#  mc.Y <-  t(t(mc.R)-S) 
  mc.Y <- sweep(mc.R,MARGIN = 2,S,'-')
#  mc.Y.c <-  t(t(mc.R.c)-S) 
  mc.Y.c <- sweep(mc.R,MARGIN = 2,S,'-')
#  Expected Annual Yield
#  mc.Y.p <-  t(t(mc.R.p)-S) 
  mc.Y.p <- sweep(mc.R.p,MARGIN = 2,S,'-')
#  Expected SR 
  mc.lnRS <- log(sweep(mc.R,MARGIN = 2,S,'/'))
  mc.lnRS.p <- log(sweep(mc.R.p,MARGIN = 2,S,'/'))
  mc.lnRS[is.nan(mc.lnRS)] <- NA
  mc.lnRS.p[is.nan(mc.lnRS.p)] <- NA  
   
#------  Create Output list file -----------------------------------------------  
  out <- list()
  out$S <- S
  out$R <- mc.R
  out$R.c <- mc.R.c
  out$R.p <- mc.R.p
  out$Y <- mc.Y
  out$Y.c <- mc.Y.c
  out$Y.p <- mc.Y.p
  out$lnRS <- mc.lnRS
  out$lnRS.p <- mc.lnRS.p
  return(out)
}

#-------------------------------------------------------------------------------
# pred_CI: Create CI and PI range of Recruit and Yield
# SR.pred: ouput of sim.out function 
# CI: User derined CI % 
#-------------------------------------------------------------------------------
pred_CI<- function(SR.pred, CI) {
#--- Import predicted data -----------------------------------------------------
  Pred <-SR.pred
#--- User defined % interval range ---------------------------------------------  
  pci <- (100-CI)/200
# Model used S
  S <- Pred$S
# Median RS 
  RS.md <- apply(Pred$R.p,2,function(x) median(x,na.rm=TRUE))
# Mean RS (Trim mean)
  RS.me <- apply(Pred$R.p,2,function(x) mean(x, trim=0.01,na.rm=TRUE))

# Calculate Lower and upper CI-PI
  Rl <- apply(Pred$R,2,function(x) quantile(x, pci,na.rm=TRUE))
  Ru <-  apply(Pred$R,2,function(x) quantile(x, 1-pci,na.rm=TRUE))    
# Lower PI   
  Rl.p <- apply(Pred$R.p,2,function(x) quantile(x, pci,na.rm=TRUE))
# Upper PI    
  Ru.p <- apply(Pred$R.p,2,function(x) quantile(x, 1-pci,na.rm=TRUE))
  out <- data.frame(cbind(S,RS.md,RS.me,Rl,Ru,Rl.p,Ru.p))
# Name 
  names(out) <- c('S','RS.md','RS.me','Rl','Ru','Rl.p','Ru.p')
  return(out)
}  

#'==============================================================================
# 5.0 Profile Analyses ----
#'==============================================================================

#'==============================================================================
## 4.1 Prob.calc: Create Profile Probability of achieving target ----
# Y: Matrix  gl: Criteria
#'==============================================================================
Prob.calc <- function(Y,gl){
# If value 
  Ygl <- apply(Y,2,function(x) ifelse(x >=gl,x-gl,0))
  Yb <- apply(Ygl,2,function(x) ifelse(x >0,1,0))
  Ypm <- colMeans(as.matrix(Yb))  # Mean probability
  return(list(Ypm=Ypm, Ygl = Ygl))
 }

#'==============================================================================
## 4.2 S.intersect: Find value of S that intersect target  Probability ----
#'==============================================================================
S.intersect <- function(S,prof,tp){
    b.p <- S[prof >= tp]
    if(length(b.p)>0) {sect <- c(min(b.p),max(b.p))} else {sect <- c(NA,NA)}
    return(sect)
 }

#'==============================================================================
## 4.3 prof_sim: for Target Yield and Recruit Analyses ----
#'  1. Calculate probability of exceeding target (tg)at given S and 
#'  2. find S ranges that intersects with target probability of achieving (tpg) 
#'  3. Create probability profile plot 
#'==============================================================================
prof_sim <- function(S,pred.m,pred.a,tg,tpg){
# Calculate probability profile  
  temp <- Prob.calc(pred.m,tg)
  prof.m <- temp$Ypm
  prof.a <- Prob.calc(pred.a,tg)$Ypm
  prof.ma <- data.frame(S=S,prof.m=prof.m, prof.a=prof.a)
  ygl <- temp$Ygl
# Calculate intersect S range   
  b.p <- S.intersect(S,prof.m,tpg/100)
  b.pa <- S.intersect(S,prof.a,tpg/100)
  range <- data.frame(b.p,b.pa)
  return(list(prof.ma=prof.ma,range=range,Dat.Prof= ygl))
 }  

#===============================================================================
# 4.2  Cut out simulation output by specific range 
#===============================================================================
SR.cut <-function(SR.pred,Srange){
  #--- Import predicted data ---------------------------------------------------
  Pred <-SR.pred
  # Model used S
  S <- Pred$S  
  # Get column   
  SS <-  which(S >= min(Srange) & S <= max(Srange))
  #------  Create Output list file ---------------------------------------------  
  out <- list()
  out$R <- Pred$R[,SS]
  out$R.p <- Pred$R.p[,SS]
  out$Y <- Pred$Y[,SS]  
  out$Y.p <- Pred$Y.p[,SS]
  return(out)
}

#===============================================================================
#  5.0 Profile Analyses functions ---- 
#===============================================================================
#-------------------------------------------------------------------------------    
# profile --- Profile Calculation Function  
# This function calculates probability of meeting specific target at S range 
# Calculate lower and upper end of S that meeting the target achievement criteria 
# Input data
  #	S: Spawner (range) 
  #	M.rep: Yield, Run, simulation matrix
  # mp: Target criteria: mp < 1, Percentage of max, mp > 1, specific target 
  # tp: Target percentage  (0-1)
#-------------------------------------------------------------------------------    
profile <- function(S, M.rep, mp,tp) {
  # Assign 0  0 if not
  if(mp > 1){
      temp1 <- t(apply(M.rep,1,function(x) x-mp))
      temp1[temp1<0]<-0
      temp2 <- temp1
      temp1[temp1>0]<-1
           } else {
      temp1 <- t(apply(M.rep,1,function(x) x-mp*max(x,na.rm=TRUE)))
      temp1[temp1<0]<-0
      temp2 <- temp1
      temp1[temp1>0]<-1      
         }
  # Mean of temp matrix is a  profile probability  
  M.Rep.prof <- colMeans(temp1)
  # Find range of S that intersect with target probability  
  S.range <- S.intersect(S,M.Rep.prof,tp)
  # Output  
  out <- list(S = S, S.prof = M.Rep.prof, S.Range = S.range,Dat.prof = temp2)
  return(out)
}


#===============================================================================
#  Stars function----- 
#===============================================================================
# Modified from Sergei Rodionov's VBA and Andrew Gardner's Matlab codes to produce an R version of STARS. 
# Go to http://www.climatelogic.com/overview for more information, or see the Rodionov (2004; 2006) references in the paper.
# Red noise correction included, but some other functions from VBA code (version 3.2) 
# still missing (e.g. tests in shifts in variance).
# Alistair Seddon, October 2013. 
# alistair.seddon@gmail.com

stars<- function(y=c(rnorm(50), rnorm(50,2,1)), L=20, p=0.05, h=1,  AR1red="none", prewhitening = F) {
  if(AR1red=="none" && prewhitening== T) stop("Impossible to perform prewhitening if AR1 coefficient is not estimated") 		
  m= round((L+1)/3)	
  # formula to estimate subsample size for calculating alpha (Rodionov 2006 + http://www.climatelogic.com/documentation/red-noise-estimation)    	  
  # library("MASS") needed if you want to use Huber correction parameter built in to MASS  
  # ------------------------------------------------------------------------------
  #    hWeightedAverage(xwin)   
  #     Calculates the mean estimate for a given range using Huber's weights.
  # ------------------------------------------------------------------------------      	
  hWeightedAverage<-function(xwin, h){
    # simple estimate of the regime mean for the windowed clip
    dblEstAve <- mean(xwin);            
    
    for(jjj in 1:2){
      sumWeights = 0
      sumAve = 0
      
      # Estimate normalised deviation
      xDev = (xwin-dblEstAve)/sqrt(sigL)
      
      # Estimate weights or normalised deviation
      xDev[xDev==0] = 1
      wDev = pmin(rep(1, length(xwin)), h/abs(xDev), na.rm=T)
      
      #sum weights and weighed values
      sumWeights = sum(wDev)
      sumAve = sum(xDev*wDev)
      
      sumAve = sumAve/sumWeights
      sumAve = sumAve*sqrt(sigL) + dblEstAve
      dblEstAve = sumAve
    }		
    dblWeightedAve = dblEstAve
    # hestimate<- huber(xwin, h)
    # dblWeightedAve = hestimate$mu
  }
  
  #-------------------------------------------------------------------
  # estimateSigma
  # Estimate the long-term, L-pt variance (assume homoskedastic signals).
  #-------------------------------------------------------------------
  estimateSigma<-function(x, L){
    # Estimate the long-term length-L variance. If the signal >> length of the analysis window, sample to estimate the variance.	
    nx<-length(x)
    if(nx/L>300) ix <- as.integer(runif(100)*(nx-2*L)+L) else ix<-seq(L,nx,1)
    s<-0
    for(i in 1:length(ix)){
      xwin <- x[(ix[i]-L+1):ix[i]]
      s <- s + var(xwin, na.rm=T)
    }
    sigL1 = s / length(ix)
    sigL1
  }
  
  
  # ------------------------------------------------------------------
  #   getThreshold()
  #   Calculate the critical threshold of deviation that signals regime changes.  
  #   This does not change over the signal.
  # ---------------------------------------------------------------
  getThreshold<-function(L, p, sigL){
    if(prewhitening == T){
      dof <- 2*L-2						# number degrees freedom
    } else {
      dof <- EqN((2*L-2), alpha)
    }
    t <- abs(qt(p/2, dof));              # crit 2-sided t-value
    thresh1 = t*sqrt(2*sigL/L);          # crit deviation
    thresh1
  }
  
  
  # ------------------------------------------------------------------------------
  # OLS estimate of AR1 coefficient from Orcutt and Winokur, 1969, 
  #      Econometrica, 37:1,1-14
  # ------------------------------------------------------------------------------
  OLScalc<-function(x){
    Nobs = length(x)
    ave1 = mean(x[2:Nobs])
    ave2 = mean(x[1:(Nobs-1)])
    sumNom=0
    sumDenom=0
    for(i in 2:Nobs){
      sumNom = sumNom + (x[i] - ave1) * (x[i - 1] - ave2)
      sumDenom = sumDenom + (x[i - 1] - ave2) * (x[i - 1] - ave2)
    }
    if(sumDenom > 0) OLSAR1 = sumNom / sumDenom else OLSAR1 = 0
    OLSAR1
  }
  
  # -------------------------------------------------------------------
  # AR1 correlation estimate (alpha)
  # ------------------------------------------------------------------- 
  AR1cor<-function (m, y){
    m= m #define this in big function above
    ny=length(y)
    iy=seq(1,ny-m+1,1)
    OLS=rep(NA, length(iy))
    # Calculate OLS for sequential samples of length m
    for(i in 1:length(iy)){        
      xwin = y[(iy[i]):(iy[i]+m-1)]
      if(length(xwin[is.na(xwin)]) == 0)   OLS[i] <- OLScalc(xwin)
    }
    
    est<-median(OLS, na.rm=T)
    
    # Calculate IP4	
    IP4= est + 1/m
    for(j in 1:3) IP4=IP4 + abs(IP4)/m
    
    # Calculate MPK
    if (m>4) MPK=((m-1)*est+1)/(m-4) else MPK= est
    
    alphaEst<-c(est, MPK, IP4) 	
  } 	
  
  # -------------------------------------------------------------------
  # Function EqP: calculates t-test using equivalent sample size as in 
  #   von Storch and Zwiers (1999, p.115)
  # ------------------------------------------------------------------- 
  EqP= function(rng1, rng2){   
    # Set standard no-result for if command at end
    EqP = 0
    # Calculate means and variances
    ave1 = mean(rng1, na.rm =T)
    ave2 = mean(rng2, na.rm =T)   
    
    var1 = sd(rng1, na.rm = T)
    var2 = sd(rng2, na.rm = T)   
    
    # Calculate effective sample sizes   
    Ns1 = length(na.omit(rng1))
    if(Ns1 < 2){
      EqP = -1
    } 
    eN1 = EqN(Ns1, alpha)
    
    Ns2 = length(na.omit(rng2))
    if(Ns2 < 2){
      EqP = -1
    } 
    eN2 = EqN(Ns2, alpha)
    
    if(EqP == -1){
      EqP
    } else{
      # Calculate t-statistics
      T_stat = sqrt(var1/eN1 + var2/ eN2)
      T_stat = abs(ave1 - ave2)/ T_stat
      
      EqP = (1-pt(T_stat, eN1 + eN2 -2))*2
      EqP
    }
  }
  
  # -------------------------------------------------------------------
  # EqN: Calculates equivalent sample size as in von Storch and Zwiers (1999, p.115)
  # -------------------------------------------------------------------
  EqN = function(Ns, alpha){
    
    sumEqN = rep(NA, Ns-1)
    for(i in 1: (Ns-1)){
      sumEqN[i] = (1-i/Ns)*alpha^i
    }
    EqN = Ns / (1 + sum(c(sumEqN)))
    
    # just in case
    if( EqN <=2) EqN = 2
    if(EqN > Ns) EqN =Ns
    EqN
  }
  
  # ------------------------------------------------------------------
  #   cusumUp()
  #       Compute the L-pt cusum for positive level changes.  For a positive regime change to be accepted, we require the L-pt lookahead samples	to produce a cusum sequence which does not go negative.
  # -------------------------------------------------------------------
  
  cusumUp<-function(k){
    # LL sets the look ahead length: L, or the number of points until the end of the signal. k is the sample point running through the iteration
    LL <- min(L, N-k+1)
    
    # dblXdev is the length-LL vector of normalized deviations of x outside of the range lvl +/- thresh
    dblXdev = ((x[k:(k+LL-1)]) - (lvl+thresh)) / sqrt(sigL)
    
    #  these are Huber weight values, so large deviations are deemphasized
    dblXdev[dblXdev==0] = 1
    dblXweight = pmin(rep(1, length(dblXdev)), h/abs(dblXdev), na.rm=T)
    
    # % the cusum is the integral of the weighted deviations; we normalize
    # % here, too, by dividing by the sum of the weights
    
    cs<- cumsum(dblXweight*dblXdev)/sum(dblXweight)
    
    # cs<-cumsum(dblXdev) #simple non weighted version
    
    # we check for cusum values below zero, which would indicate a failed
    # regime change; otherwise, we have a positive shift
    if (length(which(cs < 0) > 0)) cs = 0 else  cs = cs[LL]
    cs
  }    
  
  # ------------------------------------------------------------------
  #   cusumDown()   
  #       Compute the L-pt cusum for positive level changes.  For a positive regime change to be accepted, we require the L-pt lookahead samples	to produce a cusum sequence which does not go negative.
  # -------------------------------------------------------------------
  cusumDown<-function(k){
    # LL sets the look ahead length: L, or the number of points until the end of the signal. k is the sample point running through the iteration
    LL <- min(L, N-k+1)
    
    # dblXdev is the length-LL vector of normalized deviations of x outside of the range lvl +/- thresh
    dblXdev = ((x[k:(k+LL-1)]) - (lvl-thresh)) / sqrt(sigL)
    
    #  these are Huber weight values, so large deviations are deemphasized
    dblXdev[dblXdev==0] = 1
    dblXweight = pmin(rep(1, length(dblXdev)), h/abs(dblXdev), na.rm=T)
    
    # % the cusum is the integral of the weighted deviations; we normalize
    # % here, too, by dividing by the sum of the weights
    
    cs<- cumsum(dblXweight*dblXdev)/sum(dblXweight)
    
    # cs<-cumsum(dblXdev) # simple non-weighted version
    
    # we check for cusum values above zero, which would indicate a failed
    # regime change; otherwise, we have a positive shift
    if (length(which(cs > 0) > 0)) cs = 0 else  cs = cs[LL]
    return(cs)
  }    
  
  #  -------------------------------------------------------------------------
  #      rsi(k)   
  #      Compute the rsi for a given sample index, regime mean, and critical
  #      threshold.
  #    -------------------------------------------------------------------------
  rsi<-function(k){
    if(x[k] > (lvl + thresh)){
      r = cusumUp(k)
    } else if(x[k] < (lvl - thresh)){
      r = cusumDown(k)
    } else {
      r = 0
    }
    r
  }  
  
  #  -------------------------------------------------------------------------
  # Red noise filtering of timeseries.	
  #  -------------------------------------------------------------------------			
  alpha<-AR1cor(m,y) # calculate alpha estimates
  if(AR1red=="est"){
    alpha = alpha[1]
  }else  if(AR1red=="MPK"){
    alpha = alpha[2]	
  }else if(AR1red=="IP4"){
    alpha = alpha[3]
  }else if(AR1red=="none"){
    alpha= 0
  }
  
  if(alpha<0) alpha <- 0 ; if(alpha>1) alpha <- 1
  
  # Filter time series if selected and select as x for main procedure, otherwise use timeseries
  
  if(prewhitening == T){ 	
    Zt=rep(NA, length(y))
    for(j in 2:length(y)) Zt[j]<-y[j]-(y[j-1]*alpha)
    
    if(alpha>0) x=Zt[-1] else x=y[-1]
    names(x) <- names(y)[-1]
    
  } else x=y[-1]
  
  x <- na.omit(x)
  
  #  -------------------------------------------------------------------------
  # initialization 
  #  -------------------------------------------------------------------------
  sigL = estimateSigma(x, L);           			# sample L-pt variance
  thresh = getThreshold(L, p, sigL);          # critical threshold
  lvl = hWeightedAverage(x[1:L], h);             # initial mean level
  R = rep(0, length(x));                      # rsi values
  RpVal<-rep(0, length(x))
  cp = 1;                                     # current change-point index
  N = length(x)                              # number of samples
  
  if(length(names(y))==0) {
    stop("Stopped: No ages supplied with timeseries")	
  } else ages = names(y) 
  
  
  # Main routine.
  for (k in 2:N){
    R[k] = rsi(k)
    
    #   too few samples to confirm last regime change?
    if (abs(R[k]) > 0 && k > (N-L+1)) break           
    
    #   test for regime shifts and update current regime mean (unless we are within L-pts of most recent change-point)
    if(R[k] == 0){
      if(k >= (cp + L)) lvl = hWeightedAverage(x[cp:k], h)    # same regime, far enough 
      
    } else{
      cp = k                              # regime change
      lvl = hWeightedAverage(x[k:(k+L-1)], h); # same regime, far enough from cp
    }
  }
  
  #  Calculation of new regime sample means and associated pvalues of shifts)
  if(R[length(R)] != 0) R[length(R)]<- 0
  cps<-which(abs(R)>0)
  lcps <- max(length(cps),1)
  rID<-rep(1, length(x))
  rLabel<-seq(2,lcps+1,1)
  Rmean<-rep(0, lcps+1)
  
  for(j in 1:lcps) rID[cps[j]:N]<-rLabel[j]
  for(j in 1:length(Rmean)) Rmean[j]<- hWeightedAverage(x[rID==j], h)
  # for(j in 1:length(Rmean)) Rmean[j]<- mean(x[rID==j])
  xNames= names(x)
  
  rID1 = rID
  for(j in 1:length(Rmean)) rID[rID==j]<-Rmean[j]
  
  xNA=rep(NA, length(y))
  xNA[match(xNames, names(y))] <- x
  
  RNA=rep(NA, length(y))
  RNA[match(xNames, names(y))] <- c(R)
  
  rIDNA=rep(NA, length(y)) 
  rIDNA[match(xNames, names(y))] <- c(rID)
  starsResult<-cbind(y,xNA, RNA , rIDNA) 
  
  colnames(starsResult) = c("ts", "AR1.cor", "rsi", "mean"); 
  rownames(starsResult) = ages
  
  # Estimate pValues of shifts on either white-noise filtered series, or by using the AR1 correction parameter
  pVal = rep(0, lcps)
  
  for(j in 1:lcps) {
    
    rs1 = x[rID1==j]
    rs2 = x[rID1==(j+1)]
    
    if(length(rs2)==1) {
      next
    } else {
      ifelse(prewhitening ==T, pVal[j] <- t.test(rs1, rs2)$p.value, pVal[j] <- EqP(rs1, rs2))
    }
  }
  
  if(length(which(pVal == -1)) >0) warning("pValue calculation of -1 due regime containing only one sample")		
  
  starsOUT=list(starsResult, alpha, pVal)
  names(starsOUT)=c("starsResult", "alpha", "pVal") 
  starsOUT
  
}


