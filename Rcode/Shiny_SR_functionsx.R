#'===============================================================================
# Shiny_SR_function.R 
# Collections of functions used for Shiny SR model apps
#'===============================================================================
#'===============================================================================
#  General
#'===============================================================================
## Show multiplier axis ---------------------------------------------------- 
mult <- function(u){
  mult <- ifelse(u==1000000,paste0('(x million)'),ifelse(u>1,paste0('(x',u,')'),''))  
  return(mult)  
}

#'===============================================================================
#  1.0 Data Modification ---- 
#'===============================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1  age.out:   Read run data and get age range out ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
age.out <- function(agedata){
  eage <- names(agedata)[substr(names(agedata),1,1) =='a']
  rage <- names(agedata)[substr(names(agedata),1,1) =='A']
  if(length(eage)>0){
    eage <- as.numeric(substr(eage,2,5))
    if(is.na(sum(eage))){age<- NULL}else{age <- floor(eage)+ 10*(eage-floor(eage))+1}
   } else if(length(rage)>0){
    age <- as.numeric(substr(rage,2,3))
    if(is.na(sum(age))){age<- NULL}	
   } else {age<- NULL}
  return(age)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  1.2  make.age:   Read run data and create age data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
make.age <- function(agedata,min.age,max.age,combine=TRUE){
  eage <- names(agedata)[substr(names(agedata),1,1) =='a']
  rage <- names(agedata)[substr(names(agedata),1,1) =='A']
  if(length(eage)>0){
    ac <- data.frame(t(agedata[,eage]))
    # Create European Age  fw.sw
    ac$eage <-as.numeric(substr(rownames(ac),2,5))
    # Convert European to Actual Age: freshwater age + seawater age + 1
    ac$age <- round(with(ac, floor(eage)+ 10*(eage-floor(eage)))+1)
  } else if(length(rage)>0){
    ac <- data.frame(t(agedata[rage]))
    ac$age <- round(as.numeric(substr(rownames(ac),2,3)))
   }
# Combine of eliminate age   
  if(isTRUE(combine)){ 
    ac$age <- with(ac, ifelse(age<min.age,min.age,ifelse(age >max.age,max.age,age)))
   } else {
    ac <- ac[which(ac$age>=min.age & ac$age<=max.age),]
   } 
# change NA to 0  
  ac[is.na(ac)] <- 0  
  # combine age 
  t.ac <- aggregate(.~age,sum,data=ac[,names(ac) != 'eage'])
  age <- t.ac$age
  t.ac <-data.frame(t(t.ac[,names(t.ac) != 'age']))
  names(t.ac) <- paste0('A',age)
  t.ac <- data.frame(proportions(as.matrix(t.ac),margin=1))
  return(t.ac)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.3  make.brood:   Read run data and create brood table and SR data -----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
make.brood <- function(data,p){
  # Extract name of age data (A2,A3,etc)
  A.age <- names(data)[substr(names(data),1,1) =='A']
  # Convert the name to to numeric age
  N.age <- as.numeric(substr(A.age,2,2))
# fage is the first age 
    fage <- min(N.age)
  # nages is the number of return ages
    nages <- length(N.age)
  # lage is the last reutn ages  
  lage <- fage+nages-1
  # Calculate maximum brood year range: 
  byr <- seq(min(data$Year)-lage,max(data$Year))
  # Set up brood year matrix  
  brood <- matrix(0,ncol=nages+2,nrow = length(byr))
  # First column is year 
  brood[,1] <- byr
  # Second column is Escapement by year   
  brood[,2] <- c(rep(NA,lage),data$S)
  # 3rd to the last columns are brood year return by age    
  # Age comp data 
  if(isTRUE(p)) {data[,A.age] <- data$N*data[,A.age]}
# Case: only 1 age (Pink Salmon)
  if(nages ==1){
    brood[,3] <- c(rep(NA,lage-fage),data[,3],rep(NA,fage))
    } else { 
    for(i in 1:nages){
      brood[,i+2] <- c(rep(NA,lage-fage+1-i),data[,i+3],rep(NA,fage+i-1))
    }
    }
  # Change to data.frame 
  brood <- data.frame(brood)
  # Name all columns 
  names(brood) <- c('b.Year','Spawner',paste0('b.Age',seq(fage,lage)))
  # Recruit is sum of brood year return by age 
  if(nages==1){
    brood$Recruit <- brood[,-c(1:2)]
  } else {
    brood$Recruit <- rowSums(brood[,-c(1:2)])
  }
  # Create SR data 
  SR <- brood[complete.cases(brood),c('b.Year','Spawner','Recruit')]
  out <- list(brood=brood,SR=SR)
  # Output data is a list data    
  return(out)
}


#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.4  cut.data: Cut data based on specified years -----
#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 cut.data <- function(data, years){
   x <- data[data$Yr>=years[1] & data$Yr<=years[2],]
   return(x)
 }

### Cut raw data based on brood years -----  
 cut.N.data <- function(data, years,lyear){
   x <- data[data[,1]>=years[1] & data[,1]<=(years[2]+lyear),]
   return(x)
 }
  
trimden <- function(x){
  q <- quantile(x,  probs = 0.999)
  x <- x[x<q]
  return(x)
 }

#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.5  Text functions -----  
#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' function extract right 
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

  
#'==============================================================================
#  2.0  Data Summmary functions  -----  
#'==============================================================================
#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  2.1  sum.ci -----  
#   Get summary from MCMC matrix
#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sum.ci.v <- function(x,ci){
  p <- (100-ci)/200
  min <- min(x, na.rm=TRUE)
  max <- max(x,na.rm=TRUE)
  lci <- quantile(x,prob=p,na.rm=TRUE)
  uci <- quantile(x,prob=1-p,na.rm=TRUE)
  mean <- mean(x, na.rm=TRUE)
  median <- median(x,na.rm=TRUE)
  sd <-  sd(x,na.rm=TRUE)
  out <- data.frame(min=min, max=max, sd=sd, mean=mean,median=median,lci=lci,uci=uci)
 }

sum.ci <- function(x,ci){
  p <- (100-ci)/200
  min <- apply(x,2,min, na.rm=TRUE)
  max <- apply(x,2,max, na.rm=TRUE)
  lci <- apply(x,2, function(x) quantile(x,prob=p,na.rm=TRUE))
  uci <- apply(x,2, function(x) quantile(x,prob=1-p,na.rm=TRUE))
  mean <- apply(x,2,mean, na.rm=TRUE)
  median <- apply(x,2,median, na.rm=TRUE)
  sd <-  apply(x,2,sd, na.rm=TRUE)
  out <- data.frame(min=min, max=max, sd=sd, mean=mean,median=median,lci=lci,uci=uci)
 }

#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  2.2  sum.fun -----  
#   Get summary from MCMC data.frame
#'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sum.fun <- function(x,ci){
  p <- (100-ci)/200
  min <- sapply(x,min, na.rm=TRUE)
  max <- sapply(x,max, na.rm=TRUE)
  lci <- sapply(x,quantile, prob=p, na.rm=TRUE)
  uci <- sapply(x,quantile, prob=1-p, na.rm=TRUE)
  mean <- sapply(x,mean, na.rm=TRUE)
  median <- sapply(x,median, na.rm=TRUE)
  sd <- sapply(x,sd, na.rm=TRUE)
  out <- rbind(min,lci,mean,median,uci,max,sd)
  rownames(out) <- c('Min',paste0(100*p,'%'),'Mean','Median',paste0(100*(1-p),'%'),'Max','SD')
  return(out)
 }


#'===============================================================================
# 3.0  Simulation functions: Read model specification and simulation data, 
#      calculate simulated SEQ, Smsy, Umsy, Smax
#      Clean up simulation results
#'===============================================================================

#'-------------------------------------------------------------------------------
# Id.outliers: Identify outliers ---
#'-------------------------------------------------------------------------------
outliers <- function(x){
  out <- boxplot(x, plot=FALSE)$out
  r <- which(x %in% out)
  return(r)
}

Id.outliers <- function(dat,target){ 
# Remove obvious outlier data 
# Assign 1 when beta or lnalpha are less than 0   
  dat$Remove <- ifelse(dat$beta<=0,1,NA)
  pars <- c('beta','lnalpha')
  if(target=='me'){pars <- c('beta','lnalpha.c')}
  for(i in 1:2){
  temp <-dat[,pars[i]]
  temp[which(dat$Remove>=1)] <- NA
  temp <- ifelse(temp>0,log(0.001*temp/(1-0.001*temp)),NA)
    if(length(outliers(temp)>0)) dat$Remove[outliers(temp)] <- (i+1) 
  }
  return(dat)
 }

outlier_sum <- function(post){
 dat <-data.frame(table(post$Remove,useNA='ifany'))   
 dat$Pct <- round(100*dat$n/sum(dat$n),1)
 dat$code <- as.integer(as.character(dat$code))
 note <- c('beta Negative','Outlier beta','Outlier lnalpha')
 code <- c(1,2,3)
 note <- data.frame(code,note)
 dat <- merge(dat,note, by =c('code'),all=TRUE)
 dat$code <- as.integer(dat$code)
 return(dat)
}


#'-------------------------------------------------------------------------------
# sim.out: Read MCMC data, remove outliers, and calculate biological reference 
# sim:  JAG MCMC 
# d:  multiplier 
# model:  SR model specification 
# add:  model addition (AR1, TVA)
# model.br: biological reference calculation function
#'-------------------------------------------------------------------------------
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
  
#'-------------------------------------------------------------------------------
# Create Biological reference points
#'-------------------------------------------------------------------------------
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

#'-------------------------------------------------------------------------------
# 4.0  SR.pred.sim:  Read simulation data, and create  ----
#   Create predicted Recruit and Yield (CI,PI) at given S
#'-------------------------------------------------------------------------------
SR.pred.sim <-function(SRpar,D,S,srmodel,add){
#'---------- Extract MCMC SR Model Parameters -----------------------------------
#' Mean or Median prediction-----------------------------------------------------  
  lnalpha.c <- SRpar$lnalpha.c  
  lnalpha <- SRpar$lnalpha 
  beta <- SRpar$beta
#' Extract sigma-----------------------------------------------------------------  
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
  Rl <- apply(Pred$R,2,function(x) quantile(x, pci))
  Ru <-  apply(Pred$R,2,function(x) quantile(x, 1-pci))    
# Lower PI   
  Rl.p <- apply(Pred$R.p,2,function(x) quantile(x, pci))
# Upper PI    
  Ru.p <- apply(Pred$R.p,2,function(x) quantile(x, 1-pci))
  out <- data.frame(cbind(S,RS.md,RS.me,Rl,Ru,Rl.p,Ru.p))
# Name 
  names(out) <- c('S','RS.md','RS.me','Rl','Ru','Rl.p','Ru.p')
  return(out)
}  

#===============================================================================
# 3.0  plot density distribution of MCMC results -----
#===============================================================================
plot_density_gg <- function(sim,D,ar1,model='Ricker',target='md',u){
   p <- ggplot(sim)+
     theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank())
  if(target =='me'){
  p1 <- p+geom_density(aes(x=alpha.c))+ggtitle("alpha.c")+xlim(range(sim$alpha.c,na.mr=TRUE))
  p2 <- p+geom_density(aes(x=lnalpha.c))+ggtitle("lnalpha.c")+xlim(range(sim$lnalpha.c,na.mr=TRUE))
  p4 <- p+geom_density(aes(x=Seq.c))+ggtitle("Seq.c")+xlim(range(sim$Seq.c,na.mr=TRUE))
  p5 <- p+geom_density(aes(x=Smsy.c))+ggtitle("Smsy.c")+xlim(range(sim$Smsy.c,na.mr=TRUE))
  p6 <- p+geom_density(aes(x=Umsy.c))+ggtitle("Umsy.c")+xlim(range(sim$Umsy.c,na.mr=TRUE))
  p7 <- p+geom_density(aes(x=Sgen.c))+ggtitle("Sgen.c")+xlim(range(sim$Sgen.c,na.mr=TRUE))
  } else {
  p1 <- p+geom_density(aes(x=alpha))+ggtitle("alpha")+xlim(range(sim$alpha,na.mr=TRUE))
  p2 <- p+geom_density(aes(x=lnalpha))+ggtitle("lnalpha")+xlim(range(sim$lnalpha,na.mr=TRUE))
  p4 <- p+geom_density(aes(x=Seq))+ggtitle("Seq")+xlim(range(sim$Seq,na.mr=TRUE))
  p5 <- p+geom_density(aes(x=Smsy))+ggtitle("Smsy")+xlim(range(sim$Smsy,na.mr=TRUE))
  p6 <- p+geom_density(aes(x=Umsy))+ggtitle("Umsy")+xlim(range(sim$Umsy,na.mr=TRUE))
  p7 <- p+geom_density(aes(x=Sgen))+ggtitle("Sgen")+xlim(range(sim$Sgen,na.mr=TRUE))
  }
  p3 <- p+geom_density(aes(x=beta))+ggtitle(paste0('beta',' x 10^(',-D,')'))
  if(isTRUE(ar1)){
     p8 <- p+geom_density(aes(x=phi))+ggtitle("phi")+xlim(range(sim$phi,na.mr=TRUE))
     pout <- plot_grid(p1,p2,p3,p8,p4,p5,p6,p7)  
    } else {
     pout <- plot_grid(p1,p2,p3,p4,p5,p6,p7)
    }
  if(model=='Ricker'){
  p9 <- p+geom_density(aes(x=Smax))+ggtitle("Smax")+xlim(range(sim$Smax,na.mr=TRUE))
    theme(axis.title=element_blank(),axis.text.y=element_blank())
    if(isTRUE(ar1)){
    pout <- plot_grid(p1,p2,p3,p8,p4,p5,p6,p7,p9)      
    } else {
    pout <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p9)  
    }
  }
  return(pout)
  }

#'===============================================================================
# 5.0 Profile Analyses ----
#'===============================================================================

#'===============================================================================
## 4.1 Prob.calc: Create Profile Probability of achieving target ----
#'===============================================================================
Prob.calc <- function(Y,gl){
  # Import MCMC Expected mean Yields 
  # Mean yields    
  Yb <- apply(Y,2,function(x) ifelse(x >gl,1,0))
  Ypm <- colMeans(as.matrix(Yb))  # mean yield
  return(Ypm)
 }

#'===============================================================================
## 4.2 S.intersect: Find value of S that intersect target  Probability ----
#'===============================================================================
S.intersect <- function(S,prof,tp){
    b.p <- S[prof > tp]
    if(length(b.p)>0) {sect <- c(min(b.p),max(b.p))} else {sect <- c(NA,NA)}
    return(sect)
 }

#'===============================================================================
## 4.3 prof_sim: for Targe Yield and Recruit Analyses ----
#' Calculate probability of exceeding target (tg)at given S and 
#'  find S ranges that intersects with target probability of achieving (tpg) 
#'  Create probability profile plot 
#'===============================================================================

prof_sim <- function(S,pred.m,pred.a,tg,tpg,u,target){
# Calculate probability profile  
  prof.m <- Prob.calc(pred.m,tg*u)
  prof.a <- Prob.calc(pred.a,tg*u)
  prof.ma <- data.frame(S=S,prof.m=prof.m, prof.a=prof.a)
# Calculate intersect S range   
  b.p <- S.intersect(S,prof.m,tpg)
  b.pa <- S.intersect(S,prof.a,tpg)
  range <- data.frame(b.p,b.pa)
# Create profile plot 
  tex <- ifelse(target =='me','Mean','Median')
  srange <- as.numeric(range$b.p)
  p1 <- ggplot(prof.ma)+
    geom_line(aes(x=S,y=prof.m,linetype=tex))+
    geom_line(aes(x=S,y=prof.a,linetype='Annual'))+
    geom_hline(yintercept=tpg, col=2)+
    scale_x_continuous(expand=c(0,0),limits=c(0,NA),labels = label_number(scale = 1/u),n.breaks = 10,oob=oob_keep)+
    scale_y_continuous(expand=c(0,0),limits=c(0,1),n.breaks = 10,oob=oob_keep)+
    labs(x=paste('Escapement',mult(u)),y='Probability')+
annotate('rect', xmin = srange[1], xmax = srange[2], ymin = 0, ymax =1, alpha=0.1, fill=3,na.rm = TRUE)+
  scale_linetype_manual(name ="", values=c(2,1))
  return(list(prof.ma=prof.ma,range=range,fig=p1))  
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



mult.den.plt.gg <- function(dat.a,dat.m,xlab.tx,leg.tx,u){
  df <- rbind(data.frame(x=as.vector(dat.m),a=leg.tx),data.frame(x=as.vector(dat.a),a='Annual'))
     p1 <- ggplot()+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.line.y = element_blank(),axis.ticks.y=element_blank())+  
  stat_density(data=df,aes(x=x, y=..scaled..,linetype=a), position="dodge", geom="line")+
   scale_linetype_manual(name ="", values=c(2,1))+  
   scale_x_continuous(expand=c(0, 0),labels = label_number(scale = 1/u),n.breaks = 10,oob=oob_keep) + xlab(paste(xlab.tx,mult(u)))+
   scale_y_continuous(expand=c(0, 0),limits=c(0,NA))
  return(p1)  

}


#===============================================================================
#  5.0 Profile Analyses functions ---- 
#===============================================================================
#-------------------------------------------------------------------------------    
# profile --- Profile Calculation Function  
# This function calculated probability of meeting specific target at S range 
# Calculate lower and upper end of S that meeting the target achievement criteria 
  #	S: Spawner 
  #	M.rep: Yield, Run, simulation matrix
  # mp: Target criteria: mp < 1, Percentage of max, mp > 1, specific target 
  # tp: Target percentage  (0-1)
#-------------------------------------------------------------------------------    
profile <- function(S, M.rep, mp,tp) {
  nrows <- dim(M.rep)[1]   # The number of simulation replicated
  ncols <- dim(M.rep)[2]   # The number of S = length(S)
  # Create an empty matrix  
  temp <-  matrix(0,nrow = nrows,ncol=ncols) 
  # For each simulation, assign 1 if value of M.rep is > minimum M.rep
  # Assign 0  0 if not
  if(mp > 1){
    for(j in 1:nrows){temp[j,] <- ifelse(M.rep[j,] > mp,1,0) } 
  } else {
    for(j in 1:nrows){temp[j,] <- ifelse(M.rep[j,] > mp*max(M.rep[j,],na.rm=TRUE),1,0)}
  }
  # Mean of temp matrix is a  profile probability  
  M.Rep.prof <- colMeans(temp)
  # Find range of S that intersect with target probability  
  S.prof <- S[M.Rep.prof >= tp]
  # Extract min and max S: Profile determined S range
  S.range <- c(NA,NA)
  if(sum(S.prof) > 0){S.range <- c(min(S.prof),max(S.prof))}
  # Output  
  out <- list(S = S, M.prof = M.Rep.prof, S.range = S.range)
  return(out)
}


#'===============================================================================
# Management Strategy Evaluation simulation function 
# MSE.sim  
# This function run MSE : Input data
# srmodel: SR model used to estimate SR parameters
# nyrs: number of simulation (management) years: default 50
# lnalpha.i:  SR lnalpha, beta: SR beta from model
# S0: Actual recent observed escapement that were not used for SR analyses
# e.Rec: Annual recruitment deviation   
# First years of recruits and run are generated by actual observed escapement 
#'===============================================================================
#'------------------------------------------------------------------------------
# Fishery opening trigger-----
#'------------------------------------------------------------------------------ 
# Low: Fishery opens when expected run exceeds lower goal
# Middle: Fishery opens when expected run exceeds Mid goal points 
# Upper:  Fishery opens when expected run exceeds Upper goal 
FTA <-function(cmode,LEG,UEG){
  FT <- ifelse(
  cmode =='Middle', mean(c(LEG,UEG)), # Fishery target: mid EG range 
  ifelse(cmode =='Upper',UEG, # Fishery target: Upper EG range 
  LEG # Fishery target lower EG range}       
  ))
  return(FT)
  }
# Low: Fishery opens when expected run exceeds lower goal
# Middle: Fishery opens when expected run exceeds Mid goal points 
# Upper:  Fishery opens when expected run exceeds Upper goal 
#'------------------------------------------------------------------------------
# Harvest Control -----
# MSST: Minimum Stock Size Threshold
#'------------------------------------------------------------------------------ 
H.control <-function(Npred,MSP,FMIN,FMSY,ABCP){
# Calculate FABC 
  FABC <- ABCP*FMSY
# Calculate Smsy 
  Smsy <- Npred*(1-FMSY)
# Calculate MSST
  MSST <- MSSTP*Smsy
# NL: Critical low abundance
  NL <- MSST/(1-FMIN)
#  Catch <- ifelse(
    # Case 1: Npred < MSST 
#    Npred < NL, FMIN,
#    # Case 2: Npred > MSST 
#    ifelse(,,),
#    )
}

MSE.sim <- function(srmodel,lnalpha.i,beta,S0,D,e.Rec,e.p,e.pred,e.imp,FT,mH){
#'-------------------------------------------------------------------------------
#  Create Brood Ages 
#'-------------------------------------------------------------------------------  
# lage: last age, nages: number of adult return age, fage: first adult return age
  lage <- length(S0)
  nages <- dim(e.p)[2]
  fage <- lage - nages + 1
  nyrs <- length(e.pred)
#-------------------------------------------------------------------------------
#  Create Empty vector for simulation and output  
#-------------------------------------------------------------------------------  
# Brood Year Recruit: R0: straight from model, R: R0 with error 
  R0 <- numeric(nyrs+lage)
  R <- numeric(nyrs+lage)
# Annual Run: N: True Run , N.pred: Predicted Run: Assessed run with error 
  N <- numeric(nyrs)
  N.pred <- numeric(nyrs)
# Annual Run by age 
  N.ta <- matrix(0,ncol=nages, nrow=nyrs+lage*2)
# Annual Harvest: H: True , H.target: Target Harvest 
  H <- numeric(nyrs)
  H.target <-numeric(nyrs)
# Annual Escapement: S: True   
  S <- numeric(nyrs)
# EG: Met lower escapement goal?   
  EG <- numeric(nyrs)
#-------------------------------------------------------------------------------
#  Population Dynamics: Initialization (b.y is brood year)
#-------------------------------------------------------------------------------
  for (b.y in 1:lage){
# Calculate expected Returns from SR model parameter   
    R0[b.y] <- srmodel(lnalpha.i[b.y],beta,S0[b.y],D)
# Add Error   
    R[b.y] <- R0[b.y]*exp(e.Rec[b.y])
# Distribute recruitment based on maturity schedule 
  #b.y+fage+a-1 is calendar year. 
  # Example: fage = 4: fish return as age 4.  
  # Fish spawn in b.y (say 2000) will return in 
  # 2004 as 4 years old, 2005 as 5yo, 2006 as 6 yo,...
  for (a in 1:nages){N.ta[b.y+fage+a-1,a] <- R[b.y]*e.p[b.y,a]}
  }  # Expected return   
  
#-------------------------------------------------------------------------------
#  Population Dynamics: Management Strategies (y is calendar year) 
#-------------------------------------------------------------------------------
  for (y in 1:nyrs){
# Brood year b.y is y + lage
    b.y <-y+lage
# Annual Run is sum of all ages
    N[y] <- sum(N.ta[b.y,])
# Predicted Run with error 
    N.pred[y] <- N[y]*(e.pred[y])
    
# Harvest Target----------------------------------------------------------------
    PY <- N.pred[y] - FT   # Predicted Yield  
    H.target[y] <- ifelse(
      # Case 1: Predicted Yield is negative: Fishery target is minimum harvest
      PY < 0, mH[1], 
      # Case 2: Predicted Yield is positive: 
      # fishery target is surplus or min harvest whichever bigger  
      # fishery target is also between surplus and the max harvest (fishing capacity) whichever smaller 
      min(mH[2],max(mH[1],(N.pred[y]-FT)))
    )  # End ifelse 
    # Target Harvest rate 
# Set Target harvest rate 
    HTR <- H.target[y]*(e.imp[y])/N.pred[y]
    #  Actual Harvest: Harvest will not exceed 95% of incoming run
    H[y] <- N[y]*min(HTR,0.95)
    S[y] <- N[y] - H[y]
    #     
    R0[b.y] <- srmodel(lnalpha.i[b.y],beta,S[y],D)
    # Expected return        
    R[b.y] <- R0[b.y]*exp(e.Rec[b.y])
    # Fill ages       
    for (a in 1:nages){N.ta[b.y+fage+a-1,a] <- R[b.y]*e.p[b.y,a]}
  }
  #  out <- list(R=data.frame(cbind(N,S,H,R[-c(1:lage)],R0[-c(1:lage)],EG)), N.ta=data.frame(N.ta))
  out <- data.frame(cbind(N,S,H,R[-c(1:lage)]))
  names(out) <- c('N','S','H','R')
  return(out)
}

MSE.sim2 <- function(srmodel,lnalpha.i,beta,S0,D,e.Rec,e.p,e.pred,e.imp,LEG,UEG,cmode){
#-------------------------------------------------------------------------------
#  Create Ages 
#-------------------------------------------------------------------------------  
# lage: last age, nages: number of adult return age, fage: first adult return age
  lage <- length(S0)
  nages <- dim(e.p)[2]
  fage <- lage - nages + 1
  nyrs <- length(e.pred)
#-------------------------------------------------------------------------------
#  Create Empty vector for simulation and outputs  
#-------------------------------------------------------------------------------  
# Recruit: R0: straight from model, R: R0 with error 
  R0 <- numeric(nyrs+lage)
  R <- numeric(nyrs+lage)
# Annual Run (N), Escapement (S), Harvest (H)
  N <- numeric(nyrs)
  S <- numeric(nyrs)
  H <- numeric(nyrs)
# EG: Met lower escapement goal?   
  EG <- numeric(nyrs)
# Annual Run by age 
  N.ta <- matrix(0,ncol=nages, nrow=nyrs+lage*2)
  
#-------------------------------------------------------------------------------
#  Population Dynamics: Initialization
#-------------------------------------------------------------------------------
  for (b.y in 1:lage){
# Calculate expected Recruit from SR model parameter   
  R0[b.y] <- srmodel(lnalpha.i[b.y],beta,S0[b.y],D)
# With error   
  R[b.y] <- R0[b.y]*exp(e.Rec[b.y])
# Distribute recruitment to each year
    for (a in 1:nages){N.ta[b.y+fage+a-1,a] <- R[b.y]*e.p[b.y,a]}
  }  # Expected return        
#-------------------------------------------------------------------------------
#  Population Dynamics: Start Management Strategies  
#-------------------------------------------------------------------------------
  for (y in 1:nyrs){
# Brood year b.y is y + lage
    b.y <-y+lage
# Annual Run is sum of all ages
    N[y] <- sum(N.ta[b.y,]) 
# Predicted Run with error 
    N.pred <- N[y]*e.pred[y]
# Management based on Escapement goal 
    H.target <- ifelse(
# If projected run is less than FT, Fishery is closed       
      N.pred < FT,0,  
# If projected run is greater than FT, 
# Fishery target is max harvest rate of surplus, or max harvest capacity    
      min(input$maxH,(N.pred-FT)*input$maxHr))
# Actual Harvest: Harvest will not exceed N
    H[y] <- min(H.target*e.imp[y]/N.pred, 0.9)*N[y]
    S[y] <- N[y] - H[y]
    # Escapement goal achievement 
    EG[y] <- ifelse(S[y]>input$LEG,1,0)
    R0[b.y] <- srmodel(lnalpha.i[b.y],beta,S[y],D)
    # Expected return        
    R[b.y] <- R0[b.y]*exp(e.Rec[b.y])
    # Fill ages       
    for (a in 1:nages){N.ta[b.y+fage+a-1,a] <- R[b.y]*e.p[b.y,a]}
  }

  out <- data.frame(cbind(N,S,H,EG))
  #  out <- data.frame(N.ta)
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
  
  colnames(starsResult) = c("ts", "AR1.cor", "rsi", "mean"); rownames(starsResult) = ages
  
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


