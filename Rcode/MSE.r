#=================================================================================
#  MSE Simulation Rutine 
#=================================================================================
library(lmtest)
library(datasets)
library(MCMCpack)

#-----------------------------------------------------------------------
#  Initialize 
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------  
# Define Freshwater 
# Enter Age 
    lnalpha <- 1.57631941937882
    beta <- 0.0000525881430189723
    sigma <- 0.331541836872867
    f0 <- c(0,0,0,0,0,0)
    f1 <- c(0,0.032606373,0.119694807,0.310275861,0.450716033,0.086706925)
    f2 <- c(0,0,0,0,0,0)
    Rm <- 8000

  efw0 <- c(0,0.5)
  efw1 <- c(0.01,0.01)
  efw2 <- c(0.5,0)
  esw1 <- c(0.05,0.08)
  esw2 <- c(0.05,0.05)
  esw3 <- c(0.04,0.03)
  esw4 <- c(0.08,0.02)
  esw5 <- c(0.16,0)
  efw <- rbind(efw0,efw1,efw2)
  esw <- rbind(esw1,esw2,esw3,esw4,esw5)  
# Set returen age  
p.i <- c(f0,f1,f2) 
  
# Set Drishelet   
  D <- 40
  phi <- 0.6
# first age 
  fage <- 1  
# number of age groups  
  nages <- 8
# last age 
  lage <- 8
  years <- 60
  burnin <-20
  train <- 20
# Total Simulation Years 
  nyrs <- burnin+train+years
  ar1 <- function(n,cv,alpha){
    ar1 <- numeric(n)
    ar1[1] <- 0
    for(i in 2:n){
      ar1[i] <- alpha*ar1[i-1]+runif(1,-cv,cv)
    }
    ar1
  } 
  e.Rec <- ar1(nyrs,0.3,0.6)
  e.p <- rdirichlet(nyrs,alpha=p.i*D)

# output data  
  spred <- 0
  sobsH <- 0
  sobsE <- 0
  simpH <- 0
  
  e.pred <- exp(rnorm(nyrs,0,spred/100))
  e.obsH <- exp(rnorm(nyrs,0,sobsH/100))
  e.obsS <- exp(rnorm(nyrs,0,sobsE/100))
  e.imp <- exp(rnorm(nyrs,0,simpH/100))


#-----------------------------------------------------------------------
#  Import Error Data 
#-----------------------------------------------------------------------  
 

# Initial Run size   
  R0 <- Rm
# first age
  fage <- 1
# last age 
  lage <- 8
  years <-   years 
  burnin <- burnin
  train <- train
# Total Simulation Years 
  nyrs <- burnin+train+years
  EGY <- 6
#-----------------------------------------------------------------------
#  Import SR and management parameters    
#-----------------------------------------------------------------------  
  alpha <- exp(lnalpha)
  beta <- beta
  Umsy <- (0.5-0.07*lnalpha)*lnalpha
#-----------------------------------------------------------------------
#  Create Empty vector  
#-----------------------------------------------------------------------  
# Recruit
  R <- numeric(nyrs)
  R.obs.ea <- numeric(nyrs)
  R.obs.na <- numeric(nyrs)  
# Annual Run
  N <- numeric(nyrs)
# Annual Escapement
  S <- numeric(nyrs)
  S.obs <- numeric(nyrs)
# Annual Harvest 
  H <- numeric(nyrs)
  H.obs <- numeric(nyrs)
# Annuual Run by age 
  N.ta <- matrix(0,ncol=length(p.i), nrow=nyrs+lage+2)
  N.ta.obs.ea <- matrix(0,ncol=length(p.i), nrow=nyrs+lage+2)
  N.ta.obs.na <- matrix(0,ncol=length(p.i), nrow=nyrs+lage+2)
  # Annual Escapement goals  
  Egoals <- matrix(0,ncol=2, nrow = nyrs+1)
# Annual SR parameters   
  SR.sim <- matrix(0,ncol=4, nrow = nyrs)
#---------------------------------------------------------------------------
#   Start simulation 
#---------------------------------------------------------------------------
for (y in 1:nyrs){
# First generaion is constant   
 if(y<=lage) {
   N.ta[y,] <- R0*exp(e.Rec[y])*e.p[y,]
  }  
# Anunual Run is sum of all ages
N[y] <- sum(N.ta[y,])
# Predicted Run
N.pred <- N[y]*e.pred[y]
# Determine target harvest criteria	
EG.l <- mean(Egoals[y,])
if(y<=(burnin+train)){
# Before management: Harvest is at Umsy 
    H.target <- N.pred*Umsy
  } else {
# Management based on Escapement goal 
    H.target <- ifelse(N.pred < EG.l,0,min(0.9,(N.pred-EG.l)*0.5))
  }
# Actual Harvest
  H[y] <- min(H.target*e.imp[y],0.99*N[y])
# Actual Escapement 
  S[y] <- N[y] - H[y]
# Calculate Future Recruits based on SR 
  R[y] <- alpha*S[y]*exp(-beta*S[y]+e.Rec[y])
# Fill Future Return by age
  for (fw in 1:3) {
    for(sw in 1:6) {
		N.ta[y+fw+sw-1,6*(fw-1)+sw] <- R[y]*e.p[y,6*(fw-1)+sw]
		}
	}

# Apply Freshwater Age readiing error 
  foo.a <- cbind(N.ta[y,1:6],N.ta[y,7:12],N.ta[y,13:18]) 
  foo.fa <- matrix(0,nrow=6,ncol=3)
  foo.fa[,1] <-  foo.a[,1]*(1-sum(efw[1,]))+foo.a[,2]*efw[2,1]
  foo.fa[,2] <-  foo.a[,2]*(1-sum(efw[2,]))+foo.a[,1]*efw[1,2]+foo.a[,3]*efw[3,1]
  foo.fa[,3] <-  foo.a[,3]*(1-sum(efw[3,]))+foo.a[,2]*efw[2,2]
# Apply Saltwater Age readiing error   
  foo.sa <- matrix(0,nrow=6,ncol=3)
  foo.sa[1,]  <- foo.fa[1,]+foo.fa[2,]*esw[1,1]
  foo.sa[2,]  <- foo.fa[2,]*(1-sum(esw[1,]))+foo.fa[3,]*esw[2,1]+foo.fa[2,]*esw[1,2]
  foo.sa[3,]  <- foo.fa[3,]*(1-sum(esw[2,]))+foo.fa[4,]*esw[3,1]+foo.fa[3,]*esw[2,2]  
  foo.sa[4,]  <- foo.fa[4,]*(1-sum(esw[3,]))+foo.fa[5,]*esw[4,1]+foo.fa[4,]*esw[3,2]   
  foo.sa[5,]  <- foo.fa[5,]*(1-sum(esw[4,]))+foo.fa[6,]*esw[5,1]+foo.fa[5,]*esw[4,2]  
  foo.sa[6,]  <- foo.fa[6,]*(1-sum(esw[5,]))+foo.fa[5,]*esw[5,2]  
  
  N.ta.obs.ea[y,] <- c(foo.sa[,1],foo.sa[,2],foo.sa[,3])
 
# Observed Escapement 
  S.obs[y] <- S[y]*e.obsS[y]
#Observed Harvest
  H.obs[y] <- H[y]*e.obsH[y]
#Observed Run by age (Assume age comp est is accurate)
  N.ta.obs.ea[y,] <- N.ta.obs.ea[y,]*sum(S.obs[y],H.obs[y])/N[y]
  N.ta.obs.na[y,] <- N.ta[y,]*sum(S.obs[y],H.obs[y])/N[y]
  
# Create Recruitment data based on observed harvest and escapement
  R.foo.ea <- sum(
  N.ta.obs.ea[y-lage+1,1],
  N.ta.obs.ea[y-lage+2,c(2,7)],
  N.ta.obs.ea[y-lage+3,c(3,8,13)],
  N.ta.obs.ea[y-lage+4,c(4,9,14)],
  N.ta.obs.ea[y-lage+5,c(5,10,15)],
  N.ta.obs.ea[y-lage+6,c(6,11,16)],
  N.ta.obs.ea[y-lage+7,c(12,17)],
  N.ta.obs.ea[y-lage+8,18])
  R.foo.na <- sum(
  N.ta.obs.na[y-lage+1,1],
  N.ta.obs.na[y-lage+2,c(2,7)],
  N.ta.obs.na[y-lage+3,c(3,8,13)],
  N.ta.obs.na[y-lage+4,c(4,9,14)],
  N.ta.obs.na[y-lage+5,c(5,10,15)],
  N.ta.obs.na[y-lage+6,c(6,11,16)],
  N.ta.obs.na[y-lage+7,c(12,17)],
  N.ta.obs.na[y-lage+8,18])  
if(y>lage) R.obs.ea[y-lage] <- R.foo.ea
if(y>lage) R.obs.na[y-lage] <- R.foo.na
    
#-------------------------------------------------------------------------------
#   Active harvest management:  Set Escapment Goal 
#-------------------------------------------------------------------------------
  if(y>=(burnin+train)) {
# Start esimating SR model parameters
# Assume data have been collected since train    
    R.est <- R.obs.ea[(burnin+1):(y-lage)]
    S.est <- S.obs[(burnin+1):(y-lage)]	
# Calcultate SR parameters   
    lnRPS <- log(R.est/S.est)
    srfit <- lm(lnRPS~S.est)
    lnalpha.est <- coef(srfit)[1]
    beta.est <- -coef(srfit)[2]
    Smsy.est <- lnalpha.est*(0.5-0.07*lnalpha.est)/beta.est
    Smax.est <- 1/beta.est	
    SR.sim[y,] <- c(lnalpha.est,beta.est,Smsy.est,Smax.est)
    EG.m <- Smsy.est
    EG.l <- round(0.8*EG.m,-floor(log10(EG.m))+1)
    EG.u <- round(1.6*EG.m,-floor(log10(EG.m))+1)
# Board of fish: change escapement goal every bord cycle
    if((y-burnin-train)%%EGY==0){Egoals[y+1,] <- c(EG.l,EG.u)}
    else {Egoals[y+1,] <- Egoals[y,]}	
    }# End of EG management 
 } # End simulation 
 simdata <- data.frame(S,R,R.obs.na,R.obs.ea)
 simdata <- simdata[10:92,]
p.N.age.na <- cbind(N.ta.obs.na[1:nyrs,1],rowSums(N.ta.obs.na[1:nyrs,c(2,7)]),rowSums(N.ta.obs.na[1:nyrs,c(3,8,13)]),
   rowSums(N.ta.obs.na[1:nyrs,c(4,9,14)]), rowSums(N.ta.obs.na[1:nyrs,c(5,10,15)]),rowSums(N.ta.obs.na[1:nyrs,c(6,11,16)]),
   rowSums(N.ta.obs.na[1:nyrs,c(12,17)]),N.ta.obs.na[1:nyrs,18])/N
p.N.age.ea <- cbind(N.ta.obs.ea[1:nyrs,1],rowSums(N.ta.obs.ea[1:nyrs,c(2,7)]),rowSums(N.ta.obs.ea[1:nyrs,c(3,8,13)]),
   rowSums(N.ta.obs.ea[1:nyrs,c(4,9,14)]), rowSums(N.ta.obs.ea[1:nyrs,c(5,10,15)]),rowSums(N.ta.obs.ea[1:nyrs,c(6,11,16)]),
   rowSums(N.ta.obs.ea[1:nyrs,c(12,17)]),N.ta.obs.ea[1:nyrs,18])/N
p.N.age.na <- data.frame(p.N.age.na)
names(p.N.age.na) <- c('Age.1','Age.2','Age.3','Age.4','Age.5','Age.6','Age.7','Age.8')
p.N.age.ea <- data.frame(p.N.age.ea)
names(p.N.age.ea) <- c('Age.1','Age.2','Age.3','Age.4','Age.5','Age.6','Age.7','Age.8')

barplot(t(p.N.age.na),ylim=c(0,1))

df.bar <- barplot(t(p.N.age.na))
#points(df.bar,p.N.age.ea$Age.1pch=19)
points(df.bar,rowSums(p.N.age.ea[,1:2]),pch=19)
points(df.bar,rowSums(p.N.age.ea[,1:3]),pch=19)
points(df.bar,rowSums(p.N.age.ea[,1:4]),pch=19)
points(df.bar,rowSums(p.N.age.ea[,1:5]),pch=19)
points(df.bar,rowSums(p.N.age.ea[,1:6]),pch=19)
points(df.bar,rowSums(p.N.age.ea[,1:7]),pch=19)
points(df.bar,rowSums(p.N.age.ea[,1:8]),pch=19)

############################################################################
for(i in 1:length(yr)){
dat1 <- age.comp[which(age.comp$Year==yr[i]),]
barplot(as.matrix(dat1[3:6]),main=yr[i],ylim=c(0,1),col=rainbow(4),names.arg=c('TF','FW','AS','LS'))
# use rainbow(4) for color 
}

 
plot(c(1:nyrs),p.N.age.ea$Age.6,pch=19,ylim=c(0,1))
lines(c(1:nyrs),p.N.age.na$Age.6)
lines(c(1:nyrs),p.N.age.na$Age.5)
points(c(1:nyrs),p.N.age.ea$Age.5,pch=19)
 
 
 
# Without aging error 
SR.na <- lm(log(R.obs.na/S)~S,data=simdata)
SR.ae <- lm(log(R.obs.ea/S)~S,data=simdata)
SR.t <- lm(log(R/S)~S,data=simdata)
ln.alpha <- c(coef(SR.ae)[1],coef(SR.na)[1],coef(SR.t)[1])
beta <- -c(coef(SR.ae)[2],coef(SR.na)[2],coef(SR.t)[2])
Seq <- ln.alpha/beta
Smsy <- Seq*(0.5-0.07*ln.alpha)
out <- data.frame(ln.alpha,beta,Seq,Smsy)
pars <- out
 maxs <- max(1.2*pars$Seq)
  s <- seq(0,maxs,length.out =101)
  pred.ae <- predict(SR.ae, newdata=data.frame(S=s))
  pred.na <- predict(SR.na, newdata=data.frame(S=s))
  pred.t <- predict(SR.t, newdata=data.frame(S=s))
  pred.Rae <- exp(pred.ae)*s
  pred.Rna <- exp(pred.na)*s
  pred.Rt <- exp(pred.t)*s  
  out2 <- data.frame(s,pred.Rae,pred.Rna,pred.Rt)  