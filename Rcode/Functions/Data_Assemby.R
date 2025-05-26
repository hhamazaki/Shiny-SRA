#'------------------------------------------------------------------------------
#'   Data_Assembly.R  
#'   This code includes reactive functions conducting input data assemblies 
#'   Input data obhjects: data()
#'  
#'------------------------------------------------------------------------------
#'------------------------------------------------------------------------------
#' datacheck function:   Check if data includes character data 
#'------------------------------------------------------------------------------
datacheck <- function(dat){
   dm <- sapply(dat,class)
   c <- length(dm[dm %in% c('character','factor')]) # c=0 if all columns are numeric or integer
   return(c)
  }
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Run data assembly 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

brood.H <- function(tbl_run){
  tbl_run[,-c(1:3)] <- tbl_run[,-c(1:3)]*(tbl_run$N-tbl_run$S)
  brood.h <- make.brood(tbl_run,FALSE)
  SR.H <- brood.h$SR
  names(SR.H) <- c('b.Year','Spawner','Recruit.H')
  return(SR.H)
   }
#'------------------------------------------------------------------------------
#' run.cv :  If Create missing CV data : for State-Space Model
#'------------------------------------------------------------------------------
run_cv <- function(dat){
      names(dat)[1:3] <- c('Year','S','N')
      H <- with(dat,N-S)
      dat$cv_N <- with(dat,ifelse(is.na(cv_N),
                     sqrt((S*cv_E)^2+(H*cv_H)^2)/N,cv_N))
      dat$cv_N[is.na(dat$cv_N)] <- max(dat$cv_N,na.rm=TRUE)
      dat$cv_E <- with(dat,ifelse(is.na(cv_E),
                      sqrt((N*cv_N)^2-(H*cv_H)^2)/S,cv_E))
      dat$cv_E[is.na(dat$cv_E)] <- max(dat$cv_E,na.rm=TRUE)
      dat$cv_H <- with(dat,ifelse(is.na(cv_H),
                      sqrt((N*dat$cv_N)^2-(E*cv_E)^2)/H,cv_H))
      dat$cv_H[is.na(dat$cv_H)] <- max(dat$cv_H,na.rm=TRUE)
      dat.cv <- dat[,c('Year','cv_N','cv_E','cv_H','efn')]
      return(dat.cv)
}

#'------------------------------------------------------------------------------
#' run.age.var:  Calculate observed variance by age'----------------------------
#' age specific variance is calculated by multiplying 
#' Run (normal) variance and Age proportion (binomial) variance 
#' using Goodman's formula:  V(xy) = x^2*var(y)+y^2*var(x) -var(x)*var(y)
#'------------------------------------------------------------------------------ 
run_age_var <-  function(run,cv){
  # Read age data. 
      p.age <- run[,-c(1:3)] 
      nyrs <- dim(p.age)[1]  # number of years
      nage <- dim(p.age)[2]  # number of ages 
      N.var <- (run$N*cv$cv_N)^2
      S.var <- (run$S*cv$cv_E)^2
      cv$efn <- ifelse(cv$efn==0,10,cv$efn)
# Calculate age prop variance as binomial
      if(nage ==1){
        age.var <- data.frame(cbind(run$Year,S.var,N.var,N.var))
       } else {
    p.age.var <- matrix(0,nyrs,nage)
    for(i in 1:nage){
      p.age.var[,i] <-p.age[,i]*(1-p.age[,i])/cv$efn
        }
# Calculate variance by age using Goodman's formula     
    age.var <- matrix(0,nyrs,nage)
    for(i in 1:nage){
      age.var[,i] <-(run$N^2)*p.age.var[,i]+(p.age[,i]^2)*N.var-N.var*p.age.var[,i]
      }
      age.var <- data.frame(cbind(run$Year,S.var,N.var,age.var))
      } 
    names(age.var) <- names(run)  
    return(age.var)
    }

make_sr_var <- function(run,run.cv,sr){
  # Step 1. create age specific run variance
  run.age.var <- run_age_var(run,run.cv)
  # Step 2. Sum variance by brood (Spawner, Recruit variance)      
  brood.var <-  make.brood(run.age.var,FALSE)
  sr.v <- brood.var$SR
  names(sr.v) <- c('Yr','Sv','Rv')
  # Step 3. Merge variance with SR data      
  temp <- merge(sr,sr.v,by='Yr')
  # lognormal variance is ln(CV^2+1)    
  temp$Svl <- with(temp,sqrt(log(Sv/(S^2)+1))) 
  temp$Slci <- with(temp,exp(log(S)-2*Svl)) 
  temp$Suci <- with(temp,exp(log(S)+2*Svl)) 
  temp$Rvl <- with(temp,sqrt(log(Rv/(R^2)+1))) 
  temp$Rlci <- with(temp,exp(log(R)-2*Rvl)) 
  temp$Ruci <- with(temp,exp(log(R)+2*Rvl))
  out <- temp[,c('Yr','S','R','Slci','Suci','Rlci','Ruci')]
  return(out)
  }
  
