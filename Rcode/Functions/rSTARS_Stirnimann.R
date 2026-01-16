
# R-package of Sequential T-test Analysis of Regime Shifts (rSTARS)
stars_citation <- function()
{
  print("Stirnimann, L., Conversi, A., and Marini, S. 2019. Detection of regime shifts in the environment: testing “STARS” using synthetic and observed time series. ICES Journal of Marine Science.")
  print("Rodionov, S. and Overland, J.E., 2005. Application of a sequential regime shift detection method to the Bering Sea ecosystem. ICES Journal of Marine Science, 62(3), pp.328-332.")
  print("Rodionov, S.N., 2004. A sequential algorithm for testing climate regime shifts. Geophysical Research Letters, 31(9).")
}

rstars <- function(data.timeseries = PDO, l.cutoff, pValue = 0.05, Huber = 1, Endfunction = F,
                  preWhitening = F, OLS = F, MPK = F, IP4 = F, SubsampleSize = (l.cutoff + 1) / 3 ,
                  FilteredData = T, save.path = (choose.dir()), timeseries = T)
{
  #call the functions
  ####definition of the parameters####
  TS <- data.timeseries
  l <- l.cutoff
  Nsub <- SubsampleSize

  #definition of prewhitening

  if (preWhitening == T)
  {
    if (OLS == F & MPK == F &  IP4 == F)
    {
      stop("preWhitening = T specify OLS, MPK or IP4")
    }

  }

  if (preWhitening == F)
  {
    FilteredData = F
    DT = 0
    if (OLS == T | MPK == T |  IP4 == T)
    {
      stop("preWhitening = F")
    }
  }

  if (preWhitening == TRUE)
  {
    RSI_mat = matrix(0, nrow = length(TS[, 1]) - 1, length(TS[1, ]))
    TabTSpw = matrix(0, nrow = length(TS[, 1]) - 1, length(TS[1, ]))
    TSpw = vector(length = length(TS[, 1]) - 1)
    RMean_mat = matrix(0, nrow = length(TS[, 1]) - 1, length(TS[1, ]))
  }

  if (preWhitening == FALSE)
  {
    RSI_mat = matrix(0, nrow = length(TS[, 1]), length(TS[1, ]))
    RMean_mat = matrix(0, nrow = length(TS[, 1]), length(TS[1, ]))
  }


 #### attaching the data set and removing of red noise ####

  for (TIMESERIESindex in 2:length(TS[1, ]))
  {
    X = ts(TS[, TIMESERIESindex])
    N = length(X)
    if (N < l)
    {
      stop("CutOff cannot be > Time series length")
    }
    #test the subsample size (Nsub) limits
    if (Nsub < 5 &  MPK == TRUE)
    {
      warning("The subsample size is too small. Automatically corrected - minimum value = 5")
      Nsub = 5
    }

    if (Nsub < 3 & (IP4 == TRUE | OLS == TRUE))
    {
      Nsub = 3
      warning("The subsample size is too small. Automatically corrected - minimum value = 3")
    }

    if (Nsub > N)
    {
      Nsub = N
    }

    #-------------------------------------------------------------------------------------------------------
    # Use prewhitening to remove red noise x(t) = x(t) - alpha * x(t-1)

    if (OLS == T | MPK == T | IP4 == T)
    {
      alpha = AlphaEstf(X,N, Nsub,MPK,IP4,OLS)
    }

    if (preWhitening == TRUE)
      #use of prewhitening to remove red noise x(t)=x(t)-alpha*x(t-1)
    {
      for (i in 2:length(X))
      {
        TSpw[i - 1] = X[i] - (alpha * X[(i - 1)])
      }
      X = TSpw
      TabTSpw[, TIMESERIESindex] = TSpw
    }

#===================#
####  STARS 3.2  ####
#===================#
# Initialization     
    df = 2 * l - 2    #Degree of freedom
    t_stu = abs(qt(pValue / 2, df)) #two tailed test     
 # Variance and Sigma calcualation for DIFF formula
    A = var(X[1:l])
    for (i in 2:(length(X) - l + 1))
    {
      B = var(X[i:(i + l - 1)])
      A = rbind(A, B)
    }
    #Sigma square
    Sigma_s = mean(A)

 #between mean values of two subsequent regimes that would be statistically
 #significant according to the Student’s t-test
    diff = t_stu * sqrt((2 * Sigma_s) / l)

#====================#
#     core steps     #
#====================#
    vRMean = 0
    RSI = seq(0, 0, length.out = length(X))
    R1 = X[1:l]
    RegimeMean = WeightedAverage(R1,Sigma_s,Huber)
    changepoint = 1
    n1 = 0

 for (intYear in 2:length(X))
   {
     if (is.na(RegimeMean) || RegimeMean == '')
      {
        break
      }
      if (Endfunction == T & intYear == (length(X) - l + 1))
      {
        if (preWhitening == F)
        {
          RSI[(length(X) - l + 1):length(X)] == seq(0, 0, length.out = l)
          break
        }

        if (preWhitening == T)
        {
          RSI[(length(X) - l + 1):(length(X) - 1)] == seq(0, 0, length.out = (l - 1))
          break
        }
      }

      if (X[intYear] > (RegimeMean + diff))
      {
        sumofWeights = 0
        cusumUP = 0
        Xdev = 0
        for (t in intYear:(intYear + l - 1))
        {
          if (t > length(X))
          {
            if (sumofWeights > 0)
            {
              break
            }
          }

     Xdev <- (X[t] - RegimeMean - diff) / sqrt(Sigma_s)
          #determine the weight of the normalized deviation
	 Xweight <- ifelse(Xdev == 0,1,min(1, (Huber / abs(Xdev)))) 
 #         if (Xdev == 0)
 #         {
 #           Xweight = 1
 #         }
 #         else if (Xdev != 0)
 #         {
 #           Xweight = min(1, (Huber / abs(Xdev)))
 #         }

          #sum weights and weighed values
          sumofWeights = sumofWeights + Xweight
          cusumUP = cusumUP + (Xdev * Xweight)

          #check if cusum turns zero
        if (cusumUP < 0)
          {
            cusumUP = 0
            break
          }
        }
        cusumUP = cusumUP / sumofWeights
		
        RSI[intYear] = cusumUP
      }

      else if (X[intYear] < (RegimeMean - diff))
      {
        sumofWeights = 0
        cusumDown = 0
        Xdev = 0
        for (t in intYear:(intYear + l - 1))
        {
          if (t > length(X))
          {
            if (sumofWeights > 0)
            {
              break
            }
          }

          Xdev = (X[t] - RegimeMean + diff) / sqrt(Sigma_s)
          #determine the weight of the normalized deviation
          if (Xdev == 0)
          {
            Xweight = 1
          }
          else if (Xdev != 0)
          {
            Xweight = min(1, (Huber / abs(Xdev)))
          }

          #sum weights and weighed values
          sumofWeights = sumofWeights + Xweight
          cusumDown = cusumDown + (Xdev * Xweight)

          #check if cusum turns zero
          if (cusumDown > 0)
          {
            cusumDown = 0
            break
          }
        }
        cusumDown = cusumDown / sumofWeights
        RSI[intYear] = cusumDown
      }


      else if (RegimeMean - diff <= X[intYear] &
               X[intYear] <= RegimeMean + diff)
      {
        RSI[intYear] = 0
      }
      #check for the situation when the test is not over for the last
      #change point, but we are too close to the end of the time series
      if (abs(RSI[intYear] > 0 & intYear > (length(X) - l + 1)))
      {
        break
      }
#------------------------------------------------------------------#
      if (RSI[intYear] == 0)
        #intYear is not a new changepoint
      {
        if ((changepoint + l) <= intYear)
        {
          #recalculate regime mean and Diff
          #currently Diff remains constant for the entire process /series
          n1 = intYear - changepoint + 1
          for (n in 1:n1)
          {
            R1[n] = X[changepoint + n - 1]
          }
          RegimeMean = WeightedAverage(R1,Sigma_s,Huber)
        }
      }

   if (RSI[intYear] != 0)
        #regime shift is detected
        #intYear is a new changepoint
      {
        changepoint = intYear
        #recalculate regime mean and Diff
        #currently Diff remains constant for the entire process /series}
        R1 = 0
        for (n in 1:l)
        {
          R1[n] = X[changepoint + n - 1]
        }
        RegimeMean = WeightedAverage(R1,Sigma_s,Huber)
      }
  }

 #Series of RegimeMeans
    if (FilteredData == T)
    {
      S = 1
      for (i in 1:length(RSI))
      {
        if (RSI[i] != 0)
        {
          E = (i - 1)
          MeanRegime = WeightedAverage(X[S:E],Sigma_s,Huber)
          vRMean1 = rep(MeanRegime, length(X[S:E]))
          vRMean = c(vRMean, vRMean1)
          S = i
        }
        if (i == length(RSI))
        {
          E = (length(RSI))
          MeanRegime = WeightedAverage(X[S:E],Sigma_s,Huber)
          vRMean1 = rep(MeanRegime, length(X[S:E]))
          vRMean = c(vRMean, vRMean1)
        }
      }
    }

    if (FilteredData == F)
    {
      X1 = TS[, TIMESERIESindex]
      S = 1
      for (i in 1:length(RSI))
      {
        if (RSI[i] != 0)
        {
          E = (i - 1)
          MeanRegime = WeightedAverage(X1[S:E],Sigma_s,Huber)
          vRMean1 = rep(MeanRegime, length(X1[S:E]))
          vRMean = c(vRMean, vRMean1)
          S = i
        }
        if (i == length(RSI))
        {
          E = (length(RSI))
          MeanRegime = WeightedAverage(X1[S:E],Sigma_s,Huber)
          vRMean1 = rep(MeanRegime, length(X1[S:E]))
          vRMean = c(vRMean, vRMean1)
        }
      }
    }

    vRMean = vRMean[-1]
    RSI_mat[, TIMESERIESindex] = RSI
    RMean_mat[, TIMESERIESindex] = vRMean

  }

  ####Saving tables of regimes avarege (tsMean.txt), RSI.txt and Filtered time series (Filredts.txt)####
   
  colnames(RMean_mat) <- colnames(data.timeseries)
  colnames(RSI_mat) <- colnames(data.timeseries)
 
  if (preWhitening == T)
  {
    zeri = seq(0, 0, length.out = length(TS[1, ]))
    RSI_mat = rbind(zeri, RSI_mat)

    empties = rep(NA, length(TS[1, ]))
    RMean_mat = rbind(empties, RMean_mat)
    TabTSpw = rbind(empties, TabTSpw)
    colnames(TabTSpw) <- colnames(data.timeseries)
    TabTSpw_save = TabTSpw
  }
  return(cbind(TS[,1],RMean_mat[,-1],RSI_mat[,-1]))  

}

# ALPHA FUNCTION
# est1 = OLS estimate of aplha
AlphaEstf = function(x,N, Nsub,MPK,IP4,OLS)
{ 
  #Subsampling and OLS process
  ss = vector(length = (N - Nsub + 1))
  yy = vector(length = Nsub)
  
  for (i in  1:(N - Nsub + 1))
  {
    for (k in 1:Nsub)
    {
      yy[k] = x[i + k - 1]
    }
    ss[i] = OLSAR1(yy)
  }

  est1 = median(ss)
  
  #bias correction of OLS if requested
  if (MPK == TRUE)
  {
    AlphaEst = IMPK(est1,Nsub)
  }
  else if (IP4 == TRUE)
  {
    AlphaEst = IPN4(est1,Nsub)
  }
  else if (OLS == TRUE)
  {
    AlphaEst = est1
  }
  
  
  #limits
  if (AlphaEst < 0)
  {
    AlphaEst = 0
  }
  if (AlphaEst > 0.99)
  {
    AlphaEst = 0.99
  }
  return (AlphaEst)
}

# IMPK
IMPK = function(est,Nsub)
{
  #Calculates bias-corrected AR(1) coefficient using the formula of
  #Marriott-Pope and Kendall(see Orcutt and Winokur, 1969, Econometrica, 37:1,1-14)
  #est is the OLS estimate of AR(1)
  #Nsub - sample size declared globaly

  if (Nsub > 4)
  {
    IMPKv = ((Nsub - 1) * est + 1) / (Nsub - 4)
  }
  else
    #should not be here
  {
    IMPKv = est
  }
  return(IMPKv)
}

IPN4 = function(est,Nsub)
  #Calculates bias-corrected AR(1) coefficient [est] using
  #interative 4-step procedure with corrections that are
  #reverse proportional to the subsample size - nsub
{
  IPN4v = est + (1 / Nsub)
  for (i in 1:3)
  {
    IPN4v = IPN4v + (abs(IPN4v) / Nsub)
  }
  return(IPN4v)
}


# ------------------------------------------------------------------------------
#    WeightedAverage(W,Sigma_s,Huber)   
#     Calculates the mean estimate for a given range using Huber's weights.
# ------------------------------------------------------------------------------      	
WeightedAverage <- function(W,Sigma_s,Huber)
{
  EstAve = mean(W)
  for (k in 1:2)
  {
    SumofWeights = 0
    SumAve = 0
    for (i in 1:length(W))
    {
      Xdev = ((W[i] - EstAve) / sqrt(Sigma_s))
      #determine the weight of the normalized deviation
      if (is.na(Xdev) || Xdev == '')
      {
        break
      }
      if (Xdev == 0)
      {
        Xweight = 1
      }
      else if (Xdev != 0)
      {
        Xweight = min(1,(Huber / abs(Xdev)))
      }   
      #sum weights and weighed values
      SumofWeights = SumofWeights + Xweight
      SumAve = SumAve + Xdev * Xweight
    }
    SumAve = SumAve / SumofWeights
    SumAve = SumAve * sqrt(Sigma_s) + EstAve
    EstAve = SumAve
  }
  WeightedAverage = EstAve
}

# ------------------------------------------------------------------------------
# OLSAR1 estimate of AR1 coefficient from Orcutt and Winokur, 1969, 
#      Econometrica, 37:1,1-14
# ------------------------------------------------------------------------------
OLSAR1 = function(OL)
{
  sumNom = 0
  sumDenom = 0
  Nobs = length(OL)
  ave1 = 0
  ave2 = 0
  for (i in 2:Nobs)
  {
    ave1 = ave1 + OL[i]
    ave2 = ave2 + OL[i - 1]
  }
  ave1 = ave1 / (Nobs - 1)
  ave2 = ave2 / (Nobs - 1)
  for (i in 2:Nobs)
  {
    sumNom = sumNom + (OL[i] - ave1) * (OL[i - 1] - ave2)
    sumDenom = sumDenom + (OL[i - 1] - ave2) * (OL[i - 1] - ave2)
  }
  
  if (sumDenom > 0)
  {
    OLSAR1v = sumNom / sumDenom
  }

  if (sumDenom <= 0)
  {
    OLSAR1v = 0
  }
  return(OLSAR1v)
}
