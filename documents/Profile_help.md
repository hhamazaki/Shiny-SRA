## Profile Analyses
Profile Analyses have two profiles: MSY and Rmax profiles that provide basis for Smsy and Smax based escapement goal range. Both profiles are probabilities of producing a **long-term median-mean** yield/recruit greater than q% of MSY/Rmax at given spawner size.  The profile based escapement goal range is determined by setting a target minimum probability (p) of achieving the goal (i.e. intersection of the profile at given p). Standard profiles are q = 90, 80, and 70% and p = 90%.  

**Note** Value of MSY-Rmax in profile analyses is variable for each Bayesian posterior MCMC simulation sample. For instance MSY and Smsy could be 10000 and 500 in one sample and 5000 in 200 another sample. If management object is attaining **defined** yield or recruitment, use yield and recruitment analyses.  

### MSY Profile 
MSY Profile calculates the probability of a given escapement producing an yields that are above q% of MSY.  The MSY profile is calculated in following steps.  
* In each Bayesian sample of SR parameters (j) and given spawner range(S)
  * Calculate expected yield $Y_{j}(S_{i})$ and find the maximum yield (**MSY**) $Max(Y_{j})$
  * Assign $X_{i,j}$ to 1 if the expected yield is above the minimum q% of the MSY or 0 if otherwise
* At each escapement take the mean of X from all the simulation samples. 
* MSY Profile Range $S_{MSY}$ is a range of spawners such that the profile probability is above the minimum p% target. 

This is written as: 
**MSY based spawner profile**
$$P(S_{i}) =\frac{\sum_{j=1}^{n}X_{i,j}}{n}$$
where
$$X_{i,j}=\\begin{cases}
               1,  & \\text{if $Y_{j}(S_{i}) \\geq q \\cdot Max(Y_{j})$} \\\\
               0, & \\text{if otherwise}
               \\end{cases}\\!$$


               
**MSY based Escapement Goal range**
$$\left \{ S_{MSY}\leftarrow \left [ S_{l}\: ,\:S_{u}  \right ]|P(S)\geq p \right \}$$

### Rmax Proile 
Rmax Profile calculates the probability of a given spawner size producing recruits that are above q% of Rmax.  **Note that Ricker SR model only.  Rmax does not exist in Beverton-Holt SR model.**   

The Rmax profile is calculated in following steps.  
* At each Bayesian samples SR parameters (j) and given spawner range(S)
  * Calculate expected recruit $R_{j}(S_{i})$ and find the maximum recruit (**Rmax**) $Max(R_{j})$
  * Assign $X_{i,j}$ to 1 if the expected recruit is above the minimum q% of the Rmax or 0 if otherwise
* At each escapement take the mean of X from all the simulation samples. 
* Rmax Profile Range $S_{max}$ is a range of spawners such that the profile probability is above the minimum p% target.  

This is written as: 
**Rmax based spawner profile**
$$P(S_{i}) =\frac{\sum_{j=1}^{n}X_{i,j}}{n}$$
where
$$X_{i,j}=\\begin{cases}
               1,  & \\text{if $R_{j}(S_{i}) \\geq q \\cdot Max(R_{j})$} \\\\
               0, & \\text{if otherwise}
               \\end{cases}\\!$$
**Smax Escapement Goal range**
$$\left \{ S_{max}\leftarrow \left [ S_{l}\: ,\:S_{u}  \right ]|P(S)\geq p \right \}$$


