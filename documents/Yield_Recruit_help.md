## Yield and Recruit Analyses
Yield and Recruit Analyses is designed to estimate a range of spawner that is expected to produce defined yield and recruit. Both analyses produces  probabilities of producing a yield/recruit that is greater than **minimum target** at given spawner size. The yield and recruit analyses produces **two** probabilities: median-mean and annual yield/recruit. The median-mean profile is a probability of producing a **long-term median-mean** yield/recruit that is greater than **minimum target** at given spawner size. The annual profile is a probability of producing a yield/recruit **at each year** that is greater than **minimum target** at given spawner size. In both cases, the profile based escapement goal range is determined by setting a target minimum probability (p) of achieving the goal.

**Note**
At any given spawner size probability of attaining annual target is **lower** than that attaining long-term median-mean target. Put it simply, definition of median-mean is center of distribution and 50% of distribution is below median-mean. Hence, when long-term mean-median target is 10000 over 10 years, you miss the target 5 out of 10 years      


### Yield Profile 
Yield Profile calculates the probability of a given escapement producing a yield that are above the minimum target yield.  The  profile is calculated in following steps.  

* At each Bayesian samples SR parameters (j) and given spawner range(S)
  * Calculate expected mean and annual yield $Y_{j}(S_{i})$ 
  * Assign $X_{i,j}$ to 1 if the expected yield is above the minimum target yield $Y_{crit}$
* At each escapement take the mean of X from all the simulation samples. 
* Yield Profile Range $S_{y}$ is a range of spawners such that the profile probability is above the minimum p% target. 

This is written as: 
**Yield based spawner profile**
$$P(S_{i}) =\frac{\sum_{j=1}^{n}X_{i,j}}{n}$$
where
$$X_{i,j}=\\begin{cases}
               1,  & \\text{if $Y_{j}(S_{i}) \\geq Y_{crit}$} \\\\
               0, & \\text{if otherwise}
               \\end{cases}\\!$$


**Yield based Escapement Goal range**
$$\left \{ S_{y}\leftarrow \left [ S_{l}\: ,\:S_{u}  \right ]|P(S)\geq p \right \}$$

### Recruti Proile 
Rmax Profile calculates the probability of a given spawner size producing recruits that are above q% of Rmax.  **Note that Ricker SR model only.  Rmax does not exist in Beverton-Holt SR model.**  

The Rmax profile is calculated in following steps.  
* At each Bayesian samples SR parameters (j) and given spawner range(S)
  * Calculate expected recruit $R_{j}(S_{i})$ 
  * Assign $X_{i,j}$ to 1 if the expected recruit is above the minimum target recruit ($R_{crit}$) or 0 if otherwise
* At each escapement take the mean of X from all the simulation samples. 
* Recruit Profile Range $S_{r}$ is a range of spawners such that the profile probability is above the minimum p% target.  

This is written as: 
**Recruit based spawner profile**
$$P(S_{i}) =\frac{\sum_{j=1}^{n}X_{i,j}}{n}$$
where
$$X_{i,j}=\\begin{cases}
               1,  & \\text{if $R_{j}(S_{i}) \\geq R_{crit}$} \\\\
               0, & \\text{if otherwise}
               \\end{cases}\\!$$
**Recruit Escapement Goal range**
$$\left \{ S_{max}\leftarrow \left [ S_{l}\: ,\:S_{u}  \right ]|P(S)\geq p \right \}$$


