# Bayesian SR model 
Bayesian SR Analysis model is coded in **JAGS** (see Model Codes Tab). The major difference between traditional (Frequent) and Bayesian statistics is the concept of point estimate. Traditional (Frequent) statistics assumes that observed data are derived from **single TRUE** parameters (e.g., mean) with observation error. In contrast, Bayesian statistic assumes that observed data can be derived from **a range of TRUE** parameters (e.g., range of means) that have underlined **distribution** (prior distribution). 

Frequent statistics produces two estimates: Point (e.g., sample mean) and Interval (e.g., 95% confidence interval) estimate.  Bayesian statistics produces single estimate: Posterior distribution (e.g., posterior distribution of mean) that is summarized to point (e.g., posterior mean, median) and interval (e.g., 95% credible interval) estimates.  

SR models included are **Ricker** and **Beverton-Holt** with options of **AR(1) Error** and **Time varying alpha (TVA)**  

---
## Running Bayesian SR model 

### Model Running Steps    
* **Select SR Model :**   Ricker or Beverton-Holt
* **Model Addition :**  None (Standard), AR(1) Error, or Time varying alpha  (See SR Help for details )
* **Set Simulation:** Use default numbers or increase numbers
* **Click Run** Wait until Trace plots and Summary show up.  After that, click other tabs. 
* **Click MCMC download** To save Bayesian model MCMC results as .csv file.  This will bring file saving interface.  

#### Note
* **The downloaded MCMC results file had NO information about data and model setting. Make sure to record data and model settings.**
* **Do not change columun names.**   

---
### Reading Bayesian SR model results and conduct Escapement gaol analyses. 
**The saved Bayesian MCMC model results .csv file can be uploaded for later analyse.**
* **Upload input data abd set SR Model setting the same conditions as the model results were generated:**  
* **Click Import MCMC data :**   File upload data interface will show up.  Choose file and upload. 


---
### Bayesian Model Setting 
Bayesian analyses is based on numerical simulation and sampling. Bayesian statistical quantities (e.g., model parameters estimates, CI) are simple summary of samples from the simulation. To obtain good statistics, samples needs to be taken from the simulation should be stable.  When the simulation reaches stable state, trace plots should look flat bands, and density plots should have a single defined peak.  **Note** Because Bayesian model is based on simulation base on pseudo-random number generator, estimated parameter values will change every time model is run. 


To achieve the stable simulation, every Bayesian models have following 4 controls:
* **Burn-in**  Initial simulation and highly unstable, so that all samples in this stage is thrown out. (Default: 5000)  
* **Simulation**  This is the stage when samples can be taken. Ideally this phase should be stable. (Default: 10000) 
* **Thinning**  Sample every xxth simulation. Length of the simulation and thinning will determine the number of samples from which parameter estimates are calculated: samples = simulation/ thinning.   Generally, this should be from 1000 to 10000 (Default 5)
* **Chains**  The number of simulation experiments with different starting points. JAGS selects starting points randomly from the priors. If model is correct, final simulation should reach identical mean-median regardless starting points. The number of chains are generally 1 - 5. (Default 3)  

Under the default settings, a total of (burn-in + simulation)xchains (5000+10000)x3 =45000) simulation is conducted.  Of those, 6000 samples are taken (simulation/thinning = 3x10000/5 = 6000).  Model parameters estimates are based on 6000 samples.  

The default is set to produce **quick and reasonable estimates**. It is **recommended** to increase the length of burn-in and simulations and the number of chains to obtain **better and more stable estimates**.  Generally, **longer burn-in and simulation and moree chains will produce better model parameter estimates.** However, this will also **increase** simulation time significantly. A general direction is run the model with default setting and check trace and density plots. 
Good model convergence usually indicates: **trace plots look like straight horizontal band** and **density plots have distinct single peak**.  If the plots do not look good, increase burn-in and simulation lengths until obtaining good plots. When good model convergence is  not achieved after long simulations, this is an indication that: (1) **data are uninformative**, or (2) **wrong model specifications**.  


## Median vs. Mean Management Target
The model provides two management target options: Median and Mean target. Major difference between the two target options are **whether fishery management target is median or mean recruit or yield.**  Here are difference 

* **Median Target**
  ** Aim to achieve that 50% of annual and long-term recruit-yield will fall below or above the target.    
  ** Use **alpha** for SR model to estimate biological reference points and profile.

* **Mean Target**
  ** Aim to achieve that 50% of long-term average recruit-yield will fall below or above the target.
  ** Use **alpha.c** for SR model to estimate biological reference points and profile.
  
* **Which Target is appropiate for your stock?**
Fisheries Science communities are not settled reagarding. 

Smsy based on Mean Target is 0.2% - 100% higher than Smsy based on Median Target.  The deviation depends on the estimate of alpha and sigma. In general, the deviation will be higher when alpha is low and sigma is high.  Average recruit-yield can be greatly affected by rare extreme events. 

SR-Yield plots, MCMC, and escapement goal analyses will be adjusted ted based on Median vs. Mean target goal.  

### SR-Yield Plots 
SR-Yield plots, predicted Median (solid) and Mean (Hashed) lines with options of plotting Years, Smsy, and Smax.  When SR model analyses is conducted with limited years, the excluded years will be ploted in gray. 


**Time variant alpha model**
When Time variant alpha model was selected, the plots shows periods that had similar ln.alpha. 
None indicate overall average alpha.  Smsy, Smax, and the rest of Escapement goal analyses will be based on selected time-periods. 


### Credible and Prediction Interval 
On the SR and Yield panel, user can choose to show **credible** of **Prediction** interval of user chosen upper and lower percent interval.

#### Credible Interval
Credible interval is similar to Confidence Interval.  The interval shows distribution of median or mean recruits overtime at given spawner size.   

#### Prediction Interval
Prediction interval shows distribution of single (annual) recruit at given spawner size. 


### Model Diagnoses
Diagnoses tab will display, distribution of model expected and observed recruits over time, and residual plots.  
**Time variant alpha model**
When Time variant alpha model was selected, annual changes of ln.alpha and model selected time periods will be displayed. Time variant alpha model 

## Choosing SR model for Escapement Goal Aanalyses
Among the three model variants (Standard, AR1, and Time variant alpha), we recommend standard or AR1 model for Escapement Goal Analyses.  Whether to use AR1 or standard can be decided by checking AR1 parameter **phi** at MCMC tab. **When phi at bottom of 5% is negative, there is a strong indication that phi is not statistically different from zero, or that standard model is sufficient.**



