## Spawner-Recruit model 

Two Spawner-Recruit models are available:**Ricker** and **Beverton-Holt** 

### Ricker model 
$$R = \alpha Se^{-\beta S}e^{\varepsilon}$$

### Beverton-Holt model 
$$R = \frac{\alpha S}{1+\beta S}e^{\varepsilon}$$

where 
$$\varepsilon_{iid} \sim N(0,\sigma ^{2})$$

In JAGS model,basic form of the model fitting is 

$$ln(`R) \sim N(\mu,{\sigma}^{2})$$
where $\mu$ is Ricker or Beverton-Holt Spawner Recruit equations.


Log linearlized form of Ricker and Beverton-Holt models are  

**Ricker Model**
$$ln(`R)= ln(\alpha )-\beta s+ln(S)+\varepsilon$$

**Beverton-Holt Model**
$$ln(`R)=ln(\alpha )-ln(1+\beta s)+ln(S)+\varepsilon$$
where
$s = S\cdot{10}^{-d}$ and $d = Int(log(S))$

This modeling formulation ensures that $ln(\alpha)$ and $\beta$ be at the same rage. This makes it easier to estimate model parameters. 

#### AR(1) model option 
AR(1) model (First-order autoregression model) assumes that error is serially correlated or that the value of residual at time *t* is a linear function of the residual at time *t-1*.  

This is modeled as 
${\varepsilon_{t}}=\phi {\varepsilon_{t-1}}+{\nu_{t}}$ where ${\nu_{t}} \sim N(0,{\sigma _{\nu }}^{2})$

#### Time variant alpha model option  
Time variant alpha model assumes that alpha takes random walk process. 
$${ln(\alpha )_{t}}={ln(\alpha )_{t-1}}+{\omega_{t}}$$ 
where ${\omega_{t}} \sim N(0,{\sigma _{\omega }}^{2})$

#### Model Parameters and Priors 
The model estimates following parameters

|Parameter |  | Prior  |  |    Note  |
|  ------- |---:| -------:|---:|-------:|
|_________|_|________________|_|________________________|
| $ln(\alpha)$  | | $ln(\alpha) \sim U(0,10)$ | | All models   | 
| $\beta$    |  |   $\beta \sim U(0,10)$  | | All models   | 
| $\sigma$    |   |   $\sigma \sim U(0,10)$  | | All models    | 
| $\phi$    |   | $\phi \sim U(-1,1)$   |  |**AR1 option**  | 
| $\varepsilon_{0}$ |   |  $\varepsilon_{0} \sim N(0,100)$   |  |**AR1 option**  | 
| $\sigma _{\omega}$|   |  $\sigma _{\omega} \sim U(0,10)$   |  |**TVA option**| 
|_________|_|________________|_|________________________|
---
All the priors are set to be flat, uninformative, reasonable range that are seen in salmon stock assessment. Model starting points are randomly selected from the above prior ranges. 

#### Bayesian Model results 
Bayesian Analyses estimates **distribution** of model parameters and biological reference points. Those are summarized in mean, median, and x% credible interval.

#### Parameters based on expected mean recruit 
Under the assumption of lognormal distribution of recruit (R), $`R \sim LN(ln(\mu),{\sigma}^{2})$ or $ln(`R) \sim N(\mu,{\sigma}^{2})$, $\mu$ is an expected **Median R**, and expected **Mean R** is $\mu = exp(ln(\mu) + {\sigma}^{2}/2)$.
**statistical bias corrected alpha parameters**  $ln(\alpha.c) = ln(\alpha) + {\sigma}^{2}/2$ and $\alpha.c = \alpha e^{{\sigma}^{2}/2}$


**Ricker model**
$$Mdian(`R) = \alpha Se^{-\beta S}$$
$$Mean(`R) = \alpha.c Se^{-\beta S}$$

**Beverton-Holt model** 
$$Median(`R) = \frac{\alpha S}{1+\beta S}$$
$$Mean(`R) = \frac{\alpha.c S}{1+\beta S}$$

---
#### Biological reference points
Following biological reference points are generated from the model 
* Seq   Spawner abundance expected to produce the same number of recruits
* Smsy  Spawner abundance espected to produce the maximum number of yield (MSY) 
* Umsy  Exploitation rate exepected to produce MSY
* Smax  Spawner abundance expected to produce the maximum number of recruits

**Ricker model** 
$$Seq =  \frac{ln(\alpha)}{\beta}$$
Smsy and Umsy of Riker model is an approximate where 0 < $ln(\alpha)$ < 3(Hilbon 1985) $$Smsy =  Seq(0.5-0.07ln(\alpha))$$
$$Umsy =  ln(\alpha)(0.5-0.07ln(\alpha))$$
$$Smax =  \frac{1}{\beta}$$

**For Seq.c, Smsy.c, Umsy.c Use $\alpha.c$ for lognormal bias correction** 

**Beverton-Holt model** 
$$Seq =  \frac{\alpha -1}{\beta}$$
$$Smsy =  \frac{\sqrt{\alpha} -1}{\beta}$$
$$Umsy = 1- \sqrt{\frac{1}{\alpha}}$$
Beverton-Holt model does not have Smax
$$Smax = NA$$

**For Seq.c, Smsy.c, Umsy.c Use $\alpha.c$ for lognormal bias correction**


### Bayesian SR model prediction 
After the model is fitted, the model parameters and biological reference parameters are calculated for each MCMC sample, from which standard summary statistics are calculated. 

#### Credible and Prediction Interval 
On the SR and Yield panel, user can choose to show **credible** of **Prediction** interval of user chosen upper and lower percent interval.

Credible Interval is a summary of predicted $R$ from postrior samples parameters (i) at given $S$.  

Ricker Model 
$$R_{i}^{'}=e^{ln(\alpha_{i}^{'} )-\beta_{i}^{'} S +ln(S)}$$

Beverton_Holt Model

$$R_{i}^{'}=e^{ln(\alpha_{i}^{'} )-ln(1+\beta_{i}^{'} S) +ln(S)}$$


Prediction Interval is a summary of $R$ predicted from postrior samples parameters (i) **with error** at given $S$.  

Ricker Model
$$R_{i}^{'}=e^{ln(\alpha_{i}^{'} )-\beta_{i}^{'} S +ln(S) +\varepsilon_{i}^{'}}$$

Beverton_Holt Model

$$R_{i}^{'}=e^{ln(\alpha_{i}^{'} )-ln(1+\beta_{i}^{'} S)+ln(S)+\varepsilon_{i}^{'}}$$


where $\varepsilon_{i}^{'}$ is a random sample from posterior samples (i) $\varepsilon_{i}^{'}\sim N(0,\sigma_{i}^{'} )$

Generally, credible interval indicates a range of mean (over many years) recruits at given $S$ whereas prediction interval indicates a range of (annual) recruits at given $S$.

