# Run data type 

Analyses methods available for this data type are: 
* SR Analyses : **Ordinary** and **State-Space** 
* Escapement Goal Analyses 
* MSE (Management Strategy Evaluation) Analyses 

Run data type consists of **Calendar Year**, Spawner (Escapement) size, Run size, and run or run proportion by age.  Age notation should be either run age, starting "A" (A3),or European scale age fw age.sw age starting from "a" (a1.1).  Run age is freshwater + saltwater scale age +1.  For example, run age of scale age 1.1 = 3. 


**Run input table example** 

| Year | |  Spawner| | Run    | | A3  | |  A4 | | A5  | | A6  | 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
|1966  | |1000   | |4500  | | 0.06 | | 0.45 | | 0.43 | |  0.06  |
|1967  | |1200   | |6000  | | 0.03 | | 0.48 | | 0.42 | |  0.07  |
|1968  | |2500   | |8250  | | 0.01 | | 0.42 | | 0.52 | |  0.06  |
|1969  | |3500   | |12500 | | 0.05 | | 0.56 | | 0.38 | |  0.01  |
|1970  | |2000   | |4000  | | 0.01 | |	0.42 | | 0.52 | |	 0.06  |
|1971  | |1600   | |3000  | | 0.01 | |	0.42 | | 0.52 | |  0.06  |
|1972  | |900    | |2000  | | 0.01 | |	0.42 | | 0.52 | |  0.06  |
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
  
or 


| Year | |  Spawner| | Run    | | a1.1  | |  a1.2 | | a1.3  | | a1.4  | 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
|1966  | |1000   | |4500  | | 0.06 | | 0.45 | | 0.43 | |  0.06  |
|1967  | |1200   | |6000  | | 0.03 | | 0.48 | | 0.42 | |  0.07  |
|1968  | |2500   | |8250  | | 0.01 | | 0.42 | | 0.52 | |  0.06  |
|1969  | |3500   | |12500 | | 0.05 | | 0.56 | | 0.38 | |  0.01  |
|1970  | |2000   | |4000  | | 0.01 | |	0.42 | | 0.52 | |	 0.06  |
|1971  | |1600   | |3000  | | 0.01 | |	0.42 | | 0.52 | |  0.06  |
|1972  | |900    | |2000  | | 0.01 | |	0.42 | | 0.52 | |  0.06  |
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|

---

From the input table, the app creates a brood table starting the first calendar year's escapement and brood year recruitment.


| b.Year | |Spawner| |b.Age3 | |b.Age4 | |b.Age5 | |b.Age6 | |Recruit| 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|______|_|_______|_|_______|_|______|_|______|_|______|_|______|
| 1959| |       | |     | |     | |     | | 270| |        |
| 1961| |       | |     | |     | | 1935| | 420| |        |
| 1962| |       | |     | | 2025| | 2520| | 495| |        |
| 1963| |       | | 45  | | 2880| | 4290| | 125| | 7340   |
| 1964| |       | | 180 | | 3465| | 4750| | 240| | 8615   |
| 1965| |       | | 83  | | 7000| | 2080| | 180| | 9343   |
| 1966| | 1000  | | 625 | | 1680| | 1560| | 120| | 3985   |
| 1967| | 1200  | | 40  | | 1260| | 1040| |    | |        |
| 1968| | 2500  | | 30  | | 840 | |     | |    | |        |
| 1969| | 3500  | | 20  | |     | |     | |    | |        |
| 1970| | 2000  | |	    | |     | |	    | |    | |	      |
| 1971| | 1600  | |	    | |     | |	    | |    | |        |
| 1972| |  900  | |	    | |     | |	    |	|    | |        |
|______|_|_______|_|_______|_|______|_|______|_|______|_|______|
---

**Reduce minor age classes**

Default is making brood table based on all ages.  User can also have an option of set minimum and maximum age with  **Select Run Age** slider.

Minor age classes reduction has two options: 

* Pool option (check **Pool Ages**) : Pool  minor age to next age (e.g. combine ages 3 and 4, and ages 5 and 6)  
* Drop option (uncheck **Pool Ages**): Drop minor age and recalculate proportion (e.g. drop ages 3 and 6)  

**Original age composition**  

| Year | |  Spawner| | Run    | | A3  | |  A4 | | A5  | | A6  | 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
|1966  | |1000   | |4500  | | 0.06 | | 0.45 | | 0.43 | |  0.06  |
|1967  | |1200   | |6000  | | 0.03 | | 0.48 | | 0.42 | |  0.07  |
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|

  
  
**Pool Option** 

| Year | |  Spawner| | Run    | | A4  | |  A5 |  
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|
|1966  | |1000   | |4500  | | 0.51 | | 0.49 | 
|1967  | |1200   | |6000  | | 0.51 | | 0.49 | 
|_____|_|_______|_|______|_|_____|_|_____|
**1966:  0.06+0.45 = 0.51   0.43+0.06 = 0.49**  
**1967:  0.03+0.48 = 0.51   0.42+0.07 = 0.49**  


**Drop Option**   

| Year | |  Spawner| | Run    | | A4  | |  A5 |  
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|
|1966  | |1000   | |4500  | | 0.51 | | 0.49 | 
|1967  | |1200   | |6000  | | 0.53 | | 0.47 | 
|_____|_|_______|_|______|_|_____|_|_____|
**1966:  0.45/(0.45+0.43) = 0.51   0.43/(0.45+0.43) = 0.49**  
**1967:  0.48/(0.48+0.42) = 0.53   0.42/(0.48+0.42) = 0.47**  

## State-Space Model    
Running State-Space model requires additional data **CV and efn** 
Additional columns cv_N, cv_E, cv_H, and efn should be entered when running a state-space SR model. 

---

| Year | | ....| | cv_N    | | cv_E  | |  cv_H | | efn  |
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
|1966  | |.... | |0.27  | | 0.48 | | 0.02 | | 100 |
|1967  | |....  | |0.35  | | 0.49 | | 0.02 | | 100 | 
|1968  | |....  | |0.24  | | 0.37 | | 0.02 | | 100 |
|1969  | |....  | |0.34  | | 0.57 | | 0.02 | | 100 |
|1970  | |....  | |0.49  | | 0.67 | |	0.02 | | 100 | 
|1971  | |.... | |0.33  | | 0.51 | |	0.02 | | 100 |
|1972  | |....   | |0.14  | | 0.54 | |	0.02 | | 100 | 
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|

Generally   
* cv_N: CV of Run size (usually large: 0.1 ~ 0.8)
* cv_E: CV of Escapement size (usually large: 0.1 ~ 0.8)
* cv_H: CV of Harvest size (usually small: 0.01 ~ 0.1)
* efn: Effective sample size (usually: 50 - 100)

---
The model allows blank (NA) for one of the 3 cv columns, and estimate the missing cv as follows:  

When run size is estimated by summing Escapement and Harvest: **Run = Escapement + Harvest**, run cv is not available. 
In this case, keep cv_N column blank.  The app will estimate run CV as  
$$run CV = \frac{\sqrt{(eCV\times Esc)^{^{2}}+(hCV\times H)^{^{2}}}}{Run}$$

When Escapement is estimated by subtracting Harvest from Run: **Escapement = Run - Harvest**, escapement cv is not available.   

In this case, keep cv_E column blank.  The app will estimate run CV as  
$$esc CV = \frac{\sqrt{(runCV\times Run)^{^{2}}-(hCV\times H)^{^{2}}}}{Esc}$$

$$harv CV = \frac{\sqrt{(runCV\times Run)^{^{2}}-(eCV\times Esc)^{^{2}}}}{H}$$

**efn: Effective sample size**
The efn is **NOT** an actual (e.g. scale) sample size to estimate run age composition, but **modeling weights** (i.e.,uncertainties about age composition).  efn of 100 usually indicates 90% CI of +/- 0.1-0.15.  
**efn: Rule of thumb** 
* 100: Default: average confidence   
* <100: Less confident
* >100: Highly confident  

## Trouble shooting and Special cases 

**Don't have complete data**
The app excludes years of missing data (see brood table), and SR analyses will be conducted using complete data set. 


**Want to use Escapement data years before complete run data were collected.**
The input data calculates brood year recruit several ages before the calendar year.  In the above example, a complete brood year starts from **1966**, but brood year recruit data are available starting from **1963**.  When you **DO** have escapement data starting 1963, you arranged the input data by inserting  **Real** escapement and **Dummy** run and age data. 

| Year | |  Spawner| | Run    | | A3  | |  A4 | | A5  | | A6  | 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
| 1963 | | 2500  || 3500 | | 0.2  | | 0.2  | | 0.3  | |  0.3   |
| 1964|  | 12000| | 13000 || 0.2  | | 0.2 |  | 0.3 |  |  0.3  |
| 1965 | | 3200  || 3250  || 0.2  | | 0.2  | | 0.3|   |  0.3   |
| 1966|  | 1000 | | 4500  || 0.01 | | 0.45|  | 0.43|  |  0.06  |
| 1967 | | 1200  || 6000|  | 0.03 | | 0.48 | | 0.42 | |  0.07  |
| 1968|  | 2500 | | 8250 | | 0.01 | | 0.42|  | 0.52|  |  0.06  |
| 1969 | | 3500|  | 12500| | 0.05 | | 0.56 | | 0.38|  |  0.01  |
| 1970  || 2000 | |	4000  || 0.01 | |	0.42|	 | 0.52|  |	 0.06  |
| 1971  || 1600 | |	3000  || 0.01  ||	0.42 | | 0.52|  |  0.06  |
| 1972  ||  900  ||	2000  || 0.01|  |	0.42| | 0.52|  |  0.06  |
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|

**NOTE: This trick does not work for State-Space Model**


