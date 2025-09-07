# Shiny SRA
Source code of Pacific Salmon Spawner-Recruit App
## General coding philosophy
**For longevity and stability of the code**
*  **Use Base R syntax**
*  **Avoid Packages just for single function use**
*  **Annotate Packages (what the package is used for)**
*  **Standardize code notatins**
**  Plt_ plt_ for plot object   
**  Tbl_ tbl_ for table object 
**  Txt_ txt_ for text  object 

## Repository Structure  
This repositroy consists of following folders: 
## document
This folder conteias documents in Help sections
*  Data_Input: Data input help
*  Esc_Goal: Escaoenent basics 
*  Escapement Only: Escapement Only Aanalses: Percentile, Risk
*  NSE_Analyses: MSE Analyses
*  SR_Model_EQ: SR equations
*  SR_Model_JAGS:  Runnig JAGS
## Rcode
R codes that 
## Report 
Rmd documents for report output 
## Sampledata
Contains sample data that are read in the model demonstration and testing
## WWW 
Contains figues, htmls, CSS that are directly read to UI 

