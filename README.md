##### **REPLICATION PACKAGE: META-ANALYSIS OF 633,317 INDIVIDUALS SHOWS ASSOCIATIONS BETWEEN HEALTHY DIETS AND MENTAL HEALTH IN 23 LOW- AND MIDDLE-INCOME COUNTRIES**



#### 

###### **OVERVIEW** 



This project's workflow combines Stata for initial data cleaning and R for the meta-analysis and figure generation. A master R script is used to orchestrate the execution of all analyses.



#### 

###### **REPOSITORY STRUCTURE**



* Code/: Analysis scripts.

&nbsp;   - 01\_pre\_procesing.do: Data cleaning (Stata)

&nbsp;   - 02\_Meta\_analysis\_HD\_MH/: R Project folder. Contains the master script "00\_main.R" and numbered scripts (01 to 16) for each analysis stage.

* Dataset/: Input data (Excel files) and maps.
* Output/: Automatic destination for generated tables and graphs.





###### **SOFTWARE REQUIREMENTS**



1. Stata (For pre-processing).
2. R and RStudio.
3. R Packages: This project uses "renv" to manage exact dependencies for reproducibility.





###### **REPLICATION INSTRUCTIONS**



**Step 1: Pre-processing (Stata)**



1. Run the file "Code/01\_pre\_procesing.do".



* This will generate the clean data required for the R analysis phase.



**Step 2: R Environment Setup**



1. Open the file "Meta-analysis HD MH.Rproj" located in "Code/02\_Meta\_analysis\_HD\_MH/".

   This opens RStudio in the correct working directory.
   
2. Restore libraries:



* In the R console, run the following command to install the exact package versions used in the study: renv::restore()



**Step 3: Path Configuration (Important) Before running the analysis, you must indicate where the project folder is saved on your computer:**



1. Open the script "00\_main.R".
   
2. Go to section "1. Define Folder Paths" (Line 24).
   
3. Modify the "root" variable to match the path where you downloaded this repository.



**Step 4: Analysis Execution**



1. Once the path is configured, run the entire "00\_main.R" script.



* This master script will automatically: a. Set up the environment and options (seeds, scientific notation). b. Load required packages. c. Sequentially run all analysis scripts (01 to 16) located in the project folder.



**RESULTS** Results will be automatically saved in the "Output/" folder defined in the master script.









CITATION Thalia Sparling, Cesar Cornejo, Bryan Cheng et al. Meta-analysis of 633,317 individuals shows associations between healthy diets and mental health in 23 low- and middle-income countries, 30 April 2025, PREPRINT (Version 1) available at Research Square \[https://doi.org/10.21203/rs.3.rs-6530671/v1]

