# Meta

**Author:** Andreas Höhn

**Version:** 1.0

**Date:**  2022-05-17

**About:** The Readme file for Parametric_LifeExpectancy

Estimating Life Expectancy using a Parametric Model and Comparing it with Chiang 1984

# How do I run the code? 
Running the code is very simple, you first need to download the folder 
containing all files and extract them into any folder on your PC (i.e. Desktop). 
Be aware that the program has not been tested for network drives. Close all 
tabs and RStudio. Then open "simple_two_state.rproj" file in the main folder. 
This will will open RStudio in project mode (check top left bar and ensure that 
"simple_two_state" is displayed there. Within R Studio, open "code/01_main.R" 
file and run it. All analyses wil be run automatically and the corresponding 
results will be displayed in the console, all relevant figures will be created 
and stored in the plots folder.

# Do I need to specify a working directory manually?
Nope, all file paths are defined relative, just ensure the file structure 
provided in the example is reproduced 

# Do I need to install packages manually?
Nope, the program will check automatically for all required libraries and load 
them If requiered (and only if required), it will download and install missing 
libraries. Therefore, running code for the very first time might take a bit longer. 
All subsequent runs wont require this and all will happen much faster. However, 
it is a good idea to install/update the following packages manually: "rcpp".

# How do I report bugs?
send an email to: hoehn@riseup.net 
