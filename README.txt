This Project is implemeted using R programming.
It creating correlation graph for a sea ice anamoly of particular region for a large time series data.Other Details inside the report inside the project


Follow the steps to run the code!

All the code in this project was run on RRO 3.2.2.
R can also be used but use of RRO is recommended as it is faster.
Install MKL library if using Intel processor. Use ACML (AMD Core Math Library) if using AMD processor.

You can install Rstudio IDE for R.

Run the code as >Rscript fullGraph.R in the terminal on linux platform.

Depending on whether you are running on Bigdata or small data few things needs to be modfifed.

1) Change the directory to the one which contains the big or small data.
this can be changed in the line setwd('C:/Users/vsingh22/Downloads/Data_for_subregion/CS310_project_subregion')

2) Change the number of rows and number of columns. For bigdata it is 304*448 and for small data it is 63*63
This can be changed by modfying the following line in te code
row<-63
col<-63

IMPORTANT: Remove readme.txt from the folder containing the data.
