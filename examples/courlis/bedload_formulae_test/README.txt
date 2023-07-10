# Validation of bedload transport formulae
***
6 cases are studied for each formula (MPM, Lefort2014, Recking2013, Recking2015) on the same geometry (flume of slope 1% and width 20 m)
***
## Description of cases
Case 00 is basecase : inflow slope of 1%, Q=100m3/s, d84=0.0573m, dm=0.03m, d16=0.00573m, d50=0.27285714m
Cases 11 & 12 studies inflow slopes : inflow slope of 2% and then 0.5%
cases 21 & 22 studies inflow : Q=300m3/s and then Q=20m3/s
Case 31 changes sediment sizes : d84=0.09545455m, dm=0.05m, d16=0.00954545m, d50=0.4545455m
## Validation
Validation is made on sediment inflow rate at last timestep (100h) using "CONCENTRATION AMONT CALCULEE AVEC PENTE EQUILIBRE = TRUE" so we check transport formula compared to excel sheets results
BUT this option is not available yet with MPM and Recking 2015 --> for these 2, we took past results for reference (sediment inflow is given by user)
***
The value is found in "FLUX MASSIQUE DE VASE" and the error should be less than 1e-4 m3/s
