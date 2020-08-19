# FutureStabilityScenarios
Projecting future ecosystem stability using climate scenrios


Uses RCM of RCP 45 and RCP85 to predict future stability of ecosystem functioning calculated using MODIS EVI data.

RCM outputs downloaded from EURO CORDEX for daily precipitation and near surface temperature. 

Files:
UnrotateCoords.R takes rotated polar lat-long from EURO CORDEX data and converts them to lat-long. Based on  Simon Funder (2020). Rotated grid transform (https://www.mathworks.com/matlabcentral/fileexchange/43435-rotated-grid-transform), MATLAB Central File Exchange. Retrieved August 19, 2020. 

pr_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4_v1_day_20460101-20501230.txt file is metadata for RCM daily precipitation data for region EUR-11 with the HadGEM2-ES driver GCM for RCP-85 using the RCA4 model for the years 2046-2050 
