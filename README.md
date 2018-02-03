# GreenSTEP
GreenSTEP model code and documentation

GreenSTEP V3.6 is consistent with the RSPM V3.8 with the major exception that RSPM V3.8 includes the autonomous vehicle and shared autonomous vehicle modeling. This version has a bug in it so use version 3.6.1.

GreenSTEP V3.6.1 adds the ability to specify a VMT tax surcharge for travel by electric vehicles (EV) and the portion of travel by plug-in hybrid electric vehicles (PHEV) that is powered by electricity. This enables users to test the effects those fees which may be imposed to replace gas taxes for those vehicles. This version also simplifies adding different MPOs without needing to reestimate the congestion model which requires 'Lamdba' parameters which are metropolitan area specific. If the user supplies a file "mpo_lambda_values.csv" in the model directory with these values, the code will read the file and substitute the values in the file for the values in the GreenSTEP_ model object. A sample file is included in the model directory. This version also corrects two bugs that were introduced in version 3.6.

## Modifications to GreenSTEP V3.6.1 12/22/17
1) PAYD function modified to add a choice weight for metropolitan area households. The modified function code is included in the Version_3.6.1/Modifications directory. See the 'UpdatedPAYD_Wts.txt' file for motivation for change. 'GreenSTEP_Sim.R' script modified to call modified function with 'SynPop..' data required for it to run.
2) Corrected calculation of average heavy truck fuel consumption in non-metropolitan areas.

## Modifications to GreenSTEP V3.6.1 2/2/18
1) calcCosts function modified to calculate EV VMT surcharge to equal gas taxes paid per mile multiplied by an input value of the proportion of gas tax per mile to be paid. Modify the 'costs.csv' input file to change the 'EVVmtSurcharge' field to 'EVVmtSurchargeProp'. Rather than input an EV VMT surcharge tax (dollars per mile), the input is the proportion of the equivalent gas tax per mile that to be levied as a VMT surcharge tax on EV travel. Add the EV VMT surcharge tax (EvGasEqDvmtTax) to the function return values list.
2) Modify 'GreenSTEP_Sim.R' script to correct the calculation of 'FutrCostPerMi' in the 'CostSummary.CoVa' matrix. Was calculated as the sum of cost per mile of households in county to the mean.
3) Add 'DailyPkgCost' and 'EvGasEqDvmtTax' to 'CostSummary.CoVa' matrix.
4) Change the calculation of total costs and VMT surcharge (Step 4e) to remove inclusion of commercial service vehicle costs and revenues. Instead, add commercial service VMT to the passenger car equivalent VMT of heavy trucks and busses to compute the household proportion of roadway modernization (add lane miles) costs. This is done to avoid having to modify the commercial service vehicle costs to account for EV VMT surcharge because if commercial service vehicle costs and revenues included, the calculation of the EV VMT surcharge would need to be done by combining the household and commercial service vehicle calculations.

