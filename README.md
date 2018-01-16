# GreenSTEP
GreenSTEP model code and documentation

GreenSTEP V3.6 is consistent with the RSPM V3.8 with the major exception that RSPM V3.8 includes the autonomous vehicle and shared autonomous vehicle modeling. This version has a bug in it so use version 3.6.1.

GreenSTEP V3.6.1 adds the ability to specify a VMT tax surcharge for travel by electric vehicles (EV) and the portion of travel by plug-in hybrid electric vehicles (PHEV) that is powered by electricity. This enables users to test the effects those fees which may be imposed to replace gas taxes for those vehicles. This version also simplifies adding different MPOs without needing to reestimate the congestion model which requires 'Lamdba' parameters which are metropolitan area specific. If the user supplies a file "mpo_lambda_values.csv" in the model directory with these values, the code will read the file and substitute the values in the file for the values in the GreenSTEP_ model object. A sample file is included in the model directory. This version also corrects two bugs that were introduced in version 3.6.

Modifications to GreenSTEP V3.6.1 12/22/17:
PAYD function modified to add a choice weight for metropolitan area households. The modified function code is included in the Version_3.6.1/NewPAYD directory. See the UpdatedPAYD_Wts.txt file for motivation for change.
Corrected calculation of average heavy truck fuel consumption in non-metropolitan areas.

