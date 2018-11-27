*------------------------------------------------------------------------------*
* 
* Multidimensional Scaling Tutorial
* Author: Alex Kremzier, akremz@umich.edu
*
*------------------------------------------------------------------------------*

//Import data 
import delim using "Cars93.csv", varnames(1) clear

//Drop observations with missing values in rearseatroom or luggageroom
drop if rearseatroom == "NA" | luggageroom == "NA"
destring rearseatroom luggageroom, replace

//Macro of continuous variables
local varlist minprice price maxprice mpgcity mpghighway enginesize /*
	*/ horsepower rpm revpermile fueltankcapacity length wheelbase width /*
	*/ turncircle rearseatroom luggageroom weight
	
//MDS: Euclidean distances
mds `varlist', id(model) std(`varlist') noplot
mdsconfig, autoaspect ynegate msize(vsmall) mlabsize(vsmall)

//MDS: Manhattan distances
mds `varlist', id(model) std(`varlist') measure(manhattan) noplot
mdsconfig, autoaspect ynegate msize(vsmall) mlabsize(vsmall)

//MDS: Maximum distances
mds `varlist', id(model) std(`varlist') measure(maximum) noplot
mdsconfig, autoaspect xnegate ynegate msize(vsmall) mlabsize(vsmall)

//MDS: Correlation distances
mds `varlist', id(model) std(`varlist') measure(correlation) noplot
mdsconfig, autoaspect ynegate msize(vsmall) mlabsize(vsmall)
