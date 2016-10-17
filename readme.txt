D.Inman
10172016

This is built under R version 3.3.1
libraries: dplyr, data.table, kSamples
This should run from the directory structure in git

*note - variable names are lowercase, input files are name-speciic, outputs and inputs are named generically - results should be removed from 
	this directory and renamed elsewhere

Monte Carlo Filtering R files

1) Inputs - "design.RDA" is the full study design. "B.csv" is the subset of interest; this file only has run numbers.
2) 1.AD.R - performs Anderson-Darling multiple comparisons test on the runs contained in "B.csv" and the "design.RDA" file. Null hypothesis is 
	that vectors i and j are sampled from the same population. AD.R outputs the following R data file: "AD.results.RDA" which contains the results of 
	the anderson-darling test by model factor, anderson-darling results that are significant at p < 0.05, B.list, and B.bar.list.
3) 2.bootstrap.R - Uses B.list and B.bar.list from 1.AD.R and performs bootstraping to resample the data for use in visualizations. Outputs from this is a 
	dataset of bootstrapped resamples for B and B.bar.
4) 3.subset.design.R - uses the bootstrapped resamples to subset the original study design. Outputs from this include an updated study design.
