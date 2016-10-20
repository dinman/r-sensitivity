D.Inman
10202016

This is built under R version 3.3.1
libraries need to be installed using install.packages ("")
libraries used: dplyr, data.table, kSamples
This should run from the directory on git

*note - variable names are lowercase, input files are name-speciic

*note - 3.subset.design.R is still under construction. The script will run as is, but requires hand-coding data ranges in for the filter. I am working on
automating this and will update the repo with an improved version.

Monte Carlo Filtering R files

1) Inputs - "design.RDA" is the full study design. "B.csv" is the subset of interest; this file only has run numbers.
2) 1.AD.R - performs Anderson-Darling multiple comparisons test on the runs contained in "B.csv" and the "design.RDA" file. Null hypothesis is 
	that vectors i and j are sampled from the same population. AD.R outputs the following R data file: "AD.results.RDA" which contains the results of 
	the anderson-darling test by model factor, anderson-darling results that are significant at p < 0.05, B.list, and B.bar.list.
3) 2.bootstrap.R - Uses B.list and B.bar.list from 1.AD.R and performs bootstraping to resample the data for use in visualizations. Outputs from this is a 
	dataset of bootstrapped resamples for B and B.bar.
4) 3.subset.design.R - uses the bootstrapped resamples to subset the original study design. Outputs from this include a filter to apply to the original study design.
5) 4.filter.check.R - Uses the filtered study design to compare it to B.csv - the new study design should be statistically different from B.


