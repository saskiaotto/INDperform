## Resubmission
This is a resubmission. In this version I have:

* Removed the vdiffr package from the import list in namespace (see below)
* fixed the tests that caused errors in some of the environments with the last released version (see below)

## Test environments

* local OS X install, R 3.5
* local OS X install, R 3.1.1
* Windows 7, R 3.1.1, 64 bit
* win-builder (devel and release)

## R CMD check results previous (last released) version
There were 2 ERRORs and 1 NOTE.

* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘vdiffr’
  All declared Imports should be used.
  
  We removed/deactivated the visual tests (code in test files as comment currently included, reference plots removed) as we were requested by Hadley Wickham to fix to failed visual test with the new ggplot v2.3.0 release (which is not on CRAN yet so the reference plots and generated test plots did not match). Consequently we also removed 'vdiffr' removed from import list namespace, which also fixed the NOTE raised above.
  
* error in ATLAS: Failure: test excl outlier (@test_model_gamm.R#101)
  We added some tolerance when testing if 2 p-values are nearly equal.

* error in MKL: Failure: compare manual results (@test_model_gamm.R#72) 
		We added some tolerance when testing if 2 p-values are nearly equal.

* error in MKL: Failure: compare manual results (@test_model_gamm.R#62) 
	 We added some tolerance to the comparison.


## Reverse dependencies

There are currently no downstream dependencies for this package.



