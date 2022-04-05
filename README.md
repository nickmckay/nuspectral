# nuspectral
This repository contains a compiled version of the *nuspectral* package for the R language, originally written by Adolf Mathias. The method is wavelet-based and closely related to the Weighted Wavelet Z-transform by Foster [1996]. 

Credit for compiling the C portion of the code goes to Matthew Graham (<mjg@caltech.edu>). Wrappers have been developed for spectral analysis by Feng Zhu and Julien Emile-Geay (<julieneg@usc.edu>).

## Installation

To install this package, use the remotes package and this command:
`remotes::install_github("nickmckay/nuspectral")`

**References**

- Foster, G. (1996), Wavelets for period analysis of unevenly sampled time series, Astron. Jour., 112, 1709, doi:10.1086/118137.

- Mathias, A., F. Grond, R. Guardans, D. Seese, M. Canela, and H. Diebner (2004), Algorithms for spectral analysis of irregularly sampled time series, Journal of Statistical Software, Articles, 11(2), 1–27, doi:10.18637/jss.v011.i02.


