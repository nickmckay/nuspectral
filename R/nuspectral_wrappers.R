#' @export
#' @family spectra
#' @title frequency axis
#' @description Define frequency axis corresponding to time axis t 
#' @param t vector of time points
#' @return freqs vector of analysis frequencies

freq_axis = function(t){
  nt <- length(t)
  dt <- median(diff(t))
  fs <- 1 / dt
  n_freqs <- nt %/% 2 + 1
  freqs <- seq(0, fs/2, length = n_freqs)
  return(freqs)
}

#' @export
#' @family spectra
#' @title Scalogram
#' @description Wrapper for the R function `nuwaveletcoeff`
#' @param time vector of time points
#' @param vals vector of values vals = y(time)
#' @param freqs vector of analysis frequencies
#' @param taus  vector of analysis time shifts
#' @param wgt  weight function for the wavelet
#' @param wgtrad radius (scale) of the weight function  
#' @param sigma scaling parameter of the wavelet 
#' @return a list of ensemble spectra results
#' \itemize{
#' \item sclgrm: 2D array (nfreqs x ntaus) of wavelet coefficients
#' \item Neffs: Number of effective degrees of freedom  (same dimensions)
#' }    
#' @references Matthias et al, (2004), Algorithms for Spectral Analysis of Irregularly Sampled Time Series, J. Stat. Soft.
#' @references Foster, G. (1996), Wavelets for period analysis of unevenly sampled time series, Astron. Jour., 112, 1709, doi:10.1086/118137. 

nuwavelet = function(time, vals, freqs, taus, wgt=cubicwgt, wgtrad=1, sigma=0.05){
  
  if(length(time) != length(vals)){stop("time and values must have the same number of rows (observations)")}
  
  omega = 2*pi*freqs 
  so = sigma*omega

  nt = length(taus)
  nf = length(freqs)

  sclgrm = matrix(nrow = nt, ncol = nf)
  Neffs  = matrix(nrow = nt, ncol = nf)

  #pb = txtProgressBar(min=1,max = nt,style = 3)
  for(i in 1:nt){
    t_shifted = time-taus[i]
    for(j in 1:nf){
      coeff = nuwaveletcoeff(time, vals, taus[i], omega[j], wgtrad=wgtrad, sigma=sigma)
      sclgrm[i, j] = abs(coeff)**2
      weight = wgt(t_shifted*so[j])
      s = sum(weight)
      Neffs[i, j] = s^2 / sum(weight^2)
    }
    #if(i%%round(nt/10)==0){
    #  setTxtProgressBar(pb,i)
    #}
  }
  # allocate output
  out = list(sclgrm = sclgrm, Neffs = Neffs)
  return(out)
}

#' @export
#' @family spectra
#' @title nuwavelet_psd
#' @description Power spectral density derived from a `nuspectral` scalogram (nuwavelet)
#' @param time vector of time points
#' @param vals vector of values f(time)
#' @param freqs vector of analysis frequencies
#' @param taus  vector of analysis time shifts
#' @param wgt  weight function for the wavelet
#' @param wgtrad radius (scale) of the weight function  
#' @param sigma scaling parameter of the wavelet 
#' @return psd 
#' @references Matthias et al, (2004), Algorithms for Spectral Analysis of Irregularly Sampled Time Series, J. Stat. Soft.
#' @references Foster, G. (1996), Wavelets for period analysis of unevenly sampled time series, Astron. Jour., 112, 1709, doi:10.1086/118137. 
#' @references Kirchner, J. W. & Neal, C. (2013), Universal fractal scaling in stream chemistry and its implications for solute transport and water quality trend detection. PNAS 110, 12213–12218.

nuwavelet_psd = function(time, vals, freqs=NULL, taus=NULL, wgtrad=1, sigma=0.05){

  nt = length(time)
  
  if(is.null(freqs)){
    freqs = freq_axis(time)
  }
  if(is.null(taus)){
    taus = seq(min(time),max(time),length = max(nt %/% 10,5))
  }
  
  # center the series
  vals_c = vals - mean(vals)
  
  nuwv.out = nuwavelet(time, vals_c, freqs, taus, wgtrad=wgtrad, sigma=sigma) # call nuwavelet
  # rescale
  power = nuwv.out$sclgrm * 0.5 * (max(time)-min(time))/nt * nuwv.out$Neffs  # Eq. (S8) and (S9) in KN13

  Neff_diff = nuwv.out$Neffs - 3 # For neff<3, the degrees of freedom, and thus the associated weight of the spectral power estimate, are assigned a value of zero (KN13)
  Neff_diff[Neff_diff<0] = 0
  # integrate
  sum_power = colSums(power * Neff_diff, na.rm = TRUE)
  sum_eff = colSums(Neff_diff, na.rm = TRUE)
  psd  <- sum_power / sum_eff
  nf = length(freqs)
  out = list(Power = psd[2:nf], Frequency = freqs[2:nf], dof = sum_eff[2:nf]) #export, leaving out zero frequency
  return(out)
}



#' @export
#' @family spectra
#' @title nupsd 
#' @description Lomb-Scargle periodogram derived from the `nuspectral` nurealcoeff routine
#' @param time vector of time points
#' @param vals vector of values vals = y(time)
#' @param freqs vector of analysis frequencies
#' @return psd 
#' @references Matthias et al, (2004), Algorithms for Spectral Analysis of Irregularly Sampled Time Series, J. Stat. Soft.

nupsd = function(time, vals, freqs=NA){
  if(all(is.na(freqs))){
    freqs = freq_axis(time)
  }
  nf = length(freqs)
  psd = vector(0,length=nf)
  for(i in 1:nf){ 
    psd[i] = abs(nuspectral::nurealcoeff(time, vals, 2*pi*fq))**2*np.size(time)/2
  }
  return(psd)
}