;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; HTOF_hrdata_tools
; 
; FUNCTIONS: 
; - create_ref_hrdata, creates reference structure for high res data.
; - h5_get_masscal, retrieves calibration masses and parameters
; - h5_get_massaxis, retrieves mass axis (vector with mass value for every bin).
;
; AUTHOR:
; F.Obersteiner, Feb 2015
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION create_ref_hrdata

  strct = { fname:     '',$
            massaxis:  PTR_NEW(/allocate_heap)  ,$
            chromtime: PTR_NEW(/allocate_heap)  ,$
            ix_buf:    PTR_NEW(/allocate_heap)  ,$
            ix_write:  PTR_NEW(/allocate_heap)  ,$
            spectra:   PTR_NEW(/allocate_heap)  ,$
            NbrWaveforms: 0L                    ,$
            SiS: 0.                                }           
  RETURN, strct
  
END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION h5_get_tpsscript, file

  file_id = H5F_OPEN(file)
    tpsscr_id = H5G_OPEN(file_id, 'TPSscripting')
      tpsscr_data = H5D_READ(H5D_OPEN(tpsscr_id, 'Data'))
  H5F_CLOSE, file_id
  
  strct={script_time: tpsscr_data[0,*] ,$
         tps_code:    tpsscr_data[1,*] ,$
         set_val:     tpsscr_data[2,*] ,$
         exe_time:    tpsscr_data[3,*] ,$
         buf_ix:      tpsscr_data[4,*] ,$
         write_ix:    tpsscr_data[5,*]     }
         
  RETURN, strct
  
END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION h5_get_massaxis, file, CALPARMS=calparms

  file_id = H5F_OPEN(file)
  fulspec_id = H5G_OPEN(file_id, 'FullSpectra')
    predef_MassAxis = H5D_READ(H5D_OPEN(fulspec_id, 'MassAxis')); m/z value for every bin as predefined in file
      n_bins = N_ELEMENTS(predef_MassAxis)
      bins_ix = INDGEN(n_bins, /LONG)
    massaxis = FLTARR(n_bins)*!Values.F_NAN
    MassCalibMode  = H5A_READ(H5A_OPEN_NAME(fulspec_id, 'MassCalibMode'))
    MassCalibFct   = H5A_READ(H5A_OPEN_NAME(fulspec_id, 'MassCalibration Function'))
    MassCalib_NPar = H5A_READ(H5A_OPEN_NAME(fulspec_id, 'MassCalibration nbrParameters'))
    
    IF NOT KEYWORD_SET(CALPARMS) THEN BEGIN
      calparms=FLTARR(MassCalib_NPar)
      FOR i=0, MassCalib_NPar[0]-1 DO BEGIN
        id_string='MassCalibration p'+STRCOMPRESS(STRING(i+1), /REMOVE_ALL)
        calparms[i] = H5A_READ(H5A_OPEN_NAME(fulspec_id, id_string))
      ENDFOR  
    ENDIF

    CASE MassCalibMode OF  
      0: BEGIN ; i = p1*sqrt(m) + p2
           p1 = calparms[0]
           p2 = calparms[1]
         END
      1: BEGIN ; i = p1*1/sqrt(m) + p2
           p1 = calparms[0]
           p2 = calparms[1]
         END
      2: BEGIN ; i = 
           p1 = calparms[0]
           p2 = calparms[1]
           p3 = calparms[2]
             FOR i=0L, n_bins-1 DO massaxis[i]=((i-p2)/p1)^(1./p3)
;             mass_vs_timebin=((bins_ix-p2)/p1)^(1./p3) ; vectorized loop gives slightly different result....
         END
      3: BEGIN ; i = p1*sqrt(m) + p2 + p3*(m-p4)^2
           p1 = calparms[0]
           p2 = calparms[1]
           p3 = calparms[2]
           p4 = calparms[3]
           p5 = calparms[4]
         END
      4: BEGIN ; i = p1*sqrt(m) + p2 + p3*m^2 + p4*m + p5
           p1 = calparms[0]
           p2 = calparms[1]
           p3 = calparms[2]
           p4 = calparms[3]
         END
      5: BEGIN ; m = p1*i^2 + p2*i + p3
           p1 = calparms[0]
           p2 = calparms[1]
           p3 = calparms[2]
           p4 = calparms[3]
         END
    ENDCASE
    
    H5F_CLOSE, file_id
    
  RETURN, massaxis
  
END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION h5_get_masscal, file

  file_id = H5F_OPEN(file)
  fulspec_id = H5G_OPEN(file_id, 'FullSpectra')

;+++
;+++ get parameters of calibration function
  MassCalibMode  = H5A_READ(H5A_OPEN_NAME(fulspec_id, 'MassCalibMode'))
  MassCalib_NPar = H5A_READ(H5A_OPEN_NAME(fulspec_id, 'MassCalibration nbrParameters'))
  calparms=FLTARR(MassCalib_NPar)
  FOR i=0, MassCalib_NPar[0]-1 DO BEGIN
    id_string='MassCalibration p'+STRCOMPRESS(STRING(i+1), /REMOVE_ALL)
    calparms[i] = H5A_READ(H5A_OPEN_NAME(fulspec_id, id_string))
  ENDFOR

;+++
;+++ get mass calibration masses and corresponding bins and weights
  MassCalib_NPts = H5A_READ(H5A_OPEN_NAME(fulspec_id, 'MassCalibration nbrPoints'))
  calmasses=FLTARR(MassCalib_NPts)
  calbins=FLTARR(MassCalib_NPts)
  calweights=FLTARR(MassCalib_NPts)
  FOR i=0, MassCalib_NPts[0]-1 DO BEGIN
    id_string_m='MassCalibration m'+STRCOMPRESS(STRING(i+1), /REMOVE_ALL)
    id_string_t='MassCalibration t'+STRCOMPRESS(STRING(i+1), /REMOVE_ALL)
    id_string_w='MassCalibration w'+STRCOMPRESS(STRING(i+1), /REMOVE_ALL)
    calmasses[i]=H5A_READ(H5A_OPEN_NAME(fulspec_id, id_string_m))
    calbins[i]=H5A_READ(H5A_OPEN_NAME(fulspec_id, id_string_t))
    calweights[i]=H5A_READ(H5A_OPEN_NAME(fulspec_id, id_string_w))
  ENDFOR
  H5F_CLOSE, file_id
  calib={mode:MassCalibMode, calparms:calparms, masses:calmasses, bins:calbins, weights:calweights}
  
  RETURN, calib
  
END
;---------------------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------------
FUNCTION extract_spectra, hr_data, rt_win, mass_win, F_HD=f_hd
  
  PRINT, '  -> extracting spectra... '
  
  w_rt_win  = WHERE((*hr_data.chromtime) GT rt_win[0] AND (*hr_data.chromtime) LT rt_win[1])
  n_spectra = N_ELEMENTS(w_rt_win)
  ix_write  = (*hr_data.ix_write)[w_rt_win]
  ix_buf    = (*hr_data.ix_buf)[w_rt_win]

  w_m_win=WHERE((*hr_data.massaxis) GT mass_win[0] AND (*hr_data.massaxis) LT mass_win[1])
  n_bins=N_ELEMENTS(w_m_win)
  x0_mass=(*hr_data.massaxis)[w_m_win] ; mass axis, in units of mass
  x0_bins=(INDGEN(N_ELEMENTS(*hr_data.massaxis), /LONG))[w_m_win] ; mass axis, in units of ADC bins
  x1_chromtime = (*hr_data.chromtime)[w_rt_win]

  IF NOT KEYWORD_SET(f_hd) THEN f_hd=1
  x0_mass_hd=REBIN(DOUBLE(x0_mass), n_bins*f_hd)
  x0_bins_hd=REBIN(DOUBLE(x0_bins), n_bins*f_hd)
;  x1_chromtime_hd=REBIN(DOUBLE(x_bins), N_ELEMENTS(x_chromtime_hd)*f_hd)
  
  v0_inte=FLTARR(n_bins, n_spectra) ; array with all selected spectra intensities
  v0_inte_hd=FLTARR(n_bins*f_hd, n_spectra)
  FOR i=0, n_spectra-1 DO BEGIN
    v0_inte[*,i]=((*hr_data.spectra)[w_m_win, 0, ix_buf[i], ix_write[i]])/hr_data.NbrWaveforms
    v0_inte_hd[*,i]=REBIN(DOUBLE(v0_inte[*,i]), n_bins*f_hd)
  ENDFOR

  extr_spec={ $
              n_spec: n_spectra ,$
              n_ppspec: n_bins ,$
              x0_mass: x0_mass ,$
              x0_mass_hd: x0_mass_hd ,$  
              x0_bins: x0_bins ,$  
              x0_bins_hd: x0_bins_hd ,$  
              x1_chromtime: x1_chromtime ,$  
;              x1_chromtime_hd: x1_chromtime_hd ,$  
              v0_inte: v0_inte ,$
              v0_inte_hd: v0_inte_hd $            
             }
             
  RETURN, extr_spec
  
END
;---------------------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------------
FUNCTION spec_to_trace, extr_spec, USE_HD=use_hd, NTERMS_FIT=nterms_fit, NTERMS_BASE=nterms_base, VERBOSE=verbose

  PRINT, '  -> generating mass trace from spectra... '

  n_fits = extr_spec.n_spec
  intensity = DBLARR(n_fits)
  IF NOT KEYWORD_SET(nterms_fit) THEN nterms_fit = 6
  IF NOT KEYWORD_SET(nterms_base) THEN nterms_base = 1
  
  IF KEYWORD_SET(use_hd) THEN BEGIN
    x=extr_spec.x0_mass_hd
    y=extr_spec.v0_inte_hd
  ENDIF ELSE BEGIN
    x=extr_spec.x0_mass
    y=extr_spec.v0_inte
  ENDELSE
  
  IF KEYWORD_SET(verbose) THEN BEGIN
    plots=OBJARR(3)
    plots[0]=plot(x,y[*,0], THICK=2, xtitle='mass [Th]', ytitle='intensity [mV]', DIMENSIONS=[1080,1080])
    plots[1]=plot(x,REPLICATE(0, extr_spec.n_ppspec), THICK=2, /OVERPLOT) 
    plots[2]=plot(x,REPLICATE(0, extr_spec.n_ppspec), THICK=2, /OVERPLOT) 
  ENDIF
  
  FOR n=0, n_fits-1 DO BEGIN
    estfit = gaussfit(x, y[*,n], A_gau, NTERMS=nterms_fit)
    fit = gaussfit(x, y[*,n], A, ESTIMATES=A_gau, NTERMS=nterms_fit)
    
    z = (x-A[1])/A[2]
    peak_int = A[0]*EXP(-z^2/2)
    CASE nterms_base OF
      1: base_int=A[3]
      2: base_int=A[3]+A[4]*x
      3: base_int=A[3]+A[4]*x+A[5]*x^2
    ENDCASE

    fit_area = int_tabulated(x, peak_int, /DOUBLE)
    IF (fit_area LT 0.) THEN fit_area = 0.
   
    intensity[n] = fit_area
    
    IF KEYWORD_SET(verbose) THEN BEGIN
      (plots[0]).Refresh, /DISABLE
      (plots[1]).Refresh, /DISABLE
      (plots[2]).Refresh, /DISABLE
      (plots[0]).Color = 'k'
      (plots[1]).Color = 'b'
      (plots[2]).Color = 'r'
      (plots[0]).SetData,   x, y[*,n]
;      (plots[1]).SetData,   x, base_int
      (plots[2]).SetData,   x, peak_int;+base_int
      (plots[0]).Refresh
      (plots[1]).Refresh
      (plots[2]).Refresh

      Print, 'halt'
    ENDIF
    
  ENDFOR
  
  masstrace = { $
               time: extr_spec.x1_chromtime ,$
               intensity: intensity $               
               }
               
  RETURN, masstrace
  
END
;---------------------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------------
FUNCTION get_mc_data, fname, hr_data, rt_win, mass_win, target_ion, SUM_SPEC=sum_spec, F_HD=f_hd, $
                      FITFCT=fitfct, ID=id, VERBOSE=verbose
                         
  IF NOT KEYWORD_SET(ID) THEN ID='NA'
  
  ; +++
  ; +++ Retention Time Settings (chromatographic time axis)
  rt_set    = rt_win
;  rt_set = [592.1, 592.5] ; CH3CCl3
;  rt_set = [299.8,300.1]; CFC 12 apex
  w_rt_win  = WHERE((*hr_data.chromtime) GT rt_set[0] AND (*hr_data.chromtime) LT rt_set[1])
  n_spectra = N_ELEMENTS(w_rt_win)
  ix_write  = (*hr_data.ix_write)[w_rt_win]
  ix_buf    = (*hr_data.ix_buf)[w_rt_win]

  ; +++
  ; +++ Mass Axis Settings
;  mass_win = [96.5,99.5] ; CH3CCl3
;  mass_win = [75.,95.] ; wide around 85 CFC 12
  w_m_win=WHERE((*hr_data.massaxis) GT mass_win[0] AND (*hr_data.massaxis) LT mass_win[1])
  n_bins=N_ELEMENTS(w_m_win)
  x_mass=(*hr_data.massaxis)[w_m_win] ; mass axis, in units of mass
  x_bins=(INDGEN(N_ELEMENTS(*hr_data.massaxis), /LONG))[w_m_win] ; mass axis, in units of ADC bins

  ; +++
  ; +++ Mass Intensity: gather spectra and find highest intensity
  v_inte=FLTARR(n_bins, n_spectra) ; array with all selected spectra intensities
  max_int=-1.
  w_max_int=-1
  spec_w_max=-1
  FOR i=0, n_spectra-1 DO BEGIN
    v_inte[*,i]=((*hr_data.spectra)[w_m_win, 0, ix_buf[i], ix_write[i]])/hr_data.NbrWaveforms
    maxi=MAX(v_inte[*,i], w_max)
    IF maxi GT max_int THEN BEGIN
      max_int=maxi ; highest intensity value in selected spectra (units of ADC intensity)
      w_max=w_max_int ; index of the maximum within the spectrum
      spec_w_max=i ; index of the spectrum containing the maximum
    ENDIF
  ENDFOR

  ; +++
  ; +++ Alternative Mass Intensity: gather spectra and build sum spectrum
  IF SIZE(v_inte, /N_DIMENSIONS) EQ 1 THEN v_inte_sum=TOTAL(v_inte) $ ; check possibility that only one spectrum was defined
    ELSE v_inte_sum=TOTAL(v_inte,2) 
  
  IF KEYWORD_SET(SUM_SPEC) THEN v_inte=v_inte_sum

  ; +++
  ; +++ increase data density
  ; +++ idl gaussfit to get estimates and lorentzian fit
  IF NOT KEYWORD_SET(f_hd) THEN f_hd=1

  x_mass_hd=REBIN(DOUBLE(x_mass), N_ELEMENTS(x_mass)*f_hd)
  x_bins_hd=REBIN(DOUBLE(x_bins), N_ELEMENTS(x_bins)*f_hd)

  IF KEYWORD_SET(SUM_SPEC) THEN v_hd=REBIN(DOUBLE(v_inte), N_ELEMENTS(v_inte)*f_hd) $
    ELSE v_hd=REBIN(DOUBLE(v_inte[*,spec_w_max]), N_ELEMENTS(v_inte[*,spec_w_max])*f_hd)

  gfit = gaussfit(x_mass_hd, v_hd, A_gau)
  gbinfit = gaussfit(x_bins_hd, v_hd, A_gaub)
  m_estimates = A_gau
  b_estimates = A_gaub

  IF NOT KEYWORD_SET(fit_fct) THEN fit_fct=0

  CASE fit_fct OF
    0: BEGIN ; gaussfit
      mpfit = mpfitpeak(x_mass_hd, v_hd, A_mp, ESTIMATES=m_estimates, /GAUSSIAN)
      mpbinfit = mpfitpeak(x_bins_hd, v_hd, A_mpbin, ESTIMATES=b_estimates, YERROR=yerror, /GAUSSIAN)
      massyerror=SQRT(TOTAL((mpfit-v_hd)^2, /NAN)/FLOAT(N_ELEMENTS(mpfit)))
      binyerror=SQRT(TOTAL((mpbinfit-v_hd)^2, /NAN)/FLOAT(N_ELEMENTS(mpfit)))
      m_estimates = A_mp
      b_estimates = A_mpbin
    END
    1: BEGIN; lorentzfit
      mpfit = mpfitpeak(x_mass_hd, v_hd, A_mp, ESTIMATES=m_estimates, YERROR=yerror, /LORENTZIAN)
      mpbinfit = mpfitpeak(x_bins_hd, v_hd, A_mpbin, ESTIMATES=b_estimates, YERROR=yerror, /LORENTZIAN)
      massyerror=SQRT(TOTAL((mpfit-v_hd)^2, /NAN)/FLOAT(N_ELEMENTS(mpfit)))
      binyerror=SQRT(TOTAL((mpbinfit-v_hd)^2, /NAN)/FLOAT(N_ELEMENTS(mpfit)))
      m_estimates = A_mp
      b_estimates = A_mpbin
    END
    2: BEGIN; moffatfit
      mpfit = mpfitpeak(x_mass_hd, v_hd, A_mp, ESTIMATES=m_estimates, YERROR=yerror, /MOFFAT)
      mpbinfit = mpfitpeak(x_bins_hd, v_hd, A_mpbin, ESTIMATES=b_estimates, YERROR=yerror, /MOFFAT)
      massyerror=SQRT(TOTAL((mpfit-v_hd)^2, /NAN)/FLOAT(N_ELEMENTS(mpfit)))
      binyerror=SQRT(TOTAL((mpbinfit-v_hd)^2, /NAN)/FLOAT(N_ELEMENTS(mpfit)))
      m_estimates = A_mp
      b_estimates = A_mpbin
    END
  ENDCASE


; +++
; +++ calculate mass resolution and accuracy
  meas_ion = A_mp[1]
  apex_flightbin = A_mpbin[1]
  HWHM = A_mp[2]
  R = A_mp[1]/(HWHM*2.)
  mass_acc = (meas_ion-target_ion)/(meas_ion*1E-6)
  mass_cal = h5_get_masscal(fname)
  match = matchmass(mass_cal.masses, meas_ion) ; function from wid_int_tools
  calmass_near = match[0]
  calmass_dif = match[2]
  
; +++
; +++ asymetric lorentz fit: max int spectrum, use gaussian sigma to define asymetry
  match=matchmass(x_mass, (meas_ion-HWHM))
  lin=linfit([28.,555.],[2.,20.]) ; 28 eq. 2, 554 eq. 20
  asy_l = lin[0]+lin[1]*meas_ion ; mass peaks tend to become more symmetric the heavier the ion
  asy_r = 20
  
; +++
; +++ max. intensity of selected spectrum or sum spec
    max_int=MAX(v_hd, ix_max)
    dx_1sig=ROUND(ix_max-match[1]) ; index difference of the 1 sig (or hwhm in this case) towards index of maximum intensity
    IF (ix_max-dx_1sig*asy_l) LE 0 THEN ix_nsig_l = 0 $ ; ensure correct subscript range
      ELSE ix_nsig_l = ROUND(ix_max-dx_1sig*asy_l)
    IF (ix_max+dx_1sig*asy_r) GE n_bins THEN ix_nsig_r = n_bins-1 $
      ELSE ix_nsig_r = ROUND(ix_max+dx_1sig*asy_r) 

    v=(v_hd)[ix_nsig_l:ix_nsig_r]
    x=x_mass[ix_nsig_l:ix_nsig_r]
    asyfit=mpfitpeak(x, v, A_asyfit, ESTIMATES=m_estimates, YERROR=yerror, /LORENTZIAN)
    
;    p=plot(x,v, title=strcompress(string(asy_l)+':'+string(asy_r), /remove_all))
;    p=plot(x,asyfit,'r',/overplot)
    R_asy=meas_ion/(2.*A_asyfit[2])
    mass_acc_asy = (A_asyfit[1]-target_ion)/(A_asyfit[1]*1E-6)
;    print, 'max.int.spec.: ', r_asy
    
  ; +++
  ; +++ summed spectra
;  max_int=MAX(v_inte_sum, ix_max)
;  ix_asy=ix_max-match[1]
;  v=(v_inte_sum)[ix_max-ix_asy:N_ELEMENTS(v_inte_sum)-1] ; from right
;  x=x_mass[ix_max-ix_asy:N_ELEMENTS(v_inte_sum)-1]
;  asyfit=mpfitpeak(x, v, A_asyfit, ESTIMATES=m_estimates, YERROR=yerror, /LORENTZIAN)
;  ;  p=plot(x,v)
;  ;  p=plot(x,asyfit,'r',/overplot)
;  R_asys=meas_ion/(2.*A_asyfit[2])
;  print, 'max.int.spec.: ', r_asy, ' ++> sum.int.spec.: ', r_asys
    
; +++
; +++ asymetric lorentz fit: all defined spectra
;  FOR i=0, n_spectra-1 DO BEGIN
;    half_max = (MAX(v_inte[*,i], ix_max)-MIN(v_inte[*,i]))/2.
;    v=(v_inte[*,i])[ix_max-5:N_ELEMENTS(v_inte[*,i])-1]
;    x=x_mass[ix_max-5:N_ELEMENTS(v_inte[*,i])-1]
;    asyfit=mpfitpeak(x, v, A_asyfit, ESTIMATES=m_estimates, YERROR=yerror, /LORENTZIAN)
;;    p=plot(x,v)
;;    p=plot(x,asyfit,'r',/overplot)
;    print, meas_ion/(2.*A_asyfit[2])    
;  ENDFOR


; +++
; +++ diagnostics
  IF KEYWORD_SET(verbose) THEN BEGIN
    print, 'hd factor ', f_hd
    print, 'apex_mass [Th]', STRING(apex_mass, FORMAT='(F12.5)')
    print, 'apex mass flight bin ', STRING(apex_flightbin, FORMAT='(F15.3)')
    print, 'R ', STRING(R, FORMAT='(I)')
    print, 'mass acc [ppm] ', STRING(mass_acc, FORMAT='(F12.1)')
    p2=plot(x_mass,v_inte_sum,xtitle='mass [Th]',ytitle='ADC intensity',title='summed intensity of sel. spectra')
    p0=plot(x_mass,v_inte[*,spec_w_max],xtitle='mass [Th]',ytitle='ADC intensity',title='max intensity spectrum')
    ;    p1=plot(x_bins,v_inte[*,spec_w_max],xtitle='ADC bins',ytitle='ADC intensity',title='max intensity spectrum')
    p3=plot(x_mass_hd, gfit, 'r', /overplot)
    p4=plot(x_mass_hd, mpfit, 'b', /overplot)
    ;    p5=plot(x_bins_hd, gfit, 'b', /overplot)
  ENDIF

  ; +++
  ; +++ results
  strct= {  fname:fname ,$
            id:id ,$
            mass:meas_ion ,$
            fwhm:2.*HWHM ,$
            bin:apex_flightbin ,$
            target:target_ion ,$
            dev_ppm:mass_acc ,$
            near_cal:calmass_near ,$
            dif_cal:calmass_dif ,$
            R:R ,$
            R_asy:R_asy ,$
            max_int:max_int ,$
            fitfct:fitfct ,$
            binfityerror:binyerror ,$
            binweight:1.-mean(binyerror)/mean(v_hd)  }

  RETURN, strct
  
END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION recalc_peakdata, fname, peaktable, mass, peakdata, LIMITS_NOM=limits_nom, $
                          LIMITS_ACC=limits_acc, USE_PT_LIMITS=use_pt_limits, $
                          F_HD=f_hd, INTEGRATE=integrate, VERBOSE=verbose
                                            
  t0=SYSTIME(1)
  
  IF NOT KEYWORD_SET(verbose) THEN verbose = 0 
  IF NOT KEYWORD_SET(f_hd) THEN f_hd = 100
  IF NOT KEYWORD_SET(limits_acc) THEN limits_acc = [0.0175, 0.0175]
  IF NOT KEYWORD_SET(limits_nom) THEN limits_nom = limits_acc*3.
  
  targets = (peaktable.mass)[WHERE(peaktable.label NE 'nominal')] ; get m/Q traces to recalculate
                                                                  ; ! make sure to use 'nominal' as tagname in peaktable.
  
  intensity = REFORM(peakdata, N_ELEMENTS(peakdata))              ; previous peakdata array
  
  hr_data = read_tofwerkh5_hr(fname)                              ; get hr data
  
  IF VERBOSE THEN print, 'hr data imported in [s]: ', SYSTIME(1)-t0
  
  n_bufs = N_ELEMENTS((*hr_data.spectra)[0, 0,*, 0])
  n_writes = N_ELEMENTS((*hr_data.spectra)[0, 0, 0, *])
  ns_per_bin = 0.6255                                             ; approx. ns per bin. DETECTOR-SPECIFIC!


  FOR i=0, N_ELEMENTS(targets)-1 DO BEGIN                         ; loop: file n, over all targeted m/Q

    t1=SYSTIME(1)
    target_mq = targets[i]
    ix_pt = WHERE(peaktable.mass EQ target_mq)
    
    IF KEYWORD_SET(use_pt_limits) THEN $                         ; keyword: use border stored in peaktable of file
      limits_acc = [target_mq-((peaktable.lower_integration_limit)[ix_pt]), $
                     ((peaktable.upper_integration_limit)[ix_pt])-target_mq]                                                                                   

    w_nom = WHERE(*hr_data.massaxis GE target_mq-limits_nom[0] AND *hr_data.massaxis LE target_mq+limits_nom[1])
    n_wn = N_ELEMENTS(w_nom)
     
    x_ns = w_nom*ns_per_bin ; x-axis in nanoseconds
    x_ns_hd = REBIN(DOUBLE(x_ns), n_wn*f_hd)
    
    x_nom = (*hr_data.massaxis)[w_nom] ; m/q x-axis
    x_nom_hd = REBIN(DOUBLE(x_nom), n_wn*f_hd) 

    y_nom = REFORM((*hr_data.spectra)[w_nom, 0, *, *], n_wn, n_bufs*n_writes)
    y_nom_hd = DBLARR(n_wn*f_hd, n_bufs*n_writes)

    IF VERBOSE THEN print, 'reprocessing intensity data of m/Q ', i, ' of', N_ELEMENTS(targets)
    
    FOR j=0, n_bufs*n_writes-1 DO y_nom_hd[*, j]=REBIN(DOUBLE(y_nom[*,j]), n_wn*f_hd)

    w_target = WHERE(x_nom_hd GE target_mq-limits_acc[0] AND x_nom_hd LE target_mq+limits_acc[1])
    n_wt = N_ELEMENTS(w_target)
    x = x_ns_hd[w_target]
    y = y_nom_hd[w_target,*]/hr_data.nbrwaveforms ; get mean signal per spectrum in [mV]
    
    IF VERBOSE THEN print, 'recreating masstrace...'
    
    target_mq_trace = DBLARR(n_writes*n_bufs)
    
    IF KEYWORD_SET(integrate) THEN $ ; calculate new intensity values / masstrace data
      FOR k=0, n_bufs*n_writes-1 DO target_mq_trace[k] = INT_TABULATED(x, y[*,k], /DOUBLE)/hr_data.SiS $
        ELSE $
          FOR k=0, n_bufs*n_writes-1 DO target_mq_trace[k] = TOTAL(y[*,k], /DOUBLE)/hr_data.SiS
          
    ix_intensity = WHERE((mass) EQ target_mq) ; find indices of intensities of current target m/Q
    ; coerce index range; sometimes intensity values are missing and array length is LT n_writes*n_bufs
    IF N_ELEMENTS(ix_intensity) LT N_ELEMENTS(target_mq_trace) THEN BEGIN
      ix_intensity = ix_intensity[0:N_ELEMENTS(ix_intensity)-1]
      target_mq_trace = target_mq_trace[0:N_ELEMENTS(ix_intensity)-1]
    ENDIF
    
    intensity[ix_intensity]=target_mq_trace                                                                                  ; apply new intensity values to input array

    t2=SYSTIME(1)
    IF VERBOSE THEN print, 'reprocessed in [s]: ', t2-t1
    
    FreeVar, x_nom_hd                                                                                                        ; clean memory
    FreeVar, y_nom_hd
    FreeVar, x
    FreeVar, y
    
  ENDFOR    

  FreeVar, hr_data
  
  IF VERBOSE THEN print, 'file done. t: ', SYSTIME(1)-t0

  RETURN, intensity
  
END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------