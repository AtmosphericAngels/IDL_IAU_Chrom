;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION int_gau_SGbase
;
; created by: Thomas Wagenhäuser from int_gau.pro 2019-12-02
; last modified:
;-
;------------------------------------------------------------------------------------------------------------------------
@peak_detection
;------------------------------------------------------------------------------------------------------------------------
FUNCTION int_gau_SGbase, xval, yval, NSIGMA_FIT=nsigma_fit, NSIGMA_INT=nsigma_int, NTERMS_BASE=nterms_base, RT_WIN=rt_win, $
                  INT_WIN=int_win, FIT_WIN=fit_win,  PEAK_RET=peak_ret, BASE_RET=base_ret,  PEAK_FIT=peak_fit, $
                  BASE_FIT=base_fit, PEAK_INT=peak_int, BASE_INT=base_int, PARAMETER=parameter, TAXIS=taxis, $
                  VERBOSE=verbose, CHK_NOISE=chk_noise, TIMESCALE=timescale, RTWINisFITWIN=rtwinisfitwin
;t0=systime(1)

  IF NOT keyword_set(nsigma_fit)  THEN nsigma_fit=[15,15]
  IF NOT keyword_set(nsigma_int)  THEN nsigma_int=[15,15]
  IF NOT keyword_set(nterms_base)THEN nterms_base=1 ; default linear baseline
  IF NOT keyword_set(rt_win) THEN rt_win =[min(xval,/nan), max(xval,/nan)]
  IF NOT keyword_set(int_win) THEN int_win=[min(xval,/nan), max(xval,/nan)]
  IF NOT keyword_set(chk_noise) THEN chk_noise = 0.
  IF NOT keyword_set(timescale) THEN timescale='Minutes' ; not set case: restoring old chromatogram
  IF timescale EQ 'Minutes' THEN timescale = 60. ELSE timescale = 1. ; 60: minutes / 1: seconds
  nterms = nterms_base + 3 ; nterms paramenter for gaussfit function
  ; datapoints per second? -> gauss min sigma


   strct=$
      {$
      rt: !VALUES.D_NAN, $
      hght: !VALUES.D_NAN, $
      area: !VALUES.D_NAN, $
      wdth: !VALUES.D_NAN, $
      ts: !VALUES.D_NAN, $
      te: !VALUES.D_NAN, $
      flag: 0 ,$
      comment:'Not Integrated'$
      }

    vd=where(finite(xval+yval),nvd)
    IF (nvd LE 0) THEN RETURN, strct
    x=xval[vd]
    y=yval[vd]

  IF N_ELEMENTS(x) LE nterms_base THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Not enough datapoints for peak detection.', /INFORMATION)
    RETURN, strct
  ENDIF

  A=FLTARR(nterms)*!VALUES.D_NAN
  A_temp=peak_detection(x,y, RT_WIN=rt_win, NTERMS_BASE=nterms_base, VERBOSE=verbose) ; NTERMS_BASE=bl_type)

  A[0:N_ELEMENTS(A_temp)-1]=A_temp ; coerce array range if fixed parameters used in peak_detection

  IF FINITE(A[1]) EQ 0 THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('No peak found.', /INFORMATION)
    RETURN, strct
  ENDIF

  ; window with data for gaussfit sigma as defined in info file
  w_rt_win=WHERE((x GE rt_win[0]) AND (x LE rt_win[1]), nw_rt_win)
  IF KEYWORD_SET(rtwinisfitwin) THEN BEGIN
    w_fit_win=w_rt_win
    nw_fit_win=nw_rt_win
    w_minfit_win=w_rt_win
    nw_minfit_win=nw_rt_win
  ENDIF ELSE BEGIN
    w_fit_win=WHERE((x GE (A[1]-nsigma_fit[0]*A[2])) AND (x LE(A[1]+nsigma_fit[1]*A[2])), nw_fit_win)

; this small feature could be integrated in the SavGol baseline part of the widget:
    nsigma_minfit=nsigma_fit ;[5.,5.] ; only get local SGbase minimum in case of high nsigma_fit window!
    if nsigma_fit[0] lt nsigma_minfit[0] then nsigma_minfit[0] = nsigma_fit[0]
    if nsigma_fit[1] lt nsigma_minfit[1] then nsigma_minfit[1] = nsigma_fit[1]
;
; otherwise throw out nsigma_minfit
    
    w_minfit_win=WHERE((x GE (A[1]-nsigma_minfit[0]*A[2])) AND (x LE(A[1]+nsigma_minfit[1]*A[2])), nw_minfit_win)
  ENDELSE

  IF nw_fit_win LE N_ELEMENTS(A) THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Not enough datapoints for peak fit.', /INFORMATION)
    RETURN, strct
  ENDIF


; apply Savitzky-Gulay-filter to y
  nleft = 3 ;provide these in GUI in future versions
  nright = nleft ;keep both variables in case of future needs
  sg_degree = 3 ;polynomial used for smoothing, provide in future versions

  sg_filter=savgol(nleft,nright,0,sg_degree,/double) ;get SG-parameters
  y_SG=convol(y,sg_filter,/EDGE_WRAP) ;apply SG-filter
  v_SG=y_SG[w_minfit_win] ;get values in fit window from full data

  ;get min and max value for Peak height
  Peak_top = max(y[w_minfit_win], w_rt_raw_t) ;max from raw data; save index: w_rt_raw_t
  IF w_rt_raw_t EQ 0 THEN BEGIN ;in case 'Peak_top' is the very first datapoint
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Not enough datapoints for peak detection.', /INFORMATION)
    RETURN, strct
  ENDIF
  Peak_min_l = min(v_SG[0 : (w_rt_raw_t-1)], w_min_l) ;left min from Savitzky-Gulay
  Peak_min_r = min(v_SG[w_rt_raw_t : -1], w_min_r) ;right min from Savitzky-Gulay
  w_min_r = w_min_r + w_rt_raw_t ;to get the right index!!
  Peak_min = min([Peak_min_l, Peak_min_r], min_sel) ;choose lower value

  w_base_ind = [abs(w_min_l + w_minfit_win[0]), abs(w_min_r + w_minfit_win[0])] ;confusing nomenclature: w_min_l and w_min_r are from w_minfit_win
  if w_base_ind[0] eq 0 then BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Index error.', /INFORMATION)
    RETURN, strct
  endif

  y_SGbase = y


  ;*********** min baseline ************
    ts=x[w_base_ind[0]]
    te=x[w_base_ind[1]]
    vs=Peak_min_l
    ve=Peak_min_r


    IF (nterms_base GT 1) THEN A_base=poly_fit([ts,te],[vs,ve],nterms_base-1, /DOUBLE) ;works with both: mean baseline and with min baseline

    IF (nterms_base GT 2) THEN BEGIN
      strct.flag=0;
      strct.comment='Not Integrated'
      RETURN, strct
    ENDIF

;  t=x[w_fit_win]

;substract baseline from raw data (yields y_SGbase)
  CASE nterms_base OF
    1: y_SGbase -= Peak_min ;subtract baseline from data
    2: BEGIN $
      base_int=A_base[0]+A_base[1]*x
      y_SGbase -= base_int ;subtract baseline from data
      END
    3: BEGIN $
      strct.flag=0;
      strct.comment='not integrated'
      IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('only use constant or linear baseline', /INFORMATION)
      RETURN, strct
      END
  ENDCASE

;+++++++++++++++++++++++
; actual peak fit... w_int_win: time axis position within +/- n_sigma_int of peak fit
;+++++++++++++++++++++++

  fit=gaussfit(x[w_fit_win],y_SGbase[w_fit_win], A, NTERMS=3, ESTIMATES=A[0:2], YERROR=v_err) ;baseline has been substracted (see above) -> NTERMS=3
  w_int_win=WHERE((x GE (A[1]-nsigma_int[0]*A[2])) AND (x LE(A[1]+nsigma_int[1]*A[2])), nw_int_win)

;+++++++++++++++++++++++
; Check gaussfit parameters A[*]
;+++++++++++++++++++++++
  IF nw_int_win LE N_ELEMENTS(A) THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Not enough datapoints for integration', /INFORMATION)
    RETURN, strct
  ENDIF

  IF (A[0] LT 1.5*chk_noise) THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Fit height less than 1.5 x Noiselevel', /INFORMATION)
    RETURN, strct
  ENDIF

  IF A[1] LT rt_win[0] OR A[1] GT rt_win[1] THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Fitted peak out of RT window', /INFORMATION)
    RETURN, strct
  ENDIF

  ; enforce a minimum number of datapoints per peak: 5 points within +- 2.5 sigma
  n_dpps=((rt_win[1]-rt_win[0])*timescale)/N_ELEMENTS(w_rt_win) ; datapoints per second
  min_sig=((n_dpps*5.)/5.)/timescale ; equals 5 datapoints in 5 sigma
  IF (A[2] LT min_sig) THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Low standard deviation: too few datapoints', /INFORMATION)
    RETURN, strct
  ENDIF

  taxis=x[w_int_win]
  int_win=[x[w_int_win[0]],x[w_int_win[-1]]]
  fit_win=[x[w_fit_win[0]],x[w_fit_win[-1]]]

  parameter=A

  ;+++++++++++++++++++++++
  ; Calculate fitted peak (PEAK_FIT) and baseline (BASE_FIT)
  ;+++++++++++++++++++++++
  t=x[w_fit_win]
  v=y[w_fit_win]
  z=(x[w_fit_win]-A[1])/A[2]
  peak_fit=A[0]*exp(-z^2/2)
  CASE nterms_base OF
    1: base_fit=Peak_min+replicate(0,nw_fit_win) ;was A[3] in prior versions
    2: base_fit=A_base[0]+A_base[1]*t ;was A[3] and a[4] in prior versions
  ENDCASE

  ;+++++++++++++++++++++++
  ; Calculate integrated peak (PEAK_INT) and baseline (BASE_INT)
  ;+++++++++++++++++++++++
  t=x[w_int_win]
  v=y[w_int_win]
  z=(x[w_int_win]-A[1])/A[2]
  peak_int=A[0]*exp(-z^2/2)
  CASE nterms_base OF
    1: base_int=Peak_min+replicate(0,nw_int_win)
    2: base_int=A_base[0]+A_base[1]*t
  ENDCASE

  ;+++++++++++++++++++++++
  ; Calculate peak inside retention time window (PEAK_RET) and baseline (BASE_RET)
  ;+++++++++++++++++++++++
  t=x[w_rt_win]
  v=y[w_rt_win]
  z=(x[w_rt_win]-A[1])/A[2]
  peak_ret=A[0]*exp(-z^2/2)
  CASE nterms_base OF
    1: base_ret=Peak_min+replicate(0,nw_rt_win)
    2: base_ret=A_base[0]+A_base[1]*t
  ENDCASE

  area=int_tabulated(x[w_int_win],peak_int,/double)
  IF (area LT 0.) THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Negative fit area', /INFORMATION)
    RETURN, strct
  ENDIF

  strct.rt=A[1]
  strct.hght=A[0]
  strct.area=area
  strct.ts=min(x[w_int_win],/nan)
  strct.te=max(x[w_int_win],/nan)
  strct.wdth=A[2]
  strct.flag=1
  strct.comment='Integrated'

  IF KEYWORD_SET(verbose) THEN BEGIN
    PRINT, 'n datapoints used for fit: ',  N_ELEMENTS(w_fit_win)
    PRINT, 'integrated n fitted datapoints: ', N_ELEMENTS(w_int_win)
    p_int_gau  = plot(t, v, TITLE='Int_Gau')
    p0_int_gau = plot(t, peak_int, 'r', /OVERPLOT)
  ENDIF

  RETURN, strct

END
