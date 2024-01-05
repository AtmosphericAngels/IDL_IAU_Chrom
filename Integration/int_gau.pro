;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION int_gau
;
; created by: H. Boenisch
; modified: F. Obersteiner, Apr 2015.
; last modified T. Schuck, May 2023
;   added upscaling of data for numeric integration
;-
;------------------------------------------------------------------------------------------------------------------------
@peak_detection
;------------------------------------------------------------------------------------------------------------------------
FUNCTION int_gau, xval, yval, NSIGMA_FIT=nsigma_fit, NSIGMA_INT=nsigma_int, NTERMS_BASE=nterms_base, RT_WIN=rt_win, $
                  INT_WIN=int_win, FIT_WIN=fit_win,  PEAK_RET=peak_ret, BASE_RET=base_ret,  PEAK_FIT=peak_fit, $
                  BASE_FIT=base_fit, PEAK_INT=peak_int, BASE_INT=base_int, PARAMETER=parameter, TAXIS=taxis, $
                  VERBOSE=verbose, CHK_NOISE=chk_noise, TIMESCALE=timescale, RTWINisFITWIN=rtwinisfitwin, upsample=upsample
;t0 = systime(1)

  IF NOT keyword_set(nsigma_fit)  THEN nsigma_fit=[15,15]
  IF NOT keyword_set(nsigma_int)  THEN nsigma_int=[15,15]
  IF NOT keyword_set(nterms_base)THEN nterms_base = 1 ; default linear baseline
  IF NOT keyword_set(rt_win) THEN rt_win =[min(xval,/nan), max(xval,/nan)]
  IF NOT keyword_set(int_win) THEN int_win=[min(xval,/nan), max(xval,/nan)]
  IF NOT keyword_set(chk_noise) THEN chk_noise = 0.
  IF NOT keyword_set(timescale) THEN timescale = 'Minutes' ; not set case: restoring old chromatogram
  IF timescale EQ 'Minutes' THEN timescale = 60. ELSE timescale = 1. ; 60: minutes / 1: seconds
  nterms = nterms_base + 3 ; nterms paramenter for gaussfit function
  ; datapoints per second? -> gauss min sigma
  IF NOT keyword_set(upscale) THEN upscale = 0.


   strct = $
      {$
      rt: !VALUES.D_NAN, $
      hght: !VALUES.D_NAN, $
      area: !VALUES.D_NAN, $
      wdth: !VALUES.D_NAN, $
      ts: !VALUES.D_NAN, $
      te: !VALUES.D_NAN, $
      upscale: 1 ,$
      t_int_axis: PTR_NEW(/ALLOCATE_HEAP), $
      flag: 0 ,$
      comment:'Not Integrated'$
      }

    vd=where(finite(xval+yval),nvd)
    IF (nvd LE 0) THEN RETURN, strct
    x = xval[vd]
    y = yval[vd]

  IF N_ELEMENTS(x) LE nterms_base THEN BEGIN
    strct.flag = -1;
    strct.comment = 'No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Not enough datapoints for peak detection.', /INFORMATION)
    RETURN, strct
  ENDIF

  A = FLTARR(nterms)*!VALUES.D_NAN
  A_temp=peak_detection(x,y, RT_WIN=rt_win, NTERMS_BASE=nterms_base, VERBOSE=verbose) ; NTERMS_BASE=bl_type)

  A[0:N_ELEMENTS(A_temp)-1] = A_temp ; coerce array range if fixed parameters used in peak_detection

  IF FINITE(A[1]) EQ 0 THEN BEGIN
    strct.flag = -1;
    strct.comment = 'No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('No peak found.', /INFORMATION)
    RETURN, strct
  ENDIF

  ; window with data for gaussfit sigma as defined in info file
  w_rt_win=WHERE((x GE rt_win[0]) AND (x LE rt_win[1]), nw_rt_win)
  IF KEYWORD_SET(rtwinisfitwin) THEN BEGIN
    w_fit_win = w_rt_win
    nw_fit_win = nw_rt_win
  ENDIF ELSE $
    w_fit_win=WHERE((x GE (A[1]-nsigma_fit[0]*A[2])) AND (x LE(A[1]+nsigma_fit[1]*A[2])), nw_fit_win)

  IF nw_fit_win LE N_ELEMENTS(A) THEN BEGIN
    strct.flag = -1;
    strct.comment = 'No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Not enough datapoints for peak fit.', /INFORMATION)
    RETURN, strct
  ENDIF

;+++++++++++++++++++++++
; actual peak fit... w_int_win: time axis position within +/- n_sigma_int of peak fit
;+++++++++++++++++++++++

  fit=gaussfit(x[w_fit_win],y[w_fit_win], A, NTERMS=nterms, ESTIMATES=A, YERROR=v_err)
  w_int_win=WHERE((x GE (A[1]-nsigma_int[0]*A[2])) AND (x LE(A[1]+nsigma_int[1]*A[2])), nw_int_win)

;+++++++++++++++++++++++
; Check gaussfit parameters A[*]
;+++++++++++++++++++++++
  IF nw_int_win LE N_ELEMENTS(A) THEN BEGIN
    strct.flag = -1;
    strct.comment = 'No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Not enough datapoints for integration', /INFORMATION)
    RETURN, strct
  ENDIF

  IF (A[0] LT 1.5*chk_noise) THEN BEGIN
    strct.flag = -1;
    strct.comment = 'No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Fit height less than 1.5 x Noiselevel', /INFORMATION)
    RETURN, strct
  ENDIF

  IF A[1] LT rt_win[0] OR A[1] GT rt_win[1] THEN BEGIN
    strct.flag = -1;
    strct.comment = 'No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Fitted peak out of RT window', /INFORMATION)
    RETURN, strct
  ENDIF

  ; enforce a minimum number of datapoints per peak: 5 points within +- 2.5 sigma
  n_dpps = ((rt_win[1]-rt_win[0])*timescale)/N_ELEMENTS(w_rt_win) ; datapoints per second
  min_sig = ((n_dpps*5.)/5.)/timescale ; equals 5 datapoints in 5 sigma
  IF (A[2] LT min_sig) THEN BEGIN
    strct.flag = -1;
    strct.comment = 'No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Low standard deviation: too few datapoints', /INFORMATION)
    RETURN, strct
  ENDIF

  taxis = x[w_int_win]
  int_win=[x[w_int_win[0]],x[w_int_win[-1]]]
  fit_win=[x[w_fit_win[0]],x[w_fit_win[-1]]]

  parameter = A

  ;+++++++++++++++++++++++
  ; Calculate peak inside retention time window (PEAK_RET) and baseline (BASE_RET)
  ;+++++++++++++++++++++++
  t = x[w_rt_win]
  z = (x[w_rt_win]-A[1])/A[2]
  peak_ret = A[0]*exp(-z^2/2)
  CASE nterms_base OF
    1: base_ret=A[3]+replicate(0,nw_rt_win)
    2: base_ret = A[3]+A[4]*t
    3: base_ret = A[3]+A[4]*t+A[5]*t^2
  ENDCASE

  ;+++++++++++++++++++++++
  ; Calculate fitted peak (PEAK_FIT) and baseline (BASE_FIT)
  ;+++++++++++++++++++++++
  t = x[w_fit_win]
  z = (x[w_fit_win]-A[1])/A[2]
  peak_fit = A[0]*exp(-z^2/2)
  CASE nterms_base OF
    1: base_fit=A[3]+replicate(0,nw_fit_win)
    2: base_fit = A[3]+A[4]*t
    3: base_fit = A[3]+A[4]*t+A[5]*t^2
  ENDCASE

  ;+++++++++++++++++++++++
  ; Calculate integrated peak (PEAK_INT) and baseline (BASE_INT)
  ;+++++++++++++++++++++++

        ;+++++++++++++++++++++++
        ;  check cycle number and number of points used for integration
        ;+++++++++++++++++++++++
      
        new_int_x = 0
        scale = 1.
        if upsample EQ 1. THEN BEGIN
          chrom_per_sec = N_ELEMENTS(w_int_win)/((x[w_int_win[-1]]-x[w_int_win[0]])*timescale)
      
          ; check number of datapoints (cycles) per time interval - should not be less than 15 per second
          IF chrom_per_sec LT 15. THEN BEGIN
            IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Low number of cycles per seconds found. Points added for numeric integration.', /INFORMATION)
            scale=FLOOR(15./chrom_per_sec)
            new_int_x=1
          ENDIF
        ENDIF
        ; print, 'scale factor:', scale
  IF new_int_x EQ 1 THEN BEGIN  ; add more points to calculate integral
    t_tmp = x[w_int_win]
    t_int_scal=INTERPOLATE(t_tmp, (N_ELEMENTS(t_tmp)-1)/(nw_int_win*scale-1.) * FINDGEN(nw_int_win*scale))   ; add more points to calculate integral
    t = t_int_scal
  ENDIF ELSE t = x[w_int_win]        

  z = (t-A[1])/A[2]
  peak_int = A[0]*exp(-z^2/2)

  CASE nterms_base OF
    1: base_int = A[3]+replicate(0,nw_int_win*scale)
    2: base_int = A[3]+A[4]*t
    3: base_int = A[3]+A[4]*t+A[5]*t^2
  ENDCASE

  area=int_tabulated(t,peak_int,/double)
  IF (area LT 0.) THEN BEGIN
    strct.flag = -1;
    strct.comment = 'No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Negative fit area', /INFORMATION)
    RETURN, strct
  ENDIF

  strct.rt = A[1]
  strct.hght = A[0]
  strct.area = area
  strct.ts=min(t,/nan)
  strct.te=max(t,/nan)
  strct.wdth = A[2]
  strct.upscale=scale
  strct.t_int_axis = ptr_new(t)
  strct.flag = 1
  strct.comment = 'Integrated'

  IF KEYWORD_SET(verbose) THEN BEGIN
    PRINT, 'n datapoints used for fit: ',  N_ELEMENTS(w_fit_win)
    PRINT, 'integrated n fitted datapoints: ', N_ELEMENTS(w_int_win)
    p_int_gau  = plot(t, v, TITLE='Int_Gau')
    p0_int_gau = plot(t, peak_int, 'r', /OVERPLOT)
  ENDIF

  RETURN, strct

END