;------------------------------------------------------------------------------------------------------------------------
;+
; by H.Boenisch
;
; PURPOSE:
; use IDL gaussfit function to detect a peak in a specified RT window.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION peak_detection, xval, yval, RT_WIN=rt_win, NTERMS_BASE=nterms_base, PEAK=peak, BASE=base, VERBOSE=verbose

  IF NOT KEYWORD_SET(rt_win) THEN rt_win=[MIN(xval,/nan),MAX(xval,/nan)]
  IF NOT KEYWORD_SET(nterms_base) THEN nterms_base_peakdet=1 ELSE nterms_base_peakdet=nterms_base; linear baseline by default

  nterms_peakdet = 3+nterms_base_peakdet

  A = FLTARR(nterms_peakdet)*!VALUES.D_NAN

  vd = WHERE(FINITE(xval+yval),nvd)
  IF (nvd LE 0) THEN RETURN,A
  x = xval[vd]
  y = yval[vd]

  w_ret_win = WHERE(x GE rt_win[0] AND x LE rt_win[1],nw_ret_win)
  t = x[w_ret_win]
  v = y[w_ret_win]

;/*******************************TW 2019.08.08.
  Est4 = [$
    max(v,max_ind),$
    t[max_ind],$
    0,$
    min(v)$
    ]

  CASE nterms_peakdet of
    4: Est = Est4
    5: Est = [Est4,0]
    6: Est = [Est4,0,0]
  ENDCASE
  ;TW 2019.08.08. *******************************/


  IF (nw_ret_win LE nterms_peakdet) THEN RETURN, A   ; not enought datapoints in rt window

  IF (nw_ret_win GT nterms_peakdet) THEN BEGIN
    width = -999
    IF nterms_peakdet NE 4 THEN BEGIN ; if non-constant baseline selected:
      v_fit1 = gaussfit(t,v,A0,NTERMS=4,Estimates=Est4) ; first fit, constant baseline, to get more reliable estimate for peak width  ;TW 2019.08.08. *******************************/
      width = A0[2]
    ENDIF
    v_fit1 = gaussfit(t,v,A,NTERMS=nterms_peakdet,Estimates=Est)  ;TW 2019.08.08. *******************************/
    IF width NE -999 THEN A[2]=width
  ENDIF


  IF KEYWORD_SET(verbose) THEN BEGIN

    ;A0 is undefined if constant baseline is selected
    z0=(t-A0[1])/A0[2]
    peak0=A0[0]*EXP(-z0^2/2)
    base0=A0[3]+REPLICATE(0,nw_ret_win)

    z=(t-A[1])/A[2]
    peak=A[0]*EXP(-z^2/2)
    CASE nterms_base_peakdet OF
      1: base=A[3]+REPLICATE(0,nw_ret_win)
      2: base=A[3]+A[4]*t
      3: base=A[3]+A[4]*t+A[5]*t^2
    ENDCASE

    PRINT, 'nterms_base, peakdetection: ', nterms_base_peakdet
    PRINT, 'nterms, peakdetection: ', nterms_peakdet
    PRINT, 'gaussfit coefficients: ', A
    p_pdet  = plot(t, v, TITLE='Peak_Detection', XRANGE=rt_win, NAME='Data')
    p0_pdet = plot(t, peak0+base0, 'r', /OVERPLOT, NAME='fit with bl setting')
    p1_pdet = plot(t, peak+base, 'b', /OVERPLOT, NAME='fit with constant bl')
    l = legend(target=[p_pdet, p0_pdet, p1_pdet], /DATA, SHADOW=0, $ ;, p1
               /AUTO_TEXT_COLOR, SAMPLE_WIDTH=0.0005, $
               HORIZONTAL_Spacing=0.07, VERTICAL_SPACING=0.04)
    l.POSITION=[0.9825,0.9]

  ENDIF

  RETURN, A

END