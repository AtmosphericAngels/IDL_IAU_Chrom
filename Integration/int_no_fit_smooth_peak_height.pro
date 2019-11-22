;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; int_no_fit_smooth_peak_height
;
; MODIFICATION HISTORY:
; T. Wagenhäuser, November 2019: created from "int_baseline_gau"
; F. Obersteiner, June 2017: modified so that different baseline fits are available.
;                            changed number of data points used for baseline fitting from 18 to 12.
; F. Obersteiner, June 2014: implemented in IAU_chrom 4.8.
; H. Boenisch, August, 2012: created.
;-
;------------------------------------------------------------------------------------------------------------------------
@peak_detection
;------------------------------------------------------------------------------------------------------------------------

FUNCTION int_peak_height_no_fit_SG , xval, yval $
                          , NTERMS_BASE=nterms_base, NSIGMA_INT=nsigma_int $
                          , RT_WIN=rt_win, PEAK_RET=peak_ret, BASE_RET=base_ret  $
                          , INT_WIN=int_win, PEAK_INT=peak_int, BASE_INT=base_int  $
                          , PARAMETER=parameter, TAXIS=taxis, VERBOSE=verbose, CHK_NOISE=chk_noise

  IF NOT keyword_set(NTERMS_BASE) THEN nterms_base=1 ; linear baseline by default
  IF NOT keyword_set(NSIGMA_INT) THEN nsigma_int=[3,30]
  IF NOT keyword_set(RT_WIN) THEN rt_win=[min(xval,/nan),max(xval,/nan)]
  IF NOT KEYWORD_SET(chk_noise) THEN chk_noise = 0.
;  nterms = nterms_base+3

  ; Create output structure (strct) for chromatographic parameters
  ;+++++++++++++++++++++++
  strct=$
    {rt: !values.d_nan, $
    hght: !values.d_nan, $
    area: !values.d_nan, $
    wdth: !values.d_nan, $
    ts:   !values.d_nan, $
    te:   !values.d_nan, $
    flag: 0,             $
    comment:'Not Integrated'}

  vd=where(finite(xval+yval),nvd)
  IF (nvd LE 0) THEN RETURN,strct
  x=xval[vd]
  y=yval[vd]

  ; Define retention time window (RT_WIN)
  ;+++++++++++++++++++++++
  w_rt_win=where((x GE rt_win[0]) AND (x LE rt_win[1]),nw_rt_win)

  ; Detect Gauss Peak inside retention time window (RT_WIN)
  ;+++++++++++++++++++++++
  t=x[w_rt_win]
  v=y[w_rt_win]

  A_gau=peak_detection(t,v,RT_WIN=rt_win,NTERMS=nterms,PEAK=peak_ret,BASE=base_ret)

  IF finite(A_gau[1]) EQ 0 THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    RETURN, strct
  ENDIF

  ; Define window for peak integration (INT_WIN)
  ;+++++++++++++++++++++++
  int_win=[A_gau[1]-nsigma_int[0]*A_gau[2],A_gau[1]+nsigma_int[1]*A_gau[2]]
  w_int_win=where((x GE int_win[0]) AND (x LE int_win[1]), nw_int_win)

  IF (nw_int_win LE n_elements(A)) OR (int_win[0] LT 0D) THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    RETURN, strct
  ENDIF

  ; Output fitted peak parameters (PARAMETER)
  ;+++++++++++++++++++++++
  parameter=A_gau

  ; Calculate integrated peak (PEAK_INT) and baseline (BASE_INT)
  ;+++++++++++++++++++++++
  t=x[w_int_win]
  v=y[w_int_win]

  taxis=x[w_int_win]
  IF N_ELEMENTS(t) LT 12 THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    RETURN, strct
  ENDIF

  nidx=6 ; use n data points left and right of signal to fit baseline
  ts=x[w_int_win[0]+(indgen(nidx)-nidx/2)]
  te=x[w_int_win[-1]+(indgen(nidx)-nidx/2)]
  vs=y[w_int_win[0]+(indgen(nidx)-nidx/2)]
  ve=y[w_int_win[-1]+(indgen(nidx)-nidx/2)]

  IF (nterms_base GT 1) THEN A=poly_fit([ts,te],[vs,ve],nterms_base-1)

  CASE nterms_base OF
    1: base_int=mean([vs,ve])+REPLICATE(0,nw_int_win)
    2: base_int=A[0]+A[1]*t
    3: base_int=A[0]+A[1]*t+A[2]*t^2
  ENDCASE

  peak_int=v-base_int

  IF (MAX(peak_int,wmax) LT 1.5*chk_noise) THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Fit height less than 1.5 x Noiselevel', /INFORMATION)
    RETURN, strct
  ENDIF

  area=int_tabulated(t, peak_int, /DOUBLE)
  IF (area LT 0.) THEN BEGIN ;OR (A[0] LT chk_noise)
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Negative fit', /INFORMATION)
    RETURN, strct
  ENDIF

  ;+++++++++++++++++++++++
  ; Calculate chromatographic parameters (peak area, height and retention time)
  ;+++++++++++++++++++++++
  strct.hght=MAX(peak_int,wmax)
  strct.rt=t[wmax]
  strct.area=area
  strct.wdth=A_gau[2]
  strct.ts=mean(ts,/nan)
  strct.te=mean(te,/nan)
  strct.flag=1
  strct.comment='Integrated'

  IF verbose THEN BEGIN
    print,'BASELINE FIT PARAMS:'
    print,A_gau
    print,ts,format='('+strtrim(2*n_elements(ts),2)+'A)'
    print,te,format='('+strtrim(2*n_elements(ts),2)+'A)'
    print,vs,format='('+strtrim(2*n_elements(ts),2)+'A)'
    print,ve,format='('+strtrim(2*n_elements(ts),2)+'A)'
    print,'height / tmax / gauss_tmax'
    print,strct.hght,t[wmax],A_gau[1]
    print,strct.area,int_tabulated(x[w_rt_win],peak_ret,/double)
    print,''
    t=x[w_rt_win]
    v=y[w_rt_win]
    yrange=[min([v,base_ret,base_int]),max([v,base_ret,base_int])]
    plot,t,v,yrange=yrange,ystyle=3
    oplot,t,peak_ret,linestyle=1,thick=1
    oplot,[mean(ts),mean(te)],[mean(vs),mean(ve)],psym=1,symsize=3
    t=x[w_int_win]
    v=y[w_int_win]
    oplot,t,base_int,linestyle=0,thick=2
  ENDIF

  RETURN, strct

END
