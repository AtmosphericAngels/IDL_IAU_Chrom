;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; 170531, F. Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; Rudimentary integration method. uses RT min and RT max to interpolate a line between two data points as baseline
; and integrate any value above/below this baseline.
; Will return 'no peak found' if noise level is higher than 1.5 times the height or area is a negative value.
;
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION int_bl_fixpoints , xval, yval $
                          , NTERMS_BASE=nterms_base, NSIGMA_INT=nsigma_int $
                          , RT_WIN=rt_win, PEAK_RET=peak_ret, BASE_RET=base_ret  $
                          , INT_WIN=int_win, PEAK_INT=peak_int, BASE_INT=base_int  $
                          , PARAMETER=parameter, TAXIS=taxis, VERBOSE=verbose, CHK_NOISE=chk_noise

  IF NOT KEYWORD_SET(RT_WIN) THEN rt_win=[MIN(xval,/nan),MAX(xval,/nan)]
  IF NOT KEYWORD_SET(chk_noise) THEN chk_noise = 0.
  IF NOT KEYWORD_SET(nterms_base) THEN nterms_base = 2 ; default linear baseline
  
  int_win=rt_win
  
  strct = { $
            rt: !VALUES.D_NAN, $
            hght: !VALUES.D_NAN, $
            area: !VALUES.D_NAN, $
            wdth: !VALUES.D_NAN, $
            ts:   !VALUES.D_NAN, $
            te:   !VALUES.D_NAN, $
            flag: 0,             $
            comment:'Not Integrated' $
            }
  
  vd=WHERE(FINITE(xval+yval),nvd)
  IF (nvd LE 0) THEN RETURN, strct
  
  x=xval[vd]
  y=yval[vd]
  
  w_rt_win=WHERE((x GE rt_win[0]) AND (x LE rt_win[1]),nw_rt_win)
  
  taxis=x[w_rt_win]
  
  v=y[w_rt_win]
  v_s = [v[0]]
  v_e = [v[-1]]
  
  base_int = DBLARR(nw_rt_win)
  
  CASE nterms_base OF
    1: BEGIN 
         dv = 0D
         FOR i=0, nw_rt_win-1 DO base_int[i]=MIN([v_s, v_e])+i*dv
       END
    ELSE: BEGIN
            dv = (v_e-v_s)/nw_rt_win
            FOR i=0, nw_rt_win-1 DO base_int[i]=v_s+i*dv
          END
  ENDCASE
  
  
  
  peak_int=v
  mx=MAX(v, ix_max)
  
;  area = TOTAL(peak_int-base_int)
  area=int_tabulated(taxis, peak_int-base_int, /DOUBLE)
  height = peak_int[ix_max]-base_int[ix_max]
  
  IF (height LT 1.5*chk_noise) OR (area LT 0.) THEN BEGIN
    strct.flag=-1;
    strct.comment='No Peak Found'
    IF KEYWORD_SET(verbose) THEN msg=DIALOG_MESSAGE('Fit height less than 1.5 x Noiselevel', /INFORMATION)
    RETURN, strct
  ENDIF
  
  hm_base = base_int+0.5*height ; baseline at half peak height
  
  IF ix_max+1 GE N_ELEMENTS(v) THEN BEGIN
    min_r = (v-hm_base)[-1]
    t_r = taxis[-1]
  ENDIF ELSE BEGIN
    min_r = MIN((v-hm_base)[ix_max+1:-1], ix_min_r, /ABSOLUT)
    t_r = (taxis[ix_max+1:-1])[ix_min_r]
  ENDELSE
  
  min_l = MIN((v-hm_base)[0:ix_max-1], ix_min_l, /ABSOLUT)
  t_l = (taxis[0:ix_max-1])[ix_min_l]
  
  width = t_r-t_l
  
  strct.hght=height
  strct.rt=taxis[ix_max]
  strct.area=area
  strct.wdth=width
  strct.ts=taxis[0]
  strct.te=taxis[-1]
  strct.flag=1
  strct.comment='Integrated'
  
  RETURN, strct
  
END