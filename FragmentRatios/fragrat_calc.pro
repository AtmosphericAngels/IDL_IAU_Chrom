;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; fragrat_calc
;
; AUTHOR:
; F.Obersteiner, Dec 2013 , Jun 2014, Feb 2015
;
; INFO:
; calculate baseline peak fits for both selected masses
; use baseline-substracted peakdata from FCT int_baseline_gau and
; compare intensity values within selected sigma window.
; Feb 2015 added gaussfit and fit comparison.
; Mar 2017 added dt_correct for scanning type MS
;-
;------------------------------------------------------------------------------------------------------------------------
@fragrat_tools
;------------------------------------------------------------------------------------------------------------------------
FUNCTION fragrat_calc, event, chrom, fragdata, tot_uniqm, USE_NOM=use_nom, INT_TYPE=int_type, $
                       PLOT=plot, DT_CORRECT=dt_correct, VERBOSE=verbose

  IF SIZE(fragdata, /TYPE) NE 8 THEN RETURN, create_fragres()
  IF NOT KEYWORD_SET(int_type) THEN int_type = 0
  IF NOT KEYWORD_SET(verbose) THEN verbose = 0
  IF NOT KEYWORD_SET(dt_correct) THEN dt_correct = 0


; +++++++++++++++++++++++
; get general information
  ID = WIDGET_INFO(event.top, find_by_uname='sel_chrom')
  sel_chrom = WIDGET_INFO(ID, /droplist_select)
  ID = WIDGET_INFO(event.top, find_by_uname='subs_preset')
  sel_subst = WIDGET_INFO(ID, /droplist_select)

  sel_masses = FLTARR(2)
  ID = WIDGET_INFO(event.top, find_by_uname='frag1_xtm')
  ixval = cb_ixval(ID)
  match=find_match(ixval.val, tot_uniqm)
  sel_masses[0] = match.val
  ID = WIDGET_INFO(event.top, find_by_uname='frag2_xtm')
  ixval = cb_ixval(ID)
  match=find_match(ixval.val, tot_uniqm)
  sel_masses[1] = match.val
  IF KEYWORD_SET(use_nom) THEN sel_masses = ROUND(sel_masses) ; use nominal masses (nom)

  psigma = FLTARR(2)
  ID = WIDGET_INFO(event.top, find_by_uname='psigma_left')
  ixval = cb_ixval(ID)
  psigma[0] = ixval.val
  chrom[sel_chrom].subst[sel_subst].sigma[0]=FIX(ixval.val,type=4)
  ID = WIDGET_INFO(event.top, find_by_uname='psigma_right')
  ixval = cb_ixval(ID)
  psigma[1] = ixval.val
  chrom[sel_chrom].subst[sel_subst].sigma[1]=FIX(ixval.val,type=4)

  fsigma = FLTARR(2)
  ID = WIDGET_INFO(event.top, find_by_uname='fsigma_left')
  ixval = cb_ixval(ID)
  fsigma[0] = ixval.val
  ID = WIDGET_INFO(event.top, find_by_uname='fsigma_right')
  ixval = cb_ixval(ID)
  fsigma[1] = ixval.val

  rt_win = DBLARR(2)
  ID = WIDGET_INFO(event.top, find_by_uname='rt_min')
  WIDGET_CONTROL, ID, get_value=rtmin
  ID = WIDGET_INFO(event.top, find_by_uname='rt_max')
  WIDGET_CONTROL, ID, get_value=rtmax
  rt_win = [rtmin, rtmax]

  vd1 = WHERE(*chrom[sel_chrom].mass EQ sel_masses[0])
   x1 = (*chrom[sel_chrom].time)[vd1]
   v1 = (*chrom[sel_chrom].intensity)[vd1]

  vd2 = WHERE(*chrom[sel_chrom].mass EQ sel_masses[1])
   x2 = (*chrom[sel_chrom].time)[vd2]
   v2 = (*chrom[sel_chrom].intensity)[vd2]

  IF verbose THEN BEGIN
    dt0 = x2[0]-x1[0]
    dt = x2-x1
    dt_mean = mean(dt)
    dt_sd = stddev(dt)
    print, "mean timeshift, t_f2-t_f1: ", dt_mean
    print, "timeshift rsd: ", 100D*dt_sd/dt_mean, " %"
  ENDIF

; +++++++++++++++++++++++
; integrate peak of mass 1
  CASE int_type OF
    0: f1_strct = int_baseline_gau(x1,v1, NSIGMA_INT=psigma, NTERMS_BASE=nterms_base, $
                                   RT_WIN=rt_win, PEAK_RET=peak_ret1, BASE_RET=base_ret1, $
                                   INT_WIN=int_win1, PEAK_INT=peak_int1, BASE_INT=base_int1, $
                                   PARAMETER=parameter1, taxis=t1, VERBOSE=0)
    1: f1_strct =          int_gau(x1,v1, NSIGMA_INT=psigma, NTERMS_BASE=nterms_base, $
                                   RT_WIN=rt_win, PEAK_RET=peak_ret1, BASE_RET=base_ret1, PEAK_FIT=peak_fit1, $
                                   BASE_FIT=base_fit1, INT_WIN=int_win1, FIT_WIN=fit_win1, PEAK_INT=peak_int1, $
                                   BASE_INT=base_int1, PARAMETER=parameter1, taxis=t1, VERBOSE=0)
  ENDCASE

  IF f1_strct.flag EQ -1 THEN BEGIN
    IF KEYWORD_SET(verbose) THEN $
      msg=DIALOG_MESSAGE('Masstrace 1: No peak found in selected file / retention time window.', /INFORMATION)
    RETURN, create_fragres()
  ENDIF

; +++++++++++++++++++++++
; integrate peak of mass 2
  CASE int_type OF
    0: f2_strct = int_baseline_gau(x2,v2, NSIGMA_INT=psigma, NTERMS_BASE=nterms_base, $
                                   RT_WIN=rt_win, PEAK_RET=peak_ret2, BASE_RET=base_ret2, $
                                   INT_WIN=int_win2, PEAK_INT=peak_int2, BASE_INT=base_int2, $
                                   PARAMETER=parameter2, taxis=t2, VERBOSE=0)
    1: f2_strct =          int_gau(x2,v2, NSIGMA_INT=psigma, NTERMS_BASE=nterms_base, $
                                   RT_WIN=rt_win, PEAK_RET=peak_ret2, BASE_RET=base_ret2, PEAK_FIT=peak_fit2, $
                                   BASE_FIT=base_fit2, INT_WIN=int_win2, FIT_WIN=fit_win2, PEAK_INT=peak_int2, $
                                   BASE_INT=base_int2, PARAMETER=parameter2, taxis=t2, VERBOSE=0)
  ENDCASE

  IF f2_strct.flag EQ -1 THEN BEGIN
    IF KEYWORD_SET(verbose) THEN $
      msg=DIALOG_MESSAGE('Masstrace 2: No peak found in selected file / retention time window.', /INFORMATION)
    RETURN, create_fragres()
  ENDIF

  IF KEYWORD_SET(plot) THEN BEGIN
    bl1_pwin = WHERE(x1 GT int_win1[0] AND x1 LT int_win1[1])
    bl2_pwin = WHERE(x2 GT int_win2[0] AND x2 LT int_win2[1])
    pxrange = [int_win1, int_win2]
    xrange = [pxrange[(WHERE(pxrange EQ MIN(pxrange)))[0]], pxrange[(WHERE(pxrange EQ MAX(pxrange)))[0]]]
    pyrange = [MIN(v1[bl1_pwin])-(MAX(v1[bl1_pwin])-MIN(v1[bl1_pwin]))*0.05, $
               MIN(v2[bl2_pwin])-(MAX(v2[bl2_pwin])-MIN(v2[bl2_pwin]))*0.05, $
               MAX(v1[bl1_pwin])+(MAX(v1[bl1_pwin])-MIN(v1[bl1_pwin]))*0.05, $
               MAX(v2[bl2_pwin])+(MAX(v2[bl2_pwin])-MIN(v2[bl2_pwin]))*0.05]
    yrange = [pyrange[(WHERE(pyrange EQ MIN(pyrange)))[0]], pyrange[(WHERE(pyrange EQ MAX(pyrange)))[0]]]

    refresh_text_pobj1, chrom, 0, 0, SET_ZERO=1

    CASE int_type OF
      0: plot_routine_pobj1, x1[bl1_pwin], v1[bl1_pwin], X_1A=x1[bl1_pwin], V_1A=v1[bl1_pwin], X_1B=x1[bl1_pwin], $
                             V_1B=base_int1, X_1C=x2[bl2_pwin], V_1C=v2[bl2_pwin], X_1D=x2[bl2_pwin], V_1D=base_int2, $
                             OVER=2345, XRANGE=xrange, YRANGE=yrange

      1: BEGIN
          ; x1[bl1_pwin], v1[bl1_pwin] : mass trace of f1
          ; x2[bl2_pwin], v2[bl2_pwin] : mass trace of f2
          ; x1[bl1_pwin], base_fit1[bl1_pwin] ; baseline for peak 1
          ; x2[bl2_pwin], base_fit2[bl2_pwin] ; baseline for peak 2
          ; x1[bl1_pwin], (peak_fit1+base_fit1)[gau1_pwin] ; gaussfit for peak 1
          ; x2[bl2_pwin], (peak_fit2+base_fit2)[gau2_pwin] ; gaussfit for peak 2
           gau1_xwin = WHERE(x1 GE fit_win1[0] AND x1 LE fit_win1[1])
           gau2_xwin = WHERE(x2 GE fit_win2[0] AND x2 LE fit_win2[1])
           gau1_ywin = INDGEN(N_ELEMENTS(gau1_xwin))
           gau2_ywin = INDGEN(N_ELEMENTS(gau2_xwin))
           plot_routine_pobj1, x1[bl1_pwin], v1[bl1_pwin], X_1A=x1[gau1_xwin], V_1A=(peak_fit1+base_fit1)[gau1_ywin], $
                               X_1B=x1[gau1_xwin], V_1B=base_fit1[gau1_ywin], X_1C=x2[bl2_pwin], V_1C=v2[bl2_pwin], $
                               X_1E=x2[gau2_xwin], V_1E=(peak_fit2+base_fit2)[gau2_ywin], X_1F=x2[gau2_xwin], V_1F=base_fit2[gau2_ywin], $
                               OVER=123467, set_colors=['k','r','b','k','orange','r','b'], XRANGE=xrange, YRANGE=yrange
         END
    ENDCASE
  ENDIF

; +++++++++++++++++++++++
; find time of peak maximum (first fragment), adjust time window and calculate ratio
  t_pmax = f1_strct.rt
  onesigma = (int_win1[1]-int_win1[0])/TOTAL(psigma)
  t_win = [t_pmax-fsigma[0]*onesigma, t_pmax+fsigma[1]*onesigma]
  t_axis= (x1)[WHERE(x1 GE t_win[0] AND x1 LE t_win[1])]
  frag1 = (peak_int1)[WHERE(t1 GE t_win[0] AND t1 LE t_win[1])]
  frag2 = (peak_int2)[WHERE(t2 GE t_win[0] AND t2 LE t_win[1])]

  f2_by_f1 = frag2/frag1

  IF dt_correct THEN BEGIN
    dt_mean = mean(x2-x1)
    f2_by_f1 = SQRT(f2_by_f1^2-dt_mean^2)
  ENDIF

  ratio = {arr: f2_by_f1, $
           mean: MEAN(f2_by_f1), $
           rsd: (STDDEV(f2_by_f1)/MEAN(f2_by_f1)), $
           nbr_dp: N_ELEMENTS(f2_by_f1)}

; +++++++++++++++++++++++
; force equal array lengths
  IF N_ELEMENTS(t_axis) GT N_ELEMENTS(ratio.arr) THEN t_axis = t_axis[0:(N_ELEMENTS(ratio.arr)-1)]
  IF N_ELEMENTS(ratio.arr) GT N_ELEMENTS(t_axis) THEN ratio.arr = ratio.arr[0:(N_ELEMENTS(t_axis)-1)]

;+++++++++++++++++++++++
; generate infotext & plot
  textcontent = STRARR(8)
  textcontent[0]=                STRING(FILE_BASENAME(chrom[sel_chrom].fname))
  textcontent[1]='F1 m/Q: '    +STRING(STRCOMPRESS(sel_masses[0], /REMOVE_ALL), FORMAT='(F12.4)')
  textcontent[2]='F2 m/Q: '    +STRING(STRCOMPRESS(sel_masses[1], /REMOVE_ALL), FORMAT='(F12.4)')
  textcontent[3]='F1 PeakMax @ '+STRING(STRCOMPRESS(t_pmax, /REMOVE_ALL))
  textcontent[4]='F2/F1 Mean: ' +STRING(STRCOMPRESS(ratio.mean, /REMOVE_ALL))
  textcontent[5]='%RSD: '       +STRING(STRCOMPRESS(ratio.rsd, /REMOVE_ALL))
  textcontent[6]='Datapoints: ' +STRING(STRCOMPRESS(ratio.nbr_dp, /REMOVE_ALL))

  IF KEYWORD_SET(plot) THEN BEGIN
    refresh_text_pobj0, SET_MANUAL=textcontent, SET_SUBTITLE='FragRatCalc', SET_COLORS=['k','k','k','k','k','k','k']
    xrange = [t_win[0]-(MAX(t_win[0])-MIN(t_win[0]))*0.01, t_win[1]+(MAX(t_win[0])-MIN(t_win[0]))*0.01]
    yrange = [MIN(ratio.arr)-(MAX(ratio.arr)-MIN(ratio.arr))*0.1, MAX(ratio.arr)+(MAX(ratio.arr)-MIN(ratio.arr))*0.1]
    plot_routine_pobj0, t_axis, ratio.arr, OVER=1, XRANGE=xrange, YRANGE=yrange
  ENDIF

;+++++++++++++++++++++++
; save results to fragres strct
  fragres = create_fragres()

           fragres.file = FILE_BASENAME(chrom[sel_chrom].fname)
         fragres.masses = sel_masses
          fragres.ratio = ratio.mean
            fragres.rsd = ratio.rsd
           fragres.n_dp = ratio.nbr_dp
           fragres.RTp1 = f1_strct.rt
  fragres.peakint_sigma = psigma
  fragres.fragrat_sigma = fsigma
  fragres.fragrat_t_win = t_win
        fragres.a_ratio = f2_strct.area/f1_strct.area
        fragres.h_ratio = f2_strct.hght/f1_strct.hght
            fragres.dRT = f2_strct.rt-f1_strct.rt

  RETURN, fragres

END