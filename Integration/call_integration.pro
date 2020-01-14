;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO call_integration
;
; INFO:
; calls specified integration methods. referrs to wid_integration
;
; MODIFICATIONS:
; - procedure now calls int method directly from chrom struct
; - case structure modified, int methods are now called (and callable) individually
;-
;------------------------------------------------------------------------------------------------------------------------
@peak_detection
@int_baseline_gau
@int_gau
@int_gbl
@int_SavGol_bl
;------------------------------------------------------------------------------------------------------------------------
PRO call_integration, sel_chrom, sel_name, PLOT=plot, FIX_XYRANGE=fix_xyrange, MAN_FLAG=man_flag, MAN_COMMENT=man_comment

  COMMON DATA
  COMMON COM_PLOT

  IF NOT KEYWORD_SET(plot) THEN plot = 0
  IF NOT KEYWORD_SET(fix_xyrange) THEN fix_xyrange = 0

  ;+++++++++++++++++++++++
  ; Default integration method: Integration Method stored in common variable subst
  ; ELSE: select method stored in chrom struct
  IF NOT KEYWORD_SET(chrom[sel_chrom].subst[sel_name].method) THEN $
    sel_intmthd = subst[sel_name].method $
      ELSE sel_intmthd = chrom[sel_chrom].subst[sel_name].method

  ;+++++++++++++++++++++++
  ; check timescale
  vd = WHERE(STRUPCASE(TAG_NAMES(chrom)) EQ 'T_SCALE', nvd) ; for restored old experiments: check if tagname t_scale exists
      IF nvd EQ 0 THEN timescale = 'Minutes' ELSE timescale = STRCOMPRESS(chrom[0].t_scale, /REMOVE_ALL)

  ;+++++++++++++++++++++++
  ; Define data for integration
  sel_quant = chrom[sel_chrom].subst[sel_name].quant
  v = chrom[sel_chrom].subst[sel_name].mass[sel_quant]
  int_mass = matchmass(tot_uniqm, v, limit_dif=0.3)
  ;  IF int_mass[0] EQ -1 THEN RETURN

  vd = WHERE(*chrom[sel_chrom].mass EQ int_mass[0])
   x = (*chrom[sel_chrom].time)[vd]
   v = (*chrom[sel_chrom].intensity)[vd]

  ;+++++++++++++++++++++++
  ; Define plot window range
  xrange = [chrom[sel_chrom].subst[sel_name].rt_win[0],chrom[sel_chrom].subst[sel_name].rt_win[1]]
  offset = (MAX(v[WHERE(x GE xrange[0] AND x LE xrange[1], nvd)], /nan)-MIN(v[WHERE(x GE xrange[0] AND x LE xrange[1], nvd)], /nan))*0.05
  yrange = [MIN(v[WHERE(x GE xrange[0] AND x LE xrange[1], nvd)], /nan)-offset, MAX(v[WHERE(x GE xrange[0] AND x LE xrange[1], nvd)], /nan)+offset]

  ;+++++++++++++++++++++++
  ; Get additional integration settings
  sigma = chrom[sel_chrom].subst[sel_name].sigma ; [sigma_left, sigma_right]
  nterms_base = chrom[sel_chrom].subst[sel_name].bl_type + 1 ; baseline type

  ;+++++++++++++++++++++++
  ; Get rt settings
  rt_win = [chrom[sel_chrom].subst[sel_name].rt_win[0], chrom[sel_chrom].subst[sel_name].rt_win[1]]
  ;  IF size(rtlock,/type) EQ 8 AND rtlock[0].active EQ 1 THEN rt_win=calc_rtlockwin(x[vd],v[vd],rtlock,rt_win)
  int_win = [chrom[sel_chrom].subst[sel_name].int_win[0], chrom[sel_chrom].subst[sel_name].int_win[1]]

  ;+++++++++++++++++++++++
  ; Get noise value if available
  chk_noise = chrom[sel_chrom].subst[sel_name].ires.noise[sel_quant]
  IF FINITE(chk_noise) NE 1 THEN chk_noise=0. ; set zero if not calculated yet

  verbose = 0

  ;****************************************************************************************************************************************************
    CASE sel_intmthd OF
      'gau': $
          BEGIN
            strct=int_gau(x,v, NSIGMA_FIT=sigma, NSIGMA_INT=nsigma_int, NTERMS_BASE=nterms_base, RT_WIN=rt_win, $
                               INT_WIN=int_win, FIT_WIN=fit_win,  PEAK_RET=peak_ret, BASE_RET=base_ret,  PEAK_FIT=peak_fit, $
                               BASE_FIT=base_fit, PEAK_INT=peak_int, BASE_INT=base_int, PARAMETER=parameter, VERBOSE=verbose, $
                               CHK_NOISE=chk_noise, TIMESCALE=timescale)

            rt=strct.rt
            area=strct.area
            height=strct.hght
            ts=strct.ts
            te=strct.te
            width=strct.wdth
            flag=strct.flag
            comment=strct.comment

            IF plot THEN BEGIN
              IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
                fit_pwin=WHERE(x GE fit_win[0] AND x LE fit_win[1])
                int_pwin=WHERE(x GE int_win[0] AND x LE int_win[1])
                prange = [rt_win, fit_win, int_win]
                xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
                yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                          MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
                plot_routine_pobj1, x, v, X_1A=x[fit_pwin], V_1A=peak_fit+base_fit, X_1B=x[fit_pwin], V_1B=base_fit, $
                                          X_1E=x[int_pwin], V_1E=peak_int+base_int, X_1F=x[int_pwin], V_1F=base_int, $
                                          OVER=12367, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
              ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF
          END
  ; ****************************************************************************************************************************************************

    'gau_SGbase': $
        BEGIN
          strct=int_gau_SGbase(x,v, NSIGMA_FIT=sigma, NSIGMA_INT=nsigma_int, NTERMS_BASE=nterms_base, RT_WIN=rt_win, $
                             INT_WIN=int_win, FIT_WIN=fit_win,  PEAK_RET=peak_ret, BASE_RET=base_ret,  PEAK_FIT=peak_fit, $
                             BASE_FIT=base_fit, PEAK_INT=peak_int, BASE_INT=base_int, PARAMETER=parameter, VERBOSE=verbose, $
                             CHK_NOISE=chk_noise, TIMESCALE=timescale)

          rt=strct.rt
          area=strct.area
          height=strct.hght
          ts=strct.ts
          te=strct.te
          width=strct.wdth
          flag=strct.flag
          comment=strct.comment

          IF plot THEN BEGIN
            IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
              fit_pwin=WHERE(x GE fit_win[0] AND x LE fit_win[1])
              int_pwin=WHERE(x GE int_win[0] AND x LE int_win[1])
              prange = [rt_win, fit_win, int_win]
              xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
              yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                        MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
              plot_routine_pobj1, x, v, X_1A=x[fit_pwin], V_1A=peak_fit+base_fit, X_1B=x[fit_pwin], V_1B=base_fit, $
                                        X_1E=x[int_pwin], V_1E=peak_int+base_int, X_1F=x[int_pwin], V_1F=base_int, $
                                        OVER=12367, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
          ENDIF
        END
    ; ****************************************************************************************************************************************************

      'bl': $
        BEGIN
          strct=int_baseline_gau(x,v, NSIGMA_INT=sigma, NTERMS_BASE=nterms_base, RT_WIN=rt_win, PEAK_RET=peak_ret, BASE_RET=base_ret, $
                                 INT_WIN=int_win, PEAK_INT=peak_int, BASE_INT=base_int, PARAMETER=parameter, VERBOSE=verbose, CHK_NOISE=chk_noise)

            rt=strct.rt
            height=strct.hght
            area=strct.area
            width=strct.wdth
            ts=strct.ts
            te=strct.te
            flag=strct.flag
            comment=strct.comment

          IF plot THEN BEGIN
            IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
              int_pwin=WHERE(x GE int_win[0] AND x LE int_win[1])
              prange=[rt_win, int_win]
              xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
              yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                        MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
              plot_routine_pobj1, x, v, X_1A=x[int_pwin], V_1A=v[int_pwin], X_1B=x[int_pwin], V_1B=base_int, $
                                  OVER=123, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
          ENDIF
        END
  ; ****************************************************************************************************************************************************
      'bl_fix': $
        BEGIN
          strct=int_bl_fixpoints(x,v, $
                                 NTERMS_BASE=nterms_base, NSIGMA_INT=nsigma_int, $
                                 RT_WIN=rt_win, PEAK_RET=peak_ret, BASE_RET=base_ret,  $
                                 INT_WIN=int_win, PEAK_INT=peak_int, BASE_INT=base_int,  $
                                 PARAMETER=parameter, TAXIS=taxis, VERBOSE=verbose, CHK_NOISE=chk_noise)


            rt=strct.rt
            height=strct.hght
            area=strct.area
            width=strct.wdth
            ts=strct.ts
            te=strct.te
            flag=strct.flag
            comment=strct.comment

          IF plot THEN BEGIN
            IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
              int_pwin=WHERE(x GE int_win[0] AND x LE int_win[1])
              xrange = [rt_win[0]/1.025,rt_win[1]*1.025]
  ;            prange=[rt_win, int_win]
  ;            xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
              yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                        MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
              plot_routine_pobj1, x, v, X_1A=x[int_pwin], V_1A=v[int_pwin], X_1B=x[int_pwin], V_1B=base_int, $
                                  OVER=123, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
          ENDIF
        END
  ; ****************************************************************************************************************************************************
      'gbl': $
        BEGIN
          strct=Integrate_GumbelPeak(x,v, NTERMS_BASE=nterms_base, NSIGMA_FIT=sigma, NSIGMA_INT=sigma, $
                                          RT_WIN=rt_win, PEAK_RET=peak_ret, BASE_RET=base_ret,  $
                                          FIT_WIN=fit_win, PEAK_FIT=peak_fit, BASE_FIT=base_fit,  $
                                          INT_WIN=int_win, PEAK_INT=peak_int, BASE_INT=base_int,  $
                                          PARAMETER=parameter, TIMESCALE=timescale, VERBOSE=verbose, CHK_NOISE=chk_noise)

            rt=strct.ret
            area=strct.area
            height=strct.hght
            ts=strct.ts
            te=strct.te
            width=strct.wdth
            flag=strct.flag
            comment=strct.comment

          IF plot THEN BEGIN
            IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
              w_fit_win=where((x GE fit_win[0]) AND (x LE fit_win[1]), nw_fit_win)
              prange=[rt_win, fit_win, int_win]
              xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
              yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                        MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
              plot_routine_pobj1, x, v, X_1A=x[w_fit_win], V_1A=peak_fit+base_fit, X_1B=x[w_fit_win], V_1B=base_fit, $
                                  OVER=123, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
          ENDIF
        END
  ; ****************************************************************************************************************************************************
      'dblgbl_1st': $
        BEGIN
          strct=Integrate_GumbelDoublePeak(x, v, NTERMS_BASE=nterms_base, NSIGMA_FIT=sigma, NSIGMA_INT=sigma, $
                                           RT_WIN=rt_win, PEAK_RET1=peak_ret1, PEAK_RET2=peak_ret2, BASE_RET=base_ret,  $
                                           FIT_WIN=fit_win, PEAK_FIT1=peak_fit1, PEAK_FIT2=peak_fit2, BASE_FIT=base_fit,  $
                                           INT_WIN=int_win, PEAK_INT1=peak_int1, PEAK_INT2=peak_int2, BASE_INT=base_int,  $
                                           PARAMETER=parameter, TIMESCALE=timescale, VERBOSE=verbose, CHK_NOISE=chk_noise)

            wpeak=1
            rt=strct.ret1
            area=strct.area1
            height=strct.hght1
            ts=strct.ts
            te=strct.te
            width=strct.wdth1
            flag=strct.flag
            comment=strct.comment

          IF plot THEN BEGIN
            IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
              w_fit_win=WHERE((x GE fit_win[0]) AND (x LE fit_win[1]), nw_fit_win)
              prange=[rt_win, fit_win, int_win]
              xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
              yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                        MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
              plot_routine_pobj1, x, v, X_1A=x[w_fit_win], V_1A=peak_fit1+base_fit, X_1B=x[w_fit_win], V_1B=base_fit, $
                                  OVER=123, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
          ENDIF
        END
  ; ****************************************************************************************************************************************************
      'dblgbl_2nd': $
        BEGIN
          strct=Integrate_GumbelDoublePeak(x, v, NTERMS_BASE=nterms_base, NSIGMA_FIT=sigma, NSIGMA_INT=sigma, $
                                           RT_WIN=rt_win, PEAK_RET1=peak_ret1, PEAK_RET2=peak_ret2, BASE_RET=base_ret,  $
                                           FIT_WIN=fit_win, PEAK_FIT1=peak_fit1, PEAK_FIT2=peak_fit2, BASE_FIT=base_fit,  $
                                           INT_WIN=int_win, PEAK_INT1=peak_int1, PEAK_INT2=peak_int2, BASE_INT=base_int,  $
                                           PARAMETER=parameter, TIMESCALE=timescale, VERBOSE=verbose, CHK_NOISE=chk_noise)

            wpeak=2
            rt=strct.ret2
            area=strct.area2
            height=strct.hght2
            ts=strct.ts
            te=strct.te
            width=strct.wdth2
            flag=strct.flag
            comment=strct.comment

          IF plot THEN BEGIN
            IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
              w_fit_win=WHERE((x GE fit_win[0]) AND (x LE fit_win[1]), nw_fit_win)
              prange=[rt_win, fit_win, int_win]
              xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
              yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                        MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
              plot_routine_pobj1, x, v, X_1B=x[w_fit_win], V_1B=base_fit, X_1C=x[w_fit_win], V_1C=peak_fit2+base_fit, $
                                  OVER=134, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
          ENDIF
        END
  ; ****************************************************************************************************************************************************
      'dblgbl_sum': $
        BEGIN
          strct=Integrate_GumbelDoublePeak(x, v, NTERMS_BASE=nterms_base, NSIGMA_FIT=sigma, NSIGMA_INT=sigma, $
                                           RT_WIN=rt_win, PEAK_RET1=peak_ret1, PEAK_RET2=peak_ret2, BASE_RET=base_ret,  $
                                           FIT_WIN=fit_win, PEAK_FIT1=peak_fit1, PEAK_FIT2=peak_fit2, BASE_FIT=base_fit,  $
                                           INT_WIN=int_win, PEAK_INT1=peak_int1, PEAK_INT2=peak_int2, BASE_INT=base_int,  $
                                           PARAMETER=parameter, TIMESCALE=timescale, VERBOSE=verbose, CHK_NOISE=chk_noise)

            wpeak=3
            rt=(strct.ret1+strct.ret2)/2D
            area=strct.area1+strct.area2
            height=strct.hght1+strct.hght2
            ts=strct.ts
            te=strct.te
            width=strct.wdth1+strct.wdth1
            flag=strct.flag
            comment=strct.comment

          IF plot THEN BEGIN
            IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
              w_fit_win=WHERE((x GE fit_win[0]) AND (x LE fit_win[1]), nw_fit_win)
              prange=[rt_win, fit_win, int_win]
              xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
              yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                        MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
              plot_routine_pobj1, x, v, X_1A=x[w_fit_win], V_1A=peak_fit1+base_fit, X_1B=x[w_fit_win], V_1B=base_fit, $
                                        X_1C=x[w_fit_win], V_1C=peak_fit2+base_fit, X_1D=x[w_fit_win], V_1D=peak_fit1+peak_fit2+base_fit, $
                                        OVER=12345, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
          ENDIF
        END
  ; ****************************************************************************************************************************************************
      'SavGol_bl': $
        BEGIN
          strct=int_SavGol_bl(x,v, y_SG=y_SG, NSIGMA_INT=sigma, NTERMS_BASE=nterms_base, RT_WIN=rt_win, PEAK_RET=peak_ret, BASE_RET=base_ret, $
                                 INT_WIN=int_win, PEAK_INT=peak_int, BASE_INT=base_int, PARAMETER=parameter, VERBOSE=verbose, CHK_NOISE=chk_noise)

            rt=strct.rt
            height=strct.hght
            area=strct.area
            width=strct.wdth
            ts=strct.ts
            te=strct.te
            flag=strct.flag
            comment=strct.comment

          IF plot THEN BEGIN
            IF STRUPCASE(strct.comment) EQ 'INTEGRATED' THEN BEGIN
              int_pwin=WHERE(x GE int_win[0] AND x LE int_win[1])
              prange=[rt_win, int_win]
              xrange = [prange[(WHERE(prange EQ MIN(prange)))[0]], prange[(WHERE(prange EQ MAX(prange)))[0]]]
              yrange = [MIN(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)-offset, $
                        MAX(v[WHERE(x GE rt_win[0] AND x LE rt_win[1], nvd)], /NAN)+offset]
              plot_routine_pobj1, x, v, X_1A=x[int_pwin], V_1A=v[int_pwin], X_1B=x[int_pwin], V_1B=base_int, $
                                  X_1E=0, V_1E=0, X_1F=x, V_1F=y_SG, $
                                  OVER=12367, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
            ENDIF ELSE plot_routine_pobj1, x, v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
          ENDIF
        END
  ; ****************************************************************************************************************************************************
    ENDCASE

  ;+++++++++++++++++++++++++++++
  ; update ires
    IF KEYWORD_SET(man_flag) AND flag NE -1 THEN flag=man_flag ; overwrite flag with manual value (keyword) if not 'no peak found'
    IF KEYWORD_SET(man_comment) AND flag NE -1 THEN comment=man_comment

    chrom[sel_chrom].subst[sel_name].ires = { rt:rt ,$
                                              height:height ,$
                                              area:area ,$
                                              ts:ts ,$
                                              te:te ,$
                                              width:width ,$
                                              flag:flag ,$
                                              comment:comment, $
                                              noise:chrom[sel_chrom].subst[sel_name].ires.noise }


  ;+++++++++++++++++++++++++++++
  ; update plot object 1 text
    IF plot THEN BEGIN
      IF int_mass[0] EQ -1 THEN sel_int_mass='NaN' ELSE sel_int_mass=int_mass[0]
      refresh_text_pobj1, chrom, sel_chrom, sel_name, int_mass=sel_int_mass
    ENDIF

END
