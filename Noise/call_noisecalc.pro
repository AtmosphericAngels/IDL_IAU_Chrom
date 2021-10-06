;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO call_noisecalc
;
; INFO:
; calls noise calculation
;
; MODIFICATIONS:
; - introduced Nov 2013 (FO)
; - June 2014: plot selected mass & plot polyfit +/- 1/2 noise (FO)
;-
;------------------------------------------------------------------------------------------------------------------------
@calc_noise_fct
@refresh_text_pobj0
;------------------------------------------------------------------------------------------------------------------------
PRO call_noisecalc, sel_chrom, sel_name, event, $
                    NOISE_UNAME=noise_uname, DELAY=delay, PLOT=plot, $
                    NO_WARN=no_warn
  COMMON DATA
  ;COMMON COM_PLOT

  ;refresh_text_pobj0, set_zero=1
  textcontent = STRARR(8)
  colors = ['k','b','r','r','deep_sky_blue','m','gold']
  subtitle = 'NoiseCalc'

  ;+++++++++++++++++++++++++++++
  ; define size of noise array according to number of masses defined for quantification
  noise = FLTARR(N_ELEMENTS(get_finval(chrom[sel_chrom].subst[sel_name].mass)))*!Values.F_NAN
  sel_quant = chrom[sel_chrom].subst[sel_name].quant

  ;+++++++++++++++++++++++++++++
  ; define standard operation if not defined
  IF NOT KEYWORD_SET(noise_uname) THEN noise_uname = 'noise_pres'
  IF NOT KEYWORD_SET(delay) THEN delay = 0.
  IF NOT KEYWORD_SET(plot) THEN plot = 0
  IF NOT KEYWORD_SET(no_warn) THEN no_warn = 0 ; warning if insufficient data on by default

  ;+++++++++++++++++++++++++++++
  ; call specified operation
    CASE noise_uname OF
      'noise_pres': $ ; apply settings to selected chrom
        BEGIN
          noise_win = chrom[sel_chrom].subst[sel_name].noise_win
          strct = calc_noise_fct(chrom, sel_chrom, sel_name, tot_uniqm, NOISE_WIN=noise_win, NO_WARN=no_warn)
            IF strct.ndatapoints LE 3 THEN BEGIN
              IF plot EQ 1 THEN msg=DIALOG_MESSAGE('Not enough datapoints for noise calculation found in specified retention time window.', /INFORMATION)
              RETURN; Not enough datapoints found for noise calculation
            ENDIF
          noise[sel_quant] = strct.noise
          chrom[sel_chrom].subst[sel_name].ires.noise = noise
          IF plot EQ 1 THEN BEGIN
            plot_routine_pobj0, strct.x, strct.y, X_0A=strct.x, V_0A=strct.yfit, X_0B=strct.x, V_0B=strct.yfit+(strct.noise)/2.0, X_0C=strct.x, V_0C=strct.yfit-(strct.noise)/2.0, $
                                OVER=1234, SET_COLORS=colors, XRANGE=strct.xrange, YRANGE=strct.yrange
            textcontent[0] = FILE_BASENAME(chrom[sel_chrom].fname)
            textcontent[1]='Noise: '+STRCOMPRESS(STRING(strct.noise), /REMOVE_ALL)
            textcontent[2]='on m/Q: '+STRCOMPRESS(STRING(strct.noisemass, FORMAT='(D14.4)'), /REMOVE_ALL)
            refresh_text_pobj0, SET_COLORS=['k','k','k','k','k'], SET_MANUAL=textcontent, SET_SUBTITLE=subtitle
          ENDIF
        END

      'noise_def_pres': $ ; apply default to selected chrom
        BEGIN
          chrom[sel_chrom].subst[sel_name].noise_win = subst[sel_name].noise_win
          noise_win = subst[sel_name].noise_win
          strct = calc_noise_fct(chrom, sel_chrom, sel_name, tot_uniqm, NOISE_WIN=noise_win, NO_WARN=no_warn)
            IF strct.ndatapoints  LE 3 THEN BEGIN
              IF plot EQ 1 THEN msg=DIALOG_MESSAGE('Not enough datapoints for noise calculation found in specified retention time window.', /INFORMATION)
              RETURN; Not enough datapoints found for noise calculation
            ENDIF
          noise[sel_quant] = strct.noise
          chrom[sel_chrom].subst[sel_name].ires.noise = noise
          IF plot EQ 1 THEN BEGIN
            plot_routine_pobj0, strct.x, strct.y, X_0A=strct.x, V_0A=strct.yfit, X_0B=strct.x, V_0B=strct.yfit+(strct.noise)/2.0, X_0C=strct.x, V_0C=strct.yfit-(strct.noise)/2.0, $
                                OVER=1234, SET_COLORS=colors, XRANGE=strct.xrange, YRANGE=strct.yrange
            textcontent[0] = FILE_BASENAME(chrom[sel_chrom].fname)
            textcontent[1]='Noise: '+STRCOMPRESS(STRING(strct.noise), /REMOVE_ALL)
            textcontent[2]='on m/Q: '+STRCOMPRESS(STRING(strct.noisemass, FORMAT='(D14.4)'), /REMOVE_ALL)
            refresh_text_pobj0, SET_MANUAL=textcontent, SET_COLORS=['k','k','k','k','k'], SET_SUBTITLE=subtitle
          ENDIF
        END

      'noise_all': $ ; apply settings to all chroms
        BEGIN
          noise_win = chrom[sel_chrom].subst[sel_name].noise_win
          FOR i=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN
            chrom[i].subst[sel_name].quant = sel_quant
            chrom[i].subst[sel_name].noise_win = noise_win
            strct = calc_noise_fct(chrom, i, sel_name, tot_uniqm, NOISE_WIN=noise_win, NO_WARN=no_warn)
            IF strct.ndatapoints GT 3 THEN BEGIN
              noise[sel_quant] = strct.noise
              chrom[i].subst[sel_name].ires.noise = noise
              IF plot EQ 1 THEN BEGIN
                plot_routine_pobj0, strct.x, strct.y, X_0A=strct.x, V_0A=strct.yfit, X_0B=strct.x, V_0B=strct.yfit+(strct.noise)/2.0, X_0C=strct.x, V_0C=strct.yfit-(strct.noise)/2.0, $
                                    OVER=1234, SET_COLORS=colors, XRANGE=strct.xrange, YRANGE=strct.yrange
                textcontent[0] = FILE_BASENAME(chrom[i].fname)
                textcontent[1]='Noise: '+STRCOMPRESS(STRING(strct.noise), /REMOVE_ALL)
                textcontent[2]='on m/Q: '+STRCOMPRESS(STRING(strct.noisemass, FORMAT='(D14.4)'), /REMOVE_ALL)
                refresh_text_pobj0, SET_MANUAL=textcontent, SET_COLORS=['k','k','k','k','k'], SET_SUBTITLE=subtitle
              ENDIF
            ENDIF ELSE IF plot EQ 1 THEN msg=DIALOG_MESSAGE('Not enough datapoints for noise calculation found in specified retention time window (skipping to next file).', /INFORMATION)
;               ID_chrom=WIDGET_INFO(event.top, find_by_uname='chrom')
;               WIDGET_CONTROL, ID_chrom, SET_DROPLIST_SELECT=i
            WAIT, delay
          ENDFOR
        END

      'noise_def_all' : $ ; apply default to all chroms
        BEGIN
          noise_win = subst[sel_name].noise_win
;          plot_routine_pobj0, SET_ZERO=1
          FOR i=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN
            chrom[i].subst[sel_name].quant = subst[sel_name].quant
            chrom[i].subst[sel_name].noise_win = subst[sel_name].noise_win
            strct = calc_noise_fct(chrom, i, sel_name, tot_uniqm, NOISE_WIN=noise_win, NO_WARN=no_warn)
            IF strct.ndatapoints GT 3 THEN BEGIN
              noise[sel_quant] = strct.noise
              chrom[i].subst[sel_name].ires.noise = noise
              IF plot EQ 1 THEN BEGIN
                plot_routine_pobj0, strct.x, strct.y, X_0A=strct.x, V_0A=strct.yfit, X_0B=strct.x, V_0B=strct.yfit+(strct.noise)/2.0, X_0C=strct.x, V_0C=strct.yfit-(strct.noise)/2.0, $
                                    OVER=1234, SET_COLORS=colors, XRANGE=strct.xrange, YRANGE=strct.yrange
                textcontent[0] = FILE_BASENAME(chrom[i].fname)
                textcontent[1]='Noise: '+STRCOMPRESS(STRING(strct.noise), /REMOVE_ALL)
                textcontent[2]='on m/Q: '+STRCOMPRESS(STRING(strct.noisemass, FORMAT='(D14.4)'), /REMOVE_ALL)
                refresh_text_pobj0, SET_MANUAL=textcontent, SET_COLORS=['k','k','k','k','k'], SET_SUBTITLE=subtitle
              ENDIF
            ENDIF ELSE IF plot EQ 1 THEN msg=DIALOG_MESSAGE('Not enough datapoints for noise calculation found in specified retention time window (skipping to next file).', /INFORMATION)
;              ID_chrom=WIDGET_INFO(event.top, find_by_uname='chrom')
;              WIDGET_CONTROL, ID_chrom, SET_DROPLIST_SELECT=i
            WAIT, delay
          ENDFOR
        END
    ENDCASE

END