;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO plot_routine_pobj0
;
; AUTHOR:
; S.Sala, modifications F.Obersteiner
;
; INFO:
; specific for plot0
;-
;------------------------------------------------------------------------------------------------------------------------
PRO plot_routine_pobj0, x, v, X_0A=x_0a, V_0A=v_0a, X_0B=x_0b, V_0B=v_0b, X_0C=x_0c, V_0C=v_0c, X_0D=x_0d, V_0D=v_0d, X_0E=x_0e, V_0E=v_0e, $
                        X_0F=x_0f, V_0F=v_0f, OVER=over, SET_COLORS=set_colors, SET_ZERO=set_zero, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange, $
                        SET_TITLE=set_title, XYTITLE=xytitle, GEN_LEGEND=gen_legend
;t=systime(1)
  COMMON COM_PLOT

  IF KEYWORD_SET(gen_legend) THEN BEGIN
    plotnames=STRARR(N_ELEMENTS(gen_legend))+ $
                     STRCOMPRESS(STRING(gen_legend, FORMAT='(D14.3)'), /REMOVE_ALL)+' Th'
    wn0=WHERE(gen_legend NE 0.)
    IF wn0[0] NE -1 THEN BEGIN
      names=STRARR(N_ELEMENTS(p_obj0))
      names[wn0]=plotnames[wn0]
      FOR i=0, N_ELEMENTS(p_obj0)-1 DO (p_obj0[i]).name=names[i]
      l=legend(TARGET=p_obj0, SAMPLE_WIDTH=0.15)
    ENDIF
    RETURN
  ENDIF
;+++++++++++++++++++++++++++++
; check if only one datapoint
  IF N_ELEMENTS(x) LT 2 THEN set_zero = 1
  IF N_ELEMENTS(v) LT 2 THEN set_zero = 1

;+++++++++++++++++++++++++++++
; keyword set_zero: reset plot data and x-/y-range
  IF KEYWORD_SET(set_zero) THEN BEGIN
    over = 0
    x = 0
    v = 0
    xrange = [0,1]
    yrange = [0,1]
  ENDIF

;+++++++++++++++++++++++++++++
; keyword set_colors: set to color array or keep default
  colors=['k','r','g','b','deep_sky_blue','m','gold']
  IF KEYWORD_SET(set_colors) THEN colors[0:(N_ELEMENTS(set_colors)-1)]=set_colors[0:(N_ELEMENTS(set_colors)-1)]

;+++++++++++++++++++++++++++++
; check keywords x- and yrange
  IF NOT KEYWORD_SET(xrange) THEN $
    xrange = [MIN(x,/NAN), MAX(x,/NAN)]

  IF NOT KEYWORD_SET(yrange) THEN BEGIN
    offset = (MAX(v,/NAN)-MIN(v,/NAN))*0.05
    yrange = [MIN(v,/NAN)-offset, MAX(v,/NAN)+offset]
  ENDIF

  IF KEYWORD_SET(fix_xyrange) THEN BEGIN
    xrange = (p_obj0[0]).xrange
    yrange = (p_obj0[0]).yrange
  ENDIF

;+++++++++++++++++++++++++++++
; check keyword set_title
  IF NOT KEYWORD_SET(SET_TITLE) THEN title='' ELSE set_title=title

;+++++++++++++++++++++++++++++
; check keyword set_title
  IF KEYWORD_SET(XYTITLE) THEN BEGIN
    (p_obj0[0]).Refresh, /DISABLE
    (p_obj0[0]).Xtitle=xytitle[0]
    (p_obj0[0]).Ytitle=xytitle[1]
    (p_obj0[0]).Refresh
  ENDIF

;+++++++++++++++++++++++++++++
; cases of keyword over:
  CASE over OF
    0: $
      BEGIN ; set_zero
        (p_obj0[0]).Refresh, /DISABLE
        (p_obj0[1]).Refresh, /DISABLE
        (p_obj0[2]).Refresh, /DISABLE
        (p_obj0[3]).Refresh, /DISABLE
        (p_obj0[4]).Refresh, /DISABLE
        (p_obj0[5]).Refresh, /DISABLE
        (p_obj0[6]).Refresh, /DISABLE
        (p_obj0[0]).Color = colors[0]
        (p_obj0[1]).Color = colors[1]
        (p_obj0[2]).Color = colors[2]
        (p_obj0[3]).Color = colors[3]
        (p_obj0[4]).Color = colors[4]
        (p_obj0[5]).Color = colors[5]
        (p_obj0[6]).Color = colors[6]
        (p_obj0[0]).SetData,   x, v
        (p_obj0[1]).SetData,   x, v
        (p_obj0[2]).SetData,   x, v
        (p_obj0[3]).SetData,   x, v
        (p_obj0[4]).SetData,   x, v
        (p_obj0[5]).SetData,   x, v
        (p_obj0[6]).SetData,   x, v
        (p_obj0[0]).Refresh
        (p_obj0[1]).Refresh
        (p_obj0[2]).Refresh
        (p_obj0[3]).Refresh
        (p_obj0[4]).Refresh
        (p_obj0[5]).Refresh
        (p_obj0[6]).Refresh
      END

    1: $
      BEGIN ; set data for p_obj0
        (p_obj0[0]).Refresh, /DISABLE
        (p_obj0[1]).Refresh, /DISABLE
        (p_obj0[2]).Refresh, /DISABLE
        (p_obj0[3]).Refresh, /DISABLE
        (p_obj0[4]).Refresh, /DISABLE
        (p_obj0[5]).Refresh, /DISABLE
        (p_obj0[6]).Refresh, /DISABLE
        (p_obj0[0]).xrange=xrange
        (p_obj0[0]).yrange=yrange
        (p_obj0[0]).Color = colors[0]
        (p_obj0[1]).Color = colors[1]
        (p_obj0[2]).Color = colors[2]
        (p_obj0[3]).Color = colors[3]
        (p_obj0[4]).Color = colors[4]
        (p_obj0[5]).Color = colors[5]
        (p_obj0[6]).Color = colors[6]
        (p_obj0[0]).SetData, x, v
        (p_obj0[1]).SetData, 0, 0
        (p_obj0[2]).SetData, 0, 0
        (p_obj0[3]).SetData, 0, 0
        (p_obj0[4]).SetData, 0, 0
        (p_obj0[5]).SetData, 0, 0
        (p_obj0[6]).SetData, 0, 0
        (p_obj0[0]).Refresh
        (p_obj0[1]).Refresh
        (p_obj0[2]).Refresh
        (p_obj0[3]).Refresh
        (p_obj0[4]).Refresh
        (p_obj0[5]).Refresh
        (p_obj0[6]).Refresh
      END

    2: $
      BEGIN ; set data for p_obj0a
        (p_obj0[1]).Refresh, /DISABLE
        (p_obj0[1]).Color = colors[1]
        (p_obj0[1]).SetData, x, v
        (p_obj0[1]).Refresh
      END

    12: $
      BEGIN ; set data for p_obj0, p_obj0a
        (p_obj0[0]).Refresh, /DISABLE
        (p_obj0[1]).Refresh, /DISABLE
        (p_obj0[2]).Refresh, /DISABLE
        (p_obj0[3]).Refresh, /DISABLE
        (p_obj0[4]).Refresh, /DISABLE
        (p_obj0[5]).Refresh, /DISABLE
        (p_obj0[6]).Refresh, /DISABLE
        (p_obj0[0]).xrange=xrange
        (p_obj0[0]).yrange=yrange
        (p_obj0[0]).Color = colors[0]
        (p_obj0[1]).Color = colors[1]
        (p_obj0[2]).Color = colors[2]
        (p_obj0[3]).Color = colors[3]
        (p_obj0[4]).Color = colors[4]
        (p_obj0[5]).Color = colors[5]
        (p_obj0[6]).Color = colors[6]
        (p_obj0[0]).SetData, x, v
        (p_obj0[1]).SetData, x_0a, v_0a
        (p_obj0[2]).SetData, 0, 0
        (p_obj0[3]).SetData, 0, 0
        (p_obj0[4]).SetData, 0, 0
        (p_obj0[5]).SetData, 0, 0
        (p_obj0[6]).SetData, 0, 0
        (p_obj0[0]).Refresh
        (p_obj0[1]).Refresh
        (p_obj0[2]).Refresh
        (p_obj0[3]).Refresh
        (p_obj0[4]).Refresh
        (p_obj0[5]).Refresh
        (p_obj0[6]).Refresh
      END

    1234: $
      BEGIN ; set data for p_obj0, p_obj0a, p_obj0b, p_obj0c; rest: set zero
        (p_obj0[0]).Refresh, /DISABLE
        (p_obj0[1]).Refresh, /DISABLE
        (p_obj0[2]).Refresh, /DISABLE
        (p_obj0[3]).Refresh, /DISABLE
        (p_obj0[4]).Refresh, /DISABLE
        (p_obj0[5]).Refresh, /DISABLE
        (p_obj0[6]).Refresh, /DISABLE
        (p_obj0[0]).xrange=xrange
        (p_obj0[0]).yrange=yrange
        (p_obj0[0]).Color = colors[0]
        (p_obj0[1]).Color = colors[1]
        (p_obj0[2]).Color = colors[2]
        (p_obj0[3]).Color = colors[3]
        (p_obj0[4]).Color = colors[4]
        (p_obj0[5]).Color = colors[5]
        (p_obj0[6]).Color = colors[6]
        (p_obj0[0]).SetData, x, v
        (p_obj0[1]).SetData, x_0a, v_0a
        (p_obj0[2]).SetData, x_0b, v_0b
        (p_obj0[3]).SetData, x_0c, v_0c
        (p_obj0[4]).SetData, 0, 0
        (p_obj0[5]).SetData, 0, 0
        (p_obj0[6]).SetData, 0, 0
        (p_obj0[0]).Refresh
        (p_obj0[1]).Refresh
        (p_obj0[2]).Refresh
        (p_obj0[3]).Refresh
        (p_obj0[4]).Refresh
        (p_obj0[5]).Refresh
        (p_obj0[6]).Refresh
      END

    1234567: $
      BEGIN ; set data for all pobjects of pobj0
        (p_obj0[0]).Refresh, /DISABLE
        (p_obj0[1]).Refresh, /DISABLE
        (p_obj0[2]).Refresh, /DISABLE
        (p_obj0[3]).Refresh, /DISABLE
        (p_obj0[4]).Refresh, /DISABLE
        (p_obj0[5]).Refresh, /DISABLE
        (p_obj0[6]).Refresh, /DISABLE
        (p_obj0[0]).xrange=xrange
        (p_obj0[0]).yrange=yrange
        (p_obj0[0]).Color = colors[0]
        (p_obj0[1]).Color = colors[1]
        (p_obj0[2]).Color = colors[2]
        (p_obj0[3]).Color = colors[3]
        (p_obj0[4]).Color = colors[4]
        (p_obj0[5]).Color = colors[5]
        (p_obj0[6]).Color = colors[6]
        (p_obj0[0]).SetData, x, v
        (p_obj0[1]).SetData, x_0a, v_0a
        (p_obj0[2]).SetData, x_0b, v_0b
        (p_obj0[3]).SetData, x_0c, v_0c
        (p_obj0[4]).SetData, x_0d, v_0d
        (p_obj0[5]).SetData, x_0e, v_0e
        (p_obj0[6]).SetData, x_0f, v_0f
        (p_obj0[0]).Refresh
        (p_obj0[1]).Refresh
        (p_obj0[2]).Refresh
        (p_obj0[3]).Refresh
        (p_obj0[4]).Refresh
        (p_obj0[5]).Refresh
        (p_obj0[6]).Refresh
      END
  ENDCASE
;print, 'plot update: ', systime(1)-t
END