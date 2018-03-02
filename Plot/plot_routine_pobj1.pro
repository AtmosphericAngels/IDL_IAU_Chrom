;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO plot_routine_pobj1
;
; AUTHOR:
; S.Sala, modifications F.Obersteiner
; 
; INFO: 
; specific for plot1
;-
;------------------------------------------------------------------------------------------------------------------------
PRO plot_routine_pobj1, x, v, X_1A=x_1a, V_1A=v_1a, X_1B=x_1b, V_1B=v_1b, X_1C=x_1c, V_1C=v_1c, $
                              X_1D=x_1d, V_1D=v_1d, X_1E=x_1e, V_1E=v_1e, X_1F=x_1f, V_1F=v_1f, $
                              OVER=over, SET_ZERO=set_zero, TITLE=title, SET_COLORS=set_colors, $
                              XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange, XYTITLE=xytitle
;t=systime(1)
  COMMON COM_PLOT

;+++++++++++++++++++++++++++++
; check if only one datapoint
  IF N_ELEMENTS(x) LT 2 THEN set_zero = 1
  IF N_ELEMENTS(v) LT 2 THEN set_zero = 1

;+++++++++++++++++++++++++++++
; check keyword over / plot config
  IF NOT KEYWORD_SET(OVER) THEN over = 1
  
;+++++++++++++++++++++++++++++
; set_zero
  IF KEYWORD_SET(SET_ZERO) THEN BEGIN
    x = 0
    v = 0
    xrange = [0,1]
    yrange = [0,1]
    title = ''
    over = 0
  ENDIF
  
;+++++++++++++++++++++++++++++
; check keywords x- and yrange 
  IF NOT KEYWORD_SET(xrange) THEN $
    xrange = [MIN(x,/NAN), MAX(x,/NAN)]
    
  IF NOT KEYWORD_SET(yrange) THEN BEGIN
    offset = (MAX(v,/NAN)-MIN(v,/NAN))*0.05
    yrange = [MIN(v,/NAN)-offset, MAX(v,/NAN)+offset]
  ENDIF
  IF KEYWORD_SET(fix_xyrange) THEN BEGIN
    xrange = (p_obj1[0]).xrange
    yrange = (p_obj1[0]).yrange
  ENDIF 
;  IF KEYWORD_SET(fix_xyrange) THEN BEGIN
;    xrange = p_obj1.xrange
;    yrange = p_obj1.yrange
;  ENDIF
  
;+++++++++++++++++++++++++++++
; check if title defined
  IF NOT KEYWORD_SET(TITLE)  THEN title = ''
  
;+++++++++++++++++++++++++++++
; keyword set_colors: set to color array or keep default
  colors=['k','r','k','g','orange','r','b']
  IF KEYWORD_SET(set_colors) THEN colors[0:(N_ELEMENTS(set_colors)-1)]=set_colors[0:(N_ELEMENTS(set_colors)-1)]
  
;+++++++++++++++++++++++++++++
; check if x- and y-title defined
  IF KEYWORD_SET(XYTITLE) THEN BEGIN
    (p_obj1[0]).Xtitle=xytitle[0]
    (p_obj1[0]).Ytitle=xytitle[1]
  ENDIF


;+++++++++++++++++++++++++++++
; process OVER case

  CASE over OF
    0: $
      BEGIN
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[0]).SetData, x, v
        (p_obj1[1]).SetData, x, v
        (p_obj1[2]).SetData, x, v
        (p_obj1[3]).SetData, x, v
        (p_obj1[4]).SetData, x, v
        (p_obj1[5]).SetData, x, v
        (p_obj1[6]).SetData, x, v
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
  
    1: $
      BEGIN $ ; set data for pobj1, delete rest
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
;        (p_obj1[0]).title = title
;        (p_obj1[0]).color = 'black'
;        (p_obj1[0]).thick = 1
        (p_obj1[0]).xrange=xrange
        (p_obj1[0]).yrange=yrange
        (p_obj1[0]).Color = colors[0]
        (p_obj1[1]).Color = colors[1]
        (p_obj1[2]).Color = colors[2]
        (p_obj1[3]).Color = colors[3]
        (p_obj1[4]).Color = colors[4]
        (p_obj1[5]).Color = colors[5]
        (p_obj1[6]).Color = colors[6]
        (p_obj1[0]).SetData, x,v
        (p_obj1[1]).SetData, 0,0
        (p_obj1[2]).SetData, 0,0
        (p_obj1[3]).SetData, 0,0
        (p_obj1[4]).SetData, 0,0
        (p_obj1[5]).SetData, 0,0
        (p_obj1[6]).SetData, 0,0
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
  
    2: $
      BEGIN ; set data for pobj1a
        (p_obj1[1]).refresh, /DISABLE
;        (p_obj1[1]).color = 'red'
;        (p_obj1[1]).thick = 2
        (p_obj1[1]).xrange=xrange
        (p_obj1[1]).yrange=yrange
        (p_obj1[1]).title=title
        (p_obj1[1]).SetData, x,v
        (p_obj1[1]).refresh
      END
  
    3: $
      BEGIN ; set data for pobj1b
        (p_obj1[2]).refresh, /DISABLE
;        (p_obj1[2]).color = 'blue'
;        (p_obj1[2]).thick = 2
        (p_obj1[2]).xrange=xrange
        (p_obj1[2]).yrange=yrange
        (p_obj1[2]).SetData, x,v
        (p_obj1[2]).refresh
      END
  
    4: $
      BEGIN ; set data for pobj1c
        (p_obj1[3]).refresh, /DISABLE
;        (p_obj1[3]).color = 'green'
;        (p_obj1[3]).thick = 2
        (p_obj1[3]).xrange=xrange
        (p_obj1[3]).yrange=yrange
        (p_obj1[3]).SetData, x,v
        (p_obj1[3]).refresh
      END
  
    5: $
      BEGIN ; set data for pobj1d
        (p_obj1[4]).refresh, /DISABLE
;        (p_obj1[4]).color = 'orange'
;        (p_obj1[4]).thick = 2
        (p_obj1[4]).xrange=xrange
        (p_obj1[4]).yrange=yrange
        (p_obj1[4]).SetData, x,v
        (p_obj1[4]).refresh
      END
  
    6: $
      BEGIN ; set data for pobj1e
        (p_obj1[5]).refresh, /DISABLE
  ;      (p_obj1[5]).color = 'red'
  ;      (p_obj1[5]).linestyle= 2
  ;      (p_obj1[5]).thick = 3
        (p_obj1[5]).xrange=xrange
        (p_obj1[5]).yrange=yrange
        (p_obj1[5]).SetData, x,v
        (p_obj1[5]).refresh
      END
  
    7: $
      BEGIN ; set data for pobj1f
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[6]).color = 'blue'
        (p_obj1[6]).linestyle= 2
        (p_obj1[6]).thick = 3
        (p_obj1[6]).xrange=xrange
        (p_obj1[6]).yrange=yrange
        (p_obj1[6]).SetData, x,v
        (p_obj1[6]).refresh
      END
  
    123: $
      BEGIN
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[0]).xrange=xrange
        (p_obj1[0]).yrange=yrange
        (p_obj1[0]).Color = colors[0]
        (p_obj1[1]).Color = colors[1]
        (p_obj1[2]).Color = colors[2]
        (p_obj1[3]).Color = colors[3]
        (p_obj1[4]).Color = colors[4]
        (p_obj1[5]).Color = colors[5]
        (p_obj1[6]).Color = colors[6]
        (p_obj1[0]).SetData,  x, v
        (p_obj1[1]).SetData, x_1a, v_1a
        (p_obj1[2]).SetData, x_1b, v_1b
        (p_obj1[3]).SetData, 0,0
        (p_obj1[4]).SetData, 0,0
        (p_obj1[5]).SetData, 0,0
        (p_obj1[6]).SetData, 0,0
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
      
    134: $
      BEGIN
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[0]).xrange=xrange
        (p_obj1[0]).yrange=yrange
        (p_obj1[0]).Color = colors[0]
        (p_obj1[1]).Color = colors[1]
        (p_obj1[2]).Color = colors[2]
        (p_obj1[3]).Color = colors[3]
        (p_obj1[4]).Color = colors[4]
        (p_obj1[5]).Color = colors[5]
        (p_obj1[6]).Color = colors[6]
        (p_obj1[0]).SetData,  x, v
        (p_obj1[1]).SetData, 0,0
        (p_obj1[2]).SetData, x_1b, v_1b
        (p_obj1[3]).SetData, x_1c, v_1c
        (p_obj1[4]).SetData, 0,0
        (p_obj1[5]).SetData, 0,0
        (p_obj1[6]).SetData, 0,0
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
      
    2345: $
      BEGIN
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[0]).xrange=xrange
        (p_obj1[0]).yrange=yrange
        (p_obj1[0]).Color = colors[0]
        (p_obj1[1]).Color = colors[1]
        (p_obj1[2]).Color = colors[2]
        (p_obj1[3]).Color = colors[3]
        (p_obj1[4]).Color = colors[4]
        (p_obj1[5]).Color = colors[5]
        (p_obj1[6]).Color = colors[6]
        (p_obj1[0]).SetData,  x, v
        (p_obj1[1]).SetData, x_1a, v_1a
        (p_obj1[2]).SetData, x_1b, v_1b
        (p_obj1[3]).SetData, x_1c, v_1c
        (p_obj1[4]).SetData, x_1d, v_1d[0:N_ELEMENTS(x_1d)-1]
        (p_obj1[5]).SetData, 0,0
        (p_obj1[6]).SetData, 0,0
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
      
    12345: $
      BEGIN
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[0]).xrange=xrange
        (p_obj1[0]).yrange=yrange
        (p_obj1[0]).Color = colors[0]
        (p_obj1[1]).Color = colors[1]
        (p_obj1[2]).Color = colors[2]
        (p_obj1[3]).Color = colors[3]
        (p_obj1[4]).Color = colors[4]
        (p_obj1[5]).Color = colors[5]
        (p_obj1[6]).Color = colors[6]
        (p_obj1[0]).SetData,  x, v
        (p_obj1[1]).SetData, x_1a, v_1a
        (p_obj1[2]).SetData, x_1b, v_1b
        (p_obj1[3]).SetData, x_1c, v_1c
        (p_obj1[4]).SetData, x_1d, v_1d
        (p_obj1[5]).SetData, 0,0
        (p_obj1[6]).SetData, 0,0
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
      
    12367: $
      BEGIN
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[0]).xrange=xrange
        (p_obj1[0]).yrange=yrange
        (p_obj1[0]).Color = colors[0]
        (p_obj1[1]).Color = colors[1]
        (p_obj1[2]).Color = colors[2]
        (p_obj1[3]).Color = colors[3]
        (p_obj1[4]).Color = colors[4]
        (p_obj1[5]).Color = colors[5]
        (p_obj1[6]).Color = colors[6]
        (p_obj1[0]).SetData,  x, v
        (p_obj1[1]).SetData, x_1a, v_1a
        (p_obj1[1]).thick=2
        (p_obj1[2]).SetData, x_1b, v_1b
        (p_obj1[2]).thick=2
        (p_obj1[3]).SetData, 0,0
        (p_obj1[4]).SetData, 0,0
        (p_obj1[5]).SetData, x_1e, v_1e
        (p_obj1[5]).linestyle = 2
        (p_obj1[5]).thick=3
        (p_obj1[6]).SetData, x_1f, v_1f
        (p_obj1[6]).linestyle = 2
        (p_obj1[6]).thick=3
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
      
    123467: $
      BEGIN 
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[0]).xrange=xrange
        (p_obj1[0]).yrange=yrange
        (p_obj1[0]).Color = colors[0]
        (p_obj1[1]).Color = colors[1]
        (p_obj1[2]).Color = colors[2]
        (p_obj1[3]).Color = colors[3]
        (p_obj1[4]).Color = colors[4]
        (p_obj1[5]).Color = colors[5]
        (p_obj1[6]).Color = colors[6]
        (p_obj1[0]).SetData,  x, v
        (p_obj1[1]).SetData, x_1a, v_1a
        (p_obj1[2]).SetData, x_1b, v_1b
        (p_obj1[3]).SetData, x_1c, v_1c
        (p_obj1[4]).SetData, 0,0
        (p_obj1[5]).SetData, x_1e, v_1e
        (p_obj1[6]).SetData, x_1f, v_1f
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
      
    1234567 : $
      BEGIN
        (p_obj1[0]).refresh,  /DISABLE
        (p_obj1[1]).refresh, /DISABLE
        (p_obj1[2]).refresh, /DISABLE
        (p_obj1[3]).refresh, /DISABLE
        (p_obj1[4]).refresh, /DISABLE
        (p_obj1[5]).refresh, /DISABLE
        (p_obj1[6]).refresh, /DISABLE
        (p_obj1[0]).xrange=xrange
        (p_obj1[0]).yrange=yrange
        (p_obj1[0]).Color = colors[0]
        (p_obj1[1]).Color = colors[1]
        (p_obj1[2]).Color = colors[2]
        (p_obj1[3]).Color = colors[3]
        (p_obj1[4]).Color = colors[4]
        (p_obj1[5]).Color = colors[5]
        (p_obj1[6]).Color = colors[6]
        (p_obj1[0]).SetData,  x, v
        (p_obj1[1]).SetData, x_1a, v_1a
        (p_obj1[2]).SetData, x_1b, v_1b
        (p_obj1[3]).SetData, x_1c, v_1c
        (p_obj1[4]).SetData, x_1d, v_1d
        (p_obj1[5]).SetData, x_1e, v_1e
        (p_obj1[6]).SetData, x_1f, v_1f
        (p_obj1[0]).refresh
        (p_obj1[1]).refresh
        (p_obj1[2]).refresh
        (p_obj1[3]).refresh
        (p_obj1[4]).refresh
        (p_obj1[5]).refresh
        (p_obj1[6]).refresh
      END
  ENDCASE
;print, 'plot update: ', systime(1)-t
END