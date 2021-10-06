;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO check_pobjects
;
; AUTHOR:
; F.Obersteiner, Nov.2013. Last modified Mar 2015.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO check_pobjects, P_OBJ=p_obj ; p_obj: ['p_obj0', 'p_obj1']

  IF NOT KEYWORD_SET(p_obj) THEN RETURN
  IF SIZE(p_obj, /TYPE) NE 7 THEN RETURN

  COMMON COM_PLOT
  COMMON DATA

  DEVICE, Get_Screen_Size=screenSize
  XCenter = FIX(ScreenSize[0])
  YCenter = FIX(ScreenSize[1])

  IF p_obj[0] EQ 'p_obj0' THEN BEGIN
    IF OBJ_VALID(p_obj0[0]) EQ 0 THEN BEGIN ; pobj0 was closed, restore
      col_p0=['k','r','g','b','deep_sky_blue','m','gold','k','k','k']
      linestyle_p0=[0,0,0,0,0,0,0,0,0,0]
      thick_p0=[1,1,1,1,1,1,1,1,1,1]
      p_obj0[0]=plot([0.,0.], XTITLE = '$t_{R}$', YTITLE = 'Intensity', COLOR=col_p0[0], THICK=1, LINESTYLE=0, $
                     WINDOW_TITLE='IAU_Chrom_v'+STRCOMPRESS(STRING(version, FORMAT='(F4.2)'), /REMOVE_ALL)+': Plot0', $
                     LOCATION=[0.5*screenSize[0], 0*screenSize[1]], dimensions=[0.7*screensize[0],0.45*screensize[1]], $
                     MARGIN=[0.1,0.09,0.03,0.12])
      FOR i=1, N_ELEMENTS(p_obj0)-1 DO p_obj0[i]=plot([0.,0.], COLOR=col_p0[i], THICK=thick_p0[i], LINESTYLE=linestyle_p0[i], /OVERPLOT)

      IF SIZE(chrom, /TYPE) EQ 8 THEN BEGIN ; set x and ytitle if chrom defined
        IF chrom[0].t_scale EQ 'Seconds' THEN xtitle = '$t_{R} [s]$' ELSE xtitle='$t_{R} [min]$'
        CASE chrom[0].instr_type OF
          0: ytitle = 'Intensity'
          3: ytitle = 'Intensity [ions/extr.]'
          4: ytitle = 'Intensity [mV]'
;          0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD
          ELSE: ytitle = 'Intensity [cts]'
        ENDCASE
        p_obj0[0].xtitle = xtitle
        p_obj0[0].ytitle = ytitle
      ENDIF

      textfsize_p0 = [12., 12., 12., 12., 12., 12., 12., 12., 12.]
      p_obj0_txt[0] = text(0.02,0.950, '', TARGET=p_obj0[0], FONT_SIZE=textfsize_p0[0])
      p_obj0_txt[1] = text(0.26,0.950, '', TARGET=p_obj0[1], FONT_SIZE=textfsize_p0[1])
      p_obj0_txt[2] = text(0.52,0.950, '', TARGET=p_obj0[2], FONT_SIZE=textfsize_p0[2])
      p_obj0_txt[3] = text(0.02,0.915, '', TARGET=p_obj0[3], FONT_SIZE=textfsize_p0[3])
      p_obj0_txt[4] = text(0.26,0.915, '', TARGET=p_obj0[4], FONT_SIZE=textfsize_p0[4])
      p_obj0_txt[5] = text(0.52,0.915, '', TARGET=p_obj0[5], FONT_SIZE=textfsize_p0[5])
      p_obj0_txt[6] = text(0.77,0.915, '', TARGET=p_obj0[6], FONT_SIZE=textfsize_p0[6])
      p_obj0_txt[7] = text(0.77,0.950, '', TARGET=p_obj0[7], FONT_SIZE=textfsize_p0[7])
      p_obj0_txt[8] = text(0.10,0.015, '', TARGET=p_obj0[8], FONT_SIZE=textfsize_p0[8], FONT_STYLE='bf')
    ENDIF
  ENDIF

  IF p_obj[1] EQ 'p_obj1' THEN BEGIN
    IF OBJ_VALID(p_obj1[0]) EQ 0 THEN BEGIN ; pobj1 was closed, restore (see def_common)
      col_p1=['k','r','g','b','orange','r','b','k','k','k']
      linestyle_p1=[0,0,0,0,0,0,2,2,0,0]
      thick_p1=[1,2,2,1,3,3,1,1,1,1]
      p_obj1[0]=plot([0,0], XTITLE='$t_{R}$', YTITLE='Intensity', COLOR=col_p1[0], THICK=1, LINESTYLE=0, $
                     WINDOW_TITLE='IAU_Chrom_v'+STRCOMPRESS(STRING(version, FORMAT='(F4.2)'), /REMOVE_ALL)+': Plot1', $
                     LOCATION=[0.5*screenSize[0], 0.5*screenSize[1]], dimensions=[0.7*screensize[0],0.45*screensize[1]], $
                     MARGIN=[0.1,0.09,0.03,0.12])
      FOR i=1, N_ELEMENTS(p_obj1)-1 DO p_obj1[i]=plot([0.,0.], COLOR=col_p1[i], THICK=thick_p1[i], LINESTYLE=linestyle_p1[i], /OVERPLOT)

      IF SIZE(chrom, /TYPE) EQ 8 THEN BEGIN ; set x and ytitle if chrom defined
        IF chrom[0].t_scale EQ 'Seconds' THEN xtitle = '$t_{R} [s]$' ELSE xtitle='$t_{R} [min]$'
        CASE chrom[0].instr_type OF
          0: ytitle = 'Intensity'
          3: ytitle = 'Intensity [ions/extr.]'
          4: ytitle = 'Intensity [mV]'
          ELSE: ytitle = 'Intensity [cts]'
;          0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD
        ENDCASE
        p_obj1[0].xtitle = xtitle
        p_obj1[0].ytitle = ytitle
      ENDIF

      textfsize_p1 = [12., 12., 12., 12., 12., 12., 12.]
      p_obj1_txt[0] = text(0.10, 0.94, '', TARGET=p_obj1[0], FONT_SIZE=textfsize_p1[0])
      p_obj1_txt[1] = text(0.50, 0.90, '', TARGET=p_obj1[1], FONT_SIZE=textfsize_p1[1])
      p_obj1_txt[2] = text(0.10, 0.90, '', TARGET=p_obj1[2], FONT_SIZE=textfsize_p1[2])
      p_obj1_txt[3] = text(0.50, 0.94, '', TARGET=p_obj1[3], FONT_SIZE=textfsize_p1[3])
      p_obj1_txt[4] = text(0.50, 0.94, '', TARGET=p_obj1[4], FONT_SIZE=textfsize_p1[4])
      p_obj1_txt[5] = text(0.50, 0.94, '', TARGET=p_obj1[5], FONT_SIZE=textfsize_p1[5])
      p_obj1_txt[6] = text(0.50, 0.94, '', TARGET=p_obj1[6], FONT_SIZE=textfsize_p1[6])

    ENDIF
  ENDIF

END