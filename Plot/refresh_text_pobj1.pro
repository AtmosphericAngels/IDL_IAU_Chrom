;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO refresh_text_pobj1
;
; AUTHOR:
; S.Sala, modifications F.Obersteiner
;
; INFO:
; specific for plot1 textfields.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO refresh_text_pobj1, chrom, sel_chrom, sel_name, INT_MASS=int_mass, SET_COLORS=set_colors, $
                        RECREATE_TXT=recreate_txt, SET_ZERO=set_zero
  ;t=systime(1)
  COMMON COM_PLOT

  n_textfields = 7
  textfontsize = 12.
  textcontent = STRARR(n_textfields)
  colors = ['k','k','k','k','k','k','k']

  IF KEYWORD_SET(set_zero) EQ 1 THEN textcontent=textcontent $
    ELSE BEGIN
      textcontent[0] = 'File: '+FILE_BASENAME(chrom[sel_chrom].fname)
      textcontent[1] = 'Peak Area: '+STRTRIM(STRCOMPRESS(STRING((chrom[sel_chrom].subst[sel_name].ires.area),FORMAT = "(G12.4)")),2)+$
                       ' / Height: '+STRTRIM(STRCOMPRESS(STRING((chrom[sel_chrom].subst[sel_name].ires.height),FORMAT = "(G12.4)")),2)+$
                       ' / t_R: '+STRTRIM(STRCOMPRESS(STRING((chrom[sel_chrom].subst[sel_name].ires.rt), FORMAT = "(F10.3)")),2)

      IF NOT KEYWORD_SET(int_mass) THEN int_mass=(chrom[sel_chrom].subst[sel_name].mass[chrom[sel_chrom].subst[sel_name].quant]) $
        ELSE int_mass=int_mass

      textcontent[2] = chrom[sel_chrom].subst[sel_name].name+' on m/Q: '+$
                       STRTRIM(STRCOMPRESS(STRING(int_mass, FORMAT = "(D14.4)")),2)
      IF FINITE(chrom[sel_chrom].subst[sel_name].ires.noise[chrom[sel_chrom].subst[sel_name].quant]) EQ 0 $
        THEN textcontent[3] = '' $
          ELSE textcontent[3] = 'S/N: '+STRCOMPRESS(STRING((chrom[sel_chrom].subst[sel_name].ires.height / $
                                                   chrom[sel_chrom].subst[sel_name].ires.noise[chrom[sel_chrom].subst[sel_name].quant]), FORMAT='(D14.1)'), /REMOVE_ALL)

    ENDELSE

  IF KEYWORD_SET(set_colors) THEN colors[0:(N_ELEMENTS(set_colors)-1)]=set_colors[0:(N_ELEMENTS(set_colors)-1)]

;+++++++++++++++++++++++++++++
; update textfields according to textcontent
  (p_obj1_txt[0]).refresh, /disable
  (p_obj1_txt[1]).refresh, /disable
  (p_obj1_txt[2]).refresh, /disable
  (p_obj1_txt[3]).refresh, /disable
  (p_obj1_txt[4]).refresh, /disable
  (p_obj1_txt[5]).refresh, /disable
  (p_obj1_txt[6]).refresh, /disable

  IF KEYWORD_SET(recreate_txt) THEN BEGIN
    textcontent[0]=(p_obj1_txt[0]).string
    textcontent[1]=(p_obj1_txt[1]).string
    textcontent[2]=(p_obj1_txt[2]).string
    textcontent[3]=(p_obj1_txt[3]).string
    textcontent[4]=(p_obj1_txt[4]).string
    textcontent[5]=(p_obj1_txt[5]).string
    textcontent[6]=(p_obj1_txt[6]).string
    (p_obj1_txt[0]).delete
    (p_obj1_txt[1]).delete
    (p_obj1_txt[2]).delete
    (p_obj1_txt[3]).delete
    (p_obj1_txt[4]).delete
    (p_obj1_txt[5]).delete
    (p_obj1_txt[6]).delete
    p_obj1_txt[0] = text(0.10, 0.94, '', TARGET=p_obj1[0], FONT_SIZE=textfontsize)
    p_obj1_txt[1] = text(0.50, 0.90, '', TARGET=p_obj1[1], FONT_SIZE=textfontsize)
    p_obj1_txt[2] = text(0.10, 0.90, '', TARGET=p_obj1[2], FONT_SIZE=textfontsize)
    p_obj1_txt[3] = text(0.50, 0.94, '', TARGET=p_obj1[3], FONT_SIZE=textfontsize)
    p_obj1_txt[4] = text(0.50, 0.94, '', TARGET=p_obj1[4], FONT_SIZE=textfontsize)
    p_obj1_txt[5] = text(0.50, 0.94, '', TARGET=p_obj1[5], FONT_SIZE=textfontsize)
    p_obj1_txt[6] = text(0.50, 0.94, '', TARGET=p_obj1[6], FONT_SIZE=textfontsize)
  ENDIF

  (p_obj1_txt[0]).string=textcontent[0]
  (p_obj1_txt[1]).string=textcontent[1]
  (p_obj1_txt[2]).string=textcontent[2]
  (p_obj1_txt[3]).string=textcontent[3]
  (p_obj1_txt[4]).string=textcontent[4]
  (p_obj1_txt[5]).string=textcontent[5]
  (p_obj1_txt[6]).string=textcontent[6]

  (p_obj1_txt[0]).color=colors[0]
  (p_obj1_txt[1]).color=colors[1]
  (p_obj1_txt[2]).color=colors[2]
  (p_obj1_txt[3]).color=colors[3]
  (p_obj1_txt[4]).color=colors[4]
  (p_obj1_txt[5]).color=colors[5]
  (p_obj1_txt[6]).color=colors[6]

  (p_obj1_txt[0]).refresh
  (p_obj1_txt[2]).refresh
  (p_obj1_txt[2]).refresh
  (p_obj1_txt[3]).refresh
  (p_obj1_txt[4]).refresh
  (p_obj1_txt[5]).refresh
  (p_obj1_txt[6]).refresh

  ;print, 'textfields update: ', systime(1)-t
;  print, chrom[sel_chrom].subst[sel_name].ires.height/chrom[sel_chrom].subst[sel_name].ires.noise[chrom[sel_chrom].subst[sel_name].quant]
END