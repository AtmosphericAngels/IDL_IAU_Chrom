;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO refresh_text_pobj0
;
; AUTHOR:
; S.Sala, modifications F.Obersteiner
;
; INFO:
; specific for plot0 textfields.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO refresh_text_pobj0, CB_DATA_STRCT=cb_data_strct, CHROM=chrom, SUBST_REL_ABDS=subst_rel_abds, SET_ZERO=set_zero, $
                        SET_MANUAL=set_manual, SET_SUBTITLE=set_subtitle, RECREATE_TXT=recreate_txt, SET_COLORS=set_colors, $
                        TOT_UNIQM=tot_uniqm                        
  COMMON COM_PLOT
  
  n_textfields = 8
  textfontsize = 12.
  textcontent=STRARR(n_textfields)
  subtitle=''
  colors=['k','r','g','b','deep_sky_blue','m','gold','k','k']
  IF KEYWORD_SET(tot_uniqm) THEN BEGIN 
    peaktable=REPLICATE({mass:0., label:'nominal'}, N_ELEMENTS(tot_uniqm))
    peaktable[*].mass=tot_uniqm[*]
  ENDIF ELSE peaktable=0
  masslabel=''
  
  
  IF KEYWORD_SET(cb_data_strct)  THEN BEGIN
    IF (KEYWORD_SET(chrom) AND chrom[0].instr_type EQ 3) THEN peaktable=(*chrom[0].peaktable)
      FOR i=0, N_ELEMENTS(cb_data_strct)-1 DO BEGIN 
        IF cb_data_strct[i].plotIO EQ 0 THEN textcontent[i]=''  $
          ELSE BEGIN
            IF cb_data_strct[i].mass EQ -1 THEN textcontent[i]='TIC' ELSE BEGIN
              limit_dif=0.5
              ml_ixval=matchmass(peaktable.mass, cb_data_strct[i].mass, LIMIT_DIF=limit_dif)
              masslabel=' / '+peaktable[ml_ixval[1]].label
              textcontent[i]='m/Q_'+STRCOMPRESS(STRING(i), /REMOVE_ALL)+': '+ $
                              STRCOMPRESS(STRING(cb_data_strct[i].mass, FORMAT='(D14.4)'), /REMOVE_ALL)+masslabel
            ENDELSE
          ENDELSE                         
      ENDFOR
      IF KEYWORD_SET(chrom) THEN textcontent[n_textfields-1] = '['+STRING(FILE_BASENAME(chrom[cb_data_strct[i-1].sel_chrom].fname))+']'
  ENDIF
  
  
  IF KEYWORD_SET(subst_rel_abds)  THEN BEGIN ; Multi mass viewer, display rel abds from msinfo
    IF chrom[0].instr_type EQ 3 THEN peaktable=(*chrom[0].peaktable) ELSE peaktable=0 ; if HTOF data: de-reference peaktable
    FOR i=0, N_ELEMENTS(cb_data_strct)-1 DO BEGIN
      IF cb_data_strct[i].plotIO EQ 0 THEN textcontent[i]=''  $ ; plot set to false, do not display information
        ELSE BEGIN
          IF SIZE(peaktable, /TYPE) EQ 8 THEN BEGIN
            limit_dif=0.5
            ml_ixval=matchmass(peaktable.mass, cb_data_strct[i].mass, LIMIT_DIF=limit_dif)
            masslabel=peaktable[ml_ixval[1]].label+' / '
          ENDIF ELSE masslabel=''
          IF i EQ 0 THEN subst_rel_abds[i] = 1000.
          textcontent[i]='m/Q_'+STRCOMPRESS(STRING(i), /REMOVE_ALL)+': '+STRCOMPRESS(STRING(cb_data_strct[i].mass, FORMAT='(D14.4)'), /REMOVE_ALL)+ $
                ' / '+STRCOMPRESS(STRING(masslabel))+STRCOMPRESS(STRING(subst_rel_abds[i]/10., FORMAT='(F5.1)'), /REMOVE_ALL)+'%'
                
        ENDELSE
    ENDFOR
  ENDIF
  
  IF KEYWORD_SET(set_zero) EQ 1 THEN BEGIN
    set_subtitle=''
  ENDIF
  
  IF KEYWORD_SET(set_manual) EQ 1 THEN BEGIN
      FOR i=0, N_ELEMENTS(set_manual)-1 DO BEGIN
        IF set_manual[i] EQ 'none' THEN textcontent[i] = '' ELSE textcontent[i] = set_manual[i]
      ENDFOR
  ENDIF
  
  IF KEYWORD_SET(set_subtitle) EQ 1 THEN subtitle=set_subtitle
  
  IF KEYWORD_SET(set_colors) THEN colors[0:(N_ELEMENTS(set_colors)-1)]=set_colors[0:(N_ELEMENTS(set_colors)-1)] 
  
  ;+++++++++++++++++++++++++++++
  ; update textfields according to textcontent
    (p_obj0_txt[0]).refresh, /disable
    (p_obj0_txt[1]).refresh, /disable
    (p_obj0_txt[2]).refresh, /disable
    (p_obj0_txt[3]).refresh, /disable
    (p_obj0_txt[4]).refresh, /disable
    (p_obj0_txt[5]).refresh, /disable
    (p_obj0_txt[6]).refresh, /disable
    (p_obj0_txt[7]).refresh, /disable
    (p_obj0_txt[8]).refresh, /disable
    
    IF KEYWORD_SET(recreate_txt) THEN BEGIN ; should be called if size of plot 0 is changed, e.g. when using mmv or mcv.
      textcontent[0]=(p_obj0_txt[0]).string
      textcontent[1]=(p_obj0_txt[1]).string
      textcontent[2]=(p_obj0_txt[2]).string
      textcontent[3]=(p_obj0_txt[3]).string
      textcontent[4]=(p_obj0_txt[4]).string
      textcontent[5]=(p_obj0_txt[5]).string
      textcontent[6]=(p_obj0_txt[6]).string
      textcontent[7]=(p_obj0_txt[7]).string
      subtitle=(p_obj0_txt[8]).string
      (p_obj0_txt[0]).delete
      (p_obj0_txt[1]).delete
      (p_obj0_txt[2]).delete
      (p_obj0_txt[3]).delete
      (p_obj0_txt[4]).delete
      (p_obj0_txt[5]).delete
      (p_obj0_txt[6]).delete
      (p_obj0_txt[7]).delete
      (p_obj0_txt[8]).delete
      p_obj0_txt[0] = text(0.02,0.950, '', TARGET=p_obj0[0], FONT_SIZE=textfontsize)
      p_obj0_txt[1] = text(0.26,0.950, '', TARGET=p_obj0[1], FONT_SIZE=textfontsize)
      p_obj0_txt[2] = text(0.52,0.950, '', TARGET=p_obj0[2], FONT_SIZE=textfontsize)
      p_obj0_txt[3] = text(0.02,0.915, '', TARGET=p_obj0[3], FONT_SIZE=textfontsize)
      p_obj0_txt[4] = text(0.26,0.915, '', TARGET=p_obj0[4], FONT_SIZE=textfontsize)
      p_obj0_txt[5] = text(0.52,0.915, '', TARGET=p_obj0[5], FONT_SIZE=textfontsize)
      p_obj0_txt[6] = text(0.77,0.915, '', TARGET=p_obj0[6], FONT_SIZE=textfontsize)
      p_obj0_txt[7] = text(0.77,0.950, '', TARGET=p_obj0[7], FONT_SIZE=textfontsize)
      p_obj0_txt[8] = text(0.10,0.015, '', TARGET=p_obj0[8], FONT_SIZE=textfontsize, FONT_STYLE='bf')
    ENDIF
    
    (p_obj0_txt[0]).string=textcontent[0]
    (p_obj0_txt[1]).string=textcontent[1]
    (p_obj0_txt[2]).string=textcontent[2]
    (p_obj0_txt[3]).string=textcontent[3]
    (p_obj0_txt[4]).string=textcontent[4]
    (p_obj0_txt[5]).string=textcontent[5]
    (p_obj0_txt[6]).string=textcontent[6]
    (p_obj0_txt[7]).string=textcontent[7]
    (p_obj0_txt[8]).string=subtitle
    
    (p_obj0_txt[0]).color=colors[0]
    (p_obj0_txt[1]).color=colors[1]
    (p_obj0_txt[2]).color=colors[2]
    (p_obj0_txt[3]).color=colors[3]
    (p_obj0_txt[4]).color=colors[4]
    (p_obj0_txt[5]).color=colors[5]
    (p_obj0_txt[6]).color=colors[6]
    (p_obj0_txt[7]).color=colors[7]
    (p_obj0_txt[8]).color=colors[8]
    
    (p_obj0_txt[0]).refresh
    (p_obj0_txt[2]).refresh
    (p_obj0_txt[2]).refresh
    (p_obj0_txt[3]).refresh
    (p_obj0_txt[4]).refresh
    (p_obj0_txt[5]).refresh
    (p_obj0_txt[6]).refresh
    (p_obj0_txt[7]).refresh
    (p_obj0_txt[8]).refresh
    
END