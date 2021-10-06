;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; intres2txt_sglsubst
;
; INFO:
; save results in individual text files, all chromatograms, one substance per file.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO intres2txt_sglsubst, chrom, sel_chrom, sel_name, PATH=path, ALL=all, NO_PICKFILE=no_pickfile

  fname = ''
  fpath = ''

  IF KEYWORD_SET(no_pickfile) THEN fpath = path ELSE $
    IF KEYWORD_SET(all) THEN $
      fpath = DIALOG_PICKFILE(PATH=path, /WRITE, /OVERWRITE_PROMPT, /DIRECTORY) ELSE $
        fname = DIALOG_PICKFILE(PATH=path, /WRITE, FILE=STRTRIM(STRCOMPRESS(chrom[0].subst[sel_name].name, /REMOVE_ALL))+'.txt')

  IF STRLEN(fname) EQ 0 AND STRLEN(fpath) EQ 0 THEN RETURN ; export aborted

  TAB = STRING(9B)
  header = ['File',TAB,'Date',TAB,'Time',TAB,'Substance',TAB,'Fragment_Mass',TAB,'Peak_Height',TAB,'Peak_Area',TAB,'',TAB,'Noise',TAB,'Comment']
  IF chrom[0].t_scale EQ 'Seconds' THEN header[14] = 'RT[s]' ELSE header[14] = 'RT[min]'

; ***
; ***
; ***
  IF KEYWORD_SET(all) THEN BEGIN ; *** all substances to individual txt files
    n_chrom = N_ELEMENTS(chrom.fname)
  ;  IF n_chrom GE 4 THEN ix = n_chrom*(1./3.) ELSE ix = 0
    ix = sel_chrom
      FOR sel_name=0, N_ELEMENTS(chrom[ix].subst.name)-1 DO BEGIN

          prefix = 'nd_'
          IF FINITE(chrom[ix].subst[sel_name].ires.rt) THEN $
            prefix=STRCOMPRESS(STRING(chrom[ix].subst[sel_name].ires.rt, FORMAT='(F12.2)'), /REMOVE_ALL)+'_'    ; determine txt file prefix: rt from ires.
            IF prefix EQ 'nd_' AND FINITE(chrom[ix].subst[sel_name].rt) THEN $
              prefix=STRCOMPRESS(STRING(chrom[ix].subst[sel_name].rt, FORMAT='(F12.1)'), /REMOVE_ALL)+'_'   ; requires correct rt in msinfo!

        suffix1='_'+STRCOMPRESS(STRING(ROUND(chrom[ix].subst[sel_name].mass[chrom[ix].subst[sel_name].quant]), FORMAT='(I)'), /REMOVE_ALL)
        suffix2='_'+jultime2timestring(SYSTIME(/JULIAN), /YMD_CLEAN)

        fname=fpath+STRTRIM(prefix+STRCOMPRESS(chrom[ix].subst[sel_name].name, /REMOVE_ALL)+suffix1+suffix2+'.txt')

        OPENW, lun, fname, /get_lun
        PRINTF, lun, header, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
        FOR i=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN
          caldat, chrom[i].jdate, mm,dd,yy,hh,mn,ss
          chromfname= STRING(FILE_BASENAME(chrom[i].fname))
          date      = STRTRIM(STRCOMPRESS(STRING((dd), FORMAT="(I02)")),2)+'.'+STRTRIM(STRCOMPRESS(STRING((mm), FORMAT="(I02)")),2)+$
                              '.'+STRTRIM(STRCOMPRESS(STRING((yy), FORMAT="(I04)")),2)
          time      = STRTRIM(STRCOMPRESS(STRING((hh), FORMAT="(I02)")),2)+':'+STRTRIM(STRCOMPRESS(STRING((mn), FORMAT="(I02)")),2)+$
                              ':'+STRTRIM(STRCOMPRESS(STRING((ss), FORMAT="(I02)")),2)
          substance = STRCOMPRESS(STRING(chrom[i].subst[sel_name].name), /REMOVE_ALL)
          mass      = STRCOMPRESS(STRING(chrom[i].subst[sel_name].mass[chrom[i].subst[sel_name].quant], FORMAT='(F12.4)'), /REMOVE_ALL)
          height    = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.height, FORMAT='(D25.8)'), /REMOVE_ALL)
          area      = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.area, FORMAT='(D25.8)'), /REMOVE_ALL)
  ;        width     = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.width), /REMOVE_ALL)
          rt        = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.rt), /REMOVE_ALL)
          noise     = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.noise[chrom[i].subst[sel_name].quant]), /REMOVE_ALL)
          comment   = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.comment))+' ('+STRCOMPRESS(STRING(chrom[i].subst[sel_name].method))+')'
          PRINTF, lun, chromfname, TAB, date, TAB, time, TAB, substance, TAB, mass, TAB, height, TAB, area, TAB, $                                    ; TAB, width,
                       rt, TAB, noise, TAB, comment, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'                                           ; A,A,
        ENDFOR
        CLOSE, lun
        FREE_LUN, lun
      ENDFOR

; ***
; ***
; ***
  ENDIF ELSE BEGIN ; *** single substance to individual txt file

    OPENW, lun, fname, /GET_LUN
    PRINTF, lun, header, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
      FOR i=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN
        caldat, chrom[i].jdate, mm,dd,yy,hh,mn,ss
        chromfname= STRING(FILE_BASENAME(chrom[i].fname))
        date      = STRTRIM(STRCOMPRESS(STRING((dd), FORMAT="(I02)")),2)+'.'+STRTRIM(STRCOMPRESS(STRING((mm), FORMAT="(I02)")),2)+$
               '.'+STRTRIM(STRCOMPRESS(STRING((yy), FORMAT="(I04)")),2)
        time      = STRTRIM(STRCOMPRESS(STRING((hh), FORMAT="(I02)")),2)+':'+STRTRIM(STRCOMPRESS(STRING((mn), FORMAT="(I02)")),2)+$
               ':'+STRTRIM(STRCOMPRESS(STRING((ss), FORMAT="(I02)")),2)
        substance = STRCOMPRESS(STRING(chrom[i].subst[sel_name].name), /REMOVE_ALL)
        mass      = STRCOMPRESS(STRING(chrom[i].subst[sel_name].mass[chrom[i].subst[sel_name].quant], FORMAT='(F12.4)'), /REMOVE_ALL)
        height    = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.height, FORMAT='(D25.8)'), /REMOVE_ALL)
        area      = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.area, FORMAT='(D25.8)'), /REMOVE_ALL)
;        width     = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.width), /REMOVE_ALL)
        rt        = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.rt), /REMOVE_ALL)
        noise     = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.noise[chrom[i].subst[sel_name].quant]), /REMOVE_ALL)
        comment   = STRCOMPRESS(STRING(chrom[i].subst[sel_name].ires.comment))+' ('+STRCOMPRESS(STRING(chrom[i].subst[sel_name].method))+')'
        PRINTF, lun, chromfname, TAB, date, TAB, time, TAB, substance, TAB, mass, TAB, height, TAB, area, TAB, rt, TAB, noise, TAB, comment, $
                     FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
      ENDFOR
    CLOSE, lun
    FREE_LUN, lun

  ENDELSE

  refr_status, message='report completed.'

END