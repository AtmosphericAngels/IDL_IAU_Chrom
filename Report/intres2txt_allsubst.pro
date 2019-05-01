;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; intres2txt_allsubst
;
; INFO:
; save results in a single text file (all chromatograms, all substances)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO intres2txt_allsubst, chrom, PATH=path

  date=conc_date(chrom[0].jdate, systime(/julian))
  fname=DIALOG_PICKFILE(file='allintres_'+date, /OVERWRITE_PROMPT, default_extension='txt', /WRITE, PATH=path)

  IF STRLEN(fname) EQ 0 THEN RETURN

  TAB=STRING(9B)
  header=['File',TAB,'Date',TAB,'Time',TAB,'Substance',TAB,'Fragment_Mass',TAB,'Peak_Height',TAB,'Peak_Area',TAB,'',TAB,'Noise',TAB,'Comment']
  IF chrom[0].t_scale EQ 'Seconds' THEN header[14]='RT[s]' ELSE header[14]='RT[min]'; timescale given, set header

  ;IF chrom[0].iauchrom_vers EQ '(not specified)' THEN iauchrom_vers='IAU_CHROM' ELSE iauchrom_vers='IAU_CHROM_v'+chrom[0].iauchrom_vers

  OPENW, lun, fname, /get_lun
  ;PRINTF, lun, '***'
  ;PRINTF, lun,  'Results generated with:'
  ;PRINTF, lun, iauchrom_vers
  ;PRINTF, lun, '***'

  FOR i=0, N_ELEMENTS(chrom[0].subst.name)-1 DO BEGIN
    subst= '*** '+STRCOMPRESS(STRING(chrom[0].subst[i].name), /REMOVE_ALL)+' ***'

    IF i GT 0 THEN PRINTF, lun, '', FORMAT='(A)'
    PRINTF, lun, subst, FORMAT='(A)'
    PRINTF, lun, header, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'

      FOR j=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN

        caldat, chrom[j].jdate, mm,dd,yy,hh,mn,ss

        file = STRING(FILE_BASENAME(chrom[j].fname))
        date = STRTRIM(STRCOMPRESS(STRING((dd), FORMAT="(I02)")),2)+'.'+STRTRIM(STRCOMPRESS(STRING((mm), FORMAT="(I02)")),2)+$
                                                        '.'+STRTRIM(STRCOMPRESS(STRING((yy), FORMAT="(I04)")),2)
        time = STRTRIM(STRCOMPRESS(STRING((hh), FORMAT="(I02)")),2)+':'+STRTRIM(STRCOMPRESS(STRING((mn), FORMAT="(I02)")),2)+$
                                                        ':'+STRTRIM(STRCOMPRESS(STRING((ss), FORMAT="(I02)")),2)
        substance = STRCOMPRESS(STRING(chrom[j].subst[i].name), /REMOVE_ALL)
        mass = STRCOMPRESS(STRING(chrom[j].subst[i].mass[chrom[j].subst[i].quant]), /REMOVE_ALL)
        height = STRCOMPRESS(STRING(chrom[j].subst[i].ires.height), /REMOVE_ALL)
        area = STRCOMPRESS(STRING(chrom[j].subst[i].ires.area), /REMOVE_ALL)
        rt = STRCOMPRESS(STRING(chrom[j].subst[i].ires.rt), /REMOVE_ALL)
        noise = STRCOMPRESS(STRING(chrom[j].subst[i].ires.noise[chrom[j].subst[i].quant]), /REMOVE_ALL)
        comment = STRCOMPRESS(STRING(chrom[j].subst[i].ires.comment))+' ('+STRCOMPRESS(STRING(chrom[j].subst[i].method))+')'

        PRINTF, lun, file, TAB, date, TAB, time, TAB, substance, TAB, mass, TAB, height, TAB, area, TAB, rt, TAB, noise, TAB, comment, $
                     FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
    ENDFOR

  ENDFOR

  CLOSE, lun
  FREE_LUN, lun

END