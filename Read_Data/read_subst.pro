;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION read_subst
;
; INFO:
; imports MSINFO txt file
;
; MODIFICATIONS:
; - initial version by S.Sala
; - Jun 2014: structural changes and introduction of noise_win (FO)
; - Mar 2015: introduction of keyword 'use_nom' (FO)
; - May 2017: added keyword DEF_FILE to override the pickfile dialog and supply a full file path
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION read_subst, PATH=path, FILTER=filter, USE_NOM=use_nom, DEF_FILE=def_file

  IF NOT KEYWORD_SET(filter) THEN filter = '*.info'

  IF SIZE(subst, /TYPE) EQ 8 THEN tmp=TEMPORARY(subst) ; delete if subst already defined

  IF NOT KEYWORD_SET(def_file) THEN $
    fname=DIALOG_PICKFILE(FILE=filter, PATH=path, TITLE='Please select an MSINFO-File') $
      ELSE fname=def_file

  IF STRLEN(fname) EQ 0 THEN RETURN, create_refs() ; no file selected / aborted

;+++++++++++++++++++++++
; pre-define variables empty
  header = ''
  n_row = 0L
  n_col = 0L
  line = ''
  counter = 0L

  OPENR, lun, fname, /get_lun
  READF, lun, header

    n_col = n_elements(strsplit(strtrim(strcompress(header),2),' ',/extract))
    IF n_col EQ 27 THEN counter = 2 ; 27 columns: new msinfo with two additional columns for noise left and right border
    n_row = file_lines(fname)

  subst = replicate(create_refs(), n_row-1)

    FOR row=0L, n_row-2 DO BEGIN

      readf, lun, line
      if strlen(strcompress(line, /REMOVE_ALL)) eq 0 then break

      bval = byte(string('"'))
      lval = byte(line)
      pos  = where(lval NE bval[0])
      line = string(lval[pos])

      tmp = strsplit(STRTRIM(strcompress(line),2),' ',/extract)

      subst[row].name    = tmp[0]
      subst[row].formula = tmp[1]
      subst[row].rt      = float(tmp[2])
      subst[row].rt_win  = float([tmp[3],tmp[4]])
      subst[row].method    = tmp[5]
      subst[row].int_win   = float([tmp[6],tmp[7]])

      IF n_col EQ 27 THEN subst[row].noise_win = float([tmp[8],tmp[9]]) ; fill noise win values, else default NaN

      subst[row].bl_type   = FIX(tmp[8+counter], TYPE=2)
      subst[row].sigma     = FIX([(strsplit(tmp[9+counter],';',/extract))[0],(strsplit(tmp[9+counter],';',/extract))[1]], TYPE=4)
      subst[row].sel_peak  = tmp[10]
      subst[row].dif_peak  = FIX([(strsplit(tmp[11+counter],';',/extract))[0],(strsplit(tmp[11+counter],';',/extract))[1]], TYPE=4)
      subst[row].svgl      = FIX(tmp[12+counter], TYPE=4)
      subst[row].thresh    = FIX([(strsplit(tmp[13+counter],';',/extract))[0],(strsplit(tmp[13+counter],';',/extract))[1]], TYPE=4)
      subst[row].quant     = FIX(tmp[14+counter], TYPE=2)

      subst[row].mass    = fltarr(10)*!VALUES.F_NAN
      subst[row].rel_abd = fltarr(10)*!VALUES.F_NAN

      FOR col=(15+counter), N_ELEMENTS(tmp)-1 DO BEGIN
        IF KEYWORD_SET(use_nom) THEN mass = ROUND(FLOAT((strsplit(tmp[col],';',/extract))[0])) ELSE mass=FLOAT((strsplit(tmp[col],';',/extract))[0])
        IF mass LT 0 THEN mass = !Values.F_NAN
        subst[row].mass[col-(15+counter)] = mass
        subst[row].rel_abd[col-(15+counter)]=FLOAT((strsplit(tmp[col],';',/extract))[1])
      ENDFOR

    ENDFOR

  CLOSE, lun
  FREE_LUN, lun

  RETURN, subst

END