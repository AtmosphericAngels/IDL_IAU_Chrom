;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION read_ecd_txt
;
; AUTHOR:
; S.Sala/T.Keber
;
; MODIFICATIONS:
; 1705, FO: added def_file keyword to import function to override pickfile dialog.
;           added sort_by_jdate keyword to sort loaded files by measurement timestamp.
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION read_ecd_txt, PATH=path, T_SCALE=t_scale, VERSION=version, DEF_FILE=def_file, SORT_BY_JDATE=sort_by_jdate, $
                       LOUD=loud

  IF NOT KEYWORD_SET(version) THEN version = 'not specified'
  IF NOT KEYWORD_SET(sort_by_jdate) THEN sort_by_jdate = 0
  IF NOT KEYWORD_SET(loud) THEN loud = 0

  IF NOT KEYWORD_SET(t_scale) THEN t_scale = 'Seconds'         ; time scale default: seconds
  IF t_scale EQ 'Minutes' THEN t_conv = 60. ELSE t_conv = 1.   ; time conversion factor = 60 if time scale is minutes else 1 for seconds


  IF NOT KEYWORD_SET(def_file) THEN $
    fname=DIALOG_PICKFILE(/MULTIPLE_FILES, PATH=path, filter='*.txt', TITLE='Please select *.txt file(s) to import.') $
      ELSE fname=def_file

  IF STRLEN(fname[0]) EQ 0 THEN RETURN create_refd()

  chrom = []
  FOR n=0, N_ELEMENTS(fname)-1 DO BEGIN
      nl = FILE_LINES(fname[n])
      nlhead = 11
      file_content_str = STRARR(nl)

      OPENR, lun, fname[n], /GET_LUN
      READF, lun, file_content_str
      FREE_LUN, lun

      headstr = file_content_str[0:nlhead-1]

;      IF (WHERE(headstr.contains('sample_name') EQ 1))[0] NE -1 THEN $
;        print, STRMID(headstr((WHERE(headstr.contains('sample_name') EQ 1))[0]), 13)

      IF (WHERE(headstr.contains('timestamp') EQ 1))[0] NE -1 THEN BEGIN; header contains tag "timestamp"...
        use_hdrtime = 1
        hdrtime = headstr[(WHERE(headstr.contains('timestamp') EQ 1))[0]]
        print, hdrtime, file_basename(fname[n])
        yy=STRMID(hdrtime,17,4)
        mn=STRMID(hdrtime,14,2)
        dd=STRMID(hdrtime,11,2)
        hh=STRMID(hdrtime,22,2)
        mm=STRMID(hdrtime,25,2)
        ss=STRMID(hdrtime,28,2)
      ENDIF ELSE use_hdrtime = 0

      datastr = STRCOMPRESS(file_content_str[nlhead:*])

;     ; HEADER
;     ; FLIGHT DATE: line 1+2
;     line=file_content_str(1)
;     date=strmid(STRTRIM(line,2),[0,4],[4,2])
;     date=float(date[0])+float(date[1])/12.
;     ; GLOBAL TROPOSPHERIC MEAN MIXING RATIO: line 3+4
;     line=file_content_str(3)
;     vsurf=float(line)
;     ; TOP OF ATMOSPHERE MIXING RATIO: line 5+6
;     line=file_content_str(5)
;     vtop=float(line)

      ; DATA
      vd = WHERE(datastr NE '' AND datastr NE ' ', NVD)
      data = DBLARR(2, NVD)
      READS, datastr, data

      ; Create REFD and Write CHROM
      refd = create_refd()
      refd.fname = fname[n]

      IF NOT KEYWORD_SET(use_hdrtime) THEN BEGIN
        yy=strmid(string(file_basename(fname[n])),00,4)
        mn=strmid(string(file_basename(fname[n])),04,2)
        dd=strmid(string(file_basename(fname[n])),06,2)
        hh=strmid(string(file_basename(fname[n])),09,2)
        mm=strmid(string(file_basename(fname[n])),11,2)
        ss=strmid(string(file_basename(fname[n])),13,2)
      ENDIF

      refd.jdate=julday(mn,dd,yy,hh,mm,ss)

      *refd.time=REFORM(data[0,*])/t_conv

      *refd.mass=INTARR(nvd)

      *refd.intensity=REFORM(data[1,*])

      refd.t_scale = t_scale
      refd.iauchrom_vers = version

      chrom=[chrom,refd]
   ENDFOR

  ; sort chrom structure by jdate vector...
  sort_ix = SORT(chrom.jdate)
  w = WHERE(sort_ix EQ INDGEN(n), nvd)

  IF KEYWORD_SET(sort_by_jdate) THEN chrom = chrom[sort_ix] $
    ELSE BEGIN
      IF nvd NE N_ELEMENTS(sort_ix) THEN $
        IF loud THEN BEGIN
          msg=[FILE_DIRNAME(fname[0]),'-> Sort files by measurement timestamp?']
          answ=DIALOG_MESSAGE(msg, /QUESTION)

          CASE answ OF
            'Yes' : chrom = chrom[sort_ix]
            'No'  :
          ENDCASE
        ENDIF
    ENDELSE

   RETURN, chrom

END