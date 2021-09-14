;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION read_ecd_fid_cdf
;
; INFO:
; F.Obersteiner 2017-04, imports ECD and FID data from GHG-GC, MPI Mainz.
;
; MODIFICATIONS:
; 1705, FO: added def_file keyword to import function to override pickfile dialog.
;           added sort_by_jdate keyword to sort loaded files by measurement timestamp.
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION read_ecd_fid_cdf, PATH=path, T_SCALE=t_scale, VERSION=version, DEF_FILE=def_file, SORT_BY_JDATE=sort_by_jdate, $
                           LOUD=loud

  IF NOT KEYWORD_SET(version) THEN version = 'not specified'
  IF NOT KEYWORD_SET(sort_by_jdate) THEN sort_by_jdate = 0
  IF NOT KEYWORD_SET(loud) THEN loud = 0

  IF NOT KEYWORD_SET(t_scale) THEN t_scale = 'Seconds'         ; time scale default: seconds
  IF t_scale EQ 'Minutes' THEN t_conv = 60. ELSE t_conv = 1.   ; time conversion factor = 60 if time scale is minutes else 1 for seconds


  filters = ['*.cdf', '*.nc']
  IF NOT KEYWORD_SET(def_file) THEN $
    fname=DIALOG_PICKFILE(/MULTIPLE_FILES, PATH=path, filter=filters, TITLE='Please select netCDF file(s) to import.') $
      ELSE fname=def_file

  IF STRLEN(fname[0]) EQ 0 THEN RETURN, create_refd()

  chrom = []
  FOR i=0, N_ELEMENTS(fname)-1 DO BEGIN
    refd = create_refd()
    ncdfstr = cdf2idl_struct(fname[i])

    refd.fname = fname[i]
    refd.info_str = 'S_NAME:' + STRING(9B) + ncdfstr.sample_name + STRING(9B) + $
                    'OPERATOR:' + STRING(9B) + ncdfstr.operator


    file_create = ncdfstr.ch_file_created
    yy=STRMID(file_create,6,4)
    mn=STRMID(file_create,3,2)
    dd=STRMID(file_create,0,2)
    hh=STRMID(file_create,11,2)
    mm=STRMID(file_create,14,2)
    ss=STRMID(file_create,17,2)

;    cs_start = ncdfstr.chemstation_start
;    yy=STRMID(cs_start,6,4)
;    mn=STRMID(cs_start,3,2)
;    dd=STRMID(cs_start,0,2)
;    hh=STRMID(cs_start,11,2)
;    mm=STRMID(cs_start,14,2)
;    ss=STRMID(cs_start,17,2)

    refd.jdate = julday(mn,dd,yy,hh,mm,ss)

    xdata=[0D,ncdfstr.ydata_t_max]
    *refd.time = interpol(xdata*60., N_ELEMENTS(ncdfstr.ydata))/t_conv

    *refd.mass = MAKE_ARRAY(N_ELEMENTS(ncdfstr.ydata), /FLOAT, VALUE=0.)

    *refd.intensity = ncdfstr.ydata

    refd.t_scale = t_scale

    refd.iauchrom_vers = version

    chrom=[chrom,refd]

;    print, strmid(ncdfstr.chemstation_start, 0, 19)
;    print, i+1
;    print, file_basename(fname[i])
;    print, ncdfstr.sample_name

  ENDFOR

  ; sort chrom structure by jdate vector...
  sort_ix = SORT(chrom.jdate)
  w = WHERE(sort_ix EQ INDGEN(i), nvd)

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