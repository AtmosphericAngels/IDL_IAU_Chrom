;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION read_agilent_cdf
; 
; MODIFICATIONS:
; originally named 'read_cdf', name changed to read_agilent_cdf to state difference to other cdf import functions
; 1705, FO: added def_file keyword to import function to override pickfile dialog.
;           added sort_by_jdate keyword to sort loaded files by measurement timestamp.
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION read_agilent_cdf, PATH=path, T_SCALE=t_scale, VERSION=version, DEF_FILE=def_file, SORT_BY_JDATE=sort_by_jdate, $
                           LOUD=loud

  IF NOT KEYWORD_SET(version) THEN version = '(not specified)'
  IF NOT KEYWORD_SET(sort_by_jdate) THEN sort_by_jdate = 0
  IF NOT KEYWORD_SET(loud) THEN loud = 0

  IF NOT KEYWORD_SET(t_scale) THEN t_scale = 'Seconds'         ; time scale default: seconds
  t_conv = 1.                                                  ; time conversion factor = 1 for default time scale (seconds)
  IF t_scale EQ 'Minutes' THEN t_conv = 60.                    ; time conversion factor = 60 if time scale is minutes
  
  
  IF NOT KEYWORD_SET(def_file) THEN $
    fname=DIALOG_PICKFILE(/MULTIPLE_FILES, PATH=path, filter='*.cdf', TITLE='Please select *.cdf file(s) to import.') $
      ELSE fname=def_file
      
  
  IF STRLEN(fname[0]) EQ 0 THEN BEGIN
     refd=create_refd()
   RETURN, refd
  ENDIF
  
  IF STRPOS(fname[0],' ') NE -1 THEN BEGIN
     msg=DIALOG_MESSAGE(fname[0]+STRING(13b)+' is not a valid filepath.'+STRING(13b)+'Whitespaces are not allowed in filepath.', /ERROR)
     refd=create_refd()
   RETURN, refd
  ENDIF
  
  chrom = []; initialize chrom as empty array
  FOR i=0, N_ELEMENTS(fname)-1 DO BEGIN 
    refd = create_refd()
    ncdfstr = cdf2idl_struct(fname[i])  
    refd.fname = fname[i]
;      print, ncdfstr.experiment_date_time_stamp
    yy      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),0,4)
    mn      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),4,2)
    dd      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),6,2)
    hh      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),8,2)
    mm      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),10,2)
    ss      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),12,2)
    sign    = STRMID(STRING(ncdfstr.experiment_date_time_stamp),14,1) 
    refd.jdate = julday(mn,dd,yy,hh,mm,ss)
    
;    print, mn,dd,yy,'   ', hh,mm,ss

    V = DOUBLE(ncdfstr.scan_acquisition_time)         ; input vector / conversion to float; normally double 
    X = ncdfstr.scan_index                            ; x-axis values for V, regular ascending
    U = DINDGEN(N_ELEMENTS(ncdfstr.intensity_values)) ; x-axis values for the result
    *refd.time = (interpol(V,X,U))/t_conv
 
    *refd.mass = ncdfstr.mass_values ;ROUND(ncdfstr.mass_values)

    *refd.intensity = ncdfstr.intensity_values
    
    refd.t_scale = t_scale
    refd.iauchrom_vers = version

    chrom=[chrom,refd]
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