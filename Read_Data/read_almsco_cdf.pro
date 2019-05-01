;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION read_almsco_cdf
;
; MODIFICATIONS:
; introduced Nov 2013, H.Boenisch, F.Obersteiner
; 1705, FO: added def_file keyword to import function to override pickfile dialog.
;           added sort_by_jdate keyword to sort loaded files by measurement timestamp.
; 1803, FO: added check for double scan index values
;
; INFO:
; based on read_agilent_cdf, does not use interpol, generates refd.time by replicating ncdfstr.scan_acquisition_time
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION read_almsco_cdf, PATH=path, T_SCALE=t_scale, VERSION=version, DEF_FILE=def_file, SORT_BY_JDATE=sort_by_jdate, $
                          LOUD=loud

  IF NOT KEYWORD_SET(version) THEN version = '(not specified)'
  IF NOT KEYWORD_SET(sort_by_jdate) THEN sort_by_jdate = 0
  IF NOT KEYWORD_SET(loud) THEN loud = 0

  IF NOT KEYWORD_SET(t_scale) THEN t_scale = 'Seconds'         ; time scale default: seconds
  t_conv = 1D                                                 ; time conversion factor = 1 for default time scale (seconds)
  IF t_scale EQ 'Minutes' THEN t_conv = 60D                    ; time conversion factor = 60 if time scale is minutes


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

    yy      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),0,4)
    mn      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),4,2)
    dd      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),6,2)
    hh      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),8,2)
    mm      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),10,2)
    ss      = STRMID(STRING(ncdfstr.experiment_date_time_stamp),12,2)
    sign    = STRMID(STRING(ncdfstr.experiment_date_time_stamp),14,1)
    refd.jdate = julday(mn,dd,yy,hh,mm,ss)


;   *refd.mass = round(ncdfstr.mass_values)

   *refd.intensity = DBLARR(N_ELEMENTS(ncdfstr.intensity_values))*!Values.D_NAN

   *refd.mass = fltarr(n_elements(ncdfstr.mass_values))
   rmv = round(ncdfstr.mass_values)
   dm = rmv-ncdfstr.mass_values                  ; difference to integer value: dm > 0 eqs. rounded up / dm < 0 eqs. rounded down
   m_int = 0.485           ; intervall around nom.mass (+/-)

   w0 = where(abs(dm) LE m_int)   ; intervall around nom.mass +/- m_int
   w1 = where(dm GT m_int)        ; intervall nom.mass + m_int to + .5
   w2 = where(dm LT -m_int)       ; intervall nom.mass - m_int to - .5

   (*refd.mass)[w2] = rmv[w2]+0.5 ; dedicate value to rounded mass - .5 (integer came from round down)
   (*refd.mass)[w1] = rmv[w1]-0.5 ; dedicate value to rounded mass + .5 (integer came from round up)
   (*refd.mass)[w0] = rmv[w0]     ; dedicate rounded value within interval to nominal mass value


 ; diagnose if mass doubles exist
 ; ***
;      rm=(*refd.mass)
;      ivd=0L
;      print, fname[i]
;      for m=1L, n_elements(rm)-1 do begin
;        if rm[m-1] eq rm[m] and rm[m] lt 200. then begin ;
;          if round(rm[m]) eq rm[m] then print, 'non-.5 ! -> '
;          print,rm[m-1], rm[m], rm[m+1], ' // ', ncdfstr.mass_values[m-1], ncdfstr.mass_values[m], ncdfstr.mass_values[m+1]
;          ivd = ivd + 1
;        endif
;      endfor
;      print, 'n doubles: ', ivd
 ; ***

    (*refd.intensity) = ncdfstr.intensity_values
    *refd.time = DBLARR(N_ELEMENTS(ncdfstr.intensity_values))*!Values.D_NAN ; generate one time point for every intensity value

    ; check for duplicate occurances of the same index
    si_sort = (ncdfstr.scan_index)[SORT(ncdfstr.scan_index)]
    si_unq = (ncdfstr.scan_index)[uniq(si_sort)]
    IF N_ELEMENTS(ncdfstr.scan_index) EQ N_ELEMENTS(si_unq) THEN BEGIN
      FOR j=0L, N_ELEMENTS(ncdfstr.scan_index)-2 DO $
        (*refd.time)[ncdfstr.scan_index[j]:ncdfstr.scan_index[j+1]-1] = ncdfstr.scan_acquisition_time[j]/t_conv
    ENDIF ELSE BEGIN ; duplicate indices: -1 will cause illegal subscript range (check takes 5 times longer)
      FOR j=0L, N_ELEMENTS(ncdfstr.scan_index)-2 DO $
        IF ncdfstr.scan_index[j] EQ ncdfstr.scan_index[j+1] THEN $ ; check for repeated occurances of the same scan index
          (*refd.time)[ncdfstr.scan_index[j]] = ncdfstr.scan_acquisition_time[j]/t_conv $
        ELSE (*refd.time)[ncdfstr.scan_index[j]:ncdfstr.scan_index[j+1]-1] = ncdfstr.scan_acquisition_time[j]/t_conv
      IF ncdfstr.scan_index[-1] LT N_ELEMENTS(ncdfstr.intensity_values) THEN $
        (*refd.time)[ncdfstr.scan_index[-1]:-1] = ncdfstr.scan_acquisition_time[-1]/t_conv
    ENDELSE

    w_i_vd = WHERE(ncdfstr.intensity_values GT 0.)
    (*refd.mass) = (*refd.mass)[w_i_vd]
    (*refd.intensity) = (*refd.intensity)[w_i_vd]
    (*refd.time) = (*refd.time)[w_i_vd]


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