;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; read_tofwerkh5
;
; INFO:
; created Nov 2013, F.Obersteiner & H.Boenisch
; Feb 14: added peaktable to refd/chrom.
; Sept 15: added TPS hsk data to refd/chrom.
; Oct 15: added option to call recalculation of peakdata and closing functions for hdf5 groups and datasets.
; 1705, FO: added def_file keyword to import function to override pickfile dialog.
;           added sort_by_jdate keyword to sort loaded files by measurement timestamp.
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------

FUNCTION h5r_peaktable, file, file_id

  group_id = H5G_OPEN(file_id, 'PeakData')
  dataset_id = H5D_OPEN(group_id, 'PeakTable')
  peaktable = H5D_READ(dataset_id)
    H5D_CLOSE, dataset_id
    H5G_CLOSE, group_id

  RETURN, peaktable
END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION tw_h5_filecheck, fname
;  ------------------------------------------------------------------------------------------------------------------------
;  +
;   :INFO: File check for Tofwerk hdf5 files. Checks if data in different groups has a size gt 0, i.e. exists and is
;          accessible.
;   :AUTHOR: F.Obersteiner, Feb 2016
;  -
;  ------------------------------------------------------------------------------------------------------------------------

  files_ok = BYTARR(N_ELEMENTS(fname))

  FOR i=0, N_ELEMENTS(fname)-1 DO BEGIN
    file_id = H5F_OPEN(fname[i])

    pt_group_id = H5G_OPEN(file_id, 'PeakData')
      pt_data_id = H5D_OPEN(pt_group_id, 'PeakTable')
        pt_size = H5D_GET_STORAGE_SIZE(pt_data_id)
        H5D_CLOSE, pt_data_id
        H5G_CLOSE, pt_group_id

    pd_group_id = H5G_OPEN(file_id, 'PeakData')
      peakdata_id = H5D_OPEN(pd_group_id, 'PeakData')
        pd_size = H5D_GET_STORAGE_SIZE(peakdata_id)
        H5D_CLOSE, peakdata_id
        H5G_CLOSE, pd_group_id

    fullspec_id = H5G_OPEN(file_id, 'FullSpectra')
      tofdata_id=H5D_OPEN(fullspec_id, 'TofData')
        spec_size = H5D_GET_STORAGE_SIZE(tofdata_id)
        H5D_CLOSE, tofdata_id
        H5G_CLOSE, fullspec_id

    t_group_id = H5G_OPEN(file_id, 'TimingData')
      timing_id = H5D_OPEN(t_group_id, 'BufTimes')
        timing_size = H5D_GET_STORAGE_SIZE(timing_id)
        H5D_CLOSE, timing_id
        H5G_CLOSE, t_group_id

    IF pt_size AND pd_size AND spec_size AND timing_size NE 0 THEN files_ok[i]=1 ;$

  ENDFOR

  RETURN, files_ok
END


;------------------------------------------------------------------------------------------------------------------------


FUNCTION h5_ptablecheck, FILE, PT_REF, PT_COMP=pt_com
;  ------------------------------------------------------------------------------------------------------------------------
;  +
;   :INFO: Peaktable check for Tofwerk hdf5 data. Checks if alias-masses are doubled and/or within mass integration borders.
;          Compares one peaktable to a reference-peaktable. Return value will be 1 if peaktable is ok, otherwise 0.
;   :AUTHOR: F.Obersteiner, Nov 2013 / modified Mar 2014
;  -
;  ------------------------------------------------------------------------------------------------------------------------
  result = 0. ; result initialized as float, returned as integer

  ; check .mass for doubles in reference peaktable
  w0=WHERE((pt_ref.mass)[0:-1]-(pt_ref.mass)[1:*] EQ 0)
  ;  print, (pt_ref.mass)[w0]
  IF TOTAL(w0) NE -1 THEN BEGIN
    msg = DIALOG_MESSAGE('Found doubled mass in peaktable. Import Aborted', /ERROR)
    PRINT, file
    PRINT, (pt_ref.mass)[w0]
    RETURN, result
  ENDIF
  ; pt_comp: check if .mass within .lower_integration_limit <> .upper_integration_limit
  w1=WHERE(pt_ref.mass LE pt_ref.lower_integration_limit)
  ;  print, (pt_ref.mass)[w1]
  w2=WHERE(pt_ref.mass GE pt_ref.upper_integration_limit)
  ;  print, (pt_ref.mass)[w2]
  IF w1+w2 NE -2 THEN msg = DIALOG_MESSAGE('Warning: Found mass(es) out of specified mass interval(s)!', /ERROR)

  IF KEYWORD_SET(pt_comp) THEN BEGIN
    result=0. ; reset result
    ; check .mass for doubles in comp. peaktable
    IF TOTAL(WHERE((pt_comp.mass)[0:-1]-(pt_comp.mass)[1:*] EQ 0)) NE -1 THEN BEGIN
      msg = DIALOG_MESSAGE('Found doubled mass in peaktable. Import Aborted', /ERROR)
      RETURN, result
    ENDIF
    ; pt_comp: check if .mass within .lower_integration_limit <> .upper_integration_limit
    w1=WHERE(pt_comp.mass LT pt_comp.lower_integration_limit)
    w2=WHERE(pt_comp.mass GT pt_comp.upper_integration_limit)
    IF w1+w2 NE -2 THEN msg = DIALOG_MESSAGE('Warning: Found mass(es) out of specified mass interval(s)!', /ERROR)

    ; check for equal peaktables (keyword pt_comp set)
    labeltest = STRCMP(pt_ref.label, pt_comp.label)
    IF PRODUCT(labeltest) LT 0.5 THEN result=result+0 ELSE result=result+0.25                   ; check labels
    IF TOTAL(pt_ref.mass) NE TOTAL(pt_comp.mass) THEN result=result+0 ELSE result=result+0.25   ; check masses
    IF TOTAL(pt_ref.lower_integration_limit) NE TOTAL(pt_comp.lower_integration_limit)  $       ; check lower integration limit
      THEN result=result+0 ELSE result=result+0.25
    IF TOTAL(pt_ref.upper_integration_limit) NE TOTAL(pt_comp.upper_integration_limit)  $       ; check upper integration limit
      THEN result=result+0 ELSE result=result+0.25
    IF result LT 0.9 THEN pt_equal = 0 ELSE pt_equal = 1

    result = pt_equal         ; keyword PT_COMP set, peaktables compared and all other tests passed, result = result of peaktable comparison
    IF result EQ 0 THEN msg=DIALOG_MESSAGE('Peaktables not equal. Import Aborted', /ERROR)
    RETURN, result

  ENDIF ELSE BEGIN
    result=1                    ; keyword PT_COMP not set and tests passed, return result=1
    RETURN, result
  ENDELSE

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION read_tofwerkh5, PATH=path, T_SCALE=t_scale, VERSION=version, PEAKDATA_RECALC=peakdata_recalc, $
                         USE_PT_LIMITS=use_pt_limits, VERBOSE=verbose, DEF_FILE=def_file, SORT_BY_JDATE=sort_by_jdate, $
                         LOUD=loud


  IF NOT KEYWORD_SET(version) THEN version = '(not specified)'
  IF NOT KEYWORD_SET(sort_by_jdate) THEN sort_by_jdate = 0
  IF NOT KEYWORD_SET(loud) THEN loud = 0


  IF NOT KEYWORD_SET(t_scale) THEN t_scale = 'Seconds'         ; time scale default: seconds
  t_conv = 1.                                                  ; time conversion factor = 1 for default time scale (seconds)
  IF t_scale EQ 'Minutes' THEN t_conv = 60.                    ; time conversion factor = 60 if time scale is minutes


  IF NOT KEYWORD_SET(def_file) THEN $
    fname=DIALOG_PICKFILE(/MULTIPLE_FILES, PATH=path, filter='*.h5', TITLE='Please select *.h5 file(s) to import.') $
      ELSE fname=def_file


  IF STRLEN(fname[0]) EQ 0 THEN BEGIN
   refd=create_refd()
   RETURN, refd                                                                ; abort if no files available
  ENDIF

  IF STRPOS(fname[0],' ') NE -1 THEN BEGIN
   msg=DIALOG_MESSAGE(fname[0]+' is not a valid filepath! Whitespaces are not allowed in filepath. File import aborted.', /ERROR)
   refd=create_refd()
   RETURN, refd
  ENDIF



  n_files = N_ELEMENTS(fname)
  chrom = []; initialize chrom as empty array

  IF KEYWORD_SET(verbose) THEN PRINT, 'Running file check...'
  files_ok = tw_h5_filecheck(fname)
  w_nvd = WHERE(files_ok NE 0)
  IF w_nvd NE -1 THEN BEGIN
    msg=DIALOG_MESSAGE('File(s) corrupted: '+FILE_BASENAME(fname[w_nvd]), /ERROR)
    refd=create_refd()
    RETURN, refd
  ENDIF

  pt_ref_file_id = H5F_OPEN(fname[0]); initialize reference peaktable
  pt_ref = h5r_peaktable(fname[0], pt_ref_file_id)

  IF KEYWORD_SET(verbose) THEN PRINT, 'Beginning file import...'
  FOR i=0, n_files-1 DO BEGIN

  IF KEYWORD_SET(verbose) THEN PRINT, 'Step ', STRCOMPRESS(STRING(i+1), /REMOVE_ALL), ' of ', $
                                      STRCOMPRESS(STRING(n_files), /REMOVE_ALL), ', ', $
                                      SYSTIME(0), ', importing peakdata from ', FILE_BASENAME(fname[i])

  file_id = H5F_OPEN(fname[i])                                                ; get hdf5 file id

; peaktable check
                                           ; set peaktable of first file as reference (all must be equal)
  pt_comp = h5r_peaktable(fname[i], file_id)
  pt_test = h5_ptablecheck(fname[i], pt_ref, PT_COMP=pt_comp)                         ; check if peaktables. test result will be 1 if peaktables ok

  IF pt_test EQ 0 THEN RETURN, chrom $
    ELSE BEGIN ; peaktable test passed, import chromatographic data

      refd = create_refd()                                                      ; generate data structure
      refd.fname = fname[i]                                                     ; WRITE FILENAME
      timestamp = H5A_READ(H5A_OPEN_NAME(file_id, 'HDF5 File Creation Time'))
      yy      = STRMID(STRING(timestamp),6,4)
      mn      = STRMID(STRING(timestamp),3,2)
      dd      = STRMID(STRING(timestamp),0,2)
      hh      = STRMID(STRING(timestamp),11,2)
      mm      = STRMID(STRING(timestamp),14,2)
      ss      = STRMID(STRING(timestamp),17,2)
      refd.jdate = JULDAY(mn,dd,yy,hh,mm,ss)                                    ; WRITE TIMESTAMP

      ; *** process peakdata
      pd_group_id = H5G_OPEN(file_id, 'PeakData')
        peakdata_id = H5D_OPEN(pd_group_id, 'PeakData')
          peakdata = H5D_READ(peakdata_id)  ; read peak data

      *refd.intensity = REFORM(peakdata, N_ELEMENTS(peakdata))                  ; WRITE INTENSITY

      ; *** get time data
      NbrBufs   = H5A_READ(H5A_OPEN_NAME(file_id, 'NbrBufs'))
      NbrWrites = H5A_READ(H5A_OPEN_NAME(file_id, 'NbrWrites'))
      t_group_id = H5G_OPEN(file_id, 'TimingData')
        timing_id = H5D_OPEN(t_group_id, 'BufTimes')
          t_axis = H5D_READ(timing_id)

      t_axis = REFORM(t_axis, N_ELEMENTS(t_axis))
      t_axis = t_axis[0:((NbrWrites*NbrBufs)-1)]                                ; coerce time axis length to write profil (writes times bufs)
      n_t = N_ELEMENTS(t_axis)
      n_mass = N_ELEMENTS(peakdata[*, 0, 0, 0])                                 ; get number of masses scanned
      time = FLTARR(n_mass*n_t)                                                 ; generate array for timing data, length n_mass * n_t
      mass = FLTARR(n_mass*n_t)
        FOR j=0, n_t-1 DO BEGIN
          time[n_mass*j:n_mass*(j+1)-1] = t_axis[j]
          mass[n_mass*j:n_mass*(j+1)-1] = pt_comp.mass
        ENDFOR
      *refd.time = time/t_conv                                                  ; WRITE TIME

      refd.t_scale = t_scale                                                    ; write time scale

      *refd.mass = mass                                                         ; WRITE MASS

      *refd.peaktable = pt_comp                                                 ; WRITE PEAKTABLE

      tps_group_id = H5G_OPEN(file_id, 'TPS2')                                  ; introduced 150921: TPS hsk data
        tpsinfo_id = H5D_OPEN(tps_group_id, 'TwInfo')
          tps_twinfo = H5D_READ(tpsinfo_id)
        tpsdata_id = H5D_OPEN(tps_group_id, 'TwData')
          tps_twdata = H5D_READ(tpsdata_id)

      *refd.twtps_hsk = {info:tps_twinfo, data:tps_twdata}

      IF KEYWORD_SET(peakdata_recalc) THEN $
        *refd.intensity = recalc_peakdata(fname[i], pt_comp, mass, peakdata, /INTEGRATE, $
                                          USE_PT_LIMITS=use_pt_limits)

    ENDELSE

    refd.iauchrom_vers = version
    chrom = [chrom, refd]                                                       ; append data

    H5D_CLOSE, peakdata_id ; close peakdata datasets and group
    H5G_CLOSE, pd_group_id

    H5D_CLOSE, timing_id ; close timing dataset and group
    H5G_CLOSE, t_group_id

    H5D_CLOSE, tpsinfo_id ; close tps datasets and group
    H5D_CLOSE, tpsdata_id
    H5G_CLOSE, tps_group_id

    H5F_CLOSE, file_id                                                          ; close hdf5 file
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

  IF KEYWORD_SET(verbose) THEN PRINT, SYSTIME(0), ' - import done.'

  RETURN, chrom

END
; ****************************************************************************************************************************************************