;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION read_AES_cdf
;
; INFO:
; F.Obersteiner 2016-10, imports AES data, written for E.Karu, MPI Mainz.
;
; MODIFICATIONS:
; 1704, FO: added import of merged data; i.e. up to 8 channels in one cdf.
; 1705, FO: added def_file keyword to import function to override pickfile dialog.
;           added sort_by_jdate keyword to sort loaded files by measurement timestamp.
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION read_AED_cdf, PATH=path, T_SCALE=t_scale, VERSION=version, DEF_FILE=def_file, SORT_BY_JDATE=sort_by_jdate, $
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

  ; file type check, specific for AED data
  tmp = cdf2idl_struct(fname[0])
  IF (WHERE(TAG_NAMES(tmp) EQ 'N_CHANNELS'))[0] NE -1 THEN is_merge = 1 ELSE is_merge = 0

  chrom = []
  FOR i=0, N_ELEMENTS(fname)-1 DO BEGIN
    refd = create_refd()
    ncdfstr = cdf2idl_struct(fname[i])
    refd.fname = fname[i]

    IF is_merge THEN BEGIN
      ; import a file that contains merged channel data

      CASE STRLEN(ncdfstr.CHFILE_TIME) OF
        21: ix_ts = [6,0,3,10,13,16,19] ; ix of mn dd yy hh mm ss AM/PM
        22: ix_ts = [6,0,3,11,14,17,20]
      ENDCASE

      yy      = STRMID(STRING(ncdfstr.CHFILE_TIME),ix_ts[0],4)
      mn      = STRMID(STRING(ncdfstr.CHFILE_TIME),ix_ts[1],2)
      dd      = STRMID(STRING(ncdfstr.CHFILE_TIME),ix_ts[2],2)
      hh      = STRMID(STRING(ncdfstr.CHFILE_TIME),ix_ts[3],2)
      mm      = STRMID(STRING(ncdfstr.CHFILE_TIME),ix_ts[4],2)
      ss      = STRMID(STRING(ncdfstr.CHFILE_TIME),ix_ts[5],2)

      am_pm   = STRUPCASE(STRMID(STRING(ncdfstr.CHFILE_TIME),ix_ts[6],2))
      CASE 1 OF
        (am_pm EQ 'AM' AND hh LT 12): hh=hh
        (am_pm EQ 'AM' AND hh EQ 12): hh=hh-12
        (am_pm EQ 'PM' AND hh LT 12): hh=hh+12
        (am_pm EQ 'PM' AND hh EQ 12): hh=hh
      ENDCASE

      refd.jdate = julday(mn, dd, yy, hh, mm, ss)

      ; assume equal number of elements in each channel...
      n_ch = LONG(ncdfstr.N_CHANNELS)
      n_dp = N_ELEMENTS(ncdfstr.CH1)*n_ch

      ; assume 5 Hz data frequency
      t_lim = [0., N_ELEMENTS(ncdfstr.CH1)/5./t_conv]
      tmp_t = interpol(t_lim, N_ELEMENTS(ncdfstr.CH1))

      CASE n_ch OF ; based on number of channels, vectorise data...
        8: $
          BEGIN
            time = [tmp_t,tmp_t,tmp_t,tmp_t,tmp_t,tmp_t,tmp_t,tmp_t]
            int = [ncdfstr.CH1,ncdfstr.CH2,ncdfstr.CH3,ncdfstr.CH4,$
                   ncdfstr.CH5,ncdfstr.CH6,ncdfstr.CH7,ncdfstr.CH8]
            tmp_m = [ncdfstr.CH1_element_id,ncdfstr.CH2_element_id,ncdfstr.CH3_element_id,ncdfstr.CH4_element_id, $
                     ncdfstr.CH5_element_id,ncdfstr.CH6_element_id,ncdfstr.CH7_element_id,ncdfstr.CH8_element_id]
          END
        7: $
          BEGIN
            time = [tmp_t,tmp_t,tmp_t,tmp_t,tmp_t,tmp_t,tmp_t]
            int = [ncdfstr.CH1,ncdfstr.CH2,ncdfstr.CH3,ncdfstr.CH4,$
                   ncdfstr.CH5,ncdfstr.CH6,ncdfstr.CH7]
            tmp_m = [ncdfstr.CH1_element_id,ncdfstr.CH2_element_id,ncdfstr.CH3_element_id,ncdfstr.CH4_element_id, $
                     ncdfstr.CH5_element_id,ncdfstr.CH6_element_id,ncdfstr.CH7_element_id]
          END
        6: $
          BEGIN
            time = [tmp_t,tmp_t,tmp_t,tmp_t,tmp_t,tmp_t]
            int = [ncdfstr.CH1,ncdfstr.CH2,ncdfstr.CH3,ncdfstr.CH4,$
                   ncdfstr.CH5,ncdfstr.CH6]
            tmp_m = [ncdfstr.CH1_element_id,ncdfstr.CH2_element_id,ncdfstr.CH3_element_id,ncdfstr.CH4_element_id, $
                     ncdfstr.CH5_element_id,ncdfstr.CH6_element_id]
          END
        5: $
          BEGIN
            time = [tmp_t,tmp_t,tmp_t,tmp_t,tmp_t]
            int = [ncdfstr.CH1,ncdfstr.CH2,ncdfstr.CH3,ncdfstr.CH4,$
                   ncdfstr.CH5]
            tmp_m = [ncdfstr.CH1_element_id,ncdfstr.CH2_element_id,ncdfstr.CH3_element_id,ncdfstr.CH4_element_id, $
                     ncdfstr.CH5_element_id]
          END
        4: $
          BEGIN
            time = [tmp_t,tmp_t,tmp_t,tmp_t]
            int = [ncdfstr.CH1,ncdfstr.CH2,ncdfstr.CH3,ncdfstr.CH4]
            tmp_m = [ncdfstr.CH1_element_id,ncdfstr.CH2_element_id,ncdfstr.CH3_element_id,ncdfstr.CH4_element_id]
          END
        3: $
          BEGIN
            time = [tmp_t,tmp_t,tmp_t]
            int = [ncdfstr.CH1,ncdfstr.CH2,ncdfstr.CH3]
            tmp_m = [ncdfstr.CH1_element_id,ncdfstr.CH2_element_id,ncdfstr.CH3_element_id]
          END
        2: $
          BEGIN
            time = [tmp_t,tmp_t]
            int = [ncdfstr.CH1,ncdfstr.CH2]
            tmp_m = [ncdfstr.CH1_element_id,ncdfstr.CH2_element_id]
          END
        1: $
          BEGIN
            time = tmp_t
            int = ncdfstr.CH1
            tmp_m = ncdfstr.CH1_element_id
          END
      ENDCASE


      mass = LONARR(n_dp)
      FOR j=0, ncdfstr.N_CHANNELS-1 DO $
        mass[j*N_ELEMENTS(ncdfstr.CH1):((j+1)*N_ELEMENTS(ncdfstr.CH1))-1] = LONG(tmp_m[j])


      *refd.time = TEMPORARY(time)
      *refd.mass = TEMPORARY(mass)
      *refd.intensity = TEMPORARY(int)


    ENDIF ELSE BEGIN
      ; import a file that contains data from only one channel

      yy      = STRMID(STRING(ncdfstr.INJECTION_DATE_TIME_STAMP),0,4)
      mn      = STRMID(STRING(ncdfstr.INJECTION_DATE_TIME_STAMP),4,2)
      dd      = STRMID(STRING(ncdfstr.INJECTION_DATE_TIME_STAMP),6,2)
      hh      = STRMID(STRING(ncdfstr.INJECTION_DATE_TIME_STAMP),8,2)
      mm      = STRMID(STRING(ncdfstr.INJECTION_DATE_TIME_STAMP),10,2)
      ss      = STRMID(STRING(ncdfstr.INJECTION_DATE_TIME_STAMP),12,2)

      refd.jdate = julday(mn, dd, yy, hh, mm, ss)

      t_lim = [0., ncdfstr.ACTUAL_RUN_TIME_LENGTH]
      *refd.time = (interpol(t_lim, N_ELEMENTS(ncdfstr.ORDINATE_VALUES)))/t_conv

      *refd.mass = MAKE_ARRAY(N_ELEMENTS(ncdfstr.ORDINATE_VALUES), /FLOAT, VALUE=1.)

      *refd.intensity = ncdfstr.ORDINATE_VALUES

    ENDELSE

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