;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; read_tofwerkh5_hr
;
; AUTHOR:
; F.Obersteiner, Feb 2015, created.
;
; INFO:
; reads high resolution TOFMS data from hdf5 and returns idl strct.
; massaxis keyword: allows external calibration.
;
; MODIFICATIONS:
; F.O., Oct 15: added SiS as variable to output strct and closing functions for hdf5 datasets.
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION read_tofwerkh5_hr, file, MASSAXIS=massaxis, VERBOSE=verbose

  IF KEYWORD_SET(verbose) THEN PRINT, SYSTIME(0), ' - importing highres data from ', FILE_BASENAME(file)

    file_id = H5F_OPEN(file)
    HTOF_rawdata = create_ref_hrdata()
    HTOF_rawdata.fname = file

  ; +++
  ; +++ timing data +++
    NbrBufs = (H5A_READ(H5A_OPEN_NAME(file_id, 'NbrBufs')))[0]
    NbrWrites = (H5A_READ(H5A_OPEN_NAME(file_id, 'NbrWrites')))[0]
    HTOF_rawdata.NbrWaveforms = (H5A_READ(H5A_OPEN_NAME(file_id, 'NbrWaveforms')))[0]
      t_group_id = H5G_OPEN(file_id, 'TimingData')
        timing_id = H5D_OPEN(t_group_id, 'BufTimes')
          buftimes = H5D_READ(timing_id)
    t_axis = FLTARR(NbrBufs*NbrWrites)
    t_axis[0:(NbrBufs*NbrWrites-1)] = buftimes[0:(NbrBufs*NbrWrites-1)]


    *HTOF_rawdata.chromtime = REFORM(t_axis, NbrBufs*NbrWrites) ; chromatographic time axis in seconds since start of run
    buf_ix = INDGEN(LONG(NbrBufs), LONG(NbrWrites), /LONG)
    FOR j=1, NbrWrites-1 DO buf_ix[*,j]=buf_ix[*,0]
    *HTOF_rawdata.ix_buf = REFORM(buf_ix, NbrBufs*NbrWrites) ; buf index
    *HTOF_rawdata.ix_write = REBIN(INDGEN(NbrWrites), NbrBufs*NbrWrites) ; write index

  ; +++
  ; +++ full spectra group +++
    fulspec_id = H5G_OPEN(file_id, 'FullSpectra')
    IF NOT KEYWORD_SET(MASSAXIS) THEN $
      *HTOF_rawdata.massaxis = h5_get_massaxis(file) ELSE $ ; m/z value for every bin derived by calibration
        *HTOF_rawdata.massaxis = massaxis
    tofdata_id=H5D_OPEN(fulspec_id, 'TofData')
    *HTOF_rawdata.spectra = H5D_READ(tofdata_id) ; bin intensities. dims: NbrBins*NbrDetectors*NbrWrites*NbrBufs
    HTOF_rawdata.SiS = (H5A_READ(H5A_OPEN_NAME(fulspec_id, 'Single Ion Signal')))[0]

    H5D_CLOSE, timing_id ; close timing dataset and group
    H5G_CLOSE, t_group_id

    H5D_CLOSE, tofdata_id ; close spectra dataset and group
    H5G_CLOSE, fulspec_id

    H5F_CLOSE, file_id

  RETURN, HTOF_rawdata
END