;------------------------------------------------------------------------------------------------------------------------
;+
; AUTHOR:
; H.Boenisch and F.Obersteiner
;
; INFO:
; total ion count calculation functions for different instruments.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION Hist_QP_TIC, v, x, xbmin, xbmax, xbsize, nxintm

  hist = lonarr(nxintm)
  vmean = fltarr(nxintm)*!VALUES.F_NAN

  vd=where(finite(x+v),nvd); ; finite-> NaN Abfrage
  IF (nvd GT 0) THEN BEGIN
    xx = x[vd]
    vv = v[vd]
  ENDIF ELSE $
    RETURN,Bin

  xhist=histogram(xx,MIN=xbmin,MAX=xbmax,BINSIZE=xbsize,REVERSE_INDICES=rx,/NAN)
  FOR i=0L, nxintm-1 DO BEGIN
    IF (rx[i] NE rx[i+1]) THEN BEGIN
      wx = rx(rx[i]:rx[i+1]-1)
      hist[i] = xhist[i]
      vmean[i] = mean(vv[wx])
    ENDIF
  ENDFOR

  RETURN, vmean

END

; **************************************************************************************

FUNCTION calc_TIC_BenchTOF, chrom, sel_chrom

  time = *chrom[sel_chrom].time

  vd_time = time[WHERE(FINITE(time) EQ 1)]
  x_tic = vd_time[UNIQ(vd_time, SORT(vd_time))]
  v_tic = x_tic*!Values.F_NAN

  ix = WHERE(time[0:-2] NE time[1:*])
  ix=[0,ix,N_ELEMENTS(time)]

  FOR j=0L, N_ELEMENTS(x_tic)-1 DO BEGIN
    IF ix[j+1] GT ix[j] THEN v_tic[j] = TOTAL((*chrom[sel_chrom].intensity)[ix[j]:ix[j+1]-1]) $
      ELSE v_tic[j]=(*chrom[sel_chrom].intensity)[ix[j]]
  ENDFOR

  tic={intensity:v_tic,$
       time:x_tic}

  RETURN, tic

END

; **************************************************************************************

FUNCTION calc_TIC_HTOF, chrom, sel_chrom, NOMONLY=nomonly
;+++++++++++++++++++++++
; Get general data
  uniq_mass = get_uniq_mass(chrom, SEL_CHROM=sel_chrom)
  IF KEYWORD_SET(nomonly) THEN $
    uniq_mass = uniq_mass[WHERE((*chrom[0].peaktable).label EQ 'nominal')]

;+++++++++++++++++++++++
; Intensity: fist mass trace
  msel = WHERE(*chrom[sel_chrom].mass EQ FIX(uniq_mass[0], type=4), nvd)
  v_tic = (*chrom[sel_chrom].intensity)[msel]
  x_tic = (*chrom[sel_chrom].time)[msel]

;+++++++++++++++++++++++
; Intensity: loop all masses and sum up intensities
  FOR i=1, N_ELEMENTS(uniq_mass)-1 DO BEGIN
    msel = WHERE(*chrom[sel_chrom].mass EQ FIX(uniq_mass[i], type=4), nvd)
    v = (*chrom[sel_chrom].intensity)[msel]
    v_tic = v_tic+v
  ENDFOR

  tic={intensity:v_tic,$
       time:x_tic}

  RETURN, tic

END

; **************************************************************************************

FUNCTION calc_TIC_QP, chrom, sel_chrom
;+++++++++++++++++++++++
; Get general data
;T0 = SYSTIME(1)
  uniq_mass = get_uniq_mass(chrom, SEL_CHROM=sel_chrom) ; calculate available masses
  xbmin = 0.                                  ; minimum of time axis
  xbmax = MAX(*chrom[sel_chrom].time, /nan)   ; maximum of time axis
  xbsize = 0.5 ; force 2 Hz                   ; time unit divided by xbsize equals nbr of points per second
  IF chrom[sel_chrom].t_scale EQ 'Minutes' THEN xbsize = xbsize/60. ; adjust to minute-timescale
  nxint = FLOOR((xbmax-xbmin)/xbsize)+1
  xint = xbmin+FINDGEN(nxint)*xbsize
  nxintm = nxint-1
  xintm = (xint(1:nxint-1)+xint(0:nxint-2))*0.5

;+++++++++++++++++++++++
; Intensity: fist mass trace / Time-Axis
  msel = WHERE(*chrom[sel_chrom].mass EQ FIX(uniq_mass[0], type=4), nvd)
  v = (*chrom[sel_chrom].intensity)[msel]
  x = (*chrom[sel_chrom].time)[msel]
  t_bin0 = SYSTIME(1)
  vmean=Hist_QP_TIC(v, x, xbmin, xbmax, xbsize, nxintm)
  v_tic = vmean
;  t_where0= SYSTIME(1)
    v_nan = WHERE(FINITE(v_tic, /nan)) ; replace NANs with zeros to avoid errors
    v_tic[v_nan] = 0.
;  t_where1= SYSTIME(1)
  x_tic = xintm

;+++++++++++++++++++++++
; loop all masses and sum up intensities
  FOR i=1, N_ELEMENTS(uniq_mass)-1 DO BEGIN
    msel = WHERE(*chrom[sel_chrom].mass EQ FIX(uniq_mass[i], type=4), nvd)
    v = (*chrom[sel_chrom].intensity)[msel]
    x = (*chrom[sel_chrom].time)[msel]
    vmean=Hist_QP_TIC(v, x, xbmin, xbmax, xbsize, nxintm)
       v_nan = WHERE(FINITE(vmean, /nan)) ; replace NANs with zeros to avoid errors
       vmean[v_nan] = 0.
    v_tic = v_tic+vmean
  ENDFOR
  tic={intensity:v_tic,$
       time:x_tic}

  RETURN, tic

END

; **************************************************************************************
; **************************************************************************************

FUNCTION calc_tic, chrom, sel_chrom, NOMONLY=nomonly

  instr_type = chrom[0].instr_type

  CASE instr_type OF; 0: not defined, 1: QP, 2: BenchTOF, 3: HTOF, 4: ECD
    0 : tic=calc_TIC_QP(chrom, sel_chrom)
    1 : tic=calc_TIC_QP(chrom, sel_chrom)
    2 : tic=calc_TIC_BenchTOF(chrom, sel_chrom)
    3 : tic=calc_TIC_HTOF(chrom, sel_chrom, NOMONLY=nomonly)
    4 : tic=calc_TIC_QP(chrom, sel_chrom)
    5 : tic=calc_TIC_QP(chrom, sel_chrom)
    6 : tic=calc_TIC_QP(chrom, sel_chrom)
  ENDCASE

  RETURN, tic

END