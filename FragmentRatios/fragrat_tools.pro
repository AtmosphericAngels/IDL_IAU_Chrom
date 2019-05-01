;------------------------------------------------------------------------------------------------------------------------
;+
; AUTHOR:
; F.Obersteiner, Dec 2013 , Jun 2014, Feb 2015
;
; INFO:
; tools for the fragment ratio calculator.
;-
;------------------------------------------------------------------------------------------------------------------------

FUNCTION create_fragres

  strct = {file:      '', $        ; filename
           masses:    FLTARR(2)*!Values.F_NAN, $ ; mass of fragment 1 and 2
           ratio:     !Values.F_NAN, $        ; mean ratio; f2/f1
           rsd:       !Values.F_NAN, $        ; relative standard deviation
           n_dp:      !Values.F_NAN, $        ; number of datapoints used for ratio calculation
           RTp1:      !Values.F_NAN, $        ; RT of peak maximum
           peakint_sigma: FLTARR(2)*!Values.F_NAN, $
           fragrat_sigma: FLTARR(2)*!Values.F_NAN, $
           fragrat_t_win: FLTARR(2)*!Values.F_NAN, $
           a_ratio: !Values.F_NAN, $
           h_ratio: !Values.F_NAN, $
           dRT: !Values.F_NAN}

  RETURN, strct

END

;------------------------------------------------------------------------------------------------------------------------

PRO fragres2txt_cur, PATH=path
  COMMON FRAGDATA
  IF SIZE(fragres, /TYPE) NE 8 THEN RETURN

  t=SYSTIME(/JULIAN)
  caldat, t, mm,dd,yy,hh,mn
  dt=STRING(yy,format='(I4)')+STRING(mm,format='(I02)')+STRING(dd,format='(I02)')+STRING(hh,format='(I02)')+STRING(mn,format='(I02)')
  fm='_'+STRCOMPRESS(STRING(fragres.masses[1]), /REMOVE_ALL)+'_vs_'+STRCOMPRESS(STRING(fragres.masses[0]), /REMOVE_ALL)+'_'
  fname=DIALOG_PICKFILE(file=dt+fm+'f-rat.txt', /OVERWRITE_PROMPT, /WRITE, PATH=path)
  IF FILE_DIRNAME(fname) NE '' THEN path=FILE_DIRNAME(fname)
  IF STRLEN(fname) EQ 0 THEN RETURN

  sep=STRING(9B)
  header=['file', sep, 'm/q_f1', sep, 'm/q_f2', sep, 'ratio', sep, 'rsd', sep, 'n_dp', sep, 'RTp1', sep, 'dRT', sep, $
          'peakint_sl', sep, 'peakint_sr', sep, 'fragrat_sl', sep, 'fragrat_sr', sep, 'a_ratio', sep, 'h_ratio']

  OPENW, lun, fname, /get_lun
  PRINTF, lun, header, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'

  file       = fragres.file
  mass_f1    = STRCOMPRESS(STRING(fragres.masses[0]), /REMOVE_ALL)
  mass_f2    = STRCOMPRESS(STRING(fragres.masses[1]), /REMOVE_ALL)
  ratio      = STRCOMPRESS(STRING(fragres.ratio), /REMOVE_ALL)
  rsd        = STRCOMPRESS(STRING(fragres.rsd), /REMOVE_ALL)
  n_dp       = STRCOMPRESS(STRING(fragres.n_dp, FORMAT='(I)'), /REMOVE_ALL)
  RTp1       = STRCOMPRESS(STRING(fragres.RTp1), /REMOVE_ALL)
  dRT        = STRCOMPRESS(STRING(fragres.dRT), /REMOVE_ALL)
  peakint_sl = STRCOMPRESS(STRING(fragres.peakint_sigma[0]), /REMOVE_ALL)
  peakint_sr = STRCOMPRESS(STRING(fragres.peakint_sigma[1]), /REMOVE_ALL)
  fragrat_sl = STRCOMPRESS(STRING(fragres.fragrat_sigma[0]), /REMOVE_ALL)
  fragrat_sr = STRCOMPRESS(STRING(fragres.fragrat_sigma[1]), /REMOVE_ALL)
  a_ratio    = STRCOMPRESS(STRING(fragres.a_ratio), /REMOVE_ALL)
  h_ratio    = STRCOMPRESS(STRING(fragres.h_ratio), /REMOVE_ALL)

  PRINTF, lun, file, sep, mass_f1, sep, mass_f2, sep, ratio, sep, rsd, sep, n_dp, sep, RTp1, sep, dRT, sep, $
               peakint_sl, sep, peakint_sr, sep, fragrat_sl, sep, fragrat_sr, sep, a_ratio, sep, h_ratio, $
               FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'

  CLOSE, lun
  FREE_LUN, lun

END

;------------------------------------------------------------------------------------------------------------------------

PRO fragres2txt_all, PATH=path
  COMMON FRAGDATA
  IF SIZE(allfragres, /TYPE) NE 8 THEN RETURN

  t=SYSTIME(/JULIAN)
  caldat, t, mm,dd,yy,hh,mn
  dt=STRING(yy,format='(I4)')+STRING(mm,format='(I02)')+STRING(dd,format='(I02)')+STRING(hh,format='(I02)')+STRING(mn,format='(I02)')
  masses=allfragres[(WHERE(FINITE(allfragres.masses[0]) NE 0))[0]].masses
  fm='_'+STRCOMPRESS(STRING(masses[1]), /REMOVE_ALL)+'_vs_'+STRCOMPRESS(STRING(masses[0]), /REMOVE_ALL)+'_'
  fname=DIALOG_PICKFILE(file=dt+fm+'f-rat.txt', /OVERWRITE_PROMPT, /WRITE, PATH=path)
  IF FILE_DIRNAME(fname) NE '' THEN path=FILE_DIRNAME(fname)
  IF STRLEN(fname) EQ 0 THEN RETURN

  sep=STRING(9B)
  header=['file', sep, 'm/q_f1', sep, 'm/q_f2', sep, 'ratio', sep, 'rsd', sep, 'n_dp', sep, 'RTp1', sep, 'dRT', sep, $
          'peakint_sl', sep, 'peakint_sr', sep, 'fragrat_sl', sep, 'fragrat_sr', sep, 'a_ratio', sep, 'h_ratio']

  OPENW, lun, fname, /get_lun
  PRINTF, lun, header, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'

  FOR i=0, N_ELEMENTS(allfragres.file)-1 DO BEGIN

    file       = allfragres[i].file
    mass_f1    = STRCOMPRESS(STRING(allfragres[i].masses[0]), /REMOVE_ALL)
    mass_f2    = STRCOMPRESS(STRING(allfragres[i].masses[1]), /REMOVE_ALL)
    ratio      = STRCOMPRESS(STRING(allfragres[i].ratio), /REMOVE_ALL)
    rsd        = STRCOMPRESS(STRING(allfragres[i].rsd), /REMOVE_ALL)
    n_dp       = STRCOMPRESS(STRING(allfragres[i].n_dp, FORMAT='(I)'), /REMOVE_ALL)
    RTp1       = STRCOMPRESS(STRING(allfragres[i].RTp1), /REMOVE_ALL)
    dRT        = STRCOMPRESS(STRING(allfragres[i].dRT), /REMOVE_ALL)
    peakint_sl = STRCOMPRESS(STRING(allfragres[i].peakint_sigma[0]), /REMOVE_ALL)
    peakint_sr = STRCOMPRESS(STRING(allfragres[i].peakint_sigma[1]), /REMOVE_ALL)
    fragrat_sl = STRCOMPRESS(STRING(allfragres[i].fragrat_sigma[0]), /REMOVE_ALL)
    fragrat_sr = STRCOMPRESS(STRING(allfragres[i].fragrat_sigma[1]), /REMOVE_ALL)
    a_ratio    = STRCOMPRESS(STRING(allfragres[i].a_ratio), /REMOVE_ALL)
    h_ratio    = STRCOMPRESS(STRING(allfragres[i].h_ratio), /REMOVE_ALL)

    PRINTF, lun, file, sep, mass_f1, sep, mass_f2, sep, ratio, sep, rsd, sep, n_dp, sep, RTp1, sep, dRT, sep, $
                 peakint_sl, sep, peakint_sr, sep, fragrat_sl, sep, fragrat_sr, sep, a_ratio, sep, h_ratio, $
               FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
  ENDFOR

  CLOSE, lun
  FREE_LUN, lun

END