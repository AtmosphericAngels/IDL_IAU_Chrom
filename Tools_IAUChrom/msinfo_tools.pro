;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO export_intsettings2msinfo
;
; AUTHOR:
; F.Obersteiner, July 2014
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
PRO export_intsettings2msinfo, chrom, sel_chrom, PATH=path
  
  IF SIZE(chrom, /TYPE) NE 8 THEN RETURN ; chrom not loaded
  
  IF WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1) EQ -1 THEN RETURN ; no msinfo loaded into chrom strct
  
  cdate = conc_date(chrom[sel_chrom].jdate, SYSTIME(/julian), cdate1=cdate1)
  fname = DIALOG_PICKFILE(PATH=path, /WRITE, /OVERWRITE_PROMPT, file=STRCOMPRESS(cdate, /REMOVE_ALL)+'_ms.info')
  IF STRLEN(fname) EQ 0 THEN RETURN ; export aborted
  
  sep=STRING(9B) ; STRING(9B) = tabulator
  header=['NAME', sep, 'FORMULA', sep, 'RT', sep, 'RT_WIN_START', sep, 'RT_WIN_END', sep, 'METHOD', sep,  'INT_WIN_START', $
          sep, 'INT_WIN_END', sep, 'NOISE_WIN_START', sep, 'NOISE_WIN_END', sep, 'BL_TYPE', sep, 'GAUSSFIT_SIGMA', sep, $
          'SEL_PEAK', sep, 'DIF_PEAK', sep, 'SAVGOL', sep, 'THRESH', sep, 'QUANTIF', sep, 'ion_0', sep, 'ion_1', sep, 'ion_2', $
          sep, 'ion_3', sep, 'ion_4', sep, 'ion_5', sep, 'ion_6', sep, 'ion_7', sep, 'ion_8', sep, 'ion_9']
  
  OPENW, lun, fname, /GET_LUN
  PRINTF, lun, header, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
  
  FOR i=0, N_ELEMENTS(chrom[sel_chrom].subst)-1 DO BEGIN
    NAME            = STRCOMPRESS(chrom[sel_chrom].subst[i].name, /REMOVE_ALL)
    FORMULA         = STRCOMPRESS(chrom[sel_chrom].subst[i].formula, /REMOVE_ALL)
    RT              = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].ires.rt, FORMAT='(F5.1)'), /REMOVE_ALL)
    RT_WIN_START    = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rt_win[0]), /REMOVE_ALL)
    RT_WIN_END      = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rt_win[1]), /REMOVE_ALL)
    METHOD          = STRCOMPRESS(chrom[sel_chrom].subst[i].method, /REMOVE_ALL)
    INT_WIN_START   = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].int_win[0]), /REMOVE_ALL)
    INT_WIN_END     = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].int_win[1]), /REMOVE_ALL)
    NOISE_WIN_START = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].noise_win[0]), /REMOVE_ALL)
    NOISE_WIN_END   = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].noise_win[1]), /REMOVE_ALL)
    BL_TYPE         = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].bl_type), /REMOVE_ALL)
    GAUSSFIT_SIGMA  = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].sigma[0], FORMAT='(F5.1)'), /REMOVE_ALL)+';'+ $ 
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].sigma[1], FORMAT='(F5.1)'), /REMOVE_ALL)
    SEL_PEAK        = STRCOMPRESS(chrom[sel_chrom].subst[i].sel_peak, /REMOVE_ALL)
    DIF_PEAK        = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].dif_peak[0]), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].dif_peak[1]), /REMOVE_ALL)
    SVGL            = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].svgl), /REMOVE_ALL)
    THRESH          = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].thresh[0]), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].thresh[1]), /REMOVE_ALL)
    QUANTIF         = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].quant), /REMOVE_ALL)
    ion_0           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[0], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[0], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_1           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[1], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[1], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_2           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[2], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[2], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_3           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[3], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[3], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_4           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[4], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[4], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_5           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[5], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[5], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_6           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[6], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[6], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_7           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[7], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[7], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_8           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[8], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[8], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_9           = STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].mass[9], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(chrom[sel_chrom].subst[i].rel_abd[9], FORMAT='(F4.0)'), /REMOVE_ALL)
  
    PRINTF, lun, NAME, sep, FORMULA, sep, RT, sep, RT_WIN_START, sep, RT_WIN_END, sep, METHOD, sep,  INT_WIN_START, $
                 sep, INT_WIN_END, sep, NOISE_WIN_START, sep, NOISE_WIN_END, sep, BL_TYPE, sep, GAUSSFIT_SIGMA, sep, $
                 SEL_PEAK, sep, DIF_PEAK, sep, SVGL, sep, THRESH, sep, QUANTIF, sep, ion_0, sep, ion_1, sep, ion_2, $
                 sep, ion_3, sep, ion_4, sep, ion_5, sep, ion_6, sep, ion_7, sep, ion_8, sep, ion_9, $
                 FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
  ENDFOR
  
  CLOSE, lun
  FREE_LUN, lun
  
END


;************************************************************************************************************************


PRO export_subst2msinfo, subst, chrom, PATH=path, FNAME=fname

  subst_defined=0
  IF subst NE !NULL THEN subst_defined=1 ; subst defined?
  IF subst_defined EQ 1 THEN IF STRLEN((subst.name)[0]) EQ 0 THEN subst_defined=0 ; subst defined but empty?
  IF subst_defined EQ 0 THEN BEGIN
    msg=DIALOG_MESSAGE('No defaults found. Reload defaults first.', /ERROR)
    RETURN ; subst not loaded
  ENDIF

  IF NOT KEYWORD_SET(fname) THEN BEGIN
    cdate = conc_date(chrom[0].jdate, SYSTIME(/julian), cdate1=cdate1)
    fname = DIALOG_PICKFILE(PATH=path, /WRITE, file=cdate+'_def_ms.info')
  ENDIF
  IF STRLEN(fname) EQ 0 THEN RETURN ; export aborted

  sep=STRING(9B)
  header=['NAME', sep, 'FORMULA', sep, 'RT', sep, 'RT_WIN_START', sep, 'RT_WIN_END', sep, 'METHOD', sep,  'INT_WIN_START', $
    sep, 'INT_WIN_END', sep, 'NOISE_WIN_START', sep, 'NOISE_WIN_END', sep, 'BL_TYPE', sep, 'GAUSSFIT_SIGMA', sep, $
    'SEL_PEAK', sep, 'DIF_PEAK', sep, 'SAVGOL', sep, 'THRESH', sep, 'QUANTIF', sep, 'ion_0', sep, 'ion_1', sep, 'ion_2', $
    sep, 'ion_3', sep, 'ion_4', sep, 'ion_5', sep, 'ion_6', sep, 'ion_7', sep, 'ion_8', sep, 'ion_9']

  OPENW, lun, fname, /get_lun
  PRINTF,lun, header, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'

  FOR i=0, N_ELEMENTS(subst)-1 DO BEGIN  
    NAME            = STRCOMPRESS(subst[i].name, /REMOVE_ALL)
    FORMULA         = STRCOMPRESS(subst[i].formula, /REMOVE_ALL)
    RT              = STRCOMPRESS(STRING(subst[i].rt), /REMOVE_ALL)
    RT_WIN_START    = STRCOMPRESS(STRING(subst[i].rt_win[0]), /REMOVE_ALL)
    RT_WIN_END      = STRCOMPRESS(STRING(subst[i].rt_win[1]), /REMOVE_ALL)
    METHOD          = STRCOMPRESS(subst[i].method, /REMOVE_ALL)
    INT_WIN_START   = STRCOMPRESS(STRING(subst[i].int_win[0]), /REMOVE_ALL)
    INT_WIN_END     = STRCOMPRESS(STRING(subst[i].int_win[1]), /REMOVE_ALL)
    NOISE_WIN_START = STRCOMPRESS(STRING(subst[i].noise_win[0]), /REMOVE_ALL)
    NOISE_WIN_END   = STRCOMPRESS(STRING(subst[i].noise_win[1]), /REMOVE_ALL)
    BL_TYPE         = STRCOMPRESS(STRING(subst[i].bl_type), /REMOVE_ALL)
    GAUSSFIT_SIGMA  = STRCOMPRESS(STRING(subst[i].sigma[0], FORMAT='(F5.1)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].sigma[1], FORMAT='(F5.1)'), /REMOVE_ALL)
    SEL_PEAK        = STRCOMPRESS(subst[i].sel_peak, /REMOVE_ALL)
    DIF_PEAK        = STRCOMPRESS(STRING(subst[i].dif_peak[0]), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].dif_peak[1]), /REMOVE_ALL)
    SVGL            = STRCOMPRESS(STRING(subst[i].svgl), /REMOVE_ALL)
    THRESH          = STRCOMPRESS(STRING(subst[i].thresh[0]), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].thresh[1]), /REMOVE_ALL)
    QUANTIF         = STRCOMPRESS(STRING(subst[i].quant), /REMOVE_ALL)
    ion_0           = STRCOMPRESS(STRING(subst[i].mass[0], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[0], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_1           = STRCOMPRESS(STRING(subst[i].mass[1], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[1], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_2           = STRCOMPRESS(STRING(subst[i].mass[2], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[2], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_3           = STRCOMPRESS(STRING(subst[i].mass[3], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[3], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_4           = STRCOMPRESS(STRING(subst[i].mass[4], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[4], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_5           = STRCOMPRESS(STRING(subst[i].mass[5], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[5], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_6           = STRCOMPRESS(STRING(subst[i].mass[6], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[6], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_7           = STRCOMPRESS(STRING(subst[i].mass[7], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[7], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_8           = STRCOMPRESS(STRING(subst[i].mass[8], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[8], FORMAT='(F4.0)'), /REMOVE_ALL)
    ion_9           = STRCOMPRESS(STRING(subst[i].mass[9], FORMAT='(F12.5)'), /REMOVE_ALL)+';'+ $
                      STRCOMPRESS(STRING(subst[i].rel_abd[9], FORMAT='(F4.0)'), /REMOVE_ALL)

    PRINTF, lun, NAME, sep, FORMULA, sep, RT, sep, RT_WIN_START, sep, RT_WIN_END, sep, METHOD, sep,  INT_WIN_START, $
                 sep, INT_WIN_END, sep, NOISE_WIN_START, sep, NOISE_WIN_END, sep, BL_TYPE, sep, GAUSSFIT_SIGMA, sep, $
                 SEL_PEAK, sep, DIF_PEAK, sep, SVGL, sep, THRESH, sep, QUANTIF, sep, ion_0, sep, ion_1, sep, ion_2, $
                 sep, ion_3, sep, ion_4, sep, ion_5, sep, ion_6, sep, ion_7, sep, ion_8, sep, ion_9, $
                 FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
  ENDFOR

  CLOSE, lun
  FREE_LUN, lun
  
END


;************************************************************************************************************************


PRO upd_defintsettings

  COMMON DATA

  IF chrom EQ !NULL THEN RETURN ; chrom not existing, abort.
  
  present_substlist = chrom[0].subst.name
  
  refs=read_subst(PATH=path)
  refi=create_refi()
  newdef_subst=add_ires2subst(refs, refi)
  new_substlist=newdef_subst.name
  
; match cases of new_substlist and present_substlist:
; 0 - equal in number and strings as well as their position
;     --> simply replace (or create) defaults
; 1 - equal in number and strings as well as their position
;     --> sort and replace (or create) defaults
; 2 - equal in number but different in strings
;     --> replace matches, keep existing which have no match in new defaults
;     --> this case is forbidden if subst was empty/undefined
; 3 - different in number and different in strings
;     --> replace matches, keep existing which have no match in new defaults
;     --> add new defaults which have no match in existing
;     --> if subst was empty/undefined, this case must only be allowed if new
;         defaults contain all substances that exist in chrom.subst
; 4 - no match
;     --> add new defaults
;     --> not allowed if subst was not defined

  subst_defined=0
  IF subst NE !NULL THEN subst_defined=1 ; subst defined?
  IF subst_defined EQ 1 THEN IF STRLEN((subst.name)[0]) EQ 0 THEN subst_defined=0 ; subst defined but empty?

  matchresult = compare_arrays(present_substlist, new_substlist)
  
  

  print, 'temp stop'
 

END