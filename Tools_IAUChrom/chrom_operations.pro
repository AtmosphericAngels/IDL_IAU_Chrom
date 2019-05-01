;------------------------------------------------------------------------------------------------------------------------
;+
; chrom_operations
;
; AUTHOR: F.Obersteiner, June/July 2014
;
; INFO:
; create_empty_chromstrct
; convert_oldchrom (if older than 4.65)
; convert_oldsubst (if does not contain noise_win)
; version_check (if newer than 4.65)
; batch_process_chrom (re-integrate and/or re-calculate noise in multiple chrom files)
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION create_empty_chromstrct, old_chrom, CHROM_ONLY=chrom_only, N_SUBST=n_subst

  n_chrom = N_ELEMENTS(old_chrom)

  IF NOT KEYWORD_SET(n_subst) THEN $
    n_subst = N_ELEMENTS(old_chrom.subst)/n_chrom

  empty_chrom = []

  FOR i=0, n_chrom-1 DO BEGIN
    refd = create_refd()
    *refd.time = DBLARR(N_ELEMENTS(*old_chrom[i].time))
    *refd.mass = DBLARR(N_ELEMENTS(*old_chrom[i].mass))
    *refd.intensity = DBLARR(N_ELEMENTS(*old_chrom[i].intensity))
    empty_chrom = [empty_chrom, refd]
  ENDFOR

  IF NOT KEYWORD_SET(chrom_only) THEN BEGIN
    subst = REPLICATE(create_refs(), n_subst)
    ires = create_refi()
    subst = add_ires2subst(subst, ires)
    empty_chrom = add_subst2chrom(empty_chrom, subst)
  ENDIF

  RETURN, empty_chrom

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION convert_oldchrom, old_chrom, current_version

  empty_chrom = create_empty_chromstrct(old_chrom)

  STRUCT_ASSIGN, old_chrom, empty_chrom

  new_chrom = TEMPORARY(empty_chrom)

  FOR i=0, N_ELEMENTS(new_chrom)-1 DO new_chrom[i].iauchrom_vers = current_version

  vd = WHERE(STRUPCASE(TAG_NAMES(old_chrom)) EQ 'INSTR_TYPE', nvd)

  IF nvd GT 0 THEN BEGIN
    FOR i=0, N_ELEMENTS(new_chrom)-1 DO new_chrom[i].instr_type = old_chrom[i].instr_type
  ENDIF

  old_chrom = !NULL

  RETURN, new_chrom

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION convert_oldsubst, old_subst
  n_subst = N_ELEMENTS(old_subst.name)
  empty_subst = REPLICATE(create_refs(), n_subst)
  new_subst = empty_subst

  vd = WHERE(STRUPCASE(TAG_NAMES(old_subst)) EQ 'NOISE_WIN', nvd) ; check if tagname 'noise' exists

  FOR row=0, n_subst-1 DO BEGIN
    new_subst[row].name      = old_subst[row].name
    new_subst[row].formula   = old_subst[row].formula
    new_subst[row].rt        = old_subst[row].rt
    new_subst[row].rt_win    = old_subst[row].rt_win
    new_subst[row].method    = old_subst[row].method
    new_subst[row].int_win   = old_subst[row].int_win

    IF nvd EQ 0 THEN new_subst[row].noise_win = FLTARR(2)*!VALUES.F_NAN $
      ELSE new_subst[row].noise_win = old_subst[row].noise_win

    new_subst[row].bl_type   = old_subst[row].bl_type
    new_subst[row].sigma     = old_subst[row].sigma
    new_subst[row].sel_peak  = old_subst[row].sel_peak
    new_subst[row].dif_peak  = old_subst[row].dif_peak
    new_subst[row].svgl      = old_subst[row].svgl
    new_subst[row].thresh    = old_subst[row].thresh
    new_subst[row].quant     = old_subst[row].quant
    new_subst[row].mass      = old_subst[row].mass
    new_subst[row].rel_abd   = old_subst[row].rel_abd
  ENDFOR

  old_subst = !NULL

  RETURN, new_subst

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION version_check, restored_chrom, VCHECK_VERSION=vcheck_version, VERS_TAG=vers_tag

  IF NOT KEYWORD_SET(vcheck_version) THEN vcheck_version = 4.65
  IF NOT KEYWORD_SET(vers_tag) THEN vers_tag = 'IAUCHROM_VERS'

  version = 0.

  w_verstag = WHERE(STRUPCASE(TAG_NAMES(restored_chrom)) EQ vers_tag, w_no_verstag)

  IF w_no_verstag EQ 0 THEN vcheck = 0 $
    ELSE version = FIX(restored_chrom[0].iauchrom_vers, TYPE=4)

  IF version LE vcheck_version THEN vcheck = 0 ELSE vcheck = 1

  RETURN, vcheck

END

;------------------------------------------------------------------------------------------------------------------------