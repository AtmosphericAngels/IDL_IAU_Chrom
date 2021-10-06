;------------------------------------------------------------------------------------------------------------------------
;+
; AUTHOR:
; H.Boenisch, S.Sala, F.Obersteiner
;
; INFO:
; sort-of general tools; however mostly IAU_Chrom-specific...
;-
;------------------------------------------------------------------------------------------------------------------------

FUNCTION add_subst2chrom, file, substadd

  chrom = create_struct(file[0], 'subst', substadd)
  FOR i=1, n_elements(file)-1 DO chrom=[chrom, create_struct(file[i], 'subst', substadd)]

  RETURN, chrom

END

; **************************************************************************************

FUNCTION add_ires2subst, refs, refi

  subst = create_struct(refs[0], 'ires', refi)
  FOR i=1,n_elements(refs)-1 DO subst=[subst, create_struct(refs[i], 'ires', refi)]

  RETURN, subst

END

; **************************************************************************************

FUNCTION export_intres, chrom

  n_chrom = n_elements(chrom)
  n_subst = n_elements(chrom[0].subst)
  refi = create_refi()
  subst_strct = { name:    '',$
                  formula: '',$
                  ires: refi }

  subst_strct = replicate(subst_strct, n_subst)

  intres_strct = { fname:      '',$
                   jdate:      !VALUES.D_NAN, $
                   instr_type: '',$
                   subst:      subst_strct }

  intres_strct = replicate(intres_strct, n_chrom)

  STRUCT_ASSIGN, chrom, intres_strct

  RETURN, intres_strct

END

; **************************************************************************************

FUNCTION get_uniq_mass, chrom, SEL_CHROM=sel_chrom                   ; get unique masses of chromatograms

  tot_uniqm = []                                                     ; init total unique masses as empty array
  IF SIZE(chrom, /TYPE) NE 8 THEN RETURN, tot_uniqm
  IF STRLEN(chrom[0].fname) EQ 0 THEN RETURN, tot_uniqm                ; abort if chrom not defined

  IF NOT KEYWORD_SET(SEL_CHROM) THEN BEGIN                           ; ALL CHROMATOGRAMS
    FOR i=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN                      ; loop over all chromatograms
      w_fin = WHERE(FINITE(*chrom[i].mass) EQ 1, n_fin)
      IF n_fin GT 0 THEN BEGIN ; only append if finite values are found
        vd_mass = (*chrom[i].mass)[w_fin] ; exclued nans
        uniqm = vd_mass[UNIQ(vd_mass, SORT(vd_mass))]
        tot_uniqm = [tot_uniqm, uniqm]
      ENDIF
    ENDFOR
    IF N_ELEMENTS(tot_uniqm) GT 0 THEN tot_uniqm = tot_uniqm[UNIQ(tot_uniqm, SORT(tot_uniqm))]
  ENDIF ELSE BEGIN
    w_fin = WHERE(FINITE(*chrom[sel_chrom].mass) EQ 1, n_fin)
    IF n_fin GT 0 THEN BEGIN ; only append if finite values are found
      vd_mass = (*chrom[sel_chrom].mass)[w_fin]
      tot_uniqm = vd_mass[UNIQ(vd_mass, SORT(vd_mass))]
    ENDIF                                            ; SPECIFIC CHROMATOGRAM
  ENDELSE

  RETURN, tot_uniqm

END

; **************************************************************************************

PRO show_list, event, chrom, EMPTY=empty

  IF SIZE(chrom, /TYPE) NE 8 THEN RETURN
  chromid = WIDGET_INFO(event.top, find_by_uname='sel_chrom')
    WIDGET_CONTROL, chromid, set_value = ''
  massid = WIDGET_INFO(event.top, find_by_uname='sel_mass')
    WIDGET_CONTROL, massid, set_value = ''
  baseid1 = WIDGET_INFO(event.top, find_by_uname='widbase11')
;    WIDGET_CONTROL, baseid1, map=0

  IF NOT KEYWORD_SET(empty) THEN BEGIN
   WIDGET_CONTROL, chromid, set_value = FILE_BASENAME(chrom.fname)

   tot_uniqm = get_uniq_mass(chrom)
   IF tot_uniqm NE !NULL THEN BEGIN
     WIDGET_CONTROL, massid, set_value = STRING(tot_uniqm, FORMAT='(D14.4)')
     WIDGET_CONTROL, baseid1, map=1
   ENDIF

  ENDIF

END
