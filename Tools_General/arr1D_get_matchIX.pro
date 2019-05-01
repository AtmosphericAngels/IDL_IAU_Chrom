;+
; FUNCTION: arr1D_get_matchIX
;
; AUTHOR: F. Obersteiner, June 2017
;
; PURPOSE:
; - compare a reference 1D array to another 1D array
;
; RETURNS:
; - an index array that gives you, if applied to ref_arr, the elements of ref_arr that are also found in comp_arr
; - in case of no match, a NaN is put into the index array
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION arr1D_get_matchIX, ref_arr, comp_arr, CASE_SENSI=case_sensi, NO_MATCH_IX=no_match_ix

  ; check input argument types
  type_ref = SIZE(ref_arr, /TYPE)
  type_comp = SIZE(comp_arr, /TYPE)
  ; must be equal
  IF type_comp NE type_ref THEN RETURN, !NULL

  ; check input argument dimensions
  ndims_ref = SIZE(ref_arr, /N_DIMENSIONS)
  ndims_comp = SIZE(comp_arr, /N_DIMENSIONS)
  ; must not be greater than 1 for each argument
  IF ndims_comp + ndims_ref GT 2 THEN RETURN, !NULL

  ; check for string type and case sensitive keyword
  IF type_ref EQ 7 AND NOT KEYWORD_SET(case_sensi) THEN BEGIN
    ref_arr=STRUPCASE(TEMPORARY(ref_arr))
    comp_arr=STRUPCASE(TEMPORARY(comp_arr))
  ENDIF

  match_ix=MAKE_ARRAY(N_ELEMENTS(ref_arr), /LONG, VALUE=-1)
  FOR n=0L, N_ELEMENTS(ref_arr)-1 DO $
    IF (WHERE(comp_arr EQ ref_arr[n]))[0] NE -1 THEN match_ix[n]=n

  w_m = WHERE(match_ix NE -1, n_m)

  IF n_m GT 0 THEN $
    match_ix = match_ix[WHERE(match_ix NE -1, COMPLEMENT=no_match_ix)] $
  ELSE BEGIN
    match_ix = -1
    no_match_ix = -1
  ENDELSE


  RETURN, match_ix

END