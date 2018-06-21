FUNCTION get_finval, array

  w_finite = WHERE(FINITE(array) EQ 1, n_fin)

  IF n_fin EQ 0 THEN RETURN, !NULL $
  ELSE RETURN, array[w_finite]

END

; **************************************************************************************

FUNCTION get_finite_xv, x, v

  emptystrct={x:[0,0], v:[0,0]}

  IF N_ELEMENTS(x) NE N_ELEMENTS(v) THEN RETURN, emptystrct ; x & v must be of same size

  w_x_finite=WHERE(FINITE(x) EQ 1)
  IF N_ELEMENTS(w_x_finite) EQ 1 THEN BEGIN
    IF w_x_finite EQ -1 THEN RETURN, emptystrct             ; NaN not allowed in x
  ENDIF

  w_v_finite=WHERE(FINITE(v) EQ 1)
  IF N_ELEMENTS(w_v_finite) EQ 1 THEN BEGIN
    IF w_v_finite EQ -1 THEN RETURN, emptystrct             ; v did not contain finite values
  ENDIF

  IF N_ELEMENTS(x[w_v_finite]) EQ 1 THEN BEGIN              ; ensure return variable type array(2)
    x_res=[x[w_v_finite], MAX(x)+2]                          ; 2: offset out of plotting range
    v_res=[v[w_v_finite], 0]
  ENDIF ELSE BEGIN
    x_res=x[w_v_finite]                                     ; 2: offset out of plotting range
    v_res=v[w_v_finite]
  ENDELSE

  strct = {x: x_res, $
    v: v_res}

  RETURN, strct

END