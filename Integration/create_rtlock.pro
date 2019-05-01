;------------------------------------------------------------------------------------------------------------------------
;+
; AUTHOR:
; H.Boenisch, S.Sala
;
; INFO:
; retention time lock functions; not in use anymore.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION create_rtlock, event, PATH=path

  IF size(rtlock, /type) eq 8 THEN tmp=temporary(rtlock)

  rtlock=create_refrt()

  id=widget_info(event.id, find_by_uname='lock')
  lock=widget_info(id, /button_set)

  IF lock NE 1 THEN return, rtlock

  rtsubst = read_subst(PATH=path, FILTER='rtlock*.info')

    rtlock=replicate(rtlock,n_elements(rtsubst))

    FOR i=0, n_elements(rtsubst)-1 DO BEGIN

  ;    refrt= $
  ;       { $
  ;         active:1,$
  ;         name:'',$
  ;         formula:'',$
  ;         mass:!Values.F_NaN,$
  ;         rt_win:fltarr(2)*!VALUES.F_NAN,$
  ;         rt:!Values.F_NaN $
  ;       }


        rtlock[i].active = 1
        rtlock[i].name = rtsubst[i].name
        rtlock[i].formula = rtsubst[i].formula
        rtlock[i].mass = rtsubst[i].mass[0]
        rtlock[i].rt_win =   rtsubst[i].rt_win
        rtlock[i].rt =  rtsubst[i].rt

    ENDFOR

  RETURN, rtlock

END

; #####################################################################################################

FUNCTION calc_rtlockwin, x, v, rtlock, rt_refwin

  rt_ref=rtlock.rt
  rt_act=fltarr(n_elements(rtlock))

  FOR i=0, n_elements(rtlock)-1 DO BEGIN

    ; Position der default - Peaks bestimmen
    pos=where(x ge rtlock[i].rt_win[0] and x le rtlock[i].rt_win[1],count)


    IF count EQ 0 THEN  return, [0,10]
    IF n_elements(x[pos]) LT 5 THEN return, [0,10]

    pfit = GAUSSFIT(x[pos], v[pos] ,coeff, nterms=5)

    pmax = max(pfit, maximalindex, /NaN)
    rt_act[i] = (x[pos])[maximalindex]

  ENDFOR

  rt_lockwin=rt_refwin+interpol(rt_act-rt_ref,rt_act,rt_refwin)

  RETURN, rt_lockwin

END