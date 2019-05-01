;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; wid_integration_tools
;
; AUTHOR:
; F.Obersteiner Dec 2013
;
; PROCEDURES:
; upd_all_fields - updates all fields (droplists etc.) of the integration widget
;-
;------------------------------------------------------------------------------------------------------------------------
@viewer_tools
;------------------------------------------------------------------------------------------------------------------------
PRO upd_intwid, event, SEL_CHROM=sel_chrom, SEL_NAME=sel_name, EXCL_GENERAL=excl_general

  COMMON DATA

  IF NOT KEYWORD_SET(sel_chrom) THEN sel_chrom=0
  IF NOT KEYWORD_SET(sel_name) THEN sel_name=0
  IF NOT KEYWORD_SET(excl_general) THEN excl_general=0

  ;+++++++++++++++++++++++
  ; General, only if keyword exclude general is flase
  IF KEYWORD_SET(excl_general) EQ 0 THEN BEGIN
    ID=WIDGET_INFO(event.top, find_by_uname='chrom')
    WIDGET_CONTROL, ID, set_value=FILE_BASENAME(chrom.fname)

    ID=WIDGET_INFO(event.top, find_by_uname='name')
    WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst.name

    ID=WIDGET_INFO(event.top, find_by_uname='mass')
    WIDGET_CONTROL, ID, set_value=STRING(get_finval(chrom[sel_chrom].subst[sel_name].mass), FORMAT='(D14.4)')
    WIDGET_CONTROL, ID, set_droplist_select=chrom[sel_chrom].subst[sel_name].quant
  ENDIF

;+++++++++++++++++++++++
; Flagging
  ID=WIDGET_INFO(event.top, find_by_uname='flag')
  WIDGET_CONTROL, ID, set_value=['-2','-1','0','1','2']
  WIDGET_CONTROL, ID, set_droplist_select=chrom[sel_chrom].subst[sel_name].ires.flag+2

  ID=WIDGET_INFO(event.top, find_by_uname='comment')
  WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst[sel_name].ires.comment

;+++++++++++++++++++++++
; Retention time settings
;  ID=WIDGET_INFO(event.top, find_by_uname='lock')
;  WIDGET_CONTROL, ID, set_button=0

  minimum = 0.
  maximum = MAX(*chrom[sel_chrom].time, /NAN)
  v_rtmin = chrom[sel_chrom].subst[sel_name].rt_win[0]
  v_rtmax = chrom[sel_chrom].subst[sel_name].rt_win[1]

  ID=WIDGET_INFO(event.top, find_by_uname='rt_min')
  WIDGET_CONTROL, ID, SET_VALUE=[v_rtmin, minimum, maximum]

  ID=WIDGET_INFO(event.top, find_by_uname='rt_max')
  WIDGET_CONTROL, ID, SET_VALUE=[v_rtmax, minimum, maximum]

;+++++++++++++++++++++++
; Integration method settings
  ID=WIDGET_INFO(event.top, find_by_uname='intsel')
  WIDGET_CONTROL, ID, set_droplist_select=WHERE(chrom[sel_chrom].subst[sel_name].method EQ intm.prgmnames)

  ID=WIDGET_INFO(event.top, find_by_uname='bl_fitfunc')
  WIDGET_CONTROL, ID, set_droplist_select=chrom[sel_chrom].subst[sel_name].bl_type

  ID=WIDGET_INFO(event.top, find_by_uname='sigma_left')
  WIDGET_CONTROL, ID, GET_VALUE=sigvals
  sigma_left=chrom[sel_chrom].subst[sel_name].sigma[0]
  w_sig = WHERE(sigvals EQ sigma_left)
  IF w_sig NE -1 THEN WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=w_sig[0] $
    ELSE BEGIN
      WIDGET_CONTROL, ID, COMBOBOX_ADDITEM=sigma_left
      WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=N_ELEMENTS(sigvals)
    ENDELSE

  ID=WIDGET_INFO(event.top, find_by_uname='sigma_right')
  WIDGET_CONTROL, ID, GET_VALUE=sigvals
  sigma_right=chrom[sel_chrom].subst[sel_name].sigma[1]
  w_sig = WHERE(sigvals EQ sigma_right)
  IF w_sig NE -1 THEN WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=w_sig[0] $
    ELSE BEGIN
      WIDGET_CONTROL, ID, COMBOBOX_ADDITEM=sigma_right
      WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=N_ELEMENTS(sigvals)
    ENDELSE

;+++++++++++++++++++++++
; Noise calculation settings

  v_noisemin = chrom[sel_chrom].subst[sel_name].noise_win[0]
  v_noisemax = chrom[sel_chrom].subst[sel_name].noise_win[1]

  ID=WIDGET_INFO(event.top, find_by_uname='noise_lb')
  WIDGET_CONTROL, ID, SET_VALUE=[v_noisemin, minimum, maximum]

  ID=WIDGET_INFO(event.top, find_by_uname='noise_rb')
  WIDGET_CONTROL, ID, SET_VALUE=[v_noisemax, minimum, maximum]

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION matchmass, vd, v, LIMIT_DIF=limit_dif; adapted from viewer_tools / FUNCTION cbox_matchval

  match_valind = [0.,0.,0.]
    IF valid_num(v) EQ 0 THEN RETURN, match_valind

  v=FLOAT(v)
  match_ind = WHERE(v EQ vd) ; try to find v in vd
  match_dif = 0.

  IF match_ind[0] GT -1 THEN BEGIN ; value found in vd, return value and index
    match_val = vd[match_ind]
  ENDIF ELSE BEGIN ; v not found in vd, calculate closest number
    match_ind = MIN((vd-v), min_index, /ABSOLUT)
    match_val = vd[min_index]
    match_dif = ABS(v-match_val)
    match_ind = min_index
  ENDELSE

  match_valind = [match_val, match_ind, match_dif]

  IF KEYWORD_SET(limit_dif) THEN BEGIN
    IF match_dif GT limit_dif THEN match_valind = [-1, -1, match_dif]
  ENDIF


  RETURN, match_valind

END

;------------------------------------------------------------------------------------------------------------------------

PRO wid_int_tools

END