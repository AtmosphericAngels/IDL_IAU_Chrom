;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; wid_massrat_tools
;
; AUTHOR:
; F.Obersteiner, Nov/Dec 2013, revised Jun 2014, Feb 2015
;
; INFO:
; Functions for operations on the mass ratio calculator widget
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------

FUNCTION cb_ixval, cbox_id
  WIDGET_CONTROL, cbox_id, /COMBOBOX_INDEX, get_value=cbox_content
  cbox_content = STRCOMPRESS(cbox_content, /REMOVE_ALL)
  cbox_current_text = STRCOMPRESS(WIDGET_INFO(cbox_ID, /COMBOBOX_GETTEXT), /REMOVE_ALL)
  ix = WHERE(cbox_content EQ cbox_current_text)

  strct = {ix:ix, val:cbox_current_text}

  RETURN, strct

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION find_match, value, vd
  IF SIZE(vd, /TYPE) NE 4 THEN value=ROUND(FLOAT(value))
    ix_match = WHERE(value EQ vd) ; try to find value in vd
      IF ix_match GT -1 THEN BEGIN ; value found in vd, return value and index
        v_match = (vd[ix_match])[0]
      ENDIF ELSE BEGIN ; value not found in vd, calculate closest number, info-message and return value and index
        ix_match = MIN((vd-value), ix_min, /ABSOLUT)
        v_match = vd[ix_min]
        ix_match = ix_min
  ;      msg=DIALOG_MESSAGE('Found no match for entered value. Value set to '+STRCOMPRESS(STRING(v_match), /REMOVE_ALL)+'.', /INFORMATION)
      ENDELSE
    match = {ix:ix_match, val:v_match}

  RETURN, match

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION frags2plot, event, chrom, sel_chrom, tot_uniqm

  frag1_xtm = WIDGET_INFO(event.top, find_by_uname='frag1_xtm')
  ixval=cb_ixval(frag1_xtm)
  f1_xtm = DOUBLE(ixval.val)
  match = find_match(f1_xtm, tot_uniqm)
  f1_mass = match.val

  frag2_xtm = WIDGET_INFO(event.top, find_by_uname='frag2_xtm')
  ixval=cb_ixval(frag2_xtm)
  f2_xtm = DOUBLE(ixval.val)
  match = find_match(f2_xtm, tot_uniqm)
  f2_mass = match.val

  f1_msel = WHERE(*chrom[sel_chrom].mass EQ f1_mass, nvd) ; FIX(f1_mass, type=4)
  f2_msel = WHERE(*chrom[sel_chrom].mass EQ f2_mass, nvd)

  f1_x = (*chrom[sel_chrom].time)[f1_msel]
  f2_x = (*chrom[sel_chrom].time)[f2_msel]
  f1_v = (*chrom[sel_chrom].intensity)[f1_msel]
  f2_v = (*chrom[sel_chrom].intensity)[f2_msel]

  rt_min_ID=widget_info(event.top, find_by_uname='rt_min')
  widget_control, rt_min_ID, get_value=xmin
  rt_max_ID=widget_info(event.top, find_by_uname='rt_max')
  widget_control, rt_max_ID, get_value=xmax
  xrange = [xmin, xmax]

  f1_ywindata = f1_v[WHERE(f1_x GT xmin AND f1_x LT xmax)]
  f2_ywindata = f2_v[WHERE(f2_x GT xmin AND f2_x LT xmax)]

  pyrange = [MIN(f1_ywindata, /NAN)-(MAX(f1_ywindata, /NAN)-MIN(f1_ywindata, /NAN))*0.05, $
             MAX(f1_ywindata, /NAN)+(MAX(f1_ywindata, /NAN)-MIN(f1_ywindata, /NAN))*0.05, $
             MIN(f2_ywindata, /NAN)-(MAX(f2_ywindata, /NAN)-MIN(f2_ywindata, /NAN))*0.05, $
             MAX(f2_ywindata, /NAN)+(MAX(f2_ywindata, /NAN)-MIN(f2_ywindata, /NAN))*0.05]

  yrange = [pyrange[(WHERE(pyrange EQ MIN(pyrange)))[0]], pyrange[(WHERE(pyrange EQ MAX(pyrange)))[0]]]

  pdata={f1_x   : f1_x ,$
         f1_v   : f1_v ,$
         f2_x   : f2_x ,$
         f2_v   : f2_v ,$
         xrange : xrange ,$
         yrange : yrange ,$
         f1_mass: f1_mass,$
         f2_mass: f2_mass}

  RETURN, pdata

END

;------------------------------------------------------------------------------------------------------------------------

PRO adj_plot_set, event, chrom, sel_chrom, sel_subst

  IF SIZE(chrom, /TYPE) NE 8 THEN RETURN

  psigma_left_ID = WIDGET_INFO(event.top, find_by_uname='psigma_left')
  WIDGET_CONTROL, psigma_left_ID, /COMBOBOX_INDEX, get_value=cbox_content
  vd=WHERE(cbox_content EQ chrom[sel_chrom].subst[sel_subst].sigma[0])
  WIDGET_CONTROL, psigma_left_ID, SET_COMBOBOX_SELECT=vd[0]
  psigma_right_ID = WIDGET_INFO(event.top, find_by_uname='psigma_right')
  WIDGET_CONTROL, psigma_right_ID, /COMBOBOX_INDEX, get_value=cbox_content
  vd=WHERE(cbox_content EQ chrom[sel_chrom].subst[sel_subst].sigma[1])
  WIDGET_CONTROL, psigma_right_ID, SET_COMBOBOX_SELECT=vd[0]

  rt_min_ID=widget_info(event.top, find_by_uname='rt_min')
  widget_control, rt_min_ID, set_value=chrom[sel_chrom].subst[sel_subst].rt_win[0]
  rt_max_ID=widget_info(event.top, find_by_uname='rt_max')
  widget_control, rt_max_ID, set_value=chrom[sel_chrom].subst[sel_subst].rt_win[1]

END

;------------------------------------------------------------------------------------------------------------------------

PRO adj_fragments, event

  COMMON DATA

  IF SIZE(fragdata, /TYPE) NE 8 THEN RETURN

  frag1 =     WIDGET_INFO(event.top, find_by_uname='frag1')
  frag1_xtm = WIDGET_INFO(event.top, find_by_uname='frag1_xtm')
  frag1_abd = WIDGET_INFO(event.top, find_by_uname='frag1_abd')
  frag2 =     WIDGET_INFO(event.top, find_by_uname='frag2')
  frag2_xtm = WIDGET_INFO(event.top, find_by_uname='frag2_xtm')
  frag2_abd = WIDGET_INFO(event.top, find_by_uname='frag2_abd')
  frag1_cb     = cb_ixval(frag1)
  frag1_xtm_cb = cb_ixval(frag1_xtm)
  frag2_cb     = cb_ixval(frag2)
  frag2_xtm_cb = cb_ixval(frag2_xtm)
  WIDGET_CONTROL, frag1, set_combobox_select=frag1_xtm_cb.ix
  WIDGET_CONTROL, frag2, set_combobox_select=frag2_xtm_cb.ix

  ID_subst = WIDGET_INFO(event.top, find_by_uname='subs_preset')
  sel_subst = WIDGET_INFO(ID_subst, /DROPLIST_SELECT)
  WIDGET_CONTROL, ID_subst, GET_VALUE=substlist
  sel_subst_name = substlist[sel_subst]
  w = WHERE(sel_subst_name EQ fragdata.name)
  abdlist = STRING([fragdata[w].frag0_abd, fragdata[w].frag1_abd, fragdata[w].frag2_abd, fragdata[w].frag3_abd, $
                    fragdata[w].frag4_abd, fragdata[w].frag5_abd, fragdata[w].frag6_abd], FORMAT='(F5.3)')
  WIDGET_CONTROL, frag1_abd, set_value=abdlist[frag1_xtm_cb.ix]
  WIDGET_CONTROL, frag2_abd, set_value=abdlist[frag2_xtm_cb.ix]

END

;------------------------------------------------------------------------------------------------------------------------

PRO adj_masses, event

  COMMON DATA

  IF SIZE(fragdata, /TYPE) NE 8 THEN RETURN

  frag1 =     WIDGET_INFO(event.top, find_by_uname='frag1')
  frag1_xtm = WIDGET_INFO(event.top, find_by_uname='frag1_xtm')
  frag1_abd = WIDGET_INFO(event.top, find_by_uname='frag1_abd')
  frag2 =     WIDGET_INFO(event.top, find_by_uname='frag2')
  frag2_xtm = WIDGET_INFO(event.top, find_by_uname='frag2_xtm')
  frag2_abd = WIDGET_INFO(event.top, find_by_uname='frag2_abd')
  frag1_cb     = cb_ixval(frag1)
  frag1_xtm_cb = cb_ixval(frag1_xtm)
  frag2_cb     = cb_ixval(frag2)
  frag2_xtm_cb = cb_ixval(frag1_xtm)
  WIDGET_CONTROL, frag1_xtm, set_combobox_select=frag1_cb.ix
  WIDGET_CONTROL, frag2_xtm, set_combobox_select=frag2_cb.ix

  ID_subst = WIDGET_INFO(event.top, find_by_uname='subs_preset')
  sel_subst = WIDGET_INFO(ID_subst, /DROPLIST_SELECT)
  WIDGET_CONTROL, ID_subst, GET_VALUE=substlist
  sel_subst_name = substlist[sel_subst]
  w = WHERE(sel_subst_name EQ fragdata.name)
  abdlist = STRING([fragdata[w].frag0_abd, fragdata[w].frag1_abd, fragdata[w].frag2_abd, fragdata[w].frag3_abd, $
    fragdata[w].frag4_abd, fragdata[w].frag5_abd, fragdata[w].frag6_abd], FORMAT='(F5.3)')
  WIDGET_CONTROL, frag1_abd, set_value=abdlist[frag1_cb.ix]
  WIDGET_CONTROL, frag2_abd, set_value=abdlist[frag2_cb.ix]

END

;------------------------------------------------------------------------------------------------------------------------

PRO set_fraglist, event

  COMMON DATA

  IF SIZE(fragdata, /TYPE) NE 8 THEN RETURN

  frag1 =     WIDGET_INFO(event.top, find_by_uname='frag1')
  frag1_xtm = WIDGET_INFO(event.top, find_by_uname='frag1_xtm')
  frag1_abd = WIDGET_INFO(event.top, find_by_uname='frag1_abd')
  frag2 =     WIDGET_INFO(event.top, find_by_uname='frag2')
  frag2_xtm = WIDGET_INFO(event.top, find_by_uname='frag2_xtm')
  frag2_abd = WIDGET_INFO(event.top, find_by_uname='frag2_abd')

  ID_subst = WIDGET_INFO(event.top, find_by_uname='subs_preset')
  sel_subst = WIDGET_INFO(ID_subst, /DROPLIST_SELECT)
  WIDGET_CONTROL, ID_subst, GET_VALUE=substlist
  sel_subst_name = substlist[sel_subst]
  w = WHERE(sel_subst_name EQ fragdata.name)
  fralist = STRING([fragdata[w].frag0, fragdata[w].frag1, fragdata[w].frag2, fragdata[w].frag3, $
                    fragdata[w].frag4, fragdata[w].frag5, fragdata[w].frag6], FORMAT='(A)')
    fralist = fralist[WHERE(STRCOMPRESS(fralist, /REMOVE_ALL) NE 'nd')]
  xtmlist = STRING([fragdata[w].frag0_xtm, fragdata[w].frag1_xtm, fragdata[w].frag2_xtm, fragdata[w].frag3_xtm, $
                    fragdata[w].frag4_xtm, fragdata[w].frag5_xtm, fragdata[w].frag6_xtm], FORMAT='(F12.4)')
    xtmlist = xtmlist[WHERE(STRCOMPRESS(xtmlist, /REMOVE_ALL) NE 'NaN')]
  abdlist = STRING([fragdata[w].frag0_abd, fragdata[w].frag1_abd, fragdata[w].frag2_abd, fragdata[w].frag3_abd, $
                    fragdata[w].frag4_abd, fragdata[w].frag5_abd, fragdata[w].frag6_abd], FORMAT='(F5.3)')
    abdlist = abdlist[WHERE(STRCOMPRESS(abdlist, /REMOVE_ALL) NE 'NaN')]

  WIDGET_CONTROL, frag1, SET_VALUE=fralist
  WIDGET_CONTROL, frag2, SET_VALUE=fralist
  WIDGET_CONTROL, frag1_xtm, SET_VALUE=xtmlist
  WIDGET_CONTROL, frag2_xtm, SET_VALUE=xtmlist
  WIDGET_CONTROL, frag2, set_combobox_select=1
  WIDGET_CONTROL, frag2_xtm, set_combobox_select=1
  WIDGET_CONTROL, frag1_abd, set_value=abdlist[0]
  WIDGET_CONTROL, frag2_abd, set_value=abdlist[1]

END

;------------------------------------------------------------------------------------------------------------------------

PRO set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm

  IF SIZE(chrom, /TYPE) NE 8 THEN RETURN

  pdata=frags2plot(event, chrom, sel_chrom, tot_uniqm)
;  plot_routine_pobj0, set_zero=1
  plot_routine_pobj0, pdata.f1_x, pdata.f1_v, OVER=1, XRANGE=pdata.xrange, YRANGE=pdata.yrange
  plot_routine_pobj0, pdata.f2_x, pdata.f2_v, OVER=2

  textcontent=STRARR(8)
  textcontent[0]=FILE_BASENAME(chrom[sel_chrom].fname)
  textcontent[1]='Fragment 1: '+STRTRIM(STRCOMPRESS(STRING(pdata.f1_mass, FORMAT='(F12.4)'), /REMOVE_ALL))
  textcontent[2]='Fragment 2: '+STRTRIM(STRCOMPRESS(STRING(pdata.f2_mass, FORMAT='(F12.4)'), /REMOVE_ALL))
  colors=['k','k','r','b','deep_sky_blue','m','gold']
  refresh_text_pobj0, SET_MANUAL=textcontent, SET_SUBTITLE='FragRatCalc', SET_COLORS=colors

END

;------------------------------------------------------------------------------------------------------------------------

PRO set_substlist, event, chrom

  su_psb = WIDGET_INFO(event.top, find_by_uname='su_psb')
  WIDGET_CONTROL, su_psb, map=1 ; show substance droplist base
  su_ps = WIDGET_INFO(event.top, find_by_uname='subs_preset')
  WIDGET_CONTROL, su_ps, set_value=STRING(chrom[0].subst.name)

END

;------------------------------------------------------------------------------------------------------------------------
