;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; wid_fragrat_ini / wid_fragrat_handle
;
; MODIFICATIONS:
; F.Obersteiner, Dec 2013: introduced, revised June 2014, Feb 2015, July 2015, March 2017
;-
;------------------------------------------------------------------------------------------------------------------------
@wid_fragrat_tools
@fragrat_calc
@fragrat_tools
@read_eifrags
;------------------------------------------------------------------------------------------------------------------------
PRO wid_fragrat_ini

  COMMON DATA
  COMMON WIDID

;+++++++++++++++++++++++
; base and menu

  mr_base = WIDGET_BASE(title='FragRat Calc', mbar=mr_menu, column=1, xoff=200, yoff=260, /BASE_ALIGN_CENTER)
    file = WIDGET_BUTTON(mr_menu, Value='File', /MENU)
      ext = WIDGET_BUTTON(file, Value='Exit', uname='exit')
    config = WIDGET_BUTTON(mr_menu, Value='Config', /MENU)
      cfg = WIDGET_BUTTON(config, Value='(Re)Load Fragment Data', uname='read_fd')
      msi = WIDGET_BUTTON(config, Value='(Re)Load MSINFO', uname='read_msinfo')
    rep = WIDGET_BUTTON(mr_menu, Value='Report', /MENU)
      sav1 = WIDGET_BUTTON(rep, Value='sel. Subst, sel. File', uname='rep_cur')
      savall = WIDGET_BUTTON(rep, Value='sel. Subst, all Files', uname='rep_all')

;+++++++++++++++++++++++
; chromatogram selection
  dl = WIDGET_DROPLIST(mr_base, value='#', title = 'Chromatogram ', uname='sel_chrom', /DYNAMIC_RESIZE, /ALIGN_CENTER)
       WIDGET_CONTROL, dl, set_value=FILE_BASENAME(chrom.fname)
  sel_chrom=WIDGET_INFO(dl, /droplist_select)

;+++++++++++++++++++++++
; substance preset
  su_psb = WIDGET_BASE(mr_base, uname='su_psb', column=1, /BASE_ALIGN_CENTER)
  su_ps = WIDGET_DROPLIST(su_psb, value='#', title = 'Substance Preset ', uname='subs_preset', /DYNAMIC_RESIZE, /ALIGN_CENTER)
    IF WHERE(STRUPCASE(TAG_NAMES(chrom)) EQ 'SUBST') NE -1 THEN WIDGET_CONTROL, su_ps, set_value=chrom[0].subst.name
  sep = WIDGET_LABEL(mr_base, value='  ')

;+++++++++++++++++++++++
; fragment settings
  lbl = WIDGET_LABEL(mr_base, value='***** Fragment Selection *****', /ALIGN_CENTER)
  fragbase=WIDGET_BASE(mr_base, col=4, /BASE_ALIGN_CENTER)
  frag1_lbl=WIDGET_LABEL(fragbase, value=' Fragement 1 ')
  frag1=WIDGET_COMBOBOX(fragbase, Value='#', uname='frag1', /DYNAMIC_RESIZE)
  xtm1_lbl=WIDGET_LABEL(fragbase, value=' m/Q (F1) ')
  frag1_xtm=WIDGET_COMBOBOX(fragbase, value='#', uname='frag1_xtm', /DYNAMIC_RESIZE)
  frag2_lbl=WIDGET_LABEL(fragbase, value=' Fragement 2 ')
  frag2=WIDGET_COMBOBOX(fragbase, Value='#', uname='frag2', /DYNAMIC_RESIZE)
  xtm2_lbl=WIDGET_LABEL(fragbase, value=' m/Q (F2) ')
  frag2_xtm=WIDGET_COMBOBOX(fragbase, value='#', uname='frag2_xtm', /DYNAMIC_RESIZE)
  abdbase=WIDGET_BASE(mr_base, col=2, /BASE_ALIGN_CENTER)
  f1abd_lbl=WIDGET_LABEL(abdbase, value='Rel.Abd. (F1) ', /ALIGN_LEFT)
  f1abd=WIDGET_TEXT(abdbase, value='#', uname='frag1_abd')
  f2abd_lbl=WIDGET_LABEL(abdbase, value='Rel.Abd. (F2) ', /ALIGN_LEFT)
  f2abd=WIDGET_TEXT(abdbase, value='#', uname='frag2_abd')

  cbbase1=WIDGET_BASE(mr_base, TITLE='cbbase1', /BASE_ALIGN_CENTER, /NONEXCLUSIVE)
    ID_nom=WIDGET_BUTTON(cbbase1, value='use nominal masses?', uname='use_nom', /ALIGN_CENTER)
      WIDGET_CONTROL, ID_nom, set_button=0
    ID_dt_correct=WIDGET_BUTTON(cbbase1, value='try correct scanshift?', uname='dt_correct', /ALIGN_CENTER)
      WIDGET_CONTROL, ID_dt_correct, set_button=0

;+++++++++++++++++++++++
; retention time and peak settings
  sep = WIDGET_LABEL(mr_base, value='  ')
  lbl = WIDGET_LABEL(mr_base, value='***** Peak Detection and Integration *****', /ALIGN_CENTER)
  RTBASE=WIDGET_BASE(mr_base, col=2, /BASE_ALIGN_CENTER)
  rt_min=cw_fslider(RTBASE, TITLE='t_R min.', Value=0, edit=1,scroll=0.1D,format='(F10.4)', $
         minimum=0, maximum=max(*chrom[sel_chrom].time, /nan), double=1, uname='rt_min', TAB_MODE=1)
  rt_max=cw_fslider(RTBASE, TITLE='t_R max.', Value=0, edit=1,scroll=0.1D,format='(F10.4)', $
          minimum=0, maximum=max(*chrom[sel_chrom].time, /nan), double=1, uname='rt_max', TAB_MODE=1)

  SGBASE0=WIDGET_BASE(mr_base, col=2, /BASE_ALIGN_CENTER)
      cb_psigval = STRING((INDGEN(20)+1), FORMAT='(F4.1)')
       sil_label = WIDGET_LABEL(SGBASE0, Value='Peak Int. sigma left            ')
   ID_cb_sigleft = WIDGET_COMBOBOX(SGBASE0, Value=cb_psigval, uname='psigma_left', /DYNAMIC_RESIZE, TAB_MODE=1, /EDITABLE, /ALIGN_LEFT)
       sir_label = WIDGET_LABEL(SGBASE0, Value='Peak Int. sigma right')
  ID_cb_sigright = WIDGET_COMBOBOX(SGBASE0, Value=cb_psigval, uname='psigma_right', /DYNAMIC_RESIZE, TAB_MODE=1, /EDITABLE, /ALIGN_RIGHT)
  ID_intm = WIDGET_DROPLIST(mr_base, Value=['Baseline:P2P','Gauss:Fit2Fit'], Title='Integration/Comparison ', uname='int_type', /ALIGN_CENTER)

  sep = WIDGET_LABEL(mr_base, value='  ')
  lbl = WIDGET_LABEL(mr_base, value='***** Fragment Ratio *****', /ALIGN_CENTER)
  SGBASE1=WIDGET_BASE(mr_base, col=2, /BASE_ALIGN_CENTER)
  sil_label = WIDGET_LABEL(SGBASE1, Value='FragRat sigma left               ')
  ID_cb_sigleft = WIDGET_COMBOBOX(SGBASE1, value=STRING(0.7, FORMAT='(F4.1)'), uname='fsigma_left', /DYNAMIC_RESIZE, TAB_MODE=1, /EDITABLE, /ALIGN_LEFT)
  sir_label = WIDGET_LABEL(SGBASE1, Value='FragRat sigma right ')
  ID_cb_sigright = WIDGET_COMBOBOX(SGBASE1, value=STRING(1.8, FORMAT='(F4.1)'), uname='fsigma_right', /DYNAMIC_RESIZE, TAB_MODE=1, /EDITABLE, /ALIGN_RIGHT)
  sep = WIDGET_LABEL(mr_base, value='  ')

;+++++++++++++++++++++++
; buttons @ bottom
  sep = WIDGET_LABEL(mr_base, value='***** Run Calc... *****')
  calcbase=WIDGET_BASE(mr_base, TITLE='calcbase', /BASE_ALIGN_CENTER, col=3)
  cp = WIDGET_BUTTON(calcbase, value='Calc: Active File', uname='calc')
  ca = WIDGET_BUTTON(calcbase, value='Calc: All Files', uname='calc_all')
  ov = WIDGET_BUTTON(calcbase, value='Plot Overview', uname='plot_ovv')

  cbbase2=WIDGET_BASE(mr_base, TITLE='cbbase2', /BASE_ALIGN_CENTER, /NONEXCLUSIVE)
    ID_plot=WIDGET_BUTTON(cbbase2, value='plot results?', uname='plot_io', /ALIGN_CENTER)
      WIDGET_CONTROL, ID_plot, set_button=1

;+++++++++++++++++++++++
; realize the widget
  widid.mrwid = mr_base
  WIDGET_CONTROL, mr_base, /REALIZE
  XMANAGER, 'wid_fragrat_ini', mr_base, /NO_BLOCK, event_handler='wid_fragrat_handle'

END

;------------------------------------------------------------------------------------------------------------------------

PRO wid_fragrat_handle, event

  COMMON DATA
  COMMON FRAGDATA
  COMMON COM_PLOT
  COMMON WIDID

  IF error_handler_IO EQ 1 THEN BEGIN
    CATCH, Error_status
    IF Error_status NE 0 THEN BEGIN
      e_ix = '(!) Error '+STRING(Error_status)
      e_msg = ', '+STRING(!ERROR_STATE.MSG)
      msg = STRCOMPRESS(STRTRIM(e_ix+e_msg+' Returning.'))
      dlg = DIALOG_MESSAGE(msg, /ERROR)
      refr_status, message='returned from error to idle.'
      CATCH, /CANCEL
      RETURN
    ENDIF
  END

;+++++++++++++++++++++++
; general settings
  check_pobjects, p_obj=['p_obj0', 'p_obj1']; needs both plot windows
  uname = WIDGET_INFO(event.id, /uname)
  ID = WIDGET_INFO(event.top, find_by_uname='sel_chrom')
  sel_chrom = WIDGET_INFO(ID, /droplist_select)
  ID = WIDGET_INFO(event.top, find_by_uname='subs_preset')
  sel_subst = WIDGET_INFO(ID, /droplist_select)
  ID = WIDGET_INFO(event.top, find_by_uname='use_nom')
  use_nom = WIDGET_INFO(ID, /button_set)
  ID = WIDGET_INFO(event.top, find_by_uname='dt_correct')
  dt_correct = WIDGET_INFO(ID, /button_set)
  plot_io=WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='plot_io'), /BUTTON_SET)
  int_type=WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='int_type'), /DROPLIST_SELECT)

;+++++++++++++++++++++++
; uname cases
  CASE uname OF
    'read_fd' : $
      BEGIN
        IF SIZE(fragdata, /TYPE) EQ 8 THEN BEGIN ; fragdata exists, ask if overwrite
          msg = DIALOG_MESSAGE('Fragment Data found. Overwrite?', /QUESTION)
            IF msg EQ 'Yes' THEN fragdata = read_eifrags(path=path)
        ENDIF ELSE fragdata = read_eifrags(path=path)
        IF WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1) EQ -1 THEN BEGIN ; msinfo not loaded yet
          refs = read_subst(path=path)
          IF STRLEN(refs[0].name) EQ 0 THEN RETURN ; no substances in msinfo
          refi = create_refi()
          subst=add_ires2subst(refs, refi)
          chrom=add_subst2chrom(chrom, subst)
        ENDIF
        set_substlist, event, chrom
        set_fraglist, event
      END
    ; **********************************************************************************************************************************************
    'read_msinfo' : $
      BEGIN
        IF WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1) EQ -1 THEN BEGIN ; msinfo not loaded yet
          refs = read_subst(path=path)
          IF STRLEN(refs[0].name) EQ 0 THEN RETURN ; no substances in msinfo
          refi = create_refi()
          subst=add_ires2subst(refs, refi)
          chrom=add_subst2chrom(chrom, subst)
        ENDIF ELSE BEGIN ; msinfo loaded, ask for overwrite
          msg=DIALOG_MESSAGE('MSINFO found. Reloading the MSINFO-file will delete all integration results! Press "No" to load present MSINFO or "Yes" to reload.', /QUESTION)
          IF msg EQ 'Yes' THEN BEGIN
            refr_status, message='reloading msinfo...'
            refs=read_subst(PATH=path, USE_NOM=use_nom)
            refi = create_refi()
            subst=add_ires2subst(refs, refi)                          ; reload subst (overwrite)
            empty_chrom = create_empty_chromstrct(chrom, /CHROM_ONLY) ; reload msinfo (overwrite)
            STRUCT_ASSIGN, chrom, empty_chrom
            chrom = empty_chrom
            chrom=add_subst2chrom(chrom, subst)
            refr_status, message='msinfo reloaded.'
          ENDIF
        ENDELSE
          set_substlist, event, chrom
        IF SIZE(fragdata, /TYPE) EQ 8 THEN BEGIN
          set_fraglist, event
        ENDIF ELSE RETURN
      END
    ; **********************************************************************************************************************************************
    'sel_chrom' : $
      BEGIN
        IF SIZE(fragdata, /TYPE) NE 8 THEN fragdata=read_eifrags(path=path)
        IF WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1) EQ -1 THEN BEGIN ; msinfo not loaded yet
          refs = read_subst(path=path)
          IF STRLEN(refs[0].name) EQ 0 THEN RETURN ; no substances in msinfo
          refi = create_refi()
          subst=add_ires2subst(refs, refi)
          chrom=add_subst2chrom(chrom, subst)
          set_substlist, event, chrom
        ENDIF
        set_fraglist, event
        adj_plot_set, event, chrom, sel_chrom, sel_subst
        set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm
      END
    ; **********************************************************************************************************************************************
    'subs_preset' : $
      BEGIN
        IF SIZE(fragdata, /TYPE) NE 8 THEN fragdata=read_eifrags(path=path)
        IF WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1) EQ -1 THEN BEGIN ; msinfo not loaded yet
          refs = read_subst(path=path)
          IF STRLEN(refs[0].name) EQ 0 THEN RETURN ; no substances in msinfo
          refi = create_refi()
          subst=add_ires2subst(refs, refi)
          chrom=add_subst2chrom(chrom, subst)
          set_substlist, event, chrom
        ENDIF
        set_fraglist, event
        adj_plot_set, event, chrom, sel_chrom, sel_subst
        set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm
      END
    ; **********************************************************************************************************************************************
    'frag1' : $
      BEGIN
        adj_masses, event
        set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm
      END
    ; **********************************************************************************************************************************************
    'frag1_xtm' : $
      BEGIN
        adj_fragments, event
        set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm
      END
    ; **********************************************************************************************************************************************
    'frag2' : $
      BEGIN
        adj_masses, event
        set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm
      END
    ; **********************************************************************************************************************************************
    'frag2_xtm' : $
      BEGIN
        adj_fragments, event
        set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm
      END
    ; **********************************************************************************************************************************************
    'rt_min': $
      BEGIN
        rt_min_ID=WIDGET_INFO(event.top, find_by_uname='rt_min')
        WIDGET_CONTROL, rt_min_ID, get_value=rt_min
        IF rt_min GT chrom[sel_chrom].subst[sel_subst].rt_win[1] THEN rt_min = MIN(*chrom[sel_chrom].time)
        chrom[sel_chrom].subst[sel_subst].rt_win[0] = rt_min
        WIDGET_CONTROL, rt_min_ID, set_value=chrom[sel_chrom].subst[sel_subst].rt_win[0]
        set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm
      END
    ; **********************************************************************************************************************************************
    'rt_max': $
      BEGIN
        rt_max_ID=WIDGET_INFO(event.top, find_by_uname='rt_max')
        WIDGET_CONTROL, rt_max_ID, get_value=rt_max
        IF rt_max LT chrom[sel_chrom].subst[sel_subst].rt_win[0] THEN rt_max = MAX(*chrom[sel_chrom].time)
        chrom[sel_chrom].subst[sel_subst].rt_win[1] = rt_max
        WIDGET_CONTROL, rt_max_ID, set_value=chrom[sel_chrom].subst[sel_subst].rt_win[1]
        set_fragplot0, event, chrom, sel_chrom, sel_subst, tot_uniqm
      END
    ; **********************************************************************************************************************************************
    'psigma_left': $
      BEGIN
      psigmal_ID=WIDGET_INFO(event.top, find_by_uname='psigma_left')
      ixval = cb_ixval(psigmal_ID)
        IF ixval.ix EQ -1 THEN BEGIN                                                   ; manual entry
          numeric_test = valid_num(ixval.val)
          IF numeric_test EQ 0 THEN BEGIN                                              ; num test NOT passed, reset to default 3 sigma
            msg=DIALOG_MESSAGE('Please enter a numeric value < = 100.', /INFORMATION)
            ixval = {ix:2, val:3.}
          ENDIF ELSE BEGIN
            IF ixval.val GT 100 OR ixval.val LT 0.1 THEN ixval = {ix:19, val:20.}      ; numtest passed but value out of range
          ENDELSE
        ENDIF

        WIDGET_CONTROL, psigmal_ID, SET_COMBOBOX_SELECT=ixval.ix
      END
    ; **********************************************************************************************************************************************
    'psigma_right': $
      BEGIN
      psigmar_ID=WIDGET_INFO(event.top, find_by_uname='psigma_right')
      ixval = cb_ixval(psigmar_ID)
        IF ixval.ix EQ -1 THEN BEGIN
          numeric_test = valid_num(ixval.val)
          IF numeric_test EQ 0 THEN BEGIN
            msg=DIALOG_MESSAGE('Please enter a numeric value < = 100.', /INFORMATION)
            ixval = {ix:2, val:3.}
          ENDIF ELSE BEGIN
            IF ixval.val GT 100 OR ixval.val LT 0.1 THEN ixval = {ix:19, val:20.}
          ENDELSE
        ENDIF

        WIDGET_CONTROL, psigmar_ID, SET_COMBOBOX_SELECT=ixval.ix
      END
    ; **********************************************************************************************************************************************
    'fsigma_left': $
      BEGIN
        fsigmal_ID=widget_info(event.top, find_by_uname='fsigma_left')
        ixval = cb_ixval(fsigmal_ID)
        IF ixval.ix EQ -1 THEN BEGIN                                                   ; manual entry
          numeric_test = valid_num(ixval.val)
          IF numeric_test EQ 0 THEN BEGIN                                              ; num test NOT passed, reset to default 3 sigma
            msg=DIALOG_MESSAGE('Please enter a numeric value < = 10.', /INFORMATION)
            ixval = {ix:0, val:1.}
          ENDIF ELSE BEGIN
            IF ixval.val GT 10 OR ixval.val LT 0.1 THEN ixval = {ix:0, val:1.}      ; numtest passed but value out of range
          ENDELSE
        ENDIF
        WIDGET_CONTROL, fsigmal_ID, SET_COMBOBOX_SELECT=ixval.ix
      END
    ; **********************************************************************************************************************************************
    'fsigma_right': $
      BEGIN
        fsigmar_ID=widget_info(event.top, find_by_uname='fsigma_right')
        ixval = cb_ixval(fsigmar_ID)
        IF ixval.ix EQ -1 THEN BEGIN
          numeric_test = valid_num(ixval.val)
          IF numeric_test EQ 0 THEN BEGIN
            msg=DIALOG_MESSAGE('Please enter a numeric value < = 10.', /INFORMATION)
            ixval = {ix:2, val:3.}
          ENDIF ELSE BEGIN
            IF ixval.val GT 10 OR ixval.val LT 0.1 THEN ixval = {ix:2, val:3.}
          ENDELSE
        ENDIF
        chrom[sel_chrom].subst[sel_subst].sigma[1]=fix(ixval.val,type=4)
        WIDGET_CONTROL, fsigmar_ID, SET_COMBOBOX_SELECT=ixval.ix
      END
    ; **********************************************************************************************************************************************
    'calc' : $
      BEGIN
        IF SIZE(fragdata, /TYPE) NE 8 THEN BEGIN
          msg=DIALOG_MESSAGE('Please load Fragment Data first.', /INFORMATION)
          RETURN
        ENDIF
        adj_fragments, event
        fragres = {}
        fragres = fragrat_calc(event, chrom, fragdata, tot_uniqm, USE_NOM=use_nom, INT_TYPE=int_type, PLOT=plot_io, $
                               DT_CORRECT=dt_correct)
      END
    ; **********************************************************************************************************************************************
    'calc_all' : $
      BEGIN
        IF SIZE(fragdata, /TYPE) NE 8 THEN BEGIN
          msg=DIALOG_MESSAGE('Please load Fragment Data first.', /INFORMATION)
          RETURN
        ENDIF
        refr_status, message='calculating fragment ratios...'
        adj_fragments, event
        allfragres = {}
          ID = WIDGET_INFO(event.top, find_by_uname='sel_chrom')
        FOR i=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN
          WIDGET_CONTROL, ID, SET_DROPLIST_SELECT=i
          res = fragrat_calc(event, chrom, fragdata, tot_uniqm, USE_NOM=use_nom, INT_TYPE=int_type, PLOT=plot_io)
          allfragres=[allfragres, res]
        ENDFOR
        refr_status, message='fragrat calculation done.'
      END
    ; **********************************************************************************************************************************************
    'plot_ovv' : $
      BEGIN
        IF ALLFRAGRES NE !null THEN $
          p=plot(INDGEN(N_ELEMENTS(allfragres.ratio)), allfragres.ratio, XTITLE='run no.', YTITLE='ratio', $
                 XGRIDSTYLE=2, XSUBGRIDSTYLE=6, XTICKLEN=1)
      END
    ; **********************************************************************************************************************************************
    'rep_cur': $
      BEGIN
        fragres2txt_cur, PATH=path
      END
    ; **********************************************************************************************************************************************
    'rep_all': $
      BEGIN
        fragres2txt_all, PATH=path
      END
    ; **********************************************************************************************************************************************
    'resplots' : $
      BEGIN
        refresh_text_pobj0, SET_ZERO=1
        plot_routine_pobj0, SET_ZERO=1
        plot_routine_pobj1, SET_ZERO=1
        refresh_text_pobj1, SET_ZERO=1
      END
    ; **********************************************************************************************************************************************
    'use_nom': $
      BEGIN
        ID = WIDGET_INFO(event.top, find_by_uname='use_nom')
        use_nom = WIDGET_INFO(ID, /button_set)
      END
    ; **********************************************************************************************************************************************
    'dt_correct': $
      BEGIN
        ID = WIDGET_INFO(event.top, find_by_uname='dt_correct')
        dt_correct = WIDGET_INFO(ID, /button_set)
      END
    ; **********************************************************************************************************************************************
    'plot_io': $
      BEGIN
      END
      ; **********************************************************************************************************************************************
    'int_type': $
      BEGIN
      END
    ; **********************************************************************************************************************************************
    'exit' : $
      BEGIN
        widid.mrwid = 0
        plot_routine_pobj0, SET_ZERO=1
        refresh_text_pobj0, SET_ZERO=1
        plot_routine_pobj1, SET_ZERO=1
        refresh_text_pobj1, SET_ZERO=1
        IF event.SELECT THEN WIDGET_CONTROL, event.TOP, /DESTROY
      END
  ENDCASE

END