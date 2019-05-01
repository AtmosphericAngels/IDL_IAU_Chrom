;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; wid_integration_ini / wid_integration_handle
;-
;------------------------------------------------------------------------------------------------------------------------
@wid_int_tools
@viewer_tools
@call_noisecalc
@call_integration
@msinfo_tools
;------------------------------------------------------------------------------------------------------------------------
PRO wid_integration_ini

  COMMON DATA
  COMMON WIDID

;  DEVICE, Get_Screen_Size=screenSize

;+++++++++++++++++++++++
; Main / select chrom, substance, mass / Report Droplist
  intbase = WIDGET_BASE(TITLE='IAU_Chrom: Peak Integration / Noise', MBAR=intwid_men, COLUMN=1, $
                        YOFF=260, /BASE_ALIGN_CENTER); , /SCROLL, X_SCROLL_SIZE=100, Y_SCROLL_SIZE=100)
                         ;xoff=0.0*screenSize[0], yoff=0.2*screensize[1],

  fil_ID = WIDGET_BUTTON(intwid_men, Value='File', /MENU)
      ID = WIDGET_BUTTON(fil_ID, value='Exit', uname='exit_pi')

  con_ID = WIDGET_BUTTON(intwid_men, Value='Config', /MENU)
      ID = WIDGET_BUTTON(con_ID, value='Reload Settings', uname='reload_msinfo')
      ID = WIDGET_BUTTON(con_ID, value='Reload Settings NOM', uname='re_msinfo_nom')
      ID = WIDGET_BUTTON(con_ID, value='Update Species (beta)', uname='update_msinfo', /SEPARATOR)
;      ID = WIDGET_BUTTON(con_ID, value='Export defaults', uname='exp_msinfo_def', /SEPARATOR)
      ID = WIDGET_BUTTON(con_ID, value='Export Settings', uname='exp_msinfo_pres', /SEPARATOR)

  bat_ID = WIDGET_BUTTON(intwid_men, Value='BatchProcess', /MENU)
      ID = WIDGET_BUTTON(bat_ID, value='Integrate', uname='bat_int')
      ID = WIDGET_BUTTON(bat_ID, value='Integrate+Calc.Noise', uname='bat_int_noise')

  rep_ID = WIDGET_BUTTON(intwid_men, Value='Report', /MENU)
      ID = WIDGET_BUTTON(rep_ID, value='Quicklook (.ps)', uname='sav_qcklk_ps')
      ID = WIDGET_BUTTON(rep_ID, value='Report: sel. Subst.', uname='sav_txt', /SEPARATOR)
      ID = WIDGET_BUTTON(rep_ID, value='Report: all Subst.', uname='sav_all_txt')

  plo_ID = WIDGET_BUTTON(intwid_men, Value='Plot', /MENU)
      ID = WIDGET_BUTTON(plo_ID, VALUE='Recreate Textfields (Plot1)', uname='recr_txt')
      ID = WIDGET_BUTTON(plo_ID, value='Plot sel. Substance', uname='plot_rep')

  ID = WIDGET_DROPLIST(intbase, Value='',Title = 'Chromatogram ', uname='chrom', /dynamic_resize)
  WIDGET_CONTROL, ID, set_value=FILE_BASENAME(chrom.fname)
  sel_chrom=WIDGET_INFO(ID, /droplist_select)
  ID=WIDGET_DROPLIST(intbase, Value='', Title = 'Substance ', uname='name', /dynamic_resize)
  WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst.name
  sel_name=WIDGET_INFO(ID, /droplist_select)
  ID_quant=WIDGET_DROPLIST(intbase, Value='', Title = 'Fragment m/Q ', uname='mass', /dynamic_resize)

;  WIDGET_CONTROL, ID_quant, set_value=STRING(get_finval(subst[0].mass), FORMAT='(D14.4)')
;  WIDGET_CONTROL, ID_quant, set_droplist_select=subst[0].quant

  WIDGET_CONTROL, ID_quant, set_value=STRING(get_finval(chrom[0].subst[0].mass), FORMAT='(D14.4)')
  WIDGET_CONTROL, ID_quant, set_droplist_select=chrom[0].subst[0].quant

  SEP=WIDGET_LABEL(intbase, Value=' ')

;+++++++++++++++++++++++
; Flagging
  FLAGBASE=WIDGET_BASE(intbase, column=3)
    ID=WIDGET_DROPLIST(flagbase, Value='', Title = 'Flag ', uname='flag', /dynamic_resize)
    WIDGET_CONTROL, ID, set_value=['-2','-1','0','1','2']
    WIDGET_CONTROL, ID, set_droplist_select=chrom[sel_chrom].subst[sel_name].ires.flag+2
    SEP=WIDGET_LABEL(flagbase, Value='  ')
    comment=['Bad Peak ','No Peak Found','Not Integrated','Integrated','Integrated (man.)']
    ID=WIDGET_TEXT(flagbase,Value='',uname='comment', xsize=15, ysize=1, uvalue=comment);, /EDITABLE)
    WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst[sel_name].ires.comment
    SEP=WIDGET_LABEL(intbase, Value=' ')

    ; general features
    intB2=WIDGET_BASE(intbase, column=3)
    ID=WIDGET_DROPLIST(intB2, value=['OFF','0.25','0.5','1'], uname='wait', title='Delay [s]')
    SEP=WIDGET_LABEL(intB2, Value='      ')
    ID=WIDGET_BUTTON(intB2, Value='Refresh Plot0 (Overview)', uname='refresh_pobj0')
    SEP=WIDGET_LABEL(intbase, Value=' ')
    chkb = WIDGET_BASE(intbase, column=1, /NONEXCLUSIVE)
    fix_xyrange = WIDGET_BUTTON(chkb, value='Plots: Fix X-/Y-RANGE', uname='fix_xyrange', /ALIGN_CENTER)
    WIDGET_CONTROL, fix_xyrange, set_button = 0

;+++++++++++++++++++++++
; Retention time settings
  wTab0=WIDGET_TAB(intbase, LOCATION=0, MULTILINE=1)
    wT0 = WIDGET_BASE(wTab0, COLUMN=3, TITLE='Retention Time', /BASE_ALIGN_CENTER)
;    lockbase0=widget_base(wT0, TITLE='Lock', /BASE_ALIGN_CENTER, /NONEXCLUSIVE)
;      ID=widget_button(lockbase0, value='Lock RT?', uname='lock')
;      WIDGET_CONTROL, ID, set_button=0
      ID=cw_fslider(wT0, TITLE='t_R min.', Value=0, edit=1,scroll=0.1D,format='(D14.4)', $
          MINIMUM=0, MAXIMUM=MAX(*chrom[sel_chrom].time, /NAN), double=1, uname='rt_min', TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst[sel_name].rt_win[0]
      ID=cw_fslider(wT0, TITLE='t_R max.', Value=0, edit=1,scroll=0.1D,format='(D14.4)', $
            MINIMUM=0, MAXIMUM=MAX(*chrom[sel_chrom].time, /NAN), double=1, uname='rt_max', TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst[sel_name].rt_win[1]

;+++++++++++++++++++++++
; Integration method settings
  INT_TAB=WIDGET_TAB(intbase, location=0, multiline=1)
  ; Gauss and Gumble
      wT1=WIDGET_BASE(INT_TAB, column=1, title='Peak Integration', column=1, /BASE_ALIGN_CENTER)
       IM=WIDGET_DROPLIST(wT1, TITLE='Method', Value=intm.dlnames, uname='intsel')
        w_im = WHERE(intm.prgmnames EQ chrom[sel_chrom].subst[sel_name].method)
        WIDGET_CONTROL, IM, SET_DROPLIST_SELECT=w_im[0]
       BF=WIDGET_DROPLIST(wT1, TITLE='   Baseline Fitfunction', Value=['constant','linear','quadratic'], uname='bl_fitfunc')
          WIDGET_CONTROL, BF, set_droplist_select=chrom[sel_chrom].subst[sel_name].bl_type

            wT11 = WIDGET_BASE(wT1, column=5)
          sigval = [get_finval(chrom[0].subst.sigma), (INDGEN(20)+1)]
       cb_sigval = STRING((sigval[UNIQ(sigval, SORT(sigval))]), FORMAT='(F4.1)')
       sil_label = WIDGET_LABEL(wT11, Value='sigma left', /SUNKEN_FRAME)
   ID_cb_sigleft = WIDGET_COMBOBOX(wT11, value=cb_sigval, uname='sigma_left', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1, /EDITABLE)
              w0 = WHERE(cb_sigval EQ chrom[sel_chrom].subst[sel_name].sigma[0])
  WIDGET_CONTROL, ID_cb_sigleft, SET_COMBOBOX_SELECT=w0[0]
             SEP = WIDGET_LABEL(wT11, Value='     ')
       sir_label = WIDGET_LABEL(wT11, Value='sigma right', /SUNKEN_FRAME)
  ID_cb_sigright = WIDGET_COMBOBOX(wT11, value=cb_sigval, uname='sigma_right', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1, /EDITABLE)
              w1 = WHERE(cb_sigval EQ chrom[sel_chrom].subst[sel_name].sigma[1])
  WIDGET_CONTROL, ID_cb_sigright, SET_COMBOBOX_SELECT=w1[0]

 INT_APPL=WIDGET_BASE(wT1, column=2)
       ID=WIDGET_BUTTON(INT_APPL, value='Apply Settings', uname='int_manual')
       ID=WIDGET_BUTTON(INT_APPL, value='Reset to Default', uname='int_def_pres')
       ID=WIDGET_BUTTON(INT_APPL, value='Apply Settings to all', uname='int_all')
       ID=WIDGET_BUTTON(INT_APPL, value='Reset all to Default', uname='int_def_all')

       chkb = WIDGET_BASE(intbase, column=1, /NONEXCLUSIVE)
       ovwr_man = WIDGET_BUTTON(chkb, value='Overwrite manual Settings (all files)', uname='ovwr_man', /ALIGN_CENTER)
       WIDGET_CONTROL, ovwr_man, set_button = 1

;+++++++++++++++++++++++
; Noise calculation settings
  wTab2=WIDGET_TAB(intbase, location=0, multiline=1)
    wT3 = WIDGET_BASE(wTab2, column=1, TITLE='Noise Calculation', /BASE_ALIGN_CENTER)
    wT4=WIDGET_BASE(wT3, column=2)
      ID=cw_fslider(wT4, TITLE='Left Boundary', Value=0, edit=1, scroll=0.1D, format='(D14.4)', $
                    minimum=0, maximum=MAX(*chrom[sel_chrom].time, /NAN), double=1, uname='noise_lb', TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst[sel_name].noise_win[0]
      ID=cw_fslider(wT4, TITLE='Right Boundary', Value=0, edit=1, scroll=0.1D, format='(D14.4)', $
            minimum=0, maximum=MAX(*chrom[sel_chrom].time, /NAN), double=1, uname='noise_rb', TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst[sel_name].noise_win[1]

; NoiseCalc call settings
  wT4=WIDGET_BASE(wT3, column=2)
   ID=WIDGET_BUTTON(wT4, value='Apply Settings', uname='noise_pres')
   ID=WIDGET_BUTTON(wT4, value='Reset to Default', uname='noise_def_pres')
   ID=WIDGET_BUTTON(wT4, value='Apply Settings to all', uname='noise_all')
   ID=WIDGET_BUTTON(wT4, value='Reset all to Default', uname='noise_def_all')
  SEP=WIDGET_LABEL(intbase, Value=' ')

widid.intwid = intbase
WIDGET_CONTROL, intbase, /realize
XMANAGER, 'wid_integration_handle', intbase, /NO_BLOCK, event_handler='wid_integration_handle'
END

;##############################################################################################################################################################

PRO wid_integration_handle, event
  COMMON DATA
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

  check_pobjects, p_obj = ['p_obj0', 'p_obj1']

;+++++++++++++++++++++++ALWAYS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; define general settings
  int_triggers = ['int_manual','rt_min','rt_max','intsel','bl_fitfunc','sigma_left','sigma_right']
  no_int_triggers = ['Bad Peak','No Peak Found','Not Integrated', 'ovwr_man']
  flags_str = ['Bad Peak ','No Peak Found','Not Integrated','Integrated','Integrated (man.)'] ; call by flag+2
  use_nom = 0
  int_called = 0

;+++++++++++++++++++++++ALWAYS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; get current selections on widget
  IF event.top NE WIDGET_INFO(event.id, /PARENT) THEN refresh=1 ELSE refresh=0

  fix_xyrange = WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='fix_xyrange'), /BUTTON_SET)
  ovwr_man = WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='ovwr_man'), /BUTTON_SET)

  wait_ID = WIDGET_INFO(event.top, find_by_uname='wait')
  wait_sel = WIDGET_INFO(wait_ID, /DROPLIST_SELECT)
  WIDGET_CONTROL, wait_ID, GET_VALUE=value
  IF wait_sel GT 0 THEN wait = FLOAT(value[wait_sel]) ELSE wait = 0.

  ID_chrom = WIDGET_INFO(event.top, find_by_uname='chrom')
  sel_chrom = WIDGET_INFO(ID_chrom, /droplist_select)
  ID_name = WIDGET_INFO(event.top, find_by_uname='name')
  sel_name = WIDGET_INFO(ID_name, /droplist_select)
  ID_mthd = WIDGET_INFO(event.top, find_by_uname='intsel')
  sel_mthd = WIDGET_INFO(ID_mthd, /droplist_select)
  sel_intmthd = intm.prgmnames[sel_mthd]
  ID_quant = WIDGET_INFO(event.top, find_by_uname='mass')
  sel_quant = WIDGET_INFO(ID_quant, /droplist_select)


;+++++++++++++++++++++++IF_EVENT+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; reload msinfo to chromstruct: nominal masses (in case of exact masses in msinfo)
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='re_msinfo_nom') THEN BEGIN
    event.id = WIDGET_INFO(event.top, find_by_uname='reload_msinfo')
    USE_NOM=1
  ENDIF

;+++++++++++++++++++++++IF_EVENT
; reload msinfo to chromstruct
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='reload_msinfo') THEN BEGIN
    msg=DIALOG_MESSAGE('Warning: Reload will delete integration results and overwrite defaults! Continue?', /QUESTION)
      IF msg EQ 'Yes' THEN BEGIN
        refr_status, message='reloading msinfo...'
        refs=read_subst(PATH=path, USE_NOM=use_nom)
        refi=create_refi()
        subst=add_ires2subst(refs, refi)                          ; reload subst (overwrite)
        empty_chrom = create_empty_chromstrct(chrom, /CHROM_ONLY) ; reload msinfo (overwrite)
        STRUCT_ASSIGN, chrom, empty_chrom
        chrom=empty_chrom
        chrom=add_subst2chrom(chrom, subst)
        upd_intwid, event
        refr_status, message='msinfo reloaded.'
        sel_chrom = 0
        sel_name = 0
      ENDIF
  ENDIF


;+++++++++++++++++++++++IF_EVENT
; batch integrate (and calculate noise)
  bat_unlist = ['bat_int', 'bat_int_noise']
  IF TOTAL(STRMATCH(bat_unlist, WIDGET_INFO(event.id, /uname), /FOLD_CASE)) EQ 1 THEN BEGIN
    n_chrom = N_ELEMENTS(chrom.fname)
    n_subst = N_ELEMENTS(chrom[0].subst.name)

    IF (SIZE(subst, /TYPE) NE 8 OR STRLEN(subst[0].name) EQ 0) THEN BEGIN
      MSG=DIALOG_MESSAGE('Please load defaults first.', /ERROR)
      RETURN
    ENDIF

    quest = DIALOG_MESSAGE('Use defaults? Note: all previous integration results will be overwritten.', /CANCEL, /QUESTION)
    CASE quest OF
      'Cancel': RETURN
      'Yes': BEGIN
              refr_status, message='running batch method...'
              FOR sel_name=0, n_subst-1 DO BEGIN
                FOR sel_chrom=0, n_chrom-1 DO BEGIN
                  chrom[sel_chrom].subst[sel_name].rt_win=subst[sel_name].rt_win
                  chrom[sel_chrom].subst[sel_name].bl_type=subst[sel_name].bl_type
                  chrom[sel_chrom].subst[sel_name].sigma=subst[sel_name].sigma
                  chrom[sel_chrom].subst[sel_name].quant=subst[sel_name].quant
                  chrom[sel_chrom].subst[sel_name].method=subst[sel_name].method
                  IF WIDGET_INFO(event.id, /uname) EQ 'bat_int_noise' THEN $
                    call_noisecalc, sel_chrom, sel_name, event, NOISE_UNAME='noise_def_pres', PLOT=0, INSDATA_WARN=1
                  call_integration, sel_chrom, sel_name, PLOT=0
                ENDFOR
              ENDFOR
            END
      'No': BEGIN
              refr_status, message='running batch method...'
              FOR sel_name=0, n_subst-1 DO BEGIN
                FOR sel_chrom=0, n_chrom-1 DO BEGIN
                  IF WIDGET_INFO(event.id, /uname) EQ 'bat_int_noise' THEN $
                    call_noisecalc, sel_chrom, sel_name, event, NOISE_UNAME='noise_pres', PLOT=0, INSDATA_WARN=1
                  call_integration, sel_chrom, sel_name, plot=0
                ENDFOR
              ENDFOR
            END
    ENDCASE
    refr_status, message='batch method done.'
    sel_name = 0
    sel_chrom = 0
  ENDIF


;+++++++++++++++++++++++IF_EVENT
; change / set quantifier mass
  WIDGET_CONTROL, ID_quant, SET_VALUE = STRING(get_finval(chrom[sel_chrom].subst[sel_name].mass), FORMAT='(D14.4)')
  IF event.id EQ ID_quant THEN chrom[sel_chrom].subst[sel_name].quant=sel_quant $                    ; change quant in chrom strct
    ELSE sel_quant=chrom[sel_chrom].subst[sel_name].quant                                            ; ...or use quant from chrom strct
  WIDGET_CONTROL, ID_quant, SET_DROPLIST_SELECT=sel_quant


;+++++++++++++++++++++++ALWAYS
; generate intensity index reference
  v=(chrom[sel_chrom].subst[sel_name].mass)[chrom[sel_chrom].subst[sel_name].quant]
  sel_mass=matchmass(tot_uniqm, v, limit_dif=0.3)
  IF sel_mass[0] EQ -1 THEN BEGIN
    msg=DIALOG_MESSAGE(STRING(v)+': Mass not found in chromatogram.', /INFORMATION)
;    RETURN
  ENDIF
  msel=WHERE(*chrom[sel_chrom].mass EQ FIX(sel_mass[0], type=4), nvd)


;+++++++++++++++++++++++ALWAYS
; select active file and mass on widget_main
  ID=WIDGET_INFO(widid.mainwid, find_by_uname='sel_chrom')
  WIDGET_CONTROL, ID, set_droplist_select=sel_chrom
  ID=WIDGET_INFO(widid.mainwid, find_by_uname='sel_mass')
  WIDGET_CONTROL, ID, get_value=dvalue
  WIDGET_CONTROL, ID, set_droplist_select=WHERE(dvalue EQ sel_mass[0])

;+++++++++++++++++++++++ALWAYS
; check flag setting
  flag_ID = WIDGET_INFO(event.top, find_by_uname='flag')
  flag = WIDGET_INFO(flag_ID, /DROPLIST_SELECT)-2
  comment_ID=WIDGET_INFO(event.top, find_by_uname='comment')

;+++++++++++++++++++++++IF_EVENT
; check comment box, if manual entry set flag to 2 (dl position 4)
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='comment') THEN BEGIN
    WIDGET_CONTROL, comment_ID, get_value=udef_cmt
    chrom[sel_chrom].subst[sel_name].ires.comment = udef_cmt
    chrom[sel_chrom].subst[sel_name].ires.flag = 2
    WIDGET_CONTROL, flag_ID, set_droplist_select = 4
  ENDIF

;+++++++++++++++++++++++IF_EVENT
; apply flag setting / comment to chrom struct if changed manually
  IF event.id EQ flag_ID THEN BEGIN
    chrom[sel_chrom].subst[sel_name].ires.flag = flag
    WIDGET_CONTROL, flag_ID, set_droplist_select=chrom[sel_chrom].subst[sel_name].ires.flag+2
    CASE flag OF
    '-2': BEGIN
            chrom[sel_chrom].subst[sel_name].ires.comment='Bad Peak'
            chrom[sel_chrom].subst[sel_name].ires.area=!values.D_NAN
            chrom[sel_chrom].subst[sel_name].ires.rt=!values.D_NAN
            chrom[sel_chrom].subst[sel_name].ires.height=!values.D_NAN
          END
    '-1': BEGIN
            chrom[sel_chrom].subst[sel_name].ires.comment='No Peak Found'
            chrom[sel_chrom].subst[sel_name].ires.area=!values.D_NAN
            chrom[sel_chrom].subst[sel_name].ires.rt=!values.D_NAN
            chrom[sel_chrom].subst[sel_name].ires.height=!values.D_NAN
          END
    '0':  BEGIN
            chrom[sel_chrom].subst[sel_name].ires.comment='Not Integrated'
            chrom[sel_chrom].subst[sel_name].ires.area=!values.D_NAN
            chrom[sel_chrom].subst[sel_name].ires.rt=!values.D_NAN
            chrom[sel_chrom].subst[sel_name].ires.height=!values.D_NAN
          END

    '1':  BEGIN
            chrom[sel_chrom].subst[sel_name].ires.comment='Integrated'
          END

    '2':  BEGIN
            chrom[sel_chrom].subst[sel_name].ires.comment='Integrated (man.)'
            chrom[sel_chrom].subst[sel_name].ires.area=!values.D_NAN
            chrom[sel_chrom].subst[sel_name].ires.rt=!values.D_NAN
            chrom[sel_chrom].subst[sel_name].ires.height=!values.D_NAN
          END
      ENDCASE
    WIDGET_CONTROL, comment_ID, set_value=chrom[sel_chrom].subst[sel_name].ires.comment ; update comment
  ENDIF


;+++++++++++++++++++++++IF_REFRESH=1
; check if RT_min value has changed
  ID=WIDGET_INFO(event.top, find_by_uname='rt_min')
  IF refresh THEN BEGIN
    WIDGET_CONTROL, ID, get_value=val
    IF val GT chrom[sel_chrom].subst[sel_name].rt_win[1] THEN val=min(*chrom[sel_chrom].time)
    chrom[sel_chrom].subst[sel_name].rt_win[0]=val
  ENDIF
  WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst[sel_name].rt_win[0]


;+++++++++++++++++++++++IF_REFRESH=1
; check if RT_max value has changed
  ID=WIDGET_INFO(event.top, find_by_uname='rt_max')
  IF refresh THEN BEGIN
    WIDGET_CONTROL, ID, get_value=val
    IF val LT chrom[sel_chrom].subst[sel_name].rt_win[0] THEN val=max(*chrom[sel_chrom].time)
    chrom[sel_chrom].subst[sel_name].rt_win[1]=val
  ENDIF
  WIDGET_CONTROL, ID, set_value=chrom[sel_chrom].subst[sel_name].rt_win[1]


;+++++++++++++++++++++++IF_REFRESH=1
; check if baseline fitfunction value has changed
  ID=WIDGET_INFO(event.top, find_by_uname='bl_fitfunc')
  IF refresh THEN BEGIN
    index=WIDGET_INFO(ID, /droplist_select)
    chrom[sel_chrom].subst[sel_name].bl_type=index
  ENDIF
  WIDGET_CONTROL, ID, set_droplist_select=chrom[sel_chrom].subst[sel_name].bl_type


;+++++++++++++++++++++++IF_REFRESH=1
; check if int-method/sigma_left value has changed
  ID=WIDGET_INFO(event.top, find_by_uname='sigma_left')
  IF refresh THEN BEGIN
    val_ix = cbox_get_valind(ID)
      IF val_ix[1] EQ -1 THEN BEGIN
        numeric_test = valid_num(val_ix[0])
          IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 3 sigma
            msg=DIALOG_MESSAGE('Please enter a numeric value < = 100.', /INFORMATION)
            val_ix = [5., 4]
          ENDIF ELSE BEGIN
            IF val_ix[0] GT 100 OR val_ix[0] LT 0.1 THEN val_ix = [20., 19]
          ENDELSE
      ENDIF
    chrom[sel_chrom].subst[sel_name].sigma[0]=fix(val_ix[0],type=4)
    WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=val_ix[1]
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, ID, /COMBOBOX_INDEX, get_value=cbox_content
    vd=WHERE(cbox_content EQ chrom[sel_chrom].subst[sel_name].sigma[0])
    WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=vd[0]
  ENDELSE


;+++++++++++++++++++++++IF_REFRESH=1
; check if int-method/sigma_right value has changed
  ID=WIDGET_INFO(event.top, find_by_uname='sigma_right')
  IF refresh THEN BEGIN
    val_ix = cbox_get_valind(ID)
      IF val_ix[1] EQ -1 THEN BEGIN
        numeric_test = valid_num(val_ix[0])
          IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 3 sigma
            msg=DIALOG_MESSAGE('Please enter a numeric value < = 100.', /INFORMATION)
            val_ix = [5., 4]
          ENDIF ELSE BEGIN
            IF val_ix[0] GT 100 OR val_ix[0] LT 0.1 THEN val_ix = [20., 19]
          ENDELSE
      ENDIF
    chrom[sel_chrom].subst[sel_name].sigma[1]=fix(val_ix[0],type=4)
    WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=val_ix[1]
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, ID, /COMBOBOX_INDEX, get_value=cbox_content
    vd=WHERE(cbox_content EQ chrom[sel_chrom].subst[sel_name].sigma[1])
    WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=vd[0]
  ENDELSE


;+++++++++++++++++++++++IF_REFRESH=1
; check if noise left boundry value has changed
  ID_noise_lb=WIDGET_INFO(event.top, find_by_uname='noise_lb')
  IF refresh THEN BEGIN
    WIDGET_CONTROL, ID_noise_lb, get_value=noise_lb
    IF noise_lb GT chrom[sel_chrom].subst[sel_name].noise_win[1] THEN noise_lb=MIN(*chrom[sel_chrom].time)
    chrom[sel_chrom].subst[sel_name].noise_win[0]=noise_lb
  ENDIF
  WIDGET_CONTROL, ID_noise_lb, set_value=chrom[sel_chrom].subst[sel_name].noise_win[0]


;+++++++++++++++++++++++IF_REFRESH=1
; check if noise left boundry value has changed
  ID_noise_rb=WIDGET_INFO(event.top, find_by_uname='noise_rb')
  IF refresh THEN BEGIN
    WIDGET_CONTROL, ID_noise_rb, get_value=noise_rb
    IF noise_rb LT chrom[sel_chrom].subst[sel_name].noise_win[0] THEN noise_rb=MAX(*chrom[sel_chrom].time)
    chrom[sel_chrom].subst[sel_name].noise_win[1]=noise_rb
  ENDIF
  WIDGET_CONTROL, ID_noise_rb, set_value=chrom[sel_chrom].subst[sel_name].noise_win[1]




;+++++++++++++++++++++++IF_REFRESH=1
; reset plot if flag is set to a no-int-trigger or integrate if integration isn't called elsewhere
;  IF refresh THEN BEGIN
;    IF TOTAL(STRMATCH(no_int_triggers, chrom[sel_chrom].subst[sel_name].ires.comment)) EQ 1 $
;      AND event.id NE WIDGET_INFO(event.top, find_by_uname='refresh_pobj0')THEN BEGIN
;      plot_routine_pobj1, x,v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
;    ENDIF
;    IF STRUPCASE(chrom[sel_chrom].subst[sel_name].ires.comment) EQ 'INTEGRATED' $
;      AND TOTAL(STRMATCH(int_triggers, WIDGET_INFO(event.id, /uname), /FOLD_CASE)) EQ 0 $
;        AND event.id NE WIDGET_INFO(event.top, find_by_uname='refresh_pobj0') THEN call_integration, sel_chrom, sel_name, plot=1, FIX_XYRANGE=fix_xyrange
;  ENDIF




;+++++++++++++++++++++++IF_EVENT
; refresh integration for current file/substance/quantifier if one of the unames is called
; comment is set to 'Integrated (man.)', flag=2, in any of those calls
  IF TOTAL(STRMATCH(int_triggers, WIDGET_INFO(event.id, /uname), /FOLD_CASE)) EQ 1 THEN BEGIN
    chrom[sel_chrom].subst[sel_name].method = sel_intmthd
    chrom[sel_chrom].subst[sel_name].ires.flag = 2
    call_integration, sel_chrom, sel_name, plot=1, FIX_XYRANGE=fix_xyrange, MAN_FLAG=2, MAN_COMMENT='Integrated (man.)'
    int_called=1
  ENDIF


;+++++++++++++++++++++++IF_EVENT
; manual integration
;  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='int_manual') THEN BEGIN
;    call_integration, sel_chrom, sel_name, plot=1, FIX_XYRANGE=fix_xyrange, MAN_FLAG=2, MAN_COMMENT='Integrated (man.)'
;  ENDIF


;+++++++++++++++++++++++IF_EVENT
; apply selected int-method to all loaded files and refresh chrom-droplist
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='int_all') THEN BEGIN
    FOR n=0, N_ELEMENTS(chrom)-1 DO BEGIN
      IF n EQ sel_chrom THEN chrom[n].subst[sel_name].ires.flag = 0 ; reset flag for current chrom
      IF ABS(chrom[n].subst[sel_name].ires.flag) EQ 2 AND ovwr_man EQ 0 THEN CONTINUE ; skip if flag is -2 or +2 and overwrite is disabled
      chrom[n].subst[sel_name].rt_win=chrom[sel_chrom].subst[sel_name].rt_win
      chrom[n].subst[sel_name].bl_type=chrom[sel_chrom].subst[sel_name].bl_type
      chrom[n].subst[sel_name].sigma=chrom[sel_chrom].subst[sel_name].sigma
      chrom[n].subst[sel_name].quant=chrom[sel_chrom].subst[sel_name].quant
      chrom[n].subst[sel_name].method=sel_intmthd
      call_integration, n, sel_name, plot=1, FIX_XYRANGE=fix_xyrange
      WAIT, wait
    ENDFOR
;      sel_chrom = n-1                                                                                  ; select last integrated file
;      ID=WIDGET_INFO(event.top, find_by_uname='chrom')
;         WIDGET_CONTROL, ID, set_droplist_select=sel_chrom
;      IF STRUPCASE(chrom[sel_chrom].subst[sel_name].ires.comment) NE 'INTEGRATED' THEN BEGIN            ; show mass trace only
;        vd=WHERE(*chrom[sel_chrom].mass EQ chrom[sel_chrom].subst[sel_name].mass[quant] AND $
;                 *chrom[sel_chrom].time GE rt_win[0] AND *chrom[sel_chrom].time LE rt_win[1], nvd)
;        x = (*chrom[sel_chrom].time)[vd]
;        v = (*chrom[sel_chrom].intensity)[vd]
;        plot_routine_pobj1, x,v,OVER=1,XRANGE=xrange,YRANGE=yrange, FIX_XYRANGE=fix_xyrange
;      ENDIF ELSE call_integration, sel_chrom, sel_name, plot=1, FIX_XYRANGE=fix_xyrange                 ; show integration
  ENDIF


;+++++++++++++++++++++++IF_EVENT
; refresh mass-droplist and apply default settings (subst structure) to all loaded files
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='int_def_all') THEN BEGIN
    IF SIZE(subst, /TYPE) NE 8 THEN BEGIN
      msg=DIALOG_MESSAGE('No default settings loaded. Please load a subst-file when restoring the experiment.', /ERROR)
      RETURN
    ENDIF
    IF N_ELEMENTS(subst.method) LT 2 THEN BEGIN
      MSG=DIALOG_MESSAGE('No default settings found, aborting.', /ERROR)
      RETURN
    ENDIF
    IF STRLEN(subst[sel_name].method) EQ 0 THEN BEGIN
      MSG=DIALOG_MESSAGE('No default settings found, aborting.', /ERROR)
      RETURN
    ENDIF
    ID_quant=WIDGET_INFO(event.top, find_by_uname='mass')
    WIDGET_CONTROL, ID_quant, set_droplist_select=subst[sel_name].quant
    FOR n=0, N_ELEMENTS(chrom)-1 DO BEGIN
      IF n EQ sel_chrom THEN chrom[n].subst[sel_name].ires.flag = 0 ; reset flag for current chrom
      IF ABS(chrom[n].subst[sel_name].ires.flag) EQ 2 AND ovwr_man EQ 0 THEN CONTINUE ; skip if flag is -2 or +2 and overwrite is disabled
      chrom[n].subst[sel_name].rt_win=subst[sel_name].rt_win
      chrom[n].subst[sel_name].bl_type=subst[sel_name].bl_type
      chrom[n].subst[sel_name].sigma=subst[sel_name].sigma
      chrom[n].subst[sel_name].quant=subst[sel_name].quant
      chrom[n].subst[sel_name].method=subst[sel_name].method
      call_integration, n, sel_name, plot=1, FIX_XYRANGE=fix_xyrange
      WAIT, wait
    ENDFOR
;    IF STRUPCASE(chrom[sel_chrom].subst[sel_name].ires.comment) NE 'INTEGRATED' THEN BEGIN
;      vd=WHERE(*chrom[sel_chrom].mass EQ chrom[sel_chrom].subst[sel_name].mass[quant] AND $
;               *chrom[sel_chrom].time GE rt_win[0] AND *chrom[sel_chrom].time LE rt_win[1], nvd)
;      x = (*chrom[sel_chrom].time)[vd]
;      v = (*chrom[sel_chrom].intensity)[vd]
;      plot_routine_pobj1, x,v,OVER=1,XRANGE=xrange,YRANGE=yrange, FIX_XYRANGE=fix_xyrange
;    ENDIF ELSE call_integration, sel_chrom, sel_name, plot=1, FIX_XYRANGE=fix_xyrange
  ENDIF


;+++++++++++++++++++++++IF_EVENT
; restore default integration settings for current chromatogram
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='int_def_pres') THEN BEGIN
    IF SIZE(subst, /TYPE) NE 8 THEN BEGIN
      msg=DIALOG_MESSAGE('No default settings loaded. Please load a subst-file when restoring the experiment or reload an MSINFO-file.', /ERROR)
      RETURN
    ENDIF
    IF N_ELEMENTS(subst.method) LT 2 THEN BEGIN
      MSG=DIALOG_MESSAGE('No default settings found, aborting.', /ERROR)
      RETURN
    ENDIF
    IF STRLEN(subst[sel_name].method) EQ 0 THEN BEGIN
      MSG=DIALOG_MESSAGE('No default settings found, aborting.', /ERROR)
      RETURN
    ENDIF
    IF ABS(chrom[sel_chrom].subst[sel_name].ires.flag) EQ 2 AND ovwr_man EQ 0 THEN RETURN
    ID_quant=WIDGET_INFO(event.top, find_by_uname='mass')
    WIDGET_CONTROL, ID_quant, set_droplist_select=subst[sel_name].quant
    chrom[sel_chrom].subst[sel_name].rt_win=subst[sel_name].rt_win
    chrom[sel_chrom].subst[sel_name].bl_type=subst[sel_name].bl_type
    chrom[sel_chrom].subst[sel_name].sigma=subst[sel_name].sigma
    chrom[sel_chrom].subst[sel_name].quant=subst[sel_name].quant
    chrom[sel_chrom].subst[sel_name].method=subst[sel_name].method
    call_integration, sel_chrom, sel_name, plot=1, FIX_XYRANGE=fix_xyrange
  ENDIF


;+++++++++++++++++++++++IF_EVENT OR REFRESH=0
; reset plot 0
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='refresh_pobj0') OR REFRESH EQ 0 THEN BEGIN
      v=(chrom[sel_chrom].subst[sel_name].mass)[chrom[sel_chrom].subst[sel_name].quant]
      sel_mass=matchmass(tot_uniqm, v, limit_dif=0.3)
      msel=WHERE(*chrom[sel_chrom].mass EQ FIX(sel_mass[0], type=4), nvd)
    plot_routine_pobj0, (*chrom[sel_chrom].time)[msel], (*chrom[sel_chrom].intensity)[msel], OVER=1, FIX_XYRANGE=fix_xyrange
    textcontent=STRARR(8)
    textcontent[0]=FILE_BASENAME(chrom[sel_chrom].fname)
    textcontent[1]='m/Q: '+STRTRIM(STRCOMPRESS(STRING((chrom[sel_chrom].subst[sel_name].mass[chrom[sel_chrom].subst[sel_name].quant]),$
                   FORMAT = "(D14.4)")),2)
    refresh_text_pobj0, SET_MANUAL=textcontent, SET_COLORS=['k','k','k','k','k']
  ENDIF


;+++++++++++++++++++++++IF_EVENT
; call noise calculation
  noise_unlist = ['noise_pres','noise_def_pres','noise_all','noise_def_all']
  IF TOTAL(STRMATCH(noise_unlist, WIDGET_INFO(event.id, /uname), /FOLD_CASE)) EQ 1 THEN BEGIN
    noisedef_unlist = ['noise_def_pres','noise_def_all']
    IF TOTAL(STRMATCH(noisedef_unlist, WIDGET_INFO(event.id, /uname), /FOLD_CASE)) EQ 1 THEN BEGIN
      IF N_ELEMENTS(subst.method) LT 2 THEN BEGIN
        MSG=DIALOG_MESSAGE('No default settings found, aborting.', /ERROR)
        RETURN
      ENDIF
    ENDIF
    call_noisecalc, sel_chrom, sel_name, event, NOISE_UNAME=WIDGET_INFO(event.id, /uname), DELAY=wait, PLOT=1
  ENDIF


;+++++++++++++++++++++++ALWAYS
; generate / update variables 'quant' and 'rt_win' (x and v data for plot)
; generate mass trace data for plot (x, v, xrange, yrange)
; apply to plot: integrate if flag is 1 or 2, else plot m/Q trace only
  quant=chrom[sel_chrom].subst[sel_name].quant
  rt_win=[chrom[sel_chrom].subst[sel_name].rt_win[0], chrom[sel_chrom].subst[sel_name].rt_win[1]]
  vd=WHERE(*chrom[sel_chrom].mass EQ sel_mass[0] AND $
    *chrom[sel_chrom].time GE rt_win[0] AND *chrom[sel_chrom].time LE rt_win[1], nvd)
  x = (*chrom[sel_chrom].time)[vd]
  v = (*chrom[sel_chrom].intensity)[vd]
  xrange=[chrom[sel_chrom].subst[sel_name].rt_win[0], chrom[sel_chrom].subst[sel_name].rt_win[1]]
  yrange=[min(v,/nan)-((max(v, /NAN)-min(v, /NAN))*0.05), max(v,/nan)+((max(v, /NAN)-min(v, /NAN))*0.05)]

  IF NOT int_called THEN BEGIN ; call integration only if not done so yet
    CASE chrom[sel_chrom].subst[sel_name].ires.flag OF
      1: call_integration, sel_chrom, sel_name, plot=1, FIX_XYRANGE=fix_xyrange
      2: call_integration, sel_chrom, sel_name, plot=1, FIX_XYRANGE=fix_xyrange, MAN_FLAG=2, MAN_COMMENT='Integrated (man.)'
      ELSE: plot_routine_pobj1, x,v, OVER=1, XRANGE=xrange, YRANGE=yrange, FIX_XYRANGE=fix_xyrange
    ENDCASE
  ENDIF


;+++++++++++++++++++++++ALWAYS
; update fields, exclued general (sel_chrom etc.)
  upd_intwid, event, SEL_CHROM=sel_chrom, SEL_NAME=sel_name, EXCL_GENERAL=1
  IF refresh EQ 0 THEN refresh_text_pobj1, chrom, sel_chrom, sel_name, int_mass=sel_mass[0]

;+++++++++++++++++++++++IF_EVENT
; save current ires strct to txt file
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='sav_txt') THEN $
    intres2txt_sglsubst, chrom, sel_chrom, sel_name, PATH=path, ALL=0

;+++++++++++++++++++++++IF_EVENT
; save all ires to txt file
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='sav_all_txt') THEN $
    intres2txt_sglsubst, chrom, sel_chrom, sel_name, PATH=path, ALL=1

;+++++++++++++++++++++++IF_EVENT
; save all ires to txt file
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='sav_qcklk_ps') THEN $
    intres_qcklook, chrom, PATH=path


;+++++++++++++++++++++++IF_EVENT
; Replace subst, do not change intsettings.
; New default substances must contain substances in chrom[*].subst, but can contain more
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='update_msinfo') THEN BEGIN
    update_msinfo
    upd_intwid, event
    refr_status, message='msinfo updated.'
  ENDIF

;+++++++++++++++++++++++IF_EVENT
; export current integration settings of selected chrom to ms.info
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='exp_msinfo_pres') THEN $
    export_intsettings2msinfo, chrom, sel_chrom, PATH=path


;+++++++++++++++++++++++IF_EVENT
; export integration defaults (subst struct, loaded msinfo) to ms.info
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='exp_msinfo_def') THEN $
    export_subst2msinfo, subst, chrom, PATH=path


;+++++++++++++++++++++++IF_EVENT
; plot report
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='plot_rep') THEN $
    plot_intres, chrom, SEL_SUBST_IX=sel_name, /RELATIVE


;+++++++++++++++++++++++IF_EVENT
; plot1 recreate textfields
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='recr_txt') THEN $
    refresh_text_pobj1, chrom, sel_chrom, sel_name, recreate_txt=1


;+++++++++++++++++++++++IF_EVENT
; Exit: set plot object data to zero
  IF event.id EQ WIDGET_INFO(event.top, find_by_uname='exit_pi') THEN BEGIN
    plot_routine_pobj0, SET_ZERO=1                    ; reset plot0 data
    plot_routine_pobj1, SET_ZERO=1                    ; reset plot1 data
    refresh_text_pobj0, SET_ZERO=1                    ; reset plot0 info texts
    refresh_text_pobj1, chrom, 0, 0, SET_ZERO=1       ; reset plot1 info texts
    widid.intwid=-1
    IF event.SELECT THEN WIDGET_CONTROL, event.top, /DESTROY
  ENDIF

END
;##############################################################################################################################################################