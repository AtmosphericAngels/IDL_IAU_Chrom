;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO wid_mmassviewer_ini / PRO wid_mmassviewer_handle
;
; AUTHOR:
; F.Obersteiner, Nov.2013. Last modified July 2015.
;-
;------------------------------------------------------------------------------------------------------------------------
@calc_tic
@viewer_tools
;------------------------------------------------------------------------------------------------------------------------

PRO wid_mmassviewer_ini

  COMMON DATA
  COMMON WIDID

  DEVICE, Get_Screen_Size = ScreenSize
          XCenter = FIX(ScreenSize[0])
          YCenter = FIX(ScreenSize[1])

  mmviewerbase = WIDGET_BASE(title='Viewer: Multiple Masses', mbar=mmv_menu, column=1, $
                xoff=0.10*screenSize[0], yoff=0.05*screensize[1], /BASE_ALIGN_CENTER)

      f_men = WIDGET_BUTTON(mmv_menu, VALUE='File', /MENU)
        f_ext = WIDGET_BUTTON(f_men, VALUE='Exit', uname='exit')
      c_men = WIDGET_BUTTON(mmv_menu, VALUE='Config', /MENU)
        c_rls = WIDGET_BUTTON(c_men, value='(Re-)Load MSINFO', uname='load_subst')
      p_men = WIDGET_BUTTON(mmv_menu, VALUE='Plot', /MENU)
        p_res = WIDGET_BUTTON(p_men, VALUE='Reset', uname='reset')
        p_rct = WIDGET_BUTTON(p_men, VALUE='Recreate Textfields', uname='recr_txt', /SEPARATOR)
        p_gel = WIDGET_BUTTON(p_men, VALUE='Generate Legend', uname='gen_leg')

      ID = WIDGET_DROPLIST(mmviewerbase, value='', title = 'Chromatogram ', uname='sel_chrom', /DYNAMIC_RESIZE, /ALIGN_CENTER)
        WIDGET_CONTROL, ID, set_value=FILE_BASENAME(chrom.fname)
      su_psb = WIDGET_BASE(mmviewerbase, uname='su_psb', column=1, map=0)
      su_ps = WIDGET_DROPLIST(su_psb, value='', title = 'Substance Preset ', uname='subs_preset', /DYNAMIC_RESIZE, /ALIGN_CENTER)
      su_w = WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1)
        IF su_w[0] NE -1 THEN BEGIN
          WIDGET_CONTROL, su_ps, set_value=chrom[0].subst.name
          WIDGET_CONTROL, su_psb, map=1
        ENDIF
      sep = WIDGET_LABEL(mmviewerbase, Value=' *** ')

      cb_mass_value = ['none', 'TIC', STRING(tot_uniqm, FORMAT='(D14.4)')]             ; generate content for mass-comboboxes
      lbl = WIDGET_LABEL(mmviewerbase, value='m/Q_0 (black)', uname='sel_mass0_lbl', /DYNAMIC_RESIZE, /ALIGN_CENTER)
      ID = WIDGET_COMBOBOX(mmviewerbase, value='#', uname='sel_mass0', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value = cb_mass_value

      lbl = WIDGET_LABEL(mmviewerbase, value='  ', /ALIGN_LEFT)
      lbl = WIDGET_LABEL(mmviewerbase, value='m/Q_1 (red)', uname='sel_mass1_lbl', /DYNAMIC_RESIZE, /ALIGN_CENTER)
      ID = WIDGET_COMBOBOX(mmviewerbase, value='#', uname='sel_mass1', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value = cb_mass_value

      lbl = WIDGET_LABEL(mmviewerbase, value='  ', /ALIGN_LEFT)
      lbl = WIDGET_LABEL(mmviewerbase, value='m/Q_2 (green)', uname='sel_mass2_lbl', /DYNAMIC_RESIZE, /ALIGN_CENTER)
      ID = WIDGET_COMBOBOX(mmviewerbase, value='#', uname='sel_mass2', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value = cb_mass_value

      lbl = WIDGET_LABEL(mmviewerbase, value='  ', /ALIGN_LEFT)
      lbl = WIDGET_LABEL(mmviewerbase, value='m/Q_3 (blue)', uname='sel_mass3_lbl', /DYNAMIC_RESIZE, /ALIGN_CENTER)
      ID = WIDGET_COMBOBOX(mmviewerbase, value='#', uname='sel_mass3', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value = cb_mass_value

      lbl = WIDGET_LABEL(mmviewerbase, value='  ', /ALIGN_LEFT)
      lbl = WIDGET_LABEL(mmviewerbase, value='m/Q_4 (cyan)', uname='sel_mass4_lbl', /DYNAMIC_RESIZE, /ALIGN_CENTER)
      ID = WIDGET_COMBOBOX(mmviewerbase, value='#', uname='sel_mass4', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value = cb_mass_value

      lbl = WIDGET_LABEL(mmviewerbase, value=' ', /ALIGN_LEFT)
      lbl = WIDGET_LABEL(mmviewerbase, value='m/Q_5 (magenta)', uname='sel_mass5_lbl', /DYNAMIC_RESIZE, /ALIGN_CENTER)
      ID = WIDGET_COMBOBOX(mmviewerbase, value='#', uname='sel_mass5', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value = cb_mass_value

      lbl = WIDGET_LABEL(mmviewerbase, value='  ', /ALIGN_LEFT)
      lbl = WIDGET_LABEL(mmviewerbase, value='m/Q_6 (yellow)', uname='sel_mass6_lbl', /DYNAMIC_RESIZE, /ALIGN_CENTER)
      ID = WIDGET_COMBOBOX(mmviewerbase, value='#', uname='sel_mass6', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
      WIDGET_CONTROL, ID, set_value = cb_mass_value

      sep = WIDGET_LABEL(mmviewerbase, Value=' *** ')
      chkb = WIDGET_BASE(mmviewerbase, column=1, /NONEXCLUSIVE)
      fix_xyrange = WIDGET_BUTTON(chkb, value='Fix X-/Y-RANGE (Plot0)', uname='fix_xyrange', /ALIGN_CENTER)
      WIDGET_CONTROL, fix_xyrange, set_button = 0
      substr_bgr = WIDGET_BUTTON(chkb, value='Substract Min.Intensity in Plot X-RANGE', uname='substr_bgr', /ALIGN_CENTER)
      WIDGET_CONTROL, substr_bgr, set_button = 0

      sep = WIDGET_LABEL(mmviewerbase, Value=' ')

    widid.mmviewerwid = mmviewerbase
    WIDGET_CONTROL, mmviewerbase, /REALIZE
    XMANAGER, 'wid_mmassviewer_handle', mmviewerbase, /NO_BLOCK, event_handler='wid_mmassviewer_handle'

END

; ********************************************************************************************************************************************************

PRO wid_mmassviewer_handle, event

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


  check_pobjects, p_obj=['p_obj0', ''] ; needs only plot 0
  uname = WIDGET_INFO(event.id, /uname)
  uname_chrom_list = ['sel_chrom']
  uname_mass_list = ['sel_mass0','sel_mass1','sel_mass2','sel_mass3','sel_mass4','sel_mass5','sel_mass6']
  uname_selmass_lbl_list = ['sel_mass0_lbl','sel_mass1_lbl','sel_mass2_lbl','sel_mass3_lbl','sel_mass4_lbl','sel_mass5_lbl','sel_mass6_lbl']
  n_plot = 7 ; maximum number of mass traces to plot
  mmv_subtitle = 'Viewer: Multiple Masses'
  fix_xyrange=WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='fix_xyrange'), /BUTTON_SET)
  bgr_substr=WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='substr_bgr'), /BUTTON_SET)

  selchrom_id = WIDGET_INFO(event.top, find_by_uname='sel_chrom') ; chrom 0 selection
  dsel = WIDGET_INFO(selchrom_id, /droplist_select)

  IF chrom[0].instr_type EQ 3 THEN nomonly = 1 ELSE nomonly = 0 ; masses with label 'nominal' only for HTOF data

  CASE uname OF
     'exit' : $
       BEGIN
         refresh_text_pobj0, SET_ZERO=1                                 ; reset plot and textfields
         plot_routine_pobj0, SET_ZERO=1
         FREEVAR, cb_mass_strct                                         ; clear variables from memory
         widid.mmviewerwid = -1                                           ; set viewer widget id to zero
         IF event.SELECT THEN WIDGET_CONTROL, event.top, /DESTROY       ; close widget window
      END

     'reset': $
       BEGIN
         FREEVAR, cb_mass_strct                                         ; free variables

         FOR i=0, N_ELEMENTS(uname_mass_list)-1 DO BEGIN                ; reset all comboboxes
         ID = WIDGET_INFO(event.top, find_by_uname=uname_mass_list[i])
            WIDGET_CONTROL, ID, SET_COMBOBOX_SELECT=0
         ENDFOR
         WIDGET_CONTROL, selchrom_id, SET_DROPLIST_SELECT=0
         ID = WIDGET_INFO(event.top, find_by_uname='fix_xyrange')
            WIDGET_CONTROL, ID, SET_BUTTON=0
         ID = WIDGET_INFO(event.top, find_by_uname='substr_bgr')
            WIDGET_CONTROL, ID, SET_BUTTON=0

         refresh_text_pobj0, SET_ZERO=1                                 ; reset plot and textfields
         plot_routine_pobj0, SET_ZERO=1
       END

     'load_subst' : $
       BEGIN
        IF WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1) EQ -1 THEN BEGIN ; msinfo not loaded yet
          refs = read_subst(path = path)
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
         su_psb = WIDGET_INFO(event.top, find_by_uname='su_psb')
         WIDGET_CONTROL, su_psb, map=1 ; show substance droplist base
         su_ps = WIDGET_INFO(event.top, find_by_uname='subs_preset')
         WIDGET_CONTROL, su_ps, set_value=STRING(chrom[0].subst.name) ; fill droplist with substance list
       END

     'subs_preset' : $
       BEGIN
         su_ps = WIDGET_INFO(event.top, find_by_uname='subs_preset')
         subst_sel = WIDGET_INFO(su_ps, /DROPLIST_SELECT)
         subst_sel_masses = chrom[dsel].subst[subst_sel].mass
         subst_rel_abds = chrom[dsel].subst[subst_sel].rel_abd

         FOR i=0, N_ELEMENTS(subst_sel_masses)-1 DO BEGIN ; loop to set cboxes to substance fragment masses from msinfo file

           IF i GT N_ELEMENTS(uname_mass_list)-1 THEN BREAK ; abort if more masses specified than can be displayed

           sel_mass=matchmass(tot_uniqm, subst_sel_masses[i], limit_dif=.5)
           ix_sel_mass = sel_mass[1] ; will be -1 if mass not found

           IF (ix_sel_mass EQ -1) THEN ix_add = 1 ELSE ix_add = 2
           IF FINITE(subst_sel_masses[i]) EQ 0 THEN ix_add = 0 ; mass was NaN, select 'none' in combobox

           cbi_id = WIDGET_INFO(event.top, find_by_uname=uname_mass_list[i])
           WIDGET_CONTROL, cbi_id, SET_COMBOBOX_SELECT=ix_sel_mass+ix_add
         ENDFOR

         cb_mass_strct = refresh_cboxes_mass(event, SEL_CHROM=dsel, UNAME_MASS_LIST=uname_mass_list)
         pdata_strct = {}
         FOR i=0, n_plot-1 DO BEGIN
           strct = {xdata:PTR_NEW(/allocate_heap), vdata:PTR_NEW(/allocate_heap)}
           pdata_strct = [pdata_strct, strct]
           IF cb_mass_strct[i].plotIO EQ 0 THEN BEGIN
             *(pdata_strct[i].xdata) = 0.
             *(pdata_strct[i].vdata) = 0.
           ENDIF ELSE BEGIN
             *(pdata_strct[i].xdata) = (*chrom[dsel].time)[*cb_mass_strct[i].msel]
             *(pdata_strct[i].vdata) = (*chrom[dsel].intensity)[*cb_mass_strct[i].msel]
           ENDELSE
         ENDFOR
         pdata=viewer_prep_pdata(pdata_strct, bgr_substr=bgr_substr, fix_xyrange=fix_xyrange)
         plot_routine_pobj0, pdata.x, pdata.v, X_0A=pdata.x_0a, V_0A=pdata.v_0a, X_0B=pdata.x_0b, V_0B=pdata.v_0b, X_0C=pdata.x_0c, $
                             V_0C=pdata.v_0c, X_0D=pdata.x_0d, V_0D=pdata.v_0d, X_0E=pdata.x_0e, V_0E=pdata.v_0e, X_0F=pdata.x_0f, V_0F=pdata.v_0f, $
                             XRANGE=pdata.xrange, YRANGE=pdata.yrange, OVER=1234567
         refresh_text_pobj0, CB_DATA_STRCT=cb_mass_strct, CHROM=chrom, SUBST_REL_ABDS=subst_rel_abds, SET_SUBTITLE=mmv_subtitle, TOT_UNIQM=tot_uniqm
       END
     'recr_txt': $
       BEGIN
         refresh_text_pobj0, /recreate_txt
       END
     'gen_leg': $
       BEGIN
         cb_mass_strct = refresh_cboxes_mass(event, SEL_CHROM=dsel, UNAME_MASS_LIST=uname_mass_list)
         plotmasses = cb_mass_strct.mass
         IF plotmasses[0] NE 0 THEN plot_routine_pobj0, 0, 0, gen_legend=plotmasses
       END
    ELSE: $
       BEGIN
          cb_mass_strct = refresh_cboxes_mass(event, SEL_CHROM=dsel, UNAME_MASS_LIST=uname_mass_list)   ; gather settings from cboxes
          pdata_strct = {}
            FOR i=0, n_plot-1 DO BEGIN
              strct = {xdata:PTR_NEW(/allocate_heap), vdata:PTR_NEW(/allocate_heap)}
              pdata_strct = [pdata_strct, strct]
              IF cb_mass_strct[i].plotIO EQ 0 THEN BEGIN
                *(pdata_strct[i].xdata) = 0.
                *(pdata_strct[i].vdata) = 0.
              ENDIF
              IF cb_mass_strct[i].mass EQ -1 THEN BEGIN                                                 ; mass index -1 calls TIC calculation
                TIC=calc_tic(chrom, dsel, NOMONLY=nomonly)
                *(pdata_strct[i].xdata) = TIC.time
                *(pdata_strct[i].vdata) = TIC.intensity
              ENDIF ELSE BEGIN
                *(pdata_strct[i].xdata) = (*chrom[dsel].time)[*cb_mass_strct[i].msel]
                *(pdata_strct[i].vdata) = (*chrom[dsel].intensity)[*cb_mass_strct[i].msel]
              ENDELSE
            ENDFOR
            pdata=viewer_prep_pdata(pdata_strct, bgr_substr=bgr_substr, fix_xyrange=fix_xyrange)
         plot_routine_pobj0, pdata.x, pdata.v, X_0A=pdata.x_0a, V_0A=pdata.v_0a, X_0B=pdata.x_0b, V_0B=pdata.v_0b, X_0C=pdata.x_0c, $
                             V_0C=pdata.v_0c, X_0D=pdata.x_0d, V_0D=pdata.v_0d, X_0E=pdata.x_0e, V_0E=pdata.v_0e, X_0F=pdata.x_0f, V_0F=pdata.v_0f, $
                             XRANGE=pdata.xrange, YRANGE=pdata.yrange, OVER=1234567
         refresh_text_pobj0, CB_DATA_STRCT=cb_mass_strct, CHROM=chrom, SUBST_REL_ABDS=subst_rel_abds, SET_SUBTITLE=mmv_subtitle, TOT_UNIQM=tot_uniqm
        END
    ENDCASE

END