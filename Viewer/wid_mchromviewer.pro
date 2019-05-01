;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO wid_mchromviewer_ini / PRO wid_mchromviewer_handle
;
; AUTHOR:
; F.Obersteiner, Nov.2013. Last modified July 2015.
;-
;------------------------------------------------------------------------------------------------------------------------
@calc_tic
@viewer_tools
;------------------------------------------------------------------------------------------------------------------------
PRO wid_mchromviewer_ini

  COMMON DATA
  COMMON WIDID

  DEVICE, Get_Screen_Size = ScreenSize
          XCenter=FIX(ScreenSize[0])
          YCenter=FIX(ScreenSize[1])

  mcviewerbase = WIDGET_BASE(title='Viewer: Multiple Chroms', mbar=mcv_menu, column=1, $
                           xoff=0.01*screenSize[0], yoff=0.05*screensize[1], /BASE_ALIGN_CENTER)

    f_men = WIDGET_BUTTON(mcv_menu, VALUE='File', /MENU)
      f_ext = WIDGET_BUTTON(f_men, VALUE='Exit', uname='exit')
    p_men = WIDGET_BUTTON(mcv_menu, VALUE='Plot', /MENU)
      p_res = WIDGET_BUTTON(p_men, VALUE='Reset', uname='reset')
      p_rct = WIDGET_BUTTON(p_men, VALUE='Recreate Textfields', uname='recr_txt', /SEPARATOR)

    cb_mass_value = ['none', 'TIC', STRING(tot_uniqm, FORMAT='(D14.4)')]             ; generate content for mass-combobox
    msel_lbl = WIDGET_LABEL(mcviewerbase, value='Selected m/Q ', /ALIGN_CENTER)
    msel_ID = WIDGET_COMBOBOX(mcviewerbase, value='#', uname='sel_mass', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
        WIDGET_CONTROL, msel_ID, set_value=cb_mass_value

    sep = WIDGET_LABEL(mcviewerbase, value='***', /ALIGN_CENTER)
    dsel0_lbl = WIDGET_LABEL(mcviewerbase, value='Chrom_0 (black) ', /ALIGN_LEFT)
    dsel0_ID = WIDGET_COMBOBOX(mcviewerbase, value='#', uname='sel_chrom0', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1)
    dsel1_lbl = WIDGET_LABEL(mcviewerbase, value='Chrom_1 (red)' , /ALIGN_LEFT)
    dsel1_ID = WIDGET_COMBOBOX(mcviewerbase, value='#', uname='sel_chrom1', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1)
    dsel2_lbl = WIDGET_LABEL(mcviewerbase, value='Chrom_2 (green) ', /ALIGN_LEFT)
    dsel2_ID = WIDGET_COMBOBOX(mcviewerbase, value='#', uname='sel_chrom2', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1)
    dsel3_lbl = WIDGET_LABEL(mcviewerbase, value='Chrom_3 (blue) ', /ALIGN_LEFT)
    dsel3_ID = WIDGET_COMBOBOX(mcviewerbase, value='#', uname='sel_chrom3', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1)
    dsel4_lbl = WIDGET_LABEL(mcviewerbase, value='Chrom_4 (cyan) ', /ALIGN_LEFT)
    dsel4_ID = WIDGET_COMBOBOX(mcviewerbase, value='#', uname='sel_chrom4', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1)
    dsel5_lbl = WIDGET_LABEL(mcviewerbase, value='Chrom_5 (magenta) ', /ALIGN_LEFT)
    dsel5_ID = WIDGET_COMBOBOX(mcviewerbase, value='#', uname='sel_chrom5', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1)
    dsel6_lbl = WIDGET_LABEL(mcviewerbase, value='Chrom_6 (yellow) ', /ALIGN_LEFT)
    dsel6_ID = WIDGET_COMBOBOX(mcviewerbase, value='#', uname='sel_chrom6', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1)
    sep = WIDGET_LABEL(mcviewerbase, value='***', /ALIGN_CENTER)

    dselID_list = [dsel0_ID, dsel1_ID, dsel2_ID, dsel3_ID, dsel4_ID, dsel5_ID, dsel6_ID]
    FOR i=0, N_ELEMENTS(dselID_list)-1 DO WIDGET_CONTROL, dselID_list[i], set_value=['none', FILE_BASENAME(chrom.fname)]

    chkb = WIDGET_BASE(mcviewerbase, column=1, /NONEXCLUSIVE)
    fix_xyrange = WIDGET_BUTTON(chkb, value='Fix X-/Y-RANGE (Plot0)', uname='fix_xyrange', /ALIGN_CENTER)
    WIDGET_CONTROL, fix_xyrange, set_button = 0
    substract = WIDGET_BUTTON(chkb, value='Difference (Chrom_1 - Chrom_0)', uname='substract', /ALIGN_CENTER)
    WIDGET_CONTROL, substract, set_button = 0

    sep = WIDGET_LABEL(mcviewerbase, value=' ')

  widid.mcviewerwid = mcviewerbase
  WIDGET_CONTROL, mcviewerbase, /REALIZE
  XMANAGER, 'wid_mchromviewer_handle', mcviewerbase, /NO_BLOCK, event_handler='wid_mchromviewer_handle'

END

; ********************************************************************************************************************************************************

PRO wid_mchromviewer_handle, event

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
  uname_chrom_list = ['sel_chrom0','sel_chrom1','sel_chrom2','sel_chrom3','sel_chrom4','sel_chrom5','sel_chrom6']
  uname_mass_list = ['sel_mass']
  n_plot=7 ; maximum number of mass traces to plot
  vd_chroms = STRING(FILE_BASENAME(chrom.fname))
  mcv_subtitle = 'Viewer: Multiple Chroms'
  sc_subtitle = 'Difference (Chrom_1 - Chrom_0)'
  fix_xyrange = WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='fix_xyrange'), /BUTTON_SET)
  substract = WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='substract'), /BUTTON_SET)

  IF chrom[0].instr_type EQ 3 THEN nomonly=1 ELSE nomonly=0 ; masses with label 'nominal' only for HTOF data

  CASE uname OF
      'exit' : $
      BEGIN
        FreeVar, cb_chrom_strct                                                 ; clear variables
        FreeVar, cb_mass_strct
        plot_routine_pobj0, SET_ZERO=1                                          ; reset plot and textfields
        refresh_text_pobj0, SET_ZERO=1
        widid.mcviewerwid=-1                                                    ; set viewer widget id to zero
        IF event.SELECT THEN WIDGET_CONTROL, event.TOP, /DESTROY                ; close widget window
      END

      'reset': $
      BEGIN
        FreeVar, cb_chrom_strct                                                 ; free variables
        FreeVar, cb_mass_strct

        msel_ID = WIDGET_INFO(event.top, find_by_uname='sel_mass')              ; reset comboboxes
        WIDGET_CONTROL, msel_ID, SET_COMBOBOX_SELECT=0
        FOR i=0, N_ELEMENTS(uname_chrom_list)-1 DO BEGIN
          csel_ID = WIDGET_INFO(event.top, find_by_uname=uname_chrom_list[i])
          WIDGET_CONTROL, csel_ID, SET_COMBOBOX_SELECT=0
        ENDFOR

        ID = WIDGET_INFO(event.top, find_by_uname='fix_xyrange')
        WIDGET_CONTROL, ID, SET_BUTTON=0

        plot_routine_pobj0, SET_ZERO=1                                          ; reset plot and textfields
        refresh_text_pobj0, SET_ZERO=1
      END

      'recr_txt': $
        BEGIN
          refresh_text_pobj0, recreate_txt=1
        END

      ELSE: $
        BEGIN
          cb_chrom_strct = refresh_cboxes_chrom(event, UNAME_CHROM_LIST=uname_chrom_list, VD_CHROMS=vd_chroms)
          msel_ID = WIDGET_INFO(event.top, find_by_uname='sel_mass')
          current_msel = STRCOMPRESS(WIDGET_INFO(msel_ID, /COMBOBOX_GETTEXT), /REMOVE_ALL)

          IF chrom[0].instr_type EQ 3 THEN BEGIN
            peaktable=(*chrom[0].peaktable)
            ml_ixval=matchmass(peaktable.mass, current_msel)
            masslabel=' ('+peaktable[ml_ixval[1]].label+')'
          ENDIF ELSE masslabel=''

          IF SIZE(current_msel, /TYPE) EQ 4 then current_msel=STRCOMPRESS(STRING(current_msel, FORMAT='(D14.4)'), /REMOVE_ALL)

          plot_text=[cb_chrom_strct.name, 'Mass: '+current_msel+masslabel]
          refresh_text_pobj0, SET_MANUAL=plot_text, SET_SUBTITLE=mcv_subtitle
          pdata_strct = {}
          FOR i=0, n_plot-1 DO BEGIN
            strct = {xdata:PTR_NEW(/allocate_heap), vdata:PTR_NEW(/allocate_heap)}
            pdata_strct = [pdata_strct, strct]
            cb_mass_strct = refresh_cboxes_mass(event, sel_chrom=WHERE(cb_chrom_strct[i].name EQ FILE_BASENAME(chrom.fname)),$
                                                UNAME_MASS_LIST=uname_mass_list)
            IF cb_chrom_strct[i].plotIO + cb_mass_strct[0].plotIO EQ 2 THEN BEGIN
              IF cb_mass_strct[0].mass EQ -1 THEN BEGIN                                                 ; mass index -1 calls TIC calculation
                TIC=calc_tic(chrom, cb_chrom_strct[i].index, NOMONLY=nomonly)
                *(pdata_strct[i].xdata) = TIC.time
                *(pdata_strct[i].vdata) = TIC.intensity
              ENDIF ELSE BEGIN
              *(pdata_strct[i].xdata) = (*chrom[cb_chrom_strct[i].index].time)[*cb_mass_strct[0].msel]
              *(pdata_strct[i].vdata) = (*chrom[cb_chrom_strct[i].index].intensity)[*cb_mass_strct[0].msel]
              ENDELSE
            ENDIF ELSE BEGIN                                                                                       ; plot nothing (none or mass not found)
              *(pdata_strct[i].xdata) = 0.
              *(pdata_strct[i].vdata) = 0.
            ENDELSE
          ENDFOR
          pdata=viewer_prep_pdata(pdata_strct, fix_xyrange=fix_xyrange)

          IF substract THEN BEGIN
            chk_select=[0,0,0]                                                                                     ; check selection: mass, chrom0 and chrom1
            csel_ID = WIDGET_INFO(event.top, find_by_uname=uname_mass_list[0])
            chk_select[0]=(cbox_get_valind(csel_ID))[1]
            csel_ID = WIDGET_INFO(event.top, find_by_uname=uname_chrom_list[0])
            chk_select[1]=(cbox_get_valind(csel_ID))[1]
            csel_ID = WIDGET_INFO(event.top, find_by_uname=uname_chrom_list[1])
            chk_select[2]=(cbox_get_valind(csel_ID))[1]
            IF MIN(chk_select) EQ 0 THEN RETURN $                                                                  ; selection of mass, chrom 0 or chrom 1 missing
            ELSE BEGIN                                                                                             ; else: two chroms & mass selected, continue
              IF N_ELEMENTS(pdata.x) LT N_ELEMENTS(pdata.x_0a) THEN pdata.x[*]=pdata.x_0a[0:N_ELEMENTS(pdata.x)-1] ; else x=x
              v_dif = pdata.v_0a-pdata.v                                                                           ; calc difference
              IF N_ELEMENTS(v_dif) LT N_ELEMENTS(pdata.v) THEN the_end=N_ELEMENTS(v_dif)-1 $
                ELSE the_end=N_ELEMENTS(pdata.v)-1
              pdata.v[0:the_end]=v_dif[0:the_end]                                                                  ; fill differences array into v
              pdata.x_0a = 0                                                                                       ; reset remaining data to zero
              pdata.v_0a = 0
              pdata.x_0b = 0
              pdata.v_0b = 0
              pdata.x_0c = 0
              pdata.v_0c = 0
              pdata.x_0d = 0
              pdata.v_0d = 0
              pdata.x_0e = 0
              pdata.v_0e = 0
              pdata.x_0f = 0
              plot_text=[(cb_chrom_strct.name)[1]+' - '+(cb_chrom_strct.name)[0], 'none','none','none','none','none','none', '[Mass: '+current_msel+']']
              refresh_text_pobj0, SET_MANUAL=plot_text, SET_SUBTITLE=sc_subtitle
            END
          END
          plot_routine_pobj0, pdata.x, pdata.v, X_0A=pdata.x_0a, V_0A=pdata.v_0a, X_0B=pdata.x_0b, V_0B=pdata.v_0b, X_0C=pdata.x_0c, $
                              V_0C=pdata.v_0c, X_0D=pdata.x_0d, V_0D=pdata.v_0d, X_0E=pdata.x_0e, V_0E=pdata.v_0e, X_0F=pdata.x_0f, V_0F=pdata.v_0f, $
                              XRANGE=pdata.xrange, YRANGE=pdata.yrange, OVER=1234567
        END
    ENDCASE
END