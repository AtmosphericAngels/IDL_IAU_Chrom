;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO wid_tpshskviewer_ini / PRO wid_tpshskviewer_handle
;
; AUTHOR:
; F.Obersteiner, Nov.2013. Last modified July 2015.
;-
;------------------------------------------------------------------------------------------------------------------------
@calc_tic
@viewer_tools
;------------------------------------------------------------------------------------------------------------------------

PRO wid_tpshskviewer_ini

  COMMON DATA
  COMMON WIDID

  DEVICE, Get_Screen_Size = ScreenSize
          XCenter=FIX(ScreenSize[0])
          YCenter=FIX(ScreenSize[1])

  tpshskv_base = WIDGET_BASE(title='Viewer: TPS HSK', mbar=mcv_menu, column=1, $
                             xoff=0.01*screenSize[0], yoff=0.05*screensize[1], /BASE_ALIGN_CENTER)

    f_men = WIDGET_BUTTON(mcv_menu, VALUE='File', /MENU)
      f_ext = WIDGET_BUTTON(f_men, VALUE='Exit', uname='exit')
    p_men = WIDGET_BUTTON(mcv_menu, VALUE='Plot', /MENU)
      p_res = WIDGET_BUTTON(p_men, VALUE='Reset', uname='reset')
      p_rct = WIDGET_BUTTON(p_men, VALUE='Recreate Textfields', uname='recr_txt', /SEPARATOR)

    tmp=*(chrom[0].twtps_hsk)
    cb_info_value = tmp.info
    msel_lbl = WIDGET_LABEL(tpshskv_base, value='parameter: ', /ALIGN_CENTER)
    msel_ID = WIDGET_COMBOBOX(tpshskv_base, value='#', uname='sel_quantity', /DYNAMIC_RESIZE, /ALIGN_CENTER, /EDITABLE, TAB_MODE=1)
        WIDGET_CONTROL, msel_ID, set_value=cb_info_value

    sep = WIDGET_LABEL(tpshskv_base, value='***', /ALIGN_CENTER)
    dsel0_lbl = WIDGET_LABEL(tpshskv_base, value='trace_0 (black) ', /ALIGN_CENTER)
    dsel0_ID = WIDGET_COMBOBOX(tpshskv_base, value='#', uname='sel_chrom0', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1)
    dsel1_lbl = WIDGET_LABEL(tpshskv_base, value='trace_1 (red)' , /ALIGN_CENTER)
    dsel1_ID = WIDGET_COMBOBOX(tpshskv_base, value='#', uname='sel_chrom1', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1)
    dsel2_lbl = WIDGET_LABEL(tpshskv_base, value='trace_2 (green) ', /ALIGN_CENTER)
    dsel2_ID = WIDGET_COMBOBOX(tpshskv_base, value='#', uname='sel_chrom2', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1)
    dsel3_lbl = WIDGET_LABEL(tpshskv_base, value='trace_3 (blue) ', /ALIGN_CENTER)
    dsel3_ID = WIDGET_COMBOBOX(tpshskv_base, value='#', uname='sel_chrom3', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1)
    dsel4_lbl = WIDGET_LABEL(tpshskv_base, value='trace_4 (cyan) ', /ALIGN_CENTER)
    dsel4_ID = WIDGET_COMBOBOX(tpshskv_base, value='#', uname='sel_chrom4', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1)
    dsel5_lbl = WIDGET_LABEL(tpshskv_base, value='trace_5 (magenta) ', /ALIGN_CENTER)
    dsel5_ID = WIDGET_COMBOBOX(tpshskv_base, value='#', uname='sel_chrom5', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1)
    dsel6_lbl = WIDGET_LABEL(tpshskv_base, value='trace_6 (yellow) ', /ALIGN_CENTER)
    dsel6_ID = WIDGET_COMBOBOX(tpshskv_base, value='#', uname='sel_chrom6', /DYNAMIC_RESIZE, /ALIGN_CENTER, TAB_MODE=1)
    sep = WIDGET_LABEL(tpshskv_base, value='***', /ALIGN_CENTER)

    dselID_list = [dsel0_ID, dsel1_ID, dsel2_ID, dsel3_ID, dsel4_ID, dsel5_ID, dsel6_ID]
    FOR i=0, N_ELEMENTS(dselID_list)-1 DO WIDGET_CONTROL, dselID_list[i], set_value=['none', FILE_BASENAME(chrom.fname)]

    chkb = WIDGET_BASE(tpshskv_base, column=1, /NONEXCLUSIVE)
    fix_xyrange = WIDGET_BUTTON(chkb, value='Fix X-/Y-RANGE (Plot0)', uname='fix_xyrange', /ALIGN_CENTER)
    WIDGET_CONTROL, fix_xyrange, set_button = 0

    sep = WIDGET_LABEL(tpshskv_base, value=' ')

  widid.tpshskviewerwid = tpshskv_base
  WIDGET_CONTROL, tpshskv_base, /REALIZE
  XMANAGER, 'wid_tpshskviewer_handle', tpshskv_base, /NO_BLOCK, event_handler='wid_tpshskviewer_handle'

END

; ********************************************************************************************************************************************************

PRO wid_tpshskviewer_handle, event

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
  uname_quantity_list = ['sel_quantity']
  n_plot=7 ; maximum number of mass traces to plot
  vd_chroms = STRING(FILE_BASENAME(chrom.fname))
  tpsv_subtitle = 'Viewer: TPS HSK'
  xytitle = ['datapoint no.','value']
  fix_xyrange = WIDGET_INFO(WIDGET_INFO(event.top, find_by_uname='fix_xyrange'), /BUTTON_SET)

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

        msel_ID = WIDGET_INFO(event.top, find_by_uname='sel_quantity')              ; reset comboboxes
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

          sel_ID = WIDGET_INFO(event.top, find_by_uname='sel_quantity')
          val_ind = cbox_get_valind(sel_ID)
          current_sel = STRCOMPRESS(val_ind[0], /REMOVE_ALL)
          sel_ix = val_ind[1]

          plot_text = [cb_chrom_strct.name, 'Quantity: '+current_sel]
          refresh_text_pobj0, SET_MANUAL=plot_text, SET_SUBTITLE=tpsv_subtitle

          pdata_strct = {}
          FOR i=0, n_plot-1 DO BEGIN
            strct = {xdata:PTR_NEW(/allocate_heap), vdata:PTR_NEW(/allocate_heap)}
            pdata_strct = [pdata_strct, strct]
            IF cb_chrom_strct[i].plotIO THEN BEGIN
              tmp=*(chrom[cb_chrom_strct[i].index].twtps_hsk)
              *(pdata_strct[i].vdata) = REFORM((tmp.data)[sel_ix,*])
              *(pdata_strct[i].xdata) = INDGEN(N_ELEMENTS((tmp.data)[sel_ix,*]))+1
            ENDIF ELSE BEGIN                                                                                       ; plot nothing (none or mass not found)
              *(pdata_strct[i].xdata) = 0.
              *(pdata_strct[i].vdata) = 0.
            ENDELSE
          ENDFOR

          pdata=viewer_prep_pdata(pdata_strct, fix_xyrange=fix_xyrange)

          plot_routine_pobj0, pdata.x, pdata.v, X_0A=pdata.x_0a, V_0A=pdata.v_0a, X_0B=pdata.x_0b, V_0B=pdata.v_0b, X_0C=pdata.x_0c, $
                              V_0C=pdata.v_0c, X_0D=pdata.x_0d, V_0D=pdata.v_0d, X_0E=pdata.x_0e, V_0E=pdata.v_0e, X_0F=pdata.x_0f, V_0F=pdata.v_0f, $
                              XYTITLE=xytitle, XRANGE=pdata.xrange, YRANGE=pdata.yrange, OVER=1234567
      END
    ENDCASE

END