;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; PRO wid_plotctrls
;
; AUTHOR:
; F.Obersteiner
;
; INFO:
; 'manual' controls for x- and y-ranges of plots 0 and 1.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO wid_plotctrls

  COMMON WIDID
  
  plotctrl_base = WIDGET_BASE(TITLE='Plot_Ctrls', MBAR=menubaseID, COLUMN=1, XOFF=200, YOFF=50, /BASE_ALIGN_CENTER)

  tabs  = WIDGET_TAB(plotctrl_base)
    p0_base = WIDGET_BASE(tabs, TITLE="plot_0", ROW=10)
      sep   = WIDGET_LABEL(p0_base, Value=' *** ')
      xti   = WIDGET_LABEL(p0_base, Value=' XRANGE: ')
      xrb   = WIDGET_BASE(p0_base, TAB_MODE=1, column=2)
      x0    = WIDGET_COMBOBOX(xrb, value='0.00', uname='p0_x0', /DYNAMIC_RESIZE, TAB_MODE=1, /EDITABLE)
      x1    = WIDGET_COMBOBOX(xrb, value='1.00', uname='p0_x1', /DYNAMIC_RESIZE, TAB_MODE=1, /EDITABLE)
      sep   = WIDGET_LABEL(p0_base, Value=' *** ')
      yti   = WIDGET_LABEL(p0_base, Value=' YRANGE: ')
      yrb   = WIDGET_BASE(p0_base, TAB_MODE=1, column=2)
      y0    = WIDGET_COMBOBOX(yrb, value='0.00', uname='p0_y0', /DYNAMIC_RESIZE, TAB_MODE=1, /EDITABLE)
      y1    = WIDGET_COMBOBOX(yrb, value='1.00', uname='p0_y1', /DYNAMIC_RESIZE, TAB_MODE=1, /EDITABLE)
      sep   = WIDGET_LABEL(p0_base, Value=' *** ')    
      setp0 = WIDGET_BUTTON(p0_base, value='Set!', uname='p0_set')
      sep   = WIDGET_LABEL(p0_base, Value=' *** ')
      getp0 = WIDGET_BUTTON(p0_base, value='Get current X-/Y-RANGE', uname='p0_get')
      
    p1_base = WIDGET_BASE(tabs, TITLE="plot_1", ROW=10)
      sep   = WIDGET_LABEL(p1_base, Value=' *** ')
      xti   = WIDGET_LABEL(p1_base, Value=' XRANGE: ')
      xrb   = WIDGET_BASE(p1_base, /BASE_ALIGN_CENTER, TAB_MODE=1, column=2)
      x0    = WIDGET_COMBOBOX(xrb, value='0.00', uname='p1_x0', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1, /EDITABLE)
      x1    = WIDGET_COMBOBOX(xrb, value='1.00', uname='p1_x1', /DYNAMIC_RESIZE, /ALIGN_RIGHT, TAB_MODE=1, /EDITABLE)
      sep   = WIDGET_LABEL(p1_base, Value=' *** ')
      yti   = WIDGET_LABEL(p1_base, Value=' YRANGE: ')
      yrb   = WIDGET_BASE(p1_base, /BASE_ALIGN_CENTER, TAB_MODE=1, column=2)
      y0    = WIDGET_COMBOBOX(yrb, value='0.00', uname='p1_y0', /DYNAMIC_RESIZE, /ALIGN_LEFT, TAB_MODE=1, /EDITABLE)
      y1    = WIDGET_COMBOBOX(yrb, value='1.00', uname='p1_y1', /DYNAMIC_RESIZE, /ALIGN_RIGHT, TAB_MODE=1, /EDITABLE)
      sep   = WIDGET_LABEL(p1_base, Value=' *** ')
      setp1 = WIDGET_BUTTON(p1_base, value='Set!', uname='p1_set', /ALIGN_CENTER)
      sep   = WIDGET_LABEL(p1_base, Value=' *** ')
      getp1 = WIDGET_BUTTON(p1_base, value='Get current X-/Y-RANGE', uname='p1_get', /ALIGN_CENTER)
          
  
  widid.plot_ctrls = plotctrl_base
  WIDGET_CONTROL, plotctrl_base, /REALIZE
  XMANAGER, 'wid_plotctrls_handle', plotctrl_base, /NO_BLOCK, event_handler='wid_plotctrls_handle'
  
END

PRO wid_plotctrls_handle, event

  COMMON WIDID
  COMMON COM_PLOT

  check_pobjects, p_obj = ['p_obj0', 'p_obj1']
  uname = WIDGET_INFO(event.id, /uname)

  CASE uname OF
    '':
  
    'p0_x0': $ ; check numeric (0=reset), check LT x1 (false: x1=x0+1)
      BEGIN
        ID=WIDGET_INFO(event.top, find_by_uname='p0_x0')
        val_ix = cbox_get_valind(ID)
        IF val_ix[1] EQ -1 THEN BEGIN
          numeric_test = valid_num(val_ix[0])
          IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 0.
            msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
            val_ix = [0., 0]
          ENDIF
          WIDGET_CONTROL, ID, SET_VALUE=STRING(val_ix[0])
        ENDIF
      END

    'p0_x1': $
      BEGIN
        ID=WIDGET_INFO(event.top, find_by_uname='p0_x1')
        val_ix = cbox_get_valind(ID)
        IF val_ix[1] EQ -1 THEN BEGIN
          numeric_test = valid_num(val_ix[0])
          IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 1.
            msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
            val_ix = [0., 0]
          ENDIF
          WIDGET_CONTROL, ID, SET_VALUE=STRING(val_ix[0])
        ENDIF
      END

    'p0_y0': $
      BEGIN
        ID=WIDGET_INFO(event.top, find_by_uname='p0_y0')
        val_ix = cbox_get_valind(ID)
        IF val_ix[1] EQ -1 THEN BEGIN
          numeric_test = valid_num(val_ix[0])
          IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 0.
            msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
            val_ix = [0., 0]
          ENDIF
          WIDGET_CONTROL, ID, SET_VALUE=STRING(val_ix[0])
        ENDIF
      END

    'p0_y1': $
      BEGIN
        ID=WIDGET_INFO(event.top, find_by_uname='p0_y1')
        val_ix = cbox_get_valind(ID)
        IF val_ix[1] EQ -1 THEN BEGIN
          numeric_test = valid_num(val_ix[0])
          IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 1.
            msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
            val_ix = [1., 0]
          ENDIF
          WIDGET_CONTROL, ID, SET_VALUE=STRING(val_ix[0])
        ENDIF
      END

    'p0_get': $ ; get current x/yrange for plot0
      BEGIN
        p0_xrange=(p_obj0[0]).xrange
        p0_yrange=(p_obj0[0]).yrange
        ID=WIDGET_INFO(event.top, find_by_uname='p0_x0')
        WIDGET_CONTROL, ID, SET_VALUE=STRCOMPRESS(STRING(p0_xrange[0], FORMAT='(F15.2)'))
        ID=WIDGET_INFO(event.top, find_by_uname='p0_x1')
        WIDGET_CONTROL, ID, SET_VALUE=STRCOMPRESS(STRING(p0_xrange[1], FORMAT='(F15.2)'))
        ID=WIDGET_INFO(event.top, find_by_uname='p0_y0')
        WIDGET_CONTROL, ID, SET_VALUE=STRCOMPRESS(STRING(p0_yrange[0], FORMAT='(F15.2)'))
        ID=WIDGET_INFO(event.top, find_by_uname='p0_y1')
        WIDGET_CONTROL, ID, SET_VALUE=STRCOMPRESS(STRING(p0_yrange[1], FORMAT='(F15.2)'))
      END

    'p0_set': $ ; check, correct and apply to plot0
      BEGIN
        ID=WIDGET_INFO(event.top, find_by_uname='p0_x0')
        WIDGET_CONTROL, ID, GET_VALUE=x0
        x0=FLOAT(x0)
        ID=WIDGET_INFO(event.top, find_by_uname='p0_x1')
        WIDGET_CONTROL, ID, GET_VALUE=x1
        x1=FLOAT(x1)
        IF x0 GE x1 THEN BEGIN
          x1=x0+1.
          WIDGET_CONTROL, ID, SET_VALUE=STRING(x0+1.)
        ENDIF

        ID=WIDGET_INFO(event.top, find_by_uname='p0_y0')
        WIDGET_CONTROL, ID, GET_VALUE=y0
        y0=FLOAT(y0)
        ID=WIDGET_INFO(event.top, find_by_uname='p0_y1')
        WIDGET_CONTROL, ID, GET_VALUE=y1
        y1=FLOAT(y1)
        IF y0 GE y1 THEN BEGIN
          y1=y0+1.
          WIDGET_CONTROL, ID, SET_VALUE=STRING(y0+1.)
        ENDIF

        p0_XRANGE=[x0,x1]
        p0_YRANGE=[y0,y1]
        (p_obj0[0]).xrange=p0_xrange
        (p_obj0[0]).yrange=p0_yrange
      END










      'p1_x0': $ ; check numeric (0=reset), check LT x1 (false: x1=x0+1)
        BEGIN
          ID=WIDGET_INFO(event.top, find_by_uname='p1_x0')
          val_ix = cbox_get_valind(ID)
          IF val_ix[1] EQ -1 THEN BEGIN
            numeric_test = valid_num(val_ix[0])
            IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 0.
              msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
              val_ix = [0., 0]
            ENDIF
            WIDGET_CONTROL, ID, SET_VALUE=STRING(val_ix[0])
          ENDIF
        END

      'p1_x1': $
        BEGIN
          ID=WIDGET_INFO(event.top, find_by_uname='p1_x1')
          val_ix = cbox_get_valind(ID)
          IF val_ix[1] EQ -1 THEN BEGIN
            numeric_test = valid_num(val_ix[0])
            IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 1.
              msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
              val_ix = [0., 0]
            ENDIF
            WIDGET_CONTROL, ID, SET_VALUE=STRING(val_ix[0])
          ENDIF
        END

      'p1_y0': $
        BEGIN
          ID=WIDGET_INFO(event.top, find_by_uname='p1_y0')
          val_ix = cbox_get_valind(ID)
          IF val_ix[1] EQ -1 THEN BEGIN
            numeric_test = valid_num(val_ix[0])
            IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 0.
              msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
              val_ix = [0., 0]
            ENDIF
            WIDGET_CONTROL, ID, SET_VALUE=STRING(val_ix[0])
          ENDIF
        END

      'p1_y1': $
        BEGIN
          ID=WIDGET_INFO(event.top, find_by_uname='p1_y1')
          val_ix = cbox_get_valind(ID)
          IF val_ix[1] EQ -1 THEN BEGIN
            numeric_test = valid_num(val_ix[0])
            IF numeric_test EQ 0 THEN BEGIN ; num test NOT passed, reset to default 1.
              msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
              val_ix = [1., 0]
            ENDIF
            WIDGET_CONTROL, ID, SET_VALUE=STRING(val_ix[0])
          ENDIF
        END

      'p1_get': $ ; get current x/yrange for plot1
        BEGIN
          p1_xrange=(p_obj1[0]).xrange
          p1_yrange=(p_obj1[0]).yrange
          ID=WIDGET_INFO(event.top, find_by_uname='p1_x0')
          WIDGET_CONTROL, ID, SET_VALUE=STRCOMPRESS(STRING(p1_xrange[0], FORMAT='(F15.2)'))
          ID=WIDGET_INFO(event.top, find_by_uname='p1_x1')
          WIDGET_CONTROL, ID, SET_VALUE=STRCOMPRESS(STRING(p1_xrange[1], FORMAT='(F15.2)'))
          ID=WIDGET_INFO(event.top, find_by_uname='p1_y0')
          WIDGET_CONTROL, ID, SET_VALUE=STRCOMPRESS(STRING(p1_yrange[0], FORMAT='(F15.2)'))
          ID=WIDGET_INFO(event.top, find_by_uname='p1_y1')
          WIDGET_CONTROL, ID, SET_VALUE=STRCOMPRESS(STRING(p1_yrange[1], FORMAT='(F15.2)'))
        END

      'p1_set': $ ; check, correct and apply to plot0
        BEGIN
          ID=WIDGET_INFO(event.top, find_by_uname='p1_x0')
          WIDGET_CONTROL, ID, GET_VALUE=x0
          x0=FLOAT(x0)
          ID=WIDGET_INFO(event.top, find_by_uname='p1_x1')
          WIDGET_CONTROL, ID, GET_VALUE=x1
          x1=FLOAT(x1)
          IF x0 GE x1 THEN BEGIN
            x1=x0+1.
            WIDGET_CONTROL, ID, SET_VALUE=STRING(x0+1.)
          ENDIF
   
          ID=WIDGET_INFO(event.top, find_by_uname='p1_y0')
          WIDGET_CONTROL, ID, GET_VALUE=y0
          y0=FLOAT(y0)
          ID=WIDGET_INFO(event.top, find_by_uname='p1_y1')
          WIDGET_CONTROL, ID, GET_VALUE=y1
          y1=FLOAT(y1)
          IF y0 GE y1 THEN BEGIN
            y1=y0+1.
            WIDGET_CONTROL, ID, SET_VALUE=STRING(y0+1.)
          ENDIF
  
          p1_XRANGE=[x0,x1]
          p1_YRANGE=[y0,y1]
          (p_obj1[0]).xrange=p1_xrange
          (p_obj1[0]).yrange=p1_yrange
  
        END

  ENDCASE
  
END