;-----------------------------------------------------------------------------------------------------------------------------------------------
;+
; wid_main_ini.pro:
; PRO destroy_wids/ PRO wid_main_handle / PRO wid_main_ini
; 
; INFO:
; main widget code.
;-
;-----------------------------------------------------------------------------------------------------------------------------------------------
@HTOF_hrdata_tools
;-----------------------------------------------------------------------------------------------------------------------------------------------
PRO wid_main_handle, event

  COMMON DATA
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
  
  UNAME = WIDGET_INFO(event.id, /UNAME)
  t_scale = STRCOMPRESS(WIDGET_INFO(WIDGET_INFO(event.top, find_by_UNAME='t_scale'), /COMBOBOX_GETTEXT), /REMOVE_ALL)
  
  CASE UNAME OF
; **********************************************************************************************************************************************
    'agilent_cdf' : $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 THEN chrom=create_refd()
        IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
          IF quest EQ 'Yes' THEN BEGIN 
            FreeVar, chrom
            FreeVar, subst
          ENDIF ELSE RETURN
        ENDIF
        destroy_wids
        
        refr_status, message='loading files...'
        chrom = read_agilent_cdf(PATH=path, T_SCALE=t_scale, VERSION=version, /LOUD)
        IF (STRLEN(chrom[0].fname) EQ 0) THEN BEGIN
          refr_status, message='idle'
          RETURN 
        ENDIF
        show_list, event, chrom

        data_select = refresh_dsel_msel(event, chrom)
        tot_uniqm = get_uniq_mass(chrom)
        chrom.instr_type = 1 ; 0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD
        refr_status, message='files loaded.'
      END
; **********************************************************************************************************************************************
    'aes_cdf' : $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 THEN chrom=create_refd()
        IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
          IF quest EQ 'Yes' THEN BEGIN
            FreeVar, chrom
            FreeVar, subst
          ENDIF ELSE RETURN
        ENDIF
        destroy_wids
        
        refr_status, message='loading files...'
        chrom = read_AES_cdf(PATH=path, T_SCALE=t_scale, VERSION=version, /LOUD)
        IF (STRLEN(chrom[0].fname) EQ 0) THEN BEGIN
          refr_status, message='idle'
          RETURN 
        ENDIF
        show_list, event, chrom
      
        data_select = refresh_dsel_msel(event, chrom)
        tot_uniqm = get_uniq_mass(chrom)
        chrom.instr_type = 5 ; 0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD
        refr_status, message='files loaded.'
      END
; **********************************************************************************************************************************************
    'almsco_cdf' : $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 THEN chrom=create_refd()
        IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
          IF quest EQ 'Yes' THEN BEGIN 
            FreeVar, chrom
            FreeVar, subst
          ENDIF ELSE RETURN
        ENDIF
        destroy_wids
        
        refr_status, message='loading files...'
        chrom = read_almsco_cdf(PATH=path, T_SCALE=t_scale, VERSION=version, /LOUD)
        IF (STRLEN(chrom[0].fname) EQ 0) THEN BEGIN
          refr_status, message='idle'
          RETURN 
        ENDIF
        show_list, event, chrom

        data_select = refresh_dsel_msel(event, chrom)
        refr_status, message='creating m/Q list...'
        tot_uniqm = get_uniq_mass(chrom)
        chrom.instr_type = 2 ; 0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD
        refr_status, message='files loaded.'
      END
; ********************************************************************************************************************************************** 
    'tofwerk_h5' : $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 THEN chrom=create_refd()
        IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
          IF quest EQ 'Yes' THEN BEGIN
            FreeVar, chrom
            FreeVar, subst
          ENDIF ELSE RETURN
        ENDIF
        destroy_wids
        
        quest=DIALOG_MESSAGE('Recalculate Peakdata? Might take some time...', /QUESTION, $
                             /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
          IF quest EQ 'Yes' THEN peakdata_recalc=1 ELSE peakdata_recalc=0
            IF peakdata_recalc THEN quest=DIALOG_MESSAGE('Use integration limits stored in peaktable? +-17.5 mTh are used otherwise.', $
                                                          /QUESTION,  /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
              IF quest EQ 'Yes' THEN use_pt_limits=1 ELSE use_pt_limits=0
        
        refr_status, message='loading files...'        
        chrom=read_tofwerkh5(PATH=path, T_SCALE=t_scale, VERSION=version, PEAKDATA_RECALC=peakdata_recalc, $
                             USE_PT_LIMITS=use_pt_limits, /LOUD)                            
        IF (STRLEN(chrom[0].fname) EQ 0) THEN BEGIN
          refr_status, message='idle'
          RETURN 
        ENDIF
        show_list, event, chrom

        data_select = refresh_dsel_msel(event, chrom)
        refr_status, message='creating m/Q list...'
        tot_uniqm = get_uniq_mass(chrom)
        chrom.instr_type = 3 ; 0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD
        refr_status, message='files loaded.'
      END
; ********************************************************************************************************************************************** 
    'ecd_txt' : $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 THEN chrom=create_refd()
        IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
          IF quest EQ 'Yes' THEN BEGIN 
            FreeVar, chrom
            FreeVar, subst
          ENDIF ELSE RETURN
        ENDIF
        destroy_wids
        
        refr_status, message='loading files...'
        chrom=read_ecd_txt(PATH=path, VERSION=version, /LOUD)
        IF (STRLEN(chrom[0].fname) EQ 0) THEN BEGIN
          refr_status, message='idle'
          RETURN 
        ENDIF
        show_list, event, chrom

        data_select = refresh_dsel_msel(event, chrom)
        tot_uniqm = get_uniq_mass(chrom)
        chrom.instr_type = 4 ; 0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD
        refr_status, message='files loaded.'
      END
; **********************************************************************************************************************************************
    'ecd_fid_cdf' : $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 THEN chrom=create_refd()
        IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
          IF quest EQ 'Yes' THEN BEGIN 
            FreeVar, chrom
            FreeVar, subst
          ENDIF ELSE RETURN
        ENDIF
        destroy_wids
        
        refr_status, message='loading files...'
        chrom = read_ecd_fid_cdf(PATH=path, T_SCALE=t_scale, VERSION=version, /LOUD)
        IF (STRLEN(chrom[0].fname) EQ 0) THEN BEGIN
          refr_status, message='idle'
          RETURN 
        ENDIF
        show_list, event, chrom

        data_select = refresh_dsel_msel(event, chrom)
        tot_uniqm = get_uniq_mass(chrom)
        chrom.instr_type = 6 ; 0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD
        refr_status, message='files loaded.'
      END
; **EXPORT**************************************************************************************************************************************      
    'gwerks_export': $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 OR STRLEN(chrom[0].fname) EQ 0 THEN BEGIN
          !NULL=DIALOG_MESSAGE('Please load Data first.', /INFORMATION)
        ENDIF ELSE BEGIN
          refr_status, message='exporting files for GCWERKS...' 
          gwerks_export, chrom, PATH=path
          refr_status, message='export done.' 
        ENDELSE
      END
; **********************************************************************************************************************************************  
; ********************************************************************************************************************************************** 
; ********************************************************************************************************************************************** 
    'sav_exp': $
      BEGIN
        IF STRLEN(chrom[0].fname)EQ 0 THEN BEGIN                                      ; chrom does not exist
          MSG=DIALOG_MESSAGE('Please load Data first.', /INFORMATION)
          RETURN 
        ENDIF     
        IF WHERE(STRUPCASE(TAG_NAMES(chrom)) EQ 'SUBST') EQ -1 THEN BEGIN             ; chrom does not contain results information
          MSG=DIALOG_MESSAGE('No Results found. Please load MSINFO fist.', /INFORMATION)
          RETURN
        ENDIF
        cdate = conc_date(chrom[0].jdate, SYSTIME(/JULIAN), cdate1=cdate1)
        savefname=DIALOG_PICKFILE(TITLE='Please select path and filename to save the experiment...', $
                                  FILE='chrom_'+cdate, /OVERWRITE_PROMPT, DEFAULT_EXTENSION='dat', /WRITE, PATH=path)
        IF STRLEN(savefname[0])EQ 0 THEN RETURN                                       ; save aborted
        refr_status, message='saving experiment...'
        
        dir=FILE_DIRNAME(savefname, /MARK_DIR)                                        ; extract directory
        tmp=FILE_BASENAME(savefname)                                                  ; extract file name
          
        IF STRMATCH(STRMID(tmp, 0, 6), 'chrom_', /FOLD_CASE) THEN tmp=STRMID(tmp, 6)  ; delete chrom prefix if set by user (default)
        tmp=STRSPLIT(tmp, '.', /EXTRACT, COUNT=count)                                 ; split file name where a '.' is found
        IF count GT 0 THEN BEGIN                                                      ; more than one '.' found
          extention='.'+tmp[-1]                                                       ; extract extention
          fname=STRJOIN(tmp[0:-2])                                                    ; recreate file name without extention
        ENDIF
             
        save, chrom, filename=dir+'chrom_'+fname+extention
        save, subst, filename=dir+'subst_'+fname+extention
        export_subst2msinfo, subst, FNAME=dir+fname+'_def_ms.info'
        refr_status, message='experiment saved.'                                                     
      END
; **********************************************************************************************************************************************     
    'res_exp': $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 THEN chrom=create_refd() ; define chrom if undefined to allow following question
        IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, DIALOG_PARENT=widid.mainwid)
          IF quest EQ 'Yes' THEN BEGIN
            FreeVar, chrom
            FreeVar, subst
          ENDIF ELSE RETURN
        ENDIF
        
        destroy_wids 
        refr_status, message='restoring experiment...'                                                                  
                
        chromfile = DIALOG_PICKFILE(PATH=path, TITLE='Please select a chrom*.dat file to restore.', $
                                    FILE='*chrom*', FILTER='*.dat', /FIX_FILTER)
        IF STRLEN(chromfile)EQ 0 THEN BEGIN
          refr_status, message='idle'
          RETURN                               ; abort if no chromfile selected
        ENDIF    
                
        chrom = create_refd()
        RESTORE, FILENAME=chromfile,  /RELAXED_STRUCTURE_ASSIGNMENT   
        
        substbasename = strreplace(FILE_BASENAME(chromfile), 'chrom', 'subst')
        substfile = FILE_DIRNAME(chromfile)+'\'+substbasename
        
        IF SIZE(subst, /TYPE) EQ 8 THEN FreeVar, subst                   ; delete if exist
        IF FILE_TEST(substfile) NE 0 THEN BEGIN
          subst = create_refs()
          RESTORE, FILENAME=substfile, /RELAXED_STRUCTURE_ASSIGNMENT     ; restore if selected, else: no default settings available
        ENDIF ELSE BEGIN
          msg = DIALOG_MESSAGE('No subst*.dat found. Integration defaults will not be available.', /INFORMATION)
          subst = create_refs()                                          ; no subst file found, create empty.
        ENDELSE        
       
        vcheck = version_check(chrom,VCHECK_VERSION=5.14)                ; check chrom, returns 1 if up-to-date
        IF vcheck EQ 0 THEN BEGIN                                        ; converstion to current version of IAU_Chrom necessary
          refr_status, message='updating old chrom version...'            
          IF FILE_TEST(substfile) NE 0 THEN $                            ; msinfo / subst file selected, convert to current version
            subst=convert_oldsubst(subst)
          IF STRLEN(chromfile) NE 0 THEN chrom=convert_oldchrom(chrom, version) $
            ELSE RETURN                                                  ; import aborted after conversion necessary                
        ENDIF           
        
        IF SIZE(chrom, /TYPE) NE 8 THEN RETURN                           ; experiment restore aborted
        
        show_list, event, chrom
        data_select = refresh_dsel_msel(event, chrom)
        refr_status, message='creating m/Q list...'
        tot_uniqm = get_uniq_mass(chrom)
        
        ID_tscale = WIDGET_INFO(event.top, find_by_UNAME='t_scale')
        WIDGET_CONTROL, ID_tscale, GET_VALUE=tscale_options
        IF STRLEN(chrom[0].t_scale) EQ 0 THEN BEGIN
          t_scale = '?'
          WIDGET_CONTROL, ID_tscale, COMBOBOX_ADDITEM=t_scale
          WIDGET_CONTROL, ID_tscale, GET_VALUE=tscale_options
          WIDGET_CONTROL, ID_tscale, SET_COMBOBOX_SELECT=(WHERE(t_scale EQ tscale_options))[0]
        ENDIF ELSE BEGIN
          t_scale = chrom[0].t_scale
          WIDGET_CONTROL, ID_tscale, SET_COMBOBOX_SELECT=(WHERE(t_scale EQ tscale_options))[0]
        ENDELSE
         
        instr_type = chrom.instr_type
        refr_status, message='experiment restored.'               
      END
; **********************************************************************************************************************************************  
    'set_path' : $
      BEGIN
        path = DIALOG_PICKFILE(/DIRECTORY, PATH=path)
        refr_status, message='file path changed.'
      END
; **********************************************************************************************************************************************    
    'exit' : $
      BEGIN
        msg=DIALOG_MESSAGE('Are you sure? Unsaved Data will be lost.', /QUESTION)
        IF msg EQ 'Yes' THEN BEGIN
          w = GETWINDOWS()
          IF N_ELEMENTS(w) GT 0 THEN FOREACH i, w DO i.close ; close plot windows
          FreeVar, chrom     ; clear chrom structure from memory
          FreeVar, subst
          HEAP_GC            ; garbage collection in heap memory     
          IF event.select THEN BEGIN ; close all open widgets / ID not zero
            destroy_wids, /ALL
          ENDIF
        ENDIF
      END
; ********************************************************************************************************************************************** 
    't_scale' : IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
                  warn=DIALOG_MESSAGE('Are you sure? You will have to update integration settings and results!', /CANCEL, /DEFAULT_CANCEL)
                    IF warn EQ 'Cancel' THEN RETURN
                  chrom=change_time(chrom, t_scale)
                  refr_status, message='changed time scale.'
                ENDIF
; **********************************************************************************************************************************************
    'sel_chrom' : $
      BEGIN
        data_select = refresh_dsel_msel(event, chrom) ; check selected chrom & mass
        check_pobjects, p_obj=['p_obj0', 'p_obj1']
        refresh_text_pobj0, set_manual=[STRING(FILE_BASENAME(chrom[data_select.dsel].fname)), $  ; set text fields
                                        'Mass: '+STRCOMPRESS(STRING(data_select.mass), /REMOVE_ALL)], $
                            set_colors=['k','k']
          IF data_select.nvd GT 0 THEN BEGIN
            x = (*chrom[data_select.dsel].time)[data_select.msel]
            v = (*chrom[data_select.dsel].intensity)[data_select.msel]
            plot_routine_pobj0, x, v, OVER=1
          ENDIF ELSE plot_routine_pobj0, 0, 0, /SET_ZERO
      END
; ********************************************************************************************************************************************** 
     'sel_mass' : $
      BEGIN
        data_select = refresh_dsel_msel(event, chrom) ; check selected chrom & mass
        check_pobjects, p_obj=['p_obj0', 'p_obj1']
        refresh_text_pobj0, set_manual=[STRING(FILE_BASENAME(chrom[data_select.dsel].fname)), $  ; set text fields
                                        'm/Q: '+STRCOMPRESS(STRING(data_select.mass), /REMOVE_ALL)], $
                            set_colors=['k','k']
          IF data_select.nvd GT 0 THEN BEGIN
            x = (*chrom[data_select.dsel].time)[data_select.msel]
            v = (*chrom[data_select.dsel].intensity)[data_select.msel]
            plot_routine_pobj0, x, v, OVER=1
          ENDIF ELSE plot_routine_pobj0, 0, 0, /SET_ZERO
      END
; **********************************************************************************************************************************************  
    'integration': $
      BEGIN
        IF STRLEN(chrom[0].fname) EQ 0 THEN MSG = DIALOG_MESSAGE('Please load Data first.', /INFORMATION) $
          ELSE BEGIN
            IF WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1) EQ -1 THEN BEGIN ; msinfo not loaded yet
              refs=read_subst(PATH=path)
              IF STRLEN(refs[0].name) EQ 0 THEN RETURN ; no substances in msinfo
              refi=create_refi()
              subst=add_ires2subst(refs, refi)
              chrom=add_subst2chrom(chrom, subst)
            ENDIF        
            wid_integration_ini
          ENDELSE
      END    
; **********************************************************************************************************************************************
    'v_multimass': $
      BEGIN
        IF STRLEN(chrom[0].fname)EQ 0 THEN MSG = DIALOG_MESSAGE('Please load Data first.', /INFORMATION) $
          ELSE wid_mmassviewer_ini
      END
; **********************************************************************************************************************************************  
    'v_multichrom': $
      BEGIN
        IF STRLEN(chrom[0].fname)EQ 0 THEN MSG = DIALOG_MESSAGE('Please load Data first.', /INFORMATION) $
          ELSE wid_mchromviewer_ini
      END
; **********************************************************************************************************************************************
    'v_tpshsk': $
      BEGIN
        MSG=0
        IF STRLEN(chrom[0].fname)EQ 0 THEN MSG = DIALOG_MESSAGE('Please load Data first.', /INFORMATION)
        IF chrom[0].instr_type NE 3 THEN MSG = DIALOG_MESSAGE('Tofwerk files only.', /INFORMATION)
        IF SIZE(MSG, /TYPE) EQ 2 THEN wid_tpshskviewer_ini
      END
; **********************************************************************************************************************************************  
    'fragrat': $
      BEGIN
        IF STRLEN(chrom[0].fname)EQ 0 THEN BEGIN
          MSG = DIALOG_MESSAGE('Please load Data first.', /INFORMATION)
          RETURN
        ENDIF
        IF chrom[0].instr_type EQ 4 THEN BEGIN
          MSG = DIALOG_MESSAGE('Fragment Ratio Calculator available for mass spectrometric Data only.', /INFORMATION)
          RETURN
        ENDIF
        IF WHERE(STRMATCH(TAG_NAMES(chrom), 'subst', /FOLD_CASE) EQ 1) EQ -1 THEN BEGIN ; msinfo not loaded yet
              refs=read_subst(path=path)
              IF STRLEN(refs[0].name) EQ 0 THEN RETURN ; no substances in msinfo
              refi=create_refi()
              subst=add_ires2subst(refs, refi)
              chrom=add_subst2chrom(chrom, subst)
        ENDIF
        wid_fragrat_ini
      END
; **********************************************************************************************************************************************      
    'plot_ctrls' : $
        wid_plotctrls
; **********************************************************************************************************************************************
    'run_db_script' : $
      BEGIN
        IF SIZE(chrom, /TYPE) NE 8 THEN chrom=create_refd()
        IF STRLEN(chrom[0].fname) NE 0 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO)
          IF quest EQ 'Yes' THEN BEGIN 
            tmp=TEMPORARY(chrom)
            tmp=TEMPORARY(subst)
            destroy_wids        
          ENDIF ELSE RETURN
        ENDIF
        
        quest=DIALOG_MESSAGE('Include noise calculation?', /QUESTION)
        IF quest EQ 'Yes' THEN calcnoise=1 ELSE calcnoise=0
        quest=DIALOG_MESSAGE('Sort files by measurement timestamp?', /QUESTION, /DEFAULT_NO)
        IF quest EQ 'Yes' THEN sort_by_jdate=1 ELSE sort_by_jdate=0
        quest=DIALOG_MESSAGE('Tofwerk TOFMS data: recalculate peakdata?', /QUESTION, /DEFAULT_NO)
        IF quest EQ 'Yes' THEN tw_recalc_mq=1 ELSE tw_recalc_mq=0
        
        call_iauchrom_dbscript, event, T_SCALE=t_scale, CALCNOISE=calcnoise, TW_RECALC_MQ=tw_recalc_mq, $
                                SORT_BY_JDATE=sort_by_jdate
      END
; **********************************************************************************************************************************************
  ENDCASE 
END
;-----------------------------------------------------------------------------------------------------------------------------------------------
PRO wid_main_ini

  COMMON DATA
  COMMON WIDID

  title = 'IAU_Chrom_v'+STRCOMPRESS(STRING(version, FORMAT='(F4.2)'), /REMOVE_ALL)
      
  mainbase = WIDGET_BASE(TITLE=title, UNAME='mainwid', MBAR=menubaseID, COLUMN=1)
    
  file_ID = WIDGET_BUTTON(menubaseID, VALUE='File', /MENU)
    load_ID = WIDGET_BUTTON(file_ID, VALUE='Import file(s)...', /MENU)
      ID = WIDGET_BUTTON(load_ID, VALUE='AED *.nc', UNAME = 'aes_cdf')
      ID = WIDGET_BUTTON(load_ID, VALUE='Agilent QPMS *.cdf', UNAME = 'agilent_cdf')
      ID = WIDGET_BUTTON(load_ID, VALUE='Almsco TOFMS *.cdf', UNAME = 'almsco_cdf')
      ID = WIDGET_BUTTON(load_ID, VALUE='AutoSpec SFMS *.cdf', UNAME = 'agilent_cdf')
      ID = WIDGET_BUTTON(load_ID, VALUE='GHG ECD/FID *.nc', UNAME = 'ecd_fid_cdf')
      ID = WIDGET_BUTTON(load_ID, VALUE='GhOST ECD *.txt', UNAME = 'ecd_txt')
      ID = WIDGET_BUTTON(load_ID, VALUE='Tofwerk TOFMS *.h5', UNAME = 'tofwerk_h5')
    expt_ID = WIDGET_BUTTON(file_ID, VALUE='Export...', /MENU)
      ID = WIDGET_BUTTON(expt_ID, VALUE='GCWERKS txt', UNAME = 'gwerks_export')      
    ID = WIDGET_BUTTON(file_ID, VALUE='Save Experiment', UNAME = 'sav_exp', /SEPARATOR)
    ID = WIDGET_BUTTON(file_ID, VALUE='Restore Experiment', UNAME = 'res_exp')
    ID = WIDGET_BUTTON(file_ID, VALUE='Set Filepath...', UNAME = 'set_path', /SEPARATOR)    
    ID = WIDGET_BUTTON(file_ID, VALUE='Exit IAU_Chrom', UNAME = 'exit')
          
  DA_ID = WIDGET_BUTTON(menubaseID, VALUE='DataAnalysis', /MENU)
    ID = WIDGET_BUTTON(DA_ID, VALUE='Peak Integration / Noise', UNAME= 'integration')
    ID = WIDGET_BUTTON(DA_ID, VALUE='Fragment Ratios', UNAME = 'fragrat', /SEPARATOR)
      
  VI_ID = WIDGET_BUTTON(menubaseID, VALUE='Viewer', /MENU)
    ID = WIDGET_BUTTON(VI_ID, VALUE='Multiple Chroms, 1 m/Q', UNAME='v_multichrom')
    ID = WIDGET_BUTTON(VI_ID, VALUE='Multiple m/Q, 1 Chrom', UNAME='v_multimass')
    ID = WIDGET_BUTTON(VI_ID, VALUE='Tofwerk TPS HSK', UNAME='v_tpshsk', /SEPARATOR)
    ID = WIDGET_BUTTON(VI_ID, VALUE='Show Plot Controls', UNAME='plot_ctrls', /SEPARATOR)
    
  ADV_ID=WIDGET_BUTTON(menubaseID, VALUE='Advanced', /MENU)
    ID = WIDGET_BUTTON(ADV_ID, VALUE='Run Database Script', UNAME='run_db_script')

 widbase1 = WIDGET_BASE(mainbase, COLUMN=1)
widbase11 = WIDGET_BASE(widbase1, UNAME='widbase11', column=1)
     t_LB = WIDGET_LABEL(widbase11, VALUE='Time Axis Format ', /ALIGN_LEFT)
     t_CB = WIDGET_COMBOBOX(widbase11, VALUE=['Seconds', 'Minutes'], UNAME='t_scale', /ALIGN_LEFT)
            WIDGET_CONTROL, t_CB,  SET_COMBOBOX_SELECT=1; minutes as default
      SEP = WIDGET_LABEL(widbase11, VALUE=' ')
     F_DL = WIDGET_DROPLIST(widbase11, VALUE='', Title='Loaded File(s) ', UNAME='sel_chrom', /DYNAMIC_RESIZE, /ALIGN_LEFT)
     M_DL = WIDGET_DROPLIST(widbase11, VALUE='', Title='Selected m/Q ', UNAME='sel_mass', /DYNAMIC_RESIZE, /ALIGN_LEFT)
      SEP = WIDGET_LABEL(widbase11, VALUE=' ')
      SEP = WIDGET_LABEL(widbase11, VALUE='Status ', /ALIGN_LEFT, /DYNAMIC_RESIZE)
      STAT = WIDGET_TEXT(widbase11, VALUE='idle', UNAME='status', /ALIGN_LEFT, XSIZE=50)
      
    credits = WIDGET_BASE(mainbase, UNAME='credits', column=1)
     SEP = WIDGET_LABEL(credits, VALUE='--', /ALIGN_LEFT)
     TXT = WIDGET_LABEL(credits, VALUE='(c) 2019 Univ. Frankfurt / IAU / Group A. Engel', /ALIGN_LEFT)

  
  widid.mainwid = mainbase 
  WIDGET_CONTROL, mainbase, /REALIZE
  XMANAGER, 'wid_main_handle', mainbase, /NO_BLOCK, event_handler='wid_main_handle' 
  
END