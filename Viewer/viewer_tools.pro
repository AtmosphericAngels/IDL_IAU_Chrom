;------------------------------------------------------------------------------------------------------------------------
;+
; INFO:
; tools for the viewers...
;-
;------------------------------------------------------------------------------------------------------------------------
;*****************************************************************************************************************************************
; gets the current value and its index of a combobox specified by cbox_ID
; return value: 2-element string vector, 0 = value, 1 = index
FUNCTION cbox_get_valind, cbox_ID 

  WIDGET_CONTROL, cbox_ID, /COMBOBOX_INDEX, get_value=cbox_content
  cbox_content = STRCOMPRESS(cbox_content, /REMOVE_ALL)
  cbox_current_text = STRCOMPRESS(WIDGET_INFO(cbox_ID, /COMBOBOX_GETTEXT), /REMOVE_ALL)
  
    CASE cbox_current_text OF
      'none': index = '0'
      'TIC' : index = '1'
      ELSE  : index = WHERE(cbox_content EQ cbox_current_text)
    ENDCASE

  index = STRCOMPRESS(STRING(index), /REMOVE_ALL)
  val_ind = [cbox_current_text, index]      
RETURN, val_ind

END 

;*****************************************************************************************************************************************
; val_ind: array with form [value, index] ; value: cbox current text, index: cbox selection index, vd: valid values
; return value: 2-element integer vector, 0: plot yes/no (1/0), 1: index of value in vd
FUNCTION cbox_chk_valind_mass, val_ind, vd 

value=STRING(val_ind[0])
index=FIX(val_ind[1], TYPE=2) ; index, integer, this index still referrs to the content of the cbox!

  CASE index OF
    -1: BEGIN ; manual entry
          numeric_test = valid_num(value)
            IF numeric_test EQ 0 THEN BEGIN;     if not numeric: error message & plot NONE
              plotIO = 0
              index = -2
              msg=DIALOG_MESSAGE('Please enter a numeric value.', /INFORMATION)
            ENDIF ELSE BEGIN;     else: is numeric -> find (closest) match in chrom0_masses
              match_valind = cbox_matchval(val_ind[0], vd)
              index = match_valind[1]
              plotIO = 1
            ENDELSE
         END         
     0: BEGIN
          plotIO = 0
          index = 0 ; NONE
        END     
     1: BEGIN
          plotIO = 1
          index = -1 ; TIC
        END    
    ELSE: BEGIN
            plotIO = 1
            index = index-2 ; index is GT 1: plot selected mass
          END         
  ENDCASE

  result = [plotIO, index] 
  
  RETURN, result
  
END

;*****************************************************************************************************************************************

FUNCTION cbox_chk_valind_chrom, val_ind, vd ; val_ind: array with form [value, index] ; value: cbox current text, index: cbox selection index

  value=STRING(val_ind[0])
  index=FIX(val_ind[1], TYPE=2) ; index, integer, this index still referrs to the content of the cbox!

  IF index EQ 0 THEN plotIO = 0 & uniq_mass_index = 0 ; NONE
  IF index GT 0 THEN plotIO = 1 & uniq_mass_index = index-1 ; index is GT 1: plot selected mass

  result = [plotIO, uniq_mass_index]
   
  RETURN, result
  
END

;*****************************************************************************************************************************************

FUNCTION cbox_matchval, value, vd ; vd is already converted to chrom0_masses in FUNCTION cbox_chk_valind

  value=FLOAT(value)
  match_ind = WHERE(value EQ vd) ; try to find value in vd
    IF match_ind GT -1 THEN BEGIN ; value found in vd, return value and index
      match_val = vd[match_ind]
    ENDIF ELSE BEGIN ; value not found in vd, calculate closest number, info-message and return value and index
      match_ind = MIN((vd-value), min_index, /ABSOLUT)
      match_val = vd[min_index]     
      match_ind = min_index
      msg=DIALOG_MESSAGE('Found no match for entered value. Value set to '+STRCOMPRESS(STRING(match_val), /REMOVE_ALL)+'.', /INFORMATION)
    ENDELSE      
  match_valind = [match_val, match_ind]
  
  RETURN, match_valind
  
END

;*****************************************************************************************************************************************

FUNCTION refresh_cboxes_mass, event, SEL_CHROM=sel_chrom, UNAME_MASS_LIST=uname_mass_list

  COMMON DATA
  
  IF NOT KEYWORD_SET(sel_chrom) THEN sel_chrom=0

  vd = tot_uniqm ; define valid data (masses) for cboxes from common variable tot_uniqm

  cb_strct = {} ; generate empty structure for combobox-contents
  FOR i=0, N_ELEMENTS(uname_mass_list)-1 DO BEGIN
    strct = {plotIO: 0B,$
             msel: PTR_NEW(/ALLOCATE_HEAP),$; msel might not be equal for different masses, therefore pointer & create in every loop to get different addresses
             mass: 0., $
             sel_chrom: sel_chrom}
           
    cbox_ID = WIDGET_INFO(event.top, find_by_uname=uname_mass_list[i])
    val_ind=cbox_get_valind(cbox_ID)                      
    result=cbox_chk_valind_mass(val_ind, vd)
    
    strct.plotIO = result[0] ; define if mass should be plotted or not
    IF strct.plotIO EQ 0 THEN BEGIN                             ; plot IO is FALSE -> selection must be none, select NONE
          WIDGET_CONTROL, cbox_ID, SET_COMBOBOX_SELECT=0
          strct.mass = 0
          *strct.msel = 0.
    ENDIF ELSE BEGIN   
      CASE result[1] OF              
      -1: BEGIN $                                               ; selection is 'TIC'
          WIDGET_CONTROL, cbox_ID, SET_COMBOBOX_SELECT=1 
          strct.mass = -1
          *strct.msel = 0.
        END        
      -2: BEGIN $                                               ; invalid value entered, select NONE
          WIDGET_CONTROL, cbox_ID, SET_COMBOBOX_SELECT=0 
          strct.mass = 0
          *strct.msel = 0.
        END          
      ELSE: BEGIN
          WIDGET_CONTROL, cbox_ID, SET_COMBOBOX_SELECT=result[1]+2; value found, +2 necessary because 'none' and 'TIC are appended to mass list
          strct.mass = tot_uniqm[result[1]] ; specify the selected mass
          *strct.msel = WHERE((*chrom[sel_chrom].mass) EQ FIX(tot_uniqm[result[1]], type=4), nvd)
       END
    ENDCASE
  ENDELSE    
    cb_strct=[cb_strct, strct]
  ENDFOR
  
  RETURN, cb_strct
  
END

;*****************************************************************************************************************************************

FUNCTION refresh_cboxes_chrom, event, UNAME_CHROM_LIST=uname_chrom_list, VD_CHROMS=vd_chroms

  cb_strct = {} ; generate empty structure for combobox-contents
  FOR i=0, N_ELEMENTS(uname_chrom_list)-1 DO BEGIN
    strct = {plotIO: 0B,$
             name: '', $
             index: 0B} 
           
    cbox_ID = WIDGET_INFO(event.top, find_by_uname=uname_chrom_list[i])
    val_ind=cbox_get_valind(cbox_ID)                  
    result=cbox_chk_valind_chrom(val_ind, vd_chroms)
    strct.plotIO = result[0] ; define if mass should be plotted or not
    
      IF strct.plotIO EQ 0 THEN strct.name='none' ELSE $
        strct.name = vd_chroms[result[1]] ; specify the selected chromatogram
        
      strct.index = result[1]
      
          IF result[0] EQ 0 THEN WIDGET_CONTROL, cbox_ID, SET_COMBOBOX_SELECT=0 $ ; selection is 'none'
            ELSE WIDGET_CONTROL, cbox_ID, SET_COMBOBOX_SELECT=result[1]+1 ; value found, +1 because 'none'
    
    cb_strct=[cb_strct, strct]
  ENDFOR
  
  RETURN, cb_strct

END

;*****************************************************************************************************************************************

FUNCTION viewer_prep_pdata, pdata_strct, BGR_SUBSTR=bgr_substr, FIX_XYRANGE=fix_xyrange

  COMMON COM_PLOT
 
  pdata_new = { $ ; generate struct with plot data
    x    : *pdata_strct[0].xdata, $
    v    : *pdata_strct[0].vdata, $
    x_0a : *pdata_strct[1].xdata, $
    v_0a : *pdata_strct[1].vdata, $
    x_0b : *pdata_strct[2].xdata, $
    v_0b : *pdata_strct[2].vdata, $
    x_0c : *pdata_strct[3].xdata, $
    v_0c : *pdata_strct[3].vdata, $
    x_0d : *pdata_strct[4].xdata, $
    v_0d : *pdata_strct[4].vdata, $
    x_0e : *pdata_strct[5].xdata, $
    v_0e : *pdata_strct[5].vdata, $
    x_0f : *pdata_strct[6].xdata, $
    v_0f : *pdata_strct[6].vdata, $
    xrange : (p_obj0[0]).xrange, $
    yrange : (p_obj0[0]).yrange  }
   
  IF KEYWORD_SET(bgr_substr) THEN BEGIN                                                     ; substract minimum values from intensity
    xwin = WHERE(pdata_new.x GE pdata_new.xrange[0] AND pdata_new.x LT pdata_new.xrange[1]) ; index array of time window selected on plot
    pdata_new.v = pdata_new.v - MIN(pdata_new.v[xwin]) 
    pdata_new.v_0a = pdata_new.v_0a - MIN(pdata_new.v_0a[xwin])
    pdata_new.v_0b = pdata_new.v_0b - MIN(pdata_new.v_0b[xwin])
    pdata_new.v_0c = pdata_new.v_0c - MIN(pdata_new.v_0c[xwin])
    pdata_new.v_0d = pdata_new.v_0d - MIN(pdata_new.v_0d[xwin])
    pdata_new.v_0e = pdata_new.v_0e - MIN(pdata_new.v_0e[xwin])
    pdata_new.v_0f = pdata_new.v_0f - MIN(pdata_new.v_0f[xwin])  
  ENDIF
 
  IF NOT KEYWORD_SET(fix_xyrange) THEN BEGIN                                                ; generate x- y-range if range not fixed
    xspan = [pdata_new.x, pdata_new.x_0a, pdata_new.x_0b, pdata_new.x_0c, pdata_new.x_0d, pdata_new.x_0e, pdata_new.x_0f]
    vspan = [pdata_new.v, pdata_new.v_0a, pdata_new.v_0b, pdata_new.v_0c, pdata_new.v_0d, pdata_new.v_0e, pdata_new.v_0f]
    pdata_new.xrange=[MIN(xspan)-(MAX(xspan)-MIN(xspan))*0.025, MAX(xspan)+(MAX(xspan)-MIN(xspan))*0.025]
    pdata_new.yrange=[MIN(vspan)-(MAX(vspan)-MIN(vspan))*0.025, MAX(vspan)+(MAX(vspan)-MIN(vspan))*0.05]
  ENDIF
  
  RETURN, pdata_new

END

;*****************************************************************************************************************************************
PRO viewer_tools
END