;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; 17-06, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; check a iau_chrom database script for consistency. no news are good news.
;
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION iauchrom_db_chk, db_info, LOUD=loud, N_TESTED=n_tested

  n_tested = 0
  n_passed = 0
  
  ; argument type ok?
  n_tested = n_tested + 1
  IF db_info EQ !NULL OR SIZE(db_info, /TYPE) NE 8 THEN RETURN, n_passed
  n_passed = n_passed + 1
  
  msg = []
  no_exp = 0
  no_data = 0
  no_msinfo = 0
  n_exp = N_ELEMENTS(db_info.data)
   
  ; header enough lines?
  n_tested = n_tested + 1
  IF N_ELEMENTS(db_info.header) LT 6 THEN $
    msg = [msg, '- not enough header lines.'] $
      ELSE  n_passed = n_passed + 1
  
  ; experiment defined?
  n_tested = n_tested + 1
  IF n_exp EQ 0 OR db_info.data[0].active EQ -1 THEN BEGIN
    msg = [msg, '- no experiment defined.']
    no_exp = 1 
  ENDIF ELSE n_passed = n_passed + 1
  
  IF NOT no_exp THEN BEGIN
    ; exp_id unique?
    n_tested = n_tested + 1
    str=db_info.data.exp_id
    str[uniq(str)]='-unique-'
    w_not_uniq=WHERE(str NE '-unique-', nvd) 
    IF nvd GT 0 THEN BEGIN
      msg = [msg, '- found double experiment id(s):']
      FOR i=0, nvd-1 DO msg = [msg, '--> '+str[w_not_uniq[i]]] 
    ENDIF ELSE n_passed = n_passed + 1
    
    ; data_import_fct is allowed?
    n_tested = n_tested + 1
    str=db_info.data.data_import_fct
    def_fct=['aes_cdf','agilent_cdf','almsco_cdf','ecd_fid_cdf','ecd_txt','tofwerk_h5']
    str_match = LONARR(N_ELEMENTS(str))
    FOR i=0, N_ELEMENTS(str)-1 DO BEGIN
      str_match[i]=MAX(STRMATCH(def_fct, str[i]))
      IF NOT str_match[i] THEN $
        msg = [msg, '- found not allowed import function(s):', '--> '+str[i]]
    ENDFOR
    IF MIN(str_match) THEN n_passed = n_passed + 1
    
    ; files of spec. type in exp_data_dir?
    n_tested = n_tested + 1
    FOR n=0, n_exp-1 DO BEGIN
      CASE db_info.data[n].data_import_fct OF
        'aes_cdf': filter = '*.nc'
        'agilent_cdf': filter = '*.cdf'
        'almsco_cdf': filter = '*.cdf'
        'ecd_fid_cdf': filter = '*.nc'
        'ecd_txt':  filter = '*.txt'
        'tofwerk_h5': filter = '*.h5'
      ENDCASE
      folder = db_info.data[n].exp_data_dir
      tmp = FILE_SEARCH(folder, filter, COUNT=count)
      IF count EQ 0 THEN BEGIN
        msg = [msg, '- no file(s) found: '+filter, '--> '+folder]
        no_data = no_data + 1
      ENDIF
    ENDFOR
    IF no_data EQ 0 THEN  n_passed = n_passed + 1
    
    ; exp_msinfo_path points to ms.info file?
    n_tested = n_tested + 1
    FOR n=0, n_exp-1 DO BEGIN
      subst_cnfg = db_info.data[n].exp_msinfo_path
      IF NOT FILE_TEST(subst_cnfg) THEN BEGIN
        msg = [msg, '- msinfo file not found:', '--> '+subst_cnfg]
        no_msinfo = no_msinfo + 1
      ENDIF
    ENDFOR
    IF no_msinfo EQ 0 THEN  n_passed = n_passed + 1
  ENDIF
  
  ; error reporting
  IF N_ELEMENTS(msg) GT 0 THEN $
    IF NOT KEYWORD_SET(loud) THEN FOR i=0, N_ELEMENTS(msg)-1 DO PRINT, msg[i] $
  ELSE popup=DIALOG_MESSAGE(msg, /ERROR)
   
  RETURN, n_passed
  
END