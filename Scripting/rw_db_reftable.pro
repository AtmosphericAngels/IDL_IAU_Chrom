;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; 17-05, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; FUNCTION read_iauchromdbfile reads iau_chrom database reference table and returns collected data.
; FUNCTION write_iauchromdbfile writes iau_chrom database reference table with specific filename generated from 
; date and time.
;
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION read_iauchromdbfile, FILE=file, PATH=path

  sep = ';'

  IF NOT KEYWORD_SET(file) THEN $
    file = DIALOG_PICKFILE(TITLE='Please chose a database reference table (.csv)', FILTER='*.csv', $
    PATH=path)

  IF file EQ !NULL OR STRLEN(file) EQ 0 THEN RETURN, !NULL

  nl = FILE_LINES(file)
  table = STRARR(nl)

  OPENR, lun, file, /GET_LUN
  READF, lun, table
  CLOSE, lun
  FREE_LUN, lun

  n_lines_hdr = LONG(table[0])

  db_strct = { $
    active    :   -1,  $
    exp_id    :   '', $
    exp_descr    :   '', $
    exp_data_dir    :   '', $
    exp_data_tag    :   '', $
    exp_msinfo_path    :   '', $
    data_import_fct    :   '', $
    savefile_path    :   '', $
    save_txtreport    :   -1,  $
    save_quicklook    :   -1,  $
    autoint_timestamp    :   '',  $
    hit_index    :   -1.  $
  }

  info_strct = {header : table[0:n_lines_hdr-1], data : REPLICATE(db_strct, nl-n_lines_hdr)}

  FOR i=0, (nl-n_lines_hdr)-1 DO BEGIN ; loop over all defined experiments
    tmp=STRCOMPRESS(strsplit(STRTRIM(table[i+n_lines_hdr]), sep, /EXTRACT, /PRESERVE_NULL), /REMOVE_ALL)
    info_strct.data[i].active = LONG(tmp[0])
    info_strct.data[i].exp_id = tmp[1]
    info_strct.data[i].exp_descr = tmp[2]
    info_strct.data[i].exp_data_dir = tmp[3]
    info_strct.data[i].exp_data_tag = tmp[4]
    info_strct.data[i].exp_msinfo_path = tmp[5]
    info_strct.data[i].data_import_fct = tmp[6]
    info_strct.data[i].savefile_path = tmp[7]
    info_strct.data[i].save_txtreport = LONG(tmp[8])
    info_strct.data[i].save_quicklook = LONG(tmp[9])
    info_strct.data[i].autoint_timestamp = tmp[10]
    info_strct.data[i].hit_index = FLOAT(tmp[11])
  ENDFOR ; end loop through experiments

  RETURN, info_strct
  
END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION write_iauchromdbfile, info_strct, PATH=path, UTC=utc

  sep = ';'

  IF SIZE(info_strct, /TYPE) NE 8 THEN RETURN, !NULL

  IF NOT KEYWORD_SET(path) THEN $
    path = DIALOG_PICKFILE(TITLE='Please chose a folder where to store database reference table', /DIRECTORY)

  IF NOT KEYWORD_SET(utc) THEN time = SYSTIME(/JULIAN) ELSE time = SYSTIME(/JULIAN, /UTC)

  CALDAT, time, MM, DD, YY, HH, MN, SS

  db_fname = path+'\'+$
    STRING(yy, FORMAT='(I04)')+STRING(mm, FORMAT='(I02)')+STRING(dd, FORMAT='(I02)')+$
    STRING(hh, FORMAT='(I02)')+STRING(mn, FORMAT='(I02)')+$
    '_iauchrom_db.csv'

  n_lines_hdr = LONG(info_strct.header[0])
  n_exp = N_ELEMENTS(info_strct.data.active)
  cnf_str = STRARR(n_exp)
  
  info_strct.header[-3] = jultime2timestring(SYSTIME(/JULIAN))+';;;;;;;;;;;'
  
  FOR i=0, n_exp-1 DO $
    cnf_str[i] = $
    STRING(info_strct.data[i].active, FORMAT='(I)') + sep + $
    info_strct.data[i].exp_id + sep + $
    info_strct.data[i].exp_descr + sep + $
    info_strct.data[i].exp_data_dir + sep + $
    info_strct.data[i].exp_data_tag + sep + $
    info_strct.data[i].exp_msinfo_path + sep + $
    info_strct.data[i].data_import_fct + sep + $
    info_strct.data[i].savefile_path + sep + $
    STRING(info_strct.data[i].save_txtreport, FORMAT='(I)') + sep + $
    STRING(info_strct.data[i].save_quicklook, FORMAT='(I)') + sep + $
    info_strct.data[i].autoint_timestamp + sep + $
    STRING(info_strct.data[i].hit_index, FORMAT='(F6.4)')

  OPENW, lun, db_fname, /GET_LUN
    FOR j=0, n_lines_hdr-1 DO PRINTF, lun, info_strct.header[j]
    FOR i=0, n_exp-1 DO PRINTF, lun, STRCOMPRESS(cnf_str[i])
  CLOSE, lun
  FREE_LUN, lun

  RETURN, 1
  
END