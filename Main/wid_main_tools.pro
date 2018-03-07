;------------------------------------------------------------------------------------------------------------------------
;+
; AUTHOR: F. Obersteiner
;
; PURPOSE: process actions on the main widget (droplist selections).
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION refresh_dsel_msel, event, chrom

  strct = {dsel: 0, msel: 0, mass: -1,  nvd: 0}
  
  IF SIZE(chrom, /TYPE) NE 8 THEN BEGIN ; abort if chrom is no structure
    MSG = DIALOG_MESSAGE('No valid data found.', /INFORMATION)
    RETURN, strct 
  ENDIF
  IF STRLEN(chrom[0].fname) EQ 0 THEN BEGIN ; abort if chrom is empty
    MSG = DIALOG_MESSAGE('No valid data found.', /INFORMATION)
    RETURN, strct
  ENDIF

  ID_sel_chrom = WIDGET_INFO(event.top, find_by_uname='sel_chrom')
  sel_chrom = WIDGET_INFO(ID_sel_chrom, /droplist_select)

  ID_sel_mass = WIDGET_INFO(event.top, find_by_uname='sel_mass')
  sel_mass = WIDGET_INFO(ID_sel_mass, /droplist_select) ; selected mass
  WIDGET_CONTROL, ID_sel_mass, get_value=masslist ; list of all masses
  
  mass = -1
  msel = WHERE(*chrom[sel_chrom].mass EQ FIX(masslist[sel_mass],type=4), nvd)
  mass = masslist[sel_mass]

  ;MSG = DIALOG_MESSAGE('m/Q not found.', /INFORMATION)
  
  strct = {dsel: sel_chrom, msel: msel, mass: mass,  nvd: nvd}

  RETURN, strct
  
END
;------------------------------------------------------------------------------------------------------------------
FUNCTION change_time, chrom, new_t_scale
  timescales_strarr=['Seconds','Minutes']
  IF new_t_scale EQ 'Seconds' THEN t_set=0 ELSE t_set=1 ; t_set = 0 --> seconds, t_set = 1, minutes
  
  old_t_scale = chrom[0].t_scale
  IF old_t_scale EQ 'Minutes' THEN t_conv=60D ELSE t_conv=1D/60D; define conversion factor to get the corresponding 
                                                            ; other time scale
                                                            
  IF old_t_scale EQ 'Seconds' AND t_set EQ 0 THEN RETURN, chrom ; is state equals set state
  IF old_t_scale EQ 'Minutes' AND t_set EQ 1 THEN RETURN, chrom ; is state equals set state

  n_chrom = N_ELEMENTS(chrom)
  t_str = timescales_strarr[t_set]
  FOR i=0, n_chrom-1 DO BEGIN
    chrom[i].t_scale=t_str
    (*chrom[i].time)=(*chrom[i].time)*t_conv
  ENDFOR
  refr_status, message='timescale changed to '+STRLOWCASE(new_t_scale)+'.'
  msg=DIALOG_MESSAGE('Timescale changed. Please refresh plots & update integration settings!')
  RETURN, chrom
END