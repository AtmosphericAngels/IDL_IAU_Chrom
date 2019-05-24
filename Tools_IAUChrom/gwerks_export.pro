;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; 
;
; AUTHOR:
; F.Obersteiner, f.obersteiner@kit.edu, May 2019
;
; INFO:
; exports mass traces to text file that can be read by GCWERKS
;
;-
;------------------------------------------------------------------------------------------------------------------------
@get_uniq_mass
;------------------------------------------------------------------------------------------------------------------------
PRO gwerks_export, chrom, PATH=path
  
  exppath = DIALOG_PICKFILE(TITLE='Please select output directory.', PATH=path, /DIRECTORY)
  IF STRLEN(exppath) LT 3 THEN RETURN
;  tic
  FOR fno=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN
    unqm = get_uniq_mass(chrom, SEL_CHROM=fno)
    time = *chrom[fno].time
    IF chrom[fno].t_scale EQ 'Minutes' THEN time = time*60D
    mass = *chrom[fno].mass
    intensity = *chrom[fno].intensity
    
    OPENW, lun, exppath+FILE_BASENAME(chrom[fno].fname)+'_gcw.txt', /GET_LUN
    
    FOR mno=0, N_ELEMENTS(unqm)-1 DO BEGIN
      idx = WHERE(mass EQ unqm[mno], nvd) ; select data based on current mass
      IF nvd GT 0 THEN BEGIN
        sel_time = time[idx] ; get time vector (GC time) and intensity for selected mass
        sel_intensity = intensity[idx]
        output = STRARR(N_ELEMENTS(sel_time))
        output[*] = $
          STRCOMPRESS(STRING(sel_time[*], FORMAT='(D25.2)'),/REMOVE_ALL) + ' ' + $
            STRCOMPRESS(STRING(sel_intensity[*], FORMAT='(D25.6)'),/REMOVE_ALL)

        PRINTF, lun, 'mass '+STRCOMPRESS(STRING(unqm[mno], FORMAT='(D25.6)'),/REMOVE_ALL), FORMAT='(A)'
        PRINTF, lun, output, FORMAT='(A)'

      ENDIF
    ENDFOR ; end loop over all masses in current file
    
    CLOSE, lun
    FREE_LUN, lun
    
  ENDFOR ; end loop over all loaded files
;  toc

END
