;+
; FUNCTION jultime2timestring
;
; AUTOR: S.Sala
; MODIFIED: F. Obersteiner, introduced keywords.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION jultime2timestring, v, ONLYDATE=onlydate, HMSONLY=hmsonly, DATE_MDY=date_mdy, $
                                YMD_CLEAN=ymd_clean

  CALDAT, v, MM, DD, YYYY, HH, MN, SS

  IF KEYWORD_SET(date_mdy) THEN BEGIN
    timestring = $
      STRCOMPRESS(STRING(MM, FORMAT='(I02)')+'.'+STRING(DD, FORMAT='(I02)')+'.'+STRING(YYYY, FORMAT='(I04)'), /REMOVE_ALL)+' '+ $
      STRCOMPRESS(STRING(HH, FORMAT='(I02)')+':'+STRING(MN, FORMAT='(I02)')+':'+STRING(SS, FORMAT='(I02)'), /REMOVE_ALL)
  ENDIF ELSE BEGIN
    timestring = $
      STRCOMPRESS(STRING(DD, FORMAT='(I02)')+'.'+STRING(MM, FORMAT='(I02)')+'.'+STRING(YYYY, FORMAT='(I04)'), /REMOVE_ALL)+' '+ $
      STRCOMPRESS(STRING(HH, FORMAT='(I02)')+':'+STRING(MN, FORMAT='(I02)')+':'+STRING(SS, FORMAT='(I02)'), /REMOVE_ALL)
  ENDELSE

  IF KEYWORD_SET(onlydate) THEN BEGIN
    IF KEYWORD_SET(date_mdy) THEN $
      timestring = $
      STRCOMPRESS(STRING(MM, FORMAT='(I02)')+'.'+STRING(DD, FORMAT='(I02)')+'.'+STRING(YYYY, FORMAT='(I04)'), /REMOVE_ALL) $
    ELSE $
      timestring = $
      STRCOMPRESS(STRING(DD, FORMAT='(I02)')+'.'+STRING(MM, FORMAT='(I02)')+'.'+STRING(YYYY, FORMAT='(I04)'), /REMOVE_ALL)
  ENDIF

  IF KEYWORD_SET(hmsonly) THEN timestring = $
    STRCOMPRESS(STRING(HH, FORMAT='(I02)')+':'+STRING(MN, FORMAT='(I02)')+':'+STRING(SS, FORMAT='(I02)'), /REMOVE_ALL)

  IF KEYWORD_SET(ymd_clean) THEN timestring = $
    STRCOMPRESS(STRING(YYYY, FORMAT='(I04)') + STRING(MM, FORMAT='(I02)') + STRING(DD, FORMAT='(I02)') + $
                STRING(HH, FORMAT='(I02)') + STRING(MN, FORMAT='(I02)'), /REMOVE_ALL)


  RETURN, timestring

END