;------------------------------------------------------------------------------------------------------------------------
;+
; INFO:
; refr_status procedure puts a message in the textbox on the main widget...
;
; AUTHOR:
; F.Obersteiner, Mar 2015.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO refr_status, MESSAGE=message, CLEAR=clear
;+++ check if keywords set, return if neither is set
  key1_set = KEYWORD_SET(message)
  key2_set = KEYWORD_SET(clear)
  IF key1_set+key2_set EQ 0 THEN RETURN

  COMMON WIDID
  IF widid.mainwid EQ -1 THEN RETURN ; widget not used yet

  ID=WIDGET_INFO(widid.mainwid, find_by_uname='status')

  IF KEYWORD_SET(message) THEN BEGIN
    IF SIZE(message, /TYPE) NE 7 THEN RETURN ; keyword message not of type string
    WIDGET_CONTROL, ID, set_value=message
  ENDIF

  IF KEYWORD_SET(clear) THEN $
    WIDGET_CONTROL, ID, set_value='idle'

END