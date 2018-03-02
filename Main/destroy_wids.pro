;------------------------------------------------------------------------------------------------------------------------
;+
; PRO: destroy_wids
;
; AUTHOR: F. Obersteiner
;
; PURPOSE: closes widget(s) if identifier returns active=1 and widget id is defined in common variable. Keyword ALL
;          also closes the main widget.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO destroy_wids, ALL=all
  
  COMMON WIDID
  
    WIDGET_CONTROL, widid.intwid, BAD_ID=bad_id
      IF bad_id EQ 0 THEN WIDGET_CONTROL, widid.intwid, /CLEAR_EVENTS, /DESTROY
    WIDGET_CONTROL, widid.mmviewerwid, BAD_ID=bad_id  
      IF bad_id EQ 0 THEN WIDGET_CONTROL, widid.mmviewerwid, /CLEAR_EVENTS, /DESTROY
    WIDGET_CONTROL, widid.mcviewerwid, BAD_ID=bad_id 
      IF bad_id EQ 0 THEN WIDGET_CONTROL, widid.mcviewerwid, /CLEAR_EVENTS, /DESTROY
    WIDGET_CONTROL, widid.plot_ctrls, BAD_ID=bad_id 
      IF bad_id EQ 0 THEN WIDGET_CONTROL, widid.plot_ctrls, /CLEAR_EVENTS, /DESTROY
    WIDGET_CONTROL, widid.tpshskviewerwid, BAD_ID=bad_id
      IF bad_id EQ 0 THEN WIDGET_CONTROL, widid.tpshskviewerwid, /CLEAR_EVENTS, /DESTROY
    WIDGET_CONTROL, widid.mrwid, BAD_ID=bad_id
      IF bad_id EQ 0 THEN WIDGET_CONTROL, widid.mrwid, /CLEAR_EVENTS, /DESTROY

  IF KEYWORD_SET(ALL) THEN WIDGET_CONTROL, widid.mainwid, /CLEAR_EVENTS, /DESTROY

END