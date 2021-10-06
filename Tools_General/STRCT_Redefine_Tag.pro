;------------------------------------------------------------------
; User defined IDL procedures and functions used in this programm
;------------------------------------------------------------------
; NONE
;
;*********************************************************************************************************************
FUNCTION STRCT_Redefine_Tag, Struct, Tag_Name=tag_name, Tag_Def=tag_def
;------------------------------------------------------------------
;+
; NAME:
;   STRCT_Redefine_Tag
;
; PURPOSE:
;   Redefining or adding one tag to an existing structure.
;
; CATEGORY:
;   Programming
;
; CALLING SEQUENCE:
;   Result=STRCT_Redefine_Tags(struct,tag_name=tag_name,tag_def=tag_def)
;
; INPUTS:
;   Struct: The existing structure that should be modified.
;
; KEYWORD parameters:
;   Tag_Name: The name of the tag that should be redefined or added.
;   Tag_Def: The definition (variable, object or pointer) assigned to the specific tag name.
;
; OUTPUTS:
;   Result: The modified structure including the new defined tag.
;
; OPTIONAL OUTPUT PARAMETERS:
;   NONE.
;
; COMMON BLOCKS:
;   NONE.
;
; SIDE EFFECTS:
;   NONE.
;
; RESTRICTIONS:
;   NONE.
;
; PROCEDURE:
;   See purpose.
;
; EXAMPLE:
;   a={k:1,l:2,m:3}
;   b={x:5,y:6,z:a}
;   ;Redefing a tag:
;   c=STRCT_Redefine_Tag(b,tag_name='z',tag_def=create_struct(b.z,'n',4))
;   ;Adding a tag:
;   d=STRCT_Redefine_Tag(b,tag_name='n',tag_def=4)
;
; MODIFICATION HISTORY:
;   Written by Harald Boenisch, IAU, University Frankfurt, 22. October, 2012.
;-
;------------------------------------------------------------------
  IF NOT keyword_set(tag_def) THEN tag_def = !NULL

  tag_names = tag_names(struct)

  w=where(tag_names EQ strupcase(tag_name[0]),nw)

  IF (nw EQ 0) THEN BEGIN
    IF size(tag_def, /type) NE 0 THEN struct_new=create_struct(struct,tag_name,tag_def) ELSE struct_new=struct
    RETURN, struct_new
  ENDIF

  n = 0
  struct_new = !NULL
  IF (w EQ 0) THEN BEGIN
    IF size(tag_def, /type) NE 0 THEN struct_new=create_struct(struct_new,tag_names[0],tag_def)
  ENDIF ELSE struct_new=create_struct(tag_names[0],struct.(n))

  FOR n=1,n_tags(struct)-1 DO BEGIN
    IF (w EQ n) THEN BEGIN
      IF size(tag_def, /type) NE 0 THEN struct_new=create_struct(struct_new,tag_names[n],tag_def)
    ENDIF ELSE struct_new=create_struct(struct_new,tag_names[n],struct.(n))

;   IF (w EQ 0) THEN struct_new=create_struct(tag_names[0],tag_def) $
;               ELSE struct_new=create_struct(tag_names[0],struct.(n))
;   FOR n=1,n_tags(struct)-1 DO BEGIN
;      IF (w EQ n) THEN struct_new=create_struct(struct_new,tag_names[n],tag_def) $
;                  ELSE struct_new=create_struct(struct_new,tag_names[n],struct.(n))
  ENDFOR

  RETURN, struct_new

END
;************************************************************************************************************************
