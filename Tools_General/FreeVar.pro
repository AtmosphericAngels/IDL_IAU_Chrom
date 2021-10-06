;-------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
;       FreeVar
;
; PURPOSE:
;       Delete/Free a variable of any type.
;		If the input argument (var) is a pointer or an object, the referring heap
;		variable and the pointer or object reference itself will be destroyed. If
;		the input argument is a structure, all included heap variables are realeased
;		before the variable will be deleted.
;		In contrast to the simple use of the IDL procedures PTR_FREE or OBJ_DESTROY,
;		there will not left dangling references.
;
; CALLING SEQUENCE:
;       FreeVar, var
;
; EXAMPLE:
;       a=fltarr(100,100)
;       FreeVar,a
;       help,a -> undefined
;
; INPUTS:
;       var: the variable to be deleted
;
; OPTIONAL INPUT PARAMETERS:
;
; MODIFICATION HISTORY:
; (c) by David N. Bresch, 19960116
;
; (c) by Harald Boenisch, 20050411
;		Free all heap variables (pointers or objects) referenced by the input argument.
;
; (c) by Harald Boenisch, 20060418
;		Free recursively all heap variables (pointers or objects) referenced by the input argument,
;		including all array elements and structure fields.
;		When a valid pointer or object reference is encountered, that heap variable is marked
;		for removal, and then is recursively examined for additional heap variables to be freed.
;		In this way, all heap variables that are referenced directly or indirectly by the input
;		argument are located and realeased.
;-
;-------------------------------------------------------------------------------------------------------------------------
;*************************************************************************************************************************
;*************************************************************************************************************************
PRO FreeVar, var

	sz=size(var,/TYPE)

	IF (sz EQ 08) THEN HEAP_FREE,var,VERBOSE=0
	IF (sz EQ 10) THEN HEAP_FREE,var,VERBOSE=0
	IF (sz EQ 11) THEN HEAP_FREE,var,VERBOSE=0

	var = 0
	dummy = temporary(var)

END
;*************************************************************************************************************************
;*************************************************************************************************************************
