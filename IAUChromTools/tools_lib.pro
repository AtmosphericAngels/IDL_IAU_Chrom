;------------------------------------------------------------------------------------------------------------------------
;+
; AUTHOR:
; H.Boenisch, S.Sala, F.Obersteiner
; 
; INFO:
; sort-of general tools; however mostly IAU_Chrom-specific...
;-
;------------------------------------------------------------------------------------------------------------------------

FUNCTION add_subst2chrom, file, substadd

  chrom=create_struct(file[0], 'subst', substadd)
  FOR i=1, n_elements(file)-1 DO chrom=[chrom, create_struct(file[i], 'subst', substadd)]
  
  RETURN, chrom
  
END 

; **************************************************************************************

FUNCTION add_ires2subst, refs, refi 

  subst=create_struct(refs[0], 'ires', refi)
  FOR i=1,n_elements(refs)-1 DO subst=[subst,create_struct(refs[i], 'ires', refi)]
  
  RETURN, subst

END

; **************************************************************************************

FUNCTION conc_date, date1, date2, cdate1=cdate1, cdate2=cdate2

  caldat, date1, mm,dd,yy
  cdate1=string(yy,format='(I4)')+string(mm,format='(I02)')+string(dd,format='(I02)')
  caldat, date2, mm,dd,yy,hh,mn,ss
  cdate2=string(yy,format='(I4)')+string(mm,format='(I02)')+string(dd,format='(I02)') $ ; +'_'
    +string(hh,format='(I02)')+string(mn,format='(I02)')
  
  cdate = cdate1+'_'+cdate2

  RETURN, cdate
  
END

; **************************************************************************************

FUNCTION export_intres, chrom

  n_chrom=n_elements(chrom)
  n_subst=n_elements(chrom[0].subst)
  refi=create_refi()       
  subst_strct = { name:    '',$
                  formula: '',$
                  ires: refi }
      
  subst_strct = replicate(subst_strct, n_subst)
  
  intres_strct = { fname:      '',$                   
                   jdate:      !VALUES.D_NAN, $
                   instr_type: '',$        
                   subst:      subst_strct }
      
  intres_strct = replicate(intres_strct, n_chrom)
  
  STRUCT_ASSIGN, chrom, intres_strct 

  RETURN, intres_strct
  
END

; **************************************************************************************

FUNCTION get_finval, array

  w_finite = WHERE(FINITE(array) EQ 1, n_fin)
  
  IF n_fin EQ 0 THEN RETURN, !NULL $
    ELSE RETURN, array[w_finite]
  
END

; **************************************************************************************

FUNCTION get_finite_xv, x, v

  emptystrct={x:[0,0], v:[0,0]}
  
  IF N_ELEMENTS(x) NE N_ELEMENTS(v) THEN RETURN, emptystrct ; x & v must be of same size  
  
  w_x_finite=WHERE(FINITE(x) EQ 1)
  IF N_ELEMENTS(w_x_finite) EQ 1 THEN BEGIN
    IF w_x_finite EQ -1 THEN RETURN, emptystrct             ; NaN not allowed in x  
  ENDIF
  
  w_v_finite=WHERE(FINITE(v) EQ 1)
  IF N_ELEMENTS(w_v_finite) EQ 1 THEN BEGIN
    IF w_v_finite EQ -1 THEN RETURN, emptystrct             ; v did not contain finite values
  ENDIF
  
  IF N_ELEMENTS(x[w_v_finite]) EQ 1 THEN BEGIN              ; ensure return variable type array(2)
   x_res=[x[w_v_finite], MAX(x)+2]                          ; 2: offset out of plotting range
   v_res=[v[w_v_finite], 0]
  ENDIF ELSE BEGIN
    x_res=x[w_v_finite]                                     ; 2: offset out of plotting range
    v_res=v[w_v_finite]    
  ENDELSE

  strct = {x: x_res, $
           v: v_res}
           
  RETURN, strct
  
END

; **************************************************************************************

FUNCTION get_uniq_mass, chrom, SEL_CHROM=sel_chrom                   ; get unique masses of chromatograms

  tot_uniqm = []                                                     ; init total unique masses as empty array
  IF SIZE(chrom, /TYPE) NE 8 THEN RETURN, tot_uniqm 
  IF STRLEN(chrom[0].fname) EQ 0 THEN RETURN, tot_uniqm                ; abort if chrom not defined
  
  IF NOT KEYWORD_SET(SEL_CHROM) THEN BEGIN                           ; ALL CHROMATOGRAMS
    FOR i=0, N_ELEMENTS(chrom.fname)-1 DO BEGIN                      ; loop over all chromatograms
      w_fin = WHERE(FINITE(*chrom[i].mass) EQ 1, n_fin)
      IF n_fin GT 0 THEN BEGIN ; only append if finite values are found
        vd_mass = (*chrom[i].mass)[w_fin] ; exclued nans
        uniqm = vd_mass[UNIQ(vd_mass, SORT(vd_mass))]
        tot_uniqm = [tot_uniqm, uniqm]
      ENDIF
    ENDFOR
    IF N_ELEMENTS(tot_uniqm) GT 0 THEN tot_uniqm = tot_uniqm[UNIQ(tot_uniqm, SORT(tot_uniqm))]          
  ENDIF ELSE BEGIN       
    w_fin = WHERE(FINITE(*chrom[sel_chrom].mass) EQ 1, n_fin)
    IF n_fin GT 0 THEN BEGIN ; only append if finite values are found
      vd_mass = (*chrom[sel_chrom].mass)[w_fin]
      tot_uniqm = vd_mass[UNIQ(vd_mass, SORT(vd_mass))]   
    ENDIF                                            ; SPECIFIC CHROMATOGRAM 
  ENDELSE

  RETURN, tot_uniqm
  
END

; **************************************************************************************

FUNCTION compare_arrays, v1, v2, CASE_SENSITIVE=case_sensitive
; --> check where v2 matches v1
; comp_id -1 -> no match, same or different length
; comp_id 0  -> complete match, equal length, same order 
; comp_id 1  -> complete match, equal length, different order
; comp_id 2  -> partly match, equal length, same order
; comp_id 3  -> partly match, equal length, different order
; comp_id 4  -> partly match, different length


  strct={}
  
  ; check type
  IF SIZE(v1, /TYPE) NE SIZE(v2, /TYPE) THEN strct={comp_id:-1, ix:[-1], nvd:0}
  
  ; if comparison of strings should not be case sensitive, set both input string to upper case
  IF NOT KEYWORD_SET(case_sensitive) AND (SIZE(v1, /TYPE) + SIZE(v2, /TYPE)) EQ 14 THEN BEGIN
    v1=STRUPCASE(v1)
    v2=STRUPCASE(v2)
  ENDIF
  
  ; principal comparison
  ar_equal = ARRAY_EQUAL(v1, v2)
  
  ; both v1 and v2 are no arrays 
  IF N_ELEMENTS(v1) AND N_ELEMENTS(v2) EQ 1 THEN $
    strct={comp_id:ar_equal-1, ix:[ar_equal-1], nvd:ar_equal}  
  
  ; v1 is array, v2 is 1-element
  IF N_ELEMENTS(v1) GT 1 AND N_ELEMENTS(v2) EQ 1 THEN BEGIN
    w=WHERE(v1 EQ v2, nvd)
    IF nvd NE 0 THEN strct={comp_id:0, ix:w, nvd:nvd} $
      ELSE strct={comp_id:-1, ix:[-1], nvd:nvd}
  ENDIF

  ; v2 is array, v1 is 1-element
  IF N_ELEMENTS(v2) GT 1 AND N_ELEMENTS(v1) EQ 1 THEN BEGIN
    w=WHERE(v1 EQ v2, nvd)
    IF nvd NE 0 THEN strct={comp_id:0, ix:w, nvd:nvd} $
      ELSE strct={comp_id:-1, ix:[-1], nvd:nvd}
  ENDIF
  
  ; v1 and v2 are arrays
  w1 = WHERE(v1 EQ v2, nvd) ; find matching indices

  ix_sort_v1 = SORT(v1)
  ix_sort_v2 = SORT(v2)
  v1_sorted = v1[ix_sort_v1]
  v2_sorted = v2[ix_sort_v2]
  w2 = WHERE(v1_sorted EQ v2_sorted, nvd_sorted) ; find matches of sorted arrays
  
;+++
;+++ both arrays, v1 as long as v2 / comp_id 0, 1, 2 & 3
  IF (N_ELEMENTS(v1)+N_ELEMENTS(v2) GT 2) AND (N_ELEMENTS(v1) EQ N_ELEMENTS(v2)) THEN BEGIN
    IF ar_equal EQ 1 THEN $
      strct={comp_id:0, ix:w1, nvd:nvd} ; arrays match completely  
    IF ar_equal EQ 0 AND nvd_sorted EQ N_ELEMENTS(v1) THEN $
      strct={comp_id:1, ix:[ix_sort_v1, ix_sort_v2], nvd:nvd_sorted} ; complete match but different order
    IF nvd + nvd_sorted EQ 0 THEN $
      strct={comp_id:-1, ix:-1, nvd:nvd} ; no matches found
      
      ; partly match
      
  ENDIF
  
;+++  
;+++ both arrays, but unequal lengths / comp_id 4
  IF (N_ELEMENTS(v1)+N_ELEMENTS(v2) GT 2) AND (N_ELEMENTS(v1) NE N_ELEMENTS(v2)) THEN BEGIN
  
    w3 = [] ; scan v2 for values of v1
    totalmatches_v2v1 = 0
    FOR i=0L, N_ELEMENTS(v1)-1 DO BEGIN
      temp=WHERE(v2 EQ v1[i], nvd)
      totalmatches_v2v1=totalmatches_v2v1+nvd
      w3=[w3,temp]
    ENDFOR
    
    w4 = [] ; scan v1 for values of v2
    totalmatches_v1v2 = 0
    FOR i=0L, N_ELEMENTS(v2)-1 DO BEGIN
      temp=WHERE(v1 EQ v2[i], nvd)
      totalmatches_v1v2=totalmatches_v1v2+nvd
      w4=[w4,temp]
    ENDFOR
    
    IF totalmatches_v1v2 + totalmatches_v2v1 EQ 0 THEN $
      strct={comp_id:-1, ix:-1, nvd:0} ; no matches found
    
  ENDIF 
 
  RETURN, strct
  
END

; **************************************************************************************

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


; **************************************************************************************
; FUNCTION strreplace
; ***********************
; NAME:
;        STRREPLACE
;
; PURPOSE:
;        The STRREPLACE procedure replaces the contents of one string
;        with another.  The first occurrence of the search substring, Find
;        within the source string, String is replaced by the string,
;        Replacement.
;
; CATEGORY:
;        String Processing.
;
; CALLING SEQUENCE:
;
;        STRREPLACE, String, Find, Replacement
;
; INPUTS:
;        String:   The string to have substring(s) replaced.  If String is
;                  an array, Find is replaced by Replacement in the first
;                  occurrence of Find of every element of the array.
;
;        Find:     The scalar substring to be replaced. If this argument is
;                  not a string, it is converted using IDL's default
;                  formatting rules.
;
;        Replacement:   A scalar string to replace the Find substring. If
;                  this argument is not a string, it is converted using IDL's
;                  default formattting rules.
;
; EXAMPLE:
;
;        If the variable A contains the string "IBM is fun", the
;        substring "IBM" can be replaced with the string "Microsoft"
;        by entering:
;
;        STRREPLACE, A, 'IBM', 'Microsoft'
;
; MODIFICATION HISTORY:
;        Written by:    Han Wen, June 1995.
;-
FUNCTION strreplace, Strings, Find1, Replacement1

  NP        = N_PARAMS()  ;   Check integrity of input parameter
  if (NP ne 3) then msg=DIALOG_MESSAGE('Must be called with 3 parameters: '+$
                                       'Strings, Find, Replacement.', /ERROR)

  sz        = SIZE(Strings)
  ns        = N_ELEMENTS(sz)
  if (sz(ns-2) ne 7) then msg=DIALOG_MESSAGE('Parameter must be of string type.', /ERROR)

  Find      = STRING(Find1)
  pos       = STRPOS(Strings,Find)
  here      = WHERE(pos ne -1, nreplace)

  if (nreplace eq 0) then return, Strings

  Replacement=STRING(Replacement1)
  Flen      = STRLEN(Find)
  
  for i=0, nreplace-1 do begin
    j         = here(i)
    prefix    = STRMID(Strings(j),0,pos(j))
    suffix    = STRMID(Strings(j),pos(j)+Flen,$
      STRLEN(Strings(j))-(pos(j)+Flen))
    Strings(j) = prefix + replacement + suffix
  endfor
  
 RETURN, Strings;(j)
 
END

; **************************************************************************************
; FUNCTION valid_num
; ***********************
; NAME: 
;     VALID_NUM()
; PURPOSE:               
;     Check if a string is a valid number representation.
; EXPLANATION:              
;     The input string is parsed for characters that may possibly
;     form a valid number.  It is more robust than simply checking
;     for an IDL conversion error because that allows strings such
;     as '22.3qwert' to be returned as the valid number 22.3
;
;     This function had a major rewrite in August 2008 to use STREGEX
;     and allow vector input.    It should be backwards compatible.
; CALLING SEQUENCE: 
;     IDL> status = valid_num(string  [,value]  [,/integer])
;    
; INPUTS:
;     string  -  the string to be tested, scalar or array
;               
; RETURNS
;     status - byte scalar or array, same size as the input string
;              set to 1 where the string is a  valid number, 0 for invalid
; OPTIONAL OUTPUT:               
;     value     - The value the string decodes to, same size as input string.
;           This will be returned as a double precision number unless 
;           /INTEGER is present, in which case a long integer is returned.
;           
; OPTIONAL INPUT KEYWORD:          
;    /INTEGER   -  if present code checks specifically for an integer.
; EXAMPLES:
;     (1) IDL> print,valid_num(3.2,/integer) 
;        --> 0     ;Since 3.2 is not an integer 
;     (2) IDL> str =['-0.03','2.3g', '3.2e12']
;         IDL> test = valid_num(str,val)
;              test = [1,0,1]    &  val =  [-0.030000000 ,NaN ,3.2000000e+12]
; REVISION HISTORY:
;          Version 1, C D Pike, RAL, 24-May-93
;          Version 2, William Thompson, GSFC, 14 October 1994
;                       Added optional output parameter VALUE to allow
;                       VALID_NUM to replace STRNUMBER in FITS routines.
;          Version 3 Wayne Landsman rewrite to use STREGEX, vectorize
;          Version 4 W.L. (fix from C. Markwardt) Better Stregex expression, 
;                    was missing numbers like '134.' before Jan 1 2010
;-            

FUNCTION valid_num, string, value, INTEGER=integer

 ON_ERROR,2
 COMPILE_OPT idl2 
 
; A derivation of the regular expressions below can be found on 
; http://wiki.tcl.tk/989

   if keyword_set(INTEGER) then $ 
    st = '^[-+]?[0-9][0-9]*$'  else $                    ;Integer
     st = '^[-+]?([0-9]+\.?[0-9]*|\.[0-9]+)([eEdD][-+]?[0-9]+)?$' ;F.P.
   
;Simple return if we just need a boolean test.
    if N_params() EQ 1 then return, stregex(strtrim(string,2),st,/boolean)

   
      vv = stregex(strtrim(string,2),st,/boolean)      
      if size(string,/N_dimen) EQ 0 then begin     ;Scalar
         if vv then $
            value= keyword_set(integer) ? long(string) : double(string) 
      endif else begin                             ;Array 
         
      g = where(vv,Ng)
      if Ng GT 0 then begin      ;Need to create output vector
        if keyword_set(integer) then begin 
              value = vv*0L 
              value[g] = long(string[g])
        endif else begin 
                value = replicate(!VALUES.D_NAN,N_elements(vv))
                value[g] = double(string[g])
        ENDELSE 
        ENDIF   
        ENDELSE 
     
  RETURN, vv
       
END



PRO plotsym, psym, psize, FILL=fill,thick=thick,Color = color
  ;+
  ; NAME:
  ;     PLOTSYM
  ; PURPOSE:
  ;     Define useful plotting symbols not in the standard !PSYM definitions.
  ; EXPLANATION:
  ;     After a symbol has been defined with PLOTSYM, a plotting command should
  ;     follow with either PSYM = 8 or !P.PSYM = 8 (see USERSYM)
  ;
  ;     For additional rotationally symmetric plotting symbols, see VSYM.PRO
  ;     Also see CGSYMCAT in the/coyote directory.
  ; CALLING SEQUENCE:
  ;     PLOTSYM, PSYM,[ PSIZE, /FILL, THICK=, COLOR=]
  ;
  ; INPUTS:
  ;     PSYM -  The following integer values of PSYM will create the
  ;             corresponding plot symbols
  ;     0 - circle
  ;     1 - downward arrow (upper limit), base of arrow begins at plot value             value
  ;     2 - upward arrow (lower limt)
  ;     3 - 5 pointed star
  ;     4 - triangle
  ;     5 - upside down triangle
  ;     6 - left pointing arrow
  ;     7 - right pointing arrow
  ;     8 - square
  ;
  ;     Arrows are defined such that their base begins at their origin.
  ;
  ; OPTIONAL INPUTS:
  ;     PSIZE - Size of the plotting symbol in multiples of the default size
  ;               (default PSIZE=1).  Does not need to be an integer
  ;
  ; OPTIONAL INPUT KEYWORD:
  ;     FILL -  Parameter indicating whether to fill the symbol (see USERSYM)
  ;             The default is 0, unfilled symbol.  Does not affect arrows
  ;             or character symbols.
  ;     THICK -  Thickness of unfilled symbols. Default is 1.
  ;     COLOR - Color of the symbols, Default is !P.color
  ; OUTPUTS:
  ;     None
  ;
  ; EXAMPLES:
  ;     Plot Y vs. X with filled stars as the symbol, twice the default size
  ;     IDL> PLOTSYM, 3 ,2, /FILL       ;Plotting symbol is a filled star,
  ;                                       ;twice default size
  ;     IDL> PLOT,X,Y,PSYM=8            ;Set PSYM = 8 to get star symbol
  ;
  ;     Now plot Y vs. X with an open circle as the symbol
  ;
  ;      IDL> PLOTSYM, 0               ;Plotting symbol is a circle
  ;      IDL> PLOT,X,Y,PSYM=8
  ;
  ; METHOD:
  ;     Appropriate X,Y vectors are used to define the symbol and passed to the
  ;     USERSYM command.
  ;
  ; REVISION HISTORY
  ;      Written       W. Landsman         June 1992
  ;      18-JAN-1996    Added a square symbol, HCW.
  ;      98Aug20         Added keyword thick parameter - RCB.
  ;      April 2001     Added COLOR keyword    WBL
  ;-
  On_error,2

  if N_elements(psym) LT 1 then begin
    print,'Syntax - PLOTSYM, psym, [ size, /FILL, THICK= ]'
    print,'  PSYM values 0 - circle, 1 - down arrow, 2 - up arrow, 3 - star'
    print,'       4 - triangle, 5 - upside down triangle, 6 - left arrow'
    print,'       7 - right arrow, 8 - square'
    return
  endif

  if ( N_elements(psize) LT 1 ) then psize = 1 else psize = psize > 0.1

  if ~keyword_set(FILL) then fill = 0
  if ~keyword_set(thick) then thick=1

  case psym of
    0:  begin          ;Circle
      ang = 2*!PI*findgen(49)/48.     ;Get position every 5 deg
      xarr = psize*cos(ang)  &  yarr = psize*sin(ang)
    end
    1:  begin                                     ;Down arrow
      xarr = [0,0,.5,0,-.5]*psize
      yarr = [0,-2,-1.4,-2,-1.4]*psize
      fill = 0
    end
    2:  begin                                     ;Up arrow
      xarr = [0,0,.5,0,-.5]*psize
      yarr = [0,2,1.4,2,1.4]*psize
      fill = 0
    end
    3:  begin                                     ;Star
      ang = (360. / 10 * findgen(11) + 90) / !RADEG  ;star angles every 36 deg
      r = ang*0
      r[2*indgen(6)] = 1.
      cp5 = cos(!pi/5.)
      r1 = 2. * cp5 - 1. / cp5
      r[2*indgen(5)+1] = r1
      r = r * psize / sqrt(!pi/4.) * 2. / (1.+r1)
      xarr = r * cos(ang)   &   yarr = r * sin(ang)
    end
    4:  begin                                     ;Triangle
      xarr = [-1,0,1,-1]*psize
      yarr = [-1,1,-1,-1]*psize
    end
    5:  begin                                     ;Upside down triangle
      xarr = [-1, 0, 1, -1]*psize
      yarr = [ 1,-1, 1, 1]*psize
    end
    6:  begin                                     ;Left pointing arrow
      yarr = [0, 0, 0.5, 0, -.5]*psize
      xarr = [0,-2,-1.4,-2,-1.4]*psize
      fill = 0
    end
    7:  begin                                     ;Left pointing arrow
      yarr = [ 0, 0, 0.5, 0, -.5] * psize
      xarr = [ 0, 2, 1.4, 2, 1.4] * psize
      fill = 0
    end
    8:  begin                                     ;Square
      xarr = [-1,-1,1, 1,-1] * psize
      yarr = [-1, 1,1,-1,-1] * psize
    end
    else: message,'Unknown plotting symbol value of '+strtrim(psym,2)
  endcase

  if N_elements(color) GT 0 then $
    usersym, xarr, yarr, FILL = fill,thick=thick, color = color else $
    usersym, xarr, yarr, FILL = fill,thick=thick
  return
  
END


; *************************************Functions********************************************
; ******************************************Procedures**************************************


PRO show_list, event, chrom, EMPTY=empty

  IF SIZE(chrom, /TYPE) NE 8 THEN RETURN
  chromid = WIDGET_INFO(event.top, find_by_uname='sel_chrom')
    WIDGET_CONTROL, chromid, set_value = ''
  massid = WIDGET_INFO(event.top, find_by_uname='sel_mass')
    WIDGET_CONTROL, massid, set_value = ''
  baseid1 = WIDGET_INFO(event.top, find_by_uname='widbase11')
;    WIDGET_CONTROL, baseid1, map=0
  
  IF NOT KEYWORD_SET(empty) THEN BEGIN  
   WIDGET_CONTROL, chromid, set_value = FILE_BASENAME(chrom.fname)
    
   tot_uniqm = get_uniq_mass(chrom)
   IF tot_uniqm NE !NULL THEN BEGIN  
     WIDGET_CONTROL, massid, set_value = STRING(tot_uniqm, FORMAT='(D14.4)')
     WIDGET_CONTROL, baseid1, map=1
   ENDIF
   
  ENDIF

END
;+++++++++++++++++++++++
;+++++++++++++++++++++++
;+++++++++++++++++++++++
PRO tools_lib
END