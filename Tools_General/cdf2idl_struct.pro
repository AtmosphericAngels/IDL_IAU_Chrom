;-------------------------------------------------------------------;
; User defined IDL procedures and functions used in this programm	  ;
;-------------------------------------------------------------------;
;
;
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;-
; cdf2idl_struct.pro - This file contains IDL functions to read netCDF data files
;               into IDL structure.
;
;  History:
;  Date       Name            Action
;  ---------  ------------    ----------------------------------------------
;  14 Dec 09  H. Boenisch     Created
;  31 Jan 12  H. Boenisch     Modified (removed bug for netcdf files without global attributes and/or variables)
;  18 Mar 14  H. Boenisch     Change illegale tagname (i.e. 'case' to '_case') in ValidateName
;  08 Aug 17  H. Boenisch     Close the open NetCdf at the end (added NETCDF_CLOSE,ncid)
;  14 Sep 21  F. Obersteiner  Do not remove whitespaces from filename in call to NCDF_OPEN - space in fname works fine.
;-
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;
;************************************************************************************************************************
;************************************************************************************************************************


;************************************************************************************************************************
function validateName, varname
	; func_description
	; This routine ensures that the given name does not start with a number,
	; nor contain a dash.  IDL cannot accept a variable starting with a
	; number or containing a dash.  If the name starts with a number, an
	; underscore is prepended to the name, and if it contains a dash, the
	; dash is replaced with an underscore.

	; Initialize the name.
	name = varname

	; If the name starts with a number, prepend it with an underscore.
	if (strpos(varname, '0') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '1') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '2') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '3') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '4') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '5') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '6') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '7') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '8') EQ 0) then name = strcompress("_"+varname)
	if (strpos(varname, '9') EQ 0) then name = strcompress("_"+varname)

	; If the name contains a dash replace it with an underscore.
	if (strpos(name, '-') NE -1) then begin
	   pieces = str_sep(name,'-')
	   n_pieces = n_elements(pieces)
	   name = pieces[0]
	   for i=1,n_pieces-1 do begin
	      name = strcompress(name+"_"+pieces[i])
	   endfor
	endif

   ; If the name contains a colon replace it with an underscore.
	if (strpos(name, ':') NE -1) then begin
	   pieces = str_sep(name,':')
	   n_pieces = n_elements(pieces)
	   name = pieces[0]
      for i=1,n_pieces-1 do begin
	      name = strcompress(name+"_"+pieces[i])
	   endfor
	endif

	; If the name contains a at sign replace it with an underscore.
	if (strpos(name, '@') NE -1) then begin
	   pieces = str_sep(name,'@')
	   n_pieces = n_elements(pieces)
	   name = pieces[0]
      for i=1,n_pieces-1 do begin
	      name = strcompress(name+"_"+pieces[i])
	   endfor
	endif

   if (strmatch(name,'case',/fold) EQ 1) then name='_case'

	; Return the file name.
	return, name

end
;************************************************************************************************************************


;************************************************************************************************************************
FUNCTION cdf2idl_struct, fname, suffix=suffix, verbose=verbose, gattname=gattname, varname=varname, lattname=lattname

	IF KEYWORD_SET(SUFFIX) THEN BEGIN
		suffix_cdf = '.'+suffix
	ENDIF ELSE BEGIN
		suffix_cdf = '.nc'
	ENDELSE

	IF (N_PARAMS() EQ 0) OR (N_ELEMENTS(fname) EQ 0) THEN BEGIN
		title = 'Select NetCDF file to load (*'+suffix_cdf+') to load'
		filter = '*'+suffix_cdf
		fname_cdf=DIALOG_PICKFILE(/READ,TITLE=title,FILTER=filter)
	ENDIF ELSE BEGIN
		fname_cdf = fname
	ENDELSE

	io=FILE_TEST(fname_cdf,/READ,/REGULAR)
	IF (io EQ 0) THEN BEGIN
		msg=DIALOG_MESSAGE(['ERROR occurred in GET_NetCDF_5975:','The file '+fname_cdf+' does not exist'])
		RETALL
	ENDIF

	file_cdf=FILE_BASENAME(fname_cdf,suffix_cdf,/FOLD)
	path_cdf=FILE_DIRNAME(fname_cdf,/MARK_DIR)

	; Ensure that the netCDF format is supported on the current platform.
	IF NOT(NCDF_EXISTS()) THEN BEGIN
	   msg = DIALOG_MESSAGE("The Network Common Data Format is not supported on this platform.")
	   RETALL
	ENDIF

	; Open the netcdf file for reading.
	ncid=NCDF_OPEN(fname_cdf) ; strcompress(fname_cdf,/remove_all))
	IF (ncid EQ -1) then begin
	   msg=DIALOG_MESSAGE("The NCDF file "+fname_cdf+" could not be opened, please check the path.")
	   RETALL
	ENDIF

	; Retrieve general information about this netCDF file.
	ncinfo = NCDF_INQUIRE(ncid)

	IF (ncinfo.Ngatts GT 0) THEN gattname = STRARR(ncinfo.Ngatts) ELSE gattname=''

	; Place the NCDF global attributes in idl structure.
	FOR i=0, ncinfo.Ngatts-1 DO BEGIN
		name=NCDF_ATTNAME(ncid,/GLOBAL,i)
		NCDF_ATTGET,ncid,/GLOBAL,name,vals
		info=NCDF_ATTINQ(ncid,name,/GLOBAL)
     	IF (info.datatype EQ 'CHAR') THEN vals = STRING(vals)
		IF (i EQ 0) THEN BEGIN
		  ; replace illegal characters!
		  name = name.replace('.', '_')
			ncdf=CREATE_STRUCT(validateName(name),vals)
		ENDIF ELSE BEGIN
		  ; replace illegal characters!
		  name = name.replace('.', '_')
			ncdf=CREATE_STRUCT(ncdf,validateName(name),vals)
		ENDELSE
		gattname[i] = name
	ENDFOR

	IF (ncinfo.Nvars GT 0) THEN varname = STRARR(ncinfo.Nvars) ELSE varname=''

	; Place the NCDF variables and local attributes in the same idl structure.
	FOR i=0, ncinfo.Nvars-1 DO BEGIN
      varinfo=NCDF_VARINQ(ncid,i)
      NCDF_VARGET,ncid,i,vals
      IF (n_elements(ncdf) EQ 0) THEN BEGIN
         ncdf=CREATE_STRUCT(validateName(varinfo.Name),vals)
      ENDIF ELSE BEGIN
         ncdf=CREATE_STRUCT(ncdf,validateName(varinfo.Name),vals)
      ENDELSE
		varname[i] = varinfo.Name
		FOR j=0, varinfo.Natts-1 DO BEGIN
         name=NCDF_ATTNAME(ncid,i,j)
			NCDF_ATTGET,ncid,i,name,vals
			info=NCDF_ATTINQ(ncid,i,name)
			IF (info.datatype EQ 'CHAR') THEN vals = STRING(vals)
			ncdf=CREATE_STRUCT(ncdf,validateName(varinfo.Name)+"_"+strcompress(name,/REMOVE_ALL),vals)
			IF (n_elements(lattname) EQ 0) THEN BEGIN
				lattname = varname[i]+"_"+name
			ENDIF ELSE BEGIN
				lattname=[lattname,varname[i]+"_"+name]
			ENDELSE
		ENDFOR
	ENDFOR

	; Close the netcdf file.
	NCDF_CLOSE, ncid

	RETURN, ncdf

END
;************************************************************************************************************************
;************************************************************************************************************************
;+++++++++++++++++++++++;
; Test program			;
;+++++++++++++++++++++++;
;fname_cdf=DIALOG_PICKFILE(/READ,TITLE='Select NetCDF file (*.cdf) to load',FILTER='*.cdf',PATH='E:\IDL_WD')
;
;cdf=cdf2idl_struct(fname_cdf,gattname=gattname,varname=varname,lattname=lattname,verbose=1)
;
;PRINT, 'Job is done !!!'
;
;END
;************************************************************************************************************************
;************************************************************************************************************************