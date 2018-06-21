;-----------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; IAU_CHROM_MAIN
; 
; Idea and Conception:
; H.Boenisch , A.Engel
;
; Programming and Developement:
; H.Boenisch , A.Engel (up to v3), S.Sala (up to v4.1), F.Obersteiner (v4.1 to v5.x)
;-
;------------------------------------------------------------------------------------------------------------------------
@def_ref_structs
@def_common
@wid_int_tools
@arr_get_finite_values
@conc_date
@FreeVar
@jultime2timestring
@plotsym
@strreplace
@valid_num
@tools_lib
@calc_tic
@msinfo_tools
@chrom_operations
@wid_main_tools
@wid_main
@wid_fragrat
@wid_integration
@wid_mmassviewer
@wid_mchromviewer
@wid_tpshskviewer
@wid_plotctrls
;------------------------------------------------------------------------------------------------------------------------
PRO IAU_Chrom_v518

  !Except=0

  def_common
  
  COMMON DATA
  COMMON COM_PLOT

  path = 'E:\KIT_DATA\MPI_GHGGC_Data_netcdf'
;  path = 'D:\'

  error_handler_IO = 0
  
  wid_main_ini
  
;  Outdir='D:\PROGRAMMING\debugging\iau_chrom\rt'
;  sfile='D:\PROGRAMMING\IDL_WD\IAU_Chrom_v5X\IAU_Chrom_v518.sav'
;  MAKE_RT, 'IAU_Chrom_v518', Outdir, SAVEFILE=sfile, /OVERWRITE
  
END
;########################################################################################################################
; *** NOMENCLATURE / COMMONLY USED VARIABLES ***
; ***
; vd: valid
; nvd: number of valid
; ivd: invalid
; nivd: number of invalid
; v: value (f(x),y etc.)
; id: identifier (integer), e.g. for widget elements
; ix: index
; strct: structure
; w: where function result
;########################################################################################################################
; *** KNOWN BUGS *** 
; ***
; - gumble integration only works properly with constant baseline
; - gumble 2nd peak and baseline integration sometimes return negative area/height; doesn't make sense if 2nd peak 
;   only is used
; - multimass-viewer: relative abundances will disappear if a different chromatogram is selected. they can be recalled
;   by reselecting the substance.
;########################################################################################################################
; *** CHANGELOG F.Obersteiner (>5.1 newest first; older version: chronological) ***
; ***
; 
; 2018-03-06: v5.18
; - updated import routine for Almsco/Markes Tof cdfs. added check if scan index vector contains duplicate entries.
;   Note: these duplicate entries seem to be created if the "dynamic baseline subtraction" of the Benchtof software is
;   used.
; 
; 2017-11-24: v5.17
; - updated the manual; screenshots and scripting feature.
; ***
; 2017-11-22: v5.17
; - modification of plot_intres routine so that result plots can be saved if running a database script.
; ***
; 2017-10-12: v5.17
; - bugfix on FUNCTION int_bl_fixpoints, fixed out of range error if maximum of peak is at the outer end of the supplied
;   data vecotr.
; ***
; 2017-09-21: v5.17
; - bl dynamic rt: returns no peak found if integration window suggested by peak detection is nonsense.
; ***
; 2017-08-30: v5.17
; - baseline integration, fixpoints: bugfix, baseline subtraction.
; - baseline integration, fixpoints: added constant baseline option (allows "slicing" of double peaks if RT is very
;   stable).
; ***
; 2017-08-30: v5.16
; - baseline integration, dynamic RT: added noise check
; - all GC data import functions: added keyword LOUD; if not set, dialog messages are suppressed.
; ***
; 2017-08-14: v5.16
; - call_iauchrom_dbscript: added keyword 'sort_by_jdate' which is given to the data import functions.
; - read_ecd_fid_cdf: switched timestamp to channel file (.ch) creation time
; - call_iauchrom_dbscript: added clearing of heap memory (dangling pointers).
; ***
; 2017-08-10: v5.16
; - updated cdf2idl_strct routine, added closing of ncid.
; - bugfix on read_iauchromdbfile function: empty cells are now imported correctly.
; - main wid / call of script: added questions if noise should be calculated and if Tofwerk peakdata should be 
;   recalculated.
; - added refresh of main wid droplists after a db script has been processed.
; - FUNCTION jultime2timestring: changed keyword name from "dateonly" to "onlydate" to avoid ambiguous keywrd error.
; - PRO call_iauchrom_dbscript: added definition of chrom.instr_type.
; - code cleanup on main widget.
; - bugfix on call_iauchrom_dbscript: now correctly processes only experiments set active (1).
; ***
; 2017-08-01: v5.15
; - PRO destroy_wids bugfix; now uses widget_control -> bad_id to check if a widget exists before trying to close it.
; - read_ecd_txt: sort by data timestamp counter variable now correct (n instead of i).
; ***
; 2017-06-12: v5.15
; - PRO intres2txt_sglsubst: added format code D25.8 to area and height variables.
; ***
; 2017-06-09: v5.15
; - FUNCTION iauchrom_db_chk: finished debugging version of script file checker.
; ***
; 2017-06-06: v5.15
; - FUNCTION read_tofwerkh5: removed FNAME keyword as this is replaced by DEF_FILE.
; - PRO call_iauchrom_dbscript: added functions to process data from other instruments.
; ***
; 2017-06-02: v5.15
; - integration widget: modified conditions under which the integration is called (avoids double call).
; - wid_main: some code cleanup
; - finished debugging version of database script processor
; ***
; 2017-06-01: v5.14
; - FUNCTION int_bl_fixpoints: added width calculation
; - FUNCTION int_baseline_gau: modified so that different baseline fitfunctions (1st and 2nd order) are available.
;                              Changed number of data points used for baseline calculation from 18 to 12 (6 on each side).
; - integration methods: removed 'gauss_fix'                              
; ***
; 2017-05-31: v5.14
; - integration widget: removed option to export default settings (redundant)
; - put functions for db reftable read/write in rw_db_reftable.pro
; - coded FUNCTION int_bl_fixpoints and integrated it into call_integration procedure.
;                  -> still misses peak width determination.   
; ***
; 2017-05-30: v5.14
; - added folder 'db_features' to iau_chrom project
; - added FUNCTION read_iauchromdbfile (loads database ref table)
; - added FUNCTION write_iauchromdbfile (saves database ref table)
; - integration methods: added method 'Fix_2point_BL' in common vars definitions.
; - integration methods: added FUNCTION int_bl_fixpoints 
;                        (not coded yet and also not yet implemented in call_integration procedure)
; ***
; 2017-05-24: v5.14
; - wid_main.pro: -> removed restart option ('File' tab).
;                 -> added 'Process Database' to 'DataAnalysis' tab.
;                 -> added 'Viewer' tab.
;                 -> moved viewers (MCV, MMV etc.) to Viewer tab.
;                 -> moved 'Plot Controls' to Viewer tab.
;                 -> deleted 'Plot' tab.
; ***
; 2017-05-16: v5.14
; - started programming procedure "call_batch_proc" to process a directory: load chromatograms, load msinfo,
;   integrate & calc. noise.
; - bugfix on read_AES_cdf: AM/PM timestamp format correction.
; ***
; 2017-05-11: upcoming v5.14
; - modified functions for data import: added keyword SORT_BY_JDATE to automatically sort files by
;   their measurement timestamp. If the keyword is not set, a question popup appears if the measurement
;   timestamp is not monotonically ascending.
; - added instrument types 5:AED and 6:ECD/FID (from GHGGC).
; - created def_ref_structs.pro to hold functions that create reference structures.
; - chrom data structure: added tag info_str, a string variable that can hold information from the loaded
;   measurement in string format e.g. operator or sample name.
; - ECD/FID from GHGGC: included sample name and operator in chrom.info_str (tab-separated).
; ***
; 2017-04 & 2017-05: v5.1 to v5.13
; - minor modifications of selection of x-/yrange from integration widget.
; - added data import functions for MPI-C AED and ECD/FID data (both netcdf).
; - modified FUNCTION read_subst: added keyword DEF_FILE to override the pickfile dialog.
; - modified functions for data import: added keyword DEF_FILE to override pickfile dialog.
; ***
; 2016-10-18: v5.1
; - minor tweaks: rewritten plot controls widget, renamed some fields on tps hsk viewer. set main widget and 
;   integration widget to autosize
; ***
;################### older versions chronological ########################################################################
; ***
; 2013-10-22: v4.23
; ***
; - Created 'read_tofwerkh5.pro' to import TOFWERK HDF5-files into IAU_CHROM 'refd'-structure. It consists of one
; function which calls two other functions. 'reas_tofwerkh5.pro' is stand-alone, i.e. it is a plugin for IAU_CHROM v4.2.
; - Minor changes in 'wid_main_ini.pro' to call the TOFWERK HDF5-import function.
;   NOTE: for peak integration in TOFWERK HDF5-files, a dedicated version of the msinfo file is required to select
; quantifier masses.
; - Added 'exit' and 'restart' functions to the IAU_CHROM main widget.
; ***
; ***
; 2013-10-24: v4.26
; ***
; - Changed the properties of 'p_obj0' in 'def_common.pro' so it can plot 7 curves.
; - Added a more general procedure in 'tools_lib.pro' which creates the selection of values of the ionmass-droplists on the widget.
;   This procedure 'show_list_viewer' can loop over multiple droplists specified in 'uname_mass_list'.
; - Created a "Multimass-Viewer", accessible under 'Data Analysis' -> 'Viewer: Multiple Masses'. It can plot seven different
;   ion masses in p_obj0. Data is automatically loaded into the viewer when initialized. If a different file is
;   selected, the previously selected masses are plotted. Pressing the 'reset plot...'-button clears p_obj0 and all masses have to
;   be selected anew. The 'exit'-button will close the selection-window and reset p_obj0.
; ***
; ***
; 2013-10-25: v4.28
; ***
; - Created a "Multichrom-Viewer" to overlay 1 mass / max. 7 chromatograms, accessible under 'Data Analysis' -> 'Viewer: MultiMass'.
;   Like the Multimass-Viewer, it uses p_obj0. Data is loaded into the viewer by pressing the 'get data...'-button on the widget.
;   After a mass has been selected, the chromatograms are plotted when selected. The 'reset plot...'-button clears p_obj0.
;   The 'exit'-button will close the selection-window and resets p_obj0.
; - Prepared features "IsoRat Calc" and "SN Calc"
; ***
; ***
; 2013-10-26: v4.283
; ***
; - Changed program structure of mm-viewer so data is already loaded when the viewer is opened. The 'refresh'-button
; still exists and can be used if different data is loaded. Same changes for mc-viewer.
; - Preparations for introduction of a signal-to-noise calculator
; ***
; ***
; 2013-11-01: v4.286
; ***
; - fixed text location bug (info-text when using peak integration), text fields now have keyword TARGET=p_obj1
; - changed logic of p_objects: base is p_obj0 and p_obj1, overplotted objects are named p_obj#a, ...b etc. while # is the
; number of the p_obj base
; - introduced text fields for p_obj0
; - changed droplists to comboboxes in mm-viewer
; ***
; ***
; 2013-11-05: v4.29
; ***
; - introduced sn-calculator
; ***
; ***
; 2013-11-06: v4.30
; ***
; - introduced editabel and tabable comboboxes in multi-mass-viewer
; ***
; ***
; 2013-11-07: v4.31
; ***
; - extended functionality of multimass-viewer
; - 'load ion preset' loads an msinfo file and displays a list of substances. if a substance is selected, the according
; molecule fragment ion mass traces are plotted.
; - p_obj0 text now displays the masses and relative abundances if available
; - externalized combobox- and viewer-specific functions to viewer_tools.pro
; ***
; ***
; 2013-11-08: v4.32
; ***
; - extended functionality of multichrom-viewer (combobox for mass entry, chrom information on plot window)
; - chrom0_mass changed to uniq_mass (original def.)
; - uniq_mass can be calculated with FUNCTION get_uniq_mass (tools.lib) for any supplied chrom with specified chrom number
; - uniq_mass is a common variable; use/refresh with caution
; - small simplification in the code of wid_main_ini/wid_main_handle
; - changed name of plot obj 1 refresh_text procedure to 'refresh_text_pobj1' and included set_zero option
; - fixed mmv-bug, it can now be opened/closed/opened etc. without crashing IAU-CHROM
; ***
; ***
; 2013-11-11: v4.40
; ***
; - included gumble integration method, functions by H.Boenisch Aug. 2012 / PeakIntegration_v02 (!) set gumble initial A1
; from 60 to 1 for timeaxis in seconds
; - small optical changes on integration widget
; - included some comments in integration widget code
; ***
; ***
; 2013-11-12: v4.41
; ***
; - introduced a selector for the chromatogram time scale on main widget. read_cdf/read_h5/read_cdf can now be signed
; with this timescale. Introduced variable t_scale in refd. PRO intres2txt will now write RT[s] if seconds are
; selected or RT[min] if minutes selected
; - small structural changes on integration widget
; - introduced a noise calculation on integration widget. currently uses int_win from chrom structure as parameters for upper
; and lower limit on the mass trace.
;   if no noise is calculated, a 'NAN' will appear in the report file.
; - changed separator in intres report file to tab
; ***
; ***
; 2013-11-13: v4.42
; ***
; - bugfix: exit-button on main wid now closes all open widgets that have been worked with (see known bugs)
; - introduced a procedure 'PRO check_pobjects' that checks if p_obj0 or p_obj1 has ben closed. if so, the closed
; p_obj will be restored
; - updated integration report file, it will now show if timescale was minutes or seconds or only 'RT' if info not available
; - changed refresh behavior of mmviewer; all plot-objects will be refreshed if any parameter is changed
; - introduced timescale keyword in int_gbl_v2. if not set, minutes is default. makes a difference in initiation parameters.
; ***
; ***
; 2013-11-14: v4.43
; ***
; - changed refresh behavior of mcviewer; all plot-objects will be refreshed if any parameter is changed
; - included downward compatibility for 'restore experiment'
; - updated ires2txt_allsubst, it now creates a textfile where ires are written down substance-wise
; - modified doublegumble function: abort and return strct if one of the two gumble-peaks is 'not integrated'.
;   added flag and comment update to result strct.
; - modified call_integration_method: plot is only refreshed if comment is not 'not integrated'
; ***
; ***
; 2013-11-15: v4.45
; ***
; - added common variable 'int_meths', a string array containing the names of the available integration methods. to add a
; method name, add it to this strarr.
; - added common variable 'version' to store the current version number as a string. version is written into chrom struct
; when files are loaded
; - introduced int_meths into integration widget and modified call_integration_method (renamed to call_integration_method_v2)
; - shifted the report all - function from main wid to integration wid (report-menue)
; - introduced reload option for msinfo on integration wid (file-menue)
; - introduced update-procedure for all fields on integration wid (wid_integration_tools -> upd_all_fields;
; needs variables event & chrom)
; - by changing the msinfo in wid integration, a different msinfo can be used in multimassviewer
; - added info text on p_obj0 when using wid integration. selected mass is displayed.
; ***
; ***
; 2013-11-18: v4.455
; ***
; - bugfix on dbl gbl integration: if first peak is found but second peak is NAN, result of first peak is reported.
; - bugfix on mmv: substance names of first chromatogram are displayed when loading preset (avoids crash).
; ***
; ***
; 2013-11-19: v4.46
; ***
; - downward compatibility bugfix: old integration method names restored. Old msinfo files usable again.
; - bugfix on 'restore': if restore is aborted, chrom & subst are reestablished as a structure so that other
;   features dont crash when called.
; ***
; ***
; 2013-11-20: v4.465
; ***
; - changed refresh behavior of pobj1 when using peak integration: integration is only called when integration
;   settings are changed or button 'apply current' & 'apply def' & 'res def'
; - separated noise calculation from peak integration
; - small structural changes on wid integration
; ***
; ***
; 2013-11-21: v4.47
; ***
; - introduced PRO plot_routine_pobj0 and renamed PRO plot_routine to plot_routine_pobj1
; - added keyword SET_ZERO to PRO plot_routine_pobj1 to use it for resetting plot object 1. No other keywords
; - have to be specified in that case.
; - added keyword EXCL_GENERAL to PRO upd_all_fields to use it for updating the settings-fields on wid integration
; - bugfix on noise settings: apply to all now saves current settings to all loaded files in chrom structure
; ***
; ***
; 2013-11-22: v4.475
; ***
; - small changes in refresh behavior of integration widget. avoids multiple calling of integration
; - changed ires.noise to type FLTARR; noise is written to noise[quantifier]
; ***
; ***
; 2013-11-27: v4.50
; ***
; - introduced common variable instr_type
; - introduced tic calculation with different methods depending on instr_type
; - changed import routine for BenchTof-files
; - adjusted mcv and mmv to display tic
; - simplifications in case structure of mcv and mmv
; - integration: enabled user defined comment
; - integration: if flag is set to 'bad Peak', the selected chromatogram will not be integrated when 'apply settings' is called
; - integration: if 'apply default' is called, comment and flag will reset to 0/'not integrated' before integration
; ***
; ***
; 2013-11-29: v4.51
; ***
; - began work on calculator for fragment ratios
; ***
; ***
; 2013-12-09: v4.54
; ***
; - fragment ratio calculator: alpha version completed. Improvements needed on peak max detection and results management.
; - removed ymin keyword from plot0 and plot1 (downward compatibility)
; - ms data import routines: added sort function to sort data according to filename
; ***
; ***
; 2013-12-17: v4.57
; ***
; - fragment database analysis tools
; - plot-routines: automatically trigger "set_zero" if x or v has less than 2 elements (plot would crash)
; - updated refresh behavior of integration plot (p_obj1) so that last chrom is displayed correctly
; ***
; ***
; 2013-12-21: v4.59
; ***
; - introduced quicklook report (a/v, h/v, a/h, rt, noise; matrix: chroms x substances)
; - introduced read_exp_info to import exp.info-file which can contain information on samples, cals, volumes etc.
; ***
; ***
; 2014-02-03: v4.60
; ***
; - included tofwerk hdf5 peaktable into refd / chrom struct.
; - AUTO-EVAL => Stephan!
; ***
; ***
; 2014-02-25: v4.61 (buggy v4.60RC)
; - changed almsco cdf import routine: float mass values are dedicated to either nominal masses or .5 masses..
; ***
; ***
; 2014-02-28: v4.62
; ***
; - introduced common variable tot_uniqm which contains the unique masses of all loaded chromatograms.
; - changed viewer tools to work with tot_uniqm.
; - changed multi-chrom-viewer to work with tot_uniqm.
; - added msel recalculation to integration widget so that after integrating multiple chroms, the correct mass trace
; is displayed in plot window 0.
; - bugfix on main_widget: previously displayed textfields and mass traces in plot window 0 will get refreshed if
; a mass / a chromatogram is selected on the iau_chrom main widget
; - updated reset and exit code of the viewers to make use of pobj0 - routine "set_zero". Chrom nbr of mmv is now also
; resetted to first chromatogram.
; - added selected chrom info on pobj0 when using mmv
; - made comboboxes of mcv tabbable
; - added "known bug" to list: disappearing relative abundances when switching chromatograms
; ***
; ***
; 2014-03-03: v4.62
; ***
; - changed update behavior of noise-calc. plot 0 will now directly show the noise calculation
; - Tofwerk hdf5: extended peaktable-check to search for double alias masses and alias masses out of intervals.
;   File import will be aborted if such cases are true. The comparison of two peaktables for equality is now optional.
; ***
;########################################################################################################################
; *** IDL VERSION 8.3 x64 ***
; ***
; ***
; 2014-03-13: v4.63
; ***
; - changed refresh behavior of integration widget: integration curves are now deleted when flagging
;   anything but integrated.
; - added save option for subst strct (when saving the experiment)
; - added restore option for subst strct (when restoring an old experiment)
; - added integration widget warning if subst not loaded
; - created VM version
; ***
; ***
; 2014-03-22: v4.64
; ***
; - bugfix on h5_ptablecheck function: added TOTAL to alias mass comparison and added useful error messages.
; - bugfix on read_tofwerkh5 function: removed error message (pt unequal) as it is now in h5_ptablecheck.
; - general bugfix to avoid crash if chrom cannot be created
; ***
; ***
; 2014-04-28: v4.64
; ***
; - added 'set filepath' option on main widget under 'file' (wid_main_ini.pro).
; - bugfix on aborted file import: iau chrom does not crash anymore due to undefined uniqmass variable.
;   variable check executed on wid_main_ini.pro.
; ***
; ***
; 2014-04-29: created RC 4.64
; ***
; ***
; 2014-06-05: v4.65
; - additions to noise calc: selected mass is now displayed on plot0, noise fit is now plotted in blue,
;   3*SD borders are now +/- 1/2 of the noise level and plotted in red.
; ***
; ***
; 2014-06-10: v4.66
; - implemented plot routine (pobj0) in main-widget code for mass and chromatogram selection.
; - changed update behavior of plot 0 when in integration mode. Plot0 is now only updated if a button is pressed.
; - fixed bug (false update behavior) on noise-calc: selected quantifier massis now correctly used for noise calculation.
; ***
; ***
; 2014-06-11: v4.70
; - structural changes in read_subst (cleand up code).
; - implemented check for noise_win in read_subst (set to NaN if not specified).
; - added functions version_check, convert_oldchrom and convert_oldsubst to tools.lib.
; - main_wid: older experiments are now converted to the current version by convert_oldchrom and
;   convert_oldsubst. Instr_type is kept if specified (otherwise 0), iauchrom_vers is set to the current version.
; - changed variable type of 'iauchrom_vers' to float
; - increased scroll speed of sliders on integration widget
; - implemented noise_win to use instead of int_win for noise calculation
; ***
; ***
; 2014-06-12: v4.72
; - cleaned up code of integration widget (refresh handling and IDs)
; - changed variable type of 'sigma' to fltarr
; - replaced sigma droplists with editable comboboxes (also changed in wid_int_tools)
; - implemented t_scale check in 'restore experiment' routine
; ***
; ***
; 2014-06-16: v4.80
; - introduced baseline integration method.
; - peak detection: now only one function for all integration methods.
; - peak integration functions: parameter NTERMS_BASE is now 1-based, i.e. 1 equals a constant baseline etc.
; - plot 1 during integration: full mass trace is plotted, xrange is rt_win.
; - revised refresh behavior of integration plot for changing flag, introduced nointtriggers variable.
; - bugfix on sigma left/right comboboxes: settings are now read from chrom strct if not set manually.
; - bugfix in integration widget code: introduced STRUPCASE function when checking chrom strct comment.
; ***
; ***
; 2014-06-17: created v4.80 beta RC
; ***
; ***
; 2014-06-18: v4.80
; - FragRat and MMViever: changed check for chrom.subst to STRMATCH function.
; - FragRat: revised PRO adj_fragments for non-defined fralist variable and NaN/nd detection.
; ***
; ***
; 2014-06-19: v4.81
; - Forbid negative values for sigma left / right (integration and fragrat).
; ***
; ***
; 2014-06-22: v4.81
; - setup running version of frag rat calc including report function.
; ***
; ***
; 2014-06-23: v4.82
; - bugfix on int_baseline_gau: abort integration if time axis has less than 10 elements (added keyword TAXIS).
; - bugfix on wid_integration: selection of correct quantifier mass on widget.
; - bugfix on wid_main: all open data analysis tools are closed when a new experiment is loaded.
; - bugfix on wid_main: selection of '?' if multiple experiments are restored without t_scale information.
; - integration report files: added method to comment.
; - gauss integration: added keyword FIT_WIN (2-element array with x values for start and end of fit window) and
;   INT_WIN (2-element array with x values for start and end of integration window). Replaced vd with w_int_win and
;   vd_int with w_fit_win (correct nomenclature).
; - call integration: changed plot colors for gumble fit. Added plot of gauss integration, vdata in fit win orange.
; ***
; ***
; 2014-06-26: v4.82
; - bugfix on plot1 setting from call integration: keywords xrange & yrange for all plot steps.
; ***
; ***
; 2014-06-27: v4.82
; - added recalculation of xrange to call_integration (no limited to rt_win).
; ***
; ***
; 2014-06-30: v4.82
; - added modified gaussfit function which uses a fixed number of datapoints (given by rt_win) to calculated the fit.
;   The ratio of datapoint before vs. after peak rt is set by sigma left /right -> No dependency on peak width via
; gaussfit sigma. Final gaussfit is integrated with sigma=[20,20]. Modified gaussfit will not be included in VM RC.
; ***
; ***
; 2014-07-01: v4.83
; - bugfix on noise calc: now aborts if there are LE 3 datapoints for noise calculation. Avoids crash due to singular
;   matrix in polyfit.
; - bugfix on int_gau: now returns No Peak Found if peak rt is out of specified retention time window.
; - bugfix on restore routine: removed CHROM_ONLY (caused removal of subst in chrom strct).
; - addition to wid integration: sigma-values of msinfo are added to comboboxes if not in the 1 to 20 list.
; - changed display of gaussfit
; - modification of YRANGE: (max-min)*5% added on each end of the y-axis.
; - modification of plot report: realtive a/h and relative rt now plotted in one window but two plots.
; ***
; ***
; 2014-07-02: created v4.83 RC
; ***
; ***
; 2014-07-03: v4.84
; - added multiplot option to plot_routine_pobj0 and plot_routine_pobj1: e.g. OVER=123 plots p_obj1, p_obj1a and
;   p_obj1b. Requires multiple x and v datasets.
; - changed refresh order of pobj0 text fields.
; - added error handling if subst structure does not contain integration settings.
; - bugfix on wid integration: refresh behavior after reloading an msinfo.
; ***
; ***
; 2014-07-07: v4.85
; - noise_calc: added selection of active chromatogram on wid integration
; - added PRO export_msinfo. Exports msinfo (default or present settings), can be called from wid integration.
; - bugfix on fragrat_calc: returns empty fragres structure on error (avoids crash due to non-structure
;   type returns).
; ***
; ***
; 2014-07-07: created v4.85 RC
; ***
; ***
; 2014-07-08: v4.86
; - read_tofwerkh5: added H5F_CLOSE.
; - wid_integration: added option 'reload defaults' (MSINFO to subst strct).
; - wid_integration: added menue 'BatchProcess' with options 'Integrate' and 'Integrate+Calc.Noise'.
; - implemented batch processing in integration widget code.
; - call_integration and call_noisecalc: added keyword PLOT (decides if results are plotted (1) or not (0)).
; ***
; ***
; 2014-07-09
; - bugfix on wid integration tools: selection of correct sigmal value after 'reset to defaults'.
; - changed intres quicklook to postscript output (intres_qcklook.pro).
; - bugfix on main wid: added check before data import; loaded data found -> replace?
; ***
; ***
; 2014-07-11
; - added plotsym.pro, used in intres_qcklook.pro.
; - added keyword XYTITLE to plot_routines, 2-element string array, sets x- and ytitle if specified. Implemented
;   call by main widget.
; - added notification if chrom is loaded when 'restart' is called on main widget.
; - intres_qcklook: added check for singular plot xval/vval combinations (will be replaced with array by
;   get_finite_xv).
; ***
; ***
; 2014-07-14
; - introduced batch_process_chrom procedure. Called from main_wid.
; - reorganized procedures/functions: CHROM_operations.pro in 'general' folder.
; - added keyword 'ALL' to intres2txt_sglsubst.pro: if keyword_set, all substances are exported to individual
;   txt-files.
; - added PRO destroy_wids to main_wid_ini. Destroys open widgets if e.g. 'restart' is called.
; - added keyword EMTPY to PRO show_list (main_wid); if set, the procedure does not generate data to fill in droplists.
; ***
; ***
; 2014-07-15: created v4.87 RC
; ***
; ***
; 2014-10-14
; - gaussfit function: added check for negative peak area, returns 'no peak found'.
; - call integration: added parameter chk_noise = noise level for selected quantifier ion mass.
; - gaussfit function: added check if gaussian height is less than 2* chk_noise, returns 'no peak found'.
; ***
; ***
; 2014-10-20: v4.88
; - data analysis tools: added substract chrom_0 from chrom_1 functionality to multi-chrom viewer.
; ***
; ***
; 2014-11-07: v4.88
; - gaussfit: added width check to avoid false positive peak detection.
; ***
; ***
; 2014-11-26: v4.89
; - added function matchmass to avoid conflicts when msinfo masses do not exactly match peaktable masses from
;   HTOF data.
; ***
; ***
; 2014-11-28: v4.90
; - changed display of ionmass on plot1 to actually used masstrace.
; - wid integration: refresh behavior / no update of ionmass display on plot1 when refresh=1 (e.g. integration
;   applied).
; - gaussfit: width check time axis scaling added. Changed minimum width to 6 sigma (was 12).
; - plot x- and yrange: added PRO wid_ctrlplot0 and PRO wid_ctrlplot1 to manually set ranges.
; ***
; ***
; 2014-12-03: v4.90
; - removed 'BatchProcess' from main widget
; - bugfix on refresh text pobj 1 : promt of int_mass now only after chrom is restored and set_zero = false.
; ***
; ***
; 2014-12-05: v4.91
; - bugfix on msinfo export (rel abd, format).
; - mmv & mcv: display of peaktable.label if HTOF data.
; ***
; ***
; 2014-12-08: v4.91
; - added optional txt file prefix: rt from subst.
; ***
; ***
; 2015-01-07: v4.92
; - added keyword NOMONLY to functions calc_TIC_HTOF and calc_tic. If set, only masses labled 'nominal' are used for
;   HTOF data tic calculation. Keyword is set to 1 in mmv or mcv if instrument type is 3 (HTOF).
; ***
; ***
; 2015-01-13: v4.92
; - added FUNCTION STRREPLACE
; - eifrags_analysis.pro: extended functionality to get txt-file with mass;abundance from fragements db txt file.
; ***
; ***
; 2015-01-22: v4.93
; - added buttons 'get current x-/y-range' to plot-controllables widgets.
; - added checkbox 'substract background' to MMV.
; - added FUNCTION viewer_prep_pdata to viewer_tools. Prepares data for plotting and checks if background should be
;   substracted and/or x- and yrange is set to auto or fix. If yes, the minimum intensity is substracted from each
;   intensity array in the specified plot window.
; - added checkbox 'fix x-/y-range on MMV and MCV. Fixes ranges of plot object 0.
; ***
; ***
; 2015-01-27: v4.93
; - Main Wid: Replaced uniq_mass with tot_uniqm. Makes uniq_mass obsolete as a common variable as it is only a
;   specific case of tot_uniqm.
; - function get_uniq_mass now calculates tot_uniqm. Makes calculaten of tot_uniqm in viewers obsolete (deleted).
; ***
; ***
; 2015-01-28: v4.94
; - FUNCTION matchmass: added test for numeric (valid_num) and return if flase (0). Avoids error due to impossible
;   string conversion for 'none' and 'TIC'.
; - tools_lib / FUNCTION get_uniq_mass: added keyword 'sel_chrom' if the calculation should be done only for a
;   specific chromatogram (with index sel_chrom).
; - bugfix on TIC calculation: FUNCTION get_uniq_mass is now called with keyword 'sel_chrom'.
; - bugfix on MMV: restricted vector length for chrom substraction.
; - bugfixes on integration and noise calculation: replaced uniq_mass with tot_uniqm.
; - MMV: changed call of chrom substraction from button to checkbox.
; - Misc: added PRO Chrom_Residuals to calculate positive and negative residuals from the substraction of two
;   chromatograms over the hole (nominal) mass range.
; ***
; ***
; 2015-02-02: v4.94
; - gauss integration: added check if fit height LT 1.5*noise. Triggers 'no Peak found'.
; - batch int+noise: reversed order, noise is now calculated first (wid integration).
; ***
; ***
; 2015-02-05: v4.9X
; - relocated read_eifrags function to read_data folder.
; - combined eifrags_analysis.pro with ptable_tools.pro. Deleted eifrags_analysis.pro. Renamed ptable_tools.pro to
;   peaktable_tools.pro.
; - created HTOF_rawdata_tools.pro in HTOF_Tools folder.
; - created read_tofwerkh5_raw.pro in Read_Data folder.
; ***
; ***
; 2015-02-10: v4.9X
; - collected HTOF tools in folder 'HTOF_Toolbox'.
; - folder 'HTOF_Toolbox' up to date contains:
; h5_content_chk - overview of htof hdf5 file content.
; htof_hrdata_tool: functions to analyze htof data content such as mass calibration parameters.
; htof_massresacc: function to calculate mass resolution and accuracy besides some other parameters.
; htof_redo_masscali: (unfinished) function to recalculate the mass calibration parameters
; peaktable_tools: from previous version.
; read_tofwerkh5_hr: function to read the high resolution mass spectra from the htof data.
; - FUNCTION matchmass: added keyword limit_dif to return -1 instead of matchval if set.
; ***
; ***
; 2015-02-23: v4.95
; - function fragrat_calc: added keyword 'verbose' and keyword 'plot'.
; - all functions & procedures referring to fragrat calc: replaced variable uniq_masses with tot_uniq_masses
;   as the former variable is no longer in use.
; - bugfix on main wid: data reload doesnt crash anymore due to undefined chrom variable.
; - added keyword fname to function read_tofwerkh5. Skips pick file dialog.
; - added keyword verbose to function peak_detection. Shows plot of detected peak if verbose=1.
; - fragrat: bugfixes on x- and yrange while plotting integrated peaks.
; - got fragrat calc working fairly stable in this version.
; ***
; ***
; 2015-02-25: v4.9X
; - fragrat calc: added keyword 'int_type' to specify integration method. default is 0. 0: baseline p2p,
;   1: gauss fit2fit.
; - fragrat wid: added droplist 'int_type' to set to specify integration & comparison method for fragrat calc.
; - plot_routine_pobj1: added keyword 'set_colors' (see plot_routine_pobj0).
; - plot_routine_pobj1: added cases 123467 and 1234567.
; ***
; ***
; 2015-03-02: v4.9X
; - read_subst: added keyword 'use_nom'. If set, mass from msinfo is rounded to integer mass.
; - integration widget, file dropdown menue: added option to load msinfo with nominal masses.
; - noisecalc: disabled behavior of selection last chrom after calculation for all is done.
; - plot obj 1 text: changed format of SN display.
; ***
; ***
; 2015-03-10: v4.9X
; - introduced keyword 'fix_xyrange' to plot routines for plot objects 0 and 1. Implemented in integration widget and
; - call integration function.
; - bugfix on default settings prompt: empty subst structure after experiment has been restored only contains one
;   element, not one per chromatogram. Check now does not refer to an array anymore.
; ***
; ***
; 2015-03-15: v5.0X
; - major change in handling plots and text fields on plots: introduced object array as plot reference holder.
;   -> created new IDL project IAU_Chrom_v5.0x.
;   -> changes apply to:
;   def_common.pro
;   check_pobjects.pro
;   plot_routine_pobj0.pro
;   plot_routine_pobj1.pro
;   refresh_text_pobj0.pro
;   refresh_text_pobj1.pro
;   wid_ctrlplot0.pro
;   wid_ctrlplot1.pro
;   viewer_tools.pro
;   wid_mchromviewer_ini.pro
;   wid_mmassviewer_ini.pro
; ***
; ***
; 2015-03-16: v5.0X
; - minor changes like vectorization of keyword application in plot and text routines.
; - added refr_status.pro to indicate a status on the main widget. status is set by keyword 'message' when calling
;   the procedure.
; ***
; ***
; 2015-03-18: v5.0X
; - introduction of the option to recreate the text fields of plot0/plot1 as the fields are not recreated every
;   time anymore. option available on mmv, mcv (plot 0) and integration widget (plot 1)
; - mmv: plot menu, introduced option to create legend.
; - mmv: msinfo is now loaded automatically if available.
; ***
; ***
; 2015-03-18: v5.01
; - refresh text p obj 0: now displays masslabel if htof data detected and mmv used anytime.
; ***
; ***
; 2015-04-03: v5.0X
; - gaussfit function: introduced keyword 'rtwinisfitwin'. If set, the rt win used in peak integration is
;   also the window from which datapoints for the fit are taken from.
; ***
; ***
; 2015-04-03: v5.0X
; - bugfix on mmv: if a substances' mass is NaN in the msinfo (subst strct), it is now correctly set to 'none' in the
; combobox.
; - matchmass function: added limit of .5 where called ('limit_dif' keyword).
; - added keyword 'tot_uniqm' to refresh_txt_pobj0. If set, a peaktable is created (for e.g. BenchTof files) containing
;   the numbers given in tot_uniqm as masses and 'nominal' as label.
; ***
; ***
; 2015-06-25: v5.0X
; - wid_integration: added option to save default integration settings as ms.info.
; - save experiment: subst structure and equivalent ms.info are now saved in same dir as chrom*.dat.
; ***
; ***
; 2015-07-07: v5.0X
; - wid_main & def_common: changed plot display and update behavior.
; ***
; ***
; 2015-07-17: v5.0X
; - added error handler to wid_main, wid_fragrad, mmv, mcv and wid_integration (in the event handlers).
; - code simplification: wid_integration; reload of msinfo with and without nominal masses.
; ***
; ***
; 2015-07-18: v5.02RC: created.
; ***
; ***
; 2015-08-04: v5.03jl: created.
; ***
; ***
; 2015-09-17: v5.0X
; - introduced variable 'error_handler_IO' to activate/deactivate the error handlers globally (dev version only).
; ***
; ***
; 2015-09-18: v5.0X
; - subst file: ...is now selected automatically; does not have to be selected first before chrom file if
;   an experiment is restored.
; ***
; ***
; 2015-09-30: v5.04
; - 'manual integration' update. On integration widget, flag 2 is now usable and sets a comment 'Integration (man.)'.
;   A checkbox allows to specify if a manually set integration settings should be overwritten if e.g. 'apply to all'
;   is triggered.
; ***
; ***
; 2015-10-07: v5.05
; - 'peakdata recalc' update. If chosen, peakdata of Tofwerk TOF data is recalculated using an interpolation method
;   to avoid faulty integration due to data binning.
; - some code clean-up here and there and addition of clean hdf5 id closeing at the end of functions that deal with
; Tofwerk hdf5 data.
; ***
; ***
; 2015-10-08: v5.X
; - bugfix on wid main: correct choice if or not to restore subst
; - bugfix on chrom_operations / function restore old subst
; - bugfix on wid main: avoided error when something is selected on the widget that is not defined in the data.
; ***
; ***
; 2015-11-11: v5.X
; - bugfix: creation of substfile path.
; ***
; ***
; 2015-11-16: v5.05RC: created.
; ***
; ***
; 2016-01-20: v5.X
; - introduced keyword INSDATA_WARN to PRO call_noisecalc and UNCTION calc_noise_fct. Can be used to deactivate
;   the insufficient data warning during noise calculation if called by batch processing.
; ***
; ***
; 2016-02-15: v5.X
; - added FUNCTION tw_h5_filecheck to read_tofwerkh5.pro to check file integrity before import. Import is aborted
; if a corrupted file is found within the total of selected files.
; ***
; ***
; 2016-02-29: v5.X
; - peak_detection: added a second gaussfit with constant baseline for peak width determination. Should create
; more stable results if the 'linear' baseline setting is used.
; ***
; ***
; 2016-04-15: v5.08
; - read almsco cdf and calc_tic: added check for scans without intensity values.
; ***
; ***
; 2016-05-31: v5.X
; - modification for int_gbl, dbl gbl (l320): ensured that convolution data and kernel dimension fit.
;########################################################################################################################