;------------------------------------------------------------------------------------------------------------------------
;+
; PURPOSE
; Definition of IAU_Chrom global variables.
;
;-
;------------------------------------------------------------------------------------------------------------------------
PRO def_common, v
;+++++++++++++++++++++++
  COMMON DATA, $
         chrom, instr_type, fragdata, error_handler_IO, exp_info, intm, $ ; rtlock
         path, subst, uniq_mass, tot_uniqm, version


  version = v

  fragdata = 0
  exp_info = 0
  intm = { $
            dlnames : ['Baseline_dynamicRT', 'GaussFit', 'Gauss_SGbase', 'GumbleFit','Dbl_Gumble_1stPeak','Dbl_Gumble_2ndPeak', $
                       'Dbl_Gumble_PeakSum', 'Fix_2point_BL', 'SavGol_BL','SavGol_top','SavGol_interpol'] ,$ ;'Gauss_FIX'
          prgmnames : ['bl','gau','gau_SGbase','gbl','dblgbl_1st','dblgbl_2nd', $
                       'dblgbl_sum', 'bl_fix', 'SavGol_bl','SavGol_top','SavGol_ipol']} ; 'gau_fix'

  path = ''
  uniq_mass = 0 ; array with unique masses of a specific chromatogram
  tot_uniqm = 0 ; array with unique masses of all chromatograms in the chrom-structure

  error_handler_IO = 0

  chrom  = create_refd()
  subst  = create_refs()
;  rtlock = create_refrt()

;+++++++++++++++++++++++
  COMMON COM_PLOT, $
         p_obj0, p_obj1, p_obj0_txt, p_obj0_txtdefpos, p_obj1_txt, p_obj1_txtdefpos

;+++++++++++++++++++++++
; plot object arrays
  p_obj0 = OBJARR(10)
  p_obj1 = OBJARR(10)
  p_obj0_txt = OBJARR(10)
  p_obj0_txtdefpos = FLTARR(4,10)
  p_obj1_txt = OBJARR(10)
  p_obj1_txtdefpos = FLTARR(4,10)

;+++++++++++++++++++++++
  COMMON WIDID, widid

  widid = $
         { mainwid         : -1, $ ; main iau_chrom window
           intwid          : -1, $ ; integration window
           mmviewerwid     : -1, $ ; multi mass viewer
           mcviewerwid     : -1, $ ; multi chrom viewer
           plot_ctrls      : -1, $ ; plots controls
           tpshskviewerwid : -1, $ ; tofwerk tps hsk viewer
           mrwid           : -1 $  ; fragrat calc window
            }

;+++++++++++++++++++++++
  COMMON FRAGDATA, fragres, allfragres

         fragres = {}
         allfragres = {}

END
