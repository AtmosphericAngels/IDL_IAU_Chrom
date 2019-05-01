;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; FUNCTION calc_noise
;
; AUTHOR:
; F.Obersteiner / H.Boenisch
;
; INFO:
; function to calculate signal noise relative to a 2-order polyfit. referrs to wid_integration
;
; VARIABLES:
; chrom: loaded chromatographic data (strct)
; sel_chrom: selected file (int)
; sel_name: selected substance (int)
;
; KEYWORDS:
; verbose: triggers print & plot
; insdata_warn: activates warning if insufficient datapoints are used for noise calcualtion
;
; RETURN VALUE:
; noise, 3*SD deviation of signal vs. fit, same dimension as f_x / chrom.intensity
; ndatapoints, number of datapoints used for calculation
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION calc_noise_fct, chrom, sel_chrom, sel_name, tot_uniqm, $
                         NOISE_WIN=noise_win, VERBOSE=verbose, INSDATA_WARN=insdata_warn
  ;+++++++++++++++++++++++++++++
  ; get quantifier ion & mass trace
  quant=chrom[sel_chrom].subst[sel_name].quant
  quantmass=chrom[sel_chrom].subst[sel_name].mass[quant]
  sel_mass=matchmass(tot_uniqm, quantmass)
  ;sel_mass=matchmass(uniq_mass, quantmass)
  IF sel_mass[0] EQ -1 THEN RETURN, 0
  sel_masstrace = sel_mass[0] ;chrom[sel_chrom].subst[sel_name].mass[quant]
  ;+++++++++++++++++++++++++++++
  ; [manual mass trace selection]
  ; sel_masstrace = 500.0

  ;+++++++++++++++++++++++++++++
  ; define noise_win if not defined
  IF NOT KEYWORD_SET(noise_win) THEN noise_win=chrom[sel_chrom].subst[sel_name].noise_win

  ;+++++++++++++++++++++++++++++
  ; define baseline (bl) section and according x/f_x-values
  bl = WHERE(*chrom[sel_chrom].mass EQ sel_masstrace $
             AND *chrom[sel_chrom].time GE noise_win[0] $
             AND *chrom[sel_chrom].time LE noise_win[1], nvd)

  IF nvd LE 3 THEN BEGIN
    strct = {ndatapoints : nvd}
    RETURN, strct ; abort, no datapoints in specified RT window
  ENDIF

  x   = (*chrom[sel_chrom].time)[bl]

  f_x = (*chrom[sel_chrom].intensity)[bl]
  blfit = POLY_FIT(x, f_x, 2, yfit=yfit)
  noise = 3*STDDEV(f_x - yfit)

  strct = {noise : noise, $
           ndatapoints : nvd, $
           xrange : [min(x,/nan), max(x,/nan)], $
           yrange : [min(f_x,/nan)-1.5*noise, max(f_x,/nan)+1.5*noise], $
           x : x, $
           y : f_x, $
           yfit : yfit, $
           noisemass : sel_masstrace}

  IF KEYWORD_SET(insdata_warn) THEN BEGIN
    filename=FILE_BASENAME(chrom[sel_chrom].fname)
    substance=chrom[sel_chrom].subst[sel_name].name
    pre='Warning: Noise calculated over less than 20 datapoints '
    suf='('+substance+' in '+filename+').'
    warn_mess=pre+suf
    IF strct.ndatapoints LT 20 THEN msg=DIALOG_MESSAGE(warn_mess, /INFORMATION)
  END

  ;PRINT, file_basename(chrom[sel_chrom].fname), strct.ndatapoints, strct.noisemass, strct.noise

  IF KEYWORD_SET(verbose) EQ 1 THEN BEGIN
    PRINT, 'Noise: ', strct.noise
    PRINT, 'N_Datapoints: ', strct.ndatapoints
     p0=plot(x, f_x, TITLE='Noise Calculation', XTITLE='t', YTITLE='Intensity', NAME='Signal')
     p1=plot(x, yfit, '2r',/OVERPLOT, NAME='PolyFit')
     p2=plot(x, yfit+(noise/3*2), '2b', /OVERPLOT, NAME='PolyFit+2SD')
     p3=plot(x, yfit-(noise/3*2), '2b', /OVERPLOT)
     p4=plot(x, yfit+noise, '2g', /OVERPLOT, NAME='PolyFit+3SD')
     p5=plot(x, yfit-noise, '2g', /OVERPLOT)
     l0=legend(TARGET=[p0,p1,p2,p4], /DATA, /AUTO_TEXT_COLOR)
  ENDIF

  ;print, strct.noise

  RETURN, strct

END
;------------------------------------------------------------------------------------------------------------------------