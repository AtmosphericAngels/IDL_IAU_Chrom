;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; intres_qcklook
;
; AUTHOR: F.Obersteiner, July 2014
;
; INFO:
; process integration results to postscript file (all chromatograms, all substances)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO intres_qcklook, chrom, PATH=path
;+++++++++++++++++++++++
;check chrom
  IF SIZE(chrom, /TYPE) NE 8 THEN RETURN
;+++++++++++++++++++++++
;generate output filename
  IF NOT KEYWORD_SET(path) THEN $
    path=DIALOG_PICKFILE(/DIRECTORY, PATH=path, TITLE="Choose directory to save postscript file.")
    
  IF STRLEN(path) EQ 0 THEN RETURN
  
  cdate = conc_date(chrom[0].jdate, SYSTIME(/julian), cdate1=cdate1)
  psfname = path+cdate+'_intresqcklook.ps'
;+++++++++++++++++++++++
;get sizes
  n_chrom = N_ELEMENTS(chrom.fname)
  n_subst = N_ELEMENTS(chrom[0].subst.name)
  rel_rt = DBLARR(n_subst, n_chrom)*!Values.D_NAN
  rel_area = DBLARR(n_subst, n_chrom)*!Values.D_NAN
  rel_hght = DBLARR(n_subst, n_chrom)*!Values.D_NAN
  rel_noise = DBLARR(n_subst, n_chrom)*!Values.D_NAN
;+++++++++++++++++++++++
;get data
  names = STRCOMPRESS(chrom[0].subst.name, /REMOVE_ALL)
  rt = chrom.subst.ires.rt
  area = chrom.subst.ires.area
  hght = chrom.subst.ires.height
  noise = FLTARR(n_subst,n_chrom)*!Values.F_NAN
  x = INDGEN(n_chrom)+1
  xrange = [0, n_chrom+1]
;+++++++++++++++++++++++
;generate relative data
  FOR sel_subst=0, n_subst-1 DO BEGIN
    rel_rt[sel_subst, *]    = rt[sel_subst, *]    / MEAN(rt[sel_subst, *], /NAN)
    rel_area[sel_subst, *]  = area[sel_subst, *]  / MEAN(area[sel_subst, *], /NAN)
    rel_hght[sel_subst, *]  = hght[sel_subst, *]  / MEAN(hght[sel_subst, *], /NAN)
    noise[sel_subst, *] = (chrom.subst[sel_subst].ires.noise[chrom[0].subst[sel_subst].quant]) ; (!) chrom 0
    rel_noise[sel_subst, *] = noise[sel_subst, *] / MEAN(noise[sel_subst, *], /NAN)
  ENDFOR
;+++++++++++++++++++++++
;loop ps printing
  mydevice = !D.NAME
  SET_PLOT, 'PS'
  DEVICE, FILENAME=psfname, /COLOR, /PORTRAIT, DECOMPOSED=1, XOFFSET=0, YOFFSET=0, XSIZE=21, YSIZE=29.7 ; A4 format [cm], no margin
  
  FOR sel_subst=0, n_subst-1 DO BEGIN
    
    !p.multi = [0, 0, 2, 0, 0] ; first page of sel_subst
    !X.MARGIN=[8,3]
    !Y.MARGIN=[4,2]
    !X.OMARGIN=[3,3]
    !Y.OMARGIN=[3,3]
    symsize=1.2
    
    title = names[sel_subst]
    ytitle = 'rel. RT'
    v = rel_rt[sel_subst, *]
    finite_xv = get_finite_xv(x,v)
    xval=finite_xv.x
    vval=finite_xv.v
    IF xval[1] NE 0 THEN BEGIN
      vrange=[MIN(vval, /NAN)-((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1), MAX(vval, /NAN)+((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1)]
      plotsym, 0, symsize, FILL=1
      PLOT, xval, vval, XRANGE=xrange, YRANGE=yrange, XSTYLE=1, XGRIDSTYLE=1, XTICKLEN=1, TITLE=title, YTITLE=ytitle, /YNOZERO, /NODATA
      PLOTS, xval,vval, PSYM=8, COLOR='FF0000'x
      OPLOT, [0, n_chrom+1], [1,1], LINESTYLE=1
    ENDIF
    
    xtitle = 'Measurement No.'
    ytitle = 'rel. A/H'
    v = rel_area[sel_subst, *] / rel_hght[sel_subst, *]
    finite_xv = get_finite_xv(x,v)
    xval=finite_xv.x
    vval=finite_xv.v
    IF xval[1] NE 0 THEN BEGIN
      vrange=[MIN(vval, /NAN)-((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1), MAX(vval, /NAN)+((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1)]
      plotsym, 4, symsize, FILL=1
      PLOT, xval, vval, XRANGE=xrange, YRANGE=yrange, XSTYLE=1, XGRIDSTYLE=1, XTICKLEN=1, /YNOZERO, XTITLE=xtitle, YTITLE=ytitle, /NODATA
      PLOTS, xval,vval, PSYM=8, COLOR='008000'x
      OPLOT, [0, n_chrom+1], [1,1], LINESTYLE=1
    ENDIF
    
    !p.multi = [0, 0, 3, 0, 0] ; second page of sel_subst
    !X.MARGIN=[18,8]
    !Y.MARGIN=[6,4]
    !X.OMARGIN=[3,3]
    !Y.OMARGIN=[3,3]
    symsize=1.2
    
    title = names[sel_subst]
    ytitle = 'rel. Area'
    v = rel_area[sel_subst, *]
    finite_xv = get_finite_xv(x,v)
    xval=finite_xv.x
    vval=finite_xv.v
    IF xval[1] NE 0 THEN BEGIN
      vrange=[MIN(vval, /NAN)-((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1), MAX(vval, /NAN)+((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1)]
      plotsym, 0, symsize, FILL=1
      PLOT, xval, vval, XRANGE=xrange, YRANGE=yrange, XSTYLE=1, XGRIDSTYLE=1, XTICKLEN=1, /YNOZERO, TITLE=title, YTITLE=ytitle, /NODATA
      PLOTS, xval,vval, PSYM=8, COLOR='FF0000'x
      OPLOT, [0, n_chrom+1], [1,1], LINESTYLE=1
    ENDIF
        
    ytitle = 'rel. Height'
    v = rel_hght[sel_subst, *]
    finite_xv = get_finite_xv(x,v)
    xval=finite_xv.x
    vval=finite_xv.v
    IF xval[1] NE 0 THEN BEGIN
      vrange=[MIN(vval, /NAN)-((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1), MAX(vval, /NAN)+((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1)]
      plotsym, 4, symsize, FILL=1
      PLOT, xval, vval, XRANGE=xrange, YRANGE=yrange, XSTYLE=1, XGRIDSTYLE=1, XTICKLEN=1, /YNOZERO, YTITLE=ytitle, /NODATA
      PLOTS, xval,vval, PSYM=8, COLOR='008000'x
      OPLOT, [0, n_chrom+1], [1,1], LINESTYLE=1
    ENDIF
    
    xtitle = 'Measurement No.'
    ytitle = 'rel. Noise'
    v = rel_noise[sel_subst, *]
    finite_xv = get_finite_xv(x,v)
    xval=finite_xv.x
    vval=finite_xv.v
    IF xval[1] NE 0 THEN BEGIN
      vrange=[MIN(vval, /NAN)-((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1), MAX(vval, /NAN)+((MAX(vval, /NAN)-MIN(vval, /NAN))*0.1)]
      plotsym, 8, symsize, FILL=1
      PLOT, xval, vval, XRANGE=xrange, YRANGE=yrange, XSTYLE=1, XGRIDSTYLE=1, XTICKLEN=1, /YNOZERO, XTITLE=xtitle, YTITLE=ytitle, /NODATA
      PLOTS, xval,vval, PSYM=8, COLOR='0000FF'x
      OPLOT, [0, n_chrom+1], [1,1], LINESTYLE=1
    ENDIF
     
  ENDFOR
  
  !X.MARGIN=[10,3] ; reset system variables (plot)
  !Y.MARGIN=[4,2]
  !X.OMARGIN=[0,0]
  !Y.OMARGIN=[0,0]
  !P.MULTI = [0, 0, 1, 0, 0]
  
  DEVICE, /CLOSE, DECOMPOSED=1
  SET_PLOT, mydevice
  
END