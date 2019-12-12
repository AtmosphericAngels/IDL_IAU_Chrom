;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; plot_intres
;
; INFO:
; plot some results from the substance currently selected.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO plot_intres, chrom, SEL_SUBST_IX=sel_subst_ix, SAVEPLOT=saveplot, FILE_EXT=file_ext, RELATIVE=relative

  IF NOT KEYWORD_SET(sel_subst_ix) THEN sel_subst_ix = 0
  IF NOT KEYWORD_SET(saveplot) THEN saveplot = 0
  IF NOT KEYWORD_SET(file_ext) THEN file_ext = '.ps'
  IF NOT KEYWORD_SET(relative) THEN relative = 0

;+++++++++++++++++++++++
; Get data
  nchrom = N_ELEMENTS(chrom.fname)
  areas = DBLARR(nchrom)
  heights = DBLARR(nchrom)
  rets = DBLARR(nchrom)
  a_vs_h = DBLARR(nchrom)
  meas_no = INDGEN(nchrom)+1
  xrange = [0,MAX(meas_no)+1]
  title=chrom[0].subst[sel_subst_ix].name

    FOR i=0, nchrom-1 DO BEGIN
      areas[i] = chrom[i].subst[sel_subst_ix].ires.area
      heights[i] = chrom[i].subst[sel_subst_ix].ires.height
      rets[i] = chrom[i].subst[sel_subst_ix].ires.rt
      a_vs_h[i] = areas[i] / heights[i]
    END

  rel_areas = areas/MEAN(areas, /NAN)
  rel_heights = heights/MEAN(heights, /NAN)
  rel_rets = rets/MEAN(rets, /NAN)

;  rel_a_vs_h = a_vs_h/MEAN(a_vs_h, /NAN)
;  rel_a_vs_h_range = [MIN(rel_a_vs_h, /NAN)-((MAX(rel_a_vs_h, /NAN)-MIN(rel_a_vs_h, /NAN))*0.1), $
;                      MAX(rel_a_vs_h, /NAN)+((MAX(rel_a_vs_h, /NAN)-MIN(rel_a_vs_h, /NAN))*0.1)]

  ;+++++++++++++++++++++++
  ; plot

  IF relative THEN BEGIN
    y0= rel_areas
    y1= rel_rets
    y2= rel_heights
    y0range= [MIN(rel_areas, /NAN)-((MAX(rel_areas, /NAN)-MIN(rel_areas, /NAN))*0.1), $
              MAX(rel_areas, /NAN)+((MAX(rel_areas, /NAN)-MIN(rel_areas, /NAN))*0.1)]
    y1range= [MIN(rel_rets, /NAN)-((MAX(rel_rets, /NAN)-MIN(rel_rets, /NAN))*0.1), $
              MAX(rel_rets, /NAN)+((MAX(rel_rets, /NAN)-MIN(rel_rets, /NAN))*0.1)]
    y2range= [MIN(rel_heights, /NAN)-((MAX(rel_heights, /NAN)-MIN(rel_heights, /NAN))*0.1), $
              MAX(rel_heights, /NAN)+((MAX(rel_heights, /NAN)-MIN(rel_heights, /NAN))*0.1)]
    y0_title= 'Area/Area_Mean'
    y1_title= 'RT/RT_Mean'
    y2_title= 'Height/Height_Mean'
  ENDIF ELSE BEGIN
    y0= areas
    y1= rets
    y2= heights
    y0range= [MIN(areas, /NAN)-((MAX(areas, /NAN)-MIN(areas, /NAN))*0.1), $
              MAX(areas, /NAN)+((MAX(areas, /NAN)-MIN(areas, /NAN))*0.1)]
    y1range= [MIN(rets, /NAN)-((MAX(rets, /NAN)-MIN(rets, /NAN))*0.1), $
              MAX(rets, /NAN)+((MAX(rets, /NAN)-MIN(rets, /NAN))*0.1)]
    y2range= [MIN(heights, /NAN)-((MAX(heights, /NAN)-MIN(heights, /NAN))*0.1), $
              MAX(heights, /NAN)+((MAX(heights, /NAN)-MIN(heights, /NAN))*0.1)]
    y0_title= 'Area'
    y1_title= 'RT'
    y2_title= 'Height'
  ENDELSE

  
  DEVICE, Get_Screen_Size=screenSize
  dim = [screenSize[0]*0.95,screenSize[1]*0.85]
  mar = [0.04,0.15,0.03,0.1]
  p0=plot(meas_no, y0, XRANGE=xrange, YRANGE=y0range, LINESTYLE=6, SYMBOL="td", SYM_SIZE=1.5, $
          SYM_COLOR='r', SYM_THICK=2, NAME='Area', WINDOW_TITLE='Plot Report', LAYOUT=[1,3,1], YTITLE=y0_title, $
          TITLE=title, MARGIN=mar, DIMENSIONS=dim)


  p2=plot(meas_no, y2, XRANGE=xrange, YRANGE=y2range, LINESTYLE=6, SYMBOL="s", SYM_SIZE=1.5, $
          SYM_COLOR='g', SYM_THICK=2, NAME='Height', CURRENT=1, LAYOUT=[1,3,2], $
          YTITLE=y2_title, MARGIN=mar, DIMENSIONS=dim)
          
  p1=plot(meas_no, y1, XRANGE=xrange, YRANGE=y1range, LINESTYLE=6, SYMBOL="s", SYM_SIZE=1.5, $
          SYM_COLOR='b', SYM_THICK=2, NAME='RT', CURRENT=1, LAYOUT=[1,3,3], $
          XTITLE='Measurment No.', YTITLE=y1_title, MARGIN=mar, DIMENSIONS=dim)


  IF saveplot THEN BEGIN
    exp_name = FILE_BASENAME(FILE_DIRNAME(FILE_DIRNAME(FILE_DIRNAME(chrom[0].fname))))
    p0.save, saveplot+'\'+exp_name+'_'+title+file_ext, RESOLUTION=300
    p0.close
  ENDIF
  ;
  ;names = chrom[0].subst.name
  ;nsubst = n_elements(names)
  ;areas = fltarr(nsubst, nchrom)
  ;FOR i=0, nchrom-1 DO BEGIN
  ;  FOR j=0, nsubst-1 DO BEGIN
  ;    areas[j,i] = chrom[i].subst[j].ires.area
  ;  ENDFOR
  ;ENDFOR
  ;
  ;substratio=areas[1,*]/areas[0,*]
  ;p2=plot(meas_no, substratio, XRANGE=xrange)

  ;cal_areas=fltarr(nsubst)
  ;he_areas=fltarr(nsubst,3)
  ;
  ;FOR j=0, nsubst-1 DO BEGIN
  ;  cal_areas[j] = mean(areas[j,0:2])
  ;  he_areas[j,0] = areas[j,3]
  ;  he_areas[j,1] = areas[j,4]
  ;  he_areas[j,2] = areas[j,5]
  ;ENDFOR
  ;
  ;for i=0, nsubst-1 do print, names[i], he_areas[i,0]/cal_areas[i]
;  PRINT, 'halt'
END

; double y axis plot
;
;x=indgen(100)
;y0=indgen(100)
;y1=(indgen(100)-1000)*(-1)
;margin=[0.1,0.1,0.1,0.1]
;p0=plot(x,y0,'r',margin=margin,axis_style=0)
;p1=plot(x,y1,'b',/current, margin=margin, axis_style=0)
;a0=axis('Y',location='left',target=p0,color='r',title='y0')
;a1=axis('Y',location='right',target=p1,color='b',title='y1')
;ax=axis('X',location='bottom',target=p0)
