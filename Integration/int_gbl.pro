;-------------------------------------------------------------------;
; User defined IDL procedures and functions used in this programm	  ;
;-------------------------------------------------------------------;
@peak_detection
;------------------------------------------------------------------------------------------------------------------------
;+
; NAME:
;	int_gbl_v2.pro
;
; MODIFICATION HISTORY:
;	H. Boenisch, IAU Frankfurt, Aug 02, 2012.
;	F. Obersteiner, IAU Frankfurt, Nov 2013.
;-
;------------------------------------------------------------------------------------------------------------------------
;
;
;************************************************************************************************************************
PRO GumbelPeak_PROC, X, A, F, PDER

	ON_ERROR,2	;Return to caller if an error occurs

	;-----------------------------------------------
	; GUMBEL DISTRIBUTION
	; Density Function:
	;    f(x) = a0*a1*exp{-a1*(x-a2)}*exp{-exp{-a1*(x-a2)}}
	;-----------------------------------------------
	GUM=A[0]*A[1]*EXP(-A[1]*(X-A[2]))*EXP(-EXP(-A[1]*(X-A[2])))

	CASE N_ELEMENTS(A) OF	;FUNCTIONS
		3: F=GUM
		4: F=GUM+A[3]
		5: F=GUM+A[3]+A[4]*X
		6: F=GUM+A[3]+A[4]*X+A[5]*X^2.
	ENDCASE

	IF (N_PARAMS() LE 3) THEN RETURN	;NEED PARTIAL?

END
;************************************************************************************************************************
;
;
;************************************************************************************************************************
PRO GumbelDoublePeak_PROC, X, A, F, PDER

	ON_ERROR, 2	;Return to caller if an error occurs

	IF (A[0] NE 0.) THEN GUM1=A[0]*A[1]*EXP(-A[1]*(X-A[2]))*EXP(-EXP(-A[1]*(X-A[2]))) ELSE GUM1=0.
	IF (A[3] NE 0.) THEN GUM2=A[3]*A[4]*EXP(-A[4]*(X-A[5]))*EXP(-EXP(-A[4]*(X-A[5]))) ELSE GUM2=0.

	GUM=GUM1+GUM2	;GAUSSIAN DOUBLE PEAK

	CASE N_ELEMENTS(A) OF	;FUNCTIONS
		6: F=GUM
		7: F=GUM+A[6]
		8: F=GUM+A[6]+A[7]*X
		9: F=GUM+A[6]+A[7]*X+A[8]*X^2.
	ENDCASE

	IF (N_PARAMS() LE 3) THEN RETURN	;NEED PARTIAL?

END
;************************************************************************************************************************
;
;
;************************************************************************************************************************
FUNCTION Integrate_GumbelPeak, xval, yval $
                             , NTERMS_BASE=nterms_base, NSIGMA_FIT=nsigma_fit, NSIGMA_INT=nsigma_int $
                             , RT_WIN=rt_win, PEAK_RET=peak_ret, BASE_RET=base_ret  $
                             , FIT_WIN=fit_win, PEAK_FIT=peak_fit, BASE_FIT=base_fit  $
                             , INT_WIN=int_win, PEAK_INT=peak_int, BASE_INT=base_int  $
                             , PARAMETER=parameter, TIMESCALE=timescale, VERBOSE=verbose

  IF NOT keyword_set(NTERMS_BASE) THEN  nterms_base=1
  IF NOT keyword_set(NSIGMA_FIT) THEN nsigma_fit=[10,20]
  IF NOT keyword_set(NSIGMA_INT) THEN nsigma_int=[10,20]
  IF NOT keyword_set(timescale) THEN timescale='Minutes' ; not set case: restoring old chromatogram

;+++++++++++++++++++++++
; Create output structure (strct) for chromatographic parameters
;+++++++++++++++++++++++
   strct=$
      {$
      ret: !values.d_nan, $
      hght: !values.d_nan, $
      area: !values.d_nan, $
      wdth: !values.d_nan, $
      ts: !values.d_nan, $
      te: !values.d_nan, $
      flag: 0 ,$
      comment:'Not Integrated'$
      }

  vd=where(finite(xval+yval),nvd)
  IF (nvd LE 0) THEN RETURN, strct
  x=xval[vd]
  y=yval[vd]

;+++++++++++++++++++++++
; Define retention time window (rt_win)
;+++++++++++++++++++++++
  w_rt_win=where((x GE rt_win[0]) AND (x LE rt_win[1]), nw_rt_win)

;+++++++++++++++++++++++
; Detect Gauss Peak inside retention time window (rt_win)
;+++++++++++++++++++++++
  t=x[w_rt_win]
  v=y[w_rt_win]
  A_gau=peak_detection(t, v, RT_WIN=rt_win, NTERMS_BASE=nterms_base)

;+++++++++++++++++++++++
; Define window for peak fitting (FIT_WIN)
;+++++++++++++++++++++++
  fit_win=[A_gau[1]-nsigma_fit[0]*A_gau[2],A_gau[1]+nsigma_fit[1]*A_gau[2]]
  w_fit_win=where((x GE fit_win[0]) AND (x LE fit_win[1]),nw_fit_win)

;+++++++++++++++++++++++
; Define window for peak integration (INT_WIN)
;+++++++++++++++++++++++
  int_win=[A_gau[1]-nsigma_int[0]*A_gau[2],A_gau[1]+nsigma_int[1]*A_gau[2]]
  w_int_win=where((x GE int_win[0]) AND (x LE int_win[1]),nw_int_win)

;+++++++++++++++++++++++
; Define initial parameters for peak fitting
;+++++++++++++++++++++++
  A=FLTARR(3+nterms_base)
  A[0]=A_gau[0]
  
  IF timescale EQ 'Minutes' THEN init_parm_A1 = 60 ELSE init_parm_A1 = 1 ; 60: minutes / 1: seconds
  A[1]=init_parm_A1 
  
  A[2]=A_gau[1]
  A[3:*]=A_gau[3:*]
  fita=bytarr(n_elements(A))+1
  
;+++++++++++++++++++++++
; Fit peak
;+++++++++++++++++++++++
  t=x[w_fit_win]
  v=y[w_fit_win]

  IF n_elements(A) GE n_elements(v) THEN RETURN, strct
  
  fit=CURVEFIT(t,v,weights,A,Sig,CHISQ=chi,FITA=fita,FUNCTION_NAME='GumbelPeak_PROC',$
               ITER=iter,ITMAX=20,NODERIVATIVE=1,STATUS=status,TOL=10e-3,YERROR=yerr)
            
;+++++++++++++++++++++++
; Output peak parameters of curvefit (PARAMETER)
;+++++++++++++++++++++++
  parameter=A

;+++++++++++++++++++++++
; Calculate fitted peak (PEAK_FIT) and baseline (BASE_FIT)
;+++++++++++++++++++++++
  t=x[w_fit_win]
  v=y[w_fit_win]
  peak_fit=A[0]*A[1]*EXP(-A[1]*(t-A[2]))*EXP(-EXP(-A[1]*(t-A[2])))
  CASE nterms_base OF
    1: base_fit=A[3]+replicate(0,nw_fit_win)
    2: base_fit=A[3]+A[4]*t
    3: base_fit=A[3]+A[4]*t+A[5]*t^2
  ENDCASE

;+++++++++++++++++++++++
; Calculate integrated peak (PEAK_INT) and baseline (BASE_INT)
;+++++++++++++++++++++++
  t=x[w_int_win]
  v=y[w_int_win]
  peak_int=A[0]*A[1]*EXP(-A[1]*(t-A[2]))*EXP(-EXP(-A[1]*(t-A[2])))
  CASE nterms_base OF
    1: base_int=A[3]+replicate(0,nw_int_win)
    2: base_int=A[3]+A[4]*t
    3: base_int=A[3]+A[4]*t+A[5]*t^2
  ENDCASE

;+++++++++++++++++++++++
; Calculate peak inside retention time window (PEAK_RET) and baseline (BASE_RET)
;+++++++++++++++++++++++
  t=x[w_rt_win]
  v=y[w_rt_win]
  peak_ret=A[0]*A[1]*EXP(-A[1]*(t-A[2]))*EXP(-EXP(-A[1]*(t-A[2])))
  CASE nterms_base OF
    1: base_ret=A[3]+replicate(0,nw_rt_win)
    2: base_ret=A[3]+A[4]*t
    3: base_ret=A[3]+A[4]*t+A[5]*t^2
  ENDCASE

;+++++++++++++++++++++++
; Calculate chromatographic parameters (peak area, height and retention time)
;+++++++++++++++++++++++
  t=x[w_int_win]
  v=y[w_int_win]
  strct.ret=A[2]
  strct.hght=max(peak_int,wmax)
  strct.area=int_tabulated(t,peak_int,/double)   ; is equal to parameter A[0]
  strct.wdth=1/A[1]   ; is not symmetric but represents the left non-tailing side of the peak
  strct.ts=min(t,/nan)
  strct.te=max(t,/nan)
  strct.flag=1
  strct.comment='Integrated'

  IF verbose THEN BEGIN
    print,'GUMBEL FIT PRAMS:'
    print,A_gau
    print,A
    print,status,iter,chi,yerr
    print,strct.hght
    print,strct.area,1/A[1],t[wmax]
    t=x[w_rt_win]
    v=y[w_rt_win]
    yrange=[min([v,base_ret,base_fit,base_int]),max([v,base_ret,base_fit,base_int])]
    plot,t,v,yrange=yrange,ystyle=3
    t=x[w_fit_win]
    v=y[w_fit_win]
    oplot,t,base_fit,linestyle=0,thick=2
    oplot,t,peak_fit+base_fit,linestyle=0,thick=2
    t=x[w_int_win]
    v=y[w_int_win]
    oplot,t,base_int,linestyle=2,thick=1
    oplot,t,peak_int+base_int,linestyle=2,thick=1
  ENDIF

;  xrange=[min([t,rt_win[0]]),max([t,rt_win[1]])]
;  yrange =[min([v[vd], bl, peak])- 0.1* max([v[vd], bl, peak]), max([v[vd], bl, peak]*1.1)]
;  yrange=[min([v,base_ret,base_fit,base_int]),max([v,base_ret,base_fit,base_int])]

;  plot_routine,t,v,OVER=0, YRANGE=yrange ,XRANGE=xrange
;  plot_routine,t,peak_int+base_int,OVER=1, XRANGE=xrange
;  plot_routine,t,base_int,OVER=2, XRANGE=xrange

  RETURN, strct
END
;
;
;************************************************************************************************************************
FUNCTION Integrate_GumbelDoublePeak, xval, yval $
                              , NTERMS_BASE=nterms_base, NSIGMA_FIT=nsigma_fit, NSIGMA_INT=nsigma_int $
                              , RT_WIN=rt_win, PEAK_RET1=peak_ret1, PEAK_RET2=peak_ret2, BASE_RET=base_ret  $
                              , FIT_WIN=fit_win, PEAK_FIT1=peak_fit1, PEAK_FIT2=peak_fit2, BASE_FIT=base_fit  $
                              , INT_WIN=int_win, PEAK_INT1=peak_int1, PEAK_INT2=peak_int2, BASE_INT=base_int  $
                              , PARAMETER=parameter, TIMESCALE=timescale, VERBOSE=verbose

	 IF NOT keyword_set(NTERMS_BASE) THEN nterms_base=1
   IF NOT keyword_set(NSIGMA_FIT) THEN nsigma_fit=[10,20]
   IF NOT keyword_set(NSIGMA_INT) THEN nsigma_int=[10,20]
   IF NOT keyword_set(rt_win) THEN rt_win =[min(xval,/nan),max(xval,/nan)]
   
;   xval=xval*60D
;   RT_WIN=rt_win*60D

   nsigma_fit_gum1=fltarr(2)
   nsigma_fit_gum2=nsigma_fit
  
   
   ;+++++++++++++++++++++++
   ; Create output structure (strct) for chromatographic parameters
   ;+++++++++++++++++++++++
   strct=$
      {$
      ret1: !values.d_nan, $
      ret2: !values.d_nan, $
      hght1: !values.d_nan, $
      hght2: !values.d_nan, $
      area1: !values.d_nan, $
      area2: !values.d_nan, $
      wdth1: !values.d_nan, $
      wdth2: !values.d_nan, $
      ts: !values.d_nan, $
      te: !values.d_nan, $
      flag:0,$
      comment:'Not Integrated'$
      }

   vd=where(finite(xval+yval),nvd)
  IF (nvd LE 0) THEN RETURN, strct
   x=xval[vd]
   y=yval[vd]

   ;+++++++++++++++++++++++
   ; Define retention time window (rt_win)
   ;+++++++++++++++++++++++
   w_rt_win=where((x GE rt_win[0]) AND (x LE rt_win[1]),nw_rt_win,complement=wc_rt_win,ncomplement=nwc_rt_win)

   ;+++++++++++++++++++++++
   ; Detect Gauss Peak inside retention time window (rt_win)
   ;+++++++++++++++++++++++
   t=x[w_rt_win]
   v=y[w_rt_win]
   A_gau=peak_detection(t,v,RT_WIN=rt_win,NTERMS_BASE=nterms_base,PEAK=peak_gau,BASE=base_gau)

  IF finite(a_gau[0]) EQ 0 THEN return, strct

   ;+++++++++++++++++++++++
   ; Define window for peak fitting (FIT_WIN)
   ;+++++++++++++++++++++++
   fit_win=[A_gau[1]-nsigma_fit[0]*A_gau[2],A_gau[1]+nsigma_fit[1]*A_gau[2]]
   w_fit_win=where((x GE fit_win[0]) AND (x LE fit_win[1]),nw_fit_win)

   ;+++++++++++++++++++++++
   ; Define window for peak integration (INT_WIN)
   ;+++++++++++++++++++++++
   int_win=[A_gau[1]-nsigma_int[0]*A_gau[2],A_gau[1]+nsigma_int[1]*A_gau[2]]
   w_int_win=where((x GE int_win[0]) AND (x LE int_win[1]),nw_int_win)
   
	 ;+++++++++++++++++++++++
   ; Calculate smoothed (Savitzky-Goolay-Filter) chromatogram (dvdt0) and its second derivative (dvdt2)
   ;+++++++++++++++++++++++
   t=x[w_rt_win]
   v=y[w_rt_win]
	 dt=float(max(t)-min(t))/n_elements(t)
   nsigma_svgf=1.
   width=round(nsigma_svgf*A_gau[2]/dt)
   degree=4
   ;+++++++++++++++++++++++
   ; skip and return strct if width LT degree
   ;+++++++++++++++++++++++  
   IF width LT degree THEN RETURN, strct
       
   order=0
   svgf=savgol(width,width,order,degree)*(factorial(order)/(dt^order))
   IF N_ELEMENTS(v) LE N_ELEMENTS(svgf) THEN RETURN, strct ; ensure that convol data is compatible with kernel
   dvdt0=convol(v,svgf,/edge_trunc)
   
   order=2
   svgf=savgol(width,width,order,degree)*(factorial(order)/(dt^order))
   dvdt2=convol(v,svgf,/edge_trunc)

   ;+++++++++++++++++++++++
   ; Find maximum (vmax(tmax)) inside rt_win
   ;+++++++++++++++++++++++
   t=x[w_rt_win]
   v=y[w_rt_win]
   vmax=max(v,wvmax)
   
   tmax=t[wvmax]

   ;+++++++++++++++++++++++
   ; Calculate fit range for the first Gumbel peak (nsigma_fit_gum1)
   ; A_gau[1] = tmax_gau > tmax (bigger peak is on the left hand side)
   ;+++++++++++++++++++++++
   t=x[w_rt_win]
   v=y[w_rt_win]

  ; IF (A_gau[1] GT tmax) THEN BEGIN
      w=where(t GE tmax)
      vmax_dvdt2=max(dvdt2[w],wvmax)
      imax_dvdt2=wvmax+min(w)
      tmax_dvdt2=t[imax_dvdt2]
      nsigma_fit_gum1[0]=nsigma_fit[0]
      nsigma_fit_gum1[1]=(tmax_dvdt2-A_gau[1])/A_gau[2]
  ; ENDIF ELSE BEGIN
  ;  print, 'A_gau[1] LT tmax'
  ;  return, strct
  ; ENDELSE
   ;+++++++++++++++++++++++
   ; Integrate the first detected peak
   ;+++++++++++++++++++++++
   t=x
   v=y

   ires_gum1=Integrate_GumbelPeak(t,v $
               ,NTERMS_BASE=nterms_base,NSIGMA_FIT=nsigma_fit_gum1,NSIGMA_INT=nsigma_int_gum1 $
               ,RT_WIN=rt_win,PEAK_RET=peak_ret_gum1,BASE_RET=base_ret_gum1  $
               ,FIT_WIN=fit_win_gum1,PEAK_FIT=peak_fit_gum1,BASE_FIT=base_fit_gum1 $
               ,INT_WIN=int_win_gum1,PEAK_INT=peak_int_gum1,BASE_INT=base_int_gum1 $
               ,PARAMETER=A_gum1,TIMESCALE=timescale,VERBOSE=0)

   
   ;+++++++++++++++++++++++
   ; check if 1st gbl integration failed, if yes then return
   ;+++++++++++++++++++++++
   IF FINITE(ires_gum1.area) EQ 0 THEN RETURN, strct 
   IF ires_gum1.comment EQ 'Not Integrated' THEN RETURN, strct

   ;+++++++++++++++++++++++
   ; Define window for peak fitting of Gumbel_1 (FIT_WIN_GUM1)
   ;+++++++++++++++++++++++
   w_fit_win_gum1=where((x GE fit_win_gum1[0]) AND (x LE fit_win_gum1[1]),nw_fit_win_gum1,complement=wc_fit_win_gum1,ncomplement=nwc_fit_win_gum1)

   ;+++++++++++++++++++++++
   ; Define window for peak fitting of Gumbel_1 (INT_WIN_GUM1)
   ;+++++++++++++++++++++++
   w_int_win_gum1=where((x GE int_win_gum1[0]) AND (x LE int_win_gum1[1]),nw_int_win_gum1,complement=wc_int_win_gum1,ncomplement=nwc_int_win_gum1)

   ;+++++++++++++++++++++++
   ; Integrate the second detected peak
   ;+++++++++++++++++++++++
   t=x
   v=y
   v[w_rt_win]=y[w_rt_win]-peak_ret_gum1

   ires_gum2=Integrate_GumbelPeak(t,v $
               ,NTERMS_BASE=nterms_base,NSIGMA_FIT=nsigma_fit_gum2,NSIGMA_INT=nsigma_int_gum2 $
               ,RT_WIN=rt_win,PEAK_RET=peak_ret_gum2,BASE_RET=base_ret_gum2  $
               ,FIT_WIN=fit_win_gum2,PEAK_FIT=peak_fit_gum2,BASE_FIT=base_fit_gum2 $
               ,INT_WIN=int_win_gum2,PEAK_INT=peak_int_gum2,BASE_INT=base_int_gum2 $
               ,PARAMETER=A_gum2,TIMESCALE=timescale,VERBOSE=verbose)

   ;+++++++++++++++++++++++
   ; check if 2nd gbl integration, if yes then return
   ;+++++++++++++++++++++++
   IF FINITE(ires_gum1.area) EQ 0 THEN RETURN, strct
   IF ires_gum2.comment EQ 'Not Integrated' THEN RETURN, strct
   
   ;+++++++++++++++++++++++
   ; Define window for peak fitting of Gumbel_2 (FIT_WIN_GUM2)
   ;+++++++++++++++++++++++
   w_fit_win_gum2=where((x GE fit_win_gum2[0]) AND (x LE fit_win_gum2[1]),nw_fit_win_gum2,complement=wc_fit_win_gum2,ncomplement=nwc_fit_win_gum2)

   ;+++++++++++++++++++++++
   ; Define window for peak fitting of Gumbel_2 (INT_WIN_GUM2)
   ;+++++++++++++++++++++++
   w_int_win_gum2=where((x GE int_win_gum2[0]) AND (x LE int_win_gum2[1]),nw_int_win_gum2,complement=wc_int_win_gum2,ncomplement=nwc_int_win_gum2)

   ;+++++++++++++++++++++++
   ; Define initial parameters for peak fitting
   ;+++++++++++++++++++++++
   A=FLTARR(6+nterms_base)
   A[0]=A_gum1[0]
   A[1]=A_gum1[1]
   A[2]=A_gum1[2]
   A[3]=A_gum2[0]
   A[4]=A_gum2[1]
   A[5]=A_gum2[2]
   A[6:*]=A_gum1[3:*]
   fita=bytarr(n_elements(A))+1

   ;+++++++++++++++++++++++
   ; Fit peak
   ;+++++++++++++++++++++++
   t=x[w_fit_win]
   v=y[w_fit_win]
   fit=CURVEFIT(t,v,weights,A,Sig,CHISQ=chi,FITA=fita,FUNCTION_NAME='GumbelDoublePeak_PROC',$
                  ITER=iter,ITMAX=20,NODERIVATIVE=1,STATUS=status,TOL=10e-3,YERROR=yerr)

   ;+++++++++++++++++++++++
   ; Output fitted peak parameters (PARAMETER)
   ;+++++++++++++++++++++++
   parameter=A

   ;+++++++++++++++++++++++
   ; Calculate fitted peak (PEAK_FIT) and baseline (BASE_FIT)
   ;+++++++++++++++++++++++
   t=x[w_fit_win]
   v=y[w_fit_win]
   peak_fit1=A[0]*A[1]*EXP(-A[1]*(t-A[2]))*EXP(-EXP(-A[1]*(t-A[2])))
   peak_fit2=A[3]*A[4]*EXP(-A[4]*(t-A[5]))*EXP(-EXP(-A[4]*(t-A[5])))
   CASE nterms_base OF
      1: base_fit=A[6]+replicate(0,nw_fit_win)
      2: base_fit=A[6]+A[7]*t
      3: base_fit=A[6]+A[7]*t+A[8]*t^2
   ENDCASE

   ;+++++++++++++++++++++++
   ; Calculate integrated peak (PEAK_INT) and baseline (BASE_INT)
   ;+++++++++++++++++++++++
   t=x[w_int_win]
   v=y[w_int_win]
   peak_int1=A[0]*A[1]*EXP(-A[1]*(t-A[2]))*EXP(-EXP(-A[1]*(t-A[2])))
   peak_int2=A[3]*A[4]*EXP(-A[4]*(t-A[5]))*EXP(-EXP(-A[4]*(t-A[5])))
   CASE nterms_base OF
      1: base_int=A[6]+replicate(0,nw_int_win)
      2: base_int=A[6]+A[7]*t
      3: base_int=A[6]+A[7]*t+A[8]*t^2
   ENDCASE

   ;+++++++++++++++++++++++
   ; Calculate peak inside retention time window (PEAK_RET) and baseline (BASE_RET)
   ;+++++++++++++++++++++++
   t=x[w_rt_win]
   v=y[w_rt_win]
   peak_ret1=A[0]*A[1]*EXP(-A[1]*(t-A[2]))*EXP(-EXP(-A[1]*(t-A[2])))
   peak_ret2=A[3]*A[4]*EXP(-A[4]*(t-A[5]))*EXP(-EXP(-A[4]*(t-A[5])))
   CASE nterms_base OF
      1: base_ret=A[6]+replicate(0,nw_int_win)
      2: base_ret=A[6]+A[7]*t
      3: base_ret=A[6]+A[7]*t+A[8]*t^2
   ENDCASE

   ;+++++++++++++++++++++++
   ; Calculate chromatographic parameters (peak area, height and retention time)
   ;+++++++++++++++++++++++
   t=x[w_int_win]
   v=y[w_int_win]
   strct.ret1=A[2]
   strct.ret2=A[5]
   strct.hght1=max(peak_int1,wmax1)
   strct.hght2=max(peak_int2,wmax2)
   strct.area1=int_tabulated(t,peak_int1,/double)   ; is equal to parameter A[0]
   strct.area2=int_tabulated(t,peak_int2,/double)   ; is equal to parameter A[3]
   strct.wdth1=1./A[1]   ; is not symmetric but represents the left non-tailing side of the peak
   strct.wdth2=1./A[4]   ; is not symmetric but represents the left non-tailing side of the peak
   strct.ts=min(t,/nan)
   strct.te=max(t,/nan)
   strct.flag=1
   strct.comment='Integrated'
   
   ;+++++++++++++++++++++++
   ; Post-Processing: remove NAN and substitute with 0. if values are supplied by first gumble integration
   ;+++++++++++++++++++++++
   IF strct.hght1 GT 0 AND FINITE(strct.hght2) EQ 0 THEN strct.hght2 = 0.
   IF strct.area1 GT 0 AND FINITE(strct.area2) EQ 0 THEN strct.area2 = 0.
 
   
   
   IF keyword_set(verbose) THEN BEGIN
      print,'Double Gumbel:'
      print,vmax,tmax
      print,A_gau
      print,A_gum1
      print,vmax_dvdt2,tmax_dvdt2,nsigma_fit_gum1
      print,''

      t=x[w_rt_win]
      v=y[w_rt_win]
      yrange=[min([v,peak_gau,base_gau]),max([v,peak_gau,base_gau])]
      plot,t,v,yrange=yrange,ystyle=3
      oplot,t,base_gau,linestyle=0,thick=2
      oplot,t,peak_gau+base_gau,linestyle=0,thick=2
      yrange=[min([yrange,dvdt2]),max([yrange,dvdt2])]
      oplot,t,dvdt0,linestyle=0,thick=1
      oplot,t,dvdt2,linestyle=0,thick=1

      t=x[w_fit_win_gum1]
      v=y[w_fit_win_gum1]
      oplot,t,base_fit_gum1,linestyle=0,thick=2
      oplot,t,peak_fit_gum1+base_fit_gum1,linestyle=0,thick=2

      t=x[w_int_win_gum1]
      v=y[w_int_win_gum1]
      oplot,t,base_int_gum1,linestyle=2,thick=1
      oplot,t,peak_int_gum1+base_int_gum1,linestyle=2,thick=1

      t=x[w_rt_win]
      v=y[w_rt_win]
      oplot,t,base_ret_gum1,linestyle=2,thick=2
      oplot,t,peak_ret_gum1+base_ret_gum1,linestyle=2,thick=2

      t=x[w_rt_win]
      v=y[w_rt_win]-peak_ret_gum1
      oplot,t,v,linestyle=0,thick=1

      t=x[w_fit_win_gum2]
      v=y[w_fit_win_gum2]
      oplot,t,base_fit_gum2,linestyle=0,thick=2
      oplot,t,peak_fit_gum2+base_fit_gum2,linestyle=0,thick=2

      t=x[w_int_win_gum2]
      v=y[w_int_win_gum2]
      oplot,t,base_int_gum2,linestyle=2,thick=1
      oplot,t,peak_int_gum2+base_int_gum2,linestyle=2,thick=1

      t=x[w_fit_win]
      v=y[w_fit_win]
      oplot,t,base_fit,linestyle=2,thick=1
      oplot,t,peak_fit1+base_fit,linestyle=2,thick=1
      oplot,t,peak_fit2+base_fit,linestyle=2,thick=1
      oplot,t,peak_fit1+peak_fit2+base_fit,linestyle=0,thick=2
   ENDIF

   RETURN, strct

END
;************************************************************************************************************************
;
;
;************************************************************************************************************************
;
PRO int_gbl

;IF 1 THEN BEGIN
;;+++++++++++++++++++++++;
;; Test program			;
;;+++++++++++++++++++++++;
;;fname_cdf=DIALOG_PICKFILE(/READ,TITLE='Select NetCDF file (*.cdf) to load',FILTER='*.cdf',PATH='D:\Uni\Messdaten\Labor_GCMS\NCI_Peaktrennung')
;fname_cdf='D:\Uni\Messdaten\GhOST_MS\201211\20121126\20121126.AIA\20121126_002.CDF'
;cdf=cdf2idl_struct(fname_cdf,verbose=1)
;
;t_scan=cdf.scan_acquisition_time
;i_scan=cdf.scan_index
;n_scan=cdf.point_count
;
;t=cdf.time_values
;FOR i=0,n_elements(t_scan)-1 DO t[i_scan[i]:i_scan[i]+n_scan[i]-1]=t_scan[i]
;
;chr={	$
;	t: t, $
;	v: cdf.intensity_values, $
;	m: cdf.mass_values $
;	}
;mass=chr.m[UNIQ(chr.m,SORT(chr.m))]
;
;;Labor GC-MS
;;rt_win=[200,280]    ;H1301
;;rt_win=[350,480]    ;H1211
;;rt_win=[430,500]    ;CH3Br
;;rt_win=[480,580]    ;H1202
;;rt_win=[580,700]    ;H2402+CH2BrCl
;;rt_win=[730,810]    ;CH2Br2+CHBrCl2
;rt_win=[1680,1980]    ;CH2Br2+CHBrCl2
;rt_win=[2.2299,2.3299]    ;CH2Br2+CHBrCl2
;
;;rt_win=[800,900]    ;CHBr2Cl
;;rt_win=[900,1000]   ;CHBr3
;
;;GhOST GC-MS
;
;w=where(chr.m EQ mass[2])
;;w=where(chr.m EQ mass[2] AND chr.t GT rt_win[0] and chr.t LT rt_win[1],nw)
;
;;iplot,chr.t[w],chr.v[w]
;nterms_base=1
;nsigma_fit=[4,10]
;
;IF 0 THEN BEGIN
;a=Integrate_GumbelPeak(chr.t[w]/60D,chr.v[w] $
;            ,NTERMS_BASE=nterms_base,NSIGMA_FIT=nsigma_fit,NSIGMA_INT=nsigma_int $
;            ,RT_WIN=rt_win,PEAK_RET=peak_ret,BASE_RET=base_ret  $
;            ,FIT_WIN=fit_win,PEAK_FIT=peak_fit,BASE_FIT=base_fit  $
;            ,INT_WIN=int_win,PEAK_INT=peak_int,BASE_INT=base_int  $
;            ,PARAMETER=A,VERBOSE=1)
;ENDIF ELSE BEGIN
;b=Integrate_GumbelDoublePeak((chr.t[w]/60D),chr.v[w] $
;                    ,NTERMS_BASE=nterms_base,NSIGMA_FIT=nsigma_fit,NSIGMA_INT=nsigma_int $
;                    ,RT_WIN=rt_win,PEAK_RET1=peak_ret1,PEAK_RET2=peak_ret2,BASE_RET=base_ret  $
;                    ,FIT_WIN=fit_win,PEAK_FIT1=peak_fit1,PEAK_FIT2=peak_fit2,BASE_FIT=base_fit  $
;                    ,INT_WIN=int_win,PEAK_INT1=peak_int1,PEAK_INT2=peak_int2,BASE_INT=base_int  $
;                    ,PARAMETER=A,VERBOSE=1)
;ENDELSE
;                    
;;print, file_basename(fname_cdf),b.area1, b.area2
;
;
;
;;PRINT, 'Job is done !!!'
;
;;delvar,i,t
;ENDIF

END
;************************************************************************************************************************