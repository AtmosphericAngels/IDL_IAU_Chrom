;------------------------------------------------------------------------------------------------------------------------
;+
; :Info: def_ref_structs.pro holds functions that create reference structures.
;-
;------------------------------------------------------------------------------------------------------------------------
;### DATA ###
FUNCTION create_refd

  refd = { $
    fname:         ''                      ,$
    info_str:      ''                      ,$ ; v5.14: string to hold information that is extractable
    ;        from the loaded file in string format. Example:
    ;        operator, sample name etc.
    jdate:         !VALUES.D_NAN           ,$ ; measurement timestamp
    time:          PTR_NEW(/ALLOCATE_HEAP) ,$
    mass:          PTR_NEW(/ALLOCATE_HEAP) ,$
    intensity:     PTR_NEW(/ALLOCATE_HEAP) ,$
    peaktable:     PTR_NEW(/ALLOCATE_HEAP) ,$ ; 14/02/03: tofwerk hdf5 peaktable
    twtps_hsk:     PTR_NEW(/ALLOCATE_HEAP) ,$ ; 15/09/21: tofwerk tps hsk date

    t_scale:       ''                      ,$ ; v4.42: timescale of x-axis / *chrom.time
    iauchrom_vers: 0.                      ,$ ; v4.44: version of iau-chrom
    instr_type:    0                        $ ; 0: not defined, 1: QP, 2: BenchTOF, 3: HTOF, 4: ECD
  }

  RETURN, refd

END

;### STUBST ###
FUNCTION create_refs
  refs = { name:      '',$                        ; Substanzname
    formula:   '',$                        ; Summenformel der Substanz
    rt:        !VALUES.F_NAN,$             ; Retentionszeit (optional)
    rt_win:    FLTARR(2)*!VALUES.F_NAN,$   ; Fenster, in welchem der Peak angezeigt wird (Integrationsfenster bestimmt durch sigma)
    noise_win: FLTARR(2)*!VALUES.F_NAN,$   ; Fenster, in welchem das Rauschen bestimmt wird (June 2014 Update)
    quant:     0,$                         ; Quantifier fuer Integration
    mass:      FLTARR(10)*!VALUES.F_NAN,$  ; Masse
    rel_abd:   FLTARR(10)*!VALUES.F_NAN,$  ; relative Abundance aus NIST - Bibliothek
    method:    '',$                        ; siehe def_common intm
    int_win:   FLTARR(2)*!VALUES.F_NAN,$   ; Fenster, in welchem die Baseline-Integration stattfindet
    bl_type:   0,$                         ; 0:konstant,1:linear,2:quadratisch,3:exponentiell
    sigma:     FLTARR(2)*!VALUES.F_NAN,$   ; linker und rechter SIGMA-Wert
    sel_peak:  '',$                        ; fuer Doppelgumbel: 1: erster Peak, 2: zweiter Peak, 1+2: erster und zweiter Peak
    dif_peak:  FLTARR(2)*!VALUES.F_NAN,$   ; fuer Doppelgumbel: [Zeitdifferenz nach links , Zeitdifferenz nach rechts]
    svgl:      !VALUES.F_NAN,$             ; Savitzky-Goolay Filterbreite
    thresh:    FLTARR(2)*!VALUES.F_NAN   } ; wenn nicht definiert, dann Integration in fixem Intervall

  RETURN, refs
  
END

;### RT_LOCK ###
FUNCTION create_refrt
  refrt = { active:  0B, $
    name:    'nd', $
    formula: 'nd', $
    mass:    !Values.F_NaN, $
    rt_win:  FLTARR(2)*!VALUES.F_NAN, $
    rt:      !Values.F_NaN }

  RETURN, refrt
  
END

;### INTEGRATION RESULTS ###
FUNCTION create_refi
  refi = { rt:         !VALUES.D_NAN, $
    height:     !VALUES.D_NAN, $
    area:       !VALUES.D_NAN, $
    ts:         !VALUES.D_NAN, $
    te:         !VALUES.D_NAN, $
    width:      !VALUES.D_NAN, $
    flag:       0, $
    comment:    'not integrated', $
    noise:      FLTARR(20)*!Values.F_NAN }

  RETURN, refi
  
END


PRO def_ref_structs
END