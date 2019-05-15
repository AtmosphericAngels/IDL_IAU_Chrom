;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; 17-05, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; run a database script to integrate multiple experiments automatically.
;
;-
;------------------------------------------------------------------------------------------------------------------------
@rw_db_reftable
;------------------------------------------------------------------------------------------------------------------------
PRO call_iauchrom_dbscript, event, T_SCALE=t_scale, CALCNOISE=calcnoise, TW_RECALC_MQ=tw_recalc_mq, $
                            SORT_BY_JDATE=sort_by_jdate

  COMMON DATA

  IF NOT KEYWORD_SET(t_scale) THEN t_scale = 'Seconds'
  IF NOT KEYWORD_SET(calcnoise) THEN calcnoise = 0
  IF NOT KEYWORD_SET(tw_recalc_mq) THEN tw_recalc_mq = 0

  ; load database table
  db_info = read_iauchromdbfile(FILE=db_file, PATH=path)
  IF db_info EQ !NULL THEN RETURN

  ; run db checker
  refr_status, message='db script: checking...'
  db_check = iauchrom_db_chk(db_info, N_TESTED=n_tested, /LOUD)

  IF n_tested EQ db_check THEN BEGIN
    ; get number of experiments + active experiments from info structure
    vd_exp = WHERE(db_info.data.active EQ 1, n_exp)

    IF n_exp EQ 0 THEN BEGIN
      refr_status, message='db script: no active entry. Returning.'
      RETURN
    ENDIF

      FOR n=0, n_exp-1 DO BEGIN ; begin loop: N EXPERIMENTS
        ; free previous structures
        IF chrom NE !NULL THEN HEAP_FREE, chrom
        IF subst NE !NULL THEN HEAP_FREE, subst
        HEAP_GC

        current_exp = ' ('+STRCOMPRESS(STRING(n+1), /REMOVE_ALL)+' of '+STRCOMPRESS(STRING(n_exp), /REMOVE_ALL)+')'

        ; check number of paths per experiment (case of data in multiple folders)
        folder=db_info.data[vd_exp[n]].exp_data_dir
        IF folder.contains(',') THEN folder=strsplit(folder, ',', /EXTRACT)

        ; generate the fname vector that contains paths of all files
        CASE db_info.data[vd_exp[n]].data_import_fct OF
          'aes_cdf': filter = '*.nc'
          'agilent_cdf': filter = '*.cdf'
          'almsco_cdf': filter = '*.cdf'
          'ecd_fid_cdf': filter = '*.nc'
          'ecd_txt':  filter = '*.txt'
          'tofwerk_h5': filter = '*.h5'
        ENDCASE

        fname=[]
        FOR i=0, N_ELEMENTS(folder)-1 DO BEGIN
          tmp=FILE_SEARCH(folder[i], filter, COUNT=count)
          fname=[fname,tmp]
        ENDFOR

        ; apply name tag filter if keyword set
        IF KEYWORD_SET(db_info.data[vd_exp[n]].exp_data_tag) THEN $
          fname = fname[WHERE((STRUPCASE(FILE_BASENAME(fname))).contains(STRUPCASE(db_info.data[vd_exp[n]].exp_data_tag)) EQ 1, n_tag)]

        ; load data
        refr_status, message='db script: load data...'+current_exp

        CASE db_info.data[vd_exp[n]].data_import_fct OF

          'aes_cdf': $
            BEGIN
              chrom = read_AES_cdf(PATH=path, T_SCALE=t_scale, VERSION=version, $
                                   DEF_FILE=fname, SORT_BY_JDATE=sort_by_jdate)
              chrom.instr_type = 5
            END
          'agilent_cdf': $
            BEGIN
              chrom = read_agilent_cdf(PATH=path, T_SCALE=t_scale, VERSION=version, $
                                       DEF_FILE=fname, SORT_BY_JDATE=sort_by_jdate)
              chrom.instr_type = 1
            END
          'almsco_cdf': $
            BEGIN
              chrom =  read_almsco_cdf(PATH=path, T_SCALE=t_scale, VERSION=version, $
                                       DEF_FILE=fname, SORT_BY_JDATE=sort_by_jdate)
              chrom.instr_type = 2
            END
          'ecd_fid_cdf': $
            BEGIN
              chrom = read_ecd_fid_cdf(PATH=path, T_SCALE=t_scale, VERSION=version, $
                                       DEF_FILE=fname, SORT_BY_JDATE=sort_by_jdate)
              chrom.instr_type = 6
            END
          'ecd_txt': $
            BEGIN
              chrom = read_ecd_txt(PATH=path, T_SCALE=t_scale, VERSION=version, $
                                   DEF_FILE=fname, SORT_BY_JDATE=sort_by_jdate)
              chrom.instr_type = 4
            END
          'tofwerk_h5': $
            BEGIN
              chrom = read_tofwerkh5(PATH=path, T_SCALE=t_scale, VERSION=version, $
                                     PEAKDATA_RECALC=tw_recalc_mq, /USE_PT_LIMITS, $
                                     DEF_FILE=fname, SORT_BY_JDATE=sort_by_jdate)
              chrom.instr_type = 3
            END
          ELSE: chrom = !NULL
        ENDCASE

        ; load ms info & add to chrom
        refs = read_subst(DEF_FILE=db_info.data[vd_exp[n]].exp_msinfo_path)
        refi = create_refi()
        subst = add_ires2subst(refs, refi)
        chrom = add_subst2chrom(chrom, subst)
        tot_uniqm = get_uniq_mass(chrom)

        ; call (noisecalc and) integration
        n_chrom = N_ELEMENTS(chrom.fname)
        n_subst = N_ELEMENTS(chrom[0].subst.name)

        refr_status, message='db script: integrate...'+current_exp

        FOR sel_name=0, n_subst-1 DO BEGIN ; integration loop: substances
          FOR sel_chrom=0, n_chrom-1 DO BEGIN ; integration loop: chromatograms
            chrom[sel_chrom].subst[sel_name].rt_win=subst[sel_name].rt_win
            chrom[sel_chrom].subst[sel_name].bl_type=subst[sel_name].bl_type
            chrom[sel_chrom].subst[sel_name].sigma=subst[sel_name].sigma
            chrom[sel_chrom].subst[sel_name].quant=subst[sel_name].quant
            chrom[sel_chrom].subst[sel_name].method=subst[sel_name].method
            IF calcnoise THEN call_noisecalc, sel_chrom, sel_name, 0, /NO_WARN, $
                                              NOISE_UNAME='noise_def_pres'
            call_integration, sel_chrom, sel_name
          ENDFOR ; end integration loop: chromatograms
        ENDFOR ; end integration loop: substances

        db_info.data[vd_exp[n]].autoint_timestamp = jultime2timestring(SYSTIME(/JULIAN))

        ; save chrom file and desired other stuff
        refr_status, message='db script: save results...'+current_exp
        cdate = conc_date(chrom[0].jdate, SYSTIME(/JULIAN), cdate1=cdate1)
        spec_filename=0
        IF KEYWORD_SET(db_info.data[vd_exp[n]].savefile_path) THEN BEGIN
          savepath = db_info.data[vd_exp[n]].savefile_path
          IF STRMATCH(savepath, '*chrom.dat') EQ 1 THEN BEGIN
              chrom_fname=savepath
              spec_filename=1
          ENDIF ELSE BEGIN
            chrom_fname = savepath+'\'+cdate+'_chrom.dat'
          ENDELSE
        ENDIF ELSE BEGIN
          savepath = folder[0]+'\__IAU_CHROM_save'
          db_info.data[vd_exp[n]].savefile_path = savepath
          chrom_fname = savepath+'\'+cdate+'_chrom.dat'
        ENDELSE

        FILE_MKDIR, FILE_DIRNAME(chrom_fname)
        save, chrom, FILENAME=chrom_fname

        subst_fname = chrom_fname.replace('chrom', 'subst')
        save, subst, FILENAME=subst_fname

        IF spec_filename THEN savepath=FILE_DIRNAME(savepath)
        IF KEYWORD_SET(db_info.data[vd_exp[n]].save_txtreport) THEN BEGIN
          txt_path = savepath+'\txt_report\'
          FILE_MKDIR, txt_path
          intres2txt_sglsubst, chrom, 0, 0, PATH=txt_path, /ALL, /NO_PICKFILE
        ENDIF

        IF KEYWORD_SET(db_info.data[vd_exp[n]].save_quicklook) THEN BEGIN
          ps_path = savepath+'\quicklook_ps\'
          FILE_MKDIR, ps_path
          intres_qcklook, chrom, PATH=ps_path
        ENDIF

        w_fin=WHERE(FINITE(chrom.subst.ires.area) EQ 1, n_fin)
        hit_index=FLOAT(n_fin)/FLOAT(n_chrom*n_subst)
        db_info.data[vd_exp[n]].hit_index = STRCOMPRESS(STRING(hit_index, FORMAT='(F5.3)'))

        db_info.data[vd_exp[n]].active = 0

        ; dump intres plots
;        FOR subst=0, N_ELEMENTS(chrom[0].subst.name)-1 DO $
;          plot_intres, chrom, SEL_SUBST_IX=subst, SAVEPLOT='E:\temp'

      ENDFOR ; end loop : N EXPERIMENTS

    ; save refreshed database reference table
    write = write_iauchromdbfile(db_info, PATH=FILE_DIRNAME(db_file))

    refr_status, message='db script processed.'

  ENDIF ELSE BEGIN
    msg = DIALOG_MESSAGE('Script error: aborted.', /ERROR)
    refr_status, /CLEAR
  ENDELSE
  show_list, event, chrom
END