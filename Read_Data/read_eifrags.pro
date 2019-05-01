;------------------------------------------------------------------------------------------------------------------------
;+
; INFO:
; read_eifrags reads fragments database needed for fragment ratio calculator.
;
; AUTHOR:
; F.Obersteiner Dec 2013
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION create_ref_eifrags

  strct={name:      '',$
         formula:   '',$
         frag0:     '',$
         frag0_nom: '' ,$
         frag0_xtm: '',$
         frag0_abd: '',$

         frag1:     '',$
         frag1_nom: '' ,$
         frag1_xtm: '',$
         frag1_abd: '',$

         frag2:     '',$
         frag2_nom: '' ,$
         frag2_xtm: '',$
         frag2_abd: '',$

         frag3:     '',$
         frag3_nom: '' ,$
         frag3_xtm: '',$
         frag3_abd: '',$

         frag4:     '',$
         frag4_nom: '' ,$
         frag4_xtm: '',$
         frag4_abd: '',$

         frag5:     '',$
         frag5_nom: '' ,$
         frag5_xtm: '',$
         frag5_abd: '',$

         frag6:     '',$
         frag6_nom: '' ,$
         frag6_xtm: '',$
         frag6_abd: ''   }

;         frag0:     '',$
;         frag0_nom: 0 ,$
;         frag0_xtm: 0.,$
;         frag0_abd: 0.,$
;
;         frag1:     '',$
;         frag1_nom: 0 ,$
;         frag1_xtm: 0.,$
;         frag1_abd: 0.,$
;
;         frag2:     '',$
;         frag2_nom: 0 ,$
;         frag2_xtm: 0.,$
;         frag2_abd: 0.,$
;
;         frag3:     '',$
;         frag3_nom: 0 ,$
;         frag3_xtm: 0.,$
;         frag3_abd: 0.,$
;
;         frag4:     '',$
;         frag4_nom: 0 ,$
;         frag4_xtm: 0.,$
;         frag4_abd: 0.,$
;
;         frag5:     '',$
;         frag5_nom: 0 ,$
;         frag5_xtm: 0.,$
;         frag5_abd: 0.,$
;
;         frag6:     '',$
;         frag6_nom: 0 ,$
;         frag6_xtm: 0.,$
;         frag6_abd: 0.   }

;         frag7:     '',$
;         frag7_nom: 0 ,$
;         frag7_xtm: 0.,$
;         frag7_abd: 0.,$
;
;         frag8:     '',$
;         frag8_nom: 0 ,$
;         frag8_xtm: 0.,$
;         frag8_abd: 0.,$
;
;         frag9:     '',$
;         frag9_nom: 0 ,$
;         frag9_xtm: 0.,$
;         frag9_abd: 0.      }


  RETURN, strct

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION read_eifrags, PATH=path, FNAME=fname

  IF NOT KEYWORD_SET(path) THEN path='C:\'

;  fname='C:\Users\oberstei\Documents\Promotion\Allgemeines\Fragmentierung\txtexport&ptables\20150203_Fragments_DB.txt'
  IF NOT KEYWORD_SET(fname) THEN info_file = DIALOG_PICKFILE(TITLE = "Please choose fragments.txt with fragment data", $
                                                             PATH=path) ELSE info_file=fname

  IF STRLEN(info_file) EQ 0 THEN RETURN, 0

  OPENR, unit, info_file, /GET_LUN

  line=''
  READF, unit, line
  separator=STRING(9B)
  temp0 = ''
  n_row = 0L

  WHILE NOT EOF(unit) DO BEGIN ; extract all lines into one string
    READF, unit, line
    temp0 = [temp0, line]
    n_row = n_row + 1
  ENDWHILE
  FREE_LUN, unit

  eifrags = REPLICATE(create_ref_eifrags(), n_row)

  FOR row=1, N_ELEMENTS(temp0)-1 DO BEGIN
    temp1 = STRING(STRSPLIT(temp0[row], separator, /EXTRACT))

    eifrags[row-1].name    = temp1[0]
    eifrags[row-1].formula = temp1[1]

    eifrags[row-1].frag0     = temp1[2]
    eifrags[row-1].frag0_nom = temp1[3]
    eifrags[row-1].frag0_xtm = temp1[4]
    eifrags[row-1].frag0_abd = temp1[5]

    eifrags[row-1].frag1     = temp1[6]
    eifrags[row-1].frag1_nom = temp1[7]
    eifrags[row-1].frag1_xtm = temp1[8]
    eifrags[row-1].frag1_abd = temp1[9]

    eifrags[row-1].frag2     = temp1[10]
    eifrags[row-1].frag2_nom = temp1[11]
    eifrags[row-1].frag2_xtm = temp1[12]
    eifrags[row-1].frag2_abd = temp1[13]

    eifrags[row-1].frag3     = temp1[14]
    eifrags[row-1].frag3_nom = temp1[15]
    eifrags[row-1].frag3_xtm = temp1[16]
    eifrags[row-1].frag3_abd = temp1[17]

    eifrags[row-1].frag4     = temp1[18]
    eifrags[row-1].frag4_nom = temp1[19]
    eifrags[row-1].frag4_xtm = temp1[20]
    eifrags[row-1].frag4_abd = temp1[21]

    eifrags[row-1].frag5     = temp1[22]
    eifrags[row-1].frag5_nom = temp1[23]
    eifrags[row-1].frag5_xtm = temp1[24]
    eifrags[row-1].frag5_abd = temp1[25]

    eifrags[row-1].frag6     = temp1[26]
    eifrags[row-1].frag6_nom = temp1[27]
    eifrags[row-1].frag6_xtm = temp1[28]
    eifrags[row-1].frag6_abd = temp1[29]

;    eifrags[row-1].frag7     = temp1[30]
;    eifrags[row-1].frag7_nom = temp1[31]
;    eifrags[row-1].frag7_xtm = temp1[32]
;    eifrags[row-1].frag7_abd = temp1[33]
;
;    eifrags[row-1].frag8     = temp1[34]
;    eifrags[row-1].frag8_nom = temp1[35]
;    eifrags[row-1].frag8_xtm = temp1[36]
;    eifrags[row-1].frag8_abd = temp1[37]
;
;    eifrags[row-1].frag9     = temp1[38]
;    eifrags[row-1].frag9_nom = temp1[39]
;    eifrags[row-1].frag9_xtm = temp1[40]
;    eifrags[row-1].frag9_abd = temp1[41]

  ENDFOR

  CLOSE, unit
  FREE_LUN, unit

  RETURN, eifrags

END