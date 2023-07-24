!                   *************************
                    SUBROUTINE NOMVAR_SISYPHE
!                   *************************
!
     &(TEXTE,TEXTPR,MNEMO,NSICLA,UNITE,MAXVAR,NPRIV,NOMBLAY,
     & N_NAMES_PRIV,NAMES_PRIVE,NADVAR,NAMES_ADVAR)
!
!***********************************************************************
! SISYPHE   V7P1
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND
!+                GEOMETRY FILES.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/95
!+
!+
!
!history  M. GONZALES DE LINARES; C.VILLARET
!+        2003
!+
!+
!
!history  JMH
!+        03/11/2009
!+        V6P0
!+   MODIFIED AFTER JACEK JANKOWSKI DEVELOPMENTS
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  CV + JMH (LNHE)
!+        17/01/2012
!+        V6P2
!+   Adaptation to greater numbers of layers and classes (up to 99 each)
!
!history  JWI (HRW)
!+        14/06/2012
!+        V6P2
!+   Increment of one to include wave orbital velocities
!
!history  PAT (LNHE)
!+        18/06/2012
!+        V6P2
!+   updated version with HRW's development
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/07/2015
!+        V7P1
!+   Adding the names of private variables.
!
!history  S.E. BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+   Now taking into account names of differentiators given by user.
!
!history  B.GLANDER (BAW)
!+        06/12/2018
!+        V7P2
!+  NEW VARIABLE: ZRL  REFERENCE LEVEL FOR NESTOR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAXVAR         |-->| MAXIMUM NUMBER OF OUTPUT VARIABLES
!| MNEMO          |<--| SYMBOLS TO SPECIFY THE VARIABLES FOR OUTPUT
!|                |   | IN THE STEERING FILE
!| NOMBLAY        |-->| NUMBER OF LAYERS
!| NPRIV          |-->| NUMBER OF PRIVATE ARRAYS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NSICLM         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| TEXTE          |<--| NAMES OF VARIABLES (PRINTOUT)
!| TEXTPR         |<--| NAMES OF VARIABLES (INPUT)
!| UNIT           |-->| LOGICAL, FILE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE, ONLY : NSICLM,NLAYMAX,NVAR_SIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NSICLA,MAXVAR,NPRIV,NOMBLAY
      INTEGER, INTENT(IN)              :: N_NAMES_PRIV
      INTEGER, INTENT(IN)              :: NADVAR
      CHARACTER(LEN=8), INTENT(INOUT)  :: MNEMO(MAXVAR)
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_PRIVE(N_NAMES_PRIV)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_ADVAR(*)
      LOGICAL, INTENT(IN)              :: UNITE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,ADD
!
!     LOCAL ARRAYS DIMENSIONED WITH FIXED PARAMETERS FROM
!     DECLARATIONS_SISYPHE. IT IS NOT A HIDDEN DYNAMIC ALLOCATION
!
      CHARACTER(LEN=32) TEXTE_QS(NSICLM)
      CHARACTER(LEN=32) TEXTE_CS(NSICLM),TEXTE_QSC(NSICLM)
      CHARACTER(LEN=32) TEXTE_QSS(NSICLM),TEXTE_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_QS(NSICLM)
      CHARACTER(LEN=8)  MNEMO_CS(NSICLM),MNEMO_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_QSC(NSICLM),MNEMO_QSS(NSICLM)
      CHARACTER(LEN=2)  CLA,LAY
      CHARACTER(LEN=32) TEXTE_CONC(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_CONC(NLAYMAX)
!
      CHARACTER(LEN=2) CHAR2
!
!-----------------------------------------------------------------------
!
      ADD=27+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+2*NOMBLAY
!
      IF(ADD.GT.MAXVAR) THEN
        WRITE(LU,*) 'NOMVAR_SISYPHE: MAXVAR SHOULD BE AT LEAST ',
     &              ADD
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     2 3RD FRACTION MEANS FRACTION OF SEDIMENT OF CLASS 3 IN 2ND LAYER
!
      DO I=1,NSICLA
        DO J=1,NOMBLAY
          IF(J.LT.10) THEN
            WRITE(LAY,'(I1)') J
          ELSEIF(J.LT.100) THEN
            WRITE(LAY,'(I2)') J
          ELSE
            WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NOMBLAY
            WRITE (LU,*) '                LAYERS'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(I.LT.10) THEN
            WRITE(CLA,'(I1)') I
          ELSEIF(I.LT.100) THEN
            WRITE(CLA,'(I2)') I
          ELSE
            WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NSICLA
            WRITE (LU,*) '                CLASSES'
            CALL PLANTE(1)
            STOP
          ENDIF
!         AVAIL: ALL LAYERS OF CLASS 1, THEN ALL LAYERS OF CLASS 2, ETC.
!         SAME ORDER AS IN POINT_SISYPHE
          TEXTE(23+(I-1)*NOMBLAY+J)=TRIM('FRACLAY '//LAY//' CL '//CLA)
          MNEMO(23+(I-1)*NOMBLAY+J)=TRIM(LAY)//'A'//CLA
        ENDDO
      ENDDO
!
      DO J=1,NSICLA
        IF(J.LT.10) THEN
          WRITE(CLA,'(I1)') J
        ELSEIF(J.LT.100) THEN
          WRITE(CLA,'(I2)') J
        ELSE
          WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NSICLA
          WRITE (LU,*) '                CLASSES'
          CALL PLANTE(1)
          STOP
        ENDIF
        TEXTE_QS(J)  = TRIM('QS CLASS '//CLA)
        TEXTE_QSC(J) = TRIM('QS BEDLOAD CL'//CLA)
        TEXTE_QSS(J) = TRIM('QS SUSP. CL'//CLA)
        IF(UNITE) THEN
          TEXTE_CS(J) = TRIM('CONC MAS CL'//CLA)
          TEXTE_CS(J)(17:19) = 'G/L'
        ELSE
          TEXTE_CS(J) = TRIM('CONC VOL CL'//CLA)
        ENDIF
        MNEMO_QS(J)  = TRIM('QS'//CLA)
        MNEMO_QSC(J) = TRIM('QSBL'//CLA)
        MNEMO_QSS(J) = TRIM('QSS'//CLA)
        MNEMO_CS(J)  = TRIM('CS'//CLA)
      ENDDO
!
      DO K=1,NOMBLAY
        IF(K.LT.10) THEN
          WRITE(LAY,'(I1)') K
          MNEMO_ES(K) = TRIM(LAY)//'ES     '
        ELSEIF(K.LT.100) THEN
          WRITE(LAY,'(I2)') K
          MNEMO_ES(K) = TRIM(LAY)//'ES    '
        ELSE
          WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NOMBLAY
          WRITE (LU,*) '                LAYERS'
          CALL PLANTE(1)
          STOP
        ENDIF
        TEXTE_ES(K)(1:16)  = 'LAYER'//LAY//' THICKNES'
        TEXTE_ES(K)(17:32) = 'M               '
      ENDDO
!
      DO K=1,NOMBLAY
        IF(K.LT.10) THEN
          WRITE(LAY,'(I1)') K
          MNEMO_CONC(K) = TRIM(LAY)//'CONC     '
        ELSEIF(K.LT.100) THEN
          WRITE(LAY,'(I2)') K
          MNEMO_CONC(K) = TRIM(LAY)//'CONC    '
        ELSE
          WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NOMBLAY
          WRITE (LU,*) '                LAYERS'
          CALL PLANTE(1)
          STOP
        ENDIF
        TEXTE_CONC(K)(1:16)  = 'LAYER'//LAY//' CONC    '
        TEXTE_CONC(K)(17:32) = 'KG/L            '
      ENDDO

!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.LNG_EN) THEN
!
!       ENGLISH VERSION
!
        TEXTE(01) = 'VELOCITY U      M/S             '
        TEXTE(02) = 'VELOCITY V      M/S             '
        TEXTE(03) = 'WATER DEPTH     M               '
        TEXTE(04) = 'FREE SURFACE    M               '
        TEXTE(05) = 'BOTTOM          M               '
        TEXTE(06) = 'FLOWRATE Q      M3/S/M          '
        TEXTE(07) = 'FLOWRATE QX     M3/S/M          '
        TEXTE(08) = 'FLOWRATE QY     M3/S/M          '
        TEXTE(09) = 'RIGID BED       M               '
        TEXTE(10) = 'FRICTION COEFT                  '
        TEXTE(11) = 'BED SHEAR STRESSN/M2        '
        TEXTE(12) = 'WAVE HEIGHT HM0 M               '
        TEXTE(13) = 'PEAK PERIOD TPR5S               '
        TEXTE(14) = 'MEAN DIRECTION  DEG             '
        TEXTE(15) = 'SOLID DISCH     M2/S            '
        TEXTE(16) = 'SOLID DISCH X   M2/S            '
        TEXTE(17) = 'SOLID DISCH Y   M2/S            '
        TEXTE(18) = 'EVOLUTION       M               '
        TEXTE(19) = 'RUGOSITE TOTALE M               '
        TEXTE(20) = 'FROT. PEAU MU                   '
        TEXTE(21) = 'MEAN DIAMETER M                 '
        TEXTE(22) = 'BOTTOM VELOCITY M/S             '
        TEXTE(23) = 'REFERENCE LEVEL M               '   ! reference level for Nestor

        NVAR_SIS = 23
!
        DO I=1,NSICLA
          TEXTE(NVAR_SIS+I+NOMBLAY*NSICLA)     = TEXTE_QS(I)
          MNEMO(NVAR_SIS+I+NOMBLAY*NSICLA)     = MNEMO_QS(I)
          TEXTE(NVAR_SIS+I+(NOMBLAY+1)*NSICLA) = TEXTE_CS(I)
          MNEMO(NVAR_SIS+I+(NOMBLAY+1)*NSICLA) = MNEMO_CS(I)
        ENDDO
!
        ADD=NVAR_SIS+NSICLA*(NOMBLAY+2)
        TEXTE(1+ADD)='QS BEDLOAD      M2/S            '
        TEXTE(2+ADD)='QS BEDLOAD X    M2/S            '
        TEXTE(3+ADD)='QS BEDLOAD Y    M2/S            '
        TEXTE(4+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(5+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(6+ADD)='QS SUSPENSION Y M2/S            '
!
      ELSE
!
!       FRENCH VERSION
!
        TEXTE(01)  = 'VITESSE U       M/S             '
        TEXTE(02)  = 'VITESSE V       M/S             '
        TEXTE(03)  = 'HAUTEUR D''EAU   M              '
        TEXTE(04)  = 'SURFACE LIBRE   M               '
        TEXTE(05)  = 'FOND            M               '
        TEXTE(06)  = 'DEBIT           M3/S/M          '
        TEXTE(07)  = 'DEBIT QX        M3/S/M          '
        TEXTE(08)  = 'DEBIT QY        M3/S/M          '
        TEXTE(09)  = 'FOND RIGIDE     M               '
        TEXTE(10)  = 'COEFT FROTTEMENT                '
        TEXTE(11)  = 'FROTTEMENT TOB  N/M2            '
        TEXTE(12)  = 'HAUTEUR HM0     M               '
        TEXTE(13)  = 'PERIODE PIC TPR5S               '
        TEXTE(14)  = 'DIRECTION MOY   DEG             '
        TEXTE(15)  = 'DEBIT SOLIDE    M2/S            '
        TEXTE(16)  = 'DEBIT SOLIDE X  M2/S            '
        TEXTE(17)  = 'DEBIT SOLIDE Y  M2/S            '
        TEXTE(18)  = 'EVOLUTION       M               '
        TEXTE(19)  = 'RUGOSITE TOTALE.M               '
        TEXTE(20)  = 'CORR FROTT PEAU MU              '
        TEXTE(21)  = 'DIAMETRE MOYEN  M               '
        TEXTE(22)  = 'VITESSE FOND    M/S             '
        TEXTE(23)  = 'REFERENCE LEVEL M               '   ! reference level for Nestor

        NVAR_SIS = 23
!
        DO I=1,NSICLA
          TEXTE(NVAR_SIS+I+NOMBLAY*NSICLA)     = TEXTE_QS(I)
          MNEMO(NVAR_SIS+I+NOMBLAY*NSICLA)     = MNEMO_QS(I)
          TEXTE(NVAR_SIS+I+(NOMBLAY+1)*NSICLA) = TEXTE_CS(I)
          MNEMO(NVAR_SIS+I+(NOMBLAY+1)*NSICLA) = MNEMO_CS(I)
        ENDDO
!
        ADD=NVAR_SIS+NSICLA*(NOMBLAY+2)
        TEXTE(1+ADD)='QS CHARRIAGE    M2/S            '
        TEXTE(2+ADD)='QS CHARRIAGE X  M2/S            '
        TEXTE(3+ADD)='QS CHARRIAGE Y  M2/S            '
        TEXTE(4+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(5+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(6+ADD)='QS SUSPENSION Y M2/S            '
!
      ENDIF
!
      ADD = NVAR_SIS + 6
      DO I=1,NSICLA
        TEXTE(ADD+I+NSICLA*(NOMBLAY+2)) = TEXTE_QSC(I)
        MNEMO(ADD+I+NSICLA*(NOMBLAY+2)) = MNEMO_QSC(I)
        TEXTE(ADD+I+NSICLA*(NOMBLAY+3)) = TEXTE_QSS(I)
        MNEMO(ADD+I+NSICLA*(NOMBLAY+3)) = MNEMO_QSS(I)
      ENDDO
!
      DO I=1,NOMBLAY
        TEXTE(ADD+I+NSICLA*(NOMBLAY+4)) = TEXTE_ES(I)
        MNEMO(ADD+I+NSICLA*(NOMBLAY+4)) = MNEMO_ES(I)
      ENDDO
!
      DO I=1,NOMBLAY
        TEXTE(ADD+I+NSICLA*(NOMBLAY+4)+NOMBLAY) = TEXTE_CONC(I)
        MNEMO(ADD+I+NSICLA*(NOMBLAY+4)+NOMBLAY) = MNEMO_CONC(I)
      ENDDO
!
      ADD=NVAR_SIS+6+NSICLA*(NOMBLAY+4)+2*NOMBLAY
!
      TEXTE(1+ADD)='PRIVE 1                         '
      TEXTE(2+ADD)='PRIVE 2                         '
      TEXTE(3+ADD)='PRIVE 3                         '
      TEXTE(4+ADD)='PRIVE 4                         '
!
!     NPRIV MAY BE GREATER THAN 4
!     TEXTE(5+ADD)='PRIVE 5                         '
!
!     IF NAMES OF PRIVATE VARIABLES GIVEN
!
      IF(N_NAMES_PRIV.GT.0) THEN
        DO I=1,N_NAMES_PRIV
          TEXTE(ADD+I)=NAMES_PRIVE(I)
        ENDDO
      ENDIF
!
      DO I=1,4+ADD
        TEXTPR(I)=TEXTE(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     OTHER NAMES FOR OUTPUT VARIABLES (STEERING FILE)
!
!     VELOCITY U
      MNEMO(1)   = 'U       '
!     VELOCITY V
      MNEMO(2)   = 'V       '
!     WATER DEPTH
      MNEMO(3)   = 'H       '
!     FREE SURFACE
      MNEMO(4)   = 'S       '
!     BOTTOM
      MNEMO(5)   = 'B       '
!     SCALAR FLOW RATE
      MNEMO(6)   = 'Q       '
!     SCALAR FLOW RATE X
      MNEMO(7)   = 'I       '
!     SCALAR FLOW RATE Y
      MNEMO(8)   = 'J       '
!     RIGID BED
      MNEMO(9)   = 'R       '
!     FRICTION COEFFICIENT
      MNEMO(10)   = 'CHESTR  '
!     MEAN BOTTOM FRICTION
      MNEMO(11)   = 'TOB     '
!     WAVE HEIGHT
      MNEMO(12)   = 'W       '
!     PEAK PERIOD
      MNEMO(13)   = 'X       '
!     WAVE DIRECTION
      MNEMO(14)   = 'THETAW  '
!     SOLID DISCHARGE
      MNEMO(15)   = 'M       '
!     SOLID DISCHARGE X
      MNEMO(16)   = 'N       '
!     SOLID DISCHARGE Y
      MNEMO(17)   = 'P       '
!     EVOLUTION
      MNEMO(18)   = 'E       '
!     KS
      MNEMO(19)   = 'KS      '
!     MU
      MNEMO(20)   = 'MU      '
!     D50
      MNEMO(21)   = 'D50     '
!
!     wave orbital velocities
      MNEMO(22)   = 'UWB     '
!     reference level for nestor
      MNEMO(23)   = 'ZRL     '
!
      MNEMO(NVAR_SIS+1+NSICLA*(NOMBLAY+2)) = 'QSBL    '
      MNEMO(NVAR_SIS+2+NSICLA*(NOMBLAY+2)) = 'QSBLX   '
      MNEMO(NVAR_SIS+3+NSICLA*(NOMBLAY+2)) = 'QSBLY   '
      MNEMO(NVAR_SIS+4+NSICLA*(NOMBLAY+2)) = 'QSSUSP  '
      MNEMO(NVAR_SIS+5+NSICLA*(NOMBLAY+2)) = 'QSSUSPX '
      MNEMO(NVAR_SIS+6+NSICLA*(NOMBLAY+2)) = 'QSSUSPY '

      ADD=NVAR_SIS+6+NSICLA*(NOMBLAY+4)+2*NOMBLAY
      MNEMO(1+ADD) = 'A       '
      MNEMO(2+ADD) = 'G       '
      MNEMO(3+ADD) = 'L       '
      MNEMO(4+ADD) = 'O       '
!     THE NUMBER OF PRIVATE ARRAYS IS A KEYWORD
!     MNEMO(31+ADD) = '????????'
!
!----------------------------
      ADD=NSICLA*(NOMBLAY+4)+2*NOMBLAY+NVAR_SIS+6+MAX(NPRIV,4)
      IF(ADD.LT.MAXVAR) THEN
        DO I=ADD+1,MAXVAR
          MNEMO(I) =' '
          TEXTE(I) =' '
          TEXTPR(I)=' '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     DIFFERENTIATORS
!
      IF(NADVAR.GT.0) THEN
        DO I=1,NADVAR
          TEXTE(ADD+I)  = NAMES_ADVAR(I)
          TEXTPR(ADD+I) = NAMES_ADVAR(I)
          WRITE(CHAR2,'(I2)') I
          MNEMO(ADD+I)  = 'AD'//ADJUSTL(CHAR2)//'    '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
