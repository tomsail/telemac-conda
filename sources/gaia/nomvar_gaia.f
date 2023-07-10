!                   **********************
                    SUBROUTINE NOMVAR_GAIA
!                   **********************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Defines the variable names for the results and
!!       geometry files, and in the steering file.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_GAIA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K
!
!     Local arrays dimensioned with fixed parameters from
!     DECLARATIONS_GAIA. It is not a hidden dynamic allocation.
!
      CHARACTER(LEN=32) TEXTE_QSC(NSICLM)
      CHARACTER(LEN=32) TEXTE_ES(NLAYMAX)
      CHARACTER(LEN=32) TEXTE_CONC(NLAYMAX)
      CHARACTER(LEN=32) TEXTE_MTRANS(NLAYMAX)
      CHARACTER(LEN=32) TEXTE_TOCEMUD(NLAYMAX)
      CHARACTER(LEN=32) TEXTE_PARTHE(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_QSC(NSICLM)
      CHARACTER(LEN=8)  MNEMO_CONC(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_MTRANS(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_TOCEMUD(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_PARTHE(NLAYMAX)
      CHARACTER(LEN=2)  CLA,LAY,NP
!
      CHARACTER(LEN=2) CHAR2
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!     Here only English is supported, do not add NOMVAR in French
!     otherwise it causes issues of files compatibility in the
!     restart
!
!     The variables names in TEXTE have to be defined with exactly
!     the same order as they were defined in VARSOR
!
      TEXTE(01) = 'U CLIPPED       M/S             '
      TEXTE(02) = 'V CLIPPED       M/S             '
      TEXTE(03) = 'H CLIPPED       M               '
      TEXTE(04) = 'ZS CLIPPED      M               '
      TEXTE(05) = 'BOTTOM          M               '
      TEXTE(06) = 'CLIPPED Q       M3/S/M          '
      TEXTE(07) = 'CLIPPED QX      M3/S/M          '
      TEXTE(08) = 'CLIPPED QY      M3/S/M          '
      TEXTE(09) = 'RIGID BED       M               '
      TEXTE(10) = 'BED SHEAR STRESSN/M2        '
      TEXTE(11) = 'WAVE HEIGHT HM0 M               '
      TEXTE(12) = 'PEAK PERIOD TPR5S               '
      TEXTE(13) = 'MEAN DIRECTION  DEG             '
      TEXTE(14) = 'SOLID DISCH     KG/(M*S)        '
      TEXTE(15) = 'SOLID DISCH X   KG/(M*S)        '
      TEXTE(16) = 'SOLID DISCH Y   KG/(M*S)        '
      TEXTE(17) = 'CUMUL BED EVOL  M               '
      TEXTE(18) = 'TOTAL ROUGHNESS M               '
      TEXTE(19) = 'SKIN FRICTION MU                '
      TEXTE(20) = 'MEAN DIAMETER M                 '
      TEXTE(21) = 'ORBITAL WAVE VELM/S             '
      TEXTE(22) = 'REFERENCE LEVEL M               '
      NVAR = 22
!
!     Set the RATIOS name in the result file (TEXTE) and in
!     the steering file (MNEMO): one output array per sand class per layer
!     "LAY 2 SAND RAT 3" corresponds to the sand ratio of sand class 3
!     in the 2nd layer
!
      NVAR_RATIOS=NVAR
      DO I=1,NSAND
        DO J=1,NOMBLAY
          IF(J.LT.10) THEN
            WRITE(LAY,'(I1)') J
          ELSEIF(J.LT.100) THEN
            WRITE(LAY,'(I2)') J
          ELSE
            WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NOMBLAY
            WRITE (LU,*) '                LAYERS'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(I.LT.10) THEN
            WRITE(CLA,'(I1)') I
          ELSEIF(I.LT.100) THEN
            WRITE(CLA,'(I2)') I
          ELSE
            WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NSAND
            WRITE (LU,*) '                CLASSES'
            CALL PLANTE(1)
            STOP
          ENDIF
!         Sand ratio: store all layers of class 1, then all layers of class 2, etc.
!         It has to be the same order as in point_gaia
          TEXTE(NVAR+(I-1)*NOMBLAY+J)=
     &       TRIM('LAY'//LAY//'SAND RAT'//CLA)
          MNEMO(NVAR+(I-1)*NOMBLAY+J)=TRIM(LAY)//'A'//CLA
        ENDDO
      ENDDO
      NVAR=NVAR+NOMBLAY*NSAND
!
!     Set the variables' names in the result file (TEXTE) and in
!     the steering file (MNEMO) for the arrays with NSICLA
!     entries -> one output array per sediment class
      DO J=1,NSICLA
!       Make sure that the numbers from 1 to 9
!       are printed with a 1 digit integer format
        IF(J.LT.11) THEN
          WRITE(CLA,'(I1)') J
!       Make sure that the numbers from 10 to 100
!       are printed with a 2 digits integer format
!       This is sufficient at the moment since NISCLM = 10
        ELSEIF(J.LT.100) THEN
          WRITE(CLA,'(I2)') J
        ELSE
          WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NSICLA
          WRITE (LU,*) '                CLASSES'
          CALL PLANTE(1)
          STOP
        ENDIF
!       Transport rate QSCL for the class number CLA
!       (sum of bedload + suspension)
        TEXTE(NVAR+J)  = TRIM('QS CLASS '//CLA)
        MNEMO(NVAR+J)  = TRIM('QS'//CLA)
!       Bedload transport rate (QSCL_C) for the class number CLA
        TEXTE_QSC(J) = TRIM('QS BEDLOAD CL'//CLA)
        MNEMO_QSC(J) = TRIM('QSBL'//CLA)
      ENDDO
!
      NVAR_QSCL=NVAR
      NVAR=NVAR+NSICLA
!
      NVAR_QS_C=NVAR
      TEXTE(NVAR_QS_C+1)='QS BEDLOAD      KG/(M*S)        '
!
      NVAR_QSXC=NVAR+1
      TEXTE(NVAR_QSXC+1)='QS BEDLOAD X    KG/(M*S)        '
!
      NVAR_QSYC=NVAR+2
      TEXTE(NVAR_QSYC+1)='QS BEDLOAD Y    KG/(M*S)        '
!
      NVAR=NVAR_QSYC+1
      NVAR_QSCL_C = NVAR
!
      DO I=1,NSICLA
        TEXTE(NVAR+I) = TEXTE_QSC(I)
        MNEMO(NVAR+I) = MNEMO_QSC(I)
      ENDDO
      NVAR=NVAR+NSICLA
!
!     Set the variables' names in the result file (TEXTE) and in
!     the steering file (MNEMO) for the arrays with NOMBLAY*NPOIN
!     entries -> one output array per layer
      DO K=1,NOMBLAY
!       Make sure that the numbers from 1 to 9
!       are printed with a 1 digit integer format
        IF(K.LT.10) THEN
          WRITE(LAY,'(I1)') K
!       Make sure that the numbers from 10 to 100
!       are printed with a 2 digits integer format
!       This is sufficient at the moment since NLAYMAX = 20
        ELSEIF(K.LT.100) THEN
          WRITE(LAY,'(I2)') K
        ELSE
          WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NOMBLAY
          WRITE (LU,*) '                LAYERS'
          CALL PLANTE(1)
          STOP
        ENDIF
        ! Thickness of the layer LAY (ES)
        TEXTE_ES(K)(1:16)  = 'LAYER'//LAY//' THICKNES'
        TEXTE_ES(K)(17:32) = 'M               '
        MNEMO_ES(K) = TRIM(LAY)//'ES     '
        ! Concentration of the layer LAY (LAYCONC)
        TEXTE_CONC(K)(1:16)  = 'LAYER'//LAY//' CONC    '
        TEXTE_CONC(K)(17:32) = 'KG/L            '
        MNEMO_CONC(K) = TRIM(LAY)//'CONC   '
        ! Mass transfer of the layer LAY (MTRANSFER)
        TEXTE_MTRANS(K)(1:16)  = 'LAYER'//LAY//' MTRANS  '
        TEXTE_MTRANS(K)(17:32) = 'KG              '
        MNEMO_MTRANS(K) = TRIM(LAY)//'MTRANS '
        ! Critical erosion shear stress of the mud of the layer LAY
        ! (TOCEMUD)
        TEXTE_TOCEMUD(K)(1:16)  = 'LAYER'//LAY//' TOCEMU  '
        TEXTE_TOCEMUD(K)(17:32) = 'KG-1 S-2 M-1    '
        MNEMO_TOCEMUD(K) = TRIM(LAY)//'TOCEMU '
        ! Partheniades erosion coefficient of the layer LAY (PARTHE)
        TEXTE_PARTHE(K)(1:16)  = 'LAYER'//LAY//' PARTHE  '
        TEXTE_PARTHE(K)(17:32) = 'KG M-2 S-1      '
        MNEMO_PARTHE(K) = TRIM(LAY)//'PARTHE '
      ENDDO
!
      DO I=1,NOMBLAY
        TEXTE(NVAR+I) = TEXTE_ES(I)
        MNEMO(NVAR+I) = MNEMO_ES(I)
        TEXTE(NVAR+NOMBLAY+I) = TEXTE_CONC(I)
        MNEMO(NVAR+NOMBLAY+I) = MNEMO_CONC(I)
      ENDDO
      NVAR_LAYTHI = NVAR
      NVAR_LAYCONC= NVAR+NOMBLAY
      NVAR=NVAR+2*NOMBLAY
!
      NVAR_PRIV=NVAR
      DO I=1,MAX(4,NPRIV)
        WRITE(NP,'(I1)') I
        TEXTE(NVAR_PRIV+I)=TRIM('PRIVE '//NP//'         ')
      ENDDO
      NVAR=NVAR+MAX(4,NPRIV)
!
!     IF NAMES OF PRIVATE VARIABLES GIVEN
!
      IF(N_NAMES_PRIV.GT.0) THEN
        DO I=1,N_NAMES_PRIV
          TEXTE(NVAR_PRIV+I)=NAMES_PRIVE(I)
        ENDDO
      ENDIF
!
      NVAR_RATIOM=NVAR
!     2 3RD FRACTION MEANS FRACTION OF MUD OF CLASS 3 IN 2ND LAYER
!
      DO I=1,NMUD
        DO J=1,NOMBLAY
          IF(J.LT.10) THEN
            WRITE(LAY,'(I1)') J
          ELSEIF(J.LT.100) THEN
            WRITE(LAY,'(I2)') J
          ELSE
            WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NOMBLAY
            WRITE (LU,*) '                LAYERS'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(I.LT.10) THEN
            WRITE(CLA,'(I1)') I
          ELSEIF(I.LT.100) THEN
            WRITE(CLA,'(I2)') I
          ELSE
            WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NMUD
            WRITE (LU,*) '                CLASSES'
            CALL PLANTE(1)
            STOP
          ENDIF
!         RATIO MUD: ALL LAYERS OF CLASS 1, THEN ALL LAYERS OF CLASS 2, ETC.
!         SAME ORDER AS IN POINT_GAIA
          TEXTE(NVAR_RATIOM+(I-1)*NOMBLAY+J)=
     &         TRIM('LAY'//LAY//'RAT MUD'//CLA)
          MNEMO(NVAR_RATIOM+(I-1)*NOMBLAY+J)=TRIM(LAY)//'R'//CLA
        ENDDO
      ENDDO
      NVAR=NVAR+NOMBLAY*NMUD
!
      NVAR_MASS_S=NVAR
!     2 3RD FRACTION MEANS FRACTION OF MUD OF CLASS 3 IN 2ND LAYER
!
      DO I=1,NSAND
        DO J=1,NOMBLAY
          IF(J.LT.10) THEN
            WRITE(LAY,'(I1)') J
          ELSEIF(J.LT.100) THEN
            WRITE(LAY,'(I2)') J
          ELSE
            WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NOMBLAY
            WRITE (LU,*) '                LAYERS'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(I.LT.10) THEN
            WRITE(CLA,'(I1)') I
          ELSEIF(I.LT.100) THEN
            WRITE(CLA,'(I2)') I
          ELSE
            WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NSAND
            WRITE (LU,*) '                CLASSES'
            CALL PLANTE(1)
            STOP
          ENDIF
!         MASS_S: ALL LAYERS OF CLASS 1, THEN ALL LAYERS OF CLASS 2, ETC.
!         SAME ORDER AS IN POINT_GAIA
          TEXTE(NVAR_MASS_S+(I-1)*NOMBLAY+J)=
     &         TRIM('LAY'//LAY//'MAS SAND'//CLA)
          MNEMO(NVAR_MASS_S+(I-1)*NOMBLAY+J)=TRIM(LAY)//'S'//CLA
        ENDDO
      ENDDO
      NVAR=NVAR+NOMBLAY*NSAND
!
      NVAR_MASS_M=NVAR
!     2 3RD FRACTION MEANS FRACTION OF MUD OF CLASS 3 IN 2ND LAYER
!
      DO I=1,NMUD
        DO J=1,NOMBLAY
          IF(J.LT.10) THEN
            WRITE(LAY,'(I1)') J
          ELSEIF(J.LT.100) THEN
            WRITE(LAY,'(I2)') J
          ELSE
            WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NOMBLAY
            WRITE (LU,*) '                LAYERS'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(I.LT.10) THEN
            WRITE(CLA,'(I1)') I
          ELSEIF(I.LT.100) THEN
            WRITE(CLA,'(I2)') I
          ELSE
            WRITE (LU,*) 'NOMVAR_GAIA: NOT IMPLEMENTED FOR ',NMUD
            WRITE (LU,*) '                CLASSES'
            CALL PLANTE(1)
            STOP
          ENDIF
!         MASS_M: ALL LAYERS OF CLASS 1, THEN ALL LAYERS OF CLASS 2, ETC.
!         SAME ORDER AS IN POINT_GAIA
          TEXTE(NVAR_MASS_M+(I-1)*NOMBLAY+J)=
     &         TRIM('LAY'//LAY//'MASS MUD'//CLA)
          MNEMO(NVAR_MASS_M+(I-1)*NOMBLAY+J)=TRIM(LAY)//'M'//CLA
        ENDDO
      ENDDO
      NVAR=NVAR+NOMBLAY*NMUD
!
      DO I=1,NOMBLAY
        TEXTE(NVAR+I) = TEXTE_MTRANS(I)
        MNEMO(NVAR+I) = MNEMO_MTRANS(I)
        TEXTE(NVAR+NOMBLAY+I) = TEXTE_TOCEMUD(I)
        MNEMO(NVAR+NOMBLAY+I) = MNEMO_TOCEMUD(I)
        TEXTE(NVAR+2*NOMBLAY+I) = TEXTE_PARTHE(I)
        MNEMO(NVAR+2*NOMBLAY+I) = MNEMO_PARTHE(I)
      ENDDO
      NVAR_MTRANS = NVAR
      NVAR_TOCEMUD= NVAR+NOMBLAY
      NVAR_PARTHE = NVAR+2*NOMBLAY
      NVAR=NVAR+NOMBLAY*3
!
      DO I=1,NVAR
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
!     MEAN BOTTOM FRICTION
      MNEMO(10)   = 'TOB     '
!     WAVE HEIGHT
      MNEMO(11)   = 'W       '
!     PEAK PERIOD
      MNEMO(12)   = 'X       '
!     WAVE DIRECTION
      MNEMO(13)   = 'THETAW  '
!     SOLID DISCHARGE
      MNEMO(14)   = 'M       '
!     SOLID DISCHARGE X
      MNEMO(15)   = 'N       '
!     SOLID DISCHARGE Y
      MNEMO(16)   = 'P       '
!     EVOLUTION
      MNEMO(17)   = 'E       '
!     KS
      MNEMO(18)   = 'KS      '
!     MU
      MNEMO(19)   = 'MU      '
!     D50
      MNEMO(20)   = 'D50     '
!     WAVE ORBITAL VELOCITY
      MNEMO(21)   = 'UWB     '
!     REFERENCE LEVEL FOR NESTPOR
      MNEMO(22)   = 'ZRL     '
!     BEDLOAD TRANSPORT RATE
      MNEMO(NVAR_QS_C+1) = 'QSBL    '
!     BEDLOAD TRANSPORT RATE X
      MNEMO(NVAR_QSXC+1) = 'QSBLX   '
!     BEDLOAD TRANSPORT RATE Y
      MNEMO(NVAR_QSYC+1) = 'QSBLY   '
!     PRIVATE VARIABLE 1
      MNEMO(NVAR_PRIV+1) = 'A       '
!     PRIVATE VARIABLE 2
      MNEMO(NVAR_PRIV+2) = 'G       '
!     PRIVATE VARIABLE 3
      MNEMO(NVAR_PRIV+3) = 'L       '
!     PRIVATE VARIABLE 4
      MNEMO(NVAR_PRIV+4) = 'O       '
!
!-----------------------------------------------------------------------
!     CLANDESTINES VARIABLES
      NVAR_VARCL=NVAR
      IF(VARCL%N.GT.0) THEN
        DO I=1,VARCL%N
          TEXTE(NVAR_VARCL+I)='VAR CLA '
          TEXTPR(NVAR_VARCL+I)='VAR CLA '
          MNEMO(NVAR_VARCL+I)='VC      '
        ENDDO
      ENDIF
      NVAR=NVAR+VARCL%N
!
!-----------------------------------------------------------------------
!
!     DIFFERENTIATORS
!
      NVAR_ADVAR=NVAR
      IF(NADVAR.GT.0) THEN
        DO I=1,NADVAR
          TEXTE(NVAR_ADVAR+I)  = NAMES_ADVAR(I)
          TEXTPR(NVAR_ADVAR+I) = NAMES_ADVAR(I)
          WRITE(CHAR2,'(I2)') I
          MNEMO(NVAR_ADVAR+I)  = 'AD'//ADJUSTL(CHAR2)//'    '
        ENDDO
      ENDIF
      NVAR=NVAR+NADVAR
!
!-----------------------------------------------------------------------
!
      IF(NVAR.LT.MAXVAR) THEN
        DO I=NVAR+1,MAXVAR
          MNEMO(I) =' '
          TEXTE(I) =' '
          TEXTPR(I)=' '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NVAR.GT.MAXVAR) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'NOMVAR_GAIA : MAXVAR DOIT VALOIR AU MOINS ',
     &                NVAR
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'NOMVAR_GAIA: MAXVAR SHOULD BE AT LEAST ',
     &                NVAR
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
