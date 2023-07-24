!                   *****************
                    SUBROUTINE TRANSF
!                   *****************
!
     &( FA    , FR    , XK    , KNEW  , NEWF  , NEWF1 , TAUX1 , TAUX2 ,
     &  NPOIN2, NDIRE , NF   )
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    CONVERTS A SPECTRUM SPECIFIED IN RELATIVE
!+                FREQUENCY FR(-,-,-) INTO A SPECTRUM IN ABSOLUTE
!+                FREQUENCY FA(-,-,-).
!
!history  M. BENOIT (LNHE)
!+        12/01//2006
!+        V5P6
!+
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
!history  G.MATTAROLO (EDF - LNHE)
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COSTET         |-->| COSINE OF TETA ANGLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| FA             |<--| DIRECTIONAL SPECTRUM IN ABSOLUTE FREQUENCIES
!| FR             |-->| DIRECTIONAL SPECTRUM IN RELATIVE FREQUENCIES
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| KNEW           |<->| WORK TABLE
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NEWF           |<->| WORK TABLE
!| NEWF1          |<->| WORK TABLE
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| RAISF          |-->| RAISON FREQUENTIELLE
!| SINTET         |-->| SINE OF TETA ANGLE
!| TAUX1          |<->| WORK TABLE
!| TAUX2          |<->| WORK TABLE
!| UC             |-->| CURRENT VELOCITY ALONG X AT THE MESH POINTS
!| VC             |-->| CURRENT VELOCITY ALONG Y AT THE MESH POINTS
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : FREQ, DFREQ, COSTET, SINTET,
     &       USDPI, UC, VC, RAISF
      USE BIEF, ONLY: OV
!
      USE INTERFACE_TOMAWAC, EX_TRANSF => TRANSF
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    :: NPOIN2, NDIRE, NF
      INTEGER, INTENT(INOUT) :: KNEW(NPOIN2),NEWF(NPOIN2), NEWF1(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: FR(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2),TAUX2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FA(NPOIN2,NDIRE,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER          IP    , JP    , JF   , NEWM  , NEWM1 , KH
      DOUBLE PRECISION F0    , UK    , AUXI , Z
      DOUBLE PRECISION FNEW  , UNSLRF
!
!-----------------------------------------------------------------------
!     CHANGES ONLY THE END DATES
!-----------------------------------------------------------------------
!!
!-----------------------------------------------------------------------
!
      F0=FREQ(1)
      UNSLRF=1.D0/LOG(RAISF)
!
      CALL OV('X=0     ', X=FA, DIM1=NPOIN2*NDIRE*NF)
      DO IP=1,NPOIN2
        TAUX1(IP)=0.D0
        TAUX2(IP)=0.D0
        KNEW (IP)=-1
        NEWF (IP)=-1
        NEWF1(IP)=-1
      ENDDO  
!
      DO JF=1,NF
        DO JP=1,NDIRE
          DO IP=1,NPOIN2
!           ---------------------------------------------------------
!           COMPUTES THE DIFFERENCE BETWEEN ABSOLUTE AND RELATIVE FREQUENCIES
!                                            -> ->
!                 Z = FREQ_ABS - FREQ_REL = (K .U)/(2.PI)
!           THE SPECTRUM IS PROJECTED ONTO THE ABSOLUTE FREQUENCIES
!           ONLY IF THE RELATIVE VARIATION Z/FREQ_REL IS SIGNIFICANT
!           ---------------------------------------------------------
            UK=SINTET(JP)*UC(IP)+COSTET(JP)*VC(IP)
!
            Z=UK*XK(IP,JF)*USDPI
!
            IF(ABS(Z)/FREQ(JF).LT.1.D-3) THEN
              KNEW (IP)=JP
              NEWF (IP)=JF
              NEWF1(IP)=-1
              TAUX1(IP)=FR(IP,JP,JF)
              TAUX2(IP)=0.D0
            ELSE
!
!             -------------------------------------------------------
!             COMPUTES FNEW AND KNEW
!             -------------------------------------------------------
!
              FNEW = FREQ(JF)+Z
              IF(FNEW.GT.0.D0) THEN
                KNEW(IP)=JP
              ELSE
                KNEW(IP)=1+MOD(JP+NDIRE/2-1,NDIRE)
                FNEW=0.D0
              ENDIF
!
!             -------------------------------------------------------
!             COMPUTES NEWF: INDEX OF THE DISCRETISED FREQUENCY
!             IMMEDIATELY LOWER THAN FNEW
!             -------------------------------------------------------
!
              IF(FNEW.LT.F0/RAISF) THEN
                NEWF(IP)=-1
              ELSE
                NEWF(IP)=INT(1.D0+LOG(FNEW/F0)*UNSLRF)
              ENDIF
!
!             -------------------------------------------------------
!             COMPUTES THE COEFFICIENTS AND INDICES FOR THE PROJECTION
!             -------------------------------------------------------
!
              IF((NEWF(IP).LT.NF).AND.(NEWF(IP).GE.1)) THEN
                NEWF1(IP)=NEWF(IP)+1
                AUXI=FR(IP,JP,JF)*DFREQ(JF)
     &               /(FREQ(NEWF1(IP))-FREQ(NEWF(IP)))
                TAUX1(IP)=AUXI*(FREQ(NEWF1(IP))-FNEW)/DFREQ(NEWF(IP))
                TAUX2(IP)=AUXI*(FNEW-FREQ(NEWF(IP)))/DFREQ(NEWF1(IP))
              ELSEIF (NEWF(IP).EQ.0) THEN
                AUXI=FR(IP,JP,JF)*DFREQ(JF)/(F0*(1.D0-1.D0/RAISF))
                TAUX2(IP)=AUXI*(FNEW-F0/RAISF)/DFREQ(1)
                NEWF (IP)=-1
                NEWF1(IP)= 1
              ELSEIF (NEWF(IP).EQ.NF) THEN
                AUXI=FR(IP,JP,JF)*DFREQ(JF)/(FREQ(NF)*(RAISF-1.D0))
                TAUX1(IP)=AUXI*(FREQ(NF)*RAISF-FNEW)/DFREQ(NF)
                NEWF1(IP)=-1
              ELSE
                NEWF (IP)=-1
                NEWF1(IP)=-1
              ENDIF
!
            ENDIF
!
          ENDDO
!
!
!         -------------------------------------------------------
!         PROJECTS THE SPECTRUM
!         -------------------------------------------------------
!
          DO IP=1,NPOIN2
            NEWM =NEWF (IP)
            NEWM1=NEWF1(IP)
            KH=KNEW(IP)
            IF(NEWM .NE.-1) FA(IP,KH,NEWM )=FA(IP,KH,NEWM )+TAUX1(IP)
            IF(NEWM1.NE.-1) FA(IP,KH,NEWM1)=FA(IP,KH,NEWM1)+TAUX2(IP)
          ENDDO
!
        ENDDO
!
      ENDDO
!
      RETURN
      END
