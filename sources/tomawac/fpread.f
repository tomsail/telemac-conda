!                   *****************
                    SUBROUTINE FPREAD
!                   *****************
!
     &( FREAD, F, NF, NDIRE, NPOIN2, EXPO)
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE PEAK FREQUENCY OF THE VARIANCE SPECTRUM
!+                USING THE SO-CALLED READ METHOD.
!
!history  M. BENOIT
!+        30/01/96
!+        V1P1
!+   CREATED
!
!history  M. BENOIT
!+        05/07/96
!+        V1P2
!+   MODIFIED
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
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DENOM          |<->| WORK TABLE
!| E              |<->| WORK TABLE
!| EXPO           |-->| EXPONENT OF READ METHOD
!| F              |---| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREAD          |<--| PEAK FREQUENCY (READ METHOD)
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, FREQ, DFREQ, TAILF
!
      USE INTERFACE_TOMAWAC, EX_FPREAD => FPREAD
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER,          INTENT(IN) :: NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN) :: EXPO  , F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FREAD(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION SEUIL , AUXI  , COEFN  , COEFD , DTETAR, DENOM,E
!
!
      SEUIL =1.D-20
      DTETAR=DEUPI/DBLE(NDIRE)
      DO IP = 1,NPOIN2
        FREAD(IP)=0.D0
        DENOM=0.D0
!
!       ------------------------------------------------------C
!       SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM     C
!       ------------------------------------------------------C
        DO JF=1,NF
!
!.......INTEGRATES WRT DIRECTIONS TO GET E(F)
!       """""""""""""""""""""""""""""""""""""""""""""""""
          E = 0.D0
          DO JP=1,NDIRE
            E = E + F(IP,JP,JF)*DTETAR
          ENDDO               ! JP
!
!.......SUMS UP THE CONTRIBUTION OF THE FREQUENCY F
!       """""""""""""""""""""""""""""""""""""""""""
          IF (E.GT.SEUIL) THEN
            AUXI = E**EXPO*DFREQ(JF)
            FREAD(IP) = FREAD(IP)+AUXI*FREQ(JF)
            DENOM = DENOM+AUXI
          ENDIF
!
        ENDDO                  ! JF
!
!-----C-------------------------------------------------------------C
!-----C (OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART     C
!-----C-------------------------------------------------------------C
        IF (TAILF.GT.1.D0) THEN
          COEFN=FREQ(NF)**2/(TAILF*EXPO-2.D0)
          COEFD=FREQ(NF)   /(TAILF*EXPO-1.D0)
          AUXI=E**EXPO
          FREAD(IP) = FREAD(IP)+AUXI*COEFN
          DENOM = DENOM+AUXI*COEFD
        ENDIF
!
!-----C-------------------------------------------------------------C
!-----C COMPUTES THE PEAK FREQUENCY                                 C
!-----C-------------------------------------------------------------C
        IF (DENOM.LT.1.D-90) THEN
          FREAD(IP) = SEUIL
        ELSE
          FREAD(IP) = FREAD(IP)/DENOM
        ENDIF
      ENDDO                     ! IP
!
      RETURN
      END
