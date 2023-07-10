!                   *****************
                    SUBROUTINE FREMOY
!                   *****************
!
     &(FMOY, F, NF, NDIRE, NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE MEAN SPECTRAL FREQUENCY FMOY FOR ALL
!+                THE NODES IN THE 2D MESH. (THIS MEAN FREQUENCY IS
!+                IDENTICAL TO THAT COMPUTED IN FEMEAN - WAM CYCLE 4).
!code
!+                     SOMME(  F(FREQ,TETA) DFREQ DTETA  )
!+             FMOY =  ----------------------------------------
!+                     SOMME(  F(FREQ,TETA)/FREQ DFREQ DTETA  )
!
!note     THE HIGH-FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!+         IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 2.
!
!history  P. THELLIER; M. BENOIT
!+        09/02/95
!+        V1P0
!+   CREATED
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

!history  WA BREUGEM. (IMDC)
!+        06/2022
!+        CHANGED LOOP ORDER TO PREVENT CACHE LOSSES

!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AUX1           |<->| WORK TABLE
!| AUX2           |<->| WORK TABLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FMOY           |<--| MEAN FREQUENCIES F-10
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, FREQ, DFREQ, TAILF,
     &                                 TRA40, TRA41
!
      USE INTERFACE_TOMAWAC, EX_FREMOY => FREMOY
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER,         INTENT(IN) ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION,INTENT(IN) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION,INTENT(INOUT) :: FMOY(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION SEUIL , DTETAR, AUX3  , AUX4
      DOUBLE PRECISION, POINTER :: A1(:), A2(:)
      A1 => TRA40
      A2 => TRA41
      SEUIL = 1.D-20
      DTETAR= DEUPI/DBLE(NDIRE)
      DO IP = 1,NPOIN2
        A1(IP)=0.D0
        A2(IP)=0.D0
      ENDDO
!
!       -------------------------------------------------------C
!        SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM     C
!       -------------------------------------------------------C
      DO JF = 1,NF-1
        AUX3=DTETAR*DFREQ(JF)
        AUX4=AUX3/FREQ(JF)
        DO JP = 1,NDIRE
          DO IP = 1,NPOIN2
            A1(IP) = A1(IP) + F(IP,JP,JF)*AUX3
            A2(IP) = A2(IP) + F(IP,JP,JF)*AUX4
          ENDDO
        ENDDO               ! JP
      ENDDO                  ! JF
!
!       -------------------------------------------------------------C
!        (OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART     C
!       -------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        AUX3=DTETAR*(DFREQ(NF)+FREQ(NF)/(TAILF-1.D0))
        AUX4=DTETAR*(DFREQ(NF)/FREQ(NF)+1.D0/TAILF)
      ELSE
        AUX3=DTETAR*DFREQ(NF)
        AUX4=AUX3/FREQ(NF)
      ENDIF
      DO JP = 1, NDIRE
        DO IP = 1,NPOIN2
          A1(IP) = A1(IP) + F(IP,JP,NF)*AUX3
          A2(IP) = A2(IP) + F(IP,JP,NF)*AUX4
        ENDDO
      ENDDO                  ! JP
!
!       -------------------------------------------------------------C
!        COMPUTES THE MEAN FREQUENCY                                 C
!       -------------------------------------------------------------C
      DO IP = 1,NPOIN2
        IF (A2(IP).LT.SEUIL) THEN
          FMOY(IP) = SEUIL
        ELSE
          FMOY(IP) = A1(IP)/A2(IP)
        ENDIF
      ENDDO ! IP
      RETURN
      END
