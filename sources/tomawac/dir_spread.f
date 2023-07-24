!                   *********************
                    SUBROUTINE DIR_SPREAD
!                   *********************
!
     &( DIRSPR, F, NDIRE , NF, NPOIN2)
!
!***********************************************************************
! TOMAWAC   V7P2                                   05/06/2016
!***********************************************************************
!
!brief    COMPUTES THE MEAN DIRECTIONAL SPREAD (=DIRECTIONAL
!+                WIDTH) S IN DEGREES.
!
!history  M. BENOIT
!+        28/12/95
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
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  S.E.BOURBAN (HRW)
!+        05/06/2016
!+        V7P2
!+   Name of the subroutine changed from SPREAD (an intrinsic Fortran
!+   routine) into DIR_SPREAD
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COSMOY         |<--| WORK TABLE
!| DIRSPR         |<--| MEAN DIRECTIONAL SPREAD
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SINMOY         |<--| WORK TABLE
!| TAUXC          |<--| WORK TABLE
!| TAUXE          |<--| WORK TABLE
!| TAUXS          |<--| WORK TABLE
!| VARIAN         |<--| WORK TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,RADDEG, FREQ, DFREQ, TAILF,
     &                                 COSTET, SINTET
!
      USE INTERFACE_TOMAWAC, EX_DIR_SPREAD => DIR_SPREAD
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    :: NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: DIRSPR(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION AUXC  , AUXS  , DFDTET, DTETAR, AUXI
      DOUBLE PRECISION SEUIL , COEFT
      DOUBLE PRECISION SINMOY, COSMOY
      DOUBLE PRECISION VARIAN, TAUXE
      DOUBLE PRECISION TAUXS, TAUXC
!
!
      SEUIL=1.D-20
      DTETAR=DEUPI/DBLE(NDIRE)
!
      DO IP=1,NPOIN2
        COSMOY=0.D0
        SINMOY=0.D0
        VARIAN=0.D0
!-----C-------------------------------------------------------C
!-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
!-----C-------------------------------------------------------C
        DO JF=1,NF
!
          TAUXC=0.D0
          TAUXS=0.D0
          TAUXE=0.D0
          DFDTET=DFREQ(JF)*DTETAR
          DO JP=1,NDIRE
            AUXC=COSTET(JP)*DFDTET
            AUXS=SINTET(JP)*DFDTET
            TAUXC=TAUXC+F(IP,JP,JF)*AUXC
            TAUXS=TAUXS+F(IP,JP,JF)*AUXS
            TAUXE=TAUXE+F(IP,JP,JF)*DFDTET
          ENDDO
          COSMOY=COSMOY+TAUXC
          SINMOY=SINMOY+TAUXS
          VARIAN=VARIAN+TAUXE
        ENDDO                    ! JF
!-----C-------------------------------------------------------------C
!-----C  TAKES INTO ACCOUNT THE HIGH FREQUENCY PART (OPTIONAL)      C
!-----C-------------------------------------------------------------C
        IF (TAILF.GT.1.D0) THEN
          COEFT=FREQ(NF)/((TAILF-1.D0)*DFREQ(NF))
          COSMOY=COSMOY+TAUXC*COEFT
          SINMOY=SINMOY+TAUXS*COEFT
          VARIAN=VARIAN+TAUXE*COEFT
        ENDIF
!-----C-------------------------------------------------------------C
!-----C  COMPUTES THE DIRECTIONAL WIDTH                             C
!-----C-------------------------------------------------------------C
        IF (VARIAN.GT.SEUIL) THEN
          AUXS=SINMOY/VARIAN
          AUXC=COSMOY/VARIAN
          AUXI=MIN(SQRT(AUXS*AUXS+AUXC*AUXC),1.D0)
          DIRSPR(IP)=SQRT(2.D0*(1.D0-AUXI))*RADDEG
        ELSE
          DIRSPR(IP)=SEUIL
        ENDIF
      ENDDO ! IP
!
      RETURN
      END
