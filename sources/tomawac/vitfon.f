!                   *****************
                    SUBROUTINE VITFON
!                   *****************
!
     &(VIFOND, F, XK , NF    , NPOIN2, NDIRE )
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
!***********************************************************************
!
!brief    COMPUTES THE MAXIMUM ORBITAL VELOCITY NEAR THE BOTTOM
!+               (AVERAGE VELOCITY ON THE SPECTRUM).
!
!history  M. BENOIT
!+        05/07/96
!+        V1P2
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
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEPTH          |-->| WATER DEPTH
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| UWBM           |<--| MAXIMUM ORBITAL VELOCITY NEAR THE BOTTOM
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT, DFREQ, DEPTH
!
      USE INTERFACE_TOMAWAC, EX_VITFON => VITFON
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: VIFOND(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION DTETAR, DEUKD , COEF , BETAA
!
!-----------------------------------------------------------------------
!
      DTETAR=DEUPI/FLOAT(NDIRE)
!
      DO IP = 1,NPOIN2
        VIFOND(IP) = 0.D0
!
!     SUMS UP THE DISCRETISED PART OF THE SPECTRUM
!
        DO JF = 1,NF
          COEF=2.D0*GRAVIT*DFREQ(JF)*DTETAR
          DEUKD = MIN(2.D0*DEPTH(IP)*XK(IP,JF),7.D2)
          BETAA = COEF*XK(IP,JF)/SINH(DEUKD)
          DO JP = 1,NDIRE
            VIFOND(IP) = VIFOND(IP) + F(IP,JP,JF)*BETAA
          ENDDO
        ENDDO
!
        IF (VIFOND(IP).GE.0) THEN
          VIFOND(IP) = SQRT(VIFOND(IP))
        ELSE
          WRITE(*,*) 'VITESSE NEGATIVE'
          CALL PLANTE(0)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
