!                   *****************
                    SUBROUTINE VISCOS
!                   *****************
!
     &(VISCVI,VISCTA,DNUTAV,DNUTAH,DNUVIV,DNUVIH,NTRAC,ITURBH,ITURBV)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    INITIALISES VISCOSITIES.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNH)
!+        14/12/00
!+        V5P1
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/06/2015
!+        V7P1
!+   DNUTAH and DNUTAV are now arrays.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DNUTAH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF TRACER
!| DNUTAV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF TRACER
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| ITURBH         |-->| HORIZONTAL TURBULENCE MODEL (3= K-EPSILON)
!| ITURBV         |-->| VERTICAL TURBULENCE MODEL (3= K-EPSILON)
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| VISCTA         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR TRACERS
!| VISCVI         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR VELOCITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NTRAC
      INTEGER, INTENT(IN)            :: ITURBH,ITURBV
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI,VISCTA
      DOUBLE PRECISION, INTENT(IN)   :: DNUVIH,DNUVIV
      DOUBLE PRECISION, INTENT(IN)   :: DNUTAH(NTRAC),DNUTAV(NTRAC)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC
!
!***********************************************************************
!
!
! VISCVI%ADR(1)%P IS THE X HORIZONTAL VISCOSITY
! VISCVI%ADR(2)%P IS THE Y HORIZONTAL VISCOSITY
! VISCVI%ADR(3)%P IS THE Z (VERTICAL) VISCOSITY
!
! FOR THE TRACERS:
!
! VISCTA%ADR(ITRAC)%P%ADR(1)%P IS THE X HORIZONTAL DIFFUSIVITY FOR THE
!    ACTIVE TRACER NUMBER ITRAC, ETC...
!
      IF(ITURBH.EQ.1) THEN
!
        CALL OS( 'X=C     ',X=VISCVI%ADR(1)%P,C=DNUVIH)
        CALL OS( 'X=C     ',X=VISCVI%ADR(2)%P,C=DNUVIH)
!
        IF(NTRAC.NE.0) THEN
!
          DO ITRAC=1,NTRAC
!
            CALL OS('X=C     ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                         C=DNUTAH(ITRAC))
            CALL OS('X=C     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                         C=DNUTAH(ITRAC))
!
          ENDDO
!
        ENDIF
!
      ENDIF
!
      IF(ITURBV.EQ.1) THEN
!
        CALL OS( 'X=C     ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
        IF(NTRAC.NE.0) THEN
!
          DO ITRAC=1,NTRAC
!
            CALL OS ('X=C     ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                          C=DNUTAV(ITRAC))
!
          ENDDO
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
