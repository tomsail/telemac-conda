!                   *****************
                    SUBROUTINE VISCKE
!                   *****************
!
     &(VISCVI,VISCTA,AK,EP,NTRAC,CMU,
     & DNUVIH,DNUVIV,DNUTAH,DNUTAV,EMIN,ITURBH,ITURBV,PRANDTL)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    COMPUTES THE TURBULENT VISCOSITY
!+                AND TURBULENT THERMAL DIFFUSIVITY
!+                ACCORDING TO K AND EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/05/2016
!+        V7P2
!+   ITURBH and ITURBV may now be independent.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT ENERGY
!| CMU            |-->| CONSTANT FOR K-EPSILON MODEL
!| DNUTAH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF TRACER
!| DNUTAV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF TRACER
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EMIN           |-->| MINIMUM VALUE OF EPSILON FOR CLIPPING
!| EP             |-->| TURBULENT DISSIPATION
!| ITURBH         |-->| HORIZONTAL TURBULENCE MODEL (3= K-EPSILON)
!| ITURBV         |-->| VERTICAL TURBULENCE MODEL (3= K-EPSILON)
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRANDTL        |-->| PRANDTL CONSTANT
!| VISCTA         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR TRACERS
!| VISCVI         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR VELOCITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NTRAC,ITURBH,ITURBV
      DOUBLE PRECISION, INTENT(IN) :: CMU,PRANDTL
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH,DNUVIV
      DOUBLE PRECISION, INTENT(IN) :: DNUTAH(NTRAC),DNUTAV(NTRAC)
      DOUBLE PRECISION, INTENT(IN) :: EMIN
      TYPE(BIEF_OBJ), INTENT(INOUT):: VISCVI, VISCTA
      TYPE(BIEF_OBJ), INTENT(IN)   :: AK,EP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,I,NPOIN3
!
!***********************************************************************
!
      NPOIN3 = AK%DIM1
!
!     VERTICAL DIFFUSION OF VELOCITIES (WITHOUT MOLECULAR DIFFUSION)
!
      IF(ITURBV.EQ.3) THEN
        DO I=1,NPOIN3
          IF(EP%R(I).GT.1.1D0*EMIN) THEN
            VISCVI%ADR(3)%P%R(I)=CMU*AK%R(I)**2/EP%R(I)
          ELSE
!           IF EPSILON IS NEAR TO CLIP VALUE, NO TURBULENCE
            VISCVI%ADR(3)%P%R(I)=0.D0
          ENDIF
        ENDDO
      ENDIF
!
!     HORIZONTAL DIFFUSION OF VELOCITIES (WITHOUT MOLECULAR DIFFUSION)
!
      IF(ITURBH.EQ.3) THEN
        IF(ITURBV.EQ.3) THEN
          CALL OS('X=Y     ',X=VISCVI%ADR(1)%P,Y=VISCVI%ADR(3)%P)
        ELSE
          DO I=1,NPOIN3
            IF(EP%R(I).GT.1.1D0*EMIN) THEN
              VISCVI%ADR(1)%P%R(I)=CMU*AK%R(I)**2/EP%R(I)
            ELSE
!             IF EPSILON IS NEAR TO CLIP VALUE, NO TURBULENCE
              VISCVI%ADR(1)%P%R(I)=0.D0
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TRACERS, USING THE ALREADY COMPUTED VALUES FOR VELOCITIES
!
      IF(NTRAC.GT.0) THEN
        IF(ITURBV.EQ.3) THEN
          IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
!           HERE PRANDTL TURBULENT = 1.0
            DO ITRAC = 1,NTRAC
              CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                           Y=VISCVI%ADR(3)%P,C=DNUTAV(ITRAC))
            ENDDO
          ELSE
            DO ITRAC = 1,NTRAC
              DO I=1,NPOIN3
                VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I)=
     &          VISCVI%ADR(3)%P%R(I)/PRANDTL + DNUTAV(ITRAC)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
        IF(ITURBH.EQ.3) THEN
          IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
!           HERE PRANDTL TURBULENT = 1.0
            DO ITRAC = 1,NTRAC
              CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                           Y=VISCVI%ADR(1)%P,C=DNUTAH(ITRAC))
              CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                           Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
            ENDDO
          ELSE
            DO ITRAC = 1,NTRAC
              DO I=1,NPOIN3
                VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I)=
     &          VISCVI%ADR(1)%P%R(I)/PRANDTL + DNUTAH(ITRAC)
              ENDDO
              CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                           Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FINAL VALUE OF HORIZONTAL AND VERTICAL DIFFUSION FOR VELOCITIES
!
      IF(ITURBH.EQ.3) THEN
        CALL OS('X=X+C   ',X=VISCVI%ADR(1)%P,C=DNUVIH)
        CALL OS('X=Y     ',X=VISCVI%ADR(2)%P,Y=VISCVI%ADR(1)%P)
      ENDIF
      IF(ITURBV.EQ.3) CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

