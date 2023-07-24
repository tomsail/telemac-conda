!                   *****************
                    SUBROUTINE VISCKO
!                   *****************
!
     &(VISCVI,VISCTA,ROTAT,AK,EP,NTRAC,DNUVIH,DNUVIV,DNUTAH,DNUTAV,
     & ITURBH,ITURBV,T1,T2,PRANDTL)
!
!***********************************************************************
! TELEMAC3D   V8P4
!***********************************************************************
!
!brief    COMPUTES THE TURBULENT VISCOSITY
!+                AND TURBULENT THERMAL DIFFUSIVITY
!+                ACCORDING TO K AND EPSILON.
!
!history  HOLGER WEILBEER   ISEB/UHA
!+        25/11/97
!+        V5P3
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  HOLGER WEILBEER   ISEB/UHA
!+        **/02/01
!+
!+   K-OMEGA
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
!| DNUTAH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF TRACER
!| DNUTAV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF TRACER
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EP             |-->| TURBULENT DISSIPATION
!| ITURBH         |-->| HORIZONTAL TURBULENT MODEL
!| ITURBV         |-->| VERTICAL TURBULENT MODEL
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRANDTL        |-->| PRANDTL NUMBER
!| ROTAT          |-->| KIND OF L1 NORM OF VORTICITY
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| VISCTA         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR TRACERS
!| VISCVI         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR VELOCITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_VISCKO => VISCKO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NTRAC,ITURBV,ITURBH
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV,PRANDTL
      DOUBLE PRECISION, INTENT(IN) :: DNUTAH(NTRAC),DNUTAV(NTRAC)
      TYPE(BIEF_OBJ), INTENT(INOUT):: VISCVI, VISCTA,T1,T2
      TYPE(BIEF_OBJ), INTENT(IN)   :: ROTAT, AK, EP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,I,NPOIN3
!
!***********************************************************************
!
      NPOIN3 = AK%DIM1
!
      CALL OS('X=CY    ',X=T1,Y=AK,C=0.3D0)
      CALL OS('X=CY    ',X=T2,Y=EP,C=0.3D0)
      CALL OS('X=+(Y,Z)',X=T2,Y=T2,Z=ROTAT)
!
      IF(ITURBV.EQ.7) CALL OS('X=Y/Z   ',X=VISCVI%ADR(3)%P,Y=T1,Z=T2)
!
!-----------------------------------------------------------------------
!
      IF(ITURBH.EQ.7) THEN
        IF(ITURBV.EQ.7) THEN
          CALL OS('X=Y     ',X=VISCVI%ADR(1)%P,Y=VISCVI%ADR(3)%P)
        ELSE
          CALL OS('X=Y/Z   ',X=VISCVI%ADR(1)%P,Y=T1,Z=T2)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC = 1,NTRAC
!         TURBULENT PRANDTL CLOSURE = 1.0
          IF(ITURBV.EQ.7) THEN
            IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
              CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                           Y=VISCVI%ADR(3)%P,C=DNUTAV(ITRAC))
            ELSE
              DO I=1,NPOIN3
                VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I)=
     &          VISCVI%ADR(3)%P%R(I)/PRANDTL + DNUTAV(ITRAC)
              ENDDO
            ENDIF
          ENDIF
          IF(ITURBH.EQ.7) THEN
            IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
!           HERE PRANDTL TURBULENT = 1.0
              CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                           Y=VISCVI%ADR(1)%P,C=DNUTAH(ITRAC))
              CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                           Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
            ELSE
              DO I=1,NPOIN3
                VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I)=
     &          VISCVI%ADR(1)%P%R(I)/PRANDTL + DNUTAH(ITRAC)
              ENDDO
              CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                           Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FINAL VALUE OF HORIZONTAL AND VERTICAL DIFFUSION FOR VELOCITIES
!
      IF(ITURBH.EQ.7) THEN
        CALL OS('X=X+C   ',X=VISCVI%ADR(1)%P,C=DNUVIH)
        CALL OS('X=Y     ',X=VISCVI%ADR(2)%P,Y=VISCVI%ADR(1)%P)
      ENDIF
      IF(ITURBV.EQ.7) CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
