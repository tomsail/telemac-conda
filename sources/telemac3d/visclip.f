!                   ******************
                    SUBROUTINE VISCLIP
!                   ******************
!
     & (VISCVI,VISCTA,H,NPLAN,NPOIN2,NTRAC,HLIM)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    LIMITS VISCOSITY ON TIDAL FLATS.
!
!history  J.-M. HERVOUET
!+        23/03/2004
!+        V5P5
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
!history  J.-M. HERVOUET
!+        11/10/2011
!+        V6P2
!+   HLIM added as argument, now a keyword.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| H              |-->| WATER DEPTH
!| HLIM           |-->| THRESHOLD FOR VISCOSITY CORRECTION ON TIDAL FLATS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| VISCTA         |<->| DYNAMIC VISCOSITY COEFFICIENTS FOR TRACERS
!| VISCVI         |<->| DYNAMIC VISCOSITY COEFFICIENTS FOR VELOCITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            ::  NPOIN2, NPLAN
      INTEGER, INTENT(IN)            :: NTRAC
      DOUBLE PRECISION, INTENT(IN)   :: HLIM
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI, VISCTA
      TYPE (BIEF_OBJ), INTENT(IN)    :: H
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,I,ITRAC
      DOUBLE PRECISION COR
!
!***********************************************************************
!
!     LIMITS TURBULENT VISCOSITY ON TIDAL FLATS
!
      DO I=1,NPOIN2
        IF(H%R(I).LT.HLIM) THEN
          COR=(MAX(H%R(I),0.D0)/HLIM)**2
          DO IPLAN=1,NPLAN
            VISCVI%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)*COR
            VISCVI%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)*COR
            VISCVI%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)*COR
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
          DO I=1,NPOIN2
            IF(H%R(I).LT.HLIM) THEN
              COR=(MAX(H%R(I),0.D0)/HLIM)**2
              DO IPLAN=1,NPLAN
                VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)*COR
                VISCTA%ADR(ITRAC)%P%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)*COR
                VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)*COR
              ENDDO
            ENDIF
          ENDDO
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
