!                   *****************
                    SUBROUTINE LONGML
!                   *****************
!
     & (LM2,Z,HN,NPOIN3,NPOIN2,NPLAN,MIXING,KARMAN)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES (MIXING LENGTH) ** 2  ACCORDING TO
!+                DIFFERENT MODELS.
!+
!+            SEE : RODI, TURBULENCE MODELS AND THEIR APPLICATIONS
!+                IN HYDRAULICS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN 95 VERSION
!
!history  C. VILLARET (LNHE) ; J.-M. HERVOUET (LNHE) ; C. GUILBAUD (SOGREAH)
!+        25/02/03
!+        V5P4
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
!history  J-M HERVOUET (LNHE)
!+        02/12/2011
!+   Z changed from 'elevation' to 'elevation above bottom'
!+
!
!history  T BENSON (HRW)
!+        30/08/2017
!+   It was possible to get a negative viscosity in very shallow
!+   areas with MIXING LENGTH MODEL=3. Now fixed to be 0.d0 at least.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HN             |-->| WATER DEPTH AT TIME N
!| KARMAN         |-->| KARMAN CONSTANT
!| LM2            |<->| SQUARE MIXING LENGTH
!| MIXING         |-->| MIXING LENGTH MODEL
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS ABOVE BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3,NPOIN2,NPLAN,MIXING
!
      DOUBLE PRECISION, INTENT(INOUT) :: LM2(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN3),HN(NPOIN2),KARMAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IPLAN,I3D
      DOUBLE PRECISION LMM,HH
!
!***********************************************************************
!
      IF(MIXING.EQ.1) THEN
!
!     PRANDTL'S MODEL
!
      DO I=1,NPOIN2
        DO IPLAN=1,NPLAN
          I3D = I + (IPLAN-1)*NPOIN2
          IF(Z(I3D).LE.0.2D0*HN(I)) THEN
            LM2(I3D)=(KARMAN*Z(I3D))**2
          ELSE
            LM2(I3D)=(0.2D0*KARMAN*HN(I))**2
          ENDIF
        ENDDO
      ENDDO
!
      ELSEIF(MIXING.EQ.3) THEN
!
!     NEZU AND NAKAGAWA MODEL
!
        DO I=1,NPOIN2
          DO IPLAN=1,NPLAN
            I3D = I + (IPLAN-1)*NPOIN2
            HH=MAX(HN(I),1.D-6)
            LM2(I3D)=(1.D0-Z(I3D)/HH)*(KARMAN*Z(I3D))**2
            LM2(I3D)=MAX(LM2(I3D),0.D0)
          ENDDO
        ENDDO
!
!     QUETIN MODEL (1977) : EOLE MODEL
!
      ELSEIF(MIXING.EQ.5) THEN
!
        DO I=1,NPOIN2
          DO IPLAN=1,NPLAN
            I3D = I + (IPLAN-1)*NPOIN2
            LMM=1.D0/(KARMAN*Z(I3D)+1.D-7)+
     &          1.D0/(0.65D0*(HN(I)-Z(I3D))+1.D-7)
            LM2(I3D)=(1.D0/LMM)**2
          ENDDO
        ENDDO
!
!     TSANIS MODEL (1989)
!
      ELSEIF(MIXING.EQ.6) THEN
!
        DO I=1,NPOIN2
          DO IPLAN=1,NPLAN
            I3D = I + (IPLAN-1)*NPOIN2
            IF(Z(I3D).LE.0.2D0*HN(I)) THEN
              LM2(I3D)=(KARMAN*Z(I3D))**2
            ELSEIF(Z(I3D).GE.0.8D0*HN(I)) THEN
              LM2(I3D)=(KARMAN*(HN(I)-Z(I3D)))**2
            ELSE
              LM2(I3D)=(0.2D0*KARMAN*HN(I))**2
            ENDIF
          ENDDO
        ENDDO
!
      ELSE
!
        WRITE(LU,12) MIXING
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
12    FORMAT('LONGML: UNEXPECTED PARAMETER MIXING: ',I2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
