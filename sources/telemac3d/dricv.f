!                   ****************
                    SUBROUTINE DRICV
!                   ****************
!
     & (FRI,FRT,RI,NPOIN3)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE DAMPING FUNCTION ACCORDING TO THE
!+                RICHARDSON NUMBER FOR VISCOSITIES OBTAINED USING
!+                A MIXING LENGTH MODEL.
!+
!+            HERE MUNK AND ANDERSON MODEL.
!
!reference  JOURNAL OF MARINE RESEARCH VOLUME 1  1948.
!
!history  C. VILLARET (LNHE)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FRI            |<->| DAMPING FUNCTION FOR VELOCITIES
!| FRT            |<->| DAMPING FUNCTION FOR TRACERS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| RI             |-->| RICHARDSON NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: FRI(NPOIN3),FRT(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: RI(NPOIN3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION A,B,EPS
!
!-----------------------------------------------------------------------
!
! DAMPING FUNCTION FOR VELOCITIES: (1+A*RI)**B
!
      A=10.D0
      B=-0.5D0
      EPS=1.D-8
!
      DO I=1,NPOIN3
        IF(RI(I).GT.EPS) THEN
          FRI(I)=(1.D0+A*RI(I))**B
        ELSE
          FRI(I)=1.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
! DAMPING FUNCTION FOR TRACERS: (1+A*RI)**B
!
      A=3.33D0
      B=-1.5D0
      EPS=1.D-8
!
      DO I=1,NPOIN3
        IF(RI(I).GT.EPS) THEN
          FRT(I)=(1.D0+A*RI(I))**B
        ELSE
          FRT(I)=1.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
