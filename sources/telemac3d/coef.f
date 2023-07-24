!                   ***************
                    SUBROUTINE COEF
!                   ***************
!
     &     (S3D_IVIDE, S3D_EPAI, TRA01 ,
     &      S3D_NPFMAX, IMAX , NDEB  ,
     &      S3D_RHOS, GRAV , S3D_DTC, DSIG1    )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS RESULTING FROM THE
!+                DISCRETISATION OF THE DIFFERENTIAL EQUATION GOVERNING
!+                THE CONSOLIDATION OF MUDDY BEDS.
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+        V5P1
!+
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DSIG1          |-->| DERIVATIVE OF THE EFFECTIVE STRESS
!|                |   | AT THE FIRST POINT OF THE MESH
!| GRAV           |-->| GRAVITY ACCELERATION
!| IMAX           |-->| NUMBER OF POINTS AT THE BOTTOM MESH
!| NDEB           |-->| INDEX LIMITING THE RANGE OF RESOLUTION
!| S3D_DTC        |-->| TIME STEP FOR CONSOLIDATION
!| S3D_EPAI       |-->| THICKNESS OF MESH ELEMENTS DISCRETISING THE BED
!| S3D_IVIDE      |-->| INDEX OF EMPTY SPACES AT MESH POINTS
!| S3D_NPFMAX     |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUDDY BOTTOM
!| S3D_RHOS       |-->| SEDIMENT DENSITY
!| TRA01          |<->| WORKING ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) ::  S3D_NPFMAX, IMAX,NDEB
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(S3D_NPFMAX,6)
      DOUBLE PRECISION, INTENT(IN) :: S3D_EPAI(S3D_NPFMAX-1)
      DOUBLE PRECISION, INTENT(IN) :: S3D_IVIDE(S3D_NPFMAX)
      DOUBLE PRECISION, INTENT(IN) :: S3D_RHOS, GRAV ,S3D_DTC, DSIG1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION A , D
!
!=======================================================================
!
!      -----INITIALISES THE COEFFICIENTS-----
!
      DO I=NDEB,IMAX
        TRA01(I,3)=0.D0
        TRA01(I,4)=1.D0
        TRA01(I,5)=0.D0
        TRA01(I,6)=0.D0
      END DO
!
!     -----COMPUTES THE COEFFICIENTS AT THE MESH NODES-----
!
!     ...NODE ON THE BOTTOM:
      TRA01(NDEB,5)=-1.D0
      TRA01(NDEB,6)=(S3D_RHOS-1000.D0)*GRAV*S3D_EPAI(1)/DSIG1
!
!     ...INTERIOR NODE:
      DO I=NDEB+1,IMAX-1
!
        IF (TRA01(I,1).GE.1.D-10) THEN
          A=TRA01(I,1)/(S3D_EPAI(I-1))
          D=1.D0
        ELSE
          A=TRA01(I,1)/(S3D_EPAI(I))
          D=0.D0
        ENDIF
!
        TRA01(I,3)=S3D_DTC*(-(TRA01(I,2)+TRA01(I-1,2))/((S3D_EPAI(I)+
     &             S3D_EPAI(I-1))*S3D_EPAI(I-1))-D*A)
        TRA01(I,5)=S3D_DTC*((1.D0-D)*A-((TRA01(I,2)+TRA01(I+1,2))
     &                /((S3D_EPAI(I)+S3D_EPAI(I-1))*S3D_EPAI(I))))
        TRA01(I,4)=1.D0-TRA01(I,3)-TRA01(I,5)
        TRA01(I,6)=S3D_IVIDE(I)
!
      END DO
!
!     ...INTERFACE NODE:
      TRA01(IMAX,6)=S3D_IVIDE(IMAX)
!
      RETURN
      END SUBROUTINE COEF
