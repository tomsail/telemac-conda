!                   *****************
                    SUBROUTINE CSTSA
!                   *****************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY : CMU,C1,C2,SIGMANU,NUMIN,
     &                                   NUMAX,ITURBV,
     &                                   CLIPNU,YAP,
     &                                   PERNORM2,PERPROD,RIMIN,RIMAX,
     &                                   LIMNUF,LIMNUS
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INTEGER,          INTENT(IN )   :: ITURBV
!     DOUBLE PRECISION, INTENT(INOUT) :: KMIN,KMAX,EMIN,EMAX
!     DOUBLE PRECISION, INTENT(INOUT) :: KARMAN,CMU,C1,C2,SIGMAK,SIGMAE
!     DOUBLE PRECISION, INTENT(INOUT) :: VIRT,PRANDTL,SCHMIT
!     DOUBLE PRECISION, INTENT(INOUT) :: ALPHA,BETA,BETAS,OMSTAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!

!
      IF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN
        SIGMANU = 2.D0/3.D0
      ENDIF
!=======================================================================
!
!     SA MODEL
!
!=======================================================================
!
      CMU    = 0.09D0
      C1     = 1.44D0
      C2     = 1.92D0
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS AT BOTTOM AND FREE SURFACE
!
!-----------------------------------------------------------------------
!

!     F : BOTTOM
!     S : FREE SURFACE
!     1 : NEUMANN
!     2 : DIRICHLET (VALUES COMPUTED IN SACL3)
!
      LIMNUF= 1 !2
      LIMNUS= 1
!
!-----------------------------------------------------------------------
!
!     PARAMETERS USED IN SUBROUTINE SOUKEP
!
!-----------------------------------------------------------------------
!
!     LIMITATION OF NU WITH PHYSICAL CRITERIA
!
      CLIPNU    = .TRUE.
      PERNORM2 = 0.5D0
      PERPROD  = 0.1D0
!
!     MIN AND MAX OF RICHARDSON NUMBER
!
      RIMIN=0.D0
      RIMAX=100.D0
!
      NUMIN=1.D-6
      NUMAX=1000.D0

!
!     YAP CORRECTION
!
      YAP=.FALSE.
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
