!                   ************************
                    SUBROUTINE USER_BED_INIT
!                   ************************
!
     &(NUMSTRAT,NPOIN,NSICLA,ESTRATUM,RATIO_INIT)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     NPOIN      Number of points
!>@param[in]     NSICLA     Number of sediment classes
!>@param[in]     NUMSTRAT   Number of initial physical layers
!>@param[in,out] ESTRATUM   Thickness of initial layers
!>@param[in,out] RATIO_INIT Initial ratio of sediment classes
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: AVA0,SED_THICK,MESH
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,         INTENT(IN)   :: NUMSTRAT,NPOIN,NSICLA
      DOUBLE PRECISION,INTENT(INOUT):: ESTRATUM(NUMSTRAT,NPOIN)
      DOUBLE PRECISION,INTENT(INOUT):: RATIO_INIT(NSICLA,NUMSTRAT,NPOIN)
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: IPOIN,ICLA,ISTRAT
!
! THICKNESS OF SEDIMENT. BY DEFAULT ZR WILL BE 100 METERS BELOW ZF
! TO BE CHANGED BY USER
      SED_THICK = 100.D0
!======================================================================!
!
      DO IPOIN=1,NPOIN
!        USER CAN CHANGE THE THICKNESS OF SEDIMENT HERE
!        (REPLACES SUBROUTINE NOEROD)
!       GRADED SEDIMENT : USER CAN DEFINE AN INTIAL STRTIFICATION
!       DEFINED BY LAYER THICKNESS AND COMPOSITION FOR EACH STRATUM
!
!       POROSITY IS DEFINED (KEYWORD) FOR STRATUMS.
!       THE VALUE FOR THE FIRST STRATUM IS COPIED IN
!       THE FIRST TWO NUMERICAL LAYERS (ACTIVE LAYER + FIRST STRATUM)
          DO ISTRAT=1,NUMSTRAT
!           DEFAULT CASE: ALL STRATUMS HAVE SAME THICKNESS.
!           THIS CAN BE CHANGED BY USER
            ESTRATUM(ISTRAT,IPOIN) = SED_THICK(ISTRAT)
!           DEFAULT CASE: ALL STRATUMS HAVE SAME COMPOSITION.
!           THIS CAN BE CHANGED BY USER
            DO ICLA=1,NSICLA
              RATIO_INIT(ICLA,ISTRAT,IPOIN) = AVA0(ICLA)
            ENDDO
          ENDDO
!
        IF(MESH%X%R(IPOIN).LT.1000.D0) THEN
          IF(MESH%Y%R(IPOIN).GT.0.D0) THEN
!           2 LAYERS, FIRST LAYER IS SMALLER THAN ACTIVE LAYER
            ESTRATUM(1,IPOIN)=0.01D0
            RATIO_INIT(1,1,IPOIN) = 0.25D0
            RATIO_INIT(2,1,IPOIN) = 0.25D0
            RATIO_INIT(3,1,IPOIN) = 0.25D0
            RATIO_INIT(4,1,IPOIN) = 0.25D0
            ESTRATUM(2,IPOIN)=0.2D0
            RATIO_INIT(1,2,IPOIN) = 0.1D0
            RATIO_INIT(2,2,IPOIN) = 0.2D0
            RATIO_INIT(3,2,IPOIN) = 0.3D0
            RATIO_INIT(4,2,IPOIN) = 0.4D0
          ELSE
!           3 LAYERS
            ESTRATUM(1,IPOIN)=0.1D0
            RATIO_INIT(1,1,IPOIN) = 0.1D0
            RATIO_INIT(2,1,IPOIN) = 0.2D0
            RATIO_INIT(3,1,IPOIN) = 0.3D0
            RATIO_INIT(4,1,IPOIN) = 0.4D0
            ESTRATUM(2,IPOIN)=0.5D0
            RATIO_INIT(1,2,IPOIN) = 0.25D0
            RATIO_INIT(2,2,IPOIN) = 0.25D0
            RATIO_INIT(3,2,IPOIN) = 0.25D0
            RATIO_INIT(4,2,IPOIN) = 0.25D0
            ESTRATUM(3,IPOIN)=0.1D0
            RATIO_INIT(1,3,IPOIN) = 0.4D0
            RATIO_INIT(2,3,IPOIN) = 0.3D0
            RATIO_INIT(3,3,IPOIN) = 0.2D0
            RATIO_INIT(4,3,IPOIN) = 0.1D0
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
