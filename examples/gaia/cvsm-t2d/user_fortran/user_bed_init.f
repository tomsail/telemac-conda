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
      USE DECLARATIONS_GAIA, ONLY: AVA0
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
      DOUBLE PRECISION   :: SED_THICK
!
! THICKNESS OF SEDIMENT. BY DEFAULT ZR WILL BE 100 METERS BELOW ZF
! TO BE CHANGED BY USER
      SED_THICK =0.2D0
!
!======================================================================!
!
!     DEFAULT CASE : NO STRATIFICATION = ONLY ONE STRATUM 100 M DEEP


        DO IPOIN=1,NPOIN
!          USER CAN CHANGE THE THICKNESS OF SEDIMENT HERE
!          (REPLACES SUBROUTINE NOEROD)
          IF(NSICLA.EQ.1) THEN ! ONLY ONE SEDIMENT CLASS
            ESTRATUM(1,IPOIN) = SED_THICK
            RATIO_INIT(1,1,IPOIN) = AVA0(1)
!
          ELSE
!         GRADED SEDIMENT : USER CAN DEFINE AN INTIAL STRTIFICATION
!         DEFINED BY LAYER THICKNESS AND COMPOSITION FOR EACH STRATUM
!
!         POROSITY IS DEFINED (KEYWORD) FOR STRATUMS.
!         THE VALUE FOR THE FIRST STRATUM IS COPIED IN
!         THE FIRST TWO NUMERICAL LAYERS (ACTIVE LAYER + FIRST STRATUM)
            DO ISTRAT=1,NUMSTRAT
!
!             DEFAULT CASE: ALL STRATUMS HAVE SAME THICKNESS.
!             THIS CAN BE CHANGED BY USER
              ESTRATUM(ISTRAT,IPOIN) = SED_THICK/NUMSTRAT
!
!             DEFAULT CASE: ALL STRATUMS HAVE SAME COMPOSITION.
!             THIS CAN BE CHANGED BY USER
              DO ICLA=1,NSICLA
                RATIO_INIT(ICLA,ISTRAT,IPOIN) = AVA0(ICLA)
              ENDDO
            ENDDO
!
! EXAMPLE OF HOW A USER COULD DEFINE AN INITIAL STRATIFICATION
!_EXAMPLE            ESTRATUM(1,IPOIN) = 0.12D0
!_EXAMPLE            RATIO_INIT(1,1,IPOIN) = 0.5D0
!_EXAMPLE            RATIO_INIT(2,1,IPOIN) = 0.5D0
!_EXAMPLE            RATIO_INIT(3,1,IPOIN) = 0.D0
!_EXAMPLE            RATIO_INIT(4,1,IPOIN) = 0.D0
!_EXAMPLE            ESTRATUM(2,IPOIN) = 2.D0
!_EXAMPLE            RATIO_INIT(1,2,IPOIN) = 0.D0
!_EXAMPLE            RATIO_INIT(2,2,IPOIN) = 0.D0
!_EXAMPLE            RATIO_INIT(3,2,IPOIN) = 0.5D0
!_EXAMPLE            RATIO_INIT(4,2,IPOIN) = 0.5D0
!
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
