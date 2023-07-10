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
      USE DECLARATIONS_GAIA, ONLY: AVA0,SED_THICK
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
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     REMOVE THIS RETURN IF YOU WANT TO ENTER INTO THIS USER SUBROUTINE
      IF (.TRUE.) RETURN
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!======================================================================!
!
!     EXAMPLE 1: DEFAULT CASE: NO STRATIFICATION = ONLY ONE STRATUM 100 M DEEP

      IF (.FALSE.) THEN
!       USER CAN CHANGE THE THICKNESS OF SEDIMENT HERE
!       (REPLACES SUBROUTINE NOEROD)
!       GRADED SEDIMENT: USER CAN DEFINE AN INITIAL STRATIFICATION
!       DEFINED BY LAYER THICKNESS AND COMPOSITION FOR EACH STRATUM
!
!       POROSITY IS DEFINED (KEYWORD) FOR STRATUMS.
!       THE VALUE FOR THE FIRST STRATUM IS COPIED IN
!       THE FIRST TWO NUMERICAL LAYERS (ACTIVE LAYER + FIRST STRATUM)
        DO IPOIN=1,NPOIN
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
        ENDDO
      ENDIF
!
!     EXAMPLE 2: OF HOW A USER COULD DEFINE MANUALY AN INITIAL
!                STRATIFICATION
!
      IF (.FALSE.) THEN
!       NSICLA = 4
!       NUMSTRAT = 2
        DO IPOIN=1,NPOIN
          ESTRATUM(1,IPOIN) = 0.12D0
          RATIO_INIT(1,1,IPOIN) = 0.5D0
          RATIO_INIT(2,1,IPOIN) = 0.5D0
          RATIO_INIT(3,1,IPOIN) = 0.D0
          RATIO_INIT(4,1,IPOIN) = 0.D0
          ESTRATUM(2,IPOIN) = 2.D0
          RATIO_INIT(1,2,IPOIN) = 0.D0
          RATIO_INIT(2,2,IPOIN) = 0.D0
          RATIO_INIT(3,2,IPOIN) = 0.5D0
          RATIO_INIT(4,2,IPOIN) = 0.5D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
