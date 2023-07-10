!                   **************************
                    SUBROUTINE BEDLOAD_SOULSBY
!                   **************************
!
     &  (UCMOY,HN, UW, NPOIN, DENS, GRAV, DM, DSTAR, D90, QSC,
     &   QSS)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    SOULSBY & VAN RIJN BEDLOAD TRANSPORT FORMULATION.
!
!history  SOGREAH
!+        22/05/2001
!+        V5P2
!+
!
!history  C.VILLARET
!+        **/11/2003
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| D90            |-->| D90
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| DSTAR          |-->| NON-DIMENSIONAL DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HN             |-->| WATER DEPTH
!| NPOIN          |-->| NUMBER OF POINTS
!| QSC            |<->| BED LOAD TRANSPORT
!| QSS            |<->| SUSPENDED LOAD TRANSPORT RATE
!| UCMOY          |-->| CURRENT INTENSITY (M/S)
!| UW             |-->| ORBITAL WAVE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_SOULSBY => BEDLOAD_SOULSBY
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)  :: HN, UCMOY, UW
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: DENS, GRAV, DM, DSTAR, D90
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: I
      DOUBLE PRECISION            :: COEF, ASS, ASB, CD
      DOUBLE PRECISION            :: UCR, VTOT, TRA
      DOUBLE PRECISION, PARAMETER :: Z0=0.006D0
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ************************* !
      ! I - SUSPENSION COEFFCIENT !
      ! ************************* !
      COEF = (DENS *GRAV*DM)**1.2D0
      ASS  = 0.012D0*DM*(DSTAR**(-0.6D0))/COEF
!
      DO I = 1, NPOIN
!
        ! *************************** !
        ! III - BEDLOAD COEFFICIENT   !
        ! *************************** !
        ASB = 0.005D0*HN%R(I)*(DM/MAX(HN%R(I),DM))**1.2D0 / COEF
!
        ! ********************************** !
        ! IV - ROUGHNESS COEFFICIENT CD      !
        !      SOULSBY: Z0=0.006 --> KS=18CM !
        ! ********************************** !
        CD = (0.4D0 / (LOG(MAX(HN%R(I),Z0)/Z0)-1.D0))**2
!
        ! ************************************************ !
        ! V - CRTITICAL CURRENT SPEED UCR                  !
        ! ************************************************ !
        IF (DM < 0.0005D0) THEN
          UCR = 0.19D0*(DM**0.1D0)*LOG10(4.D0*MAX(HN%R(I),D90)/D90)
        ELSE
          UCR = 8.50D0*(DM**0.6D0)*LOG10(4.D0*MAX(HN%R(I),D90)/D90)
        ENDIF
!
        ! ************************************************* !
        ! VI - SPEED INDUCED BY THE CURRENT AND WAVES       !
        ! ************************************************* !
        VTOT = SQRT(UCMOY%R(I)**2+(0.018D0/CD)*UW%R(I)**2)
!
        ! *********************************************** !
        ! VII - SUSPENDED AND BEDLOAD TRANSPORT           !
        ! *********************************************** !
        IF (VTOT > UCR) THEN
          TRA     = UCMOY%R(I)  * (VTOT - UCR )**2.4D0
          QSS%R(I)= ASS * TRA
          QSC%R(I)= ASB * TRA
        ELSE
          QSS%R(I) = 0.D0
          QSC%R(I) = 0.D0
        ENDIF
      ENDDO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_SOULSBY
