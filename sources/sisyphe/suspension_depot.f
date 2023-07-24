!                   ***************************
                    SUBROUTINE SUSPENSION_DEPOT
!                   ***************************
!
     &(TOB,HN, NPOIN, HMIN,XWC,VITCD,ZERO,KARMAN,
     & FDM,FD90,XMVE,T1,T2,ZREF,FLUDPT,DEBUG,SEDCO)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!
!brief    COMPUTES THE FLUX OF DEPOSITION AND EROSION.
!
!note     T1: TOB
!note  TO DO:  REPLACE USTAR WITH TOB
!
!history  J-M HERVOUET + C VILLARET
!+        31/07/2008
!+        V5P9
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
!+   Name of variables
!
!history  J-M HERVOUET & P.TASSI (EDF LAB, LNHE)
!+        31/05/2016
!+        V7P2
!+   Cleaner and safer formulas in the IF(SEDCO) case.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| FLUDPT         |<->| IMPLICIT DEPOSITION FLUX
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN CONSTANT
!| NPOIN          |-->| NUMBER OF POINTS
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| TOB            |-->| BED SHEAR STRESS (TOTAL FRICTION)
!| VITCD          |-->| CRITICAL SHEAR VELOCITY FOR MUD DEPOSITION
!| XMVE           |-->| FLUID DENSITY
!| XWC            |-->| SETTLING VELOCITIES
!| ZERO           |-->| ZERO
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE, ONLY : SET_LAG
      USE INTERFACE_SISYPHE,EX_SUSPENSION_DEPOT => SUSPENSION_DEPOT
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (BIEF_OBJ),  INTENT(IN)    ::  HN,TOB
      INTEGER,          INTENT(IN)    ::  NPOIN,DEBUG
      LOGICAL,          INTENT(IN)    :: SEDCO
      DOUBLE PRECISION, INTENT(IN)    ::  HMIN
      DOUBLE PRECISION, INTENT(IN)    :: FDM,FD90,XWC
      DOUBLE PRECISION, INTENT(IN)    :: VITCD
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, KARMAN,XMVE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1,T2
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I
      DOUBLE PRECISION :: AUX
!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!
!     ****************************************************
!     THE TOTAL FRICTION VELOCITY    --> USTAR (T1)
!     HAS BEEN REPLACED BY USTARP (SKIN FRICTION VELOCITY)
!     FOR EROSION FLUX FROM V6P0 ON
!     ****************************************************
!
      CALL OS('X=CY    ', X=T1, Y=TOB, C=1.D0/XMVE)
!     TOB assumed >=0, otherwise mistake elsewhere...
      CALL OS('X=SQR(Y)', X=T1, Y=T1)
!
      IF(SEDCO) THEN
!
!       *********************************************************
!       IA - FORMULATION FOR COHESIVE SEDIMENTS (WITHOUT BEDLOAD)
!       *********************************************************
!
!       COMPUTES THE PROBABILITY FOR DEPOSITION
!
        DO I = 1, NPOIN
!         HERE T1 >=0, so case VITCD=0.D0 excluded by the test
          IF(T1%R(I).LT.VITCD) THEN
            AUX = 1.D0-(T1%R(I)/VITCD)**2
          ELSE
            AUX = 0.D0
          ENDIF
!         COMPUTES THE IMPLICIT PART OF THE DEPOSITION FLUX
          FLUDPT%R(I)= XWC*AUX
        ENDDO
!       UNIFORM SEDIMENT ALONG THE VERTICAL
        CALL CPSTVC(TOB,T2)
        CALL OS('X=C     ', X=T2, C=1.D0)
!
!       **********************************************************
!       IB - FORMULATION FOR NON-COHESIVE SEDIMENTS (WITH BEDLOAD)
!       **********************************************************
!
      ELSE
!
!       *******************************************************
!       COMPUTES THE RATIO BETWEEN NEAR BED CONC. AND MEAN CONC
!                                  -->  T2    (TO KEEP )
!       *******************************************************
!
!       DMK Modification 06/05/2011
        IF(.NOT.(SET_LAG)) THEN
          IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_ROUSE'
          CALL SUSPENSION_ROUSE(T1,HN,NPOIN,
     &                        KARMAN,ZERO,XWC,ZREF,T2)
          IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_ROUSE'
        ELSE
          IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BETAFACTOR'
          CALL SUSPENSION_MILES(HN,NPOIN,HMIN,
     &                  FDM,FD90,XWC,T2)
          IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_BETAFACTOR'
        ENDIF
!       End of DMK mod
!
!       **************************************************
!       COMPUTES THE DEPOSITION FLUX --> FLUDPT = XWC * T2
!       **************************************************
!
        CALL OS('X=CY    ', X=FLUDPT, Y=T2, C=XWC)
!
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
