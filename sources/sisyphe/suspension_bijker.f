!                   ****************************
                    SUBROUTINE SUSPENSION_BIJKER
!                   ****************************
!
     &(TAUP,NPOIN,CHARR,QSC,ZREF,ZERO,CSTAEQ,XMVE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE REFERENCE CONCENTRATION AT Z= 2*D50
!+                USING ZYSERMAN AND FREDSOE FORMULATION (1994).
!
!history  C. VILLARET
!+        14/04/2004
!+        V5P5
!+
!
!history  F. HUVELIN
!+        04/01/2005
!+        V5P6
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
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHARR          |-->| BEDLOAD
!| CSTAEQ         |<->| EQUILIBRIUM CONCENTRATION
!| NPOIN          |-->| NUMBER OF POINTS
!| QSC            |-->| BED LOAD TRANSPPORT RATE
!| TAUP           |-->| SKIN FROCTION
!| XMVE           |-->| FLUID DENSITY
!| ZERO           |-->| ZERO
!| ZREF           |-->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_BIJKER => SUSPENSION_BIJKER
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TAUP,QSC
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,XMVE
!
      TYPE(BIEF_OBJ),   INTENT(INOUT)   ::  CSTAEQ
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: I
      DOUBLE PRECISION            :: USTARP
!
!     MAXIMUM CONCENTRATION CORRESPONDING TO DENSE PACKING
!
      DOUBLE PRECISION, PARAMETER :: CMAX = 0.6D0
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      IF(.NOT.CHARR) THEN
        WRITE(LU,*) 'SUSPENSION_BIJKER ERROR ON CHARR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,NPOIN
!
        IF(TAUP%R(I).LE.ZERO) THEN
          CSTAEQ%R(I) = 0.D0
        ELSE
          USTARP=SQRT(TAUP%R(I)/XMVE)
          CSTAEQ%R(I) = QSC%R(I)/(6.34D0*USTARP*ZREF%R(I))
          CSTAEQ%R(I) = MIN(CSTAEQ%R(I),CMAX)
        ENDIF
!
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE SUSPENSION_BIJKER
