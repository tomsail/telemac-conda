!                 ********************************
                  SUBROUTINE BEDLOAD_HIDING_FACTOR
!                 ********************************
!
     &(ACLADM, HIDFAC, NPOIN, HIDI, DM, KARIM_HOLLY_YANG, HIDING)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    HIDING FACTOR FOR EACH NODE, SEDIMENT CLASS
!+                AND TIME STEP.
!
!history  B. MINH DUC
!+        **/11/2002
!+        V5P3
!+
!
!history  M. GONZALES DE LINARES
!+        **/**/2002
!+        V5P3
!+
!
!history  F. HUVELIN
!+        14/09/2004
!+        V5P5
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
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| HIDFAC         |-->| HIDING FACTOR FORMULAS
!| HIDI           |-->| HIDING FACTOR FOR PARTICULAR SIZE CLASS (HIDFAC =0)
!| HIDING         |-->| HIDING FACTOR CORRECTION
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_HIDING_FACTOR => BEDLOAD_HIDING_FACTOR
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      !
      !
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      !
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM
      INTEGER,          INTENT(IN)    :: HIDFAC, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI, DM, KARIM_HOLLY_YANG
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      !
      !
      ! 3/ LOCAL VARIABLES
      ! ------------------
      !
      INTEGER          :: J
      DOUBLE PRECISION :: C1, C2
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
! *************************** !
! IA - CONSTANT HIDING FACTOR !
! *************************** !
!
      IF (HIDFAC == 0) THEN
!
        CALL OS('X=C     ', X=HIDING, C=HIDI)
!
! ************************** !
! IB - EGIAZAROFF FORMULATION !
! ************************** !
!
      ELSEIF (HIDFAC == 1) THEN
!
        C1 = LOG10(19.D0)
        C2 = 19.D0*DM
        DO J = 1, NPOIN
          HIDING%R(J) = (C1/LOG10(C2/ACLADM%R(J)))**2
        ENDDO
!
! ********************************** !
! IC - ASHIDA AND MICHIUE FORMULATION !
! ********************************** !
!
      ELSEIF (HIDFAC == 2) THEN
!
        C1 = LOG10(19.D0)
        C2 = 19.D0*DM
        DO J = 1, NPOIN
!
          IF(DM/ACLADM%R(J) >= 0.4D0) THEN
            HIDING%R(J) = (C1 / LOG10(C2/ACLADM%R(J)) )**2
          ELSE
            HIDING%R(J) = 0.85D0*(ACLADM%R(J)/DM)
          ENDIF
!
        ENDDO
!
! ************************************* !
! IE - KARIM, HOLLY AND YANG FORMULATION !
! ************************************* !
!
      ELSEIF (HIDFAC == 4) THEN
!
        CALL OS('X=1/Y   ', X=HIDING, Y=ACLADM)
        CALL OS('X=CX    ', X=HIDING, C=DM)
        CALL OS('X=Y**C  ', X=HIDING, Y=HIDING, C=KARIM_HOLLY_YANG)
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN HIDING FACTOR FORMULA: ',HIDFAC
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE BEDLOAD_HIDING_FACTOR
