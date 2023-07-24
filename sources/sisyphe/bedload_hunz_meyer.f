!                   *****************************
                    SUBROUTINE BEDLOAD_HUNZ_MEYER
!                   *****************************
!
     &  (TOB, MU, ACLADM, UNLADM, NPOIN, DENS, XMVE, GRAV, DM, AC,
     &    TETAP, AHUNZI, ACP, HIDING, QSC)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    HUNZIKER BEDLOAD FORMULATION (1995)
!+
!+           (ADAPTED FROM MEYER-PETER FORMULATION).
!
!note     **-1.5 AND **1.5 SHOULD BE OPTIMISED (JMH)
!
!history  BUI MINH DUC
!+        **/01/2002
!+        V5P2
!+
!
!history  C. VILLARET
!+        **/10/2003
!+        V5P4
!+
!
!history
!+
!+        V6P0
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
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| ACP            |<->| MODIFIED SHIELDS PARAMETER
!| AHUNZI         |<->| COEFFICIENT OF HUNZIKER FORMULA
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HIDING         |-->| HIDING FACTOR CORRECTION
!| MU             |<->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| QSC            |<->| BED LOAD TRANSPORT
!| TETAP          |<->| DIMENSIONLESS BED SHEAR STRESS
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| UNLADM         |-->| MEAN DIAMETER OF ACTIVE STRATUM LAYER
!| XMVE           |-->| FLUID DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &          EX_BEDLOAD_HUNZ_MEYER => BEDLOAD_HUNZ_MEYER
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU, ACLADM, UNLADM
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, XMVE, GRAV, DM, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETAP, AHUNZI ! WORK ARRAY T1, T2
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP           ! WORK ARRAY T3
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: HIDING, QSC
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: C1, C2
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ************************************* !
      ! I - ADIMENSIONAL SKIN STRESS          !
      ! ************************************* !
      C1 = 1.D0/(DENS*XMVE*GRAV*DM)
      C2 = 5.D0*SQRT(GRAV*DENS*DM**3)
      CALL OS('X=CYZ   ', X=TETAP, Y=TOB, Z=MU, C=C1)
!
      CALL OS('X=+(Y,C)', X=TETAP , Y=TETAP, C= 1.D-02 )
!
      CALL OS('X=Y**C  ', X=AHUNZI, Y=TETAP, C=-1.5D0  )
      CALL OS('X=CX    ', X=AHUNZI, C= 0.011D0)
      CALL OS('X=X+C   ', X=AHUNZI, C=-0.3D0  )
!
! BEWARE: AHUNZI CAN BECOME SO LARGE THAT THE HIDING FACTOR BECOMES
! INFINITE; HUNZIKER HIMSELF SUGGESTS THAT IT BE CAPPED TO 2.3.
! THIS INITIALLY ADOPTS A LIMIT OF APPROXIMATELY 10.
! (WHICH IS APPARENT IN TETAP BEING SET TO VALUES .GE. 0.01)
!
!     TODO: REMARK BY JMH: I WOULD STRONGLY RECOMMEND A SINGLE LOOP
!                    WITH THE WHOLE FORMULA, INSTEAD OF PILING
!                    UP CALLS TO OS
!
      DO I = 1, NPOIN
        HIDING%R(I) = (DM/ACLADM%R(I))**(-AHUNZI%R(I))
      ENDDO
      ! ************************************************* !
      ! IV - CORRECTS THE ADIMENSIONAL CRITICAL STRESS    !
      ! ************************************************* !
      CALL OS('X=Y/Z   ', X=ACP, Y=UNLADM, Z=ACLADM)
      CALL OS('X=Y**C  ', X=ACP, Y=ACP   , C=0.33D0)
      CALL OS('X=CX    ', X=ACP, C=AC)
      ! ********************* !
      ! V - TRANSPORT RATE    !
      ! ********************* !
      CALL OS('X=Y-Z   ', X=QSC, Y=TETAP , Z=ACP )
      CALL OS('X=+(Y,C)', X=QSC, Y=QSC   , C=0.D0)
      CALL OS('X=XY    ', X=QSC, Y=HIDING)
      CALL OS('X=Y**C  ', X=QSC, Y=QSC   , C=1.5D0)

      CALL OS('X=CX    ', X=QSC, C=C2)
!======================================================================!
!======================================================================!
      RETURN
      END
