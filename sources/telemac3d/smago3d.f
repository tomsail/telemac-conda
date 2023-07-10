!                   ******************
                    SUBROUTINE SMAGO3D
!                   ******************
!
     &(U,V,W,TRAV1,TRAV2,TRAV3,TRAV4,TRAV5,TRAV6,
     & SVIDE,MESH3,IELM3,MSK,MASKEL)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    COMPUTES TURBULENT VISCOSITY USING
!+                3D SMAGORINSKI MODEL:
!code
!+                                            (1/2)
!+    NUSMAGO    =   CS2 * ( 2.0 * SIJ * SIJ )      * (MESH SIZE)**2
!
!history  OLIVER GOETHEL - UNI HANNOVER
!+        17/02/04
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/05/2015
!+        V7P1
!+   Adding a missing coefficient unsv3d**(1/3) in the formula.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM3          |-->| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3          |-->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| SVIDE          |<->| VOID STRUCTURE
!| TRAV1          |<->| WORK ARRAY
!| TRAV2          |<->| WORK ARRAY
!| TRAV3          |<->| WORK ARRAY
!| TRAV4          |<->| WORK ARRAY
!| TRAV5          |<->| WORK ARRAY
!| TRAV6          |<->| WORK ARRAY
!| U              |-->| COMPONENT OF VELOCITY
!| V              |-->| COMPONENT OF VELOCITY
!| W              |-->| COMPONENT OF VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, ONLY : UNSV3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: IELM3
      LOGICAL, INTENT(IN)            :: MSK
      TYPE (BIEF_OBJ), INTENT(IN)    :: U, V, W
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV1, TRAV2, TRAV3, TRAV4,TRAV6
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV5
      TYPE (BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SVIDE
      TYPE (BIEF_MESH)               :: MESH3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION CS,CS2,TIERS
      INTEGER I
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      CS = 0.1D0
      CS2 = CS**2
      TIERS=1.D0/3.D0
!
!-----------------------------------------------------------------------
!
!     COMPUTES GRADIENTS (IN FACT AVERAGED GRADIENT MULTIPLIED BY
!     A VOLUME WHICH IS THE INTEGRAL OF TEST FUNCTIONS ON THE DOMAIN,
!     THIS VOLUME IS CONSIDERED TO BE (MESH SIZE)**3), HENCE THE
!     FACTOR UNSV3D**(1/3) TO GET (MESH SIZE)**2 )
!
      CALL VECTOR(TRAV1,'=','GRADF          X',IELM3,
     &            1.D0,U,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV2,'=','GRADF          Y',IELM3,
     &            1.D0,U,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV3,'=','GRADF          Z',IELM3,
     &            1.D0,U,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
!
      CALL VECTOR(TRAV2,'+','GRADF          X',IELM3,
     &            1.D0,V,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV5,'=','GRADF          Y',IELM3,
     &            1.D0,V,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV6,'=','GRADF          Z',IELM3,
     &            1.D0,V,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
!
      CALL VECTOR(TRAV3,'+','GRADF          X',IELM3,
     &            1.D0,W,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV6,'+','GRADF          Y',IELM3,
     &            1.D0,W,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV4,'=','GRADF          Z',IELM3,
     &            1.D0,W,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH3,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(TRAV1,2,MESH3)
        CALL PARCOM(TRAV2,2,MESH3)
        CALL PARCOM(TRAV3,2,MESH3)
        CALL PARCOM(TRAV4,2,MESH3)
        CALL PARCOM(TRAV5,2,MESH3)
        CALL PARCOM(TRAV6,2,MESH3)
      ENDIF
!
      DO I=1,U%DIM1
!
      TRAV5%R(I)=CS2*SQRT( 2.D0*(TRAV1%R(I)**2
     &                          +TRAV5%R(I)**2
     &                          +TRAV4%R(I)**2)
     &                          +TRAV2%R(I)**2
     &                          +TRAV3%R(I)**2
     &                          +TRAV6%R(I)**2  ) * UNSV3D%R(I)**TIERS
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

