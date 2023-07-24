!                   ****************
                    SUBROUTINE SMAGO
!                   ****************
!
     &(U,V,T1,T2,T3,T4,NUSMAG,MESH3,IELM3,MSK,MASKEL)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    COMPUTES VISCOSITIES USING THE SMAGORINSKY MODEL:
!code
!+                                           (1/2)
!+    NUSMAG    =   CS2 * ( 2.0 * SIJ * SIJ )      * (MESH SIZE)**2
!+
!+                         2        2            2
!+                     (DU)     (DV)   (DU   DV)
!+     2*SIJ*SIJ = ( 2*(--) + 2*(--) + (-- + --)
!+                     (DX)     (DY)   (DY   DX)
!
!history  C. GUILBAUD SOGREAH
!+        03/08/00
!+        V5P1
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
!!history  R. ATA (EDF LAB, LNHE)
!+        30/11/2016
!+        V7P2
!+   Adding missing parcom for parallel case.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM3          |-->| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3          |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NUSMAG         |<->| VISCOSITY COEFFICIENT FOR SMAGORINSKY MODEL
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| T3             |<->| WORK ARRAY
!| T4             |<->| WORK ARRAY
!| U              |-->| COMPONENT OF VELOCITY
!| V              |-->| COMPONENT OF VELOCITY
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
      INTEGER, INTENT(IN)             :: IELM3
      LOGICAL, INTENT(IN)             :: MSK
      TYPE (BIEF_OBJ), INTENT(IN)     :: U,V
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: T1,T2,T3,T4
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: NUSMAG
      TYPE (BIEF_OBJ), INTENT(IN)     :: MASKEL
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH3
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
      CALL VECTOR(T1,'=','GRADF          X',IELM3,
     &            1.D0,U,U,U,U,U,U,MESH3,MSK,MASKEL)
      CALL VECTOR(T2,'=','GRADF          Y',IELM3,
     &            1.D0,U,U,U,U,U,U,MESH3,MSK,MASKEL)
      CALL VECTOR(T3,'=','GRADF          X',IELM3,
     &            1.D0,V,V,V,V,V,V,MESH3,MSK,MASKEL)
      CALL VECTOR(T4,'=','GRADF          Y',IELM3,
     &            1.D0,V,V,V,V,V,V,MESH3,MSK,MASKEL)
!
!     FOR PARALLEL CASES
!
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM(T1,2,MESH3)
        CALL PARCOM(T2,2,MESH3)
        CALL PARCOM(T3,2,MESH3)
        CALL PARCOM(T4,2,MESH3)
      ENDIF
!
      DO I=1,NUSMAG%DIM1
        NUSMAG%R(I)=CS2*
     &  SQRT(2.D0*(T1%R(I)**2+T4%R(I)**2)+(T2%R(I)+T3%R(I))**2)
     &  *UNSV3D%R(I)**TIERS
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

