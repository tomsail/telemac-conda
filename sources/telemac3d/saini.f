!                   *****************
                    SUBROUTINE SAINI
!                   *****************
!
     &(NU,U,V,W,Z,NPOIN2,NPLAN,KARMAN,NUMIN,
     & UETCAR,H,KFROT,ITURB,STRAIN,MSK,MASKEL,S,MESH3D)
!
!***********************************************************************
! TELEMAC3D   V8P0                                   21/08/2018
!***********************************************************************
!
!brief    INITIALISES SA and DES MODELS
!
!history  A. BOURGOIN
!+        21/08/2018
!+        V8P0
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NU             |<->| TURBULENT ENERGY
!| EMIN           |-->| MINIMUM VALUE FOR EPSILON WHEN CLIPPING
!| EP             |<->| TURBULENT DISSIPATION
!| KARMAN         |-->| KARMAN CONSTANT
!| KMIN           |-->| MINIMUM VALUE FOR K WHEN CLIPPING
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| S              |-->| VOID STRUCTURE
!| STRAIN         |<->| NORM OF THE STRAIN RATE TENSOR
!| U              |-->| COMPONENT OF VELOCITY
!| V              |-->| COMPONENT OF VELOCITY
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY: IPBOT
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN)            :: MSK
      INTEGER, INTENT(IN)            :: NPOIN2,NPLAN,KFROT,ITURB
      DOUBLE PRECISION, INTENT(INOUT):: NU(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: Z(NPOIN2,NPLAN),H(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: UETCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: KARMAN,NUMIN
      TYPE(BIEF_OBJ)  , INTENT(IN   ):: S,MASKEL,U,V,W
      TYPE(BIEF_OBJ)  , INTENT(INOUT):: STRAIN
      TYPE (BIEF_MESH), INTENT(INOUT):: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,IPLAN,IBOT,IP,IELM3
!
      INTRINSIC SQRT, MAX
!
      DOUBLE PRECISION, PARAMETER :: FICTIFEPS = 2.D0
      DOUBLE PRECISION :: HAUT,DISTFOND
!-----------------------------------------------------------------------
!
!     INITIALIZATION OF STRAIN WHICH IS USED IN VISCSA TO RETRIEVE K
!      AND EPS
      IELM3=STRAIN%ELM
      CALL VECTOR(STRAIN,'=','STRAIN          ',IELM3,1.D0,S,
     &            S,S,U,V,W,MESH3D,MSK,MASKEL)
!     INITILIALIZE THE REMAINING VARIABLES
      IF(ITURB.EQ.5) THEN
        IF(KFROT.NE.0) THEN
          DO IPOIN2 = 1,NPOIN2
            HAUT=MAX(H(IPOIN2),1.D-7)
            DO IPLAN = 1,NPLAN
              IP=MAX(IPLAN,2)
              IBOT=MIN(IPBOT%I(IPOIN2)+1,NPLAN-1)
              DISTFOND = (Z(IPOIN2,IP)-Z(IPOIN2,IBOT))
              NU(IPOIN2,IPLAN)=MAX(KARMAN*(1.D0-DISTFOND/HAUT)**2*
     &        SQRT(UETCAR(IPOIN2))*DISTFOND,NUMIN)
            ENDDO
          ENDDO
        ELSE
          DO IPOIN2 = 1,NPOIN2
            DO IPLAN = 1,NPLAN
              NU(IPOIN2,IPLAN)=NUMIN
            ENDDO
          ENDDO
        ENDIF
      ELSEIF(ITURB.EQ.9) THEN
        DO IPOIN2 = 1,NPOIN2
          DO IPLAN = 1,NPLAN
            NU(IPOIN2,IPLAN)=NUMIN
          ENDDO
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END
