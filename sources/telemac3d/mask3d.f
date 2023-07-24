!                   *****************
                    SUBROUTINE MASK3D
!                   *****************
!
     &(IFABOR3D,MASKEL,MASKPT,MASKBR,
     & X,Y,ZF,ZFE,H,HMIN,AT,LT,ITRA01,NELBO3,
     & NELMA2,NELEM2,NPOIN2,NELEB2D,NPLAN,NETAGE,IELM3,MESH2D)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief
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
!history  J-M HERVOUET
!+        13/01/2016
!+        V7P1
!+   Name NELEB2D changed into NELEB2D.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME OF TIME STEP
!| H              |-->| WATER DEPTH
!| HMIN           |-->| ACCEPTABLE MINIMUM VALUE OF DEPTH
!| IELM3          |-->| 3D DISCRETISATION TYPE
!| IFABOR3D       |<->| ARRAY OF ELEMENT ADJACENT TO EDGES (3D)
!| ITRA01         |<->| WORK ARRAY OF INTEGERS
!| LT             |-->| CURRENT TIME STEP NUMBER
!| MASKBR         |<->| 3D MASK ON LATERAL BOUNDARIES
!| MASKEL         |<->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |<->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MESH2D         |<->| 2D MESH
!| NELBO3         |-->| ELEMENTS OF THE BORDER
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELMA2         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NETAGE         |-->| NPLAN - 1
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NELEB2D        |-->| NUMBER OF BOUNDARY ELEMENTS IN 2D
!| X              |-->| COORDINATE
!| Y              |-->| COORDINATE
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFE            |-->| ELEVATION OF BOTTOM PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM2, NPOIN2, NETAGE, NPLAN
      INTEGER, INTENT(IN)             :: NELMA2, NELEB2D
      INTEGER, INTENT(INOUT)          :: IFABOR3D(NELEM2,5,NETAGE)
      DOUBLE PRECISION, INTENT(INOUT) :: MASKEL(NELEM2,NETAGE)
      DOUBLE PRECISION, INTENT(INOUT) :: MASKBR(NELEB2D,NETAGE)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2), Y(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2), H(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZFE(NELEM2)
      INTEGER, INTENT(IN)             :: NELBO3(NELEB2D,NETAGE)
      INTEGER, INTENT(INOUT)          :: ITRA01(NELEM2)
      INTEGER, INTENT(IN)             :: LT, IELM3
      DOUBLE PRECISION, INTENT(IN)    :: HMIN, AT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: MASKPT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM2,IETAGE,IADR,IPLAN
!
!***********************************************************************
!
! INITIALISES MASKEL IF MASK!
!  MASKS TIDAL FLATS (MASKBD)
!  MASKS A USER-DEFINED AREA (MASKOB)
!  FILLS IN MASKPT (MASKTO)
!=======================================================================
!
      CALL OV('X=C     ', X=MASKEL, C=1.D0, DIM1=NELEM2)
!
      CALL MASKBD(MASKEL,ZFE,ZF,H,HMIN,MESH2D%IKLE%I,MESH2D%IFABOR%I,
     &            ITRA01,NELEM2,NPOIN2)
!
      CALL MASKOB(MASKEL,X,Y,MESH2D%IKLE%I,NELEM2,NELMA2,NPOIN2,AT,LT)
!
      CALL MASKTO(MASKEL,MASKPT,IFABOR3D,MESH2D%IKLE%I,
     &            MESH2D%IFABOR%I,MESH2D%ELTSEG%I,MESH2D%NSEG,
     &            NELEM2,IELM3,MESH2D)
!
!=======================================================================
!     COPIES MASKEL ON HIGHER LEVELS
!=======================================================================
!
      IF(NETAGE.GE.2) THEN
        DO IETAGE = 2,NETAGE
          DO IELEM2 = 1,NELEM2
            MASKEL(IELEM2,IETAGE) = MASKEL(IELEM2,1)
          ENDDO
        ENDDO
      ENDIF
!
!=======================================================================
!     COPIES MASKPT ON HIGHER LEVELS
!=======================================================================
!
      DO IPLAN=2,NPLAN
        IADR=(IPLAN-1)*NPOIN2
        CALL OV('X=Y     ', X=MASKPT%R(IADR+1:IADR+NPOIN2),
     &                     Y=MASKPT%R, DIM1=NPOIN2)
      ENDDO
!
!=======================================================================
!     INITIALISES MASKBR
!=======================================================================
!
      CALL OV ('X=C     ', X=MASKBR, C=1.D0, DIM1=NELEB2D*NETAGE)
!
!     AND SETS IT WITH THE NEIGHBOURING MASKEL
!
      CALL OVBD ('X=Y     ',MASKBR,MASKEL,MASKEL,
     &           0.D0,NELBO3,NELEB2D*NETAGE)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
