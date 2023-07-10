!                   *********************
                    SUBROUTINE MAKE_ZCONV
!                   *********************
!
     &(ZCONV,GRAZCO,ZFLATS,DH,HN,ZF,TETAZCOMP,TETAH,NELEM2,NELMAX2,
     & OPTBAN,IKLE2,MESH2D)
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    COMPUTES ZCONV AND GRAZCO. THE ADVECTING FIELD WILL BE:
!+        UCONV-GRAV*DT*TETAU*TETAH*GRAD(ZCONV)
!
!history  J-M HERVOUET (LNHE)
!+        23/03/2012
!+        V6P2
!+   First version.
!
!history  J-M HERVOUET (LNHE)
!+        11/09/2017
!+        V7P3
!+   Adding argument NELMAX2 to be used sometimes instead of NELEM2.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GRAZCO         |<->| GRADIENT OF ZCONV
!| HN             |-->| DEPTH AT OLD TIME STEP
!| NELEM2         |-->| NUMBER OF 2D ELEMENTS
!| OPTBAN         |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!| TETAH          |-->| IMPLICITATION ON DEPTH
!| TETAZCOMP      |-->| COMPATIBILITY OF FREE-SURFACE GRADIENT
!| ZCONV          |<->| PIECE-WISE LINEAR FREE SURFACE AT NEW TIME STEP
!|                |   | WHOSE GRADIENT WILL BE A PIECE-WISE CONSTANT
!| ZF             |-->| BOTTOM TOPOGRAPHY
!| ZFLATS         |<->| PIECE-WISE LINEAR FREE SURFACE AT OLD TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NELEM2,OPTBAN,NELMAX2
      INTEGER, INTENT(IN)            :: IKLE2(NELMAX2,3)
      DOUBLE PRECISION, INTENT(IN)   :: TETAH,TETAZCOMP
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: ZCONV,GRAZCO
      TYPE(BIEF_OBJ), INTENT(IN)     :: DH,ZFLATS,HN,ZF
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3
      DOUBLE PRECISION C
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM2
        ZCONV%R(IELEM          )=DH%R(IKLE2(IELEM,1))
        ZCONV%R(IELEM+  NELMAX2)=DH%R(IKLE2(IELEM,2))
        ZCONV%R(IELEM+2*NELMAX2)=DH%R(IKLE2(IELEM,3))
      ENDDO
      IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
        C=(1.D0-TETAZCOMP)/TETAH
        IF(OPTBAN.EQ.1) THEN
!         FREE SURFACE PIECE-WISE LINEAR IN ZFLATS
          CALL OS('X=X+CY  ',X=ZCONV,Y=ZFLATS,C=C)
        ELSE
!         FREE SURFACE LINEAR
          DO IELEM=1,NELEM2
            I1=IKLE2(IELEM,1)
            I2=IKLE2(IELEM,2)
            I3=IKLE2(IELEM,3)
            ZCONV%R(IELEM          )=ZCONV%R(IELEM          )+
     &      C*(HN%R(I1)+ZF%R(I1))
            ZCONV%R(IELEM+  NELMAX2)=ZCONV%R(IELEM+  NELMAX2)+
     &      C*(HN%R(I2)+ZF%R(I2))
            ZCONV%R(IELEM+2*NELMAX2)=ZCONV%R(IELEM+2*NELMAX2)+
     &      C*(HN%R(I3)+ZF%R(I3))
          ENDDO
        ENDIF
      ENDIF
!
!     COMPUTING THE GRADIENT OF ZCONV
!
      CALL GRAD_ZCONV(GRAZCO%R,ZCONV%R,MESH2D%XEL%R,MESH2D%YEL%R,
     &                NELEM2,NELMAX2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
