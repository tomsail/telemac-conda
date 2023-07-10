!                   *****************
                    SUBROUTINE FSGRAD
!                   *****************
!
     &(GRADZS,ZFLATS,Z,ZF,IELM2H,MESH2D,MSK,MASKEL,UNSV2D,T2_01,
     & NPOIN2,OPTBAN,S)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   31/07/2014
!***********************************************************************
!
!brief    COMPUTES THE FREE SURFACE GRADIENT, TAKING INTO
!+                ACCOUNT THE TREATMENT OF TIDAL FLATS.
!
!history  J-M HERVOUET (LNHE)
!+        28/07/2009
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
!history  J-M HERVOUET (LNHE)
!+        31/07/2014
!+        V7P0
!+   Atmospheric pressure gradient systematically added to free surface
!+   gradient.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GRADZS         |<->| FREE SURFACE GRADIENT (BLOCK OF 2 COMPONENTS)
!| IELM2H         |-->| TYPE OF ELEMENT
!| MASKEL         |-->| ARRAY OF MASKS, PER ELEMENT
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE IS MASKING, MASKEL IS TO BE USED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| S              |-->| EMPTY BIEF_OBJ STRUCTURE
!| T2_01          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASES
!| Z              |-->| LAST PLANE OF THE Z COORDINATES OF THE 3D MESH
!|                |   | (SEE CALLS TO FSGRAD), SO THE FREE SURFACE.
!| ZF             |-->| BOTTOM ELEVATION
!| ZFLATS         |<->| PIECE-WISE LINEAR FREE SURFACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : ATMOS,RHO0,GRAV,HN,TE3,T2_02,
     &                                   NELEM2,SVIDE
      USE METEO_TELEMAC, ONLY: PATMOS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)                  :: IELM2H,NPOIN2,OPTBAN
      DOUBLE PRECISION, TARGET, INTENT(IN) :: Z(NPOIN2)
      LOGICAL, INTENT(IN)                  :: MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)        :: GRADZS,ZFLATS,T2_01
      TYPE(BIEF_OBJ), INTENT(IN)           :: ZF,UNSV2D,S,MASKEL
      TYPE(BIEF_MESH), INTENT(INOUT)       :: MESH2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_T2_01
      DOUBLE PRECISION C
      LOGICAL MSKGRA
      INTEGER I,IELEM,I1,I2,I3,IAD1,IAD2,IAD3,NELMAX2
!
!-----------------------------------------------------------------------
!
      NELMAX2=MESH2D%NELMAX
!
      IF(OPTBAN.EQ.1) THEN
!
!       COMPUTES THE FREE SURFACE GRADIENT AS IN TELEMAC-2D
!
        CALL CRSL11(ZFLATS%R,Z,
     &              ZF%R,MESH2D%IKLE%I,MESH2D%NELEM,NELMAX2)
        CALL VECTOR(GRADZS%ADR(1)%P,'=','GRADF          X',IELM2H,
     &              1.D0,ZFLATS,S,S,S,S,S,MESH2D,MSK,MASKEL)
        CALL VECTOR(GRADZS%ADR(2)%P,'=','GRADF          Y',IELM2H,
     &              1.D0,ZFLATS,S,S,S,S,S,MESH2D,MSK,MASKEL)
!
      ELSE
!
        SAVE_T2_01=>T2_01%R
        T2_01%R   =>Z
!
        CALL CPSTVC(ZF,T2_01)
!       THIS COPY IS REPLACED WITH T2_01%R POINTING TO Z
!       CALL OV('X=Y     ', X=T2_01%R, Y=Z, DIM1=NPOIN2)
        CALL VECTOR(GRADZS%ADR(1)%P,'=','GRADF          X',IELM2H,
     &              1.D0,T2_01,S,S,S,S,S,MESH2D,MSK,MASKEL)
        CALL VECTOR(GRADZS%ADR(2)%P,'=','GRADF          Y',IELM2H,
     &              1.D0,T2_01,S,S,S,S,S,MESH2D,MSK,MASKEL)
!
        T2_01%R=>SAVE_T2_01
!
      ENDIF
!
!     ADDING THE ATMOSPHERIC PRESSURE GRADIENT
!
      IF(ATMOS) THEN
        C=1.D0/(RHO0*GRAV)
!       PRESSURE GRADIENTS WILL BE LOCALLY MASKED
        IF(MSK.OR.OPTBAN.EQ.1) THEN
          MSKGRA = .TRUE.
          IF(OPTBAN.EQ.1) THEN
            CALL OV('X=Y+Z   ',T2_01%R,HN%R,ZF%R,0.D0,NPOIN2)
            CALL DECVRT(TE3,T2_01,ZF,MESH2D)
          ENDIF
          IF(OPTBAN.EQ.1.AND.MSK) THEN
            CALL OV('X=XY    ',TE3%R,MASKEL%R,MASKEL%R,0.D0,NELEM2)
          ENDIF
          IF(MSK.AND.OPTBAN.NE.1) THEN
            CALL OV('X=Y     ',TE3%R,MASKEL%R,MASKEL%R,C,TE3%DIM1)
          ENDIF
        ELSE
          MSKGRA = .FALSE.
        ENDIF
!
!       ATMOSPHERIC PRESSURE GRADIENT ADDED TO GRADZS
!
        CALL VECTOR(T2_01,'=','GRADF          X',IELM2H,
     &              C,PATMOS,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &              MESH2D,MSKGRA,TE3)
        CALL VECTOR(T2_02,'=','GRADF          Y',IELM2H,
     &              C,PATMOS,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &              MESH2D,MSKGRA,TE3)
        DO I=1,NPOIN2
          GRADZS%ADR(1)%P%R(I)=GRADZS%ADR(1)%P%R(I)+T2_01%R(I)
          GRADZS%ADR(2)%P%R(I)=GRADZS%ADR(2)%P%R(I)+T2_02%R(I)
        ENDDO
!
!       ATMOSPHERIC PRESSURE ALSO ADDED TO ZFLATS
!
        IF(OPTBAN.EQ.1) THEN
          DO IELEM=1,NELEM2
            IF(TE3%R(IELEM).GT.0.5D0) THEN
              IAD1=IELEM
              IAD2=IAD1+NELMAX2
              IAD3=IAD2+NELMAX2
              I1=MESH2D%IKLE%I(IAD1)
              I2=MESH2D%IKLE%I(IAD2)
              I3=MESH2D%IKLE%I(IAD3)
              ZFLATS%R(IAD1)=ZFLATS%R(IAD1)+C*PATMOS%R(I1)
              ZFLATS%R(IAD2)=ZFLATS%R(IAD2)+C*PATMOS%R(I2)
              ZFLATS%R(IAD3)=ZFLATS%R(IAD3)+C*PATMOS%R(I3)
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(GRADZS%ADR(1)%P,2,MESH2D)
        CALL PARCOM(GRADZS%ADR(2)%P,2,MESH2D)
      ENDIF
!
!     DIVISION BY INTEGRAL OF 2D BASES
!
      CALL OS('X=XY    ',X=GRADZS%ADR(1)%P,Y=UNSV2D)
      CALL OS('X=XY    ',X=GRADZS%ADR(2)%P,Y=UNSV2D)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

