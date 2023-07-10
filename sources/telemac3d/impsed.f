!                   *****************
                    SUBROUTINE IMPSED
!                   *****************
!
     &(S3D_IVIDE, S3D_EPAI, S3D_CONC, S3D_TEMP, S3D_HDEP, PDEPOT,
     & S3D_FLUER, ZR    , ZF    , TA    , WC    , X     ,
     & Y     , NPOIN2, NPOIN3, S3D_NPFMAX, S3D_NCOUCH, NPF   ,
     & LT    , S3D_RHOS, S3D_CFMAX, S3D_CFDEP, S3D_EPAI0,
     & S3D_TASSE, S3D_GIBSON, PRIVE , LISPRD)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GENERATES A RESULT FILE THAT REPRESENTS GRAPHICALLY
!+                THE MUD BED EVOLUTION.
!
!history  C LE NORMANT (LNH)
!+        06/05/93
!+        V5P4
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LISPRD         |-->| TIME STEP FOR GRAPHICAL PRINTOUTS
!| LT             |-->| NUMBER OF TIME STEP
!| NPF            |-->| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| PDEPOT         |-->| PROBABILITY OF DEPOSITION
!| PRIVE          |<->| PRIVATE ARRAYS FOR USERS
!| S3D_CFDEP      |-->| CONCENTRATION (G/L) OF FRESH DEPOSIT
!| S3D_CFMAX      |-->| CONCENTRATION (G/L) OF CONSOLIDATED MUD
!| S3D_CONC       |-->| CONCENTRATION OF MUD BED LAYER
!|                |   | (ONLY FOR MULTILAYER MODEL)
!| S3D_EPAI       |-->| THICKNESS OF SOLID BED LAYER
!| S3D_EPAI0      |-->| REFERENCE THICKNESS
!|                |   | FOR NEW GRID POINTS GENERATION
!| S3D_FLUER      |-->| EROSION FLUX
!| S3D_GIBSON     |-->| LOGICAL FOR S3D_GIBSONMODEL
!| S3D_HDEP       |-->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| S3D_IVIDE      |-->| VOID RATIO
!|                |   | (S3D_GIBSONMODEL ONLY)
!| S3D_NCOUCH     |-->| NUMBER OF BED LAYERS
!|                |   | (MULTILAYER CONSOLIDATION MODEL)
!| S3D_NPFMAX     |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES
!|                |   | WITHIN THE MUD BED (S3D_GIBSONMODEL)
!| S3D_RHOS       |-->| SEDIMENT DENSITY
!| S3D_TASSE      |-->| LOGICAL FOR MULTILAYER CONSOLIDATION MODEL
!| S3D_TEMP       |-->| TIME COUNTER FOR CONSOLIDATION MODEL
!| TA             |-->| SUSPENDED SEDIMENT CONCENTRATION (G/L)
!|                |   | (MULTILAYER MODEL ONLY)
!| WC             |-->| SETTLING VELOCITY (M/S)
!| X              |-->| COORDINATES OF 2D MESH
!| Y              |-->| COORDINATES OF 2D MESH
!| ZF             |-->| BOTTOM LEVEL
!| ZR             |-->| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_IMPSED => IMPSED
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2,NPOIN3,S3D_NPFMAX,S3D_NCOUCH,LT
      INTEGER, INTENT(IN) :: LISPRD
!
      DOUBLE PRECISION, INTENT(IN)  :: S3D_EPAI(S3D_NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: S3D_IVIDE(S3D_NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: S3D_HDEP(NPOIN2) , PDEPOT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: S3D_FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: S3D_CONC(S3D_NCOUCH)
      DOUBLE PRECISION, INTENT(IN)  :: S3D_TEMP(*)
      DOUBLE PRECISION, INTENT(IN)  :: ZR(NPOIN2), ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN3), Y(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: PRIVE
!
      DOUBLE PRECISION, INTENT(IN)  :: S3D_RHOS,S3D_CFMAX,S3D_CFDEP
      DOUBLE PRECISION, INTENT(IN)  :: S3D_EPAI0
!
      INTEGER, INTENT(IN) :: NPF(NPOIN2)
!
      LOGICAL, INTENT(IN) :: S3D_TASSE, S3D_GIBSON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC MOD
!
!-----------------------------------------------------------------------
!
      IF (MOD(LT,LISPRD).NE.0) RETURN
!
      CALL USER_IMPSED
     &(S3D_IVIDE, S3D_EPAI, S3D_CONC, S3D_TEMP, S3D_HDEP, PDEPOT,
     & S3D_FLUER, ZR    , ZF    , TA    , WC    , X     ,
     & Y     , NPOIN2, NPOIN3, S3D_NPFMAX, S3D_NCOUCH, NPF   ,
     & LT    , S3D_RHOS, S3D_CFMAX, S3D_CFDEP, S3D_EPAI0,
     & S3D_TASSE, S3D_GIBSON, PRIVE , LISPRD)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE IMPSED
