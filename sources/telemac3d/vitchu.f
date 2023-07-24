!                   *****************
                    SUBROUTINE VITCHU
!                   *****************
!
     &(S3D_WCHU,S3D_WCHU0,U,V,S3D_TURBA,
     & S3D_TURBB,TRAV1,TRAV2,TRAV3,S,MESH3D,IELM3,NPOIN2,NPOIN3,NPLAN,
     & NTRAC,MSK,MASKEL,UETCAR,TA,HN,S3D_FLOC,S3D_FLOC_TYPE,
     & S3D_HINDER,S3D_HIND_TYPE,S3D_CGEL,S3D_CINI)
!
!***********************************************************************
! TELEMAC3D   V7P2                                  31/08/2016
!***********************************************************************
!
!brief    COMPUTES THE SETTLING VELOCITY AS A FUNCTION
!+                OF TEMPERATURE, SALINITY AND CONCENTRATION OF
!+                SUSPENDED SEDIMENT.
!
!history  C LE NORMANT (LNH)
!+        01/08/91
!+        V5P1
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM3          |-->| DISCRETISATION TYPE FOR 3D
!| HN             |-->| WATER DEPTH AT TIME N
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NTRAC          |-->| NUMBER OF TRACERS
!| S              |-->| VOID STRUCTURE
!| S3D_TURBA      |-->| FLOCCULATION COEFFICIENT
!| S3D_TURBB      |-->| COEFFICIENT RELATIVE TO S3D_FLOCDESTRUCTION
!| S3D_WCHU       |<--| SEDIMENT SETTLING VELOCITY (M/S)
!| S3D_WCHU0      |-->| CONSTANT SEDIMENT SETTLING VELOCITY (M/S)
!| SOULSBYWC      |-->| SWITCH FOR SOULSBY FLOCCULATION FORMULA
!| TA             |-->| TRACER CONCENTRATION (LAST ONE, NTRAC, IS SED)
!| TOB            |-->| BED SHEAR STRESS (INCLUDES TURBULENCE DAMPING)
!| TRAV1          |<->| WORK ARRAY
!| TRAV2          |<->| WORK ARRAY
!| TRAV3          |<->| WORK ARRAY
!| U,V            |-->| VELOCITY COMPONENTS
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_VITCHU => VITCHU
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),INTENT(INOUT) :: MESH3D
      TYPE(BIEF_OBJ), INTENT(INOUT) :: S3D_WCHU,TRAV1,TRAV2,TRAV3
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,S,HN,U,V
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA,UETCAR
      LOGICAL, INTENT(IN)           :: MSK,S3D_HINDER
      LOGICAL, INTENT(IN)           :: S3D_FLOC
      INTEGER, INTENT(IN)           :: NPOIN2,NPOIN3,NPLAN,IELM3
      INTEGER, INTENT(IN)           :: NTRAC,S3D_HIND_TYPE
      INTEGER, INTENT(IN)           :: S3D_FLOC_TYPE
      DOUBLE PRECISION, INTENT(IN)  :: S3D_WCHU0,S3D_TURBA,S3D_TURBB
      DOUBLE PRECISION, INTENT(IN)  :: S3D_CGEL
      DOUBLE PRECISION, INTENT(IN)  :: S3D_CINI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     CONSTANT VALUE GIVEN HERE
!
      CALL OS( 'X=C     ' , X=S3D_WCHU, C=S3D_WCHU0)
!
! 1. FLOCCULATION
!
      IF(S3D_FLOC) THEN
        IF(S3D_FLOC_TYPE.EQ.1) THEN
!
!         APPLY REDUCTION DUE TO TURBULENT BREAKUP OF FLOCS
!
          CALL WCTURB(S3D_WCHU,S3D_WCHU0,U,V,
     &                TRAV1,TRAV2,TRAV3, S,MESH3D,IELM3,NPOIN2,
     &                NPLAN,S3D_TURBA,S3D_TURBB,MSK,MASKEL,UETCAR)
!
        ELSEIF(S3D_FLOC_TYPE.EQ.2) THEN
!
!         SOULSBY FLOC MODEL
!
          IF(S3D_HINDER) THEN
            CALL OS('X=-(Y,C)',X=TRAV1,Y=TA%ADR(NTRAC)%P,C=S3D_CINI)
          ELSE
            CALL OS('X=Y     ',X=TRAV1,Y=TA%ADR(NTRAC)%P)
          ENDIF
!
          CALL SOULSBYFLOC3D(S3D_WCHU,TRAV1%R,MESH3D,NPOIN2,
     &                       NPOIN3,NPLAN,HN,UETCAR%R)
!
        ELSE
!
          WRITE(LU,*) 'UNKNOWN FLOCCULATION FORMULA: ',S3D_FLOC_TYPE
          CALL PLANTE(1)
          STOP
!
        ENDIF
      ENDIF
!
! 2. HINDERED SETTLING
!
! LIMIT THE CONCENTRATION TO S3D_CINI(IF HINDERED SETTLING IS ON) (tbe comment: no!
!                                  It only gets limited for the floc model)
!
      IF(S3D_HINDER) THEN
!       we don't limit the concentration here, otherwise hindering won't happen
        !CALL OS('X=-(Y,C)',X=TRAV1,Y=TA%ADR(NTRAC)%P,C=CINI)
        ! this copy of concentration is a bit unecessary.
        ! Would be better to pass
        ! a pointer and use double array in WCHIND
        CALL OS('X=Y     ',X=TRAV1,Y=TA%ADR(NTRAC)%P)
        CALL WCHIND(S3D_WCHU%R,TRAV1,S3D_CINI,S3D_CGEL,NPOIN3,
     &              S3D_HIND_TYPE)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
