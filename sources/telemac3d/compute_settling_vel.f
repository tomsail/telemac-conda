!                   *******************************
                    SUBROUTINE COMPUTE_SETTLING_VEL
!                   *******************************
!
     &(WCHU,U,V,TRAV1,TRAV2,TRAV3,S,MESH3D, IELM3,
     & NPOIN2,NPOIN3,NPLAN,MSK,MASKEL,UETCAR,TA,HN)
!
!***********************************************************************
! TELEMAC3D   V7P2                                  31/08/2016
!***********************************************************************
!
!brief    COMPUTES THE SETTLING VELOCITY AS A FUNCTION
!+        OF TEMPERATURE, SALINITY AND CONCENTRATION OF
!+        SUSPENDED SEDIMENT.
!+        CREATED FROM ALREADY EXISTING SUBROUTINE : VITCHU
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
!| S              |-->| VOID STRUCTURE
!| SOULSBYWC      |-->| SWITCH FOR SOULSBY FLOCCULATION FORMULA
!| TA             |-->| TRACER CONCENTRATION (LAST ONE, NTRAC, IS SED)
!| TOB            |-->| BED SHEAR STRESS (INCLUDES TURBULENCE DAMPING)
!| TRAV1          |<->| WORK ARRAY
!| TRAV2          |<->| WORK ARRAY
!| TRAV3          |<->| WORK ARRAY
!| TURBA          |-->| FLOCCULATION COEFFICIENT
!| TURBB          |-->| COEFFICIENT RELATIVE TO FLOC DESTRUCTION
!| U,V            |-->| VELOCITY COMPONENTS
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY
!| WCHU           |<--| SEDIMENT SETTLING VELOCITY (M/S)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY: IND_SED
      USE DECLARATIONS_GAIA, ONLY:NSUSP_TEL,NUM_ISUSP_ICLA,
     &     FLOC,FLOC_TYPE,HINDER,HIND_TYPE,CGEL,CINI,TURBA,TURBB,
     &     XWC0,TYPE_SED
      USE INTERFACE_TELEMAC3D, EX_COMPUTE_SETTLING_VEL =>
     &                         COMPUTE_SETTLING_VEL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),INTENT(INOUT) :: MESH3D
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WCHU,TRAV1,TRAV2,TRAV3
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,S,HN,U,V
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA,UETCAR
      LOGICAL, INTENT(IN)           :: MSK
      INTEGER, INTENT(IN)           :: NPOIN2,NPOIN3,NPLAN,IELM3
!
      INTEGER ITRAC,ISUSP,IPLAN,IPOIN,IPOIN3
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     CONSTANT VALUE GIVEN HERE
!
      DO ITRAC=IND_SED,IND_SED+NSUSP_TEL-1
        ISUSP=ITRAC-IND_SED+1
        DO IPOIN=1,NPOIN2
          DO IPLAN=1,NPLAN
            IPOIN3=(IPLAN-1)*NPOIN2+IPOIN
            WCHU%ADR(ITRAC)%P%R(IPOIN3) = XWC0(NUM_ISUSP_ICLA(ISUSP))
          ENDDO
        ENDDO
      ENDDO
!
! 1. FLOCCULATION
!
      DO ITRAC=IND_SED,IND_SED+NSUSP_TEL-1
        ISUSP=ITRAC-IND_SED+1
        IF(TYPE_SED(NUM_ISUSP_ICLA(ISUSP)).EQ.'CO') THEN
          IF(FLOC) THEN
            IF(FLOC_TYPE.EQ.1) THEN
!
!         APPLY REDUCTION DUE TO TURBULENT BREAKUP OF FLOCS
!
              CALL WCTURB(WCHU%ADR(ITRAC)%P,
     &              XWC0(NUM_ISUSP_ICLA(ISUSP)),U,V,
     &              TRAV1,TRAV2,TRAV3, S,MESH3D,IELM3,
     &              NPOIN2,NPLAN,TURBA,TURBB,MSK,MASKEL,UETCAR)
!
            ELSEIF(FLOC_TYPE.EQ.2) THEN
!
!           SOULSBY FLOC MODEL
!
              IF(HINDER) THEN
                CALL OS('X=-(Y,C)',X=TRAV1,Y=TA%ADR(ITRAC)%P,C=CINI)
              ELSE
                CALL OS('X=Y     ',X=TRAV1,Y=TA%ADR(ITRAC)%P)
              ENDIF
!
              CALL SOULSBYFLOC3D(WCHU%ADR(ITRAC)%P,TRAV1%R,MESH3D,
     &                        NPOIN2,NPOIN3,NPLAN,HN,UETCAR%R)
!
            ELSE
!
              WRITE(LU,*) 'UNKNOWN FLOCCULATION FORMULA: ',FLOC_TYPE
              CALL PLANTE(1)
              STOP
!
            ENDIF
          ENDIF

!
! 2. HINDERED SETTLING
!
! LIMIT THE CONCENTRATION TO CINI (IF HINDERED SETTLING IS ON) (tbe comment: no!
!                                  It only gets limited for the floc model)
!
          IF(HINDER) THEN
!         we don't limit the concentration here, otherwise hindering won't happen
!           CALL OS('X=-(Y,C)',X=TRAV1,Y=TA%ADR(ITRAC)%P,C=CINI)
!           this copy of concentration is a bit unecessary.
!           Would be better to pass
!           a pointer and use double array in WCHIND
            CALL OS('X=Y     ',X=TRAV1,Y=TA%ADR(ITRAC)%P)
            CALL WCHIND(WCHU%ADR(ITRAC)%P%R,TRAV1,CINI,CGEL,NPOIN3,
     &               HIND_TYPE)
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
