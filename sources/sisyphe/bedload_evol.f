!                   ***********************
                    SUBROUTINE BEDLOAD_EVOL
!                   ***********************
!
     &(S,ELAY,AVA,COEFPN,CALFA,SALFA,LIMTEC,EBOR,
     & MASKEL,MASK,V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,
     & IELMT,KENT,KDIR,KDDL,
     & DTS,
     & VF,ENTET,MSK,MESH,
     & QS,T1,T2,T3,T4,T8,
     & T11,T12,T13,CSF_SABLE,BREACH,QSX,QSY,ZFCL,SLOPEFF,ICLA,
     & FLBCLA,LIQBOR,QBOR,MAXADV)
!
!***********************************************************************
! SISYPHE   V7P0                                    03/06/2014
!***********************************************************************
!
!brief    COMPUTES THE EVOLUTION FOR THE BEDLOAD TRANSPORT.
!
!history  F. HUVELIN
!+        14/09/2004
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
!history  J-M HERVOUET (EDF-LNHE)
!+        27/01/2012
!+        V6P2
!+  Argument ICLA added
!
!history  J-M HERVOUET (EDF-LNHE)
!+        09/01/2013
!+        V6P3
!+  Pointer FLULIM added to avoid a hidden temporary array allocation
!
!history  R.ATA (EDF-LNHE)
!+        02/06/2014
!+        V7P0
!+  Corrections of normals and nubo tables
!+  after changes in FV data structure of Telemac2d
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AVA            |-->| PERCENT AVAILABLE
!| BREACH         |<->| INDICATOR FOR NON ERODIBLE BED (FINITE VOLUMES SHEMES)
!| CALFA          |<->| COSINUS OF THE ANGLE BETWEEN MEAN FLOW AND TRANSPORT
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DTS            |<->| TIME STEP FOR SUSPENSION
!| EBOR           |<->| BOUNDARY CONDITION FOR BED EVOLUTION (DIRICHLET)
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ELAY0          |<->| ACTIVE LAYER THICKNESS
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| FLBCLA         |-->| BLOCK OF FLUXES AT BOUNDARY FOR EACH CLASS
!| ICLA           |-->| CLASS NUMBER
!| IELMT          |-->| NUMBER OF ELEMENTS
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| LIMTEC         |<->| TYPE OF BOUNDARY CONDITION
!| LIQBOR         |-->| TYPE OF BOUNDARY CONDITION FOR BEDLOAD DISCHARGE
!| MASK           |-->| BLOCK OF MASKS, EVERY ONE FOR A TYPE OF BOUNDARY
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MAXADV         |-->| MAXIMUM NUMBER OF ITERATIONS (IN POSITIVE_DEPTHS)
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| QBOR           |-->| PRESCRIBED BEDLOAD DISCHARGE
!| QS             |<->| EDLOAD TRANSPORT RATE
!| QSX            |<->| SOLID DISCHARGE X
!| QSY            |<->| SOLID DISCHARGE Y
!| S              |-->| VOID STRUCTURE
!| SALFA          |<->| SINUS OF THE ANGLE BETWEEN TRANSPORT RATE AND CURRENT
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T11            |<->| WORK BIEF_OBJ STRUCTURE
!| T12            |<->| WORK BIEF_OBJ STRUCTURE
!| T13            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| VF             |-->| LOGICAL, FINITE VOLUMES OR NOT
!| ZFCL           |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_BEDLOAD_EVOL => BEDLOAD_EVOL
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,UNSV2D,ELAY
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFPN,CALFA,SALFA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,MASK,V2DPAR
      INTEGER,          INTENT(IN)    :: DEBUG,SLOPEFF,NPOIN,NPTFR,ICLA
      INTEGER,          INTENT(IN)    :: IELMT,KENT,KDIR,KDDL
      INTEGER,          INTENT(IN)    :: MAXADV
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      DOUBLE PRECISION, INTENT(IN)    :: AVA(NPOIN)
      LOGICAL,          INTENT(IN)    :: VF,ENTET,MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS,EBOR,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8,T11, T12, T13
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, QSX, QSY, ZFCL,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIQBOR,QBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: J
      DOUBLE PRECISION, POINTER :: FLULIM(:)
!
!=======================================================================
!                               PROGRAM
!=======================================================================
!
!     POINTER TO A WORK ARRAY
!
      FLULIM => MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG)
!
!     SLOPE EFFECT
!
      IF(SLOPEFF.EQ.1) CALL OS('X=XY    ', X=QS , Y=COEFPN)
      CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
      CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
!
!     TREATMENT OF NON ERODABLE BOTTOM
!
      IF(VF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BEDLOAD_NERBED_VF'
        CALL BEDLOAD_NERBED_VF
     &        (MESH,LIMTEC,KDDL,ELAY%R,V2DPAR%R,QSX,QSY,AVA,NPOIN,
     &         MESH%NSEG,NPTFR,DTS,QS,T1,T2,T3,BREACH,CSF_SABLE,
     &         MESH%NUBO%I,MESH%VNOIN%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETURN FROM BEDLOAD_NERBED_VF'
        CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
        CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
      ENDIF
!
!     SOLVES THE BED-EVOLUTION EQUATION : F.V.
!
      IF(VF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BEDLOAD_SOLVS_VF'
        CALL BEDLOAD_SOLVS_VF(MESH,QSX,QSY,LIMTEC,UNSV2D,EBOR,
     &                        BREACH,MESH%NSEG,NPTFR,NPOIN,
     &                        KENT,KDIR,KDDL,DTS,ZFCL,T11,
     &                        CSF_SABLE,FLBCLA%ADR(ICLA)%P,AVA,
     &                        LIQBOR,QBOR,MESH%NUBO%I,MESH%VNOIN%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETURN FROM BEDLOAD_SOLVS_VF'
!
!     SOLVES THE BED-EVOLUTION EQUATION : F.E.
!
      ELSE
        DO J=1,NPOIN
!         T13 IS THE SEDIMENT HEIGHT (EXCLUDING VOIDS, SO *CSF_SABLE)
          T13%R(J)=AVA(J)*ELAY%R(J)*CSF_SABLE
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'BEDLOAD_SOLVS_FE'
        CALL BEDLOAD_SOLVS_FE(MESH,S,EBOR,MASKEL,MASK,
     &                        QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,KDDL,
     &                        LIMTEC,DTS,MSK,ENTET,T1,T2,T3,T4,T8,
     &                        ZFCL,T12,T13,MESH%GLOSEG%I,
     &                        MESH%GLOSEG%DIM1,MESH%MSEG%X,
     &                        FLULIM,MESH%NSEG,UNSV2D,CSF_SABLE,ICLA,
     &                        FLBCLA%ADR(ICLA)%P,AVA,LIQBOR,QBOR,
     &                        MAXADV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_BEDLOAD_SOLVS_FE'
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END

