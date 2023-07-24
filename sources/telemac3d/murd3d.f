!                   *****************
                    SUBROUTINE MURD3D
!                   *****************
!
     &(SFC,FC,FN,VOLU,VOLUN,VOLU2,SVOLU2,B,DB,XB,DIM1XB,
     & TRA01,TRA02,TRA03,STRA01,STRA02,STRA03,IKLE3,MESH2D,MESH3D,
     & NELEM3,NELMAX,NPOIN3,DT,SCHCF,INFOR,CALFLU,FLUXB,FLUX,
     & FLUEXT,S0F,NSCE,ISCE,KSCE,SOURCES,
     & FSCE,RAIN,PLUIE,PARAPLUIE,TRAIN,NPOIN2,MINFC,MAXFC,MASKPT,
     & OPTBAN,FLODEL,FLOPAR,GLOSEG,DIMGLO,NSEG,NPLAN,IELM3,OPTSOU,
     & NPTFR3,NBOR3,FLUEXTPAR,FBORL,ZN,FI_I,ZSTART,ZEND,FINSUB,
     & T2_01,BEDBOU,BEDFLU,OPTADV,NCO_DIST)
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    ADVECTION OF A VARIABLE WITH THE DISTRIBUTIVE SCHEME
!+                AFTER HAVING COMPUTED THE DISTRIBUTION MATRIX WHICH IS:
!+
!+            - COMMON TO ALL THE VARIABLES (N SCHEME),
!+
!+            - SPECIFIC TO EACH VARIABLE (PSI SCHEME).
!
!warning  FOR THE N SCHEME
!+            MATRIX B MUST BE CONFUSED WITH MATRIX A
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history
!+        28/08/07
!+
!+   PSI SCHEME RE-WRITTEN, NO THEORETICAL CHANGES BUT
!
!history
!+        19/11/07
!+
!+   RAIN HAD BEEN LEFT OUT IN THE PART WITH ALFA
!
!history  J-M HERVOUET (LNHE)
!+        19/12/07
!+
!+   CHANGED MONOTONICITY CRITERION
!
!history  J-M HERVOUET (LNHE)
!+        29/07/08
!+
!+   TIDAL FLATS WITH OPTBAN=2
!
!history  J-M HERVOUET (LNHE)
!+        04/08/08
!+
!+   DIMENSIONS OF XA AND XB INVERTED (SEE ALSO MT14PP)
!
!history  J-M HERVOUET (LNHE)
!+        22/06/09
!+
!+   FINITE VOLUME SCHEME ADDED
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
!+
!history  J-M HERVOUET (LNHE)
!+        04/01/2012
!+        V6P2
!+   Adaptation to tetrahedra, PSI scheme optimised
!
!history  J-M HERVOUET (LNHE)
!+        23/04/2012
!+        V6P2
!+   Values of tracers in rain taken into account.
!
!history  J-M HERVOUET (LNHE)
!+        27/07/2015
!+        V7P1
!+   Guilty point in case of maximum iterations is now printed with its
!+   global number in parallel.
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        28/08/2015
!+        V7P1
!+   Add the option OPTSOU to treat sources as a dirac (OPTSOU=2) or
!+   not (OPTSOU=1).
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/03/2016
!+        V7P2
!+   New predictor-corrector schemes from Sara Pavan PhD.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/08/2016
!+        V7P2
!+   Correction in parallelism in the case of bed fluxes.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/09/2016
!+        V7P2
!+   Two errors corrected:
!+   1) In formulas for boundary fluxes, sources and rain, one must not
!+   mix the next value being built: FC, and the value at the
!+   beginning of the sub-iteration, which is here called FINSUB.
!+   In previous versions there was only FC, causing an error when more
!+   than 1 type of source or sink was considered.
!+   2) With sources, in the last IF(OPTSOU.EQ.1).. ELSEIF(OPTSOU.EQ.2)..
!+   the actions were swapped.
!+   3) The acceptable extrema in monotony proofs depend on the scheme.
!+   Here they are different between first and second order.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        14/11/2016
!+        V7P2
!+   Other IF(OPTSOU.EQ.1).. ELSEIF(OPTSOU.EQ.2).. with actions swapped.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/09/2017
!+        V7P3
!+   Corrections when NELMAX is different from NELEM3.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/10/2017
!+        V7P3
!+   Treatment of explicit source terms moved at the end, after the
!+   corrector. Otherwise the corrector cancels it.
!+   Correction of the stability criterion of predictor-correctors (due
!+   to error corrected in the previous theory, as done in 2D).
!+   Diagonal of MURD matrix always recomputed (may be different
!+   depending on the scheme option, currently not even assembled in
!+   matriy.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |-->| MATRIX
!| CALFLU         |-->| INDICATE IF FLUX IS CALCULATED FOR BALANCE
!| DB             |<->| NOT SYMMETRIC MURD MATRIX OPTION N
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| DIM1XB         |-->| FIRST DIMENSION OF XB
!| DT             |-->| TIME STEP
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FLODEL         |-->| FLUX BY MESH EDGES
!| FLOPAR         |-->| FLUXES BY SEGMENT, ASSEMBLED IN PARALLEL
!| FLUEXT         |-->| OUTPUT FLUX BY NODE
!| FLUX           |<->| FLUXES TO BE CHANGED
!| FLUXB          |<->| FLUX FOR F FOR BALANCE
!| FN             |-->| VARIABLE AT TIME N
!| FSCE           |-->| SOURCE
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| IELM3          |-->| TYPE OF ELEMENT (41:PRISM, ETC.)
!| IKLE3          |-->| GLOBAL 3D CONNECTIVITY
!| INFOR          |-->| INFORMATIONS FOR SOLVERS
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MAXFC          |<->| MAXIMUM VALUE FOR FC
!| MESH2D         |<->| 2D MESH
!| MESH3D         |<->| 3D MESH
!| MINFC          |<->| MINIMUM VALUE FOR FC
!| NCO_DIST       |-->| NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPTADV         |-->| NUMBER OF SEGMENTS
!|                |   | 1 CLASSICAL EXPLICIT SCHEME
!|                |   | 2 PREDICTOR CORRECTOR
!|                |   | 3 PREDICTOR CORRECTOR SECOND ORDER
!|                |   | 4 LOCALLY IMPLICIT
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| PARAPLUIE      |-->| RAIN IN M/S MULTIPLIED BY V2DPAR
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| S0F            |-->| EXPLICIT SOURCE TERM
!| SCHCF          |-->| ADVECTION SCHEME FOR F
!| SFC            |<->| BIEF STRUCTURE OF FC
!| SOURCES        |-->| SOURCES
!| STRA01         |<->| STRUCTURE OF TRA01
!| STRA02         |<->| STRUCTURE OF TRA02
!| STRA03         |<->| STRUCTURE OF TRA03
!| SVOLU2         |-->| STRUCTURE OF VOLU2
!| TRA01          |<->| WORK ARRAY OF DIMENSION NPOIN3 EQUIVALENT TO
!|                |   | VOLU2 FOR CURRENT FINAL TIME
!| TRA02          |<->| WORK ARRAY
!| TRA03          |<->| WORK ARRAY
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| VOLU           |-->| CONTROL VOLUME AT TIME N+1
!| VOLU2          |<->| LIKE VOLU, BUT ASSEMBLED IN PARALLEL
!| VOLUN          |-->| CONTROL VOLUME AT TIME N
!| XB             |<->| NOT SYMMETRIC MURD MATRIX OPTION N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, ONLY : MAXFRO,NUMLIQ
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: SCHCF,NELEM3,NPOIN3,NPOIN2
      INTEGER, INTENT(IN)             :: IELM3,DIM1XB,OPTSOU,NPTFR3
      INTEGER, INTENT(IN)             :: NELMAX
!                                                     6 OR 4
      INTEGER, INTENT(IN)             :: IKLE3(NELMAX,*),NSCE,OPTBAN
      INTEGER, INTENT(IN)             :: NSEG,NPLAN,DIMGLO,NBOR3(NPTFR3)
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
      INTEGER, INTENT(IN)             :: ISCE(NSCE),KSCE(NSCE)
      INTEGER, INTENT(IN)             :: OPTADV,NCO_DIST
!
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3),FBORL(NPTFR3)
      DOUBLE PRECISION, INTENT(IN)    :: FN(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FLUEXT(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FLUEXTPAR(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3),VOLU(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE),MASKPT(*)
      DOUBLE PRECISION, INTENT(IN)    :: DT,TRAIN
      DOUBLE PRECISION, INTENT(INOUT) :: VOLU2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3),TRA03(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUXB(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX
      DOUBLE PRECISION, INTENT(INOUT) :: FINSUB(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: ZSTART(NPOIN3),ZEND(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: DB(NPOIN3),XB(DIM1XB,NELEM3)
!
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: SVOLU2,MINFC,MAXFC,T2_01,B,SFC
      TYPE(BIEF_OBJ),  INTENT(IN)     :: SOURCES,S0F,PLUIE,PARAPLUIE
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: STRA01,STRA02,STRA03,FI_I
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: BEDFLU
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D,MESH3D
!
!     DIMENSION OF FLODEL AND FLOPAR=NSEG2D*NPLAN+NPOIN2*NETAGE
      DOUBLE PRECISION, INTENT(IN)    :: FLODEL(*),FLOPAR(*)
      DOUBLE PRECISION, INTENT(IN)    :: ZN(NPOIN3)
!
      LOGICAL, INTENT(IN)             :: INFOR,CALFLU,RAIN,BEDBOU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION DTJ,PHIP,PHIM,ALFA,C,ALFA2,DTJALFA,MINEL,MAXEL
      DOUBLE PRECISION M12,M13,M14,M15,M16,M23,M24,M25,M26,M34
      DOUBLE PRECISION M35,M36,M45,M46,M56,T1,T2,T3,T4,T5,T6
      DOUBLE PRECISION F1MF2,F1MF3,F1MF4,F2MF3,F2MF4,F3MF4,ALFALOC
      DOUBLE PRECISION TETA
!
      INTEGER IELEM,IPOIN,NITER,IS,IGUILT,IIS,IPTFR3,ICOR,ILIQ
      INTEGER I1,I2,I3,I4,I5,I6,OPT,ISEG3D,NSEGH,NSEGV
!
      LOGICAL PRECOR
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION, PARAMETER :: EPS = 1.D-6
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
!     CONTROL OF OPTION OPTADV
!
      IF(OPTADV.LT.1.OR.OPTADV.GT.3) THEN
        WRITE(LU,*) 'MURD3D: SCHEME OPTION FOR ADVECTION ',OPTADV,
     &              ' NOT TREATED HERE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     OPTIONS WITH AN EXPLICIT PREDICTOR-CORRECTOR
!
      IF((SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI).AND.
     &   (OPTADV.EQ.2.OR.OPTADV.EQ.3)) THEN
        PRECOR=.TRUE.
      ELSE
        PRECOR=.FALSE.
      ENDIF
!
!     SELECTING THE IMPLICITATION COEFFICIENT
!
      IF((SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI).AND.OPTADV.EQ.3) THEN
!       2ND ORDER PREDICTOR CORRECTOR SCHEME
        TETA=0.5D0
      ELSE
!       EXPLICIT N OR PSI, EXPLICIT PREDICTOR-CORRECTOR PSI
        TETA=0.D0
      ENDIF
!
!***********************************************************************
!
      CALL CPSTVC(SVOLU2,STRA01)
      CALL CPSTVC(SVOLU2,STRA02)
      CALL CPSTVC(SVOLU2,STRA03)
      CALL CPSTVC(SVOLU2,MINFC)
      CALL CPSTVC(SVOLU2,MAXFC)
!
      NITER = 0
      DTJ = DT
!
      CALL OV('X=Y     ', X=FC, Y=FN, DIM1=NPOIN3)
!
!     TRA01 WILL BE THE CURRENT VOLUME AT THE END OF THE SUB-ITERATION
!
      CALL OV ('X=Y     ', X=TRA01, Y=VOLUN, DIM1=NPOIN3)
      IF(NCSIZE.GT.1) CALL PARCOM(STRA01,2,MESH3D)
!
!     ZEND WILL BE THE FINAL MESH AT THE END OF THE SUB-ITERATION
!     INITIALIZE ZEND TO ZN
!
      IF(PRECOR.AND.NCO_DIST.GT.0) THEN
        DO IPOIN=1,NPOIN3
          ZEND(IPOIN)=ZN(IPOIN)
        ENDDO
      ENDIF
!
!     FINSUB WILL BE THE STARTING VALUE OF EVERY SUB-ITERATION
!     THUS FIRST INITILIASED WITH FN
!
      DO IPOIN=1,NPOIN3
        FINSUB(IPOIN)=FN(IPOIN)
      ENDDO
!
!     VOLU2: VOLUME AT THE END OF THE FULL TIME STEP
!
      CALL OV('X=Y     ', X=VOLU2, Y=VOLU, DIM1=NPOIN3)
      IF(NCSIZE.GT.1) CALL PARCOM(SVOLU2,2,MESH3D)
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
!  BUILDS THE PSI SCHEME FROM THE N SCHEME IF SCHCF=5
!  SEE "HYDRODYNAMICS OF FREE SURFACE FLOWS" PAGE 193
!
      IF(SCHCF.EQ.ADV_PSI) THEN
!
        DO IPOIN=1,NPOIN3
          TRA02(IPOIN)=0.D0
          DB(IPOIN)=0.D0
        ENDDO
!
        IF(IELM3.EQ.41) THEN
!
        DO IELEM = 1,NELEM3
!
          I1 = IKLE3(IELEM,1)
          I2 = IKLE3(IELEM,2)
          I3 = IKLE3(IELEM,3)
          I4 = IKLE3(IELEM,4)
          I5 = IKLE3(IELEM,5)
          I6 = IKLE3(IELEM,6)
!
          T1 = FC(I1)
          T2 = FC(I2)
          T3 = FC(I3)
          T4 = FC(I4)
          T5 = FC(I5)
          T6 = FC(I6)
!
          M12 = (XB(01,IELEM)-XB(16,IELEM)) * (T1-T2)
          M13 = (XB(02,IELEM)-XB(17,IELEM)) * (T1-T3)
          M14 = (XB(03,IELEM)-XB(18,IELEM)) * (T1-T4)
          M15 = (XB(04,IELEM)-XB(19,IELEM)) * (T1-T5)
          M16 = (XB(05,IELEM)-XB(20,IELEM)) * (T1-T6)
          M23 = (XB(06,IELEM)-XB(21,IELEM)) * (T2-T3)
          M24 = (XB(07,IELEM)-XB(22,IELEM)) * (T2-T4)
          M25 = (XB(08,IELEM)-XB(23,IELEM)) * (T2-T5)
          M26 = (XB(09,IELEM)-XB(24,IELEM)) * (T2-T6)
          M34 = (XB(10,IELEM)-XB(25,IELEM)) * (T3-T4)
          M35 = (XB(11,IELEM)-XB(26,IELEM)) * (T3-T5)
          M36 = (XB(12,IELEM)-XB(27,IELEM)) * (T3-T6)
          M45 = (XB(13,IELEM)-XB(28,IELEM)) * (T4-T5)
          M46 = (XB(14,IELEM)-XB(29,IELEM)) * (T4-T6)
          M56 = (XB(15,IELEM)-XB(30,IELEM)) * (T5-T6)
!
          PHIP = MAX( M12,0.D0) + MAX( M13,0.D0) + MAX( M14,0.D0)
     &         + MAX( M15,0.D0) + MAX( M16,0.D0) + MAX( M23,0.D0)
     &         + MAX( M24,0.D0) + MAX( M25,0.D0) + MAX( M26,0.D0)
     &         + MAX( M34,0.D0) + MAX( M35,0.D0) + MAX( M36,0.D0)
     &         + MAX( M45,0.D0) + MAX( M46,0.D0) + MAX( M56,0.D0)
          PHIM = MAX(-M12,0.D0) + MAX(-M13,0.D0) + MAX(-M14,0.D0)
     &         + MAX(-M15,0.D0) + MAX(-M16,0.D0) + MAX(-M23,0.D0)
     &         + MAX(-M24,0.D0) + MAX(-M25,0.D0) + MAX(-M26,0.D0)
     &         + MAX(-M34,0.D0) + MAX(-M35,0.D0) + MAX(-M36,0.D0)
     &         + MAX(-M45,0.D0) + MAX(-M46,0.D0) + MAX(-M56,0.D0)
!
          IF(PHIP.GE.PHIM) THEN
            ALFA = (PHIP - PHIM) / MAX(PHIP,1.D-10)
            IF(T2.GT.T1) THEN
              TRA02(I2)=TRA02(I2)+XB(16,IELEM)*ALFA*(FC(I1)-FC(I2))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(01,IELEM)*ALFA*(FC(I2)-FC(I1))
            ENDIF
            IF(T3.GT.T1) THEN
              TRA02(I3)=TRA02(I3)+XB(17,IELEM)*ALFA*(FC(I1)-FC(I3))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(02,IELEM)*ALFA*(FC(I3)-FC(I1))
            ENDIF
            IF(T4.GT.T1) THEN
              TRA02(I4)=TRA02(I4)+XB(18,IELEM)*ALFA*(FC(I1)-FC(I4))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(03,IELEM)*ALFA*(FC(I4)-FC(I1))
            ENDIF
            IF(T5.GT.T1) THEN
              TRA02(I5)=TRA02(I5)+XB(19,IELEM)*ALFA*(FC(I1)-FC(I5))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(04,IELEM)*ALFA*(FC(I5)-FC(I1))
            ENDIF
            IF(T6.GT.T1) THEN
              TRA02(I6)=TRA02(I6)+XB(20,IELEM)*ALFA*(FC(I1)-FC(I6))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(05,IELEM)*ALFA*(FC(I6)-FC(I1))
            ENDIF
            IF(T3.GT.T2) THEN
              TRA02(I3)=TRA02(I3)+XB(21,IELEM)*ALFA*(FC(I2)-FC(I3))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(06,IELEM)*ALFA*(FC(I3)-FC(I2))
            ENDIF
            IF(T4.GT.T2) THEN
              TRA02(I4)=TRA02(I4)+XB(22,IELEM)*ALFA*(FC(I2)-FC(I4))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(07,IELEM)*ALFA*(FC(I4)-FC(I2))
            ENDIF
            IF(T5.GT.T2) THEN
              TRA02(I5)=TRA02(I5)+XB(23,IELEM)*ALFA*(FC(I2)-FC(I5))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(08,IELEM)*ALFA*(FC(I5)-FC(I2))
            ENDIF
            IF(T6.GT.T2) THEN
              TRA02(I6)=TRA02(I6)+XB(24,IELEM)*ALFA*(FC(I2)-FC(I6))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(09,IELEM)*ALFA*(FC(I6)-FC(I2))
            ENDIF
            IF(T4.GT.T3) THEN
              TRA02(I4)=TRA02(I4)+XB(25,IELEM)*ALFA*(FC(I3)-FC(I4))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(10,IELEM)*ALFA*(FC(I4)-FC(I3))
            ENDIF
            IF(T5.GT.T3) THEN
              TRA02(I5)=TRA02(I5)+XB(26,IELEM)*ALFA*(FC(I3)-FC(I5))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(11,IELEM)*ALFA*(FC(I5)-FC(I3))
            ENDIF
            IF(T6.GT.T3) THEN
              TRA02(I6)=TRA02(I6)+XB(27,IELEM)*ALFA*(FC(I3)-FC(I6))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(12,IELEM)*ALFA*(FC(I6)-FC(I3))
            ENDIF
            IF(T5.GT.T4) THEN
              TRA02(I5)=TRA02(I5)+XB(28,IELEM)*ALFA*(FC(I4)-FC(I5))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(13,IELEM)*ALFA*(FC(I5)-FC(I4))
            ENDIF
            IF(T6.GT.T4) THEN
              TRA02(I6)=TRA02(I6)+XB(29,IELEM)*ALFA*(FC(I4)-FC(I6))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(14,IELEM)*ALFA*(FC(I6)-FC(I4))
            ENDIF
            IF(T6.GT.T5) THEN
              TRA02(I6)=TRA02(I6)+XB(30,IELEM)*ALFA*(FC(I5)-FC(I6))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(15,IELEM)*ALFA*(FC(I6)-FC(I5))
            ENDIF
          ELSE
            ALFA = (PHIM - PHIP) / MAX(PHIM,1.D-10)
            IF(T2.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(01,IELEM)*ALFA*(FC(I2)-FC(I1))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(16,IELEM)*ALFA*(FC(I1)-FC(I2))
            ENDIF
            IF(T3.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(02,IELEM)*ALFA*(FC(I3)-FC(I1))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(17,IELEM)*ALFA*(FC(I1)-FC(I3))
            ENDIF
            IF(T4.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(03,IELEM)*ALFA*(FC(I4)-FC(I1))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(18,IELEM)*ALFA*(FC(I1)-FC(I4))
            ENDIF
            IF(T5.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(04,IELEM)*ALFA*(FC(I5)-FC(I1))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(19,IELEM)*ALFA*(FC(I1)-FC(I5))
            ENDIF
            IF(T6.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(05,IELEM)*ALFA*(FC(I6)-FC(I1))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(20,IELEM)*ALFA*(FC(I1)-FC(I6))
            ENDIF
            IF(T3.GT.T2) THEN
              TRA02(I2)=TRA02(I2)+XB(06,IELEM)*ALFA*(FC(I3)-FC(I2))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(21,IELEM)*ALFA*(FC(I2)-FC(I3))
            ENDIF
            IF(T4.GT.T2) THEN
              TRA02(I2)=TRA02(I2)+XB(07,IELEM)*ALFA*(FC(I4)-FC(I2))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(22,IELEM)*ALFA*(FC(I2)-FC(I4))
            ENDIF
            IF(T5.GT.T2) THEN
              TRA02(I2)=TRA02(I2)+XB(08,IELEM)*ALFA*(FC(I5)-FC(I2))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(23,IELEM)*ALFA*(FC(I2)-FC(I5))
            ENDIF
            IF(T6.GT.T2) THEN
              TRA02(I2)=TRA02(I2)+XB(09,IELEM)*ALFA*(FC(I6)-FC(I2))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(24,IELEM)*ALFA*(FC(I2)-FC(I6))
            ENDIF
            IF(T4.GT.T3) THEN
              TRA02(I3)=TRA02(I3)+XB(10,IELEM)*ALFA*(FC(I4)-FC(I3))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(25,IELEM)*ALFA*(FC(I3)-FC(I4))
            ENDIF
            IF(T5.GT.T3) THEN
              TRA02(I3)=TRA02(I3)+XB(11,IELEM)*ALFA*(FC(I5)-FC(I3))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(26,IELEM)*ALFA*(FC(I3)-FC(I5))
            ENDIF
            IF(T6.GT.T3) THEN
              TRA02(I3)=TRA02(I3)+XB(12,IELEM)*ALFA*(FC(I6)-FC(I3))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(27,IELEM)*ALFA*(FC(I3)-FC(I6))
            ENDIF
            IF(T5.GT.T4) THEN
              TRA02(I4)=TRA02(I4)+XB(13,IELEM)*ALFA*(FC(I5)-FC(I4))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(28,IELEM)*ALFA*(FC(I4)-FC(I5))
            ENDIF
            IF(T6.GT.T4) THEN
              TRA02(I4)=TRA02(I4)+XB(14,IELEM)*ALFA*(FC(I6)-FC(I4))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(29,IELEM)*ALFA*(FC(I4)-FC(I6))
            ENDIF
            IF(T6.GT.T5) THEN
              TRA02(I5)=TRA02(I5)+XB(15,IELEM)*ALFA*(FC(I6)-FC(I5))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(30,IELEM)*ALFA*(FC(I5)-FC(I6))
            ENDIF
          ENDIF
        ENDDO
!
        IF(PRECOR) THEN
          DO IELEM = 1,NELEM3
            I1 = IKLE3(IELEM,1)
            I2 = IKLE3(IELEM,2)
            I3 = IKLE3(IELEM,3)
            I4 = IKLE3(IELEM,4)
            I5 = IKLE3(IELEM,5)
            I6 = IKLE3(IELEM,6)
!           DB COMPUTED WITH LAMBDA N AND NOT PSI!!
            DB(I1)=DB(I1)-XB(01,IELEM)-XB(02,IELEM)-XB(03,IELEM)
     &                   -XB(04,IELEM)-XB(05,IELEM)
     &                   -XB(16,IELEM)-XB(17,IELEM)-XB(18,IELEM)
     &                   -XB(19,IELEM)-XB(20,IELEM)
            DB(I2)=DB(I2)-XB(16,IELEM)-XB(06,IELEM)-XB(07,IELEM)
     &                   -XB(08,IELEM)-XB(09,IELEM)
     &                   -XB(01,IELEM)-XB(21,IELEM)-XB(22,IELEM)
     &                   -XB(23,IELEM)-XB(24,IELEM)
            DB(I3)=DB(I3)-XB(17,IELEM)-XB(21,IELEM)-XB(10,IELEM)
     &                   -XB(11,IELEM)-XB(12,IELEM)
     &                   -XB(02,IELEM)-XB(06,IELEM)-XB(25,IELEM)
     &                   -XB(26,IELEM)-XB(27,IELEM)
            DB(I4)=DB(I4)-XB(18,IELEM)-XB(22,IELEM)-XB(25,IELEM)
     &                   -XB(13,IELEM)-XB(14,IELEM)
     &                   -XB(03,IELEM)-XB(07,IELEM)-XB(10,IELEM)
     &                   -XB(28,IELEM)-XB(29,IELEM)
            DB(I5)=DB(I5)-XB(19,IELEM)-XB(23,IELEM)-XB(26,IELEM)
     &                   -XB(28,IELEM)-XB(15,IELEM)
     &                   -XB(04,IELEM)-XB(08,IELEM)-XB(11,IELEM)
     &                   -XB(13,IELEM)-XB(30,IELEM)
            DB(I6)=DB(I6)-XB(20,IELEM)-XB(24,IELEM)-XB(27,IELEM)
     &                   -XB(29,IELEM)-XB(30,IELEM)
     &                   -XB(05,IELEM)-XB(09,IELEM)-XB(12,IELEM)
     &                   -XB(14,IELEM)-XB(15,IELEM)
          ENDDO
        ELSE
          DO IELEM = 1,NELEM3
            I1 = IKLE3(IELEM,1)
            I2 = IKLE3(IELEM,2)
            I3 = IKLE3(IELEM,3)
            I4 = IKLE3(IELEM,4)
            I5 = IKLE3(IELEM,5)
            I6 = IKLE3(IELEM,6)
!           DB COMPUTED WITH LAMBDA N AND NOT PSI!!
            DB(I1)=DB(I1)-XB(01,IELEM)-XB(02,IELEM)-XB(03,IELEM)
     &                   -XB(04,IELEM)-XB(05,IELEM)
            DB(I2)=DB(I2)-XB(16,IELEM)-XB(06,IELEM)-XB(07,IELEM)
     &                   -XB(08,IELEM)-XB(09,IELEM)
            DB(I3)=DB(I3)-XB(17,IELEM)-XB(21,IELEM)-XB(10,IELEM)
     &                   -XB(11,IELEM)-XB(12,IELEM)
            DB(I4)=DB(I4)-XB(18,IELEM)-XB(22,IELEM)-XB(25,IELEM)
     &                   -XB(13,IELEM)-XB(14,IELEM)
            DB(I5)=DB(I5)-XB(19,IELEM)-XB(23,IELEM)-XB(26,IELEM)
     &                   -XB(28,IELEM)-XB(15,IELEM)
            DB(I6)=DB(I6)-XB(20,IELEM)-XB(24,IELEM)-XB(27,IELEM)
     &                   -XB(29,IELEM)-XB(30,IELEM)
          ENDDO
        ENDIF
!
        ELSEIF(IELM3.EQ.51) THEN
!
        DO IELEM = 1,NELEM3
!
          I1 = IKLE3(IELEM,1)
          I2 = IKLE3(IELEM,2)
          I3 = IKLE3(IELEM,3)
          I4 = IKLE3(IELEM,4)
!
          F1MF2 = FC(I1)-FC(I2)
          F1MF3 = FC(I1)-FC(I3)
          F1MF4 = FC(I1)-FC(I4)
          F2MF3 = FC(I2)-FC(I3)
          F2MF4 = FC(I2)-FC(I4)
          F3MF4 = FC(I3)-FC(I4)
!
          M12 = (XB(01,IELEM)-XB(07,IELEM)) * F1MF2
          M13 = (XB(02,IELEM)-XB(08,IELEM)) * F1MF3
          M14 = (XB(03,IELEM)-XB(09,IELEM)) * F1MF4
          M23 = (XB(04,IELEM)-XB(10,IELEM)) * F2MF3
          M24 = (XB(05,IELEM)-XB(11,IELEM)) * F2MF4
          M34 = (XB(06,IELEM)-XB(12,IELEM)) * F3MF4
!
          PHIP = MAX( M12,0.D0) + MAX( M13,0.D0) + MAX( M14,0.D0)
     &         + MAX( M23,0.D0) + MAX( M24,0.D0) + MAX( M34,0.D0)
          PHIM = MAX(-M12,0.D0) + MAX(-M13,0.D0) + MAX(-M14,0.D0)
     &         + MAX(-M23,0.D0) + MAX(-M24,0.D0) + MAX(-M34,0.D0)
!
          IF(PHIP.GE.PHIM) THEN
            ALFA = (PHIP - PHIM) / MAX(PHIP,1.D-10)
            IF(F1MF2.LT.0.D0) THEN
              TRA02(I2)=TRA02(I2)+XB(07,IELEM)*ALFA*F1MF2
            ELSE
              TRA02(I1)=TRA02(I1)-XB(01,IELEM)*ALFA*F1MF2
            ENDIF
            IF(F1MF3.LT.0.D0) THEN
              TRA02(I3)=TRA02(I3)+XB(08,IELEM)*ALFA*F1MF3
            ELSE
              TRA02(I1)=TRA02(I1)-XB(02,IELEM)*ALFA*F1MF3
            ENDIF
            IF(F1MF4.LT.0.D0) THEN
              TRA02(I4)=TRA02(I4)+XB(09,IELEM)*ALFA*F1MF4
            ELSE
              TRA02(I1)=TRA02(I1)-XB(03,IELEM)*ALFA*F1MF4
            ENDIF
            IF(F2MF3.LT.0.D0) THEN
              TRA02(I3)=TRA02(I3)+XB(10,IELEM)*ALFA*F2MF3
            ELSE
              TRA02(I2)=TRA02(I2)-XB(04,IELEM)*ALFA*F2MF3
            ENDIF
            IF(F2MF4.LT.0.D0) THEN
              TRA02(I4)=TRA02(I4)+XB(11,IELEM)*ALFA*F2MF4
            ELSE
              TRA02(I2)=TRA02(I2)-XB(05,IELEM)*ALFA*F2MF4
            ENDIF
            IF(F3MF4.LT.0.D0) THEN
              TRA02(I4)=TRA02(I4)+XB(12,IELEM)*ALFA*F3MF4
            ELSE
              TRA02(I3)=TRA02(I3)-XB(06,IELEM)*ALFA*F3MF4
            ENDIF
          ELSE
            ALFA = (PHIM - PHIP) / MAX(PHIM,1.D-10)
            IF(F1MF2.LT.0.D0) THEN
              TRA02(I1)=TRA02(I1)-XB(01,IELEM)*ALFA*F1MF2
            ELSE
              TRA02(I2)=TRA02(I2)+XB(07,IELEM)*ALFA*F1MF2
            ENDIF
            IF(F1MF3.LT.0.D0) THEN
              TRA02(I1)=TRA02(I1)-XB(02,IELEM)*ALFA*F1MF3
            ELSE
              TRA02(I3)=TRA02(I3)+XB(08,IELEM)*ALFA*F1MF3
            ENDIF
            IF(F1MF4.LT.0.D0) THEN
              TRA02(I1)=TRA02(I1)-XB(03,IELEM)*ALFA*F1MF4
            ELSE
              TRA02(I4)=TRA02(I4)+XB(09,IELEM)*ALFA*F1MF4
            ENDIF
            IF(F2MF3.LT.0.D0) THEN
              TRA02(I2)=TRA02(I2)-XB(04,IELEM)*ALFA*F2MF3
            ELSE
              TRA02(I3)=TRA02(I3)+XB(10,IELEM)*ALFA*F2MF3
            ENDIF
            IF(F2MF4.LT.0.D0) THEN
              TRA02(I2)=TRA02(I2)-XB(05,IELEM)*ALFA*F2MF4
            ELSE
              TRA02(I4)=TRA02(I4)+XB(11,IELEM)*ALFA*F2MF4
            ENDIF
            IF(F3MF4.LT.0.D0) THEN
              TRA02(I3)=TRA02(I3)-XB(06,IELEM)*ALFA*F3MF4
            ELSE
              TRA02(I4)=TRA02(I4)+XB(12,IELEM)*ALFA*F3MF4
            ENDIF
          ENDIF
!
        ENDDO
!
        ELSE
          WRITE(LU,*) 'ELEMENT ',IELM3,' NOT COMPUTED IN MURD3D'
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(SCHCF.EQ.ADV_NSC) THEN
!
!       COMPUTES THE FLUX TERMS PER UNIT OF TIME:
!       I.E. : SUM ON J (LAMBDA(I,J)*(FC(J)-FC(I))
!       DECOMPOSED INTO : -(SUM ON J (LAMBDA(I,J)*(FC(I)) WHICH IS DB * FC(I)
!                         +(SUM ON J (LAMBDA(I,J)*(FC(J)) WHICH IS XB * FC
!       IT IS EQUIVALENT TO A MATRIX-VECTOR PRODUCT...
!
!       RETRIEVING THE DIAGONAL FROM THE OFF-DIAGONAL TERMS
!       THIS IS NOT DONE BY MATRIY, SEE CALLS TO MT14PP AND MT14TT
!
        DO IPOIN=1,NPOIN3
          DB(IPOIN)=0.D0
        ENDDO
        IF(IELM3.EQ.41) THEN
          DO IELEM = 1,NELEM3
            I1 = IKLE3(IELEM,1)
            I2 = IKLE3(IELEM,2)
            I3 = IKLE3(IELEM,3)
            I4 = IKLE3(IELEM,4)
            I5 = IKLE3(IELEM,5)
            I6 = IKLE3(IELEM,6)
            DB(I1)=DB(I1)-XB(01,IELEM)-XB(02,IELEM)-XB(03,IELEM)
     &                   -XB(04,IELEM)-XB(05,IELEM)
            DB(I2)=DB(I2)-XB(16,IELEM)-XB(06,IELEM)-XB(07,IELEM)
     &                   -XB(08,IELEM)-XB(09,IELEM)
            DB(I3)=DB(I3)-XB(17,IELEM)-XB(21,IELEM)-XB(10,IELEM)
     &                   -XB(11,IELEM)-XB(12,IELEM)
            DB(I4)=DB(I4)-XB(18,IELEM)-XB(22,IELEM)-XB(25,IELEM)
     &                   -XB(13,IELEM)-XB(14,IELEM)
            DB(I5)=DB(I5)-XB(19,IELEM)-XB(23,IELEM)-XB(26,IELEM)
     &                   -XB(28,IELEM)-XB(15,IELEM)
            DB(I6)=DB(I6)-XB(20,IELEM)-XB(24,IELEM)-XB(27,IELEM)
     &                   -XB(29,IELEM)-XB(30,IELEM)
          ENDDO
        ELSEIF(IELM3.EQ.51) THEN
          DO IELEM = 1,NELEM3
            I1 = IKLE3(IELEM,1)
            I2 = IKLE3(IELEM,2)
            I3 = IKLE3(IELEM,3)
            I4 = IKLE3(IELEM,4)
            DB(I1)=DB(I1)-XB(01,IELEM)-XB(02,IELEM)-XB(03,IELEM)
            DB(I2)=DB(I2)-XB(04,IELEM)-XB(05,IELEM)-XB(07,IELEM)
            DB(I3)=DB(I3)-XB(06,IELEM)-XB(08,IELEM)-XB(10,IELEM)
            DB(I4)=DB(I4)-XB(09,IELEM)-XB(11,IELEM)-XB(12,IELEM)
          ENDDO
        ENDIF
        CALL MATVEC('X=AY     ',STRA02,B,SFC,0.D0,MESH3D)
        IF(PRECOR) THEN
!         DB HAS TO BE COMPLETED (COMPARE THE IMPLEMENTATIONS FOR THE PSI
!         SCHEME ABOVE, DEPENDING ON PRECOR). IT IS HERE IN FACT REDONE
          IF(IELM3.NE.41) THEN
            WRITE(LU,*) 'MURD3D: ELEMENT ',IELM3,' NOT IMPLEMENTED'
            WRITE(LU,*) 'WITH SCHEME ',SCHCF,' AND PREDICTOR-CORRECTOR'
            CALL PLANTE(1)
            STOP
          ENDIF
          DO IPOIN=1,NPOIN3
            DB(IPOIN)=0.D0
          ENDDO
          DO IELEM = 1,NELEM3
            I1 = IKLE3(IELEM,1)
            I2 = IKLE3(IELEM,2)
            I3 = IKLE3(IELEM,3)
            I4 = IKLE3(IELEM,4)
            I5 = IKLE3(IELEM,5)
            I6 = IKLE3(IELEM,6)
!           DB COMPUTED WITH LAMBDA N AND NOT PSI!!
            DB(I1)=DB(I1)-XB(01,IELEM)-XB(02,IELEM)-XB(03,IELEM)
     &                   -XB(04,IELEM)-XB(05,IELEM)
     &                   -XB(16,IELEM)-XB(17,IELEM)-XB(18,IELEM)
     &                   -XB(19,IELEM)-XB(20,IELEM)
            DB(I2)=DB(I2)-XB(16,IELEM)-XB(06,IELEM)-XB(07,IELEM)
     &                   -XB(08,IELEM)-XB(09,IELEM)
     &                   -XB(01,IELEM)-XB(21,IELEM)-XB(22,IELEM)
     &                   -XB(23,IELEM)-XB(24,IELEM)
            DB(I3)=DB(I3)-XB(17,IELEM)-XB(21,IELEM)-XB(10,IELEM)
     &                   -XB(11,IELEM)-XB(12,IELEM)
     &                   -XB(02,IELEM)-XB(06,IELEM)-XB(25,IELEM)
     &                   -XB(26,IELEM)-XB(27,IELEM)
            DB(I4)=DB(I4)-XB(18,IELEM)-XB(22,IELEM)-XB(25,IELEM)
     &                   -XB(13,IELEM)-XB(14,IELEM)
     &                   -XB(03,IELEM)-XB(07,IELEM)-XB(10,IELEM)
     &                   -XB(28,IELEM)-XB(29,IELEM)
            DB(I5)=DB(I5)-XB(19,IELEM)-XB(23,IELEM)-XB(26,IELEM)
     &                   -XB(28,IELEM)-XB(15,IELEM)
     &                   -XB(04,IELEM)-XB(08,IELEM)-XB(11,IELEM)
     &                   -XB(13,IELEM)-XB(30,IELEM)
            DB(I6)=DB(I6)-XB(20,IELEM)-XB(24,IELEM)-XB(27,IELEM)
     &                   -XB(29,IELEM)-XB(30,IELEM)
     &                   -XB(05,IELEM)-XB(09,IELEM)-XB(12,IELEM)
     &                   -XB(14,IELEM)-XB(15,IELEM)
          ENDDO
        ENDIF
!
      ELSEIF(SCHCF.EQ.ADV_LPO) THEN
!
        IF(IELM3.NE.41) THEN
          WRITE(LU,*) 'MURD3D: ELEMENT ',IELM3,' NOT IMPLEMENTED'
          WRITE(LU,*) '        WITH SCHEME ',SCHCF
          CALL PLANTE(1)
          STOP
        ENDIF
!
        NSEGH=NSEG*NPLAN
        NSEGV=(NPLAN-1)*NPOIN2
!       COMPUTES DB AND TRA02 IN UPWIND EXPLICIT FINITE VOLUMES
!       POSSIBLE OPTIMISATION: DB IS NOT COMPUTED IF OPT=2
        DO IPOIN=1,NPOIN3
          DB(IPOIN)=0.D0
          TRA02(IPOIN)=0.D0
        ENDDO
!       HORIZONTAL AND VERTICAL FLUXES ONLY
!       BEWARE : FLUXES FROM POINT 2 TO 1 IN SEGMENT
!                WITH POINTS 1 AND 2 (SEE PRECON AND FLUX3D)
        IF(PRECOR) THEN
          DO ISEG3D = 1,NSEGH+NSEGV
            I1=GLOSEG(ISEG3D,1)
            I2=GLOSEG(ISEG3D,2)
            DB(I1) = DB(I1)-ABS(FLODEL(ISEG3D))
            DB(I2) = DB(I2)-ABS(FLODEL(ISEG3D))
            IF(FLOPAR(ISEG3D).GT.0.D0) THEN
              TRA02(I1)=TRA02(I1)-FLODEL(ISEG3D)*(FC(I1)-FC(I2))
            ELSEIF(FLOPAR(ISEG3D).LT.0.D0) THEN
              TRA02(I2)=TRA02(I2)+FLODEL(ISEG3D)*(FC(I2)-FC(I1))
            ENDIF
          ENDDO
        ELSE
          DO ISEG3D = 1,NSEGH+NSEGV
            I1=GLOSEG(ISEG3D,1)
            I2=GLOSEG(ISEG3D,2)
            IF(FLOPAR(ISEG3D).GT.0.D0) THEN
              DB(I1)   = DB(I1)   -FLODEL(ISEG3D)
              TRA02(I1)= TRA02(I1)-FLODEL(ISEG3D)*(FC(I1)-FC(I2))
            ELSEIF(FLOPAR(ISEG3D).LT.0.D0) THEN
              DB(I2)   = DB(I2)   +FLODEL(ISEG3D)
              TRA02(I2)= TRA02(I2)+FLODEL(ISEG3D)*(FC(I2)-FC(I1))
            ENDIF
          ENDDO
        ENDIF
!
      ELSE
        WRITE(LU,*) 'MURD3D: UNKNOWN ADVECTION SCHEME: ',SCHCF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(STRA02,2,MESH3D)
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE LIMITING SUB-TIMESTEP :
!     MULTIPLIES THE REMAINING SUB TIMESTEP BY DTJ
!     AND ADDS TO VOLU NEGATIVE TERM (MASS AT N+1)
!
!     OPT=1 : OLD CRITERION
!     OPT=2 : NEW CRITERION LESS RESTRICTIVE
!
      IF(OPTADV.EQ.1) THEN
        OPT=2
      ELSE
        OPT=1
      ENDIF
!
      IF(OPT.EQ.2) THEN
!
!     COMPUTES THE LOCAL EXTREMA
!
      DO IPOIN=1,NPOIN3
        MINFC%R(IPOIN)=FC(IPOIN)
        MAXFC%R(IPOIN)=FC(IPOIN)
      ENDDO
!
!     BOUNDARY VALUES
!
      DO IPTFR3=1,NPTFR3
        IPOIN=NBOR3(IPTFR3)
        IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
          MINFC%R(IPOIN)=MIN(MINFC%R(IPOIN),FBORL(IPTFR3))
          MAXFC%R(IPOIN)=MAX(MAXFC%R(IPOIN),FBORL(IPTFR3))
        ENDIF
      ENDDO
!
      IF(IELM3.EQ.41) THEN
!
        DO IELEM=1,NELEM3
          I1=IKLE3(IELEM,1)
          I2=IKLE3(IELEM,2)
          I3=IKLE3(IELEM,3)
          I4=IKLE3(IELEM,4)
          I5=IKLE3(IELEM,5)
          I6=IKLE3(IELEM,6)
          MINEL=MIN(FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6))
          MAXEL=MAX(FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6))
          MINFC%R(I1)=MIN(MINFC%R(I1),MINEL)
          MINFC%R(I2)=MIN(MINFC%R(I2),MINEL)
          MINFC%R(I3)=MIN(MINFC%R(I3),MINEL)
          MINFC%R(I4)=MIN(MINFC%R(I4),MINEL)
          MINFC%R(I5)=MIN(MINFC%R(I5),MINEL)
          MINFC%R(I6)=MIN(MINFC%R(I6),MINEL)
          MAXFC%R(I1)=MAX(MAXFC%R(I1),MAXEL)
          MAXFC%R(I2)=MAX(MAXFC%R(I2),MAXEL)
          MAXFC%R(I3)=MAX(MAXFC%R(I3),MAXEL)
          MAXFC%R(I4)=MAX(MAXFC%R(I4),MAXEL)
          MAXFC%R(I5)=MAX(MAXFC%R(I5),MAXEL)
          MAXFC%R(I6)=MAX(MAXFC%R(I6),MAXEL)
        ENDDO
!
      ELSEIF(IELM3.EQ.51) THEN
!
        DO IELEM=1,NELEM3
          I1=IKLE3(IELEM,1)
          I2=IKLE3(IELEM,2)
          I3=IKLE3(IELEM,3)
          I4=IKLE3(IELEM,4)
          MINEL=MIN(FC(I1),FC(I2),FC(I3),FC(I4))
          MAXEL=MAX(FC(I1),FC(I2),FC(I3),FC(I4))
          MINFC%R(I1)=MIN(MINFC%R(I1),MINEL)
          MINFC%R(I2)=MIN(MINFC%R(I2),MINEL)
          MINFC%R(I3)=MIN(MINFC%R(I3),MINEL)
          MINFC%R(I4)=MIN(MINFC%R(I4),MINEL)
          MAXFC%R(I1)=MAX(MAXFC%R(I1),MAXEL)
          MAXFC%R(I2)=MAX(MAXFC%R(I2),MAXEL)
          MAXFC%R(I3)=MAX(MAXFC%R(I3),MAXEL)
          MAXFC%R(I4)=MAX(MAXFC%R(I4),MAXEL)
        ENDDO
!
      ELSE
        WRITE(LU,*) 'ELEMENT ',IELM3,' NOT COMPUTED IN MURD3D'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     IN PARALLEL MODE: GLOBAL EXTREMA
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(MAXFC,3,MESH3D)
        CALL PARCOM(MINFC,4,MESH3D)
      ENDIF
!
!     NEW COMPUTATION OF TRA03
!
      DO IPOIN=1,NPOIN3
        IF(TRA02(IPOIN).GT.0.D0) THEN
          TRA03(IPOIN)=VOLU2(IPOIN)-DTJ*TRA02(IPOIN)/
     &                 MAX(MAXFC%R(IPOIN)-FC(IPOIN),1.D-12)
        ELSE
          TRA03(IPOIN)=VOLU2(IPOIN)+DTJ*TRA02(IPOIN)/
     &                 MAX(FC(IPOIN)-MINFC%R(IPOIN),1.D-12)
        ENDIF
      ENDDO
!
!     BOUNDARY TERMS
!
      DO IPTFR3=1,NPTFR3
        IPOIN=NBOR3(IPTFR3)
        IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
          TRA03(IPOIN)=TRA03(IPOIN)+DTJ*FLUEXTPAR(IPOIN)
        ENDIF
      ENDDO
!
!     POSITIVE SOURCES CHANGE THE MONOTONICITY CRITERION
!
      IF(NSCE.GT.0) THEN
        IF(OPTSOU.EQ.1) THEN
!         SOURCE NOT CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            DO IPOIN=1,NPOIN3
!                            WITH PARCOM
              IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                TRA03(IPOIN)=TRA03(IPOIN)
     &                      -DTJ*SOURCES%ADR(IS)%P%R(IPOIN)
!                                WITH PARCOM
              ENDIF
            ENDDO
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
!         SOURCE CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            IF(ISCE(IS).GT.0) THEN
              IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
!                            WITH PARCOM
              IF(SOURCES%ADR(1)%P%R(IPOIN).GT.0.D0) THEN
                TRA03(IPOIN)=TRA03(IPOIN)
     &                      -DTJ*SOURCES%ADR(1)%P%R(IPOIN)
!                                WITH PARCOM
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     POSITIVE BED FLUXES CHANGE THE MONOTONICITY CRITERION
!
      IF(BEDBOU) THEN
!       STORE BEDFLU IN T2_01 AS IT NEEDS TO BE ASSEMBLED
        CALL OS('X=Y     ',X=T2_01,Y=BEDFLU)
        IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
        DO IPOIN=1,NPOIN2
          IF(T2_01%R(IPOIN).GT.0.D0) THEN
            TRA03(IPOIN)=TRA03(IPOIN)-DTJ*T2_01%R(IPOIN)
!                                         WITH PARCOM
          ENDIF
        ENDDO
      ENDIF
!
!     RAIN CHANGES THE MONOTONICITY CRITERION
!
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
!            WITH PARCOM
          IF(PARAPLUIE%R(IPOIN).GT.0.D0) THEN
            IS=NPOIN3-NPOIN2+IPOIN
!                                   WITH PARCOM
            TRA03(IS)=TRA03(IS)-DTJ*PARAPLUIE%R(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
      ELSEIF(OPT.EQ.1) THEN
!
!     TRA03 : COEFFICIENT OF FC(I), THAT MUST REMAIN POSITIVE FOR MONOTONICITY
!     HERE TRA03 WILL BE ASSEMBLED IN PARALLEL AT THE END
!     IF(PRECOR.AND.NCO_DIST.GT.0) THEN
!       CALL OV('X=Y+CZ  ',TRA03, VOLU, DB, 2.D0*DTJ, NPOIN3)
!     ELSE
        CALL OV('X=Y+CZ  ',TRA03, VOLU, DB, DTJ, NPOIN3)
!     ENDIF
!
!     NEGATIVE BOUNDARY TERMS (ENTERING FLUXES)
!
      IF(PRECOR) THEN
        DO IPOIN = 1,NPOIN3
          TRA03(IPOIN)=TRA03(IPOIN)-DTJ*ABS(FLUEXT(IPOIN))
        ENDDO
      ELSE
        DO IPOIN = 1,NPOIN3
!            WITH PARCOM
          IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
!                                         WITHOUT PARCOM
            TRA03(IPOIN)=TRA03(IPOIN)+DTJ*FLUEXT(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
!     SOURCES CHANGE THE MONOTONICITY CRITERION
!
      IF(NSCE.GT.0) THEN
        IF(OPTSOU.EQ.1) THEN
!         SOURCE NOT CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            IIS=IS
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
!           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            IF(PRECOR) THEN
              DO IPOIN=1,NPOIN3
                TRA03(IPOIN)=TRA03(IPOIN)
     &                      -DTJ*ABS(SOURCES%ADR(IIS)%P%R(IPOIN))
              ENDDO
            ELSE
              DO IPOIN=1,NPOIN3
!                              WITH PARCOM
                IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                  TRA03(IPOIN)=TRA03(IPOIN)
     &                        -DTJ*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                              WITHOUT PARCOM
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
!         SOURCE CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            IF(ISCE(IS).GT.0) THEN
              IIS=1
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=2
              IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
              IF(PRECOR) THEN
                TRA03(IPOIN)=TRA03(IPOIN)
     &                      -DTJ*ABS(SOURCES%ADR(IIS)%P%R(IPOIN))
              ELSE
!                              WITH PARCOM
                IF(SOURCES%ADR(1)%P%R(IPOIN).GT.0.D0) THEN
                  TRA03(IPOIN)=TRA03(IPOIN)
     &                        -DTJ*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                              WITHOUT PARCOM
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
      IF(RAIN) THEN
        IF(PRECOR) THEN
          DO IPOIN=1,NPOIN2
            IS=NPOIN3-NPOIN2+IPOIN
            TRA03(IS)=TRA03(IS)-DTJ*ABS(PLUIE%R(IPOIN))
          ENDDO
        ELSE
          DO IPOIN=1,NPOIN2
!              WITH PARCOM
            IF(PARAPLUIE%R(IPOIN).GT.0.D0) THEN
              IS=NPOIN3-NPOIN2+IPOIN
!                                     WITHOUT PARCOM
              TRA03(IS)=TRA03(IS)-DTJ*PLUIE%R(IPOIN)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     POSITIVE BED FLUXES CHANGE THE MONOTONICITY CRITERION
!
      IF(BEDBOU) THEN
!       STORE BEDFLU IN T2_01 AS IT NEEDS TO BE ASSEMBLED
        CALL CPSTVC(BEDFLU,T2_01)
        CALL OS('X=Y     ',X=T2_01,Y=BEDFLU)
        IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
        IF(PRECOR) THEN
          DO IPOIN=1,NPOIN2
            TRA03(IPOIN)=TRA03(IPOIN)-DTJ*ABS(BEDFLU%R(IPOIN))
          ENDDO
        ELSE
          DO IPOIN=1,NPOIN2
!              WITH PARCOM
            IF(T2_01%R(IPOIN).GT.0.D0) THEN
              TRA03(IPOIN)=TRA03(IPOIN)-DTJ*BEDFLU%R(IPOIN)
!                                           WITHOUT PARCOM
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(STRA03,2,MESH3D)
!
      ELSE
        WRITE(LU,*) 'MURD3D: OPT=',OPT,' NOT IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF !OPT =1 OR 2
!
!     IF MONOTONICITY IS NOT ENSURED, REDUCTION OF TIME-STEP BY FACTOR ALFA
!     THE MINIMUM ON ALL POINTS WILL BE TAKEN
!
      ALFA = 1.D0
      IGUILT=0
      IF(OPTBAN.EQ.2) THEN
        DO IPOIN = 1,NPOIN3
          IF(TRA03(IPOIN).LT.0.D0.AND.MASKPT(IPOIN).GT.0.5D0) THEN
            IF(ABS(TRA01(IPOIN)-TRA03(IPOIN)).GT.EPS) THEN
!             CONSIDERING THAT THE NEW TIME-STEP WILL BE ALFA*DTJ
!             VOLU WILL BE AT THAT TIME ALFA*VOLU(N+1)+(1-ALFA)*VOLU(IN TRA01)
!             MAXIMUM POSSIBLE ALFA IS SUCH THAT VOLU+DB*ALFA*DTJ=0
!             HENCE THE FOLLOWING FORMULA :
              ALFA2=TRA01(IPOIN)/(TRA01(IPOIN)-TRA03(IPOIN))
              IF(ALFA.GT.ALFA2) THEN
                ALFA = ALFA2
                IGUILT=IPOIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ELSE
        DO IPOIN = 1,NPOIN3
!                                          TIDAL FLATS : VOLUN=0
          IF(TRA03(IPOIN).LT.0.D0.AND.TRA01(IPOIN).GT.EPS) THEN
            IF(ABS(TRA01(IPOIN)-TRA03(IPOIN)).GT.EPS) THEN
!             CONSIDERING THAT THE NEW TIME-STEP WILL BE ALFA*DTJ
!             VOLU WILL BE AT THAT TIME ALFA*VOLU(N+1)+(1-ALFA)*VOLU(IN TRA01)
!             MAXIMUM POSSIBLE ALFA IS SUCH THAT VOLU+DB*ALFA*DTJ=0
!             HENCE THE FOLLOWING FORMULA :
              ALFA2=TRA01(IPOIN)/(TRA01(IPOIN)-TRA03(IPOIN))
              IF(ALFA.GT.ALFA2) THEN
                ALFA = ALFA2
                IGUILT=IPOIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!     SAVING THE LOCAL ALFA (IN CASE OF MAXIMUM ITERATION REACHED
!                            TO KNOW THE MOST GUILTY PROCESSOR)
      ALFALOC=ALFA
      IF(NCSIZE.GT.1) ALFA = P_MIN(ALFA)
      DTJALFA=DTJ*ALFA
!
!     COMPUTES VOLU AFTER AN EXTRA ALFA*DTJ
!
      CALL OV('X=CX    ',TRA01,TRA01,TRA01,1.D0-ALFA,NPOIN3)
      CALL OV('X=X+CY  ',TRA01,VOLU2,VOLU2,     ALFA,NPOIN3)
!
!     SAME THING FOR ZEND
!
      IF(PRECOR.AND.NCO_DIST.GT.0) THEN
!       COMPUTES DELTA Z AFTER ALFA*DTJ = VALUE OF Z AT THE END OF THE SUBTIME STEP
!       DELTA ZEND = (1-ALFA) DELTA(ZN) + ALFA DELTA(ZN+1) (FISSO-MESH3D%Z)
        DO IPOIN=1,NPOIN3
          ZSTART(IPOIN)=ZEND(IPOIN)
          ZEND(IPOIN)=(1.D0-ALFA)*ZEND(IPOIN)+ALFA*MESH3D%Z%R(IPOIN)
        ENDDO
      ELSE
!
!     BOUNDARY FLUX AND SOURCE COMPUTATION FOR ALL EXPLICIT SCHEMES
!     EXCEPT PC SCHEME AND SEMI-IMPLICIT (FOR PC SCHEME DONE AT THE LAST CORRECTOR STEP)
!     COMPUTING THE FLUXES FOR THE MASS BALANCE
!
      IF(CALFLU) THEN
!
!       BOUNDARY TERMS
!
        DO IPTFR3=1,NPTFR3
          IPOIN=NBOR3(IPTFR3)
          IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
            FLUX=FLUX+DTJALFA*FLUEXT(IPOIN)*FBORL(IPTFR3)
          ELSE
            FLUX=FLUX+DTJALFA*FLUEXT(IPOIN)*FC(IPOIN)
          ENDIF
        ENDDO
!
        DO IPTFR3 = 1,NPTFR3
          IPOIN=NBOR3(IPTFR3)
          ILIQ=NUMLIQ%I(IPTFR3)
          IF(ILIQ.GT.0) THEN
            IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
              FLUXB(ILIQ)=FLUXB(ILIQ)
     &                   +DTJALFA*FLUEXT(IPOIN)*FBORL(IPTFR3)
            ELSE
              FLUXB(ILIQ)=FLUXB(ILIQ)+DTJALFA*FLUEXT(IPOIN)*FC(IPOIN)
            ENDIF
          ENDIF
        ENDDO
!
!       SOURCES
!
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            IF(OPTSOU.EQ.1) THEN
!             SOURCE NOT CONSIDERED AS A DIRAC
              IIS=IS
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=IIS+NSCE
              DO IPOIN=1,NPOIN3
!                              WITH PARCOM
                IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                  FLUX=FLUX
     &                -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                                   WITHOUT PARCOM
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &                -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                                   WITHOUT PARCOM
                ELSE
                  FLUX=FLUX
     &                -DTJALFA*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                                    WITHOUT PARCOM
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &                -DTJALFA*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                                    WITHOUT PARCOM
                ENDIF
              ENDDO
            ELSEIF(OPTSOU.EQ.2) THEN
!             SOURCE CONSIDERED AS A DIRAC
              IF(ISCE(IS).GT.0) THEN
                IIS=1
!               HERE IN PARALLEL SOURCES WITHOUT PARCOM
!               ARE STORED AT ADRESSES 2 (SEE SOURCES_SINKS.F)
                IF(NCSIZE.GT.1) IIS=2
                IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
!                              WITH PARCOM
                IF(SOURCES%ADR(1)%P%R(IPOIN).GT.0.D0) THEN
                  FLUX=FLUX
     &                -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                                   WITHOUT PARCOM
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &                -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                                   WITHOUT PARCOM
                ELSE
                  FLUX=FLUX
     &                -DTJALFA*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                                    WITHOUT PARCOM
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &                -DTJALFA*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                                    WITHOUT PARCOM
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
!
!       FOR BED FLUXES
!
        IF(BEDBOU) THEN
          DO IPOIN=1,NPOIN2
            IF(T2_01%R(IPOIN).LE.0.D0) THEN
              FLUX=FLUX-DTJALFA*FC(IPOIN)*T2_01%R(IPOIN)
              FLUXB(MAXFRO+NSCE+1)=FLUXB(MAXFRO+NSCE+1)
     &                            -DTJALFA*FC(IPOIN)*T2_01%R(IPOIN)
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF   ! IF(CALFLU)
      ENDIF ! .NOT.PRECOR
!
!     ADVECTION DURING ALFA*DTJ
!
!     SOURCES (BUT WHEN INTAKE, FSCE=FC)
!
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          IF(OPTBAN.EQ.2) THEN
            IF(OPTSOU.EQ.1) THEN
!             THE SOURCE IS NOT CONSIDERED AS A DIRAC
              DO IPOIN=1,NPOIN3
                IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
                    FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FINSUB(IPOIN))
     &              *MAX(SOURCES%ADR(IS)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
!                                    WITH PARCOM
                ENDIF
              ENDDO
            ELSEIF(OPTSOU.EQ.2) THEN
!             THE SOURCE IS CONSIDERED AS A DIRAC
              IF(ISCE(IS).GT.0) THEN
                IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
                IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
                  FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FINSUB(IPOIN))
     &            *MAX(SOURCES%ADR(1)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
!                                  WITH PARCOM
                ENDIF
              ENDIF
            ENDIF
          ELSE
            IF(OPTSOU.EQ.1) THEN
!             THE SOURCE IS NOT CONSIDERED AS A DIRAC
              DO IPOIN=1,NPOIN3
                IF(TRA01(IPOIN).GT.EPS) THEN
                  FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FINSUB(IPOIN))
     &            *MAX(SOURCES%ADR(IS)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
!                                  WITH PARCOM
                ENDIF
              ENDDO
            ELSEIF(OPTSOU.EQ.2) THEN
!             THE SOURCE IS CONSIDERED AS A DIRAC
              IF(ISCE(IS).GT.0) THEN
                IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
                IF(TRA01(IPOIN).GT.EPS) THEN
                  FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FINSUB(IPOIN))
     &            *MAX(SOURCES%ADR(1)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
!                                  WITH PARCOM
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!     BEDFLUXES
!
      IF(BEDBOU) THEN
        IF(OPTBAN.EQ.2) THEN
          DO IPOIN=1,NPOIN2
            IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
              FC(IPOIN)=FC(IPOIN)-DTJALFA*FINSUB(IPOIN)
     &          *MAX(T2_01%R(IPOIN),0.D0)/TRA01(IPOIN)
            ENDIF
          ENDDO
        ELSE
          DO IPOIN=1,NPOIN2
            IF(TRA01(IPOIN).GT.EPS) THEN
              FC(IPOIN)=FC(IPOIN)-DTJALFA*FINSUB(IPOIN)
     &        *MAX(T2_01%R(IPOIN),0.D0)/TRA01(IPOIN)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     RAIN (NOTE: SHOULD BE TAKEN INTO ACCOUNT IN STABILITY CRITERION)
!     VALUE OF TRACER IN RAIN TAKEN INTO ACCOUNT ONLY IF RAIN POSITIVE
!     NOT IN CASE OF EVAPORATION, HENCE THE MAX(PLUIE,0)
!
      IF(RAIN) THEN
        IF(OPTBAN.EQ.2) THEN
          DO IPOIN=1,NPOIN2
            IS=NPOIN3-NPOIN2+IPOIN
            IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IS).GT.EPS) THEN
              C=TRAIN*MAX(PARAPLUIE%R(IPOIN),0.D0)
     &         -FINSUB(IS)*PARAPLUIE%R(IPOIN)
              FC(IS)=FC(IS)+DTJALFA*C/TRA01(IS)
            ENDIF
          ENDDO
        ELSE
          DO IPOIN=1,NPOIN2
            IS=NPOIN3-NPOIN2+IPOIN
            IF(TRA01(IS).GT.EPS) THEN
              C=TRAIN*MAX(PARAPLUIE%R(IPOIN),0.D0)
     &         -FINSUB(IS)*PARAPLUIE%R(IPOIN)
              FC(IS)=FC(IS)+DTJALFA*C/TRA01(IS)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     BOUNDARY TERMS
!
      IF(OPTBAN.EQ.2) THEN
        DO IPTFR3=1,NPTFR3
          IPOIN=NBOR3(IPTFR3)
          IF(MASKPT(IPOIN).GT.0.5D0.AND.FLUEXTPAR(IPOIN).LT.0.D0.AND.
     &       TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)-DTJALFA*(FBORL(IPTFR3)-FINSUB(IPOIN))
     &                *FLUEXTPAR(IPOIN)/TRA01(IPOIN)
          ENDIF
        ENDDO
      ELSE
        DO IPTFR3=1,NPTFR3
          IPOIN=NBOR3(IPTFR3)
          IF(FLUEXTPAR(IPOIN).LT.0.D0.AND.TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)-DTJALFA*(FBORL(IPTFR3)-FINSUB(IPOIN))
     &                *FLUEXTPAR(IPOIN)/TRA01(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
!     FLUXES
!
      IF(OPTBAN.EQ.2) THEN
        DO IPOIN=1,NPOIN3
          IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*TRA02(IPOIN)/TRA01(IPOIN)
          ENDIF
        ENDDO
      ELSE
        DO IPOIN=1,NPOIN3
          IF(TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*TRA02(IPOIN)/TRA01(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------
!   CORRECTOR STEP FOR N AND PSI
!-----------------------------------------------
!
      IF(PRECOR.AND.NCO_DIST.GT.0)THEN
        DO ICOR=1,NCO_DIST
!
!         COMPUTING THE MINIMUM AND MAXIMUM AND POSSIBLE LIMITATION
!         LIMITATION ONLY IF:
!         - WE HAVE EXPLICIT 2ND ORDER PREDICTOR-CORRECTOR
!         - FOR FIRST ORDER OPTION (TETAF=0) AND ICOR.GT.1
!
          IF(OPTADV.EQ.3.OR.ICOR.GT.1) THEN
            DO IPOIN=1,NPOIN3
              MINFC%R(IPOIN)=FINSUB(IPOIN)
              MAXFC%R(IPOIN)=FINSUB(IPOIN)
            ENDDO
            IF(OPTADV.EQ.2) THEN
              DO IELEM=1,NELEM3
                I1 = IKLE3(IELEM,1)
                I2 = IKLE3(IELEM,2)
                I3 = IKLE3(IELEM,3)
                I4 = IKLE3(IELEM,4)
                I5 = IKLE3(IELEM,5)
                I6 = IKLE3(IELEM,6)
!               NOT ALL FC TAKEN
                MINFC%R(I1)=MIN(MINFC%R(I1),FC(I1),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I1)=MAX(MAXFC%R(I1),FC(I1),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I2)=MIN(MINFC%R(I2),FC(I2),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I2)=MAX(MAXFC%R(I2),FC(I2),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I3)=MIN(MINFC%R(I3),FC(I3),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I3)=MAX(MAXFC%R(I3),FC(I3),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I4)=MIN(MINFC%R(I4),FC(I4),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I4)=MAX(MAXFC%R(I4),FC(I4),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I5)=MIN(MINFC%R(I5),FC(I5),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I5)=MAX(MAXFC%R(I5),FC(I5),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I6)=MIN(MINFC%R(I6),FC(I6),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I6)=MAX(MAXFC%R(I6),FC(I6),
     &                          FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &                          FINSUB(I4),FINSUB(I5),FINSUB(I6))
              ENDDO
            ELSE
!             ALL FC TAKEN
              DO IELEM=1,NELEM3
                I1 = IKLE3(IELEM,1)
                I2 = IKLE3(IELEM,2)
                I3 = IKLE3(IELEM,3)
                I4 = IKLE3(IELEM,4)
                I5 = IKLE3(IELEM,5)
                I6 = IKLE3(IELEM,6)
                MINFC%R(I1)=MIN(MINFC%R(I1),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I1)=MAX(MAXFC%R(I1),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I2)=MIN(MINFC%R(I2),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I2)=MAX(MAXFC%R(I2),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I3)=MIN(MINFC%R(I3),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I3)=MAX(MAXFC%R(I3),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I4)=MIN(MINFC%R(I4),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I4)=MAX(MAXFC%R(I4),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I5)=MIN(MINFC%R(I5),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I5)=MAX(MAXFC%R(I5),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MINFC%R(I6)=MIN(MINFC%R(I6),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
                MAXFC%R(I6)=MAX(MAXFC%R(I6),
     &               FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6),
     &               FINSUB(I1),FINSUB(I2),FINSUB(I3),
     &               FINSUB(I4),FINSUB(I5),FINSUB(I6))
              ENDDO
            ENDIF
            IF(NCSIZE.GT.1) THEN
              CALL PARCOM(MINFC,4,MESH3D)
              CALL PARCOM(MAXFC,3,MESH3D)
            ENDIF
          ENDIF
!
          IF(OPTADV.EQ.3) THEN
!           FOR SECOND ORDER, LIMITATION OF F ALREADY AT THIS LEVEL
!
            DO IPOIN=1,NPOIN3
              FC(IPOIN)=MIN(FC(IPOIN),2.D0*FINSUB(IPOIN)-MINFC%R(IPOIN))
              FC(IPOIN)=MAX(FC(IPOIN),2.D0*FINSUB(IPOIN)-MAXFC%R(IPOIN))
!             THESE CONDITIONS ARE ENSURED BY THE PREDICTOR
!             FC%R(I)=MIN(FC%R(I),FMAX(I)+(FINSUB(I)-FMAX(I))/(2*2-1.D0))
!             FC%R(I)=MAX(FC%R(I),FMIN(I)+(FINSUB(I)-FMIN(I))/(2*2-1.D0))
            ENDDO
            IF(ICOR.GT.1) THEN
              DO IPOIN=1,NPOIN3
                FC(IPOIN)=MIN(FC(IPOIN),
     &              FINSUB(IPOIN)+1.D0/(2.D0-TETA)*
     &              (MAXFC%R(IPOIN)-FINSUB(IPOIN)))
                FC(IPOIN)=MAX(FC(IPOIN),
     &              FINSUB(IPOIN)+1.D0/(2.D0-TETA)*
     &              (MINFC%R(IPOIN)-FINSUB(IPOIN)))
              ENDDO
            ENDIF
          ELSEIF(OPTADV.EQ.2.AND.ICOR.GT.1) THEN
            DO IPOIN=1,NPOIN3
!             LIMITING THE PREDICTOR
              FC(IPOIN)=MIN(FC(IPOIN),FINSUB(IPOIN)+
     &              0.5D0*(MAXFC%R(IPOIN)-FINSUB(IPOIN)))
              FC(IPOIN)=MAX(FC(IPOIN),FINSUB(IPOIN)+
     &              0.5D0*(MINFC%R(IPOIN)-FINSUB(IPOIN)))
            ENDDO
          ENDIF !OPTADV.EQ.3
!
!         END OF LIMITATION OF THE VALUE COMPUTED BY THE PREDICTOR
!
          DO IPOIN=1,NPOIN3
!           DFDT(IPOIN)=(FC(IPOIN)-FINSUB(IPOIN))/DTJALFA
            TRA02(IPOIN)=(FC(IPOIN)-FINSUB(IPOIN))/DTJALFA
          ENDDO
!
!         COMPUTE THE FLUXES FOR THE CORRECTOR
!
!                                                    DFDT
          CALL FLUX_COR(FC,FINSUB,FI_I%R,ZSTART,ZEND,TRA02,XB,DIM1XB,
     &                  TETA,IKLE3,MESH3D,NELEM3,NELMAX,NPOIN3,IELM3,
     &                  SCHCF)
          CALL PARCOM(FI_I,2,MESH3D)
!
!         MODIFICATIONS OF BOUNDARY FLUXES FOR PRED COR
!         MASS BALANCE ONLY AT THE LAST CORRECTION
          IF(CALFLU.AND.ICOR.EQ.NCO_DIST) THEN
!
!           BOUNDARY TERMS
!
            DO IPTFR3=1,NPTFR3
              IPOIN=NBOR3(IPTFR3)
              ILIQ=NUMLIQ%I(IPTFR3)
              IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
                FLUX=FLUX+DTJALFA*FLUEXT(IPOIN)*FBORL(IPTFR3)
                IF(ILIQ.GT.0) THEN
                  FLUXB(ILIQ)=FLUXB(ILIQ)+DTJALFA*FLUEXT(IPOIN)*
     &                        FBORL(IPTFR3)
                ENDIF
              ELSE
                FLUX=FLUX+DTJALFA*FLUEXT(IPOIN)*
     &          ((1.D0-TETA)*FINSUB(IPOIN)+TETA*FC(IPOIN))
                IF(ILIQ.GT.0) THEN
                  FLUXB(ILIQ)=FLUXB(ILIQ)+DTJALFA*FLUEXT(IPOIN)*
     &            ((1.D0-TETA)*FINSUB(IPOIN)+TETA*FC(IPOIN))
                ENDIF
              ENDIF
            ENDDO
!
            IF(NSCE.GT.0) THEN
              DO IS=1,NSCE
                IIS=IS
!               HERE IN PARALLEL SOURCES WITHOUT PARCOM
!               ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
                IF(NCSIZE.GT.1) IIS=IIS+NSCE
                DO IPOIN=1,NPOIN3
                  IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                    FLUX=FLUX
     &             -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
                    FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &             -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
                  ELSE
                    FLUX=FLUX
     &             -DTJALFA*SOURCES%ADR(IIS)%P%R(IPOIN)*
     &             ((1.D0-TETA)*FINSUB(IPOIN)+TETA*FC(IPOIN))
                    FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)
     &             -DTJALFA*SOURCES%ADR(IIS)%P%R(IPOIN)*
     &             ((1.D0-TETA)*FINSUB(IPOIN)+TETA*FC(IPOIN))
                  ENDIF
                ENDDO
              ENDDO
            ENDIF ! NSCE.GT.0
          ENDIF !CALFLU.AND.ICOR.EQ.NCO_DIST
!
!         BOUNDARY TERMS
!
          IF(OPTBAN.EQ.2) THEN
            DO IPTFR3=1,NPTFR3
              IPOIN=NBOR3(IPTFR3)
              IF(MASKPT(IPOIN).GT.0.5D0.AND.FLUEXTPAR(IPOIN).LT.0.D0
     &           .AND.TRA01(IPOIN).GT.EPS) THEN
                FC(IPOIN)=FC(IPOIN)
     & -DTJALFA*(FBORL(IPTFR3)-(1.D0-TETA)*FINSUB(IPOIN)-TETA*FC(IPOIN))
     &         *FLUEXTPAR(IPOIN)/TRA01(IPOIN)
              ENDIF
            ENDDO
          ELSE
            DO IPTFR3=1,NPTFR3
              IPOIN=NBOR3(IPTFR3)
              IF(FLUEXTPAR(IPOIN).LT.0.D0.AND.TRA01(IPOIN).GT.EPS) THEN
                FC(IPOIN)=FC(IPOIN)
     & -DTJALFA*(FBORL(IPTFR3)-(1.D0-TETA)*FINSUB(IPOIN)-TETA*FC(IPOIN))
     &         *FLUEXTPAR(IPOIN)/TRA01(IPOIN)
              ENDIF
            ENDDO
          ENDIF
!
!         FLUXES FROM THE CORRECTOR
!
          DO IPOIN=1,NPOIN3
            IF(TRA01(IPOIN).GT.EPS) THEN
              FC(IPOIN)=FC(IPOIN)-DTJALFA*FI_I%R(IPOIN)/TRA01(IPOIN)
            ENDIF
          ENDDO
!
        ENDDO
      ENDIF  ! IF(PREDICOR.AND.NCO_DIST.GT.1)
!
!     EXPLICIT SOURCE TERMS, TREATED A POSTERIORI
!
      IF(S0F%TYPR.NE.'0') THEN
        DO IPOIN=1,NPOIN3
          IF(OPTBAN.EQ.2) THEN
            IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
              FC(IPOIN)=FC(IPOIN)+DTJALFA*S0F%R(IPOIN)/TRA01(IPOIN)
            ENDIF
          ELSE
            IF(TRA01(IPOIN).GT.EPS) THEN
              FC(IPOIN)=FC(IPOIN)+DTJALFA*S0F%R(IPOIN)/TRA01(IPOIN)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!
!
!     DTJ WAS THE REMAINING TIME, ALFA*DTJ HAS BEEN DONE, THE REST IS:
      DTJ = DTJ * (1.D0-ALFA)
      NITER = NITER + 1
      IF(NITER.GE.100.AND.ALFA.LT.1.D0) THEN
        WRITE(LU,*) 'MURD3D: ITERATION NO. REACHED ',NITER,', STOP.'
        IF(NCSIZE.GT.1) THEN
!         GLOBAL NUMBERING IF THERE IS A GUILTY POINT
          IF(IGUILT.GT.0) IGUILT=MESH3D%KNOLG%I(IGUILT)
!         ONLY THE MORE GUILTY PROCESSOR IS KEPT
          IF(ALFALOC.NE.P_MIN(ALFALOC)) IGUILT=0
!         RETRIEVING THE CORRESPONDING POINT
          IGUILT=P_MAX(IGUILT)
        ENDIF
        WRITE(LU,*) 'ALFA = ',ALFA,' GUILTY POINT = ',IGUILT
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(ALFA.LT.1.D0) THEN
!       PREPARING NEXT ITERATION:
!       FC IN FINSUB (F AT THE BEGINNING OF EVERY SUBITERATION)
        DO IPOIN=1,NPOIN3
          FINSUB(IPOIN)=FC(IPOIN)
        ENDDO
        GOTO 10
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(INFOR) THEN
        WRITE(LU,102) SCHCF,NITER
      ENDIF
!
102   FORMAT(' MURD3D OPTION: ',1I2,'    ',1I4,' ITERATIONS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END

