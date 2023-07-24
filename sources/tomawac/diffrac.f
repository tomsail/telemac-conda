!                       ******************
                        SUBROUTINE DIFFRAC
!                       ******************
!
     &( CX    , CY    , CT    , XK    , CG    , NPOIN2, NDIRE ,
     &  IFF   , NF    , F     , RX    , RY    , RXX   , RYY   , NEIGB )
!
!***********************************************************************
! TOMAWAC   V7P0                                   25/06/2012
!***********************************************************************
!
!brief    COMPUTES DIFFRACTION.
!+
!+            COMPUTES THE DIFFRACTION TERM AND THE
!+            DIFFRACTION-CORRECTED GROUP VELOCITY
!
!history  E. KRIEZI (LNH)
!+        04/12/2006
!+        V5P5
!+
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modification for V6P2
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        10/12/2012
!+        V6P3
!+   4 subroutines GRAD-... inlined and removed from the tomawac library
!+   and then optimised.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/03/2013
!+        V6P3
!+   Now velocities modified, not fully computed.
!+   A preliminary call of conwac is requested.
!
!history  J-M HERVOUET (EDF - LNHE)
!+        08/01/2014
!+        V7P0
!+   CALL PARCOM suppressed by using new argument ASSPAR in VECTOR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSTET         |-->| COSINE OF TETA ANGLE
!| CX             |<->| ADVECTION FIELD ALONG X(OR PHI)
!| CY             |<->| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CT             |<->| ADVECTION FIELD ALONG TETA
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| DIFFRA         |-->| IF >0 DIFFRACTION IS CONSIDERED
!|                      IF = 1 MSE FORMULATION
!|                      IF = 2 RMSE FORMULATION
!| F              |<->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FLTDIF         |-->| IF TRUE, LOCAL AMPLITUDES ARE FILTERED
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| IFF            |-->| FREQUENCY INDEX
!| MAXNSP         |-->| CONSTANT FOR MESHFREE TECHNIQUE
!| NB_CLOSE       |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NEIGB          |-->| NEIGHBOUR POINTS FOR MESHFREE METHOD
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| RX             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RXX            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RY             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RYY            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| SINTET         |-->| SINE OF TETA ANGLE
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKONPT         |<--| ARRAY USED FOR COMPUTING DIFFRACTION PARAMETER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,ST1,ST0,IELM2,SA,MESH,
     &     FREQ  , COSTET, SINTET,  DFREQ, SPHE, PROINF, NBOR, NPTFR,
     &                                 SCCG,SDELTA,SXKONPT,SDDX,SDDY,
     &                                 SQRDELTA,SQRCCG,FRDK,FRDA,SCDA,
     &     L_DELTA,DEJA_DIFFRAC,
     &     OPTDER, FLTDIF, DIFFRA, NB_CLOSE, F2DIFM, AMPLI, DIV , MAXNSP
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_DIFFRAC => DIFFRAC
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NF,NDIRE,NPOIN2,IFF
      INTEGER, INTENT(IN) :: NEIGB(NPOIN2,MAXNSP)
      DOUBLE PRECISION, INTENT(INOUT) :: CX(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: CY(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: CT(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: RX(MAXNSP,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RY(MAXNSP,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RXX(MAXNSP,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RYY(MAXNSP,NPOIN2)

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, IP, IPOIN
      DOUBLE PRECISION CDELTA,DELTAN
!
!***********************************************************************
!
!     TODO: THERE ARE ENOUGH ARRAYS ELSEWHERE, THIS IS USELESS...
!
      IF(.NOT.DEJA_DIFFRAC)THEN
        ALLOCATE(SQRDELTA(NPOIN2))
        ALLOCATE(SQRCCG(NPOIN2))
        ALLOCATE(FRDK(NPOIN2,2))
        ALLOCATE(FRDA(NPOIN2,2))
        ALLOCATE(SCDA(NPOIN2,3))
        ALLOCATE(L_DELTA(NPOIN2))
        DEJA_DIFFRAC=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!     INFINITE WATER DEPTH ...
!-----------------------------------------------------------------------
!
      IF(PROINF) THEN
!
        WRITE(LU,*) ''
        WRITE(LU,*) '***************************************'
        WRITE(LU,*) ' ATTENTION : DIFFRACTION IS NOT TAKEN  '
        WRITE(LU,*) ' INTO ACCOUNT IN THE CASE OF INFINITE  '
        WRITE(LU,*) ' WATER DEPTH                           '
        WRITE(LU,*) '***************************************'
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!     FINITE DEPTH
!-----------------------------------------------------------------------
!
      ELSE
!
!     ------------------------------------------------------------------
!        ... CARTESIAN COORDINATES
!     ------------------------------------------------------------------
!
      IF(.NOT.SPHE) THEN
!
!     DIFFRACTION IS TAKEN INTO ACCOUNT
!
!     CCG VECTOR COMPUTATION
!
      DO IPOIN=1,NPOIN2
        SCCG%R(IPOIN) = CG(IPOIN,IFF)*DEUPI*FREQ(IFF)/XK(IPOIN,IFF)
        SXKONPT%R(IPOIN)=1.D0/(XK(IPOIN,IFF)**2)
        SQRCCG(IPOIN)=SQRT(ABS(SCCG%R(IPOIN)))
      ENDDO
!
!     INVERSE OF INTEGRALS OF TEST FUNCTIONS
!
      CALL VECTOR(ST0,'=','MASBAS          ',IELM2,1.D0,
     &            ST1,ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,ST1,
     &            ASSPAR=.TRUE.)
      CALL OS('X=1/Y   ',X=ST0,Y=ST0)
!
!     LOOP OVER THE DIRECTIONS
!
      DO IP = 1,NDIRE
!
!       COMPUTATION OF LOCAL AMPLITUDES OF DIRECTIONAL SPECTRA
!
        DO IPOIN = 1,NPOIN2
          AMPLI(IPOIN) = SQRT(2.D0*F(IPOIN,IP,IFF)*DFREQ(IFF)
     &         *DEUPI/NDIRE)
          IF(DIFFRA.EQ.2)THEN
            AMPLI(IPOIN)=AMPLI(IPOIN)*XK(IPOIN,IFF)*SQRCCG(IPOIN)
          ENDIF
        ENDDO
!
!       Filtering the local amplitudes of directional spectra
!
        IF(FLTDIF) CALL FILT_SA
!
        CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SA,
     &              ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &              ASSPAR=.TRUE.)
        DO I=1,NPOIN2
          FRDA(I,1)=ST1%R(I)*ST0%R(I)
        ENDDO
        CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SA,
     &              ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &              ASSPAR=.TRUE.)
        DO I=1,NPOIN2
          FRDA(I,2)=ST1%R(I)*ST0%R(I)
        ENDDO
!
!       DIFFRA=1 - Mean Slope Equation model
!       DIFFRA=2 - Revised Mean Slope Equation model
!
        IF(DIFFRA.EQ.1) THEN
!
          CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SCCG,
     &                ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &                ASSPAR=.TRUE.)
          DO I=1,NPOIN2
            FRDK(I,1)=ST1%R(I)*ST0%R(I)
          ENDDO
          CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SCCG,
     &                ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &                ASSPAR=.TRUE.)
          DO I=1,NPOIN2
            FRDK(I,2)=ST1%R(I)*ST0%R(I)
          ENDDO
!
        ELSE
!
          CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SXKONPT,
     &                ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &                ASSPAR=.TRUE.)
          DO I=1,NPOIN2
            FRDK(I,1)=ST1%R(I)*ST0%R(I)
          ENDDO
          CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SXKONPT,
     &                ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &                ASSPAR=.TRUE.)
          DO I=1,NPOIN2
            FRDK(I,2)=ST1%R(I)*ST0%R(I)
          ENDDO
!
        ENDIF
!
!
!       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       CALCUL DE D2A/DX2 + D2A/DY2 AVEC LA METHODE FREEMESH
!       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
        IF(OPTDER.EQ.1) THEN
!
          DO IPOIN = 1,NPOIN2
!           calculate first and second derivative of A ( FFD=A)
!           TODO: note JMH: pour first il y a .FALSE.  !!!
!                     et seul SCDA(IPOIN,3) est utilise
            CALL RPI_INTR(NEIGB,NB_CLOSE,
     &                    RX(1,IPOIN),RY(1,IPOIN),
     &                    RXX(1,IPOIN),RYY(1,IPOIN),
     &                    NPOIN2,IPOIN,MAXNSP,AMPLI,
     &                    FRDA(IPOIN,1),FRDA(IPOIN,2),
     &                    SCDA(IPOIN,1),SCDA(IPOIN,2),
     &                    SCDA(IPOIN,3),.FALSE.,.TRUE.)
        ENDDO
!
        ELSEIF(OPTDER.EQ.2) THEN
!
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         CALCUL DE D2A/DX2 + D2A/DY2 AVEC LA METHODE BETE
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!         FRDA not very convenient, copy required here...
!
          DO I=1,NPOIN2
            SDDX%R(I)=FRDA(I,1)
            SDDY%R(I)=FRDA(I,2)
          ENDDO
          CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SDDX,
     &                ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &                ASSPAR=.TRUE.)
          DO I=1,NPOIN2
            SCDA(I,3)=ST1%R(I)*ST0%R(I)
          ENDDO
          CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SDDY,
     &                ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &                ASSPAR=.TRUE.)
          DO I=1,NPOIN2
            SCDA(I,3)=SCDA(I,3)+ST1%R(I)*ST0%R(I)
          ENDDO
!
        ELSE
          WRITE(LU,*) 'OPTDER=',OPTDER,' NOT TREATED'
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       DIFFRA=1 - Mean Slope Equation model
!       DIFFRA=2 - Revised Mean Slope Equation model
!
        IF(DIFFRA.EQ.1)THEN
          DO IPOIN = 1,NPOIN2
            DIV(IPOIN)=SCCG%R(IPOIN)*SCDA(IPOIN,3)
     &              + FRDK(IPOIN,1)*FRDA(IPOIN,1)
     &              + FRDK(IPOIN,2)*FRDA(IPOIN,2)
          ENDDO
        ELSE
          DO IPOIN = 1,NPOIN2
            DIV(IPOIN)=SXKONPT%R(IPOIN)*SCDA(IPOIN,3)
     &               + FRDK(IPOIN,1)*FRDA(IPOIN,1)
     &               + FRDK(IPOIN,2)*FRDA(IPOIN,2)
          ENDDO
        ENDIF
!
!       Calculating Delta=div/A
!
        DO IPOIN = 1,NPOIN2
          L_DELTA(IPOIN)=.TRUE.
          IF(F(IPOIN,IP,IFF).LE.F2DIFM) THEN
            SDELTA%R(IPOIN) = 0.D0
            L_DELTA(IPOIN)=.FALSE.
            SQRDELTA(IPOIN) =1.D0
          ELSE
!           DIFFRA=1 - Mean Slope Equation model
!           DIFFRA=2 - Revised Mean Slope Equation model
            IF(DIFFRA.EQ.1) THEN
              SDELTA%R(IPOIN)=DIV(IPOIN)*SXKONPT%R(IPOIN)/
     &                     (SCCG%R(IPOIN)*AMPLI(IPOIN))
            ELSE
              SDELTA%R(IPOIN)=(DIV(IPOIN)/AMPLI(IPOIN))
            ENDIF
!
            IF(SDELTA%R(IPOIN).LE.-1.D0) THEN
!             TODO: JMH: discutable !!!!!!
              SQRDELTA(IPOIN) =1.D0
              L_DELTA(IPOIN)=.FALSE.
              SDELTA%R(IPOIN)= 0.D0
            ELSE
              SQRDELTA(IPOIN) = SQRT(1.D0+SDELTA%R(IPOIN))
              L_DELTA(IPOIN)=.TRUE.
            ENDIF
!           TODO: JMH: discutable !!!!!
            IF(SQRDELTA(IPOIN).LE.F2DIFM) THEN
              SQRDELTA(IPOIN) =1.D0
              L_DELTA(IPOIN)=.FALSE.
              SDELTA%R(IPOIN)= 0.D0
            ENDIF
          ENDIF
        ENDDO
!
        DO I = 1,NPTFR
          IPOIN = NBOR(I)
          L_DELTA(IPOIN)=.FALSE.
!         TODO: JMH: discutable !!!
          SDELTA%R(IPOIN)= 0.D0
        ENDDO
!
!       DELTA GRADIENT COMPUTATION
!
        CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SDELTA,
     &              ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &              ASSPAR=.TRUE.)
        CALL OV('X=YZ    ', X=SDDX%R, Y=ST1%R, Z=ST0%R, DIM1=NPOIN2)
        CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SDELTA,
     &              ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &              ASSPAR=.TRUE.)
        CALL OV('X=YZ    ', X=SDDY%R, Y=ST1%R, Z=ST0%R, DIM1=NPOIN2)
!
!       calculation of CG_n =CG(1+delta)^0.5
!       and of modified transfer rates Cx,Cy,Ctheta
!
        DO IPOIN=1,NPOIN2
          IF(L_DELTA(IPOIN)) THEN
            DELTAN = -SINTET(IP)*SDDY%R(IPOIN)+COSTET(IP)*SDDX%R(IPOIN)
            CDELTA = CG(IPOIN,IFF)/SQRDELTA(IPOIN)/2.D0
            CT(IPOIN,IP)=CT(IPOIN,IP)*SQRDELTA(IPOIN)-CDELTA*DELTAN
            CX(IPOIN,IP)=CX(IPOIN,IP)*SQRDELTA(IPOIN)
            CY(IPOIN,IP)=CY(IPOIN,IP)*SQRDELTA(IPOIN)
          ENDIF
        ENDDO
!
      ENDDO !    IP
!
!     ----------------------------------------------------------------
!       ... AND SPHERICAL COORDINATES
!     ----------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,*) ''
        WRITE(LU,*) '***************************************'
        WRITE(LU,*) ' ATTENTION : THE PRESENT VERSION OF    '
        WRITE(LU,*) ' TOMAWAC CANNOT SIMULATE DIFFRACTION   '
        WRITE(LU,*) ' WHEN SPHERICAL COORDINATES ARE SET    '
        WRITE(LU,*) '***************************************'
        CALL PLANTE(1)
        STOP
!
! ENDIF (Finite depth)
      ENDIF
!ENDIF (Cartesian coordinates)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
