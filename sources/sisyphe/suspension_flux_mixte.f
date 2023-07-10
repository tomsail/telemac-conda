!                   ********************************
                    SUBROUTINE SUSPENSION_FLUX_MIXTE
!                   ********************************
!
     &(TAUP,FDM,NPOIN,CHARR,XMVE,XMVS,VCE,GRAV,XWC,
     & ZERO,PARTHENIADES,FLUER_SABLE,FLUER_VASE,ZREF,
     & AC,CSTAEQ,QSC,ICQ,DEBUG,AVAIL,NSICLA,ES,
     & TOCE_VASE,TOCE_SABLE,NOMBLAY,DT,TOCE_MIXTE,MS_SABLE,MS_VASE)
!
!***********************************************************************
! SISYPHE   V7P0                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE FLUX OF DEPOSITION AND EROSION.
!
!history  C. VILLARET, JMH
!+        2008
!+
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
!history  C. VILLARET (EDF-LNHE)
!+        20/03/2011
!+        V6P1
!+  Change of arguments FDM insteam of ACLADM
!+   KARMAN suppressed
!+   Added TOCE _ SABLE + VCE
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
!+
!
!history  J-M HERVOUET (EDFLAB, LNHE)
!+        28/04/2014
!+        V7P0
!+   Possible divisions by 0 secured. Formatting.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| FDM            |-->| DIAMETER DM FOR EACH CLASS
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CHARR          |-->| BEDLOAD
!| CS             |<->| CONCENTRATION AT TIME N
!| CSTAEQ         |<->| EQUILIBRIUM CONCENTRATION
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DT             |-->| TIME STEP
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| FLUER_SABLE    |<->| EROSION FLUX FOR MIXED SEDIMENTS
!| FLUER_VASE     |<->| EROSION FLUX FOR MIXED SEDIMENTS
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| ICQ            |-->| REFERENCE CONCENTRATION FORMULA
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NOMBLAY        |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| PARTHENIADES   |-->| CONSTANT OF THE KRONE AND PARTHENIADES EROSION LAW (KG/M2/S)
!| QSC            |<->| BEDLOAD TRANSPORT RATE
!| TAUP           |-->| CRITICAL SHEAR STRESS
!| TOCE_MIXTE     |<->| CRITICAL SHEAR STRESS FOR MIXED SEDIMENTS
!| TOCE_SABLE     |<->| CRITICAL SHEAR STRESS FOR SAND
!| TOCE_VASE      |<->| CRITICAL EROSION SHEAR STRESS OF THE MUD PER LAYER (N/M2)
!| VCE            |-->| FLOW VISCOSITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| WATER DENSITY
!| XWC            |-->| SETTLING VELOCITIES
!| ZERO           |-->| ZERO
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_FLUX_MIXTE=>SUSPENSION_FLUX_MIXTE
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NLAYMAX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (BIEF_OBJ),  INTENT(IN)     :: TAUP
      INTEGER,          INTENT(IN)     :: NPOIN,DEBUG,NSICLA
      INTEGER,          INTENT(IN)     :: NOMBLAY
      LOGICAL,          INTENT(IN)     :: CHARR
      DOUBLE PRECISION, INTENT(IN)     :: XMVE, XMVS, VCE,GRAV
      DOUBLE PRECISION, INTENT(IN)     :: XWC
      DOUBLE PRECISION, INTENT(IN)     :: ZERO, PARTHENIADES
      TYPE (BIEF_OBJ),  INTENT(IN)     :: ZREF
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT)  :: AC
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      TYPE (BIEF_OBJ),  INTENT(INOUT)  :: CSTAEQ
      TYPE (BIEF_OBJ),  INTENT(INOUT)  :: FLUER_SABLE,FLUER_VASE
      DOUBLE PRECISION, INTENT(INOUT)  :: MS_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: TOCE_MIXTE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: DT, FDM
      TYPE(BIEF_OBJ),   INTENT(IN)     ::  QSC
      INTEGER,          INTENT (IN)    :: ICQ
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_SABLE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, J
      DOUBLE PRECISION FLUERSABLE,FLUERVASE,FLUER_LOC(NLAYMAX)
!
      DOUBLE PRECISION QE_MOY,TEMPS,QER_VASE,QER_SABLE
      DOUBLE PRECISION F2,DETER
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     DOES THE EROSION COMPUTATION ONLY ONCE (SAND FOR EXAMPLE
!     BECAUSE THE COMPUTED FLUX IS A GLOBAL FLUX COMMON TO THE 2 SEDIMENTS)
!     COMPUTES THE THEORETICAL FLUX OF EROSION FOR EACH (SEDIMENT INFINITELY
!     AVAILABLE IN EACH LAYER)
!
!     COMPUTES THE CRITICAL STRESS FOR EACH LAYER AS A FUNCTION
!     OF THE PROPORTION OF MUD
!
      DO J=1,NOMBLAY
        DO I=1,NPOIN
!         WRITE(LU,*)'I=',I,' J=',J,' (MS_VASE(I, J)=',MS_VASE(I,J)
!         WRITE(LU,*)'I=',I,' J=',J,' (MS_SABLE(I, J)=',MS_SABLE(I,J)
          DETER=MS_VASE(I,J) + MS_SABLE(I,J)
          IF(DETER.GT.1.D-20) THEN
            F2=MS_VASE(I, J)/DETER
          ELSE
            F2=0.5D0
          ENDIF
!         F2= MS_VASE(I, J)/(MS_VASE(I, J) + MS_SABLE(I, J))
          IF(F2.LE.0.3D0) THEN
            TOCE_MIXTE(I,J)=TOCE_SABLE
          ELSEIF(F2.GE.0.5D0)THEN
            TOCE_MIXTE(I,J)=TOCE_VASE(J)
          ELSE
            TOCE_MIXTE(I,J)=TOCE_SABLE +
     &   (F2-0.3D0)*(TOCE_VASE(J)-TOCE_SABLE)/(0.5D0-0.3D0)
          ENDIF
        ENDDO
      ENDDO
!
      IF(ICQ.EQ.1) THEN
        IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_FREDSOE'
        CALL SUSPENSION_FREDSOE(FDM,TAUP,NPOIN,
     &                           GRAV,XMVE,XMVS,AC,CSTAEQ)
        IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_FREDSOE'
!
        DO I=1,NPOIN
          CSTAEQ%R(I)=CSTAEQ%R(I)*AVAIL(I,1,1)
        ENDDO
!
      ELSEIF(ICQ.EQ.2) THEN
!
        IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BIJKER'
        CALL SUSPENSION_BIJKER(TAUP,NPOIN,CHARR,QSC,ZREF,
     &                         ZERO,CSTAEQ,XMVE)
        IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_BIJKER'
!
      ELSEIF(ICQ.EQ.3) THEN
        IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_VANRIJN'
        CALL SUSPENSION_VANRIJN(FDM,TAUP,NPOIN,
     &                          GRAV,XMVE,XMVS,VCE,
     &                          ZERO,AC,CSTAEQ,ZREF)
        IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_VANRIJN'
        DO I=1,NPOIN
          CSTAEQ%R(I)=CSTAEQ%R(I)*AVAIL(I,1,1)
        ENDDO
!
      ENDIF
!
      DO I=1,NPOIN
!
        DO J=1,NOMBLAY
!
!         COMPUTES FLUER_SABLE_VASE AS A FUNCTION OF THE PROPORTION OF MUD
!
          DETER=MS_VASE(I,J) + MS_SABLE(I,J)
          IF(DETER.GT.1.D-20) THEN
            F2=MS_VASE(I, J)/DETER
          ELSE
            F2=0.5D0
          ENDIF
!         F2= MS_VASE(I, J)/(MS_VASE(I, J) + MS_SABLE(I, J))
          IF(F2.LE.0.3D0) THEN
!           PROPORTION OF MUD < 30%, FLUXES ARE SIMILAR TO THOSE FOR SAND ONLY
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
              FLUER_LOC(J)=CSTAEQ%R(I)*XWC
            ELSE
              FLUER_LOC(J)=0.D0
            ENDIF
          ELSEIF(F2.GE.0.5D0) THEN
!           PROPORTION OF MUD > 50%, FLUXES ARE SIMILAR TO THOSE FOR MUD ONLY
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
              FLUER_LOC(J)=PARTHENIADES*
     &              ((TAUP%R(I)/TOCE_MIXTE(I,J))-1.D0)
            ELSE
              FLUER_LOC(J)=0.D0
            ENDIF
          ELSE
!           PROPORTION OF MUD >30% AND <50%, INTERPOLATES THE FLUXES
!           AND CRITICAL SHEAR STRESS
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J)) THEN
              FLUERSABLE=CSTAEQ%R(I)*XWC
              FLUERVASE=PARTHENIADES*(TAUP%R(I)/TOCE_MIXTE(I,J)-1.D0)
            ELSE
              FLUERSABLE=0.D0
              FLUERVASE=0.D0
            ENDIF
              FLUER_LOC(J)=(F2-0.3D0)/
     &           (0.5D0-0.3D0)*(FLUERVASE-FLUERSABLE)+FLUERSABLE
          ENDIF
        ENDDO
!
!       COMPUTES THE EROSION DEPTH ZER_MOY AND ERODED MASSES
!
        QER_VASE = 0.D0
        QER_SABLE = 0.D0
!
        TEMPS= DT
!
        DO J= 1, NOMBLAY
          IF(ES(I,J).GE.1.D-6) THEN
!           COMPUTES THE MASS POTENTIALLY ERODABLE IN LAYER J (KG/M2)
            QE_MOY= FLUER_LOC(J) *XMVS * TEMPS
            IF(QE_MOY.LT.MS_SABLE(I,J)+MS_VASE(I,J)) THEN
              QER_VASE = QER_VASE
     &     +  QE_MOY*MS_VASE(I,J)/(MS_VASE(I,J)+MS_SABLE(I,J))
              QER_SABLE = QER_SABLE
     &     +  QE_MOY*MS_SABLE(I,J)/(MS_VASE(I,J)+MS_SABLE(I,J))
              GO TO 10
            ELSE
              QER_VASE = QER_VASE + MS_VASE(I,J)
              QER_SABLE = QER_SABLE + MS_SABLE(I,J)
              TEMPS= TEMPS -
     &        (MS_SABLE(I,J)+MS_VASE(I,J))/FLUER_LOC(J)/XMVS
            ENDIF
          ENDIF
        ENDDO
        WRITE(LU,*) 'BEWARE, ALL LAYERS ARE EMPTY'
        CALL PLANTE(1)
!       STOP
10      CONTINUE
!
!       Q_VASE REPRESENTS THE SURFACE MASS OF MUD
!       TO BE ERODED TO REACH ZER_MOY
!       Q_SABLE REPRESENTS THE SURFACE MASS OF SAND
!       TO BE ERODED TO REACH ZER_MOY
!
        FLUER_VASE%R(I)  = QER_VASE /(DT*XMVS)
        FLUER_SABLE%R(I) = QER_SABLE/(DT*XMVS)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
