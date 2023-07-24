!                   *************************
                    SUBROUTINE INIT_TRANSPORT
!                   *************************
!
     &(TROUVE,DEBU,HIDING,NSICLA,NPOIN,
     & T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     & CHARR,QS_C,QSXC,QSYC,CALFA_CL,SALFA_CL,COEFPN,SLOPEFF,
     & SUSP,QS_S,QS,QSCL,QSCL_C,QSCL_S,QSCLXS,QSCLYS,
     & UNORM,U2D,V2D,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,HOULE,
     & AVAIL,ACLADM,UNLADM,KSP,KSR,KS,
     & ICF,HIDFAC,XMVS,XMVE,GRAV,VCE,HMIN,KARMAN,
     & ZERO,PI,AC,IMP_INFLOW_C,ZREF,ICQ,CSTAEQ,CSRATIO,
     & CMAX,CS,CS0,SECCURRENT,BIJK,
     & IELMT,FDM,XWC,FD90,SEDCO,VITCE,PARTHENIADES,VITCD,
     & U3D,V3D,CODE)
!
!***********************************************************************
! SISYPHE   V7P2                                   21/07/2011
!***********************************************************************
!
!brief
!
!history  JMH
!+        24/01/2008
!+
!+   TEST 'IF(CHARR.OR.SUSP)' ADDED AT THE END
!
!history  JMH
!+        16/09/2009
!+
!+   AVAIL(NPOIN,10,NSICLA)
!
!history  JMH
!+        07/12/2009
!+
!+   MODIFICATIONS FOR RESTART WITH WARNINGS WHEN A VARIABLE
!
!history  C. VILLARET (LNHE)
!+
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
!+
!history  C. VILLARET (LNHE)+ R. KOPMANN + U.MERKEL
!+
!+        20/03/2011
!+        V6P1
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!
!history  MAK (HRW)
!+        31/05/2012
!+        V6P2
!+  Include CSRATIO
!
!history  P TASSI (LNHE)
!+        18/06/2012
!+        V6P2
!+   updated version with HRW's development for Soulsby-van Rijn's concentration
!
!history  R KOPMANN (BAW)
!+        10/05/2016
!+        V7P2
!+ CALFA,SALFA dependent of grain classes
!
!history  F.CORDIER & P.TASSI (EDF-LNHE)
!+        12/09/2018
!+        V8P0
!+  CALCULATION OF SAND FRACTION CONTENT AT EACH NODE
!+  FOR WILCOCK AND CROWE FORMULA (2003)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| BIJK           |-->| COEFFICIENT OF THE BIJKER FORMULA
!| CALFA          |<->| COSINUS OF THE ANGLE BETWEEN MEAN FLOW AND TRANSPORT
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| CHARR          |-->| BEDLOAD
!| CMAX           |---| MAX(PARTHENIADES/SETTLING VELOCITY)
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| CS             |<->| CONCENTRATION AT TIME N
!| CS0            |-->| CONCENTRATION AT TIME 0
!| CSTAEQ         |<->| EQUILIBRIUM CONCENTRATION
!| CSRATIO        |<->| EQUILIBRIUM CONCENTRATION FOR SOULSBY-VAN RIJN EQ.
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| FD90           |-->| DIAMETER D90
!| FDM            |-->| DIAMETER DM FOR EACH CLASS
!| FW             |-->| WAVE FRICTION FACTOR
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HIDFAC         |-->| HIDING FACTOR FORMULAS
!| HIDING         |-->| HIDING FACTOR CORRECTION
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| HOULE          |-->| LOGICAL, FOR WAVE EFFECTS
!| ICF            |-->| BED-LOAD OR TOTAL LOAD TRANSPORT FORMULAS
!| ICQ            |-->| REFERENCE CONCENTRATION FORMULA
!| IELMT          |-->| NUMBER OF ELEMENTS
!| IMP_INFLOW_C   |-->| IMPOSED CONCENTRATION IN INFLOW
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KS             |-->| BED ROUGHNESS
!| KSP            |-->| BED SKIN ROUGHNESS
!| KSR            |-->| RIPPLE BED ROUGHNESS
!| MU             |<->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SEDIMENT CLASSES
!| PARTHENIADES   |-->| CONSTANT OF THE KRONE AND PARTHENIADES EROSION LAW (KG/M2/S)
!| PI             |-->| PI
!| QS             |<->| BEDLOAD TRANSPORT RATE
!| QSCL           |<->| SUSPENDED LOAD TRANSPORT RATE
!| QSCLXS         |<->| SUSPENDED LOAD TRANSPORT RATE FOR EACH CLASS X-DIRECTION
!| QSCLYS         |<->| SUSPENDED LOAD TRANSPORT RATE FOR EACH CLASS Y-DIRECTION
!| QSCL_C         |<->| BEDLOAD TRANSPORT RATE
!| QSCL_S         |<->| SUSPENDED LOAD TRANSPORT RATE
!| QSXS           |<->| SOLID DISCHARGE X (SUSPENSION)
!| QSYS           |<->| SOLID DISCHARGE Y (SUSPENSION)
!| QS_C           |-->| BEDLOAD TRANSPORT RATE
!| QS_S           |<->| SUSPENDED LOAD TRANSPORT RATE
!| SALFA          |<->| SINUS OF THE ANGLE BETWEEN TRANSPORT RATE AND CURRENT
!| SECCURRENT     |-->| LOGICAL, PARAMETRISATION FOR SECONDARY CURRENTS
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT
!| SUSP           |-->| LOGICAL, SUSPENSION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| T11            |<->| WORK BIEF_OBJ STRUCTURE
!| T12            |<->| WORK BIEF_OBJ STRUCTURE
!| T13            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| T9             |<->| WORK BIEF_OBJ STRUCTURE
!| THETAW         |-->| ANGLE BETWEEN WAVE AND CURRENT
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOBW           |-->| WAVE INDUCED SHEAR STRESS
!| TW             |-->| WAVE PERIOD
!| U2D            |<->| MEAN FLOW VELOCITY X-DIRECTION
!| UNLADM         |-->| MEAN DIAMETER OF ACTIVE STRATUM LAYER
!| UNORM          |<->| NORM OF THE MEAN FLOW VELOCITY
!| UW             |-->| ORBITAL WAVE VELOCITY
!| V2D            |<->| MEAN FLOW VELOCITY Y-DIRECTION
!| VCE            |-->| WATER VISCOSITY
!| VITCD          |-->| CRITICAL SHEAR VELOCITY FOR MUD DEPOSITION
!| VITCE          |-->| CRITICAL EROSION SHEAR VELOCITY OF THE MUD
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| WATER DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZERO           |-->| ZERO
!| ZREF           |-->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_TRANSPORT => INIT_TRANSPORT
!
      USE DECLARATIONS_SISYPHE, ONLY : NOMBLAY,MPM_ARAY,MPM
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NSICLA,NPOIN,TROUVE(*),ICQ
      INTEGER, INTENT(IN)              :: ICF,HIDFAC,IELMT,SLOPEFF
      LOGICAL, INTENT(IN)              :: CHARR,DEBU,SUSP,IMP_INFLOW_C
      LOGICAL, INTENT(IN)              :: SECCURRENT,SEDCO(*)
      LOGICAL, INTENT(IN)              :: HOULE
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U2D,V2D,UNORM,HN,CF
      TYPE(BIEF_OBJ),    INTENT(IN)    :: MU,TOB,TOBW,UW,TW,THETAW,FW
      TYPE(BIEF_OBJ),    INTENT(IN)    :: ACLADM,UNLADM,KSP,KSR,KS
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QS_C, QSXC, QSYC
      TYPE(BIEF_OBJ),    INTENT(INOUT) ::  CALFA_CL,SALFA_CL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T9,T10,T11,T12
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: ZREF,CSTAEQ,CSRATIO
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: CS
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QS_S,QS,QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: COEFPN
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QSCLXS,QSCLYS,QSCL
      DOUBLE PRECISION,  INTENT(IN)    :: XMVS,XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(IN)    :: HMIN,KARMAN,ZERO,PI
      DOUBLE PRECISION,  INTENT(IN)    :: PARTHENIADES,BIJK,XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(IN)    :: FD90(NSICLA),CS0(NSICLA)
      DOUBLE PRECISION,  INTENT(IN)    :: VITCE,VITCD
      DOUBLE PRECISION,  INTENT(INOUT) :: AC(NSICLA),CMAX,FDM(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
!
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
      DOUBLE PRECISION AAA,USTARP,U3DNORM
      LOGICAL NEED_CS
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION SANFRA(NPOIN)
      INTEGER K
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
! --- START : INITIALISES RATE OF TRANSPORT AND SUSPENSION
!
!     FOR INITIALISATION : SLOPE EFFECT AND DEVIATION ARE CANCELLED
!
!     RK in case of coupling with T3D, the direction should
!     come from the bottom velocity
!
!     Calculation of sand fraction content at each node (Wilcock and Crowe, 2003)
      IF (ICF == 10) THEN
        DO K = 1, NPOIN
          SANFRA(K) = 0.0D0
          DO I = 1, NSICLA
            IF (FDM(I).LT.2D-3) THEN
              SANFRA(K) = SANFRA(K) + AVAIL(K,1,I)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      IF(CODE(1:9).EQ.'TELEMAC3D') THEN
        DO I=1,NPOIN
          U3DNORM=SQRT(U3D%R(I)*U3D%R(I)+V3D%R(I)*V3D%R(I))
          IF(U3DNORM.GE.1.D-12) THEN
            CALFA_CL%ADR(1)%P%R(I)=U3D%R(I)/U3DNORM
            SALFA_CL%ADR(1)%P%R(I)=V3D%R(I)/U3DNORM
          ELSE
            CALFA_CL%ADR(1)%P%R(I)=1.D0
            SALFA_CL%ADR(1)%P%R(I)=0.D0
          ENDIF
        ENDDO
      ELSE
        CALL OS('X=Y/Z   ',X=CALFA_CL%ADR(1)%P, Y=U2D, Z=UNORM,
     &           C=0.D0, IOPT=2, INFINI=1.D0, ZERO=1.D-12)
        CALL OS('X=Y/Z   ',X=SALFA_CL%ADR(1)%P, Y=V2D, Z=UNORM,
     &           C=0.D0, IOPT=2, INFINI=0.D0, ZERO=1.D-12)
      ENDIF
      IF(NSICLA.GT.1) THEN
        DO I=2,NSICLA
          CALL OS('X=Y     ', X=CALFA_CL%ADR(I)%P,
     &            Y=CALFA_CL%ADR(1)%P)
          CALL OS('X=Y     ', X=SALFA_CL%ADR(I)%P,
     &            Y=SALFA_CL%ADR(1)%P)
        ENDDO
      ENDIF
!
!     appel a effpnt ?
!
      CALL OS('X=C     ',X=COEFPN,C=1.D0)
!
      IF(CHARR) THEN
!
!       MPM for each Layer
!
        CALL OS('X=C     ', X=MPM_ARAY, C=MPM)
!
        CALL OS('X=C     ',X=HIDING,C=1.D0)
!
        DO I = 1, NSICLA
!
          IF(SEDCO(I)) THEN
!           IF COHESIVE: NO BEDLOAD TRANSPORT
            CALL OS('X=0     ', X=QSCL_C%ADR(I)%P)
          ELSE
!           IF NON COHESIVE
            CALL BEDLOAD_FORMULA
     &        (U2D,V2D,UNORM,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,
     &        ACLADM, UNLADM,KSP,KSR,AVAIL(1:NPOIN,1,I),
     &        NPOIN,ICF,HIDFAC,XMVS,XMVE,
     &        FDM(I),GRAV,VCE,HMIN,XWC(I),FD90(I),KARMAN,ZERO,
     &        PI,SUSP,AC(I),HIDING,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     &        T11,T12,QSCL_C%ADR(I)%P,QSCL_S%ADR(I)%P,
     &        IELMT,SECCURRENT,SLOPEFF,
     &        COEFPN,CALFA_CL%ADR(I)%P,SALFA_CL%ADR(I)%P,
! FC
     &        BIJK,HOULE,SANFRA)
! ENDFC
!
          ENDIF
!         SUM ON ALL CLASSES
          DO J=1,NPOIN
            QS_C%R(J) = QS_C%R(J) + QSCL_C%ADR(I)%P%R(J)
!
!       COMPUTES THE X AND Y COMPONENTS OF TRANSPORT
            QSXC%R(J) = QSXC%R(J) + QSCL_C%ADR(I)%P%R(J)
     &                  *CALFA_CL%ADR(I)%P%R(J)
            QSYC%R(J) = QSYC%R(J) + QSCL_C%ADR(I)%P%R(J)
     &                  *SALFA_CL%ADR(I)%P%R(J)
          ENDDO
!
        ENDDO
!
!
      ENDIF
!
!     START : COMPUTES THE SUSPENDED TRANSPORT
!
      IF(SUSP) THEN
!
!       INITIALISES ZREF
!
        IF(ICQ.EQ.1) THEN
          CALL OS('X=Y     ', X=ZREF, Y=KSP)
        ELSEIF(ICQ.EQ.2) THEN
          CALL OS('X=Y     ', X=ZREF, Y=KSR)
        ELSEIF(ICQ.EQ.3) THEN
          CALL OS('X=CY    ', X=ZREF, Y=KS,C=0.5D0)
        ENDIF
!
!       FOR RANK OF CS IN TROUVE SEE POINT_SISYPHE, NOMVAR_SISYPHE
!       22+I+(NOMBLAY+1)*NSICLA IS THE ADDRESS OF CONCENTRATIONS
!
        NEED_CS=.FALSE.
        DO I=1,NSICLA
          IF(TROUVE(22+I+(NOMBLAY+1)*NSICLA).EQ.0) NEED_CS=.TRUE.
        ENDDO
!
!       COMPUTES THE INITIAL CONCENTRATIONS
!
        IF(.NOT.DEBU.OR.NEED_CS) THEN
!
          CALL CONDIM_SUSP(CS,CS0,NSICLA)
!
!         END MODIFICATIONS (CV)
!         OPTION: IMPOSED INFLOW CONCENTRATIONS ...
!
          IF(IMP_INFLOW_C) THEN
!
!           TAUP IN T8
            CALL OS('X=CYZ   ', X=T8, Y=TOB, Z=MU, C=1.D0)
!           USTAR (TOTAL) IN T9
            CALL OS('X=CY    ', X=T9, Y=TOB, C=1.D0/XMVE)
            CALL OS('X=SQR(Y)', X=T9, Y=T9)
!
!           START: LOOP ON THE CLASSES
!
            DO I=1,NSICLA
!
              IF(.NOT.SEDCO(I)) THEN
!
!               NON COHESIVE SED: INITIALISES CSTAEQ
!
                IF(ICQ.EQ.1) THEN
                  CALL SUSPENSION_FREDSOE( FDM(I) ,T8,NPOIN,
     &                GRAV, XMVE, XMVS, AC(I),  CSTAEQ )
                ELSEIF(ICQ.EQ.2) THEN
                  CALL SUSPENSION_BIJKER(T8,NPOIN,CHARR,QS_C,ZREF,
     &                                   ZERO,CSTAEQ,XMVE)
                ELSEIF(ICQ.EQ.3) THEN
                  CALL SUSPENSION_VANRIJN(FDM(I),T8,NPOIN,
     &               GRAV,XMVE,XMVS,VCE,ZERO,AC(I), CSTAEQ,ZREF)
                ELSEIF(ICQ.EQ.4) THEN
                  CSRATIO%R=1D0
                  CALL SUSPENSION_SANDFLOW(FDM(I),FD90(I),NPOIN,
     &                                     GRAV,XMVE,XMVS,ZERO,
     &                                     CSTAEQ,HN,U2D,V2D,
     &                                     CSRATIO)
                ENDIF
!               ROUSE CONCENTRATION PROFILE IS ASSUMED BASED ON TOTAL FRICTION
!               VELOCITY
!
                CALL SUSPENSION_ROUSE(T9,HN,NPOIN,
     &                             KARMAN,ZERO,XWC(I),ZREF,T12)
!
                DO J=1,NPOIN
                  CSTAEQ%R(J)=CSTAEQ%R(J)*AVAIL(J,1,I)
                ENDDO
!               CALL OS( 'X=XY    ',X=CSTAEQ,Y=AVAI%ADR(I)%P)
                CALL OS( 'X=Y/Z   ',X=CS%ADR(I)%P,Y=CSTAEQ,Z=T12)
!
!               END NON-COHESIVE
!
              ELSE
!
!               FOR COHESIVE SEDIMENT
!
!               THIS VALUE CAN BE ALSO CHANGED BY THE USER
!               IN SUBROUTINE USER_KRONE_PARTHENIADES
!
                CALL OS('X=Y     ', X=ZREF, Y=KSP)
!
                CMAX = MAX(CMAX,PARTHENIADES/XWC(I))
!
                IF(VITCE.GT.1.D-8.AND.VITCD.GT.1.D-8) THEN
                  DO J = 1, NPOIN
!                 FLUER
                  USTARP= SQRT(T8%R(J)/XMVE)
                  AAA= PARTHENIADES*
     &                MAX(((USTARP/VITCE)**2-1.D0),ZERO)
!                 FLUDPT
!                 BBB=XWC(I)*MAX((1.D0-(USTARP/VITCD)**2),ZERO)
!                 IF NO DEPOSITION, THE EQUILIBRIUM CONCENTRATION IS INFINITE!
                  CS%ADR(I)%P%R(J) = AAA/XWC(I)
!
                  ENDDO
                ELSE
                  CALL OS('X=0     ',X=CS%ADR(I)%P)
                ENDIF
!
                DO J=1,NPOIN
                  CS%ADR(I)%P%R(J)=CS%ADR(I)%P%R(J)*AVAIL(J,1,I)
                ENDDO
!
! END COHESIVE
!
              ENDIF
!
! END OF LOOP ON THE CLASSES
!
            ENDDO
!
! END OF OPTION: IMPOSED INFLOW CONCENTRATION
!
          ENDIF
!
! END OF IF(.NOT.DEBU.OR.NEED_CS.EQ.0)) THEN
!
        ENDIF
!
! COMPUTES SUSPENDED TRANSPORT
!
        DO I=1,NSICLA
!                                    UCONV DONE IN SUSPENSION_COMPUTATION
!                                    HERE WE USE
!                                    U2D AS TENTATIVE VALUE OF UCONV
          CALL OS('X=YZ    ',X=T11,Y=U2D, Z=HN)
          CALL OS('X=YZ    ',X=T12,Y=V2D, Z=HN)
!
          CALL OS('X=YZ    ',X=QSCLXS%ADR(I)%P,Y=CS%ADR(I)%P,Z=T11)
          CALL OS('X=YZ    ',X=QSCLYS%ADR(I)%P,Y=CS%ADR(I)%P,Z=T12)
!
          CALL OS('X=N(Y,Z) ',X=QSCL_S%ADR(I)%P,
     &                        Y=QSCLXS%ADR(I)%P,Z=QSCLYS%ADR(I)%P)
!
        ENDDO

          DO J=1,NPOIN
            DO I=1,NSICLA
            QS_S%R(J) = QS_S%R(J) + QSCL_S%ADR(I)%P%R(J)
            ENDDO
          ENDDO
      ENDIF
!
! END OF SUSPENSION
!
!
!     COMPUTES THE TRANSPORT FOR EACH CLASS (IF NOT RESTART OR IF
!                                              DATA NOT FOUND)
      DO I=1, NSICLA
        WRITE(LU,*) 'QSCL REINITIALISED IN INIT_TRANSPORT'
        WRITE(LU,*) 'FOR CLASS ',I
        IF(CHARR.AND.SUSP) THEN
          CALL OS('X=Y+Z   ', X=QSCL%ADR(I)%P,
     &            Y=QSCL_S%ADR(I)%P, Z=QSCL_C%ADR(I)%P)
        ELSEIF(CHARR) THEN
          CALL OS('X=Y     ',X=QSCL%ADR(I)%P,Y=QSCL_C%ADR(I)%P)
        ELSEIF(SUSP) THEN
          CALL OS('X=Y     ',X=QSCL%ADR(I)%P,Y=QSCL_S%ADR(I)%P)
        ENDIF
      ENDDO
!
!     COMPUTES TOTAL TRANSPORT QS
!
      WRITE(LU,*) 'QS REINITIALISED IN INIT_TRANSPORT'
      IF(CHARR.AND.SUSP) THEN
        CALL OS('X=Y+Z   ',X=QS,Y=QS_C,Z=QS_S)
      ELSEIF(CHARR) THEN
        CALL OS('X=Y     ',X=QS,Y=QS_C)
      ELSEIF(SUSP) THEN
        CALL OS('X=Y     ',X=QS,Y=QS_S)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
