!                   ******************************
                    SUBROUTINE SOURCE_HYDRO_ICOVER
!                   ******************************
     &(NPOIN,FU,FV,H,U,V,T1,T2,T3,S,MESH,MSK,MASKEL,GRAV,
     & KARMAN,CHESTR,DT,AT,KFROT,T4,T5,T6,CF,UNSV2D,IFV)
!
!***********************************************************************
! KHIONE   V7P2                                             02/11/2016
!***********************************************************************
!
!brief    COMPUTES CONTRIBUTION TO MOMENTUM FORCES AND WATER LEVEL
!+        TERMS RESULTING FROM ICE PROCESSES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT         |-->| TIME IN SECONDS
!| CF         |-->| ADIMENSIONNAL BOTTOM FRICTION
!| CHESTR     |-->| FRICTION COEFFICIENT
!| DT         |-->| TIME STEP
!| FU         |<->| SOURCE TERMS ON VELOCITY U
!| FV         |<->| SOURCE TERMS ON VELOCITY V
!| GRAV       |-->| GRAVITY
!| H          |-->| WATER DEPTH
!| KARMAN     |-->| VON KARMAN'S CONSTANT
!| KFROT      |-->| BOTTOM FRICTION LAW FROM TELEMAC
!| NPOIN      |-->| NUMBER OF NODES IN THE MESH
!| MESH       |<->| MESH STRUCTURE
!| MSK        |-->| IF YES, THERE IS MASKED ELEMENTS
!| MASKEL     |-->| MASKING OF ELEMENTS
!|            |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| T1         |<->| WORKING ARRAY
!| T2         |<->| WORKING ARRAY
!| T3         |<->| WORKING ARRAY
!| T4         |<->| WORKING ARRAY
!| T5         |<->| WORKING ARRAY
!| T6         |<->| WORKING ARRAY
!| U          |-->| X COMPONENT OF THE VELOCITY
!| V          |-->| Y COMPONENT OF THE VELOCITY
!| WINDX      |<->| FIRST COMPONENT OF WIND VELOCITY
!| WINDY      |<->| SECOND COMPONENT OF WIND VELOCITY
!| UNSV2D     |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| IFV        |-->| 1 IF FINITE VOLUME, 0 ELSE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY: ICOVER_IMPACT,BD_ICE,RHO_AIR,FICE,
     &                               FICE_MAX,VZ,IFROT,IFICE,ICESTR,
     &                               THIE,THIFEM,THIFEMS,RO0,IFRIC,
     &                               RHO_ICE,DCOVX,DCOVY,IPRES
!
      USE METEO_TELEMAC,       ONLY: SYNC_METEO,WINDX,WINDY,PATMOS
      USE INTERFACE_KHIONE,EX_SOURCE_HYDRO_ICOVER => SOURCE_HYDRO_ICOVER
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN,KFROT,IFV
      LOGICAL,          INTENT(IN)    :: MSK
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT,GRAV,KARMAN
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FU,FV
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CHESTR,H,U,V,CF,S
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,UNSV2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: SP_EAU,ALPHAB,NB,NI,AUXP
      DOUBLE PRECISION            :: VMAG,WMAG,CD,CSTAR,CWSTAR,VZ31
      DOUBLE PRECISION            :: VZ32,UNTIER,UNSIX
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!=======================================================================
!
!     FOR GOOD MEASURE - SHOULD BE DONE AT THE TELEMAC-2D LEVELa
!
!-----------------------------------------------------------------------
!
!     NOTE THAT "AT" FROM PROSOU IS AREADY TOO FAR GONE
      CALL SYNC_METEO(AT-DT)
!
!
!=======================================================================
!
!     STATIC BORDER ICE GROWTH
!
!-----------------------------------------------------------------------
!
      IF( BD_ICE ) THEN
!
!-----------------------------------------------------------------------
!
!       PREPARATION TO STATIC BORDER ICE GROWTH
!
        CSTAR  = 0.25D0
        CWSTAR = 1.D0
        CD     = 1.3D-3
        UNTIER = 1.D0/3.D0
        UNSIX  = 1.D0/6.D0
!
        DO I = 1,NPOIN
!
! ~~>     WIND SPEED EFFECTS ON ICE
          WMAG = SQRT( WINDX%R(I)**2 + WINDY%R(I)**2 )
! ~~>     FLOW SPEED EFFECTS ON ICE
          VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!
! ~~>     VERTICAL TURBULENT INTENSITY /!\ REPLACE BY CF%R (TODO)
          VZ31 = ( SQRT( CSTAR*GRAV ) * VMAG*CHESTR%R(I)
     &             / MAX( H%R(I), 0.05D0 )**UNSIX)**3
          VZ32 = CWSTAR * (CD*RHO_AIR/RO0)**(1.5D0)*WMAG**3
          VZ%R(I) = (VZ31 + VZ32)**UNTIER
!
        ENDDO
!
      ENDIF
!
!=======================================================================
!
!     ICE COVER IMPACT ON HYDRODYNAMICS
!
!-----------------------------------------------------------------------
!
      IF( ICOVER_IMPACT ) THEN
!
!       PRESSURE INCREASE DUE TO ICE THICKNESS
!       **************************************
!
!       IF FINITE VOLUME FORCE EXPLICIT FORMULATION
        IF (IPRES.EQ.2.AND.IFV.EQ.1) THEN
          IPRES = 1
        ENDIF
!
! ~~>   EXPLICIT IMPLEMENTATION
        IF (IPRES.EQ.1) THEN
          CALL VECTOR(DCOVX,'=','GRADF          X',U%ELM,
     &                1.D0,THIFEM,S,S,S,S,S,MESH,MSK,MASKEL)
          CALL VECTOR(DCOVY,'=','GRADF          Y',U%ELM,
     &                1.D0,THIFEM,S,S,S,S,S,MESH,MSK,MASKEL)
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM(DCOVX,2,MESH)
            CALL PARCOM(DCOVY,2,MESH)
          ENDIF
!         UPDATE SOURCE TERM
          AUXP = GRAV*RHO_ICE/RO0
          DO I=1,NPOIN
            IF(H%R(I).GT.EPS.AND.THIFEM%R(I).GT.EPS) THEN
              FU%R(I) = FU%R(I) - AUXP*DCOVX%R(I)*UNSV2D%R(I)
              FV%R(I) = FV%R(I) - AUXP*DCOVY%R(I)*UNSV2D%R(I)
            ENDIF
          ENDDO
!
! ~~>   IMPLICIT IMPLEMENTATION USING PATMOS (SEE PROPAG)
        ELSE IF (IPRES.EQ.2) THEN
          DO I=1,NPOIN
            IF(H%R(I).GT.EPS.AND.THIFEM%R(I).GT.EPS) THEN
              PATMOS%R(I) = PATMOS%R(I) + GRAV*RHO_ICE*THIFEM%R(I)
            ENDIF
          ENDDO
        ENDIF
!
!       FRICTION IMPACT
!       ***************
!
!       COMPUTATION OF SOLID ICE THICKNESS
        CALL OS( 'X=Y     ', X=T2,Y=THIFEMS )
!
!       UPDATE FRICTION IF LINEAR FRICTION COEF LAW IS SELECTED
        IF (IFICE .EQ. 1) THEN
          IF(IFROT.EQ.4) THEN
            CALL OS( 'X=CY    ', X=ICESTR, Y=T2, C=( FICE/THIE ) )
            CALL OS( 'X=+(Y,C)', X=ICESTR, Y=ICESTR, C=FICE )
            CALL OS( 'X=-(Y,C)', X=ICESTR, Y=ICESTR, C=FICE_MAX )
          ELSE
            WRITE(LU,*) 'IF LINEAR FRICTION IS SELECTED'
            WRITE(LU,*) 'PLEASE SELECT MANNING LAW'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
        IF(IFRIC.EQ.1) THEN ! ICE FRICTION ON THE TOTAL WATER DEPTH
!
          CALL FRICTION_KHIONE(NPOIN,IFROT,GRAV,KARMAN,ICESTR,T1,H,U,V)
!
!         COMPUTATION WATER ELEVATION
          CALL OS( 'X=+(Y,C)', X=T3, Y=H, C=EPS )
!
! ~~>     EFFECT OF THE ICE COVER ON THE HYDRODYNAMICS
          DO I = 1,NPOIN
!           T1: ICE FRICTION COEFFICIENT
!           T2: TOTAL ICE THICKNESS
!           T3: WATER DEPTH
!
            IF(T2%R(I).GT.EPS) THEN
              SP_EAU = SQRT( U%R(I)**2 + V%R(I)**2 )
              IF( H%R(I).LT.EPS ) SP_EAU =
     &          MAX( SP_EAU, SQRT(GRAV*(EPS-H%R(I))*H%R(I)/EPS) )
!
! ~~>         UPDATE SOURCE TERM WITH ICE COVER FRICTION
              FU%R(I) = FU%R(I) - 0.5D0*T1%R(I)*SP_EAU*U%R(I) / T3%R(I)
              FV%R(I) = FV%R(I) - 0.5D0*T1%R(I)*SP_EAU*V%R(I) / T3%R(I)
!
            ENDIF
          ENDDO
!
        ELSEIF(IFRIC.EQ.2) THEN!ICE AND BOTTOM FRICTION WITH RATIO(SHEN)
!
!         ONLY MANNING AND STRICKLER TAKEN INTO ACCOUNT
          IF((IFROT.NE.4.AND.IFROT.NE.3).OR.
     &       (KFROT.NE.4.AND.KFROT.NE.3)) THEN
            WRITE(LU,*) 'ICE FRICTION WITH RATIOS'
            WRITE(LU,*) 'PLEASE USE MANNING OR STRICKLER LAW'
            CALL PLANTE(1)
            STOP
          ENDIF
!
          DO I=1,NPOIN
!           COMPUTE ICE MANNING
            IF(IFROT.EQ.3) THEN
              NI = 1.D0/ICESTR%R(I)
            ELSE
              NI = ICESTR%R(I)
            ENDIF
!           COMPUTE BOTTOM MANNING
            IF(KFROT.EQ.3) THEN
              NB = 1.D0/CHESTR%R(I)
            ELSE
              NB = CHESTR%R(I)
            ENDIF
!
            IF(T2%R(I).GT.EPS) THEN
!             COMPUTATION OF DEPTH RATIOS
!             T4 : ICE RATIO
!             T5 : BOTTOM RATIO
              ALPHAB = (NI/NB)**1.5D0
              ALPHAB = 1.D0/(1.D0+ALPHAB)
              T4%R(I) = (1.D0-ALPHAB)*H%R(I)
              T5%R(I) = ALPHAB*H%R(I)
            ELSE
              T4%R(I) = 0.D0
              T5%R(I) = 0.D0
            ENDIF
          ENDDO
!         COMPUTATION OF FRICTIONS
!         T1 : ICE FRICTION COEFF
!         T6 : BOTTOM FRICTION COEFF
          CALL FRICTION_KHIONE(NPOIN,IFROT,GRAV,KARMAN,ICESTR,T1,T4,
     &                             U,V)
          CALL FRICTION_KHIONE(NPOIN,KFROT,GRAV,KARMAN,CHESTR,T6,T5,
     &                             U,V)
!
!         COMPUTES DEPTH + EPS
!
          CALL OS( 'X=+(Y,C)', X=T4, Y=T4, C=EPS )
          CALL OS( 'X=+(Y,C)', X=T5, Y=T5, C=EPS )
!
!         COMPUTES FRICTION FORCES
!
          DO I=1,NPOIN
            IF(T2%R(I).GT.EPS) THEN
              SP_EAU = SQRT( U%R(I)**2 + V%R(I)**2 )
              IF( H%R(I).LT.EPS ) SP_EAU =
     &          MAX( SP_EAU, SQRT(GRAV*(EPS-H%R(I))*H%R(I)/EPS) )
!
! ~~>         UPDATE SOURCE TERM WITH ICE COVER FRICTION
              FU%R(I) = FU%R(I) - 0.5D0*T1%R(I)*SP_EAU*U%R(I) / T4%R(I)
              FV%R(I) = FV%R(I) - 0.5D0*T1%R(I)*SP_EAU*V%R(I) / T4%R(I)
! ~~>         UPDATE SOURCE TERM WITH BOTTOM FRICTION
              FU%R(I) = FU%R(I) - 0.5D0*T6%R(I)*SP_EAU*U%R(I) / T5%R(I)
              FV%R(I) = FV%R(I) - 0.5D0*T6%R(I)*SP_EAU*V%R(I) / T5%R(I)
! ~~>         UPDATE COEFFICIENT WITH BOTTOM FRICTION CORRECTION
              CF%R(I) = 0.D0
!
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF ! ICE COVER IMPACT ON HYDRO
!
!=======================================================================
!
!     CLOGGING
!
!     NO IMPACT ON THE HYDRODYNAMICS
!
!     TODO: Implement CLOGGING=TRUE
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END
