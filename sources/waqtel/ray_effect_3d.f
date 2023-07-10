!                   ************************
                    SUBROUTINE RAY_EFFECT_3D
!                   ************************
!
     &(SECCHI,TRR,NPOIN2,NPLAN,MEXT,I0,IK,KPE,EFF,ZPROP,T1,T2,T3)
!
!***********************************************************************
! WAQTEL   V8P4
!***********************************************************************
!
!brief    COMPUTES RAY EFFECT IN 3D: COEFFICIENT OF SUNSHINE ON
!                                    THE GROWTH OF ALGAE
!
!history  C.-T. PHAM (LNHE)
!+        20/08/2022
!+        V8P4
!+        Creation from ray_effect
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EFF            |<--| SUNSHINE EFFECT ON ALGAE GROWTH
!| KPE            |-->| COEFFICIENT OF VEGETAL TURBIDITY WITHOUT
!|                |   | PHYTOPLANKTON
!| I0             |-->| PARAMETER FOR THE CALIBRATION OF SMITH FORMULA
!| IK             |-->| PARAMETER FOR THE CALIBRATION OF SMITH FORMULA
!| MEXT           |-->| METHOD OF RAY EXTINCTION
!| NPLAN          |-->| NUMBER OF VERTICAL PLANES
!| NPOIN2         |-->| NUMBER OF NODES IN THE 2D MESH
!| SECCHI         |-->| SECCHI DEPTH
!| T1,..,T3       |<->| 3D WORKING STRUCTURES
!| TRR            |-->| TRACER (CAN BE PHY: PHYTOPLAKTONIC BIOMASS)
!| ZPROP          |-->| Z COORDINATES FOR NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL, ONLY : EXTINC
      USE INTERFACE_WAQTEL, EX_RAY_EFFECT_3D => RAY_EFFECT_3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN2,NPLAN,MEXT
      DOUBLE PRECISION, INTENT(IN)    :: KPE,I0,IK,SECCHI,ZPROP(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: TRR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: EFF,T1,T2,T3
!   LOCAL VARIABLES
      INTEGER K,I,IPLAN
      DOUBLE PRECISION, PARAMETER:: EPS=1.E-6
      DOUBLE PRECISION, PARAMETER:: MOSS=0.015D0
      DOUBLE PRECISION           :: CC,IK2,CNUM
      INTRINSIC MAX,SQRT,LOG,EXP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     PRELIMINARIES
!
      IK2=IK**2
      CNUM=I0+SQRT(I0**2+IK2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INITIALISATION
!
      CALL OS( 'X=0     ' ,X=EFF)
!
!     COMPUTE KE AND PUT IT IN T1
!
      IF(MEXT.EQ.1)THEN
!       ATKINS METHOD
        CC=1.7D0/MAX(SECCHI,EPS)
        CALL OS( 'X=C     ' ,X=T1,       C=CC  )
      ELSEIF(MEXT.EQ.2)THEN
!       MOSS METHOD
        CALL OS( 'X=CY     ' ,X=T1,Y=TRR,C=MOSS)
        CALL OS( 'X=X+C    ' ,X=T1,      C=KPE )
      ELSEIF(MEXT.EQ.3)THEN
!       GIVEN CONSTANT
        CALL OS( 'X=C      ' ,X=T1,      C=EXTINC )
      ELSE
        WRITE(LU,101) MEXT
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,NPOIN2
        DO IPLAN=1,NPLAN
          K = I+(IPLAN-1)*NPOIN2
          T3%R(K)=ZPROP(I+(NPLAN-1)*NPOIN2)-ZPROP(K)
!         COMPUTE IH: STORED IN T2
          T2%R(K)=I0*EXP(-T1%R(K)*T3%R(K))
          CC=T3%R(K)*T1%R(K)
          IF(CC.GT.EPS) THEN
            EFF%R(K)=LOG(CNUM/(T2%R(K)+SQRT(IK2+T2%R(K)**2)))/CC
          ENDIF
        ENDDO
      ENDDO
!
101    FORMAT(1X,'RAY_EFFECT: METHOD OF COMPUTATION OF THE COEFFICIENT',
     &      /,1X,'OF EXTINCTION OF SUN RAY NOT IMPLEMENTED YET :',I6/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
