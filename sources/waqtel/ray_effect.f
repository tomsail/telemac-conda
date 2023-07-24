!                   **********************
                    SUBROUTINE RAY_EFFECT
!                   **********************
!
     &(SECCHI,TRR,NPOIN,MEXT,I0,IK,KPE,EFF,H,T1,T2)
!
!***********************************************************************
! WAQTEL   V8P1
!***********************************************************************
!
!brief    COMPUTES RAY EFFECT: COEFFICIENT OF SUNSHINE ON
!                              THE GROWTH OF ALGAE
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EFF            |<--| SUNSHINE EFFECT ON ALGAE GROWTH
!| H              |-->| WATER DEPTH ON ALL MESH NODES
!| KPE            |-->| COEFFICIENT OF VEGETAL TURBIDITY WITHOUT
!|                |   | PHYTOPLANKTON
!| I0             |-->| PARAMETER FOR THE CALIBRATION OF SMITH FORMULA
!| IK             |-->| PARAMETER FOR THE CALIBRATION OF SMITH FORMULA
!| MEXT           |-->| METHOD OF RAY EXTINCTION
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| SECCHI         |-->| SECCHI DEPTH
!| TRR            |-->| TRACER (CAN BE PHY: PHYTOPLAKTONIC BIOMASS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL, ONLY : EXTINC
      USE INTERFACE_WAQTEL, EX_RAY_EFFCT => RAY_EFFECT
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,MEXT
      DOUBLE PRECISION, INTENT(IN)    :: KPE,I0,IK,SECCHI
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: H,TRR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: EFF,T1,T2
!   LOCAL VARIABLES
      INTEGER                    :: KK
      DOUBLE PRECISION, PARAMETER:: EPS=1.E-6
      DOUBLE PRECISION, PARAMETER:: MOSS=0.015D0
      DOUBLE PRECISION           :: CC,IK2,I02,CNUM
      INTRINSIC MAX,SQRT,LOG,EXP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     PRELIMINARIES
!
      IK2=IK**2
      I02=I0**2
      CNUM=I0+SQRT(I02+IK2)
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
!     COMPUTE Ih: STOCKED IN T2
!
      DO KK=1,NPOIN
        T2%R(KK)=I0*EXP(-T1%R(KK)*MAX(H%R(KK),0.D0))
      ENDDO
!
!     RAY EFFECT IS READY TO BE COMPUTED
!
!     warning: the formula of smith is depth integrated, here after a tentative
!              generalization, to be investigated later.
      DO KK=1,NPOIN
        CC=H%R(KK)*T1%R(KK)
        IF(CC.GT.EPS)THEN
          EFF%R(KK)=LOG(CNUM/(T2%R(KK)+SQRT(IK2+T2%R(KK)**2)))/CC
        ENDIF
      ENDDO
!
101    FORMAT(1X,'RAY_EFFECT: METHOD OF COMPUTATION OF THE COEFFICIENT',
     &      /,1X,'OF EXTINCTION OF SUN RAY NOT IMPLEMENTED YET :',I6/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
