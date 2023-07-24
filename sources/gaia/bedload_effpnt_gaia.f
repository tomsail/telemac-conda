!                 ******************************
                  SUBROUTINE BEDLOAD_EFFPNT_GAIA
!                 ******************************
!
     & (MASKEL,LIQBOR,S,ZF,NPOIN,NPTFR,IELMT,KENT,
     &  BETA,PI,MSK,MESH,DZFDX,DZFDY,CTETA,STETA,
     &  COEF,COEFCR, CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     &  TOB,XMVS,XMVE,DCLA,GRAV,UNSV2D)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the parameters of the slope effect.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     BETA    Coefficient for sloping bed effect
!!                       (koch and flokstra)
!>@param[in]     BETA2   Coefficient for the deviation  (talmon et al.)
!>@param[in,out] CALFA   Cosinus of the angle between transport rate and
!!                       x-axis
!>@param[in,out] COEF    Correction coefficient for the intensity of
!!                       bedload transport
!>@param[in,out] COEFCR  Correction coefficient due to slope for the critical 
!!                       Shields parameter 
!>@param[in,out] CTETA   Cosinus of the angle between mean flow and x-axis
!>@param[in]     DEVIA   Slope effect formula for deviation
!>@param[in]     DCLA    Sediment grain diameter
!>@param[in,out] DZFDX   Bottom slope in the x-direction
!>@param[in,out] DZFDY   Bottom slope in the y-direction
!>@param[in]     GRAV    Acceleration of gravity
!>@param[in]     IELMT   Number of elements
!>@param[in]     KENT    Convention for liquid input with prescribed value
!>@param[in]     LIQBOR  Type of boundary condition for qs
!>@param[in]     MASKEL  Masking of elements
!>@param[in,out] MESH    Mesh structure
!>@param[in]     MSK     If yes, there is masked elements
!>@param[in]     NPOIN   Number of points
!>@param[in]     NPTFR   Number of boundary points
!>@param[in]     PHISED  Angle of repose of the sediment
!>@param[in]     PI      Pi
!>@param[in]     S       Void structure
!>@param[in,out] SALFA   Sinus of the angle between transport rate and
!!                       x-axis
!>@param[in]     SLOPEFF Formula for slope effect
!>@param[in,out] STETA   Cosinus of the angle between mean flow and y-axis
!>@param[in,out] TOB     Bed shear stress (total friction)
!>@param[in]     UNSV2D  Inverse of integrals of test functions
!>@param[in]     XMVE    Fluid density
!>@param[in]     XMVS    Water density
!>@param[in]     ZF      Elevation of bottom
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_EFFPNT => BEDLOAD_EFFPNT_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     GLOBAL VARIABLES
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,LIQBOR,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF, TOB
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, KENT
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BETA, PI, PHISED, BETA2
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, GRAV, DCLA
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZFDX, DZFDY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CTETA,STETA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: COEF, COEFCR, CALFA, SALFA
!
!     LOCAL VARIABLES
      INTEGER          :: I, K
      DOUBLE PRECISION :: C,ZETA,C1,CALPHA,SALPHA,AA,BB
      DOUBLE PRECISION :: CPSI,SPSI,DZF,TANPHI,CZETA,SZETA,SURBETA2
      DOUBLE PRECISION :: NORM,TT1
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!
! NOTE JMH : IF WE SWAP THE DEVIA AND THE SLOPEFF ACTIONS BELOW
!            IT IS PROBABLY NOT USEFUL TO DO A COPY OF CALFA AND SALFA ?
!
!     COS AND SIN TETA (CALFA AND SALFA HAVE ALREADY BEEN COMPUTED
!                       IN BEDLOAD_SOLIDISCHARGE_GAIA, SO JUST A COPY HERE)
!     TETA = ANGLE OF THE FLOW WITH OX
!
      CALL OS('X=Y     ',X=CTETA,Y=CALFA)
      CALL OS('X=Y     ',X=STETA,Y=SALFA)
!
!----------------------------------------------------------------------
!
!     COMPUTES THE SLOPE  : D(ZF)/DX ET D(ZF)/DY (AT THE NODES)
!
      CALL VECTOR(DZFDX, '=', 'GRADF          X',IELMT,1.D0,ZF,S,S,
     &            S,S,S,MESH,MSK,MASKEL)
      CALL VECTOR(DZFDY, '=', 'GRADF          Y',IELMT,1.D0,ZF,S,S,
     &            S,S,S,MESH,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(DZFDX,2,MESH)
        CALL PARCOM(DZFDY,2,MESH)
      ENDIF
!
      CALL OS('X=XY    ',X=DZFDX,Y=UNSV2D)
      CALL OS('X=XY    ',X=DZFDY,Y=UNSV2D)
!
!======================================================================
!
!     COMPUTES THE SOLID TRANSPORT ANGLE ALFA = TETA + DEVIATION
!
!     1 : KOCH AND FLOKSTRA
!
      IF(DEVIA==1) THEN
!
      C = 2.D0*(XMVS-XMVE)*GRAV*DCLA/3.D0
      DO I=1,NPOIN
        TT1=C/MAX(TOB%R(I),1.D-10)
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM
      ENDDO
!
!     2 : TALMON ET AL. JHR 1995 33(4)
!
      ELSEIF(DEVIA==2) THEN
!
      SURBETA2=1.D0/BETA2
      C = (XMVS-XMVE)*GRAV*DCLA*SURBETA2**2
      DO I=1,NPOIN
        TT1=SQRT(C/MAX(TOB%R(I),1.D-10))
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM
      ENDDO
!
      ENDIF
!
!======================================================================
!
!     COMPUTES THE COEFFICIENT TO TAKE THE SLOPE EFFECT ON THE MAGNITUDE
!     OF THE SOLID TRANSPORT INTO ACCOUNT
!
      CALL OS('X=C     ',X=COEF,C=1.D0)
      CALL OS('X=C     ',X=COEFCR,C=1.D0)
!     METHOD 1 (EMPIRICAL)
!
      IF(SLOPEFF==1) THEN
!
        DO I=1,NPOIN
          COEF%R(I)=MAX(0.D0,
     &    1.D0-BETA*(DZFDX%R(I)*CTETA%R(I)+DZFDY%R(I)*STETA%R(I)) )
        ENDDO
!
!     METHOD 2 : SOULSBY 1997 DYNAMICS OF MARINE SANDS P107-108
!
      ELSEIF(SLOPEFF.EQ.2) THEN
!
        TANPHI = TAN(PHISED*PI/180.D0)
!
        DO I=1,NPOIN
!
!         COSINE AND SINE OF THE DIRECTION OF THE SLOPE
          DZF=SQRT(DZFDX%R(I)**2+DZFDY%R(I)**2)
          IF(DZF.GT.1.D-12) THEN
            CALPHA=DZFDX%R(I)/DZF
            SALPHA=DZFDY%R(I)/DZF
          ELSE
            CALPHA=1.D0
            SALPHA=0.D0
          ENDIF
!
!         ZETA: ANGLE OF THE SLOPE WITH HORIZONTAL (BETA IN SOULSBY)
          ZETA=ATAN(DZF)
          CZETA=COS(ZETA)
          SZETA=SIN(ZETA)
!
!         PSI: ANGLE OF THE CURRENT WITH THE SLOPE DIRECTION
!         PSI=TETA%R(I)-ALPHA
          CPSI=CTETA%R(I)*CALPHA+STETA%R(I)*SALPHA
          SPSI=STETA%R(I)*CALPHA-CTETA%R(I)*SALPHA
          C1=(CZETA*TANPHI)**2-(SPSI*SZETA)**2
          COEFCR%R(I)=MAX((CPSI*SZETA+SQRT(MAX(C1,0.D0)))/TANPHI,0.D0)
          COEFCR%R(I)=MAX(COEFCR%R(I),0.D0)
!
        ENDDO
!
      ENDIF
!
! ********************************************************************* !
!     V - TREATS THE BOUNDARY POINTS WITH IMPOSED RATE                  !
!         QS IS NOT MODIFIED WHEN SPECIFIED BY THE USER                 !
! ********************************************************************* !
!
! RK no slope effect at the open boundaries
      DO K = 1, NPTFR
        IF (LIQBOR%I(K) == KENT) THEN
          COEF%R(MESH%NBOR%I(K)) = 1.D0
          COEFCR%R(MESH%NBOR%I(K)) = 1.D0
          CALFA%R(MESH%NBOR%I(K)) = CTETA%R(MESH%NBOR%I(K))
          SALFA%R(MESH%NBOR%I(K)) = STETA%R(MESH%NBOR%I(K))
        ENDIF
!     R.K. MAY 2007
!     KSORT = 4
        IF (LIQBOR%I(K) == 4) THEN
          COEF%R(MESH%NBOR%I(K)) = 1.D0
          COEFCR%R(MESH%NBOR%I(K)) = 1.D0
          CALFA%R(MESH%NBOR%I(K)) = CTETA%R(MESH%NBOR%I(K))
          SALFA%R(MESH%NBOR%I(K)) = STETA%R(MESH%NBOR%I(K))
        ENDIF
      ENDDO
!
!======================================================================
!======================================================================
!
      RETURN
      END SUBROUTINE BEDLOAD_EFFPNT_GAIA
