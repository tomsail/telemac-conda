!                   ******************************
                    SUBROUTINE PREP_ADVECTION_GAIA
!                   ******************************
!
     &(UCONV_TEL,VCONV_TEL,ICONVF,SOLSYS,J,LITBOR,TBOR,TN,KENT,FLBOR_W,
     & HN_TEL,MASSOU)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Prepare the deposition flux and the velocity field for the
!advection of concentration
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param [in]  UCONV_TEL   X velocity field of TELEMAC
!>@param [in]  VCONV_TEL   Y velocity field of TELEMAC
!>@param [in]  ICONVF      Advection scheme
!>@param [in]  SOLSYS      Option for the advection (see cvdftr)
!>@param [in]  J           Number of the sediment class considered
!>@param [in]  LITBOR      Type of boundary conditions on tracers
!>@param [in]  TBOR        Block with prescribed values of tracers
!>@param [in]  TN          Tracers at time t
!>@param [in]  KENT        Convention for prescribed value at entrance
!>@param [in]  FLBOR_W     Water flux on boundary
!>@param [in]  HN_TEL      Water depth sent by TELEMAC
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN) :: UCONV_TEL,VCONV_TEL,FLBOR_W
      TYPE(BIEF_OBJ), INTENT(IN), TARGET :: HN_TEL
      INTEGER, INTENT(IN) :: ICONVF,SOLSYS,J,LITBOR(NPTFR),KENT
      DOUBLE PRECISION, INTENT(INOUT) :: TBOR(NPTFR),TN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I
      DOUBLE PRECISION :: AUX,VITCD,FD90,C
!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!
!     MODIFY THE CONVECTIVE VELOCITY FIELD ACCORDING TO THE
!     CONCENTRATION PROFILE
!
      CALL GAIA_SUSPENSION_CONV(UCONV_TEL,VCONV_TEL,ICONVF,SOLSYS,
     &     J,FLBOR_W)
!
!     ****************************************************
!     THE TOTAL FRICTION VELOCITY    --> USTAR (T1)
!     HAS BEEN REPLACED BY USTARP (SKIN FRICTION VELOCITY)
!     FOR EROSION FLUX FROM V6P0 ON
!     ****************************************************
!
      CALL OS('X=CY    ', X=T1, Y=TOBCW_MEAN, C=1.D0/XMVE)
!     TOB assumed >=0, otherwise mistake elsewhere...
      CALL OS('X=SQR(Y)', X=T1, Y=T1)

!     VALUE OF D90 OF SEDIMENT
!     WITH ONE SAND CLASS, IT IS USER SET
!     WITH MORE THAN ONE SAND CLASS, IT IS
!     CONSIDERED AS THE RATIO BETWEEN SKIN FRICTION AND MEAN
!     DIAMETER * D50
      IF (NSAND.EQ.1) THEN
        FD90 = D90
      ELSE
        FD90=DCLA(J)*KSPRATIO
      ENDIF
!
      IF(SEDCO(J)) THEN

        VITCD=SQRT(TOCD_MUD0(J)/XMVE)
!
!       *********************************************************
!       IA - FORMULATION FOR COHESIVE SEDIMENTS (WITHOUT BEDLOAD)
!       *********************************************************
!
!       COMPUTES THE PROBABILITY FOR DEPOSITION
!
        DO I = 1, NPOIN
!         HERE T1 >=0, so case TOCD_MUD0(J)=0.D0 excluded by the test
          IF(T1%R(I).LT.VITCD) THEN
            AUX = 1.D0-(T1%R(I)/VITCD)**2
          ELSE
            AUX = 0.D0
          ENDIF
!         COMPUTES THE IMPLICIT PART OF THE DEPOSITION FLUX
          FLUDPT%ADR(J)%P%R(I)= XWC(J)*AUX
        ENDDO
!     UNIFORM SEDIMENT ALONG THE VERTICAL
        CALL CPSTVC(TOBCW_MEAN,CSRATIO%ADR(J)%P)
        CALL OS('X=C     ', X=CSRATIO%ADR(J)%P, C=1.D0)
!
!       **********************************************************
!       IB - FORMULATION FOR NON-COHESIVE SEDIMENTS (WITH BEDLOAD)
!       **********************************************************
!
      ELSE
!
!       *******************************************************
!       COMPUTES THE RATIO BETWEEN NEAR BED CONC. AND MEAN CONC
!                                  -->  CSRATIO    (TO KEEP )
!       *******************************************************
!
!       DMK Modification 06/05/2011
        IF(.NOT.(SET_LAG)) THEN
          IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_ROUSE'
          CALL SUSPENSION_ROUSE_GAIA(T1,HN,NPOIN,
     &              KARMAN,ZERO,XWC(J),ZREF,CSRATIO%ADR(J)%P)
          IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_ROUSE'
        ELSE
          IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BETAFACTOR'
          CALL SUSPENSION_MILES_GAIA(HN,NPOIN,HMIN,
     &                  DCLA(J),FD90,XWC(J),CSRATIO%ADR(J)%P)
          IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_BETAFACTOR'
        ENDIF
!       End of DMK mod
!
!       ****************************************************
!       COMPUTES THE DEPOSITION FLUX --> FLUDPT = XWC * CSRATIO
!       ****************************************************
!
        CALL OS('X=CY    ',X=FLUDPT%ADR(J)%P,Y=CSRATIO%ADR(J)%P,
     &         C=XWC(J))
!
      ENDIF
!
!     Impose the concentration at the inflow to respect the equilibrium
!     concentration on the boundary nodes
!
      IF(IMP_INFLOW_C) THEN
        IF (DEBUG > 0) WRITE(LU,*) 'IMP_INFLOW_C'
        CALL EQCAE_BC_GAIA(LITBOR,TBOR,TN,J,KENT)
        IF (DEBUG > 0) WRITE(LU,*) 'FIN IMP_INFLOW_C'
      ENDIF
!
!     Treatment of tidal flats
!
      IF(OPTBAN.EQ.2) THEN
        CALL OS('X=XY    ',X=FLUER%ADR(J)%P ,Y=MASKPT)
      ENDIF
!
!     Allocation des variables pour utilisation dans telemac2d
!     Be aware: HN is in fact H in Telemac-2D or 3D (except if
!     HN<HMIN)
!
      CALL OS('X=Y     ',X=HN_GAI,Y=HN)
      CALL OS('X=Y     ',X=FLUER_ADV,Y=FLUER%ADR(J)%P)
      CALL OS('X=-Y    ',X=FLUDPT_ADV,Y=FLUDPT%ADR(J)%P)
!
      DO I=1,NPOIN
        IF(HN%R(I).GT.HMIN) THEN
          FLUER_ADV%R(I)=FLUER_ADV%R(I)/HN%R(I)
        ELSE
          FLUER_ADV%R(I)=0.D0
!         FLUER WILL BE USED AS T11*HN, SO IT MUST BE
!         CANCELLED ACCORDINGLY, OTHERWISE MASS BALANCE WRONG
          FLUER%ADR(J)%P%R(I)=0.D0
        ENDIF
      ENDDO
!
      IF(OPTBAN.NE.0) THEN
        CALL CPSTVC(HN_TEL,T1)
!       HN_TEL IS NOT CLIPPED
        DO I=1,NPOIN
          T1%R(I)=MAX(HN_TEL%R(I),HMIN)
        ENDDO
        HOLD=>T1
      ELSE
        HOLD=>HN_TEL
      ENDIF
!
!     MASS ADDED BY EXPLICIT TERMS
!     THE MASS ADDED BY IMPLICIT TERMS IS COMPUTED IN CVDFTR
!     MASSOU ALREADY INITIALISED IN DIFFSOU IN CASE OF SEDIMENT RELEASE
!     MASSOU DIFFERENT FROM ZERO (E.G. IF NERD SCHEME) SO IT SHOULD NOT BE
!     PUT TO ZERO
!
      C=0.D0
      DO I=1,NPOIN
        C=C+HN%R(I)*FLUER_ADV%R(I)*VOLU2D%R(I)
      ENDDO
      C=C*DT
      IF(NCSIZE.GT.1) C=P_SUM(C)
      MASSOU=MASSOU+C
!======================================================================!
!======================================================================!
!
      RETURN
      END
