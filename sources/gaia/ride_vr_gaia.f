!                   ***********************
                    SUBROUTINE RIDE_VR_GAIA
!                   ***********************
!
     & (KSR,KS,UNORM,HN,GRAV,XMVE,XMVS,NPOIN,ACLADM)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the dimensions of equilibrium ripples.
!!       VAN RIJN (2007) (CURRENT ONLY).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     ACLADM Mean diameter of active layer
!>@param[in]     GRAV   Acceleration of gravity
!>@param[in]     HN     Water depth
!>@param[in,out] KS     Total roughness
!>@param[in,out] KSR    Bed skin roughness
!>@param[in]     NPOIN  Number of points
!>@param[in]     UNORM  Norm of the mean flow velocity
!>@param[in]     XMVE   Fluid density
!>@param[in]     XMVS   Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER I,NPOIN
!
      DOUBLE PRECISION, INTENT(INOUT)  :: KSR(NPOIN),KS(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: GRAV,XMVE,XMVS
      DOUBLE PRECISION, INTENT(IN)     :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: ACLADM(NPOIN),UNORM(NPOIN)
!
! LOCAL VARIABLES
!
      DOUBLE PRECISION AI,KSCR,KSCD,KSCMR,MOB,FES,FFS
      DOUBLE PRECISION DSAND,DGRAVEL,DSILT
!
!---------------------------------------------------------------------
!
      DSILT=0.000032D0
      DGRAVEL=0.002D0
      DSAND=0.000062D0
!
! COMPUTES CURRENT-DOMINATED ROUGHNESS USING VAN RIJN (2007)
!
      DO I=1,NPOIN
!
! MOBILITY NUMBER FOR CURRENT ONLY
!
        AI  = ACLADM(I)*GRAV*(XMVS-XMVE)/XMVE
        MOB = UNORM(I)**2/AI
!
! RIPPLE ROUGHNESS
!
        IF(ACLADM(I).LE.0.25D0*DGRAVEL)THEN
          FES=1.D0
        ELSE
          FES=(0.25D0*DGRAVEL/ACLADM(I))**1.5D0
        ENDIF
!
        IF(ACLADM(I).LT.DSILT)THEN
          KSCR=20.D0*DSILT
        ELSE
          AI= TANH(0.015D0*(MOB-150.D0))
          KSCR=FES*ACLADM(I)*(85.D0-65.D0*AI)
        ENDIF
!
! MEGARIPPLE ROUGHNESS
!
        IF(ACLADM(I).GE.(1.5D0*DSAND))THEN
          FFS=1.D0
        ELSE
          FFS=ACLADM(I)/1.5D0/DSAND
        ENDIF
        IF(ACLADM(I).LE.DSILT)THEN
          KSCMR=0.D0
        ELSE
          KSCMR=0.00002D0*FFS*HN(I)*(1.D0-EXP(-0.05D0*MOB))
          IF(MOB.GT.550.D0.AND.ACLADM(I).GE.1.5D0*DSAND)THEN
            KSCMR=0.02D0
          ELSEIF(MOB.GT.550D0.AND.ACLADM(I).LT.1.5D0*DSAND)THEN
            KSCMR=200.D0*ACLADM(I)
          ENDIF
        ENDIF
!
! DUNE ROUGHNESS
!
        IF(ACLADM(I).LT.DSILT) THEN
          KSCD=0.D0
        ELSE
          AI=(1.D0-EXP(-0.02D0*MOB))*(600.D0-MOB)
          KSCD=0.00008D0*FFS*HN(I)* AI
        ENDIF
        IF(MOB.GT.600.D0) KSCD=0.D0
        IF(KSCD.GT.1.D0) KSCD=1.D0
!
! ***RIPPLE BED ROUGHNESS FOR SEDIMENT COMPUTATIONS IN GAIA ***
!
        KSR(I)=KSCR
!
! *** TOTAL ROUGHNESS FOR COMPUTATIONS IN TELEMAC2D **
!
        KS(I)=SQRT(KSCR**2+KSCMR**2+KSCD**2)
!
      ENDDO
!
!AGD****************************************************
!
!---------------------------------------------------------------------
!
      RETURN
      END
