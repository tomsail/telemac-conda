!                   ******************
                    SUBROUTINE RIDE_VR
!                   ******************
!
     & (KSR,KS,UNORM,HN,GRAV,XMVE,XMVS,NPOIN,ACLADM)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE DIMENSIONS OF EQUILIBRIUM RIPPLES.
!+                VAN RIJN (2007) (CURRENT ONLY).
!
!history  C. VILLARET (LNHE); AG DAVIES (UCW)
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
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HN             |-->| WATER DEPTH
!| KS             |-->| TOTAL ROUGHNESS
!| KSP            |-->| BED SKIN ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| UNORM          |-->| INTENSITE DU COURANT
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
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
      DOUBLE PRECISION AI,ZERO,KSCR,KSCD,KSCMR,MOB,FES,FFS
      DOUBLE PRECISION DSAND,DGRAVEL,DSILT
!
!---------------------------------------------------------------------
!
      ZERO=1.D-6
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
! ***RIPPLE BED ROUGHNESS FOR SEDIMENT COMPUTATIONS IN SISYPHE ***
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
