!                 **********************************
                  SUBROUTINE ACTIVELAYER
!                 **********************************
!
!***********************************************************************
! GAIA   V8P1                                   05/02/2020
!***********************************************************************
!
!>@brief  CALCULATES THE ACTIVE LAYER THICKNESS ELAY
!!        ACCORDING TO A COUPLE OF FORMULAS
!
!>@history  UWE MERKEL IN CVSM MODEL CVSP_ALT
!!        20/07/2011
!!       V6P2

      USE DECLARATIONS_GAIA

      IMPLICIT NONE

      INTEGER :: IPOIN, I, K, DMIN_I(1)
      DOUBLE PRECISION :: TAUC,DMAX,SUMME,DSTAR,FMAX,DENS,DMIN
      DOUBLE PRECISION :: ACMIN

!CALCULATION OF THE ACTIVE LAYER THICKNESS
      SELECT CASE (ALT_MODEL)
      CASE (0)
        DO IPOIN=1,NPOIN
          ELAY%R(IPOIN)= ELAY0
        END DO
!-----------------------------------------------------------------------
! HUNZIKER & GUENTHER
!-----------------------------------------------------------------------
!
      CASE (1)
! In the first time step the active layer is only virtual and the values in the
! first layer are zero
        K=1
        IF(LT.LE.1) K=2
        DO IPOIN=1,NPOIN
!-----------------------------------------------------------------------
! DMAX - NEW APPROXIMATION: DMAX = D99 / BEFORE DMAX = MAX(DCLA)
! ALL CLASSES MUST BE NON-COHESIVE
!-----------------------------------------------------------------------
          FMAX = 0.99D0*MASS_SAND_TOT(K,IPOIN)
          SUMME = MASS_SAND(1,K,IPOIN)
          DMAX = 0.D0
          IF (SUMME.GE.FMAX) THEN
            DMAX = DCLA(1)
          ELSE
            DO I=2,NSAND
              IF(MASS_SAND(I,K,IPOIN).GT.0.D0) THEN
                SUMME = MASS_SAND(I,K,IPOIN) + SUMME
                IF(SUMME.GE.FMAX.AND.DMAX.EQ.0.D0) THEN
                  DMAX = DCLA(I-1) + ( (DCLA(I)-DCLA(I-1)) /
     &            MASS_SAND(I,K,IPOIN))*
     &            (FMAX-(SUMME-MASS_SAND(I,K,IPOIN)))
                ENDIF
              ENDIF
            ENDDO
          ENDIF
!DMAX = MAX(DCLA(1), DMAX)
          IF (DMAX.LE.0.D0.AND.SUMME.GT.ZERO) THEN
            WRITE(LU,*)'UHM DMAXERROR'
            DO I=1,NSICLA
            WRITE(LU,*)'DMAXERROR',IPOIN,I,DCLA(I),MASS_SAND(I,K,IPOIN)
     &                    ,SUMME
              WRITE(LU,*)ZF%R(IPOIN)-ZR%R(IPOIN)
            ENDDO
            STOP
          ENDIF
          IF(DMAX.GT.MAXVAL(DCLA(1:NSICLA)).AND.SUMME.GT.ZERO)THEN
            PRINT*,'DMAX',DMAX,DClA(1:NSICLA)
            STOP
          ENDIF
          ELAY%R(IPOIN)= 5.D0 * DMAX
        END DO
!
!-----------------------------------------------------------------------
! FREDSOE & DEIGAARD 1992
! DENSITY IS TAKEN FROM THE FIRST CLASS
!-----------------------------------------------------------------------
!
      CASE (2)
        DO IPOIN=1,NPOIN
          ELAY%R(IPOIN)= 2.D0 * TOB%R(IPOIN) / (GRAV*(XMVS0(1)-XMVE))
     &     / TAN(PHISED/180.0D0*PI) / (1.D0-XKV0(1))
        END DO

!
!-----------------------------------------------------------------------
! VAN RIJN 1993
!-----------------------------------------------------------------------
!
      CASE (3)
        CALL MEAN_GRAIN_SIZE_GAIA
! CRICITAL SHEAR STRESS AND DENSITY AND DIAMETER FOR THE FINEST CLASS
        DMIN = MINVAL(DCLA(1:NSICLA))
        DMIN_I = MINLOC(DCLA(1:NSICLA))
        ACMIN = AC(DMIN_I(1))
        TAUC = ACMIN*((XMVS0(1)-XMVE)*GRAV*DMIN)
!
        DO IPOIN=1,NPOIN
          ELAY%R(IPOIN) = 0.D0
          IF(TAUC.GT.0.D0) THEN
            IF(TOB%R(IPOIN).GT.TAUC) THEN
              DENS = (XMVS0(1) - XMVE) / XMVE
              DSTAR = ACLADM%R(IPOIN)*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
              ELAY%R(IPOIN) = 0.3D0*(DSTAR**0.7D0)*
     &           ((TOB%R(IPOIN)-TAUC)/TAUC)**0.5*ACLADM%R(IPOIN)
            ENDIF
          ENDIF
        ENDDO
!-----------------------------------------------------------------------
! WONG 2006
! DENSITY IS TAKEN FROM THE FIRST CLASS!
! HARD CODED SHIELDS THRESHOLD -- THIS FORMULA IS NOT SUITABLE FOR
! FINE SEDIMENTS
!-----------------------------------------------------------------------
      CASE (4)
        CALL MEAN_GRAIN_SIZE_GAIA
        DMIN = MINVAL(DCLA(1:NSICLA))
        DO IPOIN=1,NPOIN
          IF((TOB%R(IPOIN)/(XMVS0(1)-XMVE)/GRAV/
     &       DMIN).LT.0.0549D0) THEN
            ELAY%R(IPOIN) = 0.D0
          ELSE
            ELAY%R(IPOIN)=5.0D0*ACLADM%R(IPOIN)*
     &          ((TOB%R(IPOIN)/(XMVS0(1)-XMVE)/
     &          GRAV/DMIN) -0.0549D0)**0.56D0
          ENDIF
        ENDDO
!-----------------------------------------------------------------------
! MALCHEREK 2003
!-----------------------------------------------------------------------
      CASE(5)
! CRICITAL SHEAR STRESS FOR THE FINEST SEDIMENT
! DENSITY FROM THE FIRST CLASS
        ACMIN = MINVAL(AC(1:NSICLA))
        DMIN = MINVAL(DCLA(1:NSICLA))
        TAUC = ACMIN*((XMVS0(1)-XMVE)*GRAV*DMIN)
!-----------------------------------------------------------------------
! D90 - ONLY FIRST APPROXIMATION!
! ALL CLASSES MUST BE NON-COHESIVE
!-----------------------------------------------------------------------
! In the first time step the active layer is only virtual and the values in the
! first layer are zero
        K=1
        IF(LT.LE.1) K=2
        DO IPOIN=1,NPOIN
          FMAX = 0.90D0*MASS_SAND_TOT(K,IPOIN)
          SUMME = MASS_SAND(1,K,IPOIN)
          D90 = 0.D0
          IF (SUMME.GE.FMAX) THEN
            D90 = DCLA(1)
          ELSE
            DO I=2,NSAND
              IF(MASS_SAND(I,K,IPOIN).GT.0.D0) THEN
                SUMME = MASS_SAND(I,K,IPOIN) + SUMME
                IF(SUMME.GE.FMAX.AND.D90.EQ.0.D0) THEN
                  D90 = DCLA(I-1) + ( (DCLA(I)-DCLA(I-1)) /
     &            MASS_SAND(I,K,IPOIN)) *
     &            (FMAX-(SUMME-MASS_SAND(I,K,IPOIN)))
                ENDIF
              ENDIF
            ENDDO
          ENDIF
!D90 = MAX(DCLA(1), D90)
          IF (D90.LE.0.D0.AND.SUMME.GT.ZERO) THEN
            WRITE(LU,*)'UHM D90ERROR', D90, FMAX, SUMME
            DO I=1,NSICLA
              WRITE(LU,*)'D90ERROR',IPOIN,I,DCLA(I),
     &                    MASS_SAND(I,K,IPOIN),SUMME
            ENDDO
            WRITE(LU,*)'D90ERROR'
          ENDIF
          ELAY%R(IPOIN) = 0.D0
          IF(TAUC.GT.0.D0)
     &         ELAY%R(IPOIN) = D90 / (1.D0-XKV0(1)) *
     &           MAX(1.D0,TOB%R(IPOIN)/TAUC)
        ENDDO
!-----------------------------------------------------------------------
! SISYPHE
!-----------------------------------------------------------------------
        CASE(6)
          CALL MEAN_GRAIN_SIZE_GAIA
          DO IPOIN=1,NPOIN
            ELAY%R(IPOIN)=3*ACLADM%R(IPOIN)
          ENDDO
        END SELECT

      END SUBROUTINE
