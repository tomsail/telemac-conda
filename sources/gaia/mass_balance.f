!                   ***********************
                    SUBROUTINE MASS_BALANCE
!                   ***********************
!
     &(DT,NPTFR,ENTET,NSICLA,NUMLIQ,NFRLIQ,FLBCLA,
     & LT,NIT,NPOIN,VOLU2D,CHARR,SUSP,EVCL_MB,EVCL_MS,MASSTOT,
     & MASS0TOT)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the mass balance of the bed.
!!       For each time step the mass balance is done for each class and
!!       for the sum over all classes.
!!       For the last time step the mass balance is done with cumulated
!!       variables, for each class and for the sum over all classes.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     CHARR    Logical, bedload or not
!>@param[in]     DT       Time step
!>@param[in]     E        Bed evolution at a given time step
!>@param[in]     EVCL_MB  Bedload mass evolution for each sediment class
!>@param[in]     EVCL_MS  Suspension mass evolution for each sediment class
!>@param[in,out] FLBCLA   Block of fluxes at boundary for each class
!>@param[in]     ENTET    If yes : information is printed
!>@param[in]     LT       Current iteration
!>@param[in]     MASSTOT  Total mass per class of sediment
!>@param[in]     MASS0TOT Initial total mass per class of sediment
!>@param[in]     NFRLIQ   Number of liquid boundaries
!>@param[in]     NIT      Number of time steps
!>@param[in]     NPOIN    Number of points
!>@param[in]     NPTFR    Number of boundary nodes
!>@param[in]     NSICLA   Number of size classes for bed materials
!>@param[in]     NUMLIQ   Liquid boundary number of boundary points
!>@param[in]     SUSP     Logical, suspension or not
!>@param[in]     VOLU2D   Integral of test functions (not assembled in parallel)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_GAIA, EX_MASS_BALANCE => MASS_BALANCE
      USE DECLARATIONS_GAIA, ONLY : NSICLM,NUM_ICLA_ISAND,
     &                              FLUER,
     &                              FLUDP, SUMMCUMUCL, SUMRMASCL,
     &                              SUM_EROSION,SUM_DEPOSITION,
     &                              MASSNESTOR,SUMMASSNESTOR,
     &                              MASS0ACT,NESTOR,BEDLOAD_B_FLUX,
     &                              SUMBEDLOAD_B_FLUX,SUMBEDLOAD_B,
     &                              MCUMUCLA
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY: P_SUM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NPTFR,NFRLIQ,NSICLA,LT,NIT
      INTEGER, INTENT(IN)          :: NPOIN,NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: DT
      LOGICAL, INTENT(IN)          :: ENTET,SUSP,CHARR
      DOUBLE PRECISION, INTENT(IN)    :: MASSTOT(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: MASS0TOT(NSICLA)
!
!-----------------------------------------------------------------------
!
!     VECTOR STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU2D
      TYPE(BIEF_OBJ), INTENT(IN)    :: EVCL_MB,EVCL_MS
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLBCLA
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IFRLQ,IPTFR,ICLA
      DOUBLE PRECISION LOST,RELATI,DENOM
      DOUBLE PRECISION FLUXTCLA
      DOUBLE PRECISION RMASCLA(NSICLM)
      DOUBLE PRECISION EROSION_FLUX(NSICLM),DEPOSITION_FLUX(NSICLM)
      DOUBLE PRECISION SUMMASSTOT,SUMMASS0TOT
      DOUBLE PRECISION SUMRMASCLA,SUMMCUMUCLA
      DOUBLE PRECISION SUMEROSTOT,SUMDEPOTOT
      DOUBLE PRECISION SUMLOST
      DOUBLE PRECISION SUMNESTOR,SUMMASS0ACT
!
!-----------------------------------------------------------------------
!
!     COMPUTES EROSION AND DEPOSITION FLUX PER CLASS EVOLUTION
!     FOR THIS TIME STEP
!     NOTE: EROSION AND DEPOSITION FLUX MUST BE 0 IF THERE IS
!     NO SUSPENSION
!
      IF(NSICLA.GT.0) THEN
        DO ICLA=1,NSICLA
          EROSION_FLUX(ICLA) = 0.D0
          DEPOSITION_FLUX(ICLA) = 0.D0
        ENDDO
      ENDIF
      IF(NSICLA.GT.0) THEN
        IF (SUSP) THEN
          DO ICLA=1,NSICLA
            DO I=1,NPOIN
              EROSION_FLUX(ICLA) = EROSION_FLUX(ICLA)
     &                     + FLUER%ADR(ICLA)%P%R(I)*VOLU2D%R(I)
            ENDDO
            IF(NCSIZE.GT.1) EROSION_FLUX(ICLA) =
     &                      P_SUM(EROSION_FLUX(ICLA))
            DO I=1,NPOIN
              DEPOSITION_FLUX(ICLA) = DEPOSITION_FLUX(ICLA)
     &                       + FLUDP%ADR(ICLA)%P%R(I)*VOLU2D%R(I)
            ENDDO
            IF(NCSIZE.GT.1) DEPOSITION_FLUX(ICLA) =
     &                      P_SUM(DEPOSITION_FLUX(ICLA))
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
!     PREPARES TERMS FOR BALANCE IN EXTENDED GRANULOMETRY
!
      IF(NSICLA.GT.0) THEN
!
        DO ICLA=1,NSICLA
!
!         COMPUTES THE EVOLUTION PER CLASS
!
          RMASCLA(ICLA) = 0.D0
          IF(SUSP.AND.CHARR) THEN
            DO I=1,NPOIN
              RMASCLA(ICLA) = RMASCLA(ICLA)+VOLU2D%R(I)*
     &        (EVCL_MB%ADR(ICLA)%P%R(I) + EVCL_MS%ADR(ICLA)%P%R(I))
            ENDDO
          ELSEIF(SUSP) THEN
            DO I=1,NPOIN
              RMASCLA(ICLA) = RMASCLA(ICLA)
     &                      + EVCL_MS%ADR(ICLA)%P%R(I)*VOLU2D%R(I)
            ENDDO
          ELSEIF(CHARR) THEN
            DO I=1,NPOIN
              RMASCLA(ICLA) = RMASCLA(ICLA)
     &                      + EVCL_MB%ADR(ICLA)%P%R(I)*VOLU2D%R(I)
            ENDDO
          ENDIF
          IF(NCSIZE.GT.1) RMASCLA(ICLA) = P_SUM(RMASCLA(ICLA))
!
!         COMPUTES THE FREE FLUXES BY CLASS, FOR EVERY BOUNDARY:
!         BEDLOAD_B_FLUX
!         FLUXTCLA CONTAINS THE SUM OF BEDLOAD_B_FLUX OVER BOUNDARIES
!
          FLUXTCLA = 0.D0
          IF(NFRLIQ.GT.0.AND.CHARR) THEN
            DO IFRLQ=1,NFRLIQ
              BEDLOAD_B_FLUX(IFRLQ,ICLA) = 0.D0
            ENDDO
            IF(NPTFR.GT.0) THEN
              DO IPTFR=1,NPTFR
                IFRLQ = NUMLIQ(IPTFR)
                IF(IFRLQ.GT.0) THEN
                  BEDLOAD_B_FLUX(IFRLQ,ICLA) =
     &            BEDLOAD_B_FLUX(IFRLQ,ICLA)+FLBCLA%ADR(ICLA)%P%R(IPTFR)
                  FLUXTCLA = FLUXTCLA + FLBCLA%ADR(ICLA)%P%R(IPTFR)
                ENDIF
              ENDDO
            ENDIF
            IF(NCSIZE.GT.1) THEN
              FLUXTCLA = P_SUM(FLUXTCLA)
              DO I=1,NFRLIQ
                BEDLOAD_B_FLUX(I,ICLA) = P_SUM(BEDLOAD_B_FLUX(I,ICLA))
              ENDDO
            ENDIF
          ENDIF
!
!         COMPUTES THE CUMULATED FLUXES PER CLASS
!
          MCUMUCLA(ICLA) = -FLUXTCLA
          SUMMCUMUCL(ICLA) = SUMMCUMUCL(ICLA) + MCUMUCLA(ICLA)*DT
          DO IFRLQ=1,NFRLIQ
            SUMBEDLOAD_B(IFRLQ,ICLA) = SUMBEDLOAD_B(IFRLQ,ICLA) +
     &                                 BEDLOAD_B_FLUX(IFRLQ,ICLA)*DT
          ENDDO
          SUMRMASCL(ICLA) = SUMRMASCL(ICLA) + RMASCLA(ICLA)
          SUM_DEPOSITION(ICLA) = SUM_DEPOSITION(ICLA)
     &                           + DEPOSITION_FLUX(ICLA)*DT
          SUM_EROSION(ICLA) = SUM_EROSION(ICLA)
     &                           + EROSION_FLUX(ICLA)*DT
          SUMMASSNESTOR(ICLA) = SUMMASSNESTOR(ICLA) + MASSNESTOR(ICLA)
        ENDDO
!
      ENDIF
!
!=======================================================================
!
!     FIXME: IF(NESTOR)...
!
!     WRITES OUT THE BALANCE
!
      IF(ENTET) THEN
!
        IF(NSICLA.GT.0) THEN
!
!         INITIALIZE FOR BALANCE OVER ALL CLASSES
!
          SUMMASS0TOT = 0.D0
          SUMMASSTOT = 0.D0
          SUMMASS0ACT = 0.D0
          SUMRMASCLA = 0.D0
          SUMMCUMUCLA = 0.D0
          SUMEROSTOT = 0.D0
          SUMDEPOTOT = 0.D0
          SUMLOST = 0.D0
          SUMNESTOR = 0.D0
          IF(NFRLIQ.GT.0) THEN
            DO IFRLQ=1,NFRLIQ
              SUMBEDLOAD_B_FLUX(IFRLQ) = 0.D0
            ENDDO
          ENDIF
!
!         BALANCE FOR EACH CLASS
!
          DO I=1,NSICLA
!           EROSION AND DEPOSITION FLUX MUST BE 0 IF THERE IS
!           NO SUSPENSION
            LOST = RMASCLA(I)+(-MCUMUCLA(I) + EROSION_FLUX(I)
     &             - DEPOSITION_FLUX(I))*DT - MASSNESTOR(I)
            WRITE(LU,*)
            WRITE(LU,2000)
            WRITE(LU,2001) I
            WRITE(LU,2010) RMASCLA(I)
            WRITE(LU,3031) MCUMUCLA(I)
            IF(NFRLIQ.GT.0.AND.CHARR) THEN
              DO IFRLQ=1,NFRLIQ
                WRITE(LU,3033) IFRLQ,-BEDLOAD_B_FLUX(IFRLQ,I)
!               SUMS FLUXES OVER ALL CLASSES
                SUMBEDLOAD_B_FLUX(IFRLQ) = SUMBEDLOAD_B_FLUX(IFRLQ)
     &                            + BEDLOAD_B_FLUX(IFRLQ,I)
              ENDDO
            ENDIF
            WRITE(LU,2112) EROSION_FLUX(I)
            WRITE(LU,2113) DEPOSITION_FLUX(I)
            WRITE(LU,2910) MASSTOT(I)
            IF(NESTOR) WRITE(LU,2022) MASSNESTOR(I)
            WRITE(LU,2033) LOST
            IF(MASS0ACT(I).GT.1.D-8) THEN
              RELATI = LOST/MASS0ACT(I)
              WRITE(LU,2130) RELATI
            ENDIF
!           FIXME: RELATIVE ERROR SEEMS STRANGE.. KEEP IT FOR THE MOMENT
!           RK: IS RELATIVE TO THE INITIAL MASS IN ACTIVE LAYER
            DENOM = MAX(RMASCLA(I),(MCUMUCLA(I) + EROSION_FLUX(I)
     &                              -DEPOSITION_FLUX(I))*DT)
!
!           UPDATES VARIABLES FOR BALANCE OVER ALL CLASSES
!
            SUMMASSTOT = SUMMASSTOT + MASSTOT(I)
            SUMMASS0TOT = SUMMASS0TOT + MASS0TOT(I)
            SUMMASS0ACT = SUMMASS0ACT + MASS0ACT(I)
            SUMRMASCLA = SUMRMASCLA + RMASCLA(I)
            SUMMCUMUCLA = SUMMCUMUCLA + MCUMUCLA(I)
            SUMEROSTOT = SUMEROSTOT + EROSION_FLUX(I)
            SUMDEPOTOT = SUMDEPOTOT + DEPOSITION_FLUX(I)
            SUMNESTOR = SUMNESTOR + MASSNESTOR(I)
            SUMLOST = SUMLOST + LOST
!
            IF(DENOM.GT.1.D-8) THEN
              LOST = LOST / DENOM
              WRITE(LU,2240) LOST
            ENDIF
          ENDDO
!
!         BALANCE OVER ALL CLASSES
!
          IF(NSICLA.GT.1) THEN
            WRITE(LU,*)
            WRITE(LU,2002)
            WRITE(LU,3010) SUMRMASCLA
            WRITE(LU,3031) SUMMCUMUCLA
            IF(NFRLIQ.GT.0.AND.CHARR) THEN
              DO IFRLQ=1,NFRLIQ
                WRITE(LU,3033) IFRLQ,-SUMBEDLOAD_B_FLUX(IFRLQ)
              ENDDO
            ENDIF
            WRITE(LU,2112) SUMEROSTOT
            WRITE(LU,2113) SUMDEPOTOT
            WRITE(LU,2910) SUMMASSTOT
            IF(NESTOR) WRITE(LU,2022) SUMNESTOR
            WRITE(LU,2033) SUMLOST
            IF(SUMMASS0ACT.GT.1.D-8) THEN
              RELATI = SUMLOST / SUMMASS0ACT
              WRITE(LU,2130) RELATI
            ENDIF
          ENDIF
!
!         FINAL GLOBAL BALANCE
!
          IF(LT.EQ.NIT) THEN
!
!           INITIALIZE FOR BALANCE OVER ALL CLASSES
!
            SUMMASS0TOT = 0.D0
            SUMMASSTOT = 0.D0
            SUMMASS0ACT = 0.D0
            SUMRMASCLA = 0.D0
            SUMMCUMUCLA = 0.D0
            SUMEROSTOT = 0.D0
            SUMDEPOTOT = 0.D0
            SUMLOST = 0.D0
            SUMNESTOR = 0.D0
            IF(NFRLIQ.GT.0) THEN
              DO IFRLQ=1,NFRLIQ
                SUMBEDLOAD_B_FLUX(IFRLQ) = 0.D0
              ENDDO
            ENDIF
!
!         BALANCE FOR EACH CLASS
!
            WRITE(LU,*)
            WRITE(LU,2150)
            DO I=1,NSICLA
              LOST = SUMRMASCL(I) - SUMMCUMUCL(I) + SUM_EROSION(I)
     &               - SUM_DEPOSITION(I) - SUMMASSNESTOR(I)
              WRITE(LU,*)
              WRITE(LU,2000)
              WRITE(LU,2001) I
              WRITE(LU,3010) SUMRMASCL(I)
              WRITE(LU,3032) SUMMCUMUCL(I)
              IF(NFRLIQ.GT.0.AND.CHARR) THEN
                DO IFRLQ=1,NFRLIQ
                  WRITE(LU,3034) IFRLQ,-SUMBEDLOAD_B(IFRLQ,I)
                  SUMBEDLOAD_B_FLUX(IFRLQ) = SUMBEDLOAD_B_FLUX(IFRLQ)
     &                              + SUMBEDLOAD_B(IFRLQ,I)
                ENDDO
              ENDIF
              WRITE(LU,2114) SUM_EROSION(I)
              WRITE(LU,2115) SUM_DEPOSITION(I)
              WRITE(LU,2028) MASS0TOT(I)
              WRITE(LU,2029) MASS0ACT(I)
              WRITE(LU,2910) MASSTOT(I)
              IF(NESTOR) WRITE(LU,2022) SUMMASSNESTOR(I)
              WRITE(LU,2034) LOST
              IF(MASS0ACT(I).GT.1.D-8) THEN
                RELATI = LOST / MASS0ACT(I)
                WRITE(LU,2130) RELATI
              ENDIF
!
              SUMMASS0TOT = SUMMASS0TOT + MASS0TOT(I)
              SUMMASSTOT = SUMMASSTOT + MASSTOT(I)
              SUMMASS0ACT = SUMMASS0ACT + MASS0ACT(I)
              SUMRMASCLA = SUMRMASCLA + SUMRMASCL(I)
              SUMMCUMUCLA = SUMMCUMUCLA + SUMMCUMUCL(I)
              SUMEROSTOT = SUMEROSTOT + SUM_EROSION(I)
              SUMDEPOTOT = SUMDEPOTOT + SUM_DEPOSITION(I)
              SUMNESTOR = SUMNESTOR + SUMMASSNESTOR(I)
              SUMLOST = SUMLOST + LOST
!
              IF(DENOM.GT.1.D-8) THEN
                LOST = LOST / DENOM
                WRITE(LU,2160) LOST
              ENDIF
              LOST = SUMMASSTOT - SUMMASS0TOT - SUMNESTOR
     &             - SUMMCUMUCLA + SUMEROSTOT - SUMDEPOTOT
              WRITE(LU,2031) LOST
              IF(SUMMASS0TOT.GT.1.D-8) THEN
                RELATI = LOST / SUMMASS0TOT
                WRITE(LU,2131) RELATI
              ENDIF


            ENDDO
!
!           BALANCE OVER ALL CLASSES
!
            IF(NSICLA.GT.1) THEN
              WRITE(LU,*)
              WRITE(LU,2002)
              WRITE(LU,3010) SUMRMASCLA
              WRITE(LU,3032) SUMMCUMUCLA
              IF(NFRLIQ.GT.0.AND.CHARR) THEN
                DO IFRLQ=1,NFRLIQ
                  WRITE(LU,3034) IFRLQ,-SUMBEDLOAD_B_FLUX(IFRLQ)
                ENDDO
              ENDIF
              WRITE(LU,2114) SUMEROSTOT
              WRITE(LU,2115) SUMDEPOTOT
              WRITE(LU,2028) SUMMASS0TOT
              WRITE(LU,2029) SUMMASS0ACT
              WRITE(LU,2910) SUMMASSTOT
              IF(NESTOR) WRITE(LU,2022) SUMNESTOR
              WRITE(LU,2034) SUMLOST
              IF(SUMMASS0ACT.GT.1.D-8) THEN
                RELATI = SUMLOST / SUMMASS0ACT
                WRITE(LU,2130) RELATI
              ENDIF
              LOST = SUMMASSTOT - SUMMASS0TOT - SUMNESTOR
     &             - SUMMCUMUCLA + SUMEROSTOT - SUMDEPOTOT
              WRITE(LU,2031) LOST
              IF(SUMMASS0TOT.GT.1.D-8) THEN
                RELATI = LOST / SUMMASS0TOT
                WRITE(LU,2131) RELATI
              ENDIF
            ENDIF
!
          ENDIF
        ENDIF ! ENDIF NSICLA
!
      ENDIF ! ENDIF ENTET
!
2000  FORMAT(20X,'GAIA MASS-BALANCE OF SEDIMENTS PER CLASS: ')
2002  FORMAT(20X,'GAIA MASS-BALANCE OF SEDIMENTS OVER ALL CLASSES: ')
2001  FORMAT(5X,'SEDIMENT CLASS NUMBER                     = ',1I8)
2010  FORMAT(5X,'TOTAL BED EVOLUTIONS                      = ',G16.7,
     &       '  ( KG )')
2028  FORMAT(5X,'INITIAL MASS                              = ',G16.7,
     &       '  ( KG )')
2029  FORMAT(5X,'INITIAL MASS ACTIVE LAYER                 = ',G16.7,
     &       '  ( KG )')
2033  FORMAT(5X,'LOST MASS                                 = ',G16.7,
     &       '  ( KG )')
2034  FORMAT(5X,'CUMULATED LOST MASS                       = ',G16.7,
     &       '  ( KG )')
2112  FORMAT(5X,'EROSION FLUX                              = ',G16.7,
     &          '  ( KG/S )')
2113  FORMAT(5X,'DEPOSITION FLUX                           = ',G16.7,
     &          '  ( KG/S )')
2114  FORMAT(5X,'CUMULATED EROSION                         = ',G16.7,
     &          '  ( KG )')
2115  FORMAT(5X,'CUMULATED DEPOSITION                      = ',G16.7,
     &          '  ( KG )')
2150  FORMAT(20X,'FINAL MASS-BALANCE OF SEDIMENTS: ')
2130  FORMAT(5X,'RELATIVE ERROR TO INITIAL ACT LAYER MASS  = ',G16.7)
2131  FORMAT(5X,'RELATIVE ERROR TO TOTAL INITIAL MASS      = ',G16.7)
2160  FORMAT(5X,'CUMULATED RELATIVE ERROR ON MASS          = ',G16.7)
2240  FORMAT(5X,'RELATIVE ERROR 0N MASS                    = ',G16.7)
2022  FORMAT(5X,'NESTOR MASS                               = ',G16.7,
     &          '  ( KG )')
2910  FORMAT(5X,'TOTAL MASS                                = ',G16.7,
     &          '  ( KG )')
3010  FORMAT(5X,'CUMULATED BED EVOLUTIONS                  = ',G16.7,
     &          '  ( KG )')
3031  FORMAT(5X,'BOUNDARIES BEDLOAD FLUX                   = '
     &       ,G16.7,'  ( KG/S  >0 = ENTERING )')
3032  FORMAT(5X,'CUMULATED BOUNDARIES BEDLOAD MASS         = ',G16.7,
     &          '  ( KG )')
3033  FORMAT(5X,'BEDLOAD FLUX BOUNDARY ',I4,'                = ', G16.7
     &      ,'  ( KG/S  >0 = ENTERING )')
3034  FORMAT(5X,'CUMULATED BEDLOAD BOUNDARY ',I4,'           = ', G16.7
     &      ,'  ( KG )')
2031  FORMAT(5X,'CUMULATED LOST MASS (INI-FINAL+FLUXES)    = ',
     & G16.7,'  ( KG )')

!
!-----------------------------------------------------------------------
!
        RETURN
        END
