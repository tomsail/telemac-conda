!                   *****************************
                    SUBROUTINE BED1_SUSPENSION_DEPOSIT
!                   *****************************
!
     &(CODE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief  Computes first layer deposition.
!!        Update of mud/sand mass due to deposition.
!!        Update of mass bed evolution due to deposition.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] CODE  Hydrodynamic code in case of coupling
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IPOIN,IMUD,ISAND,ICLA,ISUSP
      INTEGER LAYER_DEPOSIT_SAND,LAYER_DEPOSIT_MUD
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LAYER_DEPOSIT_SAND = 1
      LAYER_DEPOSIT_MUD = 1
      ISAND = 0
      IMUD = 0

! here mass_* is in [kg/m^2] like in the sub suspension_erode
      DO ISUSP = 1,NSUSP_TEL
        ICLA = NUM_ISUSP_ICLA(ISUSP)
        IF(ICLA.NE.0) THEN
          IF(SEDCO(ICLA))THEN
            IMUD = NUM_ICLA_IMUD(ICLA)
! IN 3D, TOCD IS NOT USED, BECAUSE VERTICAL TURBULENCE IS COMPUTED
            DO IPOIN=1,NPOIN
!
              IF(HIRANO)THEN
                IF(LAYER_DEPOSIT_MUD.EQ.1)THEN
!                 TOTAL MASS MUD AND CONCENTRATION OF MUD IN THE ACTIVE LAYER BEFORE UPDATE
                  CONC_MUD(1,IPOIN)=
     &               MAX(CONC_MUD(1,IPOIN),CONC_MUD(2,IPOIN))
!
                  CONC_MUD(1,IPOIN)= 
     &                MIN(CONC_MUD(1,IPOIN),CONC_MUD(NOMBLAY,IPOIN))
!
                  CONC_MUD_ACTIV_TEMPO(IPOIN)= CONC_MUD(1,IPOIN)
!
                  MASS_MUD_ACTIV_TEMPO(IPOIN)= 0.D0
                  DO I = 1,NMUD
                    MASS_MUD_ACTIV_TEMPO(IPOIN)=
     &                    MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &                    MASS_MUD(I,1,IPOIN)
                  ENDDO
                ELSE
                  WRITE(LU,*)'LAYER DEPOSIT MUST BE EGAL',
     &                         'TO 1 WITH HIRANO'
                  CALL PLANTE(1)
                  STOP
                ENDIF
              ENDIF
!
!             MASS EVOLUTION DUE TO SUSPENSION
              EVCL_MS%ADR(ICLA)%P%R(IPOIN)=EVCL_MS%ADR(ICLA)%P%R(IPOIN)
     &            +FLUDP%ADR(ICLA)%P%R(IPOIN)*DT*MOFAC_BED
!             UPDATE OF MUD MASS WITH EVOLUTION DUE TO SUSPENSION
!             (OF THE FIRST LAYER)
              MASS_MUD(IMUD,LAYER_DEPOSIT_MUD,IPOIN) =
     &          MASS_MUD(IMUD,LAYER_DEPOSIT_MUD,IPOIN)
     &          + FLUDP%ADR(ICLA)%P%R(IPOIN)*DT*MOFAC_BED
!
!               UPDATE CONCENTRATION OF MUD IN THE ACTIVE LAYER
              IF(HIRANO)THEN
                IF(MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &             FLUDP%ADR(ICLA)%P%R(IPOIN)*DT*MOFAC_BED.GE.
     &             MIN_SED_MASS_COMP) THEN
!
                  CONC_MUD_ACTIV_TEMPO(IPOIN)=
! DEPOSIT IS   MADE WITH THE LOWER CONCENTRATION OF LAYERS CONC_MUD(2,*)
     &            ( ( CONC_MUD_ACTIV_TEMPO(IPOIN)
     &              * MASS_MUD_ACTIV_TEMPO(IPOIN))
     &            + ( FLUDP%ADR(ICLA)%P%R(IPOIN)*DT*MOFAC_BED
     &                * CONC_MUD(2,IPOIN)))
     &            / ( MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &                FLUDP%ADR(ICLA)%P%R(IPOIN)*DT*MOFAC_BED)
! UPDATE CRIT  ICAL SHEAR STRESS OF MUD IN ACTIV LAYER IN FUNCTION OF CONCENTRATION OF MUD IN THE ACTIVE LAYER
! SIMPLE INTE  RPOLATION BETWEEN TOCE_MUD DISCRETIZATION GIVEN BY USER
                  IF(CONC_MUD_ACTIV_TEMPO(IPOIN)
     &               .LE.CONC_MUD(2,IPOIN)) THEN
                    CONC_MUD(1,IPOIN) = CONC_MUD(2,IPOIN)
                  ELSEIF(CONC_MUD_ACTIV_TEMPO(IPOIN).GE.
     &                   CONC_MUD(NOMBLAY,IPOIN))THEN
                    CONC_MUD(1,IPOIN) = CONC_MUD(NOMBLAY,IPOIN)
                  ELSE
                    CONC_MUD(1,IPOIN) = CONC_MUD_ACTIV_TEMPO(IPOIN)
                  ENDIF
                ELSE
                  CONC_MUD(1,IPOIN) = CONC_MUD_ACTIV_TEMPO(IPOIN)
                ENDIF
              ENDIF
            ENDDO
          ELSE
            ISAND = NUM_ICLA_ISAND(ICLA)
            DO IPOIN = 1,NPOIN
!           MASS EVOLUTION DUE TO SUSPENSION
              EVCL_MS%ADR(ICLA)%P%R(IPOIN)=EVCL_MS%ADR(ICLA)%P%R(IPOIN)
     &          +FLUDP%ADR(ICLA)%P%R(IPOIN)*DT*MOFAC_BED
!             UPDATE OF SAND MASS WITH EVOLUTION DUE TO SUSPENSION
!            (OF THE FIRST LAYER)
              MASS_SAND(ISAND,LAYER_DEPOSIT_SAND,IPOIN) =
     &          MASS_SAND(ISAND,LAYER_DEPOSIT_SAND,IPOIN)
     &        + FLUDP%ADR(ICLA)%P%R(IPOIN)*DT*MOFAC_BED
            ENDDO
          ENDIF
        ENDIF
      ENDDO
!
!     EVOLUTIONS FOR EACH CLASS ARE ADDED: TOTAL MASS EVOLUTION
!     FIXME: COMPUTE EVOL_MS IS NOT REALLY USEFULL BECAUSE MASS BALANCE
!     IS THEN COMPUTED FOR EACH CLASS AND THE SUM IS LOCALLY COMPUTED
!
!     INITIALISES
!
      CALL OS('X=0     ',X=EVOL_MS)
!
      DO I=1,NSICLA
        CALL OS('X=X+Y   ', X=EVOL_MS, Y=EVCL_MS%ADR(I)%P)
      ENDDO
!
      RETURN
      END
