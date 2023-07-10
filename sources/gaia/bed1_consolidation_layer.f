!           ***********************************
            SUBROUTINE BED1_CONSOLIDATION_LAYER
!           ***********************************
!
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Computes bed consolidation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER IPOIN,ILAYER,ISAND,IMUD
      DOUBLE PRECISION INTERPOL
!======================================================================
!    total mass of sediment in each point before consolidation
!    just for check that evolution of mass = 0
!>@todo can be removed if it works
!     INITIALISES
      CALL OS('X=0     ',X=EVOL_MC)
!    EVOL_MC%R() contained -mass of sediment before consolidation
!    after we add mass of sediment after consolidation to have evolution
!
      IF(NSAND.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO ISAND = 1,NSAND
              EVOL_MC%R(IPOIN) = EVOL_MC%R(IPOIN)
     &        - MASS_SAND(ISAND,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(NMUD.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO IMUD = 1,NMUD
              EVOL_MC%R(IPOIN) = EVOL_MC%R(IPOIN)
     &        - MASS_MUD(IMUD,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!-------------------------------------------------------------------------
!     For the active layer the concentration of mud is not constant in time
!     UPDATE TRANS_MASS OF MUD IN ACTIV LAYER IN FUNCTION OF CONCENTRATION
!     OF MUD IN THE ACTIVE LAYER:
!     SIMPLE INTERPOLATION BETWEEN TRANS_MASS DISCRETIZATION GIVEN BY USER
!     NUM_TRANSF IS LAYER NUMBER WHERE MASS OF ACTIVE LAYER IS TRANSFERED
      IF(HIRANO)THEN
        DO IPOIN = 1,NPOIN
          IF(CONC_MUD(1,IPOIN).LE.CONC_MUD(2,IPOIN))THEN
            CONC_MUD(1,IPOIN)=CONC_MUD(2,IPOIN)
            TRANS_MASS(1,IPOIN)= TRANS_MASS(2,IPOIN)
            NUM_TRANSF(IPOIN)= 2
          ELSEIF(CONC_MUD(1,IPOIN).GE.
     &                CONC_MUD(NOMBLAY,IPOIN))THEN
            CONC_MUD(1,IPOIN)= CONC_MUD(NOMBLAY,IPOIN)
            TRANS_MASS(1,IPOIN)= TRANS_MASS(NOMBLAY,IPOIN)
!           TRANS_MASS(NOMBLAY,IPOIN) is equal to 0
            NUM_TRANSF(IPOIN)= NOMBLAY
          ELSE
            DO ILAYER=2,NOMBLAY
              IF(CONC_MUD(1,IPOIN).LE.
     &                CONC_MUD(ILAYER,IPOIN))THEN
!
                INTERPOL=(CONC_MUD(1,IPOIN)-
     &                    CONC_MUD(ILAYER-1,IPOIN))/
     &                   (CONC_MUD(ILAYER,IPOIN)-
     &                    CONC_MUD(ILAYER-1,IPOIN))
!
                TRANS_MASS(1,IPOIN)=INTERPOL*
     &           (TRANS_MASS(ILAYER,IPOIN)-TRANS_MASS(ILAYER-1,IPOIN))
     &           +TRANS_MASS(ILAYER-1,IPOIN)
!
                NUM_TRANSF(IPOIN)= ILAYER
!
                GOTO 200
              ENDIF
            ENDDO
200         CONTINUE
          ENDIF
        ENDDO ! NPOIN
      ELSE
!       IF NOT HIRANO NUM_TRANSF = 0 is usefull
!       this can be done in the initialization step
        DO IPOIN = 1,NPOIN
          NUM_TRANSF(IPOIN)=0
        ENDDO
      ENDIF
!
!     For other layers the concentration of mud is fixed
!     The flux of mass from upper layer to layer is dM/dt=TRANS_MASS*M
!
!     Computes the mud flux transfer between layers
      DO IPOIN = 1,NPOIN
        DO ILAYER = 1,NOMBLAY
          IF(MASS_MUD_TOT(ILAYER,IPOIN).GE.MIN_SED_MASS_COMP) THEN
!
            FLUX_MASS_MUD_TOT(ILAYER,IPOIN) =
     &           TRANS_MASS(ILAYER,IPOIN)*MASS_MUD_TOT(ILAYER,IPOIN)*DT

!
            DO IMUD = 1,NMUD
!
              FLUX_MASS_MUD(IMUD,ILAYER,IPOIN) =
     &             MIN(MASS_MUD(IMUD,ILAYER,IPOIN),
     &             FLUX_MASS_MUD_TOT(ILAYER,IPOIN)
     &             *RATIO_MUD(IMUD,ILAYER,IPOIN))
!
            ENDDO
          ELSE
            DO IMUD = 1,NMUD
              FLUX_MASS_MUD(IMUD,ILAYER,IPOIN) = 0.D0
            ENDDO
          ENDIF
!         Computes the total flux transfer for all mud classes
          FLUX_MASS_MUD_TOT(ILAYER,IPOIN) = 0.D0
          DO IMUD = 1,NMUD
!
            FLUX_MASS_MUD_TOT(ILAYER,IPOIN) =
     &           FLUX_MASS_MUD_TOT(ILAYER,IPOIN)
     &           +FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)
!
          ENDDO
        ENDDO
      ENDDO
!     Computes the flux transfer of sand (valid for sand-mud mixtures)
      IF(NSAND.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO ISAND = 1,NSAND
              IF(MASS_MUD_TOT(ILAYER,IPOIN).GE.MIN_SED_MASS_COMP) THEN
!
                FLUX_MASS_SAND(ISAND,ILAYER,IPOIN) =
     &               FLUX_MASS_MUD_TOT(ILAYER,IPOIN)/
     &               MASS_MUD_TOT(ILAYER,IPOIN)
     &               *MASS_SAND(ISAND,ILAYER,IPOIN)
!
                FLUX_MASS_SAND(ISAND,ILAYER,IPOIN) =
     &               MAX(0.D0,FLUX_MASS_SAND(ISAND,ILAYER,IPOIN))
!
                FLUX_MASS_SAND(ISAND,ILAYER,IPOIN) =
     &               MIN(FLUX_MASS_SAND(ISAND,ILAYER,IPOIN),
     &               MASS_SAND(ISAND,ILAYER,IPOIN))
!
              ELSE
                FLUX_MASS_SAND(ISAND,ILAYER,IPOIN) = 0.D0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!     Mass transfer of mud
!
      IF(HIRANO)THEN
!       the active layer is treated independently
!       removing the flux from the active layer
        DO IPOIN = 1,NPOIN
          DO IMUD = 1,NMUD
            FLUX_NEG_MUD_ACTIV_LAYER(IMUD,IPOIN) =
     &               FLUX_MASS_MUD(IMUD,1,IPOIN)
            FLUX_MASS_MUD(IMUD,1,IPOIN)= 0.D0
          ENDDO
        ENDDO
!
!     Mass transfer of sand
!
        DO IPOIN = 1,NPOIN
          DO ISAND = 1,NSAND
            FLUX_NEG_SAND_ACTIV_LAYER(ISAND,IPOIN) =
     &               FLUX_MASS_SAND(ISAND,1,IPOIN)
            FLUX_MASS_SAND(ISAND,1,IPOIN)=0.D0
          ENDDO
        ENDDO
!       here we add the active layer flux for the suitable layer
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            IF(NUM_TRANSF(IPOIN).EQ.ILAYER)THEN
              DO IMUD = 1,NMUD
                FLUX_POS_MUD_ACTIV_LAYER(IMUD,ILAYER,IPOIN) =
     &                 FLUX_NEG_MUD_ACTIV_LAYER(IMUD,IPOIN)
              ENDDO
              DO ISAND = 1,NSAND
                FLUX_POS_SAND_ACTIV_LAYER(ISAND,ILAYER,IPOIN) =
     &                 FLUX_NEG_SAND_ACTIV_LAYER(ISAND,IPOIN)
              ENDDO
            ELSE
              DO IMUD = 1,NMUD
                FLUX_POS_MUD_ACTIV_LAYER(IMUD,ILAYER,IPOIN) =0.D0
              ENDDO
              DO ISAND = 1,NSAND
                FLUX_POS_SAND_ACTIV_LAYER(ISAND,ILAYER,IPOIN) =0.D0
              ENDDO
            ENDIF
          ENDDO
        ENDDO
!      ELSE
! if not hirano (init_zero)
!FLUX_POS_MUD_ACTIV_LAYER=0
!FLUX_NEG_MUD_ACTIV_LAYER=0
      ENDIF !HIRANO
!
!     GENERAL CASE (HIRANO OR NOT)
      DO IPOIN = 1,NPOIN
        DO ILAYER = 1,NOMBLAY
          DO IMUD = 1,NMUD
!
            IF(ILAYER.EQ.NOMBLAY) THEN
!
              MASS_MUD(IMUD,ILAYER,IPOIN) =
     &             MASS_MUD(IMUD,ILAYER,IPOIN)
     &             +FLUX_MASS_MUD(IMUD,ILAYER-1,IPOIN)
     &             +FLUX_POS_MUD_ACTIV_LAYER(IMUD,ILAYER,IPOIN)
!
            ELSEIF(ILAYER.EQ.1) THEN
!
              MASS_MUD(IMUD,ILAYER,IPOIN) =
     &             MASS_MUD(IMUD,ILAYER,IPOIN)
     &             -FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)
     &             -FLUX_NEG_MUD_ACTIV_LAYER(IMUD,IPOIN)
!
            ELSE
!
              MASS_MUD(IMUD,ILAYER,IPOIN) =
     &             MASS_MUD(IMUD,ILAYER,IPOIN)
     &             +FLUX_MASS_MUD(IMUD,ILAYER-1,IPOIN)
     &             -FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)
     &             +FLUX_POS_MUD_ACTIV_LAYER(IMUD,ILAYER,IPOIN)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!     mass transfer of sand
      IF(NSAND.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO ISAND = 1,NSAND
!
              IF(ILAYER.EQ.NOMBLAY) THEN
!
                MASS_SAND(ISAND,ILAYER,IPOIN) =
     &               MASS_SAND(ISAND,ILAYER,IPOIN)
     &               +FLUX_MASS_SAND(ISAND,ILAYER-1,IPOIN)
     &               +FLUX_POS_SAND_ACTIV_LAYER(ISAND,ILAYER,IPOIN)
!
              ELSEIF(ILAYER.EQ.1) THEN
!
                MASS_SAND(ISAND,ILAYER,IPOIN) =
     &               MASS_SAND(ISAND,ILAYER,IPOIN)
     &               -FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)
     &               -FLUX_NEG_SAND_ACTIV_LAYER(ISAND,IPOIN)
!
              ELSE
!
                MASS_SAND(ISAND,ILAYER,IPOIN) =
     &               MASS_SAND(ISAND,ILAYER,IPOIN)
     &               +FLUX_MASS_SAND(ISAND,ILAYER-1,IPOIN)
     &               -FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)
     &               +FLUX_POS_SAND_ACTIV_LAYER(ISAND,ILAYER,IPOIN)
!
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!    total mass of sediment in each point after consolidation
!    just for check that evolution of mass = 0
!    EVOLUTIONS FOR EACH CLASS ARE ADDED: TOTAL MASS EVOLUTION
!    EVOL_MC%R() contained -mass of sediment before consolidation
!    we add mass of sediment after consolidation to have evolution
!>@todo can be removed if it works
      IF(NSAND.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO ISAND = 1,NSAND
              EVOL_MC%R(IPOIN) = EVOL_MC%R(IPOIN)
     &        + MASS_SAND(ISAND,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(NMUD.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO IMUD = 1,NMUD
              EVOL_MC%R(IPOIN) =EVOL_MC%R(IPOIN)
     &        + MASS_MUD(IMUD,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END SUBROUTINE BED1_CONSOLIDATION_LAYER
