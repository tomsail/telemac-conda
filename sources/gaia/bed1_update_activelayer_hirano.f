!                 *****************************************
                  SUBROUTINE BED1_UPDATE_ACTIVELAYER_HIRANO
!                 *****************************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief  Mass transfer between the active layer (first layer) and the
!!        layer underneath. This is possible and needed only if there
!!        is no consolidation. The algorithm is based on the classical
!!        active layer formulation of Hirano, improved to consider
!!        cases involving sand, mud or sand-mud mixtures.
!!
!!        This subroutine updates mass_sand and mass_mud using their mass
!!        flux.
!!
!!        For the case of consolidation, the mass transfer between layers
!!        is only caused by consolidation and the active layer is virtual
!!        and therefore recomputed when needed from the consolidation layers
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA, ONLY: MIN_SED_MASS_COMP,ES,MASS_SAND,
     &    MASS_MUD,NSAND,NMUD,NPOIN,NOMBLAY,XKV0,CONC_MUD,
     &    ELAY,CONC_MUD_ACTIV_TEMPO,MASS_MUD_ACTIV_TEMPO,
     &    FLUX_MASS_MUD,FLUX_MASS_SAND,NUM_TRANSF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      INTEGER IPOIN,ILAYER,ISAND,IMUD
      DOUBLE PRECISION THICK_TRANSFER,THICK_TRANSFER_TEMPO
      DOUBLE PRECISION RATIO_XKV
!
!----------------------------------------------------------------------
!
!     INITIALIZATION
!
      DO IPOIN=1,NPOIN
        DO ILAYER=1,NOMBLAY
          DO IMUD=1,NMUD
            FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)=0.D0
          ENDDO
          DO ISAND=1,NSAND
            FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)=0.D0
          ENDDO
        ENDDO
      ENDDO


      CALL ACTIVELAYER

!
!     COMPUTATION OF THE THICKNESS FOR TRANSFER
!
      DO IPOIN=1,NPOIN
!       TOTAL MASS MUD AND CONCENTRATION OF MUD IN THE ACTIVE LAYER BEFORE UPDATE
        CONC_MUD(1,IPOIN)=
     &      MAX(CONC_MUD(1,IPOIN),CONC_MUD(2,IPOIN))
        CONC_MUD(1,IPOIN)=
     &      MIN(CONC_MUD(1,IPOIN),CONC_MUD(NOMBLAY,IPOIN))
!
        CONC_MUD_ACTIV_TEMPO(IPOIN)= CONC_MUD(1,IPOIN)
!
        MASS_MUD_ACTIV_TEMPO(IPOIN)= 0.D0
        DO IMUD = 1,NMUD
          MASS_MUD_ACTIV_TEMPO(IPOIN)= MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &                                 MASS_MUD(IMUD,1,IPOIN)
        ENDDO
        THICK_TRANSFER = ES(IPOIN,1)- ELAY%R(IPOIN)
!       POSITIVE SIGNE MEANS DEPOSITION, NEGATIVE MEANS EROSION
!
!-----------------------------------------------------------------------
!       DEPOSITION CASE
!-----------------------------------------------------------------------
!
        IF(THICK_TRANSFER.GT.0.D0) THEN
!         ACTIVE LAYER TOO LARGE: TRANSFER OF MASS FROM UPPER LAYER TO
!         LOWER LAYER
          DO ILAYER=2,NOMBLAY
            IF(CONC_MUD_ACTIV_TEMPO(IPOIN).LE.
     &              CONC_MUD(ILAYER,IPOIN))THEN
              NUM_TRANSF(IPOIN)= MAX(ILAYER-1,2)
              GOTO 200
            ENDIF
            NUM_TRANSF(IPOIN)= NOMBLAY
          ENDDO
200    CONTINUE
!
          IF(NMUD.GE.1) THEN
            DO IMUD = 1,NMUD
              FLUX_MASS_MUD(IMUD,1,IPOIN)= -(THICK_TRANSFER/ES(IPOIN,1))
     &         *MASS_MUD(IMUD,1,IPOIN)
              FLUX_MASS_MUD(IMUD,NUM_TRANSF(IPOIN),IPOIN)=
     &         +(THICK_TRANSFER/ES(IPOIN,1))
     &         *MASS_MUD(IMUD,1,IPOIN)
            ENDDO
          ENDIF
          IF(NSAND.GE.1) THEN
            DO ISAND=1,NSAND
            FLUX_MASS_SAND(ISAND,1,IPOIN)=-(THICK_TRANSFER/ES(IPOIN,1))
     &       *MASS_SAND(ISAND,1,IPOIN)
            FLUX_MASS_SAND(ISAND,NUM_TRANSF(IPOIN),IPOIN)=
     &       +(THICK_TRANSFER/ES(IPOIN,1))
     &       *MASS_SAND(ISAND,1,IPOIN)
            ENDDO
          ENDIF
          THICK_TRANSFER_TEMPO = 0.D0
!         IN THIS CASE: CONCENTRATION OF MUD IN THE ACTIVE LAYER DONT CHANGE
!
!-----------------------------------------------------------------------
!        EROSION CASE
!-----------------------------------------------------------------------
!
        ELSEIF(THICK_TRANSFER.LT.0.D0) THEN
!         ACTIVE LAYER TOO SMALL: TRANSFER OF MASS FROM SUBLAYER TO
!         UPPER LAYER
          THICK_TRANSFER_TEMPO = ABS(THICK_TRANSFER)

          DO ILAYER =2,NOMBLAY
!
            RATIO_XKV = (1.D0-XKV0(ILAYER))/(1.D0-XKV0(1))
!
            IF(THICK_TRANSFER_TEMPO.GE.ES(IPOIN,ILAYER)*RATIO_XKV) THEN
!
              IF(NMUD.GE.1) THEN
                DO IMUD=1,NMUD
                  FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)=
     &             FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)-
     &             MASS_MUD(IMUD,ILAYER,IPOIN)

                  FLUX_MASS_MUD(IMUD,1,IPOIN)=
     &             FLUX_MASS_MUD(IMUD,1,IPOIN)+
     &             MASS_MUD(IMUD,ILAYER,IPOIN)
!
                  IF(MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &               MASS_MUD(IMUD,ILAYER,IPOIN).GE.
     &               MIN_SED_MASS_COMP) THEN

!              UPDATE CONCENTRATION OF MUD IN THE ACTIVE LAYER
                  CONC_MUD_ACTIV_TEMPO(IPOIN)=
     &             (CONC_MUD_ACTIV_TEMPO(IPOIN)*
     &              MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &              CONC_MUD(ILAYER,IPOIN)*MASS_MUD(IMUD,ILAYER,IPOIN))/
     &         (MASS_MUD_ACTIV_TEMPO(IPOIN)+MASS_MUD(IMUD,ILAYER,IPOIN))

                  MASS_MUD_ACTIV_TEMPO(IPOIN)=
     &          MASS_MUD_ACTIV_TEMPO(IPOIN)+MASS_MUD(IMUD,ILAYER,IPOIN)
                  ELSE
                    CONC_MUD_ACTIV_TEMPO(IPOIN)=CONC_MUD(ILAYER,IPOIN)
                  ENDIF
                ENDDO
              ENDIF
              IF(NSAND.GE.1) THEN
                DO ISAND=1,NSAND
                  FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)=
     &             FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)-
     &             MASS_SAND(ISAND,ILAYER,IPOIN)

                  FLUX_MASS_SAND(ISAND,1,IPOIN)=
     &             FLUX_MASS_SAND(ISAND,1,IPOIN)+
     &             MASS_SAND(ISAND,ILAYER,IPOIN)
                ENDDO
              ENDIF
              THICK_TRANSFER_TEMPO = THICK_TRANSFER_TEMPO
     &                             - ES(IPOIN,ILAYER)*RATIO_XKV
            ELSE
              IF(NMUD.GE.1) THEN
                DO IMUD=1,NMUD
                  FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)=
     &            FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)-
     &               (THICK_TRANSFER_TEMPO/(ES(IPOIN,ILAYER)*RATIO_XKV))
     &                *MASS_MUD(IMUD,ILAYER,IPOIN)
!
                FLUX_MASS_MUD(IMUD,1,IPOIN)=
     &                FLUX_MASS_MUD(IMUD,1,IPOIN)+
     &               (THICK_TRANSFER_TEMPO/(ES(IPOIN,ILAYER)*RATIO_XKV))
     &                *MASS_MUD(IMUD,ILAYER,IPOIN)
!              UPDATE CONCENTRATION OF MUD IN THE ACTIVE LAYER
                  IF(MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &             (THICK_TRANSFER_TEMPO/(ES(IPOIN,ILAYER)*RATIO_XKV))
     &             *MASS_MUD(IMUD,ILAYER,IPOIN).GE.MIN_SED_MASS_COMP)
     &               THEN
                CONC_MUD_ACTIV_TEMPO(IPOIN)=
     &             (CONC_MUD_ACTIV_TEMPO(IPOIN)*
     &              MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &              CONC_MUD(ILAYER,IPOIN)*
     &             (THICK_TRANSFER_TEMPO/(ES(IPOIN,ILAYER)*RATIO_XKV))
     &              *MASS_MUD(IMUD,ILAYER,IPOIN))/
     &             (MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &             (THICK_TRANSFER_TEMPO/(ES(IPOIN,ILAYER)*RATIO_XKV))
     &             *MASS_MUD(IMUD,ILAYER,IPOIN))
                  ELSE
                    CONC_MUD_ACTIV_TEMPO(IPOIN)=CONC_MUD(ILAYER,IPOIN)
                  ENDIF
!
                  MASS_MUD_ACTIV_TEMPO(IPOIN)=
     &              MASS_MUD_ACTIV_TEMPO(IPOIN)+
     &             (THICK_TRANSFER_TEMPO/(ES(IPOIN,ILAYER)*RATIO_XKV))
     &             *MASS_MUD(IMUD,ILAYER,IPOIN)
                ENDDO
              ENDIF
              IF(NSAND.GE.1) THEN
                DO ISAND=1,NSAND
                  FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)=
     &                FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)-
     &               (THICK_TRANSFER_TEMPO/(ES(IPOIN,ILAYER)*RATIO_XKV))
     &                *MASS_SAND(ISAND,ILAYER,IPOIN)

                  FLUX_MASS_SAND(ISAND,1,IPOIN)=
     &                FLUX_MASS_SAND(ISAND,1,IPOIN)+
     &               (THICK_TRANSFER_TEMPO/(ES(IPOIN,ILAYER)*RATIO_XKV))
     &                *MASS_SAND(ISAND,ILAYER,IPOIN)
                ENDDO
              ENDIF
              THICK_TRANSFER_TEMPO = 0.D0
            GOTO 100
            ENDIF
          ENDDO
100     CONTINUE
        ENDIF ! THICK_TRANSFER.GE.0.D0
        IF(NMUD.GE.1) THEN
! CHECK CONCENTRATION OF ACTIVE LAYER AND UPDATE CONC_MUD(1,IPOIN)
          IF(CONC_MUD_ACTIV_TEMPO(IPOIN).LE.CONC_MUD(2,IPOIN)) THEN

            CONC_MUD(1,IPOIN)= CONC_MUD(2,IPOIN)
          ELSEIF(CONC_MUD_ACTIV_TEMPO(IPOIN).GE.
     &                CONC_MUD(NOMBLAY,IPOIN)) THEN
            CONC_MUD(1,IPOIN)=CONC_MUD(NOMBLAY,IPOIN)
          ELSE
            CONC_MUD(1,IPOIN)= CONC_MUD_ACTIV_TEMPO(IPOIN)
          ENDIF
        ENDIF
      ENDDO ! NPOIN
!
!-----------------------------------------------------------------------
!     TRANSFER OF MUD MASSES
!-----------------------------------------------------------------------
!
      IF(NMUD.GE.1) THEN
        DO IPOIN=1,NPOIN
          DO ILAYER=1,NOMBLAY
            DO IMUD = 1,NMUD
              MASS_MUD(IMUD,ILAYER,IPOIN)= MASS_MUD(IMUD,ILAYER,IPOIN)
     &                                +FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!     TRANSFER OF SAND MASSES
!-----------------------------------------------------------------------
!
!     NEGATIVE FLUX_MASS_SAND MEANS THE FLUX IS FROM UPPER LAYER TO
!     LOWER LAYER
      IF(NSAND.GE.1) THEN
        DO IPOIN=1,NPOIN
          DO ILAYER=1,NOMBLAY
            DO ISAND=1,NSAND
              MASS_SAND(ISAND,ILAYER,IPOIN)=
     &               MASS_SAND(ISAND,ILAYER,IPOIN)
     &               +FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      RETURN
      END
