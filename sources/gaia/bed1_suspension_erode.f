!           ********************************
            SUBROUTINE BED1_SUSPENSION_ERODE
!           ********************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Computes erosion flux [kg*s-1*m-2] : fluer
!!        Update of mud/sand mass due to erosion
!!        Update of mass bed evolution due to erosion
!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_GAIA
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN
      INTEGER ILAYER,ISAND,IMUD,ICLA
      INTEGER NSAND_VIRTUAL
      DOUBLE PRECISION CHECK_POSITIF_MUD_TOT1,CHECK_POSITIF_MUD_TOT2
      DOUBLE PRECISION CHECK_POSITIF_MUD,CHECK_POSITIF_SAND
      DOUBLE PRECISION FLUER_MIX_TOT
      DOUBLE PRECISION INTERPOL,TIME_MIN
!
      IF(NSAND.EQ.0) THEN
        NSAND_VIRTUAL = 1
!       ON FAIT CELA POUR PASSER UNE FOIS DANS LA BOUCLE SABLE MEME S IL N Y EN PAS
!       CELA NE POSE PAS DE PB CAR RATIO_MUD_SAND=1
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            RATIO_SAND(NSAND_VIRTUAL,ILAYER,IPOIN)= 1.D0
          ENDDO
        ENDDO
      ELSE
        NSAND_VIRTUAL = NSAND
      ENDIF
!
      DO IPOIN=1,NPOIN
        DO ICLA=1,NSICLA
          FLUER%ADR(ICLA)%P%R(IPOIN)=0.D0
        ENDDO
      ENDDO
!
! INITIALIZATION OF PHYSICAL PARAMETERS OF THE ACTIVE LAYER IF MUD
      IF(HIRANO.AND.NMUD.GE.1) THEN
        DO IPOIN = 1,NPOIN

! UPDATE CRITICAL SHEAR STRESS OF MUD IN ACTIV LAYER IN FUNCTION OF CONCENTRATION OF MUD IN THE ACTIVE LAYER
! SIMPLE INTERPOLATION BETWEEN TOCE_MUD DISCRETIZATION GIVEN BY USER
            IF(CONC_MUD(1,IPOIN).LE.CONC_MUD(2,IPOIN))THEN
              CONC_MUD(1,IPOIN)=CONC_MUD(2,IPOIN)
              TOCE_MUD(1,IPOIN)=TOCE_MUD(2,IPOIN)
              PARTHENIADES(1,IPOIN)=PARTHENIADES(2,IPOIN)
            ELSEIF(CONC_MUD(1,IPOIN).GE.
     &                CONC_MUD(NOMBLAY,IPOIN))THEN
                    CONC_MUD(1,IPOIN)=CONC_MUD(NOMBLAY,IPOIN)
                    TOCE_MUD(1,IPOIN)=TOCE_MUD(NOMBLAY,IPOIN)
                    PARTHENIADES(1,IPOIN)=PARTHENIADES(NOMBLAY,IPOIN)
            ELSE
              DO ILAYER=2,NOMBLAY
                IF(CONC_MUD(1,IPOIN).LE.
     &                CONC_MUD(ILAYER,IPOIN))THEN
                  INTERPOL=(CONC_MUD(1,IPOIN)-
     &                      CONC_MUD(ILAYER-1,IPOIN))/
     &                     (CONC_MUD(ILAYER,IPOIN)-
     &                      CONC_MUD(ILAYER-1,IPOIN))
!
                TOCE_MUD(1,IPOIN)=INTERPOL*
     &                   (TOCE_MUD(ILAYER,IPOIN)
     &                   -TOCE_MUD(ILAYER-1,IPOIN))
     &                   +TOCE_MUD(ILAYER-1,IPOIN)
!
                PARTHENIADES(1,IPOIN)=INTERPOL*
     &                   (PARTHENIADES(ILAYER,IPOIN)
     &                   -PARTHENIADES(ILAYER-1,IPOIN))
     &                   +PARTHENIADES(ILAYER-1,IPOIN)
                GOTO 200
              ENDIF
            ENDDO !END LOOP NOMBLAY
200         CONTINUE
          ENDIF
!
          CONC_MUD_ACTIV_TEMPO(IPOIN)= CONC_MUD(1,IPOIN)
!
        ENDDO !END LOOP NPOIN
      ENDIF !ENDIF HIRANO.AND.NMUD.GE.1
!
! INITIALIZATION OF SHEAR STRESS OF MIXTURE AND EQUILIBRIUM CONCENTRATION FOR EACH LAYER
!
      DO ILAYER = 1,NOMBLAY
!
          DO ISAND = 1,NSAND_VIRTUAL
!           TOCE_SAND(ISAND,IPOIN) IS COMPUTED IN INIT_SEDIMENT_GAIA
!>          @todo AJOUTER LE HIDING FACTOR SUR tOC POUR COHERENCE AVEC BEDLOAD!
!           CE N'EST PAS FAIT ACTUELLMENT DANS SISYPHE POUR LA SUSPENSION
            DO IPOIN=1,NPOIN
              IF(RATIO_MUD_SAND(ILAYER,IPOIN).LE.0.3D0)THEN
                TOCE_MIX(ISAND,ILAYER,IPOIN)= TOCE_SAND(ISAND,IPOIN)
                RATIO_TOCE%ADR(NUM_ISAND_ICLA(ISAND))%P%R(IPOIN)= 1.D0
              ELSEIF(RATIO_MUD_SAND(ILAYER,IPOIN).GE.0.5D0)THEN
                TOCE_MIX(ISAND,ILAYER,IPOIN) = TOCE_MUD(ILAYER,IPOIN)
!
                IF(NSAND.NE.0)THEN
                  RATIO_TOCE%ADR(NUM_ISAND_ICLA(ISAND))%P%R(IPOIN)=
     &                 1.D0
!-----------not use in this case and Fix at 1
                ENDIF
!
              ELSE
                TOCE_MIX(ISAND,ILAYER,IPOIN) =
     &            (RATIO_MUD_SAND(ILAYER,IPOIN)-0.3D0)/(0.5D0-0.3D0)
     &           *(TOCE_MUD(ILAYER,IPOIN) - TOCE_SAND(ISAND,IPOIN))
     &           + TOCE_SAND(ISAND,IPOIN)
!
                RATIO_TOCE%ADR(NUM_ISAND_ICLA(ISAND))%P%R(IPOIN)=
     &           TOCE_MIX(ISAND,ILAYER,IPOIN)/TOCE_SAND(ISAND,IPOIN)
              ENDIF
!!!!!!!!!!!!warning RATIO_TOCE IS NOT SAVE WITH NOMBLAY
            ENDDO !END LOOP NPOIN
          ENDDO !END LOOP NSAND_VIRTUAL
!
          DO ISAND = 1,NSAND
            IF(NSAND.NE.0)THEN
              IF(SUSP_SAND)THEN
!                COMPUTE EQUILIBRIUM CONCENTRATION FOR EACH LAYER!! IT IS NOT NECESSARY FOR EACH POINT
!                 CAN BE OPTIMIZED IF NO LOOP ON NPOIN IN COMPUTE CAE
                CALL SUSPENSION_COMPUTE_CAE(TAUP,HN,
     &          DCLA(NUM_ISAND_ICLA(ISAND)),NPOIN,CHARR,
     &          XMVE,XMVS0(NUM_ISAND_ICLA(ISAND)),VCE,GRAV,
     &          ZERO,ZREF,AC(NUM_ISAND_ICLA(ISAND)),
     &          CSTAEQ%ADR(NUM_ISAND_ICLA(ISAND))%P,
     &                QSCL_C%ADR(NUM_ISAND_ICLA(ISAND))%P,
     &          ICQ,U2D,V2D,CSRATIO%ADR(NUM_ISAND_ICLA(ISAND))%P,DEBUG,
     &          RATIO_TOCE%ADR(NUM_ISAND_ICLA(ISAND))%P)
              ELSE
                DO IPOIN=1,NPOIN
                  CSTAEQ%ADR(NUM_ISAND_ICLA(ISAND))%P%R(IPOIN)=0.D0
                ENDDO
              ENDIF
!
            ENDIF
!!!!!!!!!!!!!warning CSTAEQ IS NOT SAVE WITH NOMBLAY
!!!!!!!!!!!!!for this time:
            DO IPOIN=1,NPOIN
              CAE_ILAY(ISAND,ILAYER,IPOIN) =
     &           CSTAEQ%ADR(NUM_ISAND_ICLA(ISAND))%P%R(IPOIN)
            ENDDO
          ENDDO !END LOOP NSAND
        ENDDO  !END LOOP NOMBLAY
!
!!!!!!!STRAT EROSION
      DO IPOIN = 1,NPOIN
!
        DO IMUD = 1,NMUD
          QER_MUD(IMUD) = 0.D0
        ENDDO
        DO ISAND = 1,NSAND_VIRTUAL
          QER_SAND(ISAND) = 0.D0
          TIME(ISAND) = DT
        ENDDO
!
        DO ILAYER = 1,NOMBLAY
!
!       COMPUTE PUR SAND FLUX
!
          DO ISAND = 1,NSAND
            IF(NSAND.NE.0)THEN
!        COMPUTE CAE MUST BE CALL HERE IF NO LOOP ON NPOIN
              FLUER_PUR_SAND(ISAND,IPOIN) =
     &        CAE_ILAY(ISAND,ILAYER,IPOIN)*XWC(NUM_ISAND_ICLA(ISAND))
            ELSE
              CAE_ILAY(ISAND,ILAYER,IPOIN) = 0.D0
              FLUER_PUR_SAND(ISAND,IPOIN) = 0.D0
            ENDIF
          ENDDO
!        COMPUTE PUR MUD FLUX
          IF(NMUD.GT.0)THEN
            DO ISAND = 1,NSAND_VIRTUAL
              IF(TAUP%R(IPOIN).GT.TOCE_MIX(ISAND,ILAYER,IPOIN)) THEN
                FLUER_PUR_MUD(ISAND,IPOIN) = PARTHENIADES(ILAYER,IPOIN)
     &         *(TAUP%R(IPOIN)/TOCE_MIX(ISAND,ILAYER,IPOIN) - 1.D0)
              ELSE
                FLUER_PUR_MUD(ISAND,IPOIN) = 0.D0
              ENDIF
            ENDDO
          ELSE
            DO ISAND = 1,NSAND_VIRTUAL
              FLUER_PUR_MUD(ISAND,IPOIN) = 0.D0
            ENDDO
          ENDIF
!
!        COMPUTE MIX FLUXES SAND-MUD
          FLUER_MIX_TOT = 0.D0
!
          DO ISAND = 1,NSAND_VIRTUAL
!
            IF(TAUP%R(IPOIN).GT.TOCE_MIX(ISAND,ILAYER,IPOIN))THEN
              IF(RATIO_MUD_SAND(ILAYER,IPOIN).LE.0.3D0)THEN
!               MUD RATIO < 30%, (PURE SAND FLUX)
                FLUER_MIX(ISAND,IPOIN)= FLUER_PUR_SAND(ISAND,IPOIN)
!               MUD RATIO > 50%, (PURE MUD FLUX)
              ELSEIF(RATIO_MUD_SAND(ILAYER,IPOIN).GE.0.5D0)THEN
                FLUER_MIX(ISAND,IPOIN) = FLUER_PUR_MUD(ISAND,IPOIN)
!               MUD RATIO >30% AND <50%, (INTERPOLATION)
              ELSE
                FLUER_MIX(ISAND,IPOIN) = (RATIO_MUD_SAND(ILAYER,IPOIN)
     &               - 0.3D0)/(0.5D0-0.3D0)
     &               *(FLUER_PUR_MUD(ISAND,IPOIN)
     &                 -FLUER_PUR_SAND(ISAND,IPOIN))
     &               + FLUER_PUR_SAND(ISAND,IPOIN)
              ENDIF
            ELSE
              FLUER_MIX(ISAND,IPOIN) = 0.D0
            ENDIF
!
            FLUER_MIX_TOT = FLUER_MIX_TOT
     &           + FLUER_MIX(ISAND,IPOIN)*RATIO_SAND(ISAND,ILAYER,IPOIN)
!
          ENDDO ! END LOOP NSAND_VIRTUAL
!
          IF(FLUER_MIX_TOT.LE.0.D0.AND.MASS_MIX_TOT(ILAYER,IPOIN).GE.
     &         MIN_SED_MASS_COMP) GOTO 10
!         des valeurs infimes de masse ne peuvent pas faire du pavage, on laisse la possibilite d eroder la couche en dessous
!         EXIT LAYER LOOP
          DO ISAND = 1,NSAND_VIRTUAL
!           COMPUTATION OF ERODABLE QUANTITY
            QE_MOY(ISAND) = FLUER_MIX(ISAND,IPOIN)*
     &           RATIO_SAND(ISAND,ILAYER,IPOIN)*TIME(ISAND)*MOFAC_BED
!
            IF(QE_MOY(ISAND).GT.0.D0) THEN
!
              IF(QE_MOY(ISAND).LT.
     &             (MASS_MUD_TOT(ILAYER,IPOIN)
     &             *RATIO_SAND(ISAND,ILAYER,IPOIN)
     &             +MASS_SAND(ISAND,ILAYER,IPOIN))) THEN
!
!             IF ERODABLE QUANTITY < MASS IN LAYER , THE LAYER IS PARTIALLY ERODED
!             AND NO TIME TO ERODE THE NEXT LAYER
!
!             QUANTITY OF MUD ERODED RESPECT RATIO_MUD_SAND
                CHECK_POSITIF_MUD_TOT1 =
     &               MIN(MASS_MUD_TOT(ILAYER,IPOIN),
     &               QE_MOY(ISAND)*RATIO_MUD_SAND(ILAYER,IPOIN))
                CHECK_POSITIF_MUD_TOT2 = 0.D0
!
                DO IMUD = 1,NMUD
!             QUANTITY OF EACH CLASS OF MUD ERODED RESPECT RATIO_MUD
                  CHECK_POSITIF_MUD =
     &                 MIN(MASS_MUD(IMUD,ILAYER,IPOIN),
     &                 CHECK_POSITIF_MUD_TOT1
     &                *RATIO_MUD(IMUD,ILAYER,IPOIN))
                  MASS_MUD(IMUD,ILAYER,IPOIN) =
     &                 MASS_MUD(IMUD,ILAYER,IPOIN) - CHECK_POSITIF_MUD
                  QER_MUD(IMUD) = QER_MUD(IMUD) + CHECK_POSITIF_MUD
                  CHECK_POSITIF_MUD_TOT2 =
     &                 CHECK_POSITIF_MUD_TOT2 + CHECK_POSITIF_MUD
                ENDDO
!
!             QUANTITY OF SAND ERODED
                CHECK_POSITIF_SAND = MIN(MASS_SAND(ISAND,ILAYER,IPOIN),
     &               (1.D0-RATIO_MUD_SAND(ILAYER,IPOIN))*QE_MOY(ISAND))

                QER_SAND(ISAND) = QER_SAND(ISAND) + CHECK_POSITIF_SAND
                MASS_SAND(ISAND,ILAYER,IPOIN) =
     &               MASS_SAND(ISAND,ILAYER,IPOIN) - CHECK_POSITIF_SAND
                TIME(ISAND) = 0.D0
                !
              ELSE
!
!             IF ERODABLE QUANTITY > MASS IN LAYER , THE LAYER IS FULLY ERODED
!             AND THE TIME REMAINING FOR EROSION OF NEXT LAYER  IS COMPUTED
!
!             QUANTITY OF MUD ERODED RESPECT RATIO_SAND
                CHECK_POSITIF_MUD_TOT1 = MIN(MASS_MUD_TOT(ILAYER,IPOIN),
     &               MASS_MUD_TOT(ILAYER,IPOIN)
     &               *RATIO_SAND(ISAND,ILAYER,IPOIN))
!
                CHECK_POSITIF_MUD_TOT2 = 0.D0
                DO IMUD = 1,NMUD
!             QUANTITY OF EACH CLASS OF MUD ERODED RESPECT RATIO_MUD
                  CHECK_POSITIF_MUD =
     &                 MIN(MASS_MUD(IMUD,ILAYER,IPOIN),
     &                 CHECK_POSITIF_MUD_TOT1
     &                 *RATIO_MUD(IMUD,ILAYER,IPOIN))
!
                  MASS_MUD(IMUD,ILAYER,IPOIN)=
     &                 MASS_MUD(IMUD,ILAYER,IPOIN) - CHECK_POSITIF_MUD
!
                  QER_MUD(IMUD) = QER_MUD(IMUD) + CHECK_POSITIF_MUD
!
                  CHECK_POSITIF_MUD_TOT2 =
     &                 CHECK_POSITIF_MUD_TOT2 + CHECK_POSITIF_MUD
                ENDDO
!             QUANTITY OF SAND ERODED
                CHECK_POSITIF_SAND =
     &               MAX(MASS_SAND(ISAND,ILAYER,IPOIN),0.D0)
                MASS_SAND(ISAND,ILAYER,IPOIN) =
     &               MASS_SAND(ISAND,ILAYER,IPOIN) - CHECK_POSITIF_SAND
                QER_SAND(ISAND) = QER_SAND(ISAND) + CHECK_POSITIF_SAND
!
!             TIME REMAINING FOR EROSION OF NEXT LAYER IS COMPUTED
                TIME(ISAND) = TIME(ISAND) -
     &             ((CHECK_POSITIF_MUD_TOT2 + CHECK_POSITIF_SAND)
     &             /(QE_MOY(ISAND)/TIME(ISAND)))
              ENDIF
!
            ENDIF
!
          ENDDO                 ! END LOOP ISAND_VIRTUAL
!
          TIME_MIN = DT
!
          DO ISAND = 1,NSAND_VIRTUAL
            TIME_MIN = MIN(TIME_MIN,TIME(ISAND))
!           IF REMAINING TIME IS NUL FOR ONE CLASS OF SAND, NO EROSION OF NEXT LAYER
            IF(TIME(ISAND).LE.0.D0) GOTO 10
!           EXIT LAYER LOOP
          ENDDO
!
          DO ISAND = 1,NSAND_VIRTUAL
            TIME(ISAND)=TIME_MIN
          ENDDO
!
        ENDDO                   ! END LOOP NOMBLAY
10      CONTINUE
!
        DO IMUD = 1,NMUD
          FLUER%ADR(NUM_IMUD_ICLA(IMUD))%P%R(IPOIN) =
     &       MAX(QER_MUD(IMUD)/DT,0.D0)/MOFAC_BED
!         ADD MUD PART INCLUDED IN BEDLOAD
          FLUER%ADR(NUM_IMUD_ICLA(IMUD))%P%R(IPOIN) =
     &      FLUER%ADR(NUM_IMUD_ICLA(IMUD))%P%R(IPOIN)+
     &      F_MUDB%ADR(NUM_IMUD_ICLA(IMUD))%P%R(IPOIN)
     &      /MOFAC_BED
        ENDDO
!
        DO ISAND = 1,NSAND
          FLUER%ADR(NUM_ISAND_ICLA(ISAND))%P%R(IPOIN) =
     &       MAX(QER_SAND(ISAND)/DT,0.D0)/MOFAC_BED
        ENDDO
!
      ENDDO ! END LOOP NPOIN
!
!     MASS EVOLUTION DUE TO SUSPENSION: NEGATIVE SINCE EROSION
      DO IPOIN=1,NPOIN
        DO ICLA=1,NSICLA
          EVCL_MS%ADR(ICLA)%P%R(IPOIN)=
     &      -FLUER%ADR(ICLA)%P%R(IPOIN)*DT
        ENDDO
      ENDDO
!
      RETURN
      END SUBROUTINE BED1_SUSPENSION_ERODE
