!                   *********************
                    SUBROUTINE BED1_UPDATE
!                   *********************
!
     &(ZR,ZF,VOLU2D)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Computes bed evolution
!!          Active layer is layer 1, it is kept at a prescribed
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in,out] ZR     Non erodable bed
!>@param[in,out] ZF     Elevation of bottom
!>@param[in]     VOLU2D Integral of test functions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: MASS_MIX_TOT,MASS_SAND_TOT,
     & MASS_MUD_TOT,MASS_MUD,MASS_SAND,NSAND,NMUD,RATIO_MUD,RATIO_SAND,
     & RATIO_MUD_SAND,NOMBLAY,CONC_MUD,ES,XKV0,XMVS0,MIN_SED_MASS_COMP,
     & NPOIN,MASSTOT,NUM_ICLA_IMUD,NUM_ICLA_ISAND,NSICLA,
     & NUM_ISAND_ICLA
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (BIEF_OBJ),  INTENT(INOUT)    :: ZR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)       :: VOLU2D
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,ILAYER,ISAND,IMUD,ICLA,K,J,I
      DOUBLE PRECISION TOT,TERM,DISCR,XMVS_LAY
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!--------------------------------------------------------------------------------------
!     ONLY MASS_SAND_TOT(1,IPOIN) SET TO ZERO SINCE AFTER BEDLOAD
!     MASS_SAND (BUT ONLY THE FIRST LAYER) IS UPDATED IN BEDLOAD_MAIN
!     to do for all the layers if masses are updated/computed somewhere before
!     VARIOUS MASSES ARE STILL IN [kg/m2]
      DO IPOIN = 1,NPOIN
!
        DO ILAYER = 1,NOMBLAY
          MASS_MIX_TOT(ILAYER,IPOIN) = 0.D0
          MASS_SAND_TOT(ILAYER,IPOIN) = 0.D0
          MASS_MUD_TOT(ILAYER,IPOIN) = 0.D0
!
          IF(NMUD.NE.0)THEN
            DO IMUD = 1,NMUD
!             THIS CLIPPING IS - A PRIORI - NOT MANDATORY
              IF(MASS_MUD(IMUD,ILAYER,IPOIN).LT.0.D0)THEN
                MASS_MUD(IMUD,ILAYER,IPOIN) = 0.D0
              ENDIF
            ENDDO
          ENDIF
!
          IF(NSAND.NE.0)THEN
            DO ISAND = 1,NSAND
!             THIS CLIPPING IS - A PRIORI - NOT MANDATORY
              IF(MASS_SAND(ISAND,ILAYER,IPOIN).LT.0.D0)THEN
                MASS_SAND(ISAND,ILAYER,IPOIN) = 0.D0
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------------
!     UPDATES TOT MASS PER LAYER
!     here we receive MASS_SAND(ISAND,ILAYER,IPOIN) or
!     MASS_MUD(IMUD,ILAYER,IPOIN) after bedload/suspension/...
!
      IF(NSAND.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO ISAND = 1,NSAND
              MASS_SAND_TOT(ILAYER,IPOIN) = MASS_SAND_TOT(ILAYER,IPOIN)
     &        + MASS_SAND(ISAND,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(NMUD.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO IMUD = 1,NMUD
              MASS_MUD_TOT(ILAYER,IPOIN) = MASS_MUD_TOT(ILAYER,IPOIN)
     &        + MASS_MUD(IMUD,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
      DO IPOIN = 1,NPOIN
        DO ILAYER = 1,NOMBLAY
          MASS_MIX_TOT(ILAYER,IPOIN) = MASS_SAND_TOT(ILAYER,IPOIN)
     &    + MASS_MUD_TOT(ILAYER,IPOIN)
        ENDDO
      ENDDO
!
!     COMPUTES MASS RATIOS PER LAYER (SAND CLASSES)
!
      IF(NSAND.GE.1)THEN
!
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            TOT = 0.D0
            DO ISAND = 1,NSAND
              IF(ISAND.NE.NSAND) THEN
                IF(MASS_SAND_TOT(ILAYER,IPOIN).GE.MIN_SED_MASS_COMP)THEN
                  RATIO_SAND(ISAND,ILAYER,IPOIN) =
     &            MIN(1.D0,MASS_SAND(ISAND,ILAYER,IPOIN)
     &            / MASS_SAND_TOT(ILAYER,IPOIN))
                  TOT = TOT + RATIO_SAND(ISAND,ILAYER,IPOIN)
                ELSE
                  RATIO_SAND(ISAND,ILAYER,IPOIN) = 0.D0
                ENDIF
              ELSE
                RATIO_SAND(NSAND,ILAYER,IPOIN) = 1.D0-TOT
              ENDIF
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!     COMPUTES MASS RATIOS PER LAYER (MUD CLASSES)
!
      IF(NMUD.GE.1)THEN
!
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            TOT = 0.D0
            DO IMUD = 1,NMUD
              IF (IMUD.NE.NMUD)THEN
                IF(MASS_MUD_TOT(ILAYER,IPOIN).GE.MIN_SED_MASS_COMP)THEN
                  RATIO_MUD(IMUD,ILAYER,IPOIN) =
     &            MIN(1.D0,MASS_MUD(IMUD,ILAYER,IPOIN)
     &            / MASS_MUD_TOT(ILAYER,IPOIN))
                  TOT = TOT + RATIO_MUD(IMUD,ILAYER,IPOIN)
                ELSE
                  RATIO_MUD(IMUD,ILAYER,IPOIN) = 0.D0
                ENDIF
              ELSE
                RATIO_MUD(NMUD,ILAYER,IPOIN) = 1.D0-TOT
              ENDIF
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!     COMPUTES SAND-MUD MASS RATIOS PER LAYER
!
      IF(NMUD.EQ.0) THEN
        DO IPOIN=1,NPOIN
          DO ILAYER=1,NOMBLAY
            RATIO_MUD_SAND(ILAYER,IPOIN) = 0.D0
          ENDDO
        ENDDO
      ELSEIF(NSAND.EQ.0) THEN
        DO IPOIN=1,NPOIN
          DO ILAYER=1,NOMBLAY
            RATIO_MUD_SAND(ILAYER,IPOIN) = 1.D0
          ENDDO
        ENDDO
      ELSE
!       NSAND AND NMUD GT 0 (IF NSAND AND NMUD EQ 0: WHAT ARE YOU
!       DOING IS GAIA?)
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            IF(MASS_MIX_TOT(ILAYER,IPOIN).GE.MIN_SED_MASS_COMP)THEN
              RATIO_MUD_SAND(ILAYER,IPOIN)= MIN(1.D0,
     &          MASS_MUD_TOT(ILAYER,IPOIN)
     &        / MASS_MIX_TOT(ILAYER,IPOIN))
            ELSE
!             CHOICE TO DO, BUT NO EROSION OF THIS LAYER IN SUSPENSION_ERODE
              RATIO_MUD_SAND(ILAYER,IPOIN) = 1.D0
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!     COMPUTE THICKNESS [m]
!
      IF (NSAND.EQ.0)THEN
        DO ILAYER = 1,NOMBLAY
          DO IPOIN = 1,NPOIN
            ES(IPOIN,ILAYER) = MASS_MIX_TOT(ILAYER,IPOIN)
     &                         /CONC_MUD(ILAYER,IPOIN)
          ENDDO
        ENDDO
      ELSE
        DO ILAYER = 1,NOMBLAY

          DO IPOIN = 1,NPOIN
            XMVS_LAY=0.D0
            DO ISAND = 1,NSAND ! AVERAGE DENSITY OF SAND FOR THE LAYER
              XMVS_LAY=XMVS_LAY+
     &       RATIO_SAND(ISAND,ILAYER,IPOIN)*XMVS0(NUM_ISAND_ICLA(ISAND))
            ENDDO
!         TTERM REPRESENTS THE DIFFERENCE BETWEEN MUD VOLUME AND VOID VOLUME
            TERM=0.D0
            IF(NMUD.GT.0) THEN
              TERM=RATIO_MUD_SAND(ILAYER,IPOIN)/CONC_MUD(ILAYER,IPOIN)-
     &       (XKV0(ILAYER)*(1.D0-RATIO_MUD_SAND(ILAYER,IPOIN)))/
     &       (XMVS_LAY*(1.D0-XKV0(ILAYER)))
            ENDIF
            DISCR=MAX(0.D0,TERM)
!           IF DISCR IS POSITIVE IT MEANS THAT MUD VOLUME IS LARGER THAN VOID VOLUME
!           IF DISCR IS NEGATIVE, THE VOID VOLUME IS NOT COMPLETELY FILLED BY MUD
            ES(IPOIN,ILAYER)=MASS_MIX_TOT(ILAYER,IPOIN)*
     &      ((1.D0-RATIO_MUD_SAND(ILAYER,IPOIN))/
     &      (XMVS_LAY*(1.D0-XKV0(ILAYER)))+ DISCR)
          ENDDO
        ENDDO
      ENDIF
!
!     COMPUTE ZF (NOTE: THIS COULD BE MOVED)
!
      DO IPOIN = 1,NPOIN
        ZF%R(IPOIN) = ZR%R(IPOIN)
        DO ILAYER = 1,NOMBLAY
          ZF%R(IPOIN) = ZF%R(IPOIN) + ES(IPOIN,ILAYER)
        ENDDO
      ENDDO
!
!     COMPUTES MASSTOT AT EVERY TIME STEP, FOR EVERY CLASS
!     NECESSARY FOR BALANCE
!     MAYBE TO DO ONLY AT THE LAST TIME STEP?
!
      DO ICLA=1,NSICLA
        MASSTOT(ICLA)=0.D0
      ENDDO
!
      DO ICLA=1,NSICLA
        K = NUM_ICLA_IMUD(ICLA)
        J = NUM_ICLA_ISAND(ICLA)
        IF(J.GT.0) THEN
          DO IPOIN=1,NPOIN
            DO I=1,NOMBLAY
              MASSTOT(ICLA)= MASSTOT(ICLA) +
     &                   MASS_SAND(J,I,IPOIN)*VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDIF
        IF(K.GT.0) THEN
          DO IPOIN=1,NPOIN
            DO I=1,NOMBLAY
              MASSTOT(ICLA)= MASSTOT(ICLA) +
     &                   MASS_MUD(K,I,IPOIN)*VOLU2D%R(IPOIN)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          MASSTOT(I)=P_SUM(MASSTOT(I))
        ENDDO
      ENDIF
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      RETURN
      END
