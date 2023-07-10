!                   ****************
                    SUBROUTINE BIL3D
!                   ****************
!
     &(LT,NIT,IKLBORL,NPTFR,NETAG)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    COMPUTES THE RELATIVE BALANCE OF THE MASSES OF
!+                WATER AND TRACERS DURING A TIMESTEP, AS WELL AS
!+                THE ABSOLUTE CUMULATIVE BALANCE.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)
!+        17/06/2008
!+        V5P9
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
!history  J-M HERVOUET (LNHE)
!+        16/03/2011
!+        V6P1
!+   Mass-lumping taken into account in computation of diffusive fluxes,
!+   like in diff3d.f (messages on mass-conservation of sediment were
!+   wrong).
!
!history  J-M HERVOUET (LNHE)
!+        23/04/2012
!+        V6P2
!+   Values of tracers in rain taken into account.
!
!history  C. VILLARET (HR-WALLINGFORD) & J-M HERVOUET (EDF LAB, LNHE)
!+        20/01/2014
!+        V7P0
!+   The fact that the bottom value may not be on the bottom (depending
!+   on IPBOT) if(SIGMAG.OR.OPTBAN.EQ.1) is taken into account.
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLBORL        |-->| CONNECTIVITY TABLE OF LATERAL BOUNDARIES
!| NETAG          |-->| NUMBER OF LAYERS (I.E. NPLAN-1)
!| NPTFR          |-->| NUMBER OF 2D BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_BIL3D => BIL3D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, EX_LT => LT, EX_NIT => NIT
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT,NIT
      INTEGER, INTENT(IN) :: NPTFR,NETAG
      INTEGER, INTENT(IN) :: IKLBORL(NELEBX,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION FLUTOT,SUR12,A1,A2,FLURAIN,D,Z_1,Z_2,REALRAIN
!
      INTEGER I,L1,L2,L3,L4,N1,N2,N3,N4,IVBIL,ILIQ,IELEB
      INTEGER IPTFR,IETAGE,ITRAC
      DOUBLE PRECISION FLUDI(MAXTRA+5),FLUS1(MAXTRA+5),FLUS0(MAXTRA+5)
      DOUBLE PRECISION FLUXTOTAL
      DOUBLE PRECISION, POINTER, DIMENSION(:,:) :: FLUXBIL
      INTRINSIC SQRT
!
!
!
      FLUXBIL(1:MAXFRO+NSCE+1,1:5+NTRAC) => FLUXB%R
!
!=======================================================================
!
!     COMPUTES THE FLUXES AT THE LIQUID BOUNDARIES
!
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUX_BOUNDARIES(I)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO I=1,NPTFR
            ILIQ=NUMLIQ%I(I)
            IF(ILIQ.GT.0) THEN
              FLUX_BOUNDARIES(ILIQ)=FLUX_BOUNDARIES(ILIQ)+FLBOR%R(I)
            ENDIF
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO I=1,NFRLIQ
            FLUX_BOUNDARIES(I)=P_SUM(FLUX_BOUNDARIES(I))
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE ADVECTION FLUXES ON THE VARIOUS OPEN BOUNDARIES
!
!=======================================================================
!
!     TOTAL FLUX, STARTING WITH THE LIQUID BOUNDARIES
!
      FLUXTOTAL=0.D0
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUXTOTAL=FLUXTOTAL+FLUX_BOUNDARIES(I)
        ENDDO
      ENDIF
!
!     RAIN AND EVAPORATION
!
      IF(RAIN) THEN
        FLURAIN=0.D0
        DO I=1,NPOIN2
          FLURAIN=FLURAIN+PLUIE%R(I)
        ENDDO
        IF(NCSIZE.GT.1) FLURAIN = P_SUM(FLURAIN)
        FLUXTOTAL=FLUXTOTAL-FLURAIN
      ENDIF
!
      IF(NSCE.GE.1) THEN
        DO I=1,NSCE
          FLUXTOTAL = FLUXTOTAL - QSCE2(I)
        ENDDO
!       IF(NTRAC.GT.0) THEN
!         DO I=1,NSCE
!         DO IVBIL=2,1+NTRAC
!         NOW DONE IN CVDF3D AND MURD3D
!                                                      TASCE(MAXSCE,NTRAC)
!             FLUX%R(IVBIL) = FLUX%R(IVBIL) - QSCE2(I)*TASCE2(I,IVBIL-1)
!         ENDDO
!         ENDDO
!       ENDIF
      ENDIF
!
! BEDFLUXES
      IF(BEDBOU) THEN
        DO I=1,NBEDFLO
          FLUXTOTAL = FLUXTOTAL - BEDFLO(I)
        ENDDO
      ENDIF
!
!=======================================================================
!
!  COMPUTES THE FLUXES BY DIFFUSION AND SOURCES OF TRACERS
!
!=======================================================================
!
      SUR12 = 1.D0/12.D0
!
!   ====================================
!   FLUX BY SOURCES OF TRACERS
!   ====================================
!
      IF(NTRAC.NE.0) THEN
        DO ITRAC=1,NTRAC
          FLUS1(5+ITRAC) = 0.D0
          IF(S1TA%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO I=1,NPOIN3
!             TODO: JMH NOTE : IS NOT VOLU WITH SUPG
!                        BUT A VALUE WEIGHTED WITH VOLUN
!                        INVESTIGATE WHEN THE CASE ARISES...
              FLUS1(5+ITRAC)=FLUS1(5+ITRAC)
     &                        +S1TA%ADR(ITRAC)%P%R(I)*VOLU%R(I)*
     &                           TA%ADR(ITRAC)%P%R(I)
            ENDDO
            IF(NCSIZE.GT.1) THEN
              FLUS1(5+ITRAC) = P_SUM(FLUS1(5+ITRAC))
            ENDIF
          ENDIF
          FLUS0(5+ITRAC) = 0.D0
          IF(S0TA%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO I=1,NPOIN3
              FLUS0(5+ITRAC)=FLUS0(5+ITRAC)
     &                        +S0TA%ADR(ITRAC)%P%R(I)*VOLU%R(I)
            ENDDO
            IF(NCSIZE.GT.1) THEN
              FLUS0(5+ITRAC) = P_SUM(FLUS0(5+ITRAC))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!   ===============================
!   FLUX BY DIFFUSION OF TRACERS
!   ===============================
!
!   IN THE CASE OF A SEDIMENT, THIS FLUX INCLUDES THE SETTLING VELOCITY
!   SEE HOW ATABOF AND BTABOF ARE DONE IN FLUSED
!
      IF(NTRAC.NE.0) THEN
!
        DO ITRAC=1,NTRAC
!
          FLUDI(5+ITRAC) = 0.D0
!
!         BOTTOM AND FREE SURFACE
!
          IF(ATABOF%ADR(ITRAC)%P%TYPR.NE.'0') THEN
!           WITH MASS-LUMPING LIKE IN DIFF3D
            IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
              DO I=1,NPOIN2
!               1) FLUX THROUGH THE BOTTOM NOT TAKEN INTO ACCOUNT FOR TIDAL FLATS
!               2) TRACER AT ACTUAL BOTTOM PLANE TAKEN INTO ACCOUNT, THEN IPBOT
!                  IN ADDRESS OF TA.
                IF(IPBOT%I(I).NE.NPLAN-1) THEN
                  FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &            + ATABOF%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
     &            *TA%ADR(ITRAC)%P%R(IPBOT%I(I)*NPOIN2+I)
                ENDIF
              ENDDO
            ELSE
              DO I=1,NPOIN2
                FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &          + ATABOF%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
     &                             *TA%ADR(ITRAC)%P%R(I)
              ENDDO
            ENDIF
          ENDIF
!
          IF(ATABOS%ADR(ITRAC)%P%TYPR.NE.'0') THEN
!           WITH MASS-LUMPING LIKE IN DIFF3D
            DO I=1,NPOIN2
              FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &        + ATABOS%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
     &        * TA%ADR(ITRAC)%P%R(I+NETAG*NPOIN2)
            ENDDO
          ENDIF
!
          IF(BTABOF%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO I=1,NPOIN2
              FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &        + VOLU2D%R(I)*BTABOF%ADR(ITRAC)%P%R(I)
            ENDDO
          ENDIF
!
          IF(BTABOS%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO I=1,NPOIN2
              FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &        +VOLU2D%R(I)*BTABOS%ADR(ITRAC)%P%R(I)
            ENDDO
          ENDIF
!
!         VALUE OF TRACER IN RAIN
!
          IF(RAIN) THEN
            REALRAIN=0.D0
            DO I=1,NPOIN2
              REALRAIN=REALRAIN+MAX(PLUIE%R(I),0.D0)
            ENDDO
            FLUDI(5+ITRAC)=FLUDI(5+ITRAC)+REALRAIN*TRAIN(ITRAC)
          ENDIF
!
!         LATERAL BOUNDARIES
!
          DO IETAGE=1,NETAG
!
            IF(ATABOL%ADR(ITRAC)%P%TYPR.NE.'0') THEN
              DO IPTFR=1,NPTFR
!
                IELEB=(IETAGE-1)*NPTFR+IPTFR
                L1 = IKLBORL(IELEB,1)
                L2 = IKLBORL(IELEB,2)
                L3 = IKLBORL(IELEB,3)
                L4 = IKLBORL(IELEB,4)
                N1 = NBOR3%I(L1)
                N2 = NBOR3%I(L2)
                N3 = NBOR3%I(L3)
                N4 = NBOR3%I(L4)
!
                FLUDI(5+ITRAC) = FLUDI(5+ITRAC) + SUR12 *
     &          ( ATABOL%ADR(ITRAC)%P%R(L1)*TA%ADR(ITRAC)%P%R(N1)
     &          + ATABOL%ADR(ITRAC)%P%R(L2)*TA%ADR(ITRAC)%P%R(N2)
     &          + ATABOL%ADR(ITRAC)%P%R(L3)*TA%ADR(ITRAC)%P%R(N3)
     &          + ATABOL%ADR(ITRAC)%P%R(L4)*TA%ADR(ITRAC)%P%R(N4) )
!
              ENDDO
            ENDIF
!
            IF(BTABOL%ADR(ITRAC)%P%TYPR.NE.'0') THEN
              DO IPTFR=1,NPTFR
!
                IELEB=(IETAGE-1)*NPTFR+IPTFR
                L1 = IKLBORL(IELEB,1)
                L2 = IKLBORL(IELEB,2)
                L3 = IKLBORL(IELEB,3)
                L4 = IKLBORL(IELEB,4)
                N1 = NBOR3%I(L1)
                N2 = NBOR3%I(L2)
                N3 = NBOR3%I(L3)
                N4 = NBOR3%I(L4)
                D = SQRT((X(N2)-X(N1))**2 + (Y(N2)-Y(N1))**2)
                Z_1 = ZPROP%R(N4) - ZPROP%R(N1)
                Z_2 = ZPROP%R(N3) - ZPROP%R(N2)
                A1 = D * (Z_1+Z_1+Z_2)
                A2 = D * (Z_2+Z_2+Z_1)
!
                FLUDI(5+ITRAC) = FLUDI(5+ITRAC) + SUR12 *
     &          ( BTABOL%ADR(ITRAC)%P%R(L1)*A1
     &           +BTABOL%ADR(ITRAC)%P%R(L2)*A2
     &           +BTABOL%ADR(ITRAC)%P%R(L3)*A2
     &           +BTABOL%ADR(ITRAC)%P%R(L4)*A1)
!
              ENDDO
            ENDIF
!
          ENDDO
!
          IF(NCSIZE.GT.1) FLUDI(5+ITRAC) = P_SUM(FLUDI(5+ITRAC))
!
        ENDDO
!
      ENDIF
!
!=======================================================================
!
! COMPUTES CUMULATIVE FLUXES AND WRITES OUT
!
!=======================================================================
!
      FLUXTOTCUM = FLUXTOTCUM + FLUXTOTAL
!
      IF(INFOGR) THEN
        WRITE(LU,602) MASSEN_WATER,MASSE_WATER,DT*FLUXTOTAL,
     &                MASSEN_WATER-MASSE_WATER-DT*FLUXTOTAL
        IF(RAIN) THEN
          WRITE(LU,604) DT*FLURAIN
        ENDIF
        IF(NFRLIQ.GT.0) THEN
          WRITE(LU,4018)
          DO I=1,NFRLIQ
            WRITE(LU,4020) I,-FLUX_BOUNDARIES(I)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO IVBIL=6,5+NTRAC
!
          FLUTOT = FLUX%R(IVBIL) - FLUDI(IVBIL) + FLUS1(IVBIL)
     &             - FLUS0(IVBIL)
          FLUCUM%R(IVBIL) = FLUCUM%R(IVBIL) + FLUTOT
!
          IF(INFOGR) THEN
            IF(S3D_SEDI.AND.(IVBIL.EQ.NTRAC+5)) THEN
              WRITE(LU,612) FLUX%R(IVBIL),
     &        -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     &                      MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
            ELSE
              WRITE(LU,622) IVBIL-5,NAMETRAC(IVBIL-5)(1:16),
     &           NAMETRAC(IVBIL-5)(17:32), FLUX%R(IVBIL),
     &          -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     &                        MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
            ENDIF
          ENDIF
!
        ENDDO
        IF(INFOGR) THEN
          IF(NFRLIQ.GT.0) THEN
            DO ITRAC=1,NTRAC
              WRITE(LU,4019) ITRAC, NAMETRAC(ITRAC)(17:32)
              DO I=1,NFRLIQ
                WRITE(LU,4020) I,-FLUXBIL(I,5+ITRAC)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
602   FORMAT(/,'  WATER',
     &       /,'VOLUME AT THE PREVIOUS TIME STEP              : ',G16.7,
     &       /,'VOLUME AT THE PRESENT TIME STEP               : ',G16.7,
     &       /,'VOLUME LEAVING DOMAIN DURING THIS TIME STEP   : ',G16.7,
     &       /,'ERROR ON THE VOLUME DURING THIS TIME STEP     : ',G16.7)
!
604   FORMAT(  'BALANCE RAIN-EVAPORATION                      : ',G16.7)
!
!
612   FORMAT(/,'  SEDIMENT IN SUSPENSION ',
     &       /,'ADVECTIVE FLUX THROUGH THE BOUNDARIES         : ',G16.7,
     &       /,'DIFFUSIVE FLUX + DEPOSITION                   : ',G16.7,
     &       /,'MASS AT THE PREVIOUS TIME STEP                : ',G16.7,
     &       /,'MASS AT THE PRESENT TIME STEP                 : ',G16.7,
     &       /,'MASS LEAVING THE DOMAIN DURING THIS TIME STEP : ',G16.7,
     &       /,'ERROR ON THE MASS DURING THIS TIME STEP       : ',G16.7)
!
622   FORMAT(/,'  TRACER ',I2,': ',A16,', UNIT : ',A16,'* M3)',
     &       /,' '
     &       /,'ADVECTIVE FLUX THROUGH BOUNDARIES OR SOURCES  : ',G16.7,
     &       /,'DIFFUSIVE FLUX THROUGH THE BOUNDARIES         : ',G16.7,
     &       /,'QUANTITY AT THE PREVIOUS TIME STEP            : ',G16.7,
     &       /,'QUANTITY AT THE PRESENT TIME STEP             : ',G16.7,
     &       /,'QUANTITY EXITING (BOUNDARIES OR SOURCE)       : ',G16.7,
     &       /,'ERROR ON THE QUANTITY DURING THIS TIME STEP   : ',G16.7)
4018  FORMAT('  BOUNDARY FLUXES FOR WATER IN M3/S ( >0 : ENTERING )')
4019  FORMAT('  BOUNDARY FLUXES FOR TRACER ',I2,' IN ',A16,'* M3/S)',
     &       ' ( >0 : ENTERING )')
4020  FORMAT('FLUX BOUNDARY ',I6,'                          : ',G16.7)
!
!=======================================================================
!
! PRINTOUTS SPECIFIC TO THE LAST TIMESTEP
!
!=======================================================================
!
      IF(LT.EQ.NIT) THEN
!
        WRITE(LU,*)
        CALL MITTIT(11,AT,LT)
        WRITE (LU,'(A4,F16.4)') 'T = ',AT
!
        WRITE(LU,702) MASINI_WATER,MASSE_WATER,
     &     DT*FLUXTOTCUM, MASINI_WATER-MASSE_WATER-DT*FLUXTOTCUM
!
!-----------------------------------------------------------------------
!
        IF (NTRAC.GT.0) THEN
!
          DO IVBIL=6,5+NTRAC
!
            IF(S3D_SEDI.AND.(IVBIL.EQ.NTRAC+5)) THEN
              WRITE(LU,712)
     &              MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     &              MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
            ELSE
              WRITE(LU,722) IVBIL-5,
     &              NAMETRAC(IVBIL-5)(1:16),NAMETRAC(IVBIL-5)(17:32),
     &              MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     &              MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
            ENDIF
!
          ENDDO
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
702   FORMAT(//,'--- WATER ---',
     &       /,'INITIAL VOLUME                      : ',G16.7,
     &       /,'FINAL VOLUME                        : ',G16.7,
     &       /,'VOLUME EXITING (BOUNDARY OR SOURCE) : ',G16.7,
     &       /,'TOTAL VOLUME LOST                   : ',G16.7)
!
712   FORMAT(//,'--- SEDIMENT ---',
     &       /,'INITIAL MASS                        : ',G16.7,
     &       /,'FINAL MASS                          : ',G16.7,
     &       /,'MASS LEAVING THE DOMAIN             : ',G16.7,
     &       /,'TOTAL MASS LOST                     : ',G16.7)
!
722   FORMAT(//,'--- TRACER',I2,': ',A16,', UNIT : ',A16,'* M3)',
     &       /,'INITIAL QUANTITY OF TRACER          : ',G16.7,
     &       /,'FINAL QUANTITY OF TRACER            : ',G16.7,
     &       /,'QUANTITY EXITING (BOUNDARY/SOURCE)  : ',G16.7,
     &       /,'TOTAL QUANTITY OF TRACER LOST       : ',G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
