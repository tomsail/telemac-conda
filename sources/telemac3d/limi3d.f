!                   *****************
                    SUBROUTINE LIMI3D
!                   *****************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    INITIALISES TYPES OF 3D BOUNDARY CONDITIONS.
!+
!+            SETS THE VALUE OF SOME COEFFICIENTS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)
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
!history  C.-T. PHAM (EDF, LNHE)
!+        01/03/2017
!+        V7P2
!+   Allowing k-epsilon model on a direction and not on the other.
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Call of USER_LIMI3D User Fortran where the modifications are done
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN2, IPLAN, IPTFR, IPTFR3, ITRAC
!
!***********************************************************************
!
!     BOUNDARY CONDITIONS ON VELOCITIES
!     *********************************
!
!     BOTTOM
!     ======
!
!     DEFAULT: IMPERMEABILITY AND LOG LAW (SEE ALSO BORD3D)
!
      IF(BC_BOTTOM.EQ.1) THEN
!
        DO IPOIN2 = 1,NPOIN2
          LIUBOF%I(IPOIN2) = KLOG
          LIVBOF%I(IPOIN2) = KLOG
          LIWBOF%I(IPOIN2) = KLOG
!         USEFUL ? SHOULD NOT BE USED ANYWAY
          UBORF%R(IPOIN2)  = 0.D0
          VBORF%R(IPOIN2)  = 0.D0
          WBORF%R(IPOIN2)  = 0.D0
        ENDDO
!
      ELSEIF(BC_BOTTOM.EQ.2) THEN
!
        DO IPOIN2 = 1,NPOIN2
          LIUBOF%I(IPOIN2) = KADH
          LIVBOF%I(IPOIN2) = KADH
          LIWBOF%I(IPOIN2) = KADH
!         USEFUL ? KADH SAYS IT IS 0.D0
          UBORF%R(IPOIN2)  = 0.D0
          VBORF%R(IPOIN2)  = 0.D0
          WBORF%R(IPOIN2)  = 0.D0
        ENDDO
!
      ELSE
        WRITE(LU,*) 'LIMI3D : BAD BOUNDARY CONDITION ON THE BOTTOM'
        WRITE(LU,*) '         VALUE ',BC_BOTTOM,' UNKNOWN'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COEFFICIENTS SET TO 0 BY MEANS OF THEIR COMPONENT TYPR
!     NO DIFFUSION FLUX THROUGH BOTTOM
!
      AUBORF%TYPR='0'
      AVBORF%TYPR='0'
      BUBORF%TYPR='0'
      BVBORF%TYPR='0'
      IF(NONHYD) THEN
        AWBORF%TYPR='0'
        BWBORF%TYPR='0'
      ENDIF
!
!     LATERAL BOUNDARIES
!     ==================
!
!     DEFAULT: 2D CONDITIONS DUPLICATED ON THE VERTICAL
!              FREE FOR W
!              NO FRICTION
!
      DO IPLAN = 2,NPLAN
        DO IPTFR = 1,NPTFR2
          IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
          LIUBOL%I(IPTFR3) = LIUBOL%I(IPTFR)
          LIVBOL%I(IPTFR3) = LIVBOL%I(IPTFR)
          UBORL%R(IPTFR3)  = UBORL%R(IPTFR)
          VBORL%R(IPTFR3)  = VBORL%R(IPTFR)
          AUBORL%R(IPTFR3) = AUBORL%R(IPTFR)
        ENDDO
      ENDDO
!
!     IDEA OF OPTIMISATION (BEWARE PARALLELISM)
!
!     IF(DOTS(AUBORL,AUBORL).GT.1.D-8) THEN
!       AUBORL%TYPR='Q'
!     ELSE
!       AUBORL%TYPR='0'
!     ENDIF
!
      DO IPTFR3 = 1,NPTFR3
!                           KSORT: W FREE ON LATERAL BOUNDARIES
        LIWBOL%I(IPTFR3)        = KSORT
!       VALUES SAVED IN SECOND DIMENSION BECAUSE ADVECTION
!       SCHEMES MAY CHANGE THE VALUES
        LIUBOL%I(IPTFR3+NPTFR3) = LIUBOL%I(IPTFR3)
        LIVBOL%I(IPTFR3+NPTFR3) = LIVBOL%I(IPTFR3)
        LIWBOL%I(IPTFR3+NPTFR3) = LIWBOL%I(IPTFR3)
        WBORL%R(IPTFR3)  = 0.D0
!       BUBORL%R(IPTFR3) = 0.D0
!       BVBORL%R(IPTFR3) = 0.D0
      ENDDO
      BUBORL%TYPR='0'
      BVBORL%TYPR='0'
!
      IF(NONHYD) THEN
!       DO IPTFR3 = 1,NPTFR3
!         AWBORL%R(IPTFR3) = 0.D0
!         BWBORL%R(IPTFR3) = 0.D0
!       ENDDO
        AWBORL%TYPR='0'
        BWBORL%TYPR='0'
      ENDIF
!
!     FREE SURFACE
!     ============
!
!     DEFAULT: IMPERMEABILITY AND NO FRICTION (SEE ALSO BORD3D)
!
      DO IPOIN2 = 1,NPOIN2
        LIUBOS%I(IPOIN2) = KLOG
        LIVBOS%I(IPOIN2) = KLOG
        LIWBOS%I(IPOIN2) = KLOG
        UBORS%R(IPOIN2)  = 0.D0
        VBORS%R(IPOIN2)  = 0.D0
        WBORS%R(IPOIN2)  = 0.D0
!       AUBORS%R(IPOIN2) = 0.D0
!       BUBORS%R(IPOIN2) = 0.D0
!       BVBORS%R(IPOIN2) = 0.D0
      ENDDO
      AUBORS%TYPR='0'
      BUBORS%TYPR='0'
      AVBORS%TYPR='0'
      BVBORS%TYPR='0'
!
      IF(NONHYD) THEN
!       DO IPOIN2 = 1,NPOIN2
!         AWBORS%R(IPOIN2) = 0.D0
!         BWBORS%R(IPOIN2) = 0.D0
!       ENDDO
        AWBORS%TYPR='0'
        BWBORS%TYPR='0'
      ENDIF
!
!     **************
!     TRACERS BC'S
!     **************
!
      IF (NTRAC.NE.0) THEN
        DO ITRAC = 1,NTRAC
!
!     BOTTOM
!     ======
!
!     DEFAULT: NEUMANN BC'S
!
          DO IPOIN2 = 1,NPOIN2
            LITABF%ADR(ITRAC)%P%I(IPOIN2) = KLOG
            TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           ATABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           BTABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
          ENDDO
          ATABOF%ADR(ITRAC)%P%TYPR='0'
          BTABOF%ADR(ITRAC)%P%TYPR='0'
!
!     SIDES
!     =====
!
!     DEFAULT: NEUMANN BC'S
!
!           WHAT HAS BEEN READ IN THE BOUNDARY CONDITIONS FILE
!           FOR 1 TRACER IS DUPLICATED ON THE VERTICAL AND FOR
!           ALL TRACERS
!
          DO IPLAN = 1,NPLAN
            DO IPTFR = 1,NPTFR2
              IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
              LITABL%ADR(ITRAC)%P%I(IPTFR3) = LITABL%ADR(1)%P%I(IPTFR)
!             SAVING ON SECOND DIMENSION BECAUSE ADVECTION SCHEMES
!             MAY CHANGE THIS VALUE
              LITABL%ADR(ITRAC)%P%I(IPTFR3+NPTFR3) =
     &                                        LITABL%ADR(1)%P%I(IPTFR)
              TABORL%ADR(ITRAC)%P%R(IPTFR3) = TABORL%ADR(1)%P%R(IPTFR)
!             ATABOL%ADR(ITRAC)%P%R(IPTFR3) = ATABOL%ADR(1)%P%R(IPTFR)
!             BTABOL%ADR(ITRAC)%P%R(IPTFR3) = BTABOL%ADR(1)%P%R(IPTFR)
            ENDDO
          ENDDO
          ATABOL%ADR(ITRAC)%P%TYPR='0'
          BTABOL%ADR(ITRAC)%P%TYPR='0'
!
!     FREE SURFACE
!     =============
!
!     DEFAULT: NEUMANN BC'S
!
          DO IPOIN2 = 1,NPOIN2
            LITABS%ADR(ITRAC)%P%I(IPOIN2) = KLOG
            TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           ATABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           BTABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
          ENDDO
          ATABOS%ADR(ITRAC)%P%TYPR='0'
          BTABOS%ADR(ITRAC)%P%TYPR='0'
!
        ENDDO
      ENDIF
!
!     SOLID BOUNDARIES FOR K AND EPSILON
!     **********************************
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7.OR.ITURBH.EQ.3.OR.ITURBH.EQ.7) THEN
!
!     BOTTOM
!     ======
!
!     DEFAULT : NO GRADIENT
!
        DO IPOIN2 = 1,NPOIN2
          AKBORF%R(IPOIN2) = 0.D0
          BKBORF%R(IPOIN2) = 0.D0
          AEBORF%R(IPOIN2) = 0.D0
          BEBORF%R(IPOIN2) = 0.D0
        ENDDO
        AKBORF%TYPR = '0'
        BKBORF%TYPR = '0'
        AEBORF%TYPR = '0'
        BEBORF%TYPR = '0'
!
!     SIDES
!     =====
!
!     DEFAULT : NO GRADIENT
!
        DO IPTFR3 = 1,NPTFR3
          AKBORL%R(IPTFR3) = 0.D0
          BKBORL%R(IPTFR3) = 0.D0
          AEBORL%R(IPTFR3) = 0.D0
          BEBORL%R(IPTFR3) = 0.D0
        ENDDO
!
!     FREE SURFACE
!     ============
!
!     DEFAULT : NO GRADIENT
!
        DO IPOIN2 = 1,NPOIN2
          AKBORS%R(IPOIN2) = 0.D0
          BKBORS%R(IPOIN2) = 0.D0
          AEBORS%R(IPOIN2) = 0.D0
          BEBORS%R(IPOIN2) = 0.D0
        ENDDO
!
      ELSEIF(ITURBV.EQ.5.OR.ITURBV.EQ.9) THEN

!     BOTTOM
!     ======

        DO IPOIN2=1,NPOIN2
          ANUBORF%R(IPOIN2) = 0.D0
          BNUBORF%R(IPOIN2) = 0.D0
        ENDDO
        ANUBORF%TYPR = '0'
        BNUBORF%TYPR = '0'

        DO IPTFR3 = 1,NPTFR3
          ANUBORL%R(IPTFR3) = 0.D0
          BNUBORL%R(IPTFR3) = 0.D0
        ENDDO
        DO IPOIN2 = 1,NPOIN2
          ANUBORS%R(IPOIN2) = 0.D0
          BNUBORS%R(IPOIN2) = 0.D0
        ENDDO
      ENDIF
!
!
!     FRICTION COEFFICIENTS
!     *********************
!
!     DEFAULT: VALUE GIVEN IN STEERING FILE
!
      CALL OV('X=C     ', X=RUGOL%R, C=RUGOL0, DIM1=NPTFR2*NPLAN)
!
!======================================================================
! DEFAULT BOUNDARY CONDITION TYPES AND VALUES FOR THE
! PRESSURE POISSON EQUATION
!======================================================================
!
      IF(NONHYD) THEN
!
!-----------------------------------------------------------------------
!
! DEFAULT TYPES AND VALUES FOR THE PRESSURE BOUNDARY CONDITIONS
! BOTTOM AND FREE SURFACE
!
!       AT ALL LATERAL BOUNDARIES AND BOTTOM DP/DN = 0;
!       DIRICHLET = 0 AT THE SURFACE; DIRICHLET CONDITIONS SET TO 0 ALL OVER
!       (CORRECT AT THE SURFACE ONLY)
!
!       BOTTOM AND SURFACE
!       CHECK KLOG BOTTOM
!
        DO IPOIN2=1,NPOIN2
          LIPBOF%I(IPOIN2) = KLOG
          LIPBOS%I(IPOIN2) = KENT
          PBORF%R(IPOIN2)  = 0.D0
          PBORS%R(IPOIN2)  = 0.D0
        ENDDO
!
!       LATERAL SURFACES: ALL TREATED AS NEUMANN
!
        DO IPTFR3=1,NPTFR3
          LIPBOL%I(IPTFR3) = KLOG
          PBORL%R(IPTFR3)  = 0.D0
        ENDDO
!
!       LATERAL SURFACES: DIRICHLET ON ENTRANCES, NEUMANN ELSEWHERE
!
!       DO IPTFR3=1,NPTFR3
!         PBORL%R(IPTFR3)  = 0.D0
!         IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
!    *       LIUBOL%I(IPTFR3).EQ.KENTU) THEN
!           LIPBOL%I(IPTFR3) = KENT
!         ELSE
!           LIPBOL%I(IPTFR3) = KLOG
!         ENDIF
!       ENDDO
!
      ENDIF

      CALL USER_LIMI3D
!
!======================================================================
!
      RETURN
      END
