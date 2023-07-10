!                   *******************
                    SUBROUTINE FLUX_COR
!                   *******************
!
     &(FC,FINSUB,FI_I,ZSTART,ZEND,DFDT,XB,DIM1XB,TETA,IKLE3,
     & MESH3,NELEM3,NELMAX,NPOIN3,IELM3,SCHCF)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    Computation of fluxes with PSI limitation for the corrector step.
!+        The result is given in terms of contribution per point, not fluxes
!+        between points, and takes a derivative in time into account.
!
!history  S. PAVAN & J-M HERVOUET (EDF LAB, LNHE)
!+        05/09/2016
!+        V7P2
!+    First version.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        21/10/2016
!+        V7P2
!+    Optimisation and simplification as done in 2D with flux_ef_vf_2:
!+    N fluxes used instead of PSI fluxes when computing the flux
!+    contribution to be added to the derivative in time.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/09/2017
!+        V7P3
!+    Adding argument NELMAX for dimensioning IKLE3.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DFDT           |-->| DERIVATIVE IN TIME (F*-FN/DT) FOR THE ACTUAL TIME-STEP
!| DIM1XB         |-->| FIRST DIMENSION OF XB
!| FC             |-->| F AT TIME *, RESULT OF THE PREDICTOR
!| FINSUB         |-->| F AT THE BEGINNING OF THE SUBITERATION
!| FI_I           |<--| ASSEMBLED VALUES OF PHI_I OF THE CORRECTOR STEP
!| IELM3          |-->| TYPE OF ELEMENT (41:PRISM, ETC.)
!| IKLE3          |-->| GLOBAL 3D CONNECTIVITY
!| MESH3          |<->| 3D MESH
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| SCHCF          |-->| ADVECTION SCHEME FOR F
!| TETA           |-->| IMPLICITATION COEFFICIENT
!| XB             |<->| OFF-DIAGONAL TERMS
!| ZSTART         |-->| Z AT THE BEGINNING OF THE SUBITERATION
!| ZEND           |-->| Z AT THE END OF THE SUBITERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: SCHCF,NELEM3,NPOIN3,NELMAX
      INTEGER, INTENT(IN)             :: IELM3,DIM1XB
!                                                     6 OR 4
      INTEGER, INTENT(IN)             :: IKLE3(NELMAX,*)
!
      DOUBLE PRECISION, INTENT(IN)    :: FC(NPOIN3),FINSUB(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: FI_I(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: ZSTART(NPOIN3),ZEND(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: TETA
      DOUBLE PRECISION, INTENT(IN)    :: DFDT(NPOIN3)
!
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3
      DOUBLE PRECISION, INTENT(INOUT) :: XB(DIM1XB,NELEM3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION H1N,H2N,H3N,H1,H2,H3,COEF
      DOUBLE PRECISION W1,W2,W3
      DOUBLE PRECISION FINCOR1,FINCOR2,FINCOR3,FINCOR4,FINCOR5
      DOUBLE PRECISION FINCOR6,BETA1,BETA2,BETA3,BETA4,BETA5,BETA6
      DOUBLE PRECISION PHITCOR,SUMAX,BETA1PSI,BETA2PSI,BETA3PSI
      DOUBLE PRECISION BETA4PSI,BETA5PSI,BETA6PSI
!
      INTEGER IELEM,IPOIN,I1,I2,I3,I4,I5,I6
!
      DOUBLE PRECISION, PARAMETER :: EPSPHI = 1.D-30
!
!-----------------------------------------------------------------------
!
      DO IPOIN=1,NPOIN3
        FI_I(IPOIN)=0.D0
      ENDDO
!
!     COMPUTE THE NEW RESIDUAL AND NEW DISTRIBUTION
!
      IF(IELM3.EQ.41) THEN
!
        IF(SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI) THEN
!
          DO IELEM = 1 , NELEM3
!
            I1 = IKLE3(IELEM,1)
            I2 = IKLE3(IELEM,2)
            I3 = IKLE3(IELEM,3)
            I4 = IKLE3(IELEM,4)
            I5 = IKLE3(IELEM,5)
            I6 = IKLE3(IELEM,6)
!
!           COMPUTE \INT_P PSI *(1-TETA)*\DELTA Z{N+1}-TETA*\DELTA Z{N})
!           (N (6) AND N+1 (5) WITH RESPECT TO THE SUB-ITER)
!
            H1N = ZSTART(I4) - ZSTART(I1)
            H2N = ZSTART(I5) - ZSTART(I2)
            H3N = ZSTART(I6) - ZSTART(I3)
!
            H1 = ZEND(I4) - ZEND(I1)
            H2 = ZEND(I5) - ZEND(I2)
            H3 = ZEND(I6) - ZEND(I3)
!
            COEF = MESH3%SURFAC%R(IELEM)/6.D0
!
            W1 = COEF * ((1.D0-TETA)*H1+TETA*H1N)
            W2 = COEF * ((1.D0-TETA)*H2+TETA*H2N)
            W3 = COEF * ((1.D0-TETA)*H3+TETA*H3N)
!
!           COMPUTE CONTRIBUTION OF N SCHEME TO NODES (NO UPWIND OF TIME DERIVATIVE)
!           OVER AN ELEMENT
!
!           PHI_I^N(CN,C*)=SURFAC/6 \DELTA Z{N+1-TETA}+(1-TETA) SUM_J LAMBDA_{IJ}(C_I^N-C_J^N)
!           + TETA SUM_J LAMBDA_{IJ}(C_I^*-C_J^*)
!
            FINCOR1 =W1*DFDT(I1)+TETA *(XB(01,IELEM) * (FC(I1)-FC(I2))
     &                                + XB(02,IELEM) * (FC(I1)-FC(I3))
     &                                + XB(03,IELEM) * (FC(I1)-FC(I4))
     &                                + XB(04,IELEM) * (FC(I1)-FC(I5))
     &                                + XB(05,IELEM) * (FC(I1)-FC(I6)))
     &         + (1.D0-TETA) * (XB(01,IELEM) * (FINSUB(I1)-FINSUB(I2))
     &                        + XB(02,IELEM) * (FINSUB(I1)-FINSUB(I3))
     &                        + XB(03,IELEM) * (FINSUB(I1)-FINSUB(I4))
     &                        + XB(04,IELEM) * (FINSUB(I1)-FINSUB(I5))
     &                        + XB(05,IELEM) * (FINSUB(I1)-FINSUB(I6)))
!
            FINCOR2 =W2*DFDT(I2)+TETA *(XB(16,IELEM) * (FC(I2)-FC(I1))
     &                                + XB(06,IELEM) * (FC(I2)-FC(I3))
     &                                + XB(07,IELEM) * (FC(I2)-FC(I4))
     &                                + XB(08,IELEM) * (FC(I2)-FC(I5))
     &                                + XB(09,IELEM) * (FC(I2)-FC(I6)))
     &         + (1.D0-TETA) * (XB(16,IELEM) * (FINSUB(I2)-FINSUB(I1))
     &                        + XB(06,IELEM) * (FINSUB(I2)-FINSUB(I3))
     &                        + XB(07,IELEM) * (FINSUB(I2)-FINSUB(I4))
     &                        + XB(08,IELEM) * (FINSUB(I2)-FINSUB(I5))
     &                        + XB(09,IELEM) * (FINSUB(I2)-FINSUB(I6)))
!
            FINCOR3 =W3*DFDT(I3)+TETA *(XB(17,IELEM) * (FC(I3)-FC(I1))
     &                                + XB(21,IELEM) * (FC(I3)-FC(I2))
     &                                + XB(10,IELEM) * (FC(I3)-FC(I4))
     &                                + XB(11,IELEM) * (FC(I3)-FC(I5))
     &                                + XB(12,IELEM) * (FC(I3)-FC(I6)))
     &         + (1.D0-TETA) * (XB(17,IELEM) * (FINSUB(I3)-FINSUB(I1))
     &                        + XB(21,IELEM) * (FINSUB(I3)-FINSUB(I2))
     &                        + XB(10,IELEM) * (FINSUB(I3)-FINSUB(I4))
     &                        + XB(11,IELEM) * (FINSUB(I3)-FINSUB(I5))
     &                        + XB(12,IELEM) * (FINSUB(I3)-FINSUB(I6)))
!
            FINCOR4 = W1*DFDT(I4)+TETA* (XB(18,IELEM) * (FC(I4)-FC(I1))
     &                                 + XB(22,IELEM) * (FC(I4)-FC(I2))
     &                                 + XB(25,IELEM) * (FC(I4)-FC(I3))
     &                                 + XB(13,IELEM) * (FC(I4)-FC(I5))
     &                                 + XB(14,IELEM) * (FC(I4)-FC(I6)))
     &         + (1.D0-TETA) * (XB(18,IELEM) * (FINSUB(I4)-FINSUB(I1))
     &                        + XB(22,IELEM) * (FINSUB(I4)-FINSUB(I2))
     &                        + XB(25,IELEM) * (FINSUB(I4)-FINSUB(I3))
     &                        + XB(13,IELEM) * (FINSUB(I4)-FINSUB(I5))
     &                        + XB(14,IELEM) * (FINSUB(I4)-FINSUB(I6)))
!
            FINCOR5 = W2*DFDT(I5)+TETA* (XB(19,IELEM) * (FC(I5)-FC(I1))
     &                                 + XB(23,IELEM) * (FC(I5)-FC(I2))
     &                                 + XB(26,IELEM) * (FC(I5)-FC(I3))
     &                                 + XB(28,IELEM) * (FC(I5)-FC(I4))
     &                                 + XB(15,IELEM) * (FC(I5)-FC(I6)))
     &         + (1.D0-TETA) * (XB(19,IELEM) * (FINSUB(I5)-FINSUB(I1))
     &                        + XB(23,IELEM) * (FINSUB(I5)-FINSUB(I2))
     &                        + XB(26,IELEM) * (FINSUB(I5)-FINSUB(I3))
     &                        + XB(28,IELEM) * (FINSUB(I5)-FINSUB(I4))
     &                        + XB(15,IELEM) * (FINSUB(I5)-FINSUB(I6)))
!
            FINCOR6 = W3*DFDT(I6)+TETA* (XB(20,IELEM) * (FC(I6)-FC(I1))
     &                                 + XB(24,IELEM) * (FC(I6)-FC(I2))
     &                                 + XB(27,IELEM) * (FC(I6)-FC(I3))
     &                                 + XB(29,IELEM) * (FC(I6)-FC(I4))
     &                                 + XB(30,IELEM) * (FC(I6)-FC(I5)))
     &         + (1.D0-TETA) * (XB(20,IELEM) * (FINSUB(I6)-FINSUB(I1))
     &                        + XB(24,IELEM) * (FINSUB(I6)-FINSUB(I2))
     &                        + XB(27,IELEM) * (FINSUB(I6)-FINSUB(I3))
     &                        + XB(29,IELEM) * (FINSUB(I6)-FINSUB(I4))
     &                        + XB(30,IELEM) * (FINSUB(I6)-FINSUB(I5)))
!
!           PHITCOR IS THE NEW TOTAL RESIDUAL: IT INCLUDES TIME DERIVATIVE
!
            PHITCOR=FINCOR1+FINCOR2+FINCOR3+FINCOR4+FINCOR5+FINCOR6
!
!           PSI SCHEME WITH UPWIND OF THE DERIVATIVE IN TIME
!
            IF(ABS(PHITCOR).GT.EPSPHI) THEN
              BETA1=FINCOR1/PHITCOR
              BETA2=FINCOR2/PHITCOR
              BETA3=FINCOR3/PHITCOR
              BETA4=FINCOR4/PHITCOR
              BETA5=FINCOR5/PHITCOR
              BETA6=FINCOR6/PHITCOR
              SUMAX=MAX(0.D0,BETA1)+MAX(0.D0,BETA2)+MAX(0.D0,BETA3)
     &             +MAX(0.D0,BETA4)+MAX(0.D0,BETA5)+MAX(0.D0,BETA6)
!
              IF(SUMAX.GT.1.D-20) THEN
                BETA1PSI=MAX(0.D0,BETA1)/SUMAX
                BETA2PSI=MAX(0.D0,BETA2)/SUMAX
                BETA3PSI=MAX(0.D0,BETA3)/SUMAX
                BETA4PSI=MAX(0.D0,BETA4)/SUMAX
                BETA5PSI=MAX(0.D0,BETA5)/SUMAX
                BETA6PSI=MAX(0.D0,BETA6)/SUMAX
                FI_I(I1)=FI_I(I1)+BETA1PSI*PHITCOR
                FI_I(I2)=FI_I(I2)+BETA2PSI*PHITCOR
                FI_I(I3)=FI_I(I3)+BETA3PSI*PHITCOR
                FI_I(I4)=FI_I(I4)+BETA4PSI*PHITCOR
                FI_I(I5)=FI_I(I5)+BETA5PSI*PHITCOR
                FI_I(I6)=FI_I(I6)+BETA6PSI*PHITCOR
              ENDIF
            ENDIF
          ENDDO !IELEM
!
        ENDIF
!
      ELSE
        WRITE(LU,*)'OTHER ELEMENTS THAN PRISMS NOT IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF !IELEM.EQ.41
!
!-----------------------------------------------------------------------
!
      RETURN
      END
