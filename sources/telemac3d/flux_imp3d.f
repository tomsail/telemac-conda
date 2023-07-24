!                   *********************
                    SUBROUTINE FLUX_IMP3D
!                   *********************
!
     &(NELEM3,NELMAX,ELTSEG,ORISEG,FXMATPAR,NSEG3,IKLE,
     & NPOIN3,FN,FI_I,SURFAC,DFDT,TETA,ZN,ZP,SUR2VOL)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief Equivalent of FLUX_EF_VF_3 but in 3D.
!
!history S. PAVAN (LHSV)
!+     23/06/2016
!+     V7P2
!+     First version
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+     25/10/2016
!+     V7P2
!+   Simplification, height of prism no longer taken into account when
!+   computing SUR2VOL. Corresponding modification in MURD3D_LIPS.
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+     10/09/2017
!+     V7P3
!+   Adding argument NELMAX for dimensioning arrays.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DFDT           |-->| DF/DT
!| ELTSEG         |-->| GIVES THE SEGMENTS IN A TRIANGLE
!| FI_I           |<--| CONTRIBUTIONS TO POINTS
!| FLULIM         |-->| LIMITATION OF FLUXES
!| FN             |-->| TRACER AT TIME T(N)
!| FXMARPAR       |-->| FLUXES ASSEMBLED IN //
!| IKLE           |-->| GLOBAL 3D CONNECTIVITY
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| NSEG3          |-->| NUMBER OF SEGMENTS IN 3D
!| NSEG2          |-->| NUMBER OF SEGMENTS IN 2D
!| ORISEG         |-->| GIVES THE ORIENTATION OF SEGMENTS IN A TRIANGLE
!| OPT_HNEG       |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!| SURFAC         |-->| SURFACE OF TRIANGLES
!| TETA           |-->| LOCAL IMPLICITATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM3,NPOIN3,NSEG3
      INTEGER, INTENT(IN)             :: NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,6)
      INTEGER, INTENT(IN)             :: ELTSEG(NELMAX,15)
      INTEGER, INTENT(IN)             :: ORISEG(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: TETA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FXMATPAR(NSEG3)
      DOUBLE PRECISION, INTENT(INOUT) :: FI_I(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM3),DFDT(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FN(NPOIN3),ZN(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: ZP(NPOIN3),SUR2VOL(NSEG3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELEM,I1,I2,I3,I4,I5,I6
      INTEGER ISEG1,ISEG2,ISEG3,ISEG4,ISEG5,ISEG6,ISEG7,ISEG8
      INTEGER ISEG9,ISEG10,ISEG11,ISEG12,ISEG13,ISEG14,ISEG15
!
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6,BETA1,BETA2,BETA3
      DOUBLE PRECISION BETA4,BETA5,BETA6,PHITCOR,COEF
      DOUBLE PRECISION FINCORR1,FINCORR2,FINCORR3,FINCORR4
      DOUBLE PRECISION FINCORR5,FINCORR6,H1,H2,H3,H4,H5,H6
      DOUBLE PRECISION L12,L13,L14,L15,L16,L21,L23,L24,L25,L26
      DOUBLE PRECISION L31,L32,L34,L35,L36,L41,L42,L43,L45,L46
      DOUBLE PRECISION L51,L52,L53,L54,L56,L61,L62,L63,L64,L65
      DOUBLE PRECISION MT1,MT2,MT3,MT4,MT5,MT6
      DOUBLE PRECISION MIN12,MIN13,MIN14,MIN15,MIN16,MIN23,MIN24,MIN25
      DOUBLE PRECISION MIN26,MIN34,MIN35,MIN36,MIN45,MIN46,MIN56
!
      INTRINSIC MIN,MAX
!
      DOUBLE PRECISION, PARAMETER :: EPSPHI=1.D-12
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN3
        FI_I(I)=0.D0
      ENDDO
!
      DO IELEM = 1,NELEM3
!
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        I4 = IKLE(IELEM,4)
        I5 = IKLE(IELEM,5)
        I6 = IKLE(IELEM,6)
!
        H1 =TETA(I1) *(ZN(I4)-ZN(I1))+(1.D0-TETA(I1))*(ZP(I4)-ZP(I1))
        H2 =TETA(I2) *(ZN(I5)-ZN(I2))+(1.D0-TETA(I2))*(ZP(I5)-ZP(I2))
        H3 =TETA(I3) *(ZN(I6)-ZN(I3))+(1.D0-TETA(I3))*(ZP(I6)-ZP(I3))
        H4 =TETA(I4) *(ZN(I4)-ZN(I1))+(1.D0-TETA(I4))*(ZP(I4)-ZP(I1))
        H5 =TETA(I5) *(ZN(I5)-ZN(I2))+(1.D0-TETA(I5))*(ZP(I5)-ZP(I2))
        H6 =TETA(I6) *(ZN(I6)-ZN(I3))+(1.D0-TETA(I6))*(ZP(I6)-ZP(I3))
!
        ISEG1 = ELTSEG(IELEM,1)
        ISEG2 = ELTSEG(IELEM,2)
        ISEG3 = ELTSEG(IELEM,3)
        ISEG4 = ELTSEG(IELEM,4)
        ISEG5 = ELTSEG(IELEM,5)
        ISEG6 = ELTSEG(IELEM,6)
        ISEG7 = ELTSEG(IELEM,7)
        ISEG8 = ELTSEG(IELEM,8)
        ISEG9 = ELTSEG(IELEM,9)
        ISEG10 = ELTSEG(IELEM,10)
        ISEG11 = ELTSEG(IELEM,11)
        ISEG12 = ELTSEG(IELEM,12)
        ISEG13 = ELTSEG(IELEM,13)
        ISEG14 = ELTSEG(IELEM,14)
        ISEG15 = ELTSEG(IELEM,15)
!
!       HERE EQUIVALENT OF LIPS IN 2D
!       (BUT WE DO SOMETHING ELSE, ASSEMBLED FLUXES ARE SHARED
!        PROPORTIONNALLY TO SURFACES, SEE SUR2VOL IN MURD3D_LIPS)
!
!       L12 = XM(01,IELEM)
!       L13 = XM(02,IELEM)
!       L14 = XM(03,IELEM)
!       L15 = XM(04,IELEM)
!       L16 = XM(05,IELEM)
!       L23 = XM(06,IELEM)
!       L24 = XM(07,IELEM)
!       L25 = XM(08,IELEM)
!       L26 = XM(09,IELEM)
!       L34 = XM(10,IELEM)
!       L35 = XM(11,IELEM)
!       L36 = XM(12,IELEM)
!       L45 = XM(13,IELEM)
!       L46 = XM(14,IELEM)
!       L56 = XM(15,IELEM)
!       L21 = XM(16,IELEM)
!       L31 = XM(17,IELEM)
!       L41 = XM(18,IELEM)
!       L51 = XM(19,IELEM)
!       L61 = XM(20,IELEM)
!       L32 = XM(21,IELEM)
!       L42 = XM(22,IELEM)
!       L52 = XM(23,IELEM)
!       L62 = XM(24,IELEM)
!       L43 = XM(25,IELEM)
!       L53 = XM(26,IELEM)
!       L63 = XM(27,IELEM)
!       L54 = XM(28,IELEM)
!       L64 = XM(29,IELEM)
!       L65 = XM(30,IELEM)
!
!       END OF EQUIVALENT OF LIPS IN 2D
!
!       CORRECTING THE FLUXES WHEN THEIR SIGN IS NOT THE SAME
!       AS THE ASSEMBLED VALUE, KNOWING THAT ALL THE FPIJ ARE
!       POSITIVE BY CONSTRUCTION
!
        COEF = SURFAC(IELEM)
!
!       SEGMENT 1
!
        IF(ORISEG(IELEM,1).EQ.1) THEN
          IF(FXMATPAR(ISEG1).GT.0.D0) THEN
            L12=0.D0
            L21= FXMATPAR(ISEG1)*COEF*SUR2VOL(ISEG1)
          ELSE
            L21=0.D0
            L12=-FXMATPAR(ISEG1)*COEF*SUR2VOL(ISEG1)
          ENDIF
        ELSE
          IF(FXMATPAR(ISEG1).GT.0.D0) THEN
            L21=0.D0
            L12= FXMATPAR(ISEG1)*COEF*SUR2VOL(ISEG1)
          ELSE
            L12=0.D0
            L21=-FXMATPAR(ISEG1)*COEF*SUR2VOL(ISEG1)
          ENDIF
        ENDIF
!
!       SEGMENT 2
!
        IF(ORISEG(IELEM,2).EQ.1) THEN
          IF(FXMATPAR(ISEG2).GT.0.D0) THEN
            L23=0.D0
            L32= FXMATPAR(ISEG2)*COEF*SUR2VOL(ISEG2)
          ELSE
            L32=0.D0
            L23=-FXMATPAR(ISEG2)*COEF*SUR2VOL(ISEG2)
          ENDIF
        ELSE
          IF(FXMATPAR(ISEG2).GT.0.D0) THEN
            L32=0.D0
            L23= FXMATPAR(ISEG2)*COEF*SUR2VOL(ISEG2)
          ELSE
            L23=0.D0
            L32=-FXMATPAR(ISEG2)*COEF*SUR2VOL(ISEG2)
          ENDIF
        ENDIF
!
!       SEGMENT 3
!
        IF(ORISEG(IELEM,3).EQ.1) THEN
          IF(FXMATPAR(ISEG3).GT.0.D0) THEN
            L31=0.D0
            L13= FXMATPAR(ISEG3)*COEF*SUR2VOL(ISEG3)
          ELSE
            L13=0.D0
            L31=-FXMATPAR(ISEG3)*COEF*SUR2VOL(ISEG3)
          ENDIF
        ELSE
          IF(FXMATPAR(ISEG3).GT.0.D0) THEN
            L13=0.D0
            L31= FXMATPAR(ISEG3)*COEF*SUR2VOL(ISEG3)
          ELSE
            L31=0.D0
            L13=-FXMATPAR(ISEG3)*COEF*SUR2VOL(ISEG3)
          ENDIF
        ENDIF
!
!       SEGMENT 4
!
        IF(ORISEG(IELEM,4).EQ.1) THEN
          IF(FXMATPAR(ISEG4).GT.0.D0) THEN
            L45=0.D0
            L54= FXMATPAR(ISEG4)*COEF*SUR2VOL(ISEG4)
          ELSE
            L54=0.D0
            L45=-FXMATPAR(ISEG4)*COEF*SUR2VOL(ISEG4)
          ENDIF
        ELSE
          IF(FXMATPAR(ISEG4).GT.0.D0) THEN
            L54=0.D0
            L45=FXMATPAR(ISEG4)*COEF*SUR2VOL(ISEG4)
          ELSE
            L45=0.D0
            L54=-FXMATPAR(ISEG4)*COEF*SUR2VOL(ISEG4)
          ENDIF
        ENDIF
!
!       SEGMENT 5
!
        IF(ORISEG(IELEM,5).EQ.1) THEN
          IF(FXMATPAR(ISEG5).GT.0.D0) THEN
            L56=0.D0
            L65= FXMATPAR(ISEG5)*COEF*SUR2VOL(ISEG5)
          ELSE
            L65=0.D0
            L56=-FXMATPAR(ISEG5)*COEF*SUR2VOL(ISEG5)
          ENDIF
        ELSE
          IF(FXMATPAR(ISEG5).GT.0.D0) THEN
            L65=0.D0
            L56= FXMATPAR(ISEG5)*COEF*SUR2VOL(ISEG5)
          ELSE
            L56=0.D0
            L65=-FXMATPAR(ISEG5)*COEF*SUR2VOL(ISEG5)
          ENDIF
        ENDIF
!
!       SEGMENT 6
!
        IF(ORISEG(IELEM,6).EQ.1) THEN
          IF(FXMATPAR(ISEG6).GT.0.D0) THEN
            L64=0.D0
            L46= FXMATPAR(ISEG6)*COEF*SUR2VOL(ISEG6)
          ELSE
            L46=0.D0
            L64=-FXMATPAR(ISEG6)*COEF*SUR2VOL(ISEG6)
          ENDIF
        ELSE
          IF(FXMATPAR(ISEG6).GT.0.D0) THEN
            L46=0.D0
            L64= FXMATPAR(ISEG6)*COEF*SUR2VOL(ISEG6)
          ELSE
            L64=0.D0
            L46=-FXMATPAR(ISEG6)*COEF*SUR2VOL(ISEG6)
          ENDIF
        ENDIF
!
!       VERTICAL SEGMENTS
!
!       SEGMENT 7 (VERTICAL)
!
        IF(FXMATPAR(ISEG7).GT.0.D0) THEN
          L14=0.D0
          L41= FXMATPAR(ISEG7)*COEF*SUR2VOL(ISEG7)
        ELSE
          L41=0.D0
          L14=-FXMATPAR(ISEG7)*COEF*SUR2VOL(ISEG7)
        ENDIF
!
!       SEGMENT 8 (VERTICAL)
!
        IF(FXMATPAR(ISEG8).GT.0.D0) THEN
          L25=0.D0
          L52= FXMATPAR(ISEG8)*COEF*SUR2VOL(ISEG8)
        ELSE
          L52=0.D0
          L25=-FXMATPAR(ISEG8)*COEF*SUR2VOL(ISEG8)
        ENDIF
!
!       SEGMENT 9 (VERTICAL)
!
        IF(FXMATPAR(ISEG9).GT.0.D0) THEN
          L36=0.D0
          L63= FXMATPAR(ISEG9)*COEF*SUR2VOL(ISEG9)
        ELSE
          L63=0.D0
          L36=-FXMATPAR(ISEG9)*COEF*SUR2VOL(ISEG9)
        ENDIF
!
!       CROSSED SEGMENTS :
!       SHARED BY ONLY 2 ELEMENTS, THEY COULD BE TREATED
!       LIKE IN 2D (COMMENTED LINES)
!       BUT HERE SAME TREATMENT AS OTHERS
!
!
!       SEGMENT 10
!
        IF(FXMATPAR(ISEG10).GT.0.D0) THEN
          L15=0.D0
!         IF(L51.GT. FXMATPAR(ISEG10)) L51=FXMATPAR(ISEG10)
          L51= FXMATPAR(ISEG10)*COEF*SUR2VOL(ISEG10)
        ELSE
          L51=0.D0
!         IF(L15.GT.-FXMATPAR(ISEG10)) L15=-FXMATPAR(ISEG10)
          L15=-FXMATPAR(ISEG10)*COEF*SUR2VOL(ISEG10)
        ENDIF
!
!       SEGMENT 11
!
        IF(FXMATPAR(ISEG11).GT.0.D0) THEN
          L24=0.D0
!         IF(L42.GT. FXMATPAR(ISEG11)) L42=FXMATPAR(ISEG11)
          L42= FXMATPAR(ISEG11)*COEF*SUR2VOL(ISEG11)
        ELSE
          L42=0.D0
!         IF(L24.GT.-FXMATPAR(ISEG11)) L24=-FXMATPAR(ISEG11)
          L24=-FXMATPAR(ISEG11)*COEF*SUR2VOL(ISEG11)
        ENDIF
!
!       SEGMENT 12
!
        IF(FXMATPAR(ISEG12).GT.0.D0) THEN
          L26=0.D0
!         IF(L62.GT. FXMATPAR(ISEG12)) L62=FXMATPAR(ISEG12)
          L62= FXMATPAR(ISEG12)*COEF*SUR2VOL(ISEG12)
        ELSE
          L62=0.D0
!         IF(L26.GT.-FXMATPAR(ISEG12)) L26=-FXMATPAR(ISEG12)
          L26=-FXMATPAR(ISEG12)*COEF*SUR2VOL(ISEG12)
        ENDIF
!
!       SEGMENT 13
!
        IF(FXMATPAR(ISEG13).GT.0.D0) THEN
          L35=0.D0
!         IF(L53.GT. FXMATPAR(ISEG13)) L53=FXMATPAR(ISEG13)
          L53= FXMATPAR(ISEG13)*COEF*SUR2VOL(ISEG13)
        ELSE
          L53=0.D0
!         IF(L35.GT.-FXMATPAR(ISEG13)) L35=-FXMATPAR(ISEG13)
          L35=-FXMATPAR(ISEG13)*COEF*SUR2VOL(ISEG13)
        ENDIF
!
!       SEGMENT 14
!
        IF(FXMATPAR(ISEG14).GT.0.D0) THEN
          L34=0.D0
!         IF(L43.GT. FXMATPAR(ISEG14)) L43=FXMATPAR(ISEG14)
          L43= FXMATPAR(ISEG14)*COEF*SUR2VOL(ISEG14)
        ELSE
          L43=0.D0
!         IF(L34.GT.-FXMATPAR(ISEG14)) L34=-FXMATPAR(ISEG14)
          L34=-FXMATPAR(ISEG14)*COEF*SUR2VOL(ISEG14)
        ENDIF
!
!       SEGMENT 15
!
        IF(FXMATPAR(ISEG15).GT.0.D0) THEN
          L16=0.D0
!         IF(L61.GT. FXMATPAR(ISEG15)) L61=FXMATPAR(ISEG15)
          L61= FXMATPAR(ISEG15)*COEF*SUR2VOL(ISEG15)
        ELSE
          L61=0.D0
!         IF(L16.GT.-FXMATPAR(ISEG15)) L16=-FXMATPAR(ISEG15)
          L16=-FXMATPAR(ISEG15)*COEF*SUR2VOL(ISEG15)
        ENDIF
!
!       END OF CORRECTION OF FLUXES
!
        F1 = FN(I1)
        F2 = FN(I2)
        F3 = FN(I3)
        F4 = FN(I4)
        F5 = FN(I5)
        F6 = FN(I6)
!
!       MINIMUM OF THE 1-TETA
!
        MT1=1.D0-TETA(I1)
        MT2=1.D0-TETA(I2)
        MT3=1.D0-TETA(I3)
        MT4=1.D0-TETA(I4)
        MT5=1.D0-TETA(I5)
        MT6=1.D0-TETA(I6)
        MIN12=MIN(MT1,MT2)
        MIN13=MIN(MT1,MT3)
        MIN14=MIN(MT1,MT4)
        MIN15=MIN(MT1,MT5)
        MIN16=MIN(MT1,MT6)
        MIN23=MIN(MT2,MT3)
        MIN24=MIN(MT2,MT4)
        MIN25=MIN(MT2,MT5)
        MIN26=MIN(MT2,MT6)
        MIN34=MIN(MT3,MT4)
        MIN35=MIN(MT3,MT5)
        MIN36=MIN(MT3,MT6)
        MIN45=MIN(MT4,MT5)
        MIN46=MIN(MT4,MT6)
        MIN56=MIN(MT5,MT6)
!
!       PART OF CONTRIBUTIONS THAT WILL NOT BE LIMITED, IMMEDIATELY ASSEMBLED
!
        FI_I(I1)=FI_I(I1)+L12*(F1*(MT1-MIN12)-F2*(MT2-MIN12))
     &                   +L13*(F1*(MT1-MIN13)-F3*(MT3-MIN13))
     &                   +L14*(F1*(MT1-MIN14)-F4*(MT4-MIN14))
     &                   +L15*(F1*(MT1-MIN15)-F5*(MT5-MIN15))
     &                   +L16*(F1*(MT1-MIN16)-F6*(MT6-MIN16))
        FI_I(I2)=FI_I(I2)+L21*(F2*(MT2-MIN12)-F1*(MT1-MIN12))
     &                   +L23*(F2*(MT2-MIN23)-F3*(MT3-MIN23))
     &                   +L24*(F2*(MT2-MIN24)-F4*(MT4-MIN24))
     &                   +L25*(F2*(MT2-MIN25)-F5*(MT5-MIN25))
     &                   +L26*(F2*(MT2-MIN26)-F6*(MT6-MIN26))
        FI_I(I3)=FI_I(I3)+L31*(F3*(MT3-MIN13)-F1*(MT1-MIN13))
     &                   +L32*(F3*(MT3-MIN23)-F2*(MT2-MIN23))
     &                   +L34*(F3*(MT3-MIN34)-F4*(MT4-MIN34))
     &                   +L35*(F3*(MT3-MIN35)-F5*(MT5-MIN35))
     &                   +L36*(F3*(MT3-MIN36)-F6*(MT6-MIN36))
        FI_I(I4)=FI_I(I4)+L41*(F4*(MT4-MIN14)-F1*(MT1-MIN14))
     &                   +L42*(F4*(MT4-MIN24)-F2*(MT2-MIN24))
     &                   +L43*(F4*(MT4-MIN34)-F3*(MT3-MIN34))
     &                   +L45*(F4*(MT4-MIN45)-F5*(MT5-MIN45))
     &                   +L46*(F4*(MT4-MIN46)-F6*(MT6-MIN46))
        FI_I(I5)=FI_I(I5)+L51*(F5*(MT5-MIN15)-F1*(MT1-MIN15))
     &                   +L52*(F5*(MT5-MIN25)-F2*(MT2-MIN25))
     &                   +L53*(F5*(MT5-MIN35)-F3*(MT3-MIN35))
     &                   +L54*(F5*(MT5-MIN45)-F4*(MT4-MIN45))
     &                   +L56*(F5*(MT5-MIN56)-F6*(MT6-MIN56))
        FI_I(I6)=FI_I(I6)+L61*(F6*(MT6-MIN16)-F1*(MT1-MIN16))
     &                   +L62*(F6*(MT6-MIN26)-F2*(MT2-MIN26))
     &                   +L63*(F6*(MT6-MIN36)-F3*(MT3-MIN36))
     &                   +L64*(F6*(MT6-MIN46)-F4*(MT4-MIN46))
     &                   +L65*(F6*(MT6-MIN56)-F5*(MT5-MIN56))
!
!       NOW PART OF CONTRIBUTIONS THAT WILL BE LIMITED
!
        COEF = SURFAC(IELEM)/6.D0
!
        FINCORR1=COEF*H1*DFDT(I1)+L12*(F1-F2)*MIN12
     &                           +L13*(F1-F3)*MIN13
     &                           +L14*(F1-F4)*MIN14
     &                           +L15*(F1-F5)*MIN15
     &                           +L16*(F1-F6)*MIN16
        FINCORR2=COEF*H2*DFDT(I2)+L21*(F2-F1)*MIN12
     &                           +L23*(F2-F3)*MIN23
     &                           +L24*(F2-F4)*MIN24
     &                           +L25*(F2-F5)*MIN25
     &                           +L26*(F2-F6)*MIN26
        FINCORR3=COEF*H3*DFDT(I3)+L31*(F3-F1)*MIN13
     &                           +L32*(F3-F2)*MIN23
     &                           +L34*(F3-F4)*MIN34
     &                           +L35*(F3-F5)*MIN35
     &                           +L36*(F3-F6)*MIN36
        FINCORR4=COEF*H4*DFDT(I4)+L41*(F4-F1)*MIN14
     &                           +L42*(F4-F2)*MIN24
     &                           +L43*(F4-F3)*MIN34
     &                           +L45*(F4-F5)*MIN45
     &                           +L46*(F4-F6)*MIN46
        FINCORR5=COEF*H5*DFDT(I5)+L51*(F5-F1)*MIN15
     &                           +L52*(F5-F2)*MIN25
     &                           +L53*(F5-F3)*MIN35
     &                           +L54*(F5-F4)*MIN45
     &                           +L56*(F5-F6)*MIN56
        FINCORR6=COEF*H6*DFDT(I6)+L61*(F6-F1)*MIN16
     &                           +L62*(F6-F2)*MIN26
     &                           +L63*(F6-F3)*MIN36
     &                           +L64*(F6-F4)*MIN46
     &                           +L65*(F6-F5)*MIN56
!
!       PHITCOR IS THE NEW TOTAL RESIDUAL,
!
        PHITCOR=FINCORR1+FINCORR2+FINCORR3+FINCORR4+FINCORR5+FINCORR6
!
        IF(PHITCOR.GT.EPSPHI) THEN
!         PSI REDUCTION
          BETA1=MAX(FINCORR1,0.D0)
          BETA2=MAX(FINCORR2,0.D0)
          BETA3=MAX(FINCORR3,0.D0)
          BETA4=MAX(FINCORR4,0.D0)
          BETA5=MAX(FINCORR5,0.D0)
          BETA6=MAX(FINCORR6,0.D0)
          COEF=PHITCOR/(BETA1+BETA2+BETA3+BETA4+BETA5+BETA6)
          FI_I(I1)=FI_I(I1)+BETA1*COEF
          FI_I(I2)=FI_I(I2)+BETA2*COEF
          FI_I(I3)=FI_I(I3)+BETA3*COEF
          FI_I(I4)=FI_I(I4)+BETA4*COEF
          FI_I(I5)=FI_I(I5)+BETA5*COEF
          FI_I(I6)=FI_I(I6)+BETA6*COEF
        ELSEIF(PHITCOR.LT.-EPSPHI) THEN
!         PSI REDUCTION
          BETA1=MIN(FINCORR1,0.D0)
          BETA2=MIN(FINCORR2,0.D0)
          BETA3=MIN(FINCORR3,0.D0)
          BETA4=MIN(FINCORR4,0.D0)
          BETA5=MIN(FINCORR5,0.D0)
          BETA6=MIN(FINCORR6,0.D0)
          COEF=PHITCOR/(BETA1+BETA2+BETA3+BETA4+BETA5+BETA6)
          FI_I(I1)=FI_I(I1)+BETA1*COEF
          FI_I(I2)=FI_I(I2)+BETA2*COEF
          FI_I(I3)=FI_I(I3)+BETA3*COEF
          FI_I(I4)=FI_I(I4)+BETA4*COEF
          FI_I(I5)=FI_I(I5)+BETA5*COEF
          FI_I(I6)=FI_I(I6)+BETA6*COEF
        ELSE
!         NO REDUCTION
          FI_I(I1)=FI_I(I1)+FINCORR1
          FI_I(I2)=FI_I(I2)+FINCORR2
          FI_I(I3)=FI_I(I3)+FINCORR3
          FI_I(I4)=FI_I(I4)+FINCORR4
          FI_I(I5)=FI_I(I5)+FINCORR5
          FI_I(I6)=FI_I(I6)+FINCORR6
        ENDIF

!
      ENDDO
!
!---------------------------------------------------------------------
!
      RETURN
      END

