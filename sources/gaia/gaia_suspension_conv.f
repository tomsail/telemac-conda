!                   ********************************
                    SUBROUTINE GAIA_SUSPENSION_CONV
!                   ********************************
     &(UCONV_TEL,VCONV_TEL,ICONVF,SOLSYS,J,FLBOR_W)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Takes into account the vertical profile of concentrations and
!        velocities
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param [in]     UCONV_TEL   X velocity field of TELEMAC
!>@param [in]     VCONV_TEL   Y velocity field of TELEMAC
!>@param [in]     ICONVF      Advection scheme
!>@param [in]     SOLSYS      Option for the advection (see cvdftr)
!>@param [in]     J           Number of the layer considered
!>@param [in]     FLBOR_W     Water flux on boundary
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN) :: UCONV_TEL,VCONV_TEL,FLBOR_W
      INTEGER, INTENT(IN) :: ICONVF,SOLSYS,J
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION R1,I1,I2,A,B,AUX,LAUX,LL,USTAR,ROUSE
      INTEGER I
      INTEGER, POINTER, DIMENSION(:) :: GLOSEG1,GLOSEG2
!
!-----------------------------------------------------------------------
!
!     OPTVF : TENS                  0 : NORMAL
!                                   1 : ADVECTION FIELD DOES NOT SATISFY
!                                       CONTINUITY
!
!     OPTVF : UNITS                 0 : CONSTANT = 0
!                                   1 : CHI-TUAN CONSTANT
!                                   2 : LEO POSTMA CONSTANT
!                                   SEE CVTRVF IN BIEF AND
!                                   V5.7 RELEASE NOTES
!
      SOLSYS_GAI=SOLSYS
      SAVE_UCONV=>UCONV_GAI%R
      SAVE_VCONV=>VCONV_GAI%R
      GLOSEG1=>MESH%GLOSEG%I(1:MESH%GLOSEG%DIM1)
      GLOSEG2=>MESH%GLOSEG%I(MESH%GLOSEG%DIM1+1:2*MESH%GLOSEG%DIM1)
!
      YAFLULIM_GAI=.FALSE.
!
      IF(CORR_CONV.AND.(.NOT.SEDCO(J))) THEN
!
        LL=LOG(30.D0)
!
        DO I = 1, NPOIN
!
          IF(TOB%R(I).GT.ZERO) THEN
!
!           Here we use TOBCW_MEAN instead of TOB to define USTAR.
!           In case there are no wave effects in the simulation,
!           TOBCW_MEAN = TOB. In case there are waves, the
!           superimposed effects of waves and current are taken
!           into account to establish the suspension concentration
!           profile through USTAR.

            USTAR = SQRT(TOBCW_MEAN%R(I)/XMVE)
!
!           B --> KS/H
!
!           AUX = 1.D0 + KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
!           B = 30.D0*EXP(-AUX)
!
            B = KSR%R(I) /MAX(HN%R(I),1.1D0*KSR%R(I))
            A = ZREF%R(I)/MAX(HN%R(I),1.1D0*ZREF%R(I))
!
!   TAKES MAX VALUE OF A = ZREF/H AND B=KSR/H
            A=MAX(A,B)
!
!   SIMPLIFIED VERSION
            ROUSE=MIN(XWC(J)/MAX(USTAR,ZERO),1.D0)/KARMAN
            R1=  1.D0-ROUSE
            LAUX=LOG(A)
!
            IF(ABS(R1).LT.1.D-8) THEN
              I1= -LAUX
              I2= -LAUX**2/2.D0
            ELSE
              AUX=A**R1
              I1=(1.D0-AUX)/R1
              I2=-(I1+LAUX*AUX)/R1
            ENDIF
!
!           AUX=LOG(A/30.D0)
            AUX=LAUX - LL
            T1%R(I)=-(I2-AUX*I1)/(I1*(AUX+1.D0))
!
          ELSE
!
            T1%R(I)=1.D0
!
          ENDIF
!
!         CHECKS 0
!
          T1%R(I)=MIN(T1%R(I),1.D0)
          T1%R(I)=MAX(T1%R(I),0.D0)
!
        ENDDO
!
!       DEPENDING ON ADVECTION SCHEME : LIMITATION OF VELOCITY OR FLUXES
!
        IF(ICONVF.EQ.13.OR.ICONVF.EQ.14) THEN
!
!         LIMITATION OF FLUXES WITH FLULIM
!
          IF (.NOT.CONV_GAI_POINTER) THEN
            DEALLOCATE(UCONV_GAI%R)
            DEALLOCATE(VCONV_GAI%R)
            CONV_GAI_POINTER = .TRUE.
          ENDIF
          UCONV_GAI%R=>UCONV_TEL%R
          VCONV_GAI%R=>VCONV_TEL%R
          DO I=1,MESH%NSEG
            FLULIM_GAI%R(I)=0.5D0*(T1%R(GLOSEG1(I))+T1%R(GLOSEG2(I)))
          ENDDO
          YAFLULIM_GAI=.TRUE.
!
        ELSE
!
!         LIMITATION OF VELOCITY
!
          SOLSYS_GAI=1
          DO I = 1,NPOIN
            UCONV_GAI%R(I) = T1%R(I)*U2D%R(I)
            VCONV_GAI%R(I) = T1%R(I)*V2D%R(I)
          ENDDO
          YAFLULIM_GAI=.FALSE.
!
        ENDIF
!
!       ADVECTION FORM WHICH ACCEPTS AN ADVECTION FIELD
!       THAT DOES NOT SATISFY CONTINUITY + LEO-POSTMA CONSTANT
!
!       WITH 12: MASS CONSERVATION BUT NO MONOTONICITY
!                THE CORRECT THEORY
        OPTVF_GAI=12
!
!       WITH 2: MONOTONICITY BUT NO MASS CONSERVATION
!               WRONG THEORY
!       OPTVF=2
!
!       OPTVF=2 IS POSSIBLE BUT WITH MASS CONSERVATION SPOILED
!       THE UNIT (HERE 2) IS REDONE IN CVDFTR ACCORDING TO THE
!       VALUE OF RESOL, SO IT IS NOT IMPORTANT HERE.
!
        CALL OS('X=Y     ',X=FLBOR_GAI,Y=FLBOR_W)
        CALL OSBD('X=CXY   ',FLBOR_GAI,T1,T1,1.D0,MESH)
!
      ELSE
!
!       POINTERS ARE USED TO AVOID COPY
!
!       HERE UCONV_TEL IS PASSED ON
        IF (.NOT.CONV_GAI_POINTER) THEN
          DEALLOCATE(UCONV_GAI%R)
          DEALLOCATE(VCONV_GAI%R)
          CONV_GAI_POINTER = .TRUE.
        ENDIF
        UCONV_GAI%R=>UCONV_TEL%R
        VCONV_GAI%R=>VCONV_TEL%R
!       ADVECTION FORM THAT REQUIRES AN ADVECTION FIELD
!       THAT SATISFIES CONTINUITY + LEO-POSTMA CONSTANT
        OPTVF_GAI=2
!
      ENDIF
!
      RETURN
      END
