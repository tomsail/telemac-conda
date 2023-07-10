!           ****************************************************
            DOUBLE PRECISION FUNCTION CVSP_INTEGRATE_VOLUME_GAIA
!           ****************************************************
!
     &(J,I,Z_HIGH,Z_LOW,A)
!
!***********************************************************************
! GAIA   V8P1                                   16/05/2017
!***********************************************************************
!
!>@brief   INTEGRATES THE VOLUME OF A FRACTION WITHIN THE
!!       VERTICAL SORTING PROFIL BETWEEN 2 DEPTH Z-COORDINATES Z_HIGH & Z_LOW
!
!>@history UWE MERKEL
!!        2011
!!        V6P2
!
!>@history  P. A. TASSI (EDF R&D, LNHE)
!!        12/03/2013
!!        V6P3
!!   Cleaning, cosmetic
!
!>@history UWE MERKEL, R. KOPMANN
!!        2016, 2017
!!        V6P3, V7P2
!!        improved stability
!
!>@history  R. KOPMANN
!!        2018
!!        V7P2
!!        max. number of fractions
!
!>@history  R. KOPMANN (BAW)
!!        25/02/2019
!!        V7P2
!!   Removing 1/NSICLA
!!   no correction of truncation error moved to new subroutine cvsp_check_mass_bilan
!!   no check_f
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] J Index of a point in mesh
!>@param[in] I Index of a fraction in vertical sorting profile
!>@param[in] Z_HIGH Higher depth coordinate
!>@param[in] Z_LOW Lower  depth coordinate
!>@param[out] A Integrated volumes per fraction
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,
     & EX_CVSP_INTEGRATE_VOLUME_GAIA => CVSP_INTEGRATE_VOLUME_GAIA
      USE DECLARATIONS_GAIA
      USE CVSP_OUTPUTFILES_GAIA, ONLY: CP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)  :: J
      INTEGER,          INTENT(IN)  :: I
      DOUBLE PRECISION, INTENT(IN)  :: Z_HIGH
      DOUBLE PRECISION, INTENT(IN)  :: Z_LOW
      DOUBLE PRECISION, INTENT(OUT) :: A(NSICLA)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TEMP, DHIG, DLOW, AT, SUMUP, FLOW, FHIG
      DOUBLE PRECISION TEMP2, TEMP2MAX, SUMUP2,TEMP3, TEMP3MAX, SUMUP3
      DOUBLE PRECISION CHSUM
      INTEGER L_CNT, F_CNT, REVCNT, HELPER, LASTCASE, JG
      INTEGER(KIND=K8) :: MYCASE
!
!-----------------------------------------------------------------------
!
      JG = J
      IF (NCSIZE.GT.1) JG = MESH%KNOLG%I(J)
!
      AT = DT*LT/PERCOU
      SUMUP = 0.D0
      SUMUP2  = 0.D0
      SUMUP3  = 0.D0
      MYCASE  = 0
!
      DO F_CNT = 1, NSICLA
        A(F_CNT) = 0.D0
      END DO
      CVSP_INTEGRATE_VOLUME_GAIA = A(I)
      IF ((Z_HIGH-Z_LOW).LT.ZERO) THEN
        RETURN
      ENDIF
!-----------------------------------------------------------------------
! DOING ALL FRACTIONS
!-----------------------------------------------------------------------
      DO F_CNT = 1, NSICLA
        TEMP = 0.D0
        TEMP2 = 0.D0
        TEMP2MAX = 0.D0
        TEMP3 = 0.D0
        TEMP3MAX = 0.D0
        CHSUM = 0.D0
!-----------------------------------------------------------------------
! GOING THROUGH ALL SECTIONS
!-----------------------------------------------------------------------
        DO L_CNT = 0,(PRO_MAX(J)-2)
          REVCNT = PRO_MAX(J)-L_CNT
!-----------------------------------------------------------------------
! DEPTH COORDINATES OF THE SECTION TO CHECK
!-----------------------------------------------------------------------
          DHIG = PRO_D(J,REVCNT,F_CNT)
          DLOW = PRO_D(J,REVCNT-1,F_CNT)
!-----------------------------------------------------------------------
! SORTING PROFILE SECTION TOTALLY INSIDE (CASE ZDDZ)
!-----------------------------------------------------------------------
          IF ( (DHIG <= Z_HIGH ) .AND.
     &         (DLOW >= Z_LOW  ) ) THEN
              FLOW = PRO_F(J,REVCNT-1,F_CNT)
              FHIG = PRO_F(J,REVCNT,F_CNT)
              MYCASE  = MYCASE  + 1
              LASTCASE = 1
              TEMP2 = 0.5D0*(FHIG+FLOW)*(DHIG-DLOW)  + TEMP2
              TEMP2MAX = Z_HIGH - DLOW
              DO HELPER = 1, NSICLA
                CHSUM = PRO_F(J,REVCNT, HELPER) + CHSUM
              ENDDO
              CHSUM = 1.D0 - CHSUM
!-----------------------------------------------------------------------
! SORTING PROFILE SECTION PARTIALLY LOWER (CASE ZDZD)
!-----------------------------------------------------------------------
          ELSEIF ((DHIG <= Z_HIGH) .AND.
     &            (DHIG >  Z_LOW ) .AND.
     &            (DLOW <  Z_LOW ) )  THEN

              FHIG = PRO_F(J,REVCNT,F_CNT)
              FLOW = PRO_F(J,REVCNT-1,F_CNT)
              FLOW = - ((FHIG-FLOW)/(DHIG-DLOW))*(DHIG-Z_LOW) + FHIG
!-----------------------------------------------------------------------
! CUT THE SECTION
!-----------------------------------------------------------------------
              DLOW = Z_LOW
              MYCASE  = MYCASE  + 1000
              LASTCASE = 2
              TEMP3 = 0.5D0*(FHIG+FLOW)*(DHIG-DLOW)  + TEMP3
              TEMP3MAX = DHIG - DLOW
!-----------------------------------------------------------------------
! SORTING PROFILE SECTION PARTIALLY HIGHER (CASE DZDZ)
!-----------------------------------------------------------------------
!
          ELSEIF ((DHIG > Z_HIGH) .AND.
     &            (DLOW >= Z_LOW) .AND.
     &            (DLOW <  Z_HIGH) ) THEN
              FLOW = PRO_F(J,REVCNT-1,F_CNT)
              FHIG = PRO_F(J,REVCNT,F_CNT)
              FHIG = ((FHIG-FLOW)/(DHIG-DLOW))*(Z_HIGH-DLOW) + FLOW
!-----------------------------------------------------------------------
!INSERT
!-----------------------------------------------------------------------
!CUT THE SECTION
              DHIG = Z_HIGH
              MYCASE  = MYCASE + 1000000
              LASTCASE = 3
!-----------------------------------------------------------------------
! LAYER TOTALLY INSIDE ONE SECTION (CASE DZZD)
!-----------------------------------------------------------------------
          ELSEIF ((DHIG >= Z_HIGH) .AND.
     &            (DLOW <= Z_LOW) ) THEN
              FHIG =
     &           - (PRO_F(J,REVCNT,F_CNT)-PRO_F(J,REVCNT-1,F_CNT)) /
     &           (DHIG-DLOW) *
     &           (DHIG-Z_HIGH)
     &           + PRO_F(J,REVCNT,F_CNT)
              FLOW =
     &           - (PRO_F(J,REVCNT,F_CNT)-PRO_F(J,REVCNT-1,F_CNT)) /
     &           (DHIG-DLOW) *
     &           (DHIG-Z_LOW)
     &           + PRO_F(J,REVCNT,F_CNT)
!-----------------------------------------------------------------------
! CUT THE SECTION
!-----------------------------------------------------------------------
              DHIG = Z_HIGH
              DLOW = Z_LOW
              LASTCASE = 8
              MYCASE  = MYCASE + 100000000
!-----------------------------------------------------------------------
! SECTION WITH 0 STRENGTH
!-----------------------------------------------------------------------
          ELSEIF (DHIG == DLOW) THEN
              FLOW = 0.D0
              FHIG = 0.D0
              MYCASE  = MYCASE + 1000000000
              LASTCASE = 4
!A TRUE BUG!
          ELSEIF (Z_LOW.GE.Z_HIGH) THEN
              FLOW = 0.D0
              FHIG = 0.D0
              IF(CP)WRITE(LU,*)'Z_LOW>=Z_HIGH',DHIG,DLOW,Z_HIGH,Z_LOW
              CALL CVSP_P_GAIA('./','ZLOHI',JG)
              MYCASE  = MYCASE + 1000000000
              LASTCASE = 5
!A TRUE BUG!
          ELSEIF (DHIG < DLOW) THEN
              FLOW = 0.D0
              FHIG = 0.D0

              IF(CP)WRITE(LU,*)'DHIG<=DLOW',J,DHIG,DLOW,Z_HIGH,Z_LOW
              CALL CVSP_P_GAIA('./','DLOHI',JG)
              MYCASE  = MYCASE + 1000000000
              LASTCASE = 6
!A SECTION THAT IS NOT INVOLVED / NOT A BUG!!
          ELSE
              FLOW = 0.D0
              FHIG = 0.D0
              MYCASE  = MYCASE + 1000000000
              LASTCASE = 7
          ENDIF
!-----------------------------------------------------------------------
! TRAPEZOID FORMULA
!-----------------------------------------------------------------------
          TEMP = 0.5D0*(FHIG+FLOW)*(DHIG-DLOW) + TEMP
!DEBUG
          IF (0.5D0*(FHIG+FLOW)*(DHIG-DLOW) < 0.D0) THEN
              WRITE(LU,FMT='(A,1X,2(I11,1X),11(G20.10,1X),1X,I11)')
     &           'INTEGRATE_VOL_ER_TMP:<0:'
     &           ,JG, I, AT, FHIG,FLOW,DHIG,DLOW, DHIG-DLOW, REVCNT,
     &           PRO_F(J,REVCNT-1,F_CNT),PRO_F(J,REVCNT,F_CNT),
     &           PRO_D(J,REVCNT-1,F_CNT),PRO_D(J,REVCNT,F_CNT),
     &           LASTCASE
              CALL CVSP_P_GAIA('./','IVKT',JG)
              CALL PLANTE(1)
          ENDIF
!
        ENDDO                 !SECTION
!-----------------------------------------------------------------------
! ADDING UP FRACTIONS FOR DEBUGGING PURPOSES
!-----------------------------------------------------------------------
        SUMUP = TEMP + SUMUP
        SUMUP2 = TEMP2 + SUMUP2
        SUMUP3 = TEMP3 + SUMUP3
        A(F_CNT) = TEMP
      ENDDO                    !FRACTION
      CVSP_INTEGRATE_VOLUME_GAIA = A(I)
!
!     DEBUG
      IF (CVSP_INTEGRATE_VOLUME_GAIA < 0.D0) THEN
        CALL CVSP_P_GAIA('./','IVK0',JG)
        WRITE(*,FMT='(A,2(I11),14(G20.10))')'INTEGRATE_VOL_ER:<0:'
     &       ,JG, I, AT, CVSP_INTEGRATE_VOLUME_GAIA,
     &       MYCASE ,Z_HIGH,Z_LOW,
     &       SUMUP,SUMUP2,SUMUP3,CHSUM,A(1),A(2),A(3),A(4),A(5)
        CALL PLANTE(1)
      ENDIF
!-----------------------------------------------------------------------
! CHECKSUM OVER ALL FRACTIONS AND LAYERS
!-----------------------------------------------------------------------
      SUMUP  = SUMUP  - ABS(Z_HIGH-Z_LOW)
      SUMUP2 = SUMUP2 - ABS(TEMP2MAX)
      SUMUP3 = SUMUP3 - ABS(TEMP3MAX)
      CHSUM =  CHSUM / 5.D0
!-----------------------------------------------------------------------
! BEWARE: 1.D-5 IS UP TO 10G OF A 1000KG BUT HIGHER ACCURACY
! LEADS AGAIN TO FLOATING POINT TRUNCATION ERRORS ...
!-----------------------------------------------------------------------
      IF (ABS(SUMUP).GT.1.D-5) THEN
        IF(CP) CALL CVSP_P_GAIA('./','IV_E',JG)
        IF(CP)WRITE(LU,*) 'INTEGRATE VOLUME ACCURRACY!!!',SUMUP,JG
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION CVSP_INTEGRATE_VOLUME_GAIA
