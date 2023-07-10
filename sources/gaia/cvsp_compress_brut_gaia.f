!                   ********************************
                    SUBROUTINE CVSP_COMPRESS_BRUT_GAIA(J)
!                   ********************************
!
!
!***********************************************************************
! GAIA   V8P1                                   16/05/2017
!***********************************************************************
!
!>@brief    COMPRESSES A VERTICAL SORTING PROFILE IN POINT J TO PREVENT
!!        EXTENSIV GROTH OF SECTION / NODE NUMBERS
!!        BRUTAL VERSION
!!        IN CASE OF EMERGENCY, IF NO OTHER ALGORITHM IS ALLOWED TO COMPRESS,
!!        TO PREVENT PRO_MAX_MAX OVERFLOW
!
!>@history UWE MERKEL
!!        23/12/2011
!!        V6P2
!
!>@history PABLO TASSI PAT (EDF-LNHE)
!!        12/02/2013
!!        V6P3
!! PREPARING FOR THE USE OF A HIGHER NSICLM VALUE
!! (BY REBEKKA KOPMANN)
!
!>@history  P. A. TASSI (EDF R&D, LNHE)
!!        12/03/2013
!!        V6P3
!!   Cleaning, cosmetic
!
!>@history  R. KOPMANN (BAW)
!!        25/02/2019
!!        V7P2
!!   Removing 1/NSICLA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] J Index of a point in mesh
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA_BEDLOAD, EX =>
     & CVSP_COMPRESS_BRUT_GAIA
      USE DECLARATIONS_GAIA
!
      USE DECLARATIONS_SPECIAL
      USE CVSP_OUTPUTFILES_GAIA, ONLY: CP
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: J
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION Z_LOW ,Z_HIGH, SECHIGHT,DUMMY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! USING T1 INSTEAD, ASSUMING THAT NUMBER OF NODES ALWAYS BIGGER THAN NUMBER OF GRAIN SIZE CLASSES
!
      INTEGER NEWPRO_MAX, K, I, JG
      DOUBLE PRECISION H,L,M
      LOGICAL RET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!TEMPORARY VERTICAL SORTING PROFILE: FRACTION + DEPTH FOR EACH LAYER, CLASS, POINT
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::PRO_FNEW
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::PRO_DNEW
!
!-----------------------------------------------------------------------
!
      ALLOCATE(PRO_DNEW(PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_FNEW(PRO_MAX_MAX,NSICLA))

      JG = J
      IF(NCSIZE.GT.1) JG = MESH%KNOLG%I(J)
      IF(CVSP_DB_GAIA(JG,0)) CALL CVSP_P_GAIA('./','BRUT',JG)
!
!-----------------------------------------------------------------------
! REMOVES NUMERIC INSTABILITIES
!-----------------------------------------------------------------------
      DO K = 1, PRO_MAX(J)
        RET = CVSP_CHECK_F_GAIA(J,K,'BeforeBRUT:   ')
      ENDDO
      CALL CVSP_CHECK_STEADY_GAIA(J)
!
!-----------------------------------------------------------------------
! WORKS LIKE THE MAKE_ACT LAYER ROUTINE BUT FOR VSP
!-----------------------------------------------------------------------
!
      NEWPRO_MAX=INT(MAX(8.D0,(DBLE(PRO_MAX_MAX - 4 * NSICLA)*0.7D0)))
!
!-----------------------------------------------------------------------
! NEW VSP SECTION HEIGHT
!-----------------------------------------------------------------------
!
      H=PRO_D(J,PRO_MAX(J),1) !High
      L=PRO_D(J,1,1)          !Low
      M=H-L                   !Mighty
!
!SECTION THICKNESS
      SECHIGHT = M / (NEWPRO_MAX - 1)
      SECHIGHT = MAX(SECHIGHT,DCLA(1))
      NEWPRO_MAX=INT(M/SECHIGHT+1)
      NEWPRO_MAX=MAX(2,NEWPRO_MAX)
      SECHIGHT = M / (NEWPRO_MAX - 1)
!
!VERY THIN SECTION
      IF (M.LE.SECHIGHT) THEN
        NEWPRO_MAX = 3
        Z_LOW  = L
        Z_HIGH = H
        DO I = 1, NSICLA
          PRO_DNEW(1,I) = L
          PRO_DNEW(2,I) = L
          PRO_DNEW(3,I) = H
        ENDDO
        IF (M.GT.0.D0) THEN
          DUMMY=CVSP_INTEGRATE_VOLUME_GAIA(J,1,Z_HIGH,Z_LOW,T1%R)/
     &          (Z_HIGH-Z_LOW)
          DO I = 1, NSICLA
            PRO_FNEW(1,I) = T1%R(I) / SECHIGHT
            PRO_FNEW(2,I) = T1%R(I) / SECHIGHT
            PRO_FNEW(3,I) = T1%R(I) / SECHIGHT
          ENDDO
        ELSE
          DO I = 1, NSICLA
            PRO_FNEW(1,I) = 0.D0
            PRO_FNEW(2,I) = 0.D0
            PRO_FNEW(3,I) = 0.D0
          ENDDO
        ENDIF
!
!     LESS THIN SECTIONS
      ELSE
        DO K = 1, NEWPRO_MAX
          PRO_DNEW(K,1) = (K-1)*SECHIGHT + PRO_D(J,1,1)
          Z_LOW  = PRO_DNEW(K,1) - 0.5D0*SECHIGHT
          Z_HIGH = PRO_DNEW(K,1) + 0.5D0*SECHIGHT
          IF(K.EQ.1)  Z_LOW = L
          IF(K.EQ.NEWPRO_MAX) Z_HIGH = H
          DUMMY=CVSP_INTEGRATE_VOLUME_GAIA(J,1,Z_HIGH,Z_LOW,T1%R)/
     &          SECHIGHT
          DO I = 1, NSICLA
            PRO_DNEW(K,I) = (K-1)*SECHIGHT + PRO_D(J,1,1)
            IF (Z_HIGH-Z_LOW.LE.ZERO) THEN
              PRO_FNEW(K,I) = 0.D0
            ELSE
              PRO_FNEW(K,I) = T1%R(I) / SECHIGHT
            ENDIF
!
!           UPPER AND LOWER SECTIONS ARE ONLY HALF STRENGTH
            IF(K.EQ.1) PRO_FNEW(K,I) = PRO_FNEW(K,I)*2
            IF(K.EQ.NEWPRO_MAX) PRO_FNEW(K,I) = PRO_FNEW(K,I)*2
            IF (PRO_FNEW(K,I).GE.1.0D0) THEN
              IF(CP) PRINT*,'JKI,F,SECHIGHT,M,NEWPMAX',
     &        J,K,I,PRO_FNEW(K,I),SECHIGHT,M,NEWPRO_MAX
              IF(CP) PRINT*,'L,H,Z_LOW,Z_HIGH',
     &        L,H,Z_LOW,Z_HIGH,T1%R(1),T1%R(2)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
! RESUBSTITUTE
!-----------------------------------------------------------------------
!
      DO I = 1, NSICLA
        DO K = 1, NEWPRO_MAX
          PRO_D(J,K,I) = PRO_DNEW(K,I)
          PRO_F(J,K,I) = PRO_FNEW(K,I)
        ENDDO
      ENDDO
!
      PRO_MAX(J) = NEWPRO_MAX
!
      IF(PRO_MAX(J).LE.2) THEN
        WRITE(LU,*) ' COMPRESSBRUT: NOT ENOUGH PRO_MAX '
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(CVSP_DB_GAIA(JG,0)) CALL CVSP_P_GAIA('./','BRUE',JG)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(PRO_DNEW)
      DEALLOCATE(PRO_FNEW)
!
!-----------------------------------------------------------------------
! REMOVES NUMERIC INSTABILITIES
!-----------------------------------------------------------------------
!
      CALL CVSP_CHECK_STEADY_GAIA(J)
      RET = .TRUE.
      DO K = 1, PRO_MAX(J)
        RET = CVSP_CHECK_F_GAIA(J,K,'AFTERBRUT:   ')
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_COMPRESS_BRUT_GAIA
