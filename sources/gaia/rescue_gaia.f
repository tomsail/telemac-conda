!                   **********************
                    SUBROUTINE RESCUE_GAIA
!                   **********************
!
     &(H,S,ZF,ZR,ES,HW,TW,THETAW,NPOIN,NOMBLAY,
     & TROUVE,ALIRE,PASS,ICF,LISTI,MAXVAR)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes missing data/variables for hydrodynamic
!!       and/or sedimentological continuation run.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     ALIRE  List variables to be read
!>@param[in,out] H      Water depth
!>@param[in,out] HW     Wave depth
!>@param[in]     ICF    Bed-load or total load transport formulas
!>@param[in]     LISTI  Logical, if yes print messages
!>@param[in]     MAXVAR Maximum number of output variables
!>@param[in]     NPOIN  Number of mesh nodes
!>@param[in]     PASS   Logical, if yes begin of computation
!>@param[in,out] S      Water surface elevation
!>@param[in,out] THETAW Wave direction (deg wrt ox axis)
!>@param[in]     TROUVE True if the variable i was found in the
!!                      subroutine suite
!>@param[in,out] TW     Wave period
!>@param[in,out] ZF     Bed level
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE INTERFACE_GAIA, EX_RESCUE_GAIA
     &           => RESCUE_GAIA
!
      USE DECLARATIONS_GAIA, ONLY : DEBU_MASS,NSAND,NMUD,NVAR_LAYTHI,
     & NVAR_LAYTHI,NVAR_MASS_M,NVAR_MASS_S,NVAR_RATIOM,NVAR_RATIOS,
     & NVAR_LAYCONC,NVAR_MTRANS,NVAR_TOCEMUD,NVAR_PARTHE,BED_MODEL,
     & CONC_MUD_FOUND,TOCE_MUD_FOUND,PARTHENIADES_FOUND,MTRANS_FOUND,
     & MASS_S, MASS_M, RATIOS, RATIOM, MASS_SAND, MASS_MUD, RATIO_SAND,
     & RATIO_MUD, TOCE_MUD, TOCEMUD, PARTHE, PARTHENIADES, MTRANSFER,
     & TRANS_MASS, LAYCONC, CONC_MUD
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: MAXVAR,NOMBLAY
      INTEGER, INTENT(IN) :: ALIRE(MAXVAR),NPOIN,ICF
      LOGICAL, INTENT(IN) :: PASS,LISTI
      DOUBLE PRECISION, INTENT(IN) :: ES(NPOIN,NOMBLAY)
!
      INTEGER, INTENT(INOUT) :: TROUVE(MAXVAR)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN) , ZF(NPOIN), H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZR(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HW(NPOIN), TW(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: THETAW(NPOIN)
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,I,J
      INTEGER CHECK_ES,CHECK_RSNL,CHECK_RMNL
      INTEGER CHECK_NSNL,CHECK_NMNL
      INTEGER CHECK_CONC,CHECK_MTRANS,CHECK_TOCEMUD,CHECK_PARTHENIADES
!
      CONC_MUD_FOUND = .FALSE.
      TOCE_MUD_FOUND = .FALSE.
      PARTHENIADES_FOUND = .FALSE.
      MTRANS_FOUND   = .FALSE.
!-----------------------------------------------------------------------
!
! PRINTOUTS :
! -----------
      IF(PASS.AND.LISTI) THEN
        WRITE(LU,200)
200     FORMAT(80('-'))
        IF(ALIRE(8).EQ.1) THEN
          WRITE(LU,301)
301       FORMAT(1X,'RESCUE : HYDRODYNAMIC FILE')
        ELSE
          WRITE(LU,311)
311       FORMAT(1X,'RESCUE : SEDIMENTOLOGICAL FILE')
        ENDIF
      ENDIF
!
! ------------------------------------------------------------------
!  WATER DEPTH :
!  -------------
      IF((ALIRE(3).EQ.1).AND.(TROUVE(3).NE.1)) THEN
        IF(TROUVE(4).EQ.1.AND.TROUVE(5).EQ.1) THEN
          IF (LISTI) THEN
            WRITE(LU,401)
          ENDIF
          CALL OV('X=Y-Z   ', X=H, Y=S, Z=ZF, DIM1=NPOIN)
        ELSE
          IF (LISTI) THEN
            WRITE(LU,421)
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
401       FORMAT(1X,'WATER DEPTH COMPUTED WITH BATHYMETRY',
     &         /,1X,'AND SURFACE ELEVATION')
421       FORMAT(1X,'WATER DEPTH UNABLE TO BE COMPUTED')
!
! ----------------------------------------------------------------------
!
! CLIPS NEGATIVE WATER DEPTHS :
! -------------------------------------
!
      DO K = 1,NPOIN
        H(K) = MAX(H(K),0.D0)
      ENDDO
!
!------------------------------------------------------------------------
!
!  WAVE HEIGHT AND PERIOD
!
      IF(ICF==4.OR.ICF==5.OR.ICF==8.OR.ICF==9) THEN
!
        IF(ALIRE(12).EQ.1.AND.TROUVE(12).EQ.0) THEN
          WRITE(LU,901)
          CALL OV('X=C     ', X=HW, C=0.D0, DIM1=NPOIN)
        ENDIF
!
901     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE HEIGHT : IT IS',
     &          ' FIXED TO ZERO')
!
        IF(ALIRE(13).EQ.1.AND.TROUVE(13).EQ.0) THEN
          WRITE(LU,903)
          CALL OV('X=C     ', X=TW, C=0.D0, DIM1=NPOIN)
        ENDIF
903     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE PERIOD : IT IS',
     &          ' FIXED TO ZERO')
!
        IF(ALIRE(14).EQ.1.AND.TROUVE(14).EQ.0) THEN
          WRITE(LU,909)
          CALL OV('X=C     ', X=THETAW, C=0.D0, DIM1=NPOIN)
        ENDIF
909     FORMAT(1X,'PREVIOUS COMPUTATION WITHOUT WAVE ANGLE : IT IS',
     &          ' FIXED TO ZERO')
      ENDIF
!
!-----------------------------------------------------------------------
!  NON-ERODABLE BED
!
      IF(ALIRE(9).EQ.1.AND.TROUVE(9).EQ.0) THEN
        WRITE(LU,908)
        CHECK_ES=0
        DO I = 1,NOMBLAY
          IF(TROUVE(5).EQ.1.AND.
     &       TROUVE(NVAR_LAYTHI+I).EQ.1) THEN
            CHECK_ES=CHECK_ES+1
          ENDIF
        ENDDO
        IF(CHECK_ES.EQ.NOMBLAY) THEN
          WRITE(LU,910)
          DO I = 1,NPOIN
            ZR(I)=ZF(I)
            DO J = 1,NOMBLAY
              ZR(I)=ZR(I)-ES(I,J)
            ENDDO
          ENDDO
          TROUVE(9) = 1
        ENDIF
      ENDIF
908   FORMAT(1X,'PREVIOUS CALCULATION WITHOUT NON ERODABLE',
     &         /,1X,'BOTTOM')
910   FORMAT(1X,'PREVIOUS CALCULATION CONTAINS ALL LAYERS',
     &         /,1X,'RIGID BED COMPUTED FROM LAYERS THICKNESS',
     &         /,1X,'AND BOTTOM')
!
!-----------------------------------------------------------------------
!  BED ELEVATION
!
      IF(ALIRE(5).EQ.1.AND.TROUVE(5).EQ.0) THEN
!
        IF(TROUVE(4).EQ.1.AND.TROUVE(3).EQ.1) THEN
          IF (LISTI) THEN
          WRITE(LU,411)
411       FORMAT(1X,'BATHYMETRY COMPUTED FROM WATER DEPTH',
     &         /,1X,'AND SURFACE ELEVATION')
          ENDIF
          CALL OV('X=Y-Z   ', X=ZF, Y=S, Z=H, DIM1=NPOIN)
        ELSE
          CALL OV('X=C     ', X=ZF, C=0.D0, DIM1=NPOIN)
          WRITE(LU,961)
        ENDIF
!
      ENDIF
961     FORMAT(1X,'BOTTOM TOPOGRAPHY NOT FOUND',/,
     &            'IT IS SET TO ZERO')
!
! --------------------------------------------------------------------
!     OPTIONS TO COMPUTE MASSES
! --------------------------------------------------------------------
      CHECK_NSNL=0
      DO I = 1,NSAND
        DO J=1,NOMBLAY
          IF(TROUVE(NVAR_MASS_S+(I-1)*NOMBLAY+J).EQ.1) THEN
            ! Updating mass_sand with what was read in file
            MASS_SAND(I,J,1:NPOIN) = MASS_S%ADR(J+(I-1)*NOMBLAY)%P%R
            CHECK_NSNL=CHECK_NSNL+1
          ENDIF
        ENDDO
      ENDDO
      CHECK_NMNL=0
      DO I = 1,NMUD
        DO J=1,NOMBLAY
          IF(TROUVE(NVAR_MASS_M+(I-1)*NOMBLAY+J).EQ.1) THEN
            ! Updating mass_mud with what was read in file
            MASS_MUD(I,J,1:NPOIN) = MASS_M%ADR(J+(I-1)*NOMBLAY)%P%R
            CHECK_NMNL=CHECK_NMNL+1
          ENDIF
        ENDDO
      ENDDO
      IF(CHECK_NSNL.EQ.NSAND*NOMBLAY.AND.CHECK_NMNL.EQ.
     &     NMUD*NOMBLAY) THEN
!       MASSES FROM PREVIOUS SEDIMENTOLOGICAL FILE
        DEBU_MASS=.TRUE.
        WRITE(LU,981)
      ELSE
        CHECK_RSNL=0
        DO I = 1,NSAND
          DO J=1,NOMBLAY
            IF(TROUVE(NVAR_RATIOS+(I-1)*NOMBLAY+J).EQ.1) THEN
              ! Updating ratio_sand with what was read in file
              RATIO_SAND(I,J,1:NPOIN) = RATIOS%ADR(J+(I-1)*NOMBLAY)%P%R
              CHECK_RSNL=CHECK_RSNL+1
            ENDIF
          ENDDO
        ENDDO
        CHECK_RMNL=0
        DO I = 1,NMUD
          DO J=1,NOMBLAY
            IF(TROUVE(NVAR_RATIOM+(I-1)*NOMBLAY+J).EQ.1) THEN
              ! Updating ratio_mud with what was read in file
              RATIO_MUD(I,J,1:NPOIN) = RATIOM%ADR(J+(I-1)*NOMBLAY)%P%R
              CHECK_RMNL=CHECK_RMNL+1
            ENDIF
          ENDDO
        ENDDO
        CHECK_ES=0
        DO I=1,NOMBLAY
          IF(TROUVE(NVAR_LAYTHI+I).EQ.1) THEN
            CHECK_ES=CHECK_ES+1
          ENDIF
        ENDDO
        IF(CHECK_RSNL.EQ.NSAND*NOMBLAY.AND.CHECK_RMNL.EQ.NMUD*NOMBLAY
     &     .AND.CHECK_ES.EQ.NOMBLAY) THEN
!         MASSES HAVE TO BE COMPUTED FROM OTHER PARAMETERS:
!         RATIO_S,RATIO_M,ES
!         XKV READ IN THE STEERING FILE
          DEBU_MASS=.FALSE.
          WRITE(LU,*)'MASSES COMPUTED USING RATIOS,POROSITY AND '//
     &     'THICKNESS RETREIVED IN THE PREVIOUS FILE'
        ELSE
!         NOT ENOUGH DATA TO RESTART COMPUTATION
          WRITE(LU,1111)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
981   FORMAT(1X,'MASS RETRIEVED FROM THE PREVIOUS SEDIMENTOLOGICAL',
     &      /,1X,'FILE')
1111  FORMAT(1X,'ERROR: COMPUTATION CANNOT RESTART SINCE MASSES OR',
     &       /,1X, 'RATIOS OR THICKNESS OR POROSITY ',
     &       /,1X, 'ARE MISSING IN THE PREVIOUS SEDIMENTOLOGICAL FILE')
!
!     CHECK ALL THE NECESSARY VARIABLES FOR SUSPENSION ARE HERE
!
      IF (NMUD.NE.0) THEN
        CHECK_CONC=0
        CHECK_TOCEMUD=0
        CHECK_PARTHENIADES=0
        DO I=1,NOMBLAY
          IF(TROUVE(NVAR_LAYCONC+I).EQ.1) THEN
            ! Updating conc_mud with what was read in file
            CONC_MUD(I,1:NPOIN)=LAYCONC%ADR(I)%P%R
            CHECK_CONC=CHECK_CONC+1
          ENDIF
          IF(TROUVE(NVAR_TOCEMUD+I).EQ.1) THEN
            ! Updating toce_mud with what was read in file
            TOCE_MUD(I,1:NPOIN) = TOCEMUD%ADR(I)%P%R
            CHECK_TOCEMUD=CHECK_TOCEMUD+1
          ENDIF
          IF(TROUVE(NVAR_PARTHE+I).EQ.1) THEN
            ! Updating partheniades with what was read in file
            PARTHENIADES(I,1:NPOIN) = PARTHE%ADR(I)%P%R
            CHECK_PARTHENIADES=CHECK_PARTHENIADES+1
          ENDIF
        ENDDO
        IF(CHECK_CONC.EQ.NOMBLAY) THEN
          CONC_MUD_FOUND = .TRUE.
          WRITE(LU,*)'MUD CONCENTRATION READ FROM PREVIOUS FILE'
        ENDIF
        IF(CHECK_TOCEMUD.EQ.NOMBLAY) THEN
          TOCE_MUD_FOUND = .TRUE.
          WRITE(LU,*)'MUD TOCE READ FROM PREVIOUS FILE'
        ENDIF
        IF(CHECK_PARTHENIADES.EQ.NOMBLAY) THEN
          PARTHENIADES_FOUND = .TRUE.
          WRITE(LU,*)'PARTHENIADES READ FROM PREVIOUS FILE'
        ENDIF
        IF (BED_MODEL.EQ.2) THEN
          CHECK_MTRANS=0
          DO I=1,NOMBLAY
            IF(TROUVE(NVAR_MTRANS+1).EQ.1) THEN
              ! Updating mtransfer with what was read in file
              MTRANSFER%ADR(I)%P%R=TRANS_MASS(I,1:NPOIN)
              CHECK_MTRANS=CHECK_MTRANS+1
              WRITE(LU,*)'MASS TRANSFER READ FROM PREVIOUS FILE'
            ENDIF
          ENDDO
          IF(CHECK_MTRANS.EQ.NOMBLAY) THEN
            MTRANS_FOUND = .TRUE.
          ENDIF
        ENDIF
      ENDIF
!
      IF (PASS.AND.LISTI) THEN
        WRITE(LU,970)
970     FORMAT(80('-'))
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE RESCUE_GAIA
