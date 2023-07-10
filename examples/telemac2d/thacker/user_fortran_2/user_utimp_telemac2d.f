!                   *******************************
                    SUBROUTINE USER_UTIMP_TELEMAC2D
!                   *******************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    WRITES OUT ADDITIONAL OUTPUT REQUIRED BY THE USER.
!
!note     THIS SUBROUTINE IS CALLED IN THE SAME PLACES AS THE
!+                MAIN TELEMAC2D OUTPUT SUBROUTINE (NAMED DESIMP),
!+                I.E. CALLED TWICE:
!+
!note   (1) ONCE PER RUN, WHEN LTL==0, INDEPENDENTLY OF WHETHER
!+             'OUTPUT OF INITIAL CONDITIONS : YES' IS SET OR NOT
!note   (2) EACH TIME STEP JUST AFTER DESIMP-OUTPUT
!
!history  JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!+        **/08/2003
!+        V5P4
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| GRADEBL        |-->| FIRST TIME STEP FOR GRAPHIC OUTPUTS
!| GRAPRDL        |-->| PERIOD OF GRAPHIC OUTPUTS
!| LISDEBL        |-->| FIRST TIME STEP FOR LISTING OUTPUTS
!| LISPRDL        |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!     CUSTOM PRINTOUT PERIOD
      LOGICAL USE_CUSTOM_LEOPRD
      INTEGER CUSTOM_LEOPRD
      PARAMETER (USE_CUSTOM_LEOPRD=.TRUE.)
      INTEGER FID
!
!***********************************************************************
!
      TYPE(BIEF_OBJ) ECLOC, EPLOC, ETA
      TYPE(BIEF_OBJ) UM, U_2, V_2, UV
      TYPE(BIEF_OBJ) SVIDE
      TYPE(BIEF_OBJ) ONES, MASSM
!
      DOUBLE PRECISION EPTOT, ECTOT,ETOT
      DOUBLE PRECISION MASSBALANCE, MASSTOT
!
      DOUBLE PRECISION ERRLINF_H, ERRLINF_U, ERRLINF_V
      DOUBLE PRECISION ERRL1_H, ERRL1_U, ERRL1_V
      DOUBLE PRECISION ERRL2_H, ERRL2_U, ERRL2_V
!
      DOUBLE PRECISION ERRLINF_HU, ERRLINF_HV
      DOUBLE PRECISION ERRL1_HU, ERRL1_HV
      DOUBLE PRECISION ERRL2_HU, ERRL2_HV
!
!-----------------------------------------------------------------------
!
!     TABLE ALLOCATION
      ALLOCATE(ETA%R(NPOIN))
      ALLOCATE(EPLOC%R(NPOIN))
      ALLOCATE(ECLOC%R(NPOIN))
      ALLOCATE(UM%R(NPOIN))
      ALLOCATE(U_2%R(NPOIN))
      ALLOCATE(V_2%R(NPOIN))
      ALLOCATE(UV%R(NPOIN))
      ALLOCATE(SVIDE%R(NPOIN))
      ALLOCATE(ONES%R(NPOIN))
      ALLOCATE(MASSM%R(NPOIN))
!
      CALL BIEF_ALLVEC(1, ETA,'ETA     ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, EPLOC,'EPLOC   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, ECLOC,'ECLOC   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, UM,'UM      ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, U_2,'U_2     ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, V_2,'V_2     ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, UV,'UV     ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, SVIDE,'SVIDE   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, ONES,'ONES   ',IELMH, 1,1,MESH)
      CALL BIEF_ALLVEC(1, MASSM,'MASSM   ',IELMH, 1,1,MESH)
!
      ETA%R(:) = 0.D0
      EPLOC%R(:) = 0.D0
      ECLOC%R(:) = 0.D0
      UM%R(:) = 0.D0
      U_2%R(:) = 0.D0
      V_2%R(:) = 0.D0
      UV%R(:) = 0.D0
      SVIDE%R(:) = 0.D0
      ONES%R(:) = 1.D0
      MASSM%R(:) = 0.D0
!
      ETA%TYPE = 2
      EPLOC%TYPE = 2
      ECLOC%TYPE = 2
      UM%TYPE = 2
      U_2%TYPE = 2
      V_2%TYPE = 2
      UV%TYPE = 2
      SVIDE%TYPE = 2
      ONES%TYPE = 2
      MASSM%TYPE = 2
!
!-----------------------------------------------------------------------
!
!     DEFINING CUSTOM PRINTOUT PERIOD
      IF(USE_CUSTOM_LEOPRD) THEN
        CUSTOM_LEOPRD=1
      ELSE
        CUSTOM_LEOPRD=LEOPRD
      ENDIF
!
!-----------------------------------------------------------------------
! ENERGY BALANCE
!-----------------------------------------------------------------------
!
      IF(MOD(LT,CUSTOM_LEOPRD).EQ.0) THEN
!
! POTENTIAL ENERGY
!
!       EP :  0.5*g* int_S H**2 PSII PSJ dS
        CALL OS('X=YZ    ' , X=ETA , Y=H  , Z=H)
        CALL VECTOR(EPLOC,'=     ','MASVEC          ',
     &        IELMH,
     &        4.905D0,ETA,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &        MESH,MSK,MASKEL)
!
        EPTOT = 0.D0
        DO I=1,NPOIN
          EPTOT = EPTOT + EPLOC%R(I)
        ENDDO
!
! LOCAL KINETIC ENERGY
!
!       EC : 0.5*int_S H*U^2 PSII PSJ dS
        CALL OS('X=YZ    ' , X=U_2 , Y=U  , Z=U )
        CALL OS('X=YZ    ' , X=V_2 , Y=V  , Z=V )
        CALL OS('X=Y+Z    ' , X=UV , Y=U_2  , Z=V_2  )
        CALL OS('X=YZ     ' , X=UM , Y=UV  , Z=H  )

        CALL VECTOR(ECLOC,'=     ','MASVEC          ',
     &        IELMH,
     &        0.5D0,UM,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &        MESH,MSK,MASKEL)
!
! TOTAL KINETIC ENERGY
!
        ECTOT = 0.D0
        DO I=1,NPOIN
          ECTOT = ECTOT + ECLOC%R(I)
        ENDDO
!
! TOTAL ENERGY BALANCE
!
        IF(NCSIZE.GT.1) THEN
          EPTOT = P_SUM(EPTOT)
          ECTOT = P_SUM(ECTOT)
        ENDIF
!
        ETOT = EPTOT+ECTOT
!
! WRITE ENERGY BALANCE IN TXT FILE
!
!       IF PARALLEL ONLY WRITE WITH FIRST NODE
        IF(IPID.EQ.0) THEN
          FID = T2D_FILES(T2DRF1)%LU
          WRITE(FID,1004) AT, ECTOT, EPTOT, ETOT
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! MASS BALANCE
!-----------------------------------------------------------------------
!
! WRITE MASS BALANCE IN TXT FILE
!
      IF(MOD(LT,CUSTOM_LEOPRD).EQ.0) THEN
        IF(IPID.EQ.0) THEN
          FID = T2D_FILES(T2DRF2)%LU
!         WRITE CURRENT MASS AND MASS BALANCE
          MASSBALANCE = MASSE0+MASSET+MASENT-MASSE2
          WRITE(FID,*) AT, MASSE2, MASSBALANCE
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! MASS MATRIX AT FINAL TIME
!-----------------------------------------------------------------------
!
!     COMPUTE MASS MATRIX
      IF(MOD(LT,CUSTOM_LEOPRD).EQ.0) THEN
!
        CALL VECTOR(MASSM,'=     ','MASVEC          ',
     &        IELMH,
     &        1.D0,ONES,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &        MESH,MSK,MASKEL)
!
!       COMPUTE TOTAL MASS
        MASSTOT=0.D0
        DO I=1,MESH%NPOIN
          MASSTOT = MASSTOT + MASSM%R(I)
        ENDDO
      ENDIF
!
!     WRITE MASS AT FINAL TIME
      IF(LT.EQ.NIT) THEN
        IF(IPID.EQ.0) THEN
          FID = T2D_FILES(T2DRF3)%LU
          DO I=1,MESH%NPOIN
            WRITE(FID,*) MASSM%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! ERRORS TIME SERIES (WITH RESPECT TO ANALYTIC SOLUTION)
!-----------------------------------------------------------------------
!
!     COMPUTE ERRORS:
!       ERROR Linf : sup( ABS(REF - CALC) )
!       ERROR L1 : sum_S (  int_S ABS(REF - CALC) PSII PSJ dS )
!       ERROR L2 : sum_S ( (int_S ABS(REF - CALC)^2 PSII PSJ dS)^(1/2) )
!
!     VARIABLE NAMES:
!       PRIVE1(I) -> H
!       PRIVE2(I) -> U
!       PRIVE3(I) -> V
!       PRIVE4(I) -> ETA
!
      IF(MOD(LT,CUSTOM_LEOPRD).EQ.0) THEN
!
        ERRLINF_H = 0.D0
        ERRLINF_U = 0.D0
        ERRLINF_V = 0.D0
        ERRLINF_HU = 0.D0
        ERRLINF_HV = 0.D0
!
        ERRL1_H = 0.D0
        ERRL1_U = 0.D0
        ERRL1_V = 0.D0
        ERRL1_HU = 0.D0
        ERRL1_HV = 0.D0
!
        ERRL2_H = 0.D0
        ERRL2_U = 0.D0
        ERRL2_V = 0.D0
        ERRL2_HU = 0.D0
        ERRL2_HV = 0.D0
!
        DO I=1,NPOIN
          ERRLINF_H = MAX(ERRLINF_H, ABS(PRIVE1(I)-H%R(I)))
          ERRLINF_U = MAX(ERRLINF_U, ABS(PRIVE2(I)-U%R(I)))
          ERRLINF_V = MAX(ERRLINF_V, ABS(PRIVE3(I)-V%R(I)))
          ERRLINF_HU = MAX(ERRLINF_HU, ABS(PRIVE1(I)*PRIVE2(I)
     &               - H%R(I)*U%R(I)))
          ERRLINF_HV = MAX(ERRLINF_HV, ABS(PRIVE1(I)*PRIVE3(I)
     &               - H%R(I)*V%R(I)))
!
          ERRL1_H = ERRL1_H + MASSM%R(I)*ABS(PRIVE1(I)-H%R(I))
          ERRL1_U = ERRL1_U + MASSM%R(I)*ABS(PRIVE2(I)-U%R(I))
          ERRL1_V = ERRL1_V + MASSM%R(I)*ABS(PRIVE3(I)-V%R(I))
          ERRL1_HU = ERRL1_HU + MASSM%R(I)*ABS(PRIVE1(I)*PRIVE2(I)
     &             - H%R(I)*U%R(I))
          ERRL1_HV = ERRL1_HV + MASSM%R(I)*ABS(PRIVE1(I)*PRIVE3(I)
     &             - H%R(I)*V%R(I))
!
          ERRL2_H = ERRL2_H + MASSM%R(I)*(ABS(PRIVE1(I)-H%R(I))**2)
          ERRL2_U = ERRL2_U + MASSM%R(I)*(ABS(PRIVE2(I)-U%R(I))**2)
          ERRL2_V = ERRL2_V + MASSM%R(I)*(ABS(PRIVE3(I)-V%R(I))**2)
          ERRL2_HU = ERRL2_HU + MASSM%R(I)*(ABS(PRIVE1(I)*PRIVE2(I)
     &             - H%R(I)*U%R(I))**2)
          ERRL2_HV = ERRL2_HV + MASSM%R(I)*(ABS(PRIVE1(I)*PRIVE3(I)
     &             - H%R(I)*V%R(I))**2)
        ENDDO
!
!       IF PARALLEL, SUM ON ALL PARTITIONS
        IF(NCSIZE.GT.1) THEN
          ERRLINF_H = P_MAX(ERRLINF_H)
          ERRLINF_U = P_MAX(ERRLINF_U)
          ERRLINF_V = P_MAX(ERRLINF_V)
          ERRLINF_HU = P_MAX(ERRLINF_HU)
          ERRLINF_HV = P_MAX(ERRLINF_HV)
!
          ERRL1_H = P_SUM(ERRL1_H)
          ERRL1_U = P_SUM(ERRL1_U)
          ERRL1_V = P_SUM(ERRL1_V)
          ERRL1_HU = P_SUM(ERRL1_HU)
          ERRL1_HV = P_SUM(ERRL1_HV)
!
          ERRL2_H = P_SUM(ERRL2_H)
          ERRL2_U = P_SUM(ERRL2_U)
          ERRL2_V = P_SUM(ERRL2_V)
          ERRL2_HU = P_SUM(ERRL2_HU)
          ERRL2_HV = P_SUM(ERRL2_HV)
!
          MASSTOT = P_SUM(MASSTOT)
        ENDIF
!
        ERRL1_H = ERRL1_H/MASSTOT
        ERRL1_U = ERRL1_U/MASSTOT
        ERRL1_V = ERRL1_V/MASSTOT
        ERRL1_HU = ERRL1_HU/MASSTOT
        ERRL1_HV = ERRL1_HV/MASSTOT
!
        ERRL2_H = SQRT(ERRL2_H/MASSTOT)
        ERRL2_U = SQRT(ERRL2_U/MASSTOT)
        ERRL2_V = SQRT(ERRL2_V/MASSTOT)
        ERRL2_HU = SQRT(ERRL2_HU/MASSTOT)
        ERRL2_HV = SQRT(ERRL2_HV/MASSTOT)
!
!       WRITE LINF ERRORS
        IF(IPID.EQ.0) THEN
          FID = T2D_FILES(T2DRF4)%LU
!         WRITE LINF ERRORS
          WRITE(FID,1005) AT, ERRLINF_H, ERRLINF_U, ERRLINF_V,
     &                     ERRLINF_HU, ERRLINF_HV
        ENDIF
!
!
!       WRITE L1 ERRORS
!       IF PARALLEL ONLY WRITE WITH FIRST NODE
        IF(IPID.EQ.0) THEN
          FID = T2D_FILES(T2DRF5)%LU
!         WRITE LINF ERRORS
          WRITE(FID,1005) AT, ERRL1_H, ERRL1_U, ERRL1_V,
     &                     ERRL1_HU, ERRL1_HV
        ENDIF
!
!
!       WRITE L2 ERRORS
!       IF PARALLEL ONLY WRITE WITH FIRST NODE
        IF(IPID.EQ.0) THEN
          FID = T2D_FILES(T2DRF6)%LU
!         WRITE LINF ERRORS
          WRITE(FID,1005) AT, ERRL2_H, ERRL2_U, ERRL2_V,
     &                     ERRL2_HU, ERRL2_HV
        ENDIF
!
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TABLE DEALLOCATION
      DEALLOCATE(ETA%R)
      DEALLOCATE(EPLOC%R)
      DEALLOCATE(ECLOC%R)
      DEALLOCATE(UM%R)
      DEALLOCATE(U_2%R)
      DEALLOCATE(V_2%R)
      DEALLOCATE(UV%R)
      DEALLOCATE(SVIDE%R)
      DEALLOCATE(MASSM%R)
      DEALLOCATE(ONES%R)

      CALL BIEF_DEALLVEC(ETA)
      CALL BIEF_DEALLVEC(EPLOC)
      CALL BIEF_DEALLVEC(ECLOC)
      CALL BIEF_DEALLVEC(UM)
      CALL BIEF_DEALLVEC(U_2)
      CALL BIEF_DEALLVEC(V_2)
      CALL BIEF_DEALLVEC(UV)
      CALL BIEF_DEALLVEC(SVIDE)
      CALL BIEF_DEALLVEC(MASSM)
      CALL BIEF_DEALLVEC(ONES)
!
 1001 FORMAT((A,I3,A,F6.2,A))
 1002 FORMAT((A,F7.1,A))
 1003 FORMAT((A,I5,A,E25.17,A))
 1004 FORMAT((E15.6,E25.10,E25.10,E25.10))
 1005 FORMAT((E15.6,6E25.10))
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
