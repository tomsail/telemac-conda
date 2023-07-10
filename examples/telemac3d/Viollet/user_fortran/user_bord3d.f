!                   **********************
                    SUBROUTINE USER_BORD3D
!                   **********************
!
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    SPECIFIC BOUNDARY CONDITIONS NOT IMPLEMENTED IN USUAL BORD3D.
!
!warning  MAY BE MODIFIED BY THE USER
!
!history  C.-T. PHAM (LNHE)
!+        04/04/2017
!+        V7P3
!+   Creation from BORD3D and 4 examples of TELEMAC-3D:
!+   stratification, tetra, NonLinearWave and Viollet
!+   Prescribed stratification over the vertical along a liquid boundary
!+   for stratification and tetra cases
!+   Specific boundary conditions for H, U, V and P for lateral
!+   boundaries for NonLinearWave case
!+   Specific boundary conditions for U, V, temp, epsilon for Viollet
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE INTERFACE_TELEMAC3D
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     SPECIFIC DECLARATIONS FOR TETRA OR STRATIFICATION CASE
!
!      INTEGER NP,IBORD,K
!
!-----------------------------------------------------------------------
!
!     SPECIFIC DECLARATIONS FOR NONLINEARWAVE CASE
!
!      INTEGER IBORD,IPLAN,I
!      DOUBLE PRECISION PI,PER,L,HEIGHT,DEPTH,KK,OMEGA,ZZ,ZSURF,TPS
!
!-----------------------------------------------------------------------
!
!     SPECIFIC DECLARATIONS FOR VIOLLET CASE
!
!      INTEGER IPOIN2,NP,IBORD,IFRLIQ,K,I3D,MSK1,IJK,NFO1
!      INTEGER YADEB(MAXFRO)
!      DOUBLE PRECISION DZ,Z_MF,UET1,UET2,H_MF,S_MF1,S_MF2,XI_S,U_MF1,
!     &                 U_MF2
!      DOUBLE PRECISION FROUD,TEMP0,TEMP1
!!
      INTEGER IPOIN2,NP,IBORD,IFRLIQ,K,I3D,MSK1,IJK,NFO1
      INTEGER YADEB(MAXFRO)
      DOUBLE PRECISION DZ,Z_MF,UET1,UET2,H_MF,S_MF1,S_MF2,XI_S,U_MF1,
     &                 U_MF2
      DOUBLE PRECISION FROUD,TEMP0,TEMP1
!
!
!-----------------------------------------------------------------------
!
!     BEGIN OF PART SPECIFIC TO TETRA OR STRATIFICATION CASE
!
!      DO K=1,NPTFR2
!        DO NP=1,NPLAN
!          IBORD = (NP-1)*NPTFR2+K
!          IF(LITABL%ADR(1)%P%I(IBORD).EQ.KENT) THEN
!! BEGINNING OF SPECIFIC TO TETRA CASE
!!            IF(NP.LE.4) THEN
!!              TABORL%ADR(1)%P%R(IBORD) = 40.D0
!!            ELSE
!!              TABORL%ADR(1)%P%R(IBORD) = 30.D0
!!            ENDIF
!! END OF SPECIFIC TO TETRA CASE
!!
!! BEGINNING OF SPECIFIC TO STRATIFICATION CASE
!!           STRATIFICATION PUT AT THE ENTRANCE
!!            IF(NP.GT.18) THEN
!!              TABORL%ADR(1)%P%R(IBORD) = 28.D0
!!            ENDIF
!! END OF SPECIFIC TO STRATIFICATION CASE
!          ENDIF
!        ENDDO
!      ENDDO
!
!     END OF PART SPECIFIC TO TETRA OR STRATIFICATION CASE
!
!-----------------------------------------------------------------------
!
!     BEGIN OF PART SPECIFIC TO NONLINEARWAVE CASE
!
!      PI=4.D0*ATAN(1.D0)
!      PER=1.01D0
!      HEIGHT=0.041D0
!      DEPTH=0.43D0
!      OMEGA=2.D0*PI/PER
!      L=1.D0
!      DO I=1,100
!        L=GRAV*PER**2/2.D0/PI*TANH(2.D0*PI*DEPTH/L)
!      ENDDO
!      KK=2.D0*PI/L
!!     TPS=AT+PER/2.D0
!      TPS=AT
!!
!      DO I=1,NPTFR2
!!
!        IF(LIHBOR%I(I).EQ.KENT.OR.LIUBOL%I(I).EQ.KENTU) THEN
!!
!          HBOR%R(I)=-ZF%R(NBOR2%I(I))
!     &             +(HEIGHT*COS(OMEGA*TPS)/2.D0
!     &    +(KK*HEIGHT**2/16.D0)*(COSH(KK*DEPTH)/SINH(KK*DEPTH)**3)
!     &    *(2.D0+COSH(2.D0*KK*DEPTH))*COS(2.D0*OMEGA*TPS))*
!!         RAMPE DE 1 S SUR LE TEMPS
!     &    MIN(1.D0,AT)
!!
!          DO  IPLAN=1, NPLAN
!            IBORD = (IPLAN-1)*NPTFR2 + I
!            ZSURF=Z(NBOR3%I((NPLAN-1)*NPTFR2 + I))
!            ZZ=Z(NBOR3%I(IBORD))-ZSURF
!            UBORL%R(IBORD)=OMEGA*COS(OMEGA*TPS)*HEIGHT/
!     &             2.D0*COSH(KK*(ZZ+DEPTH))/SINH(KK*DEPTH)
!     & +3.D0/16.D0*OMEGA*KK*HEIGHT**2*COSH(2.D0*KK*(ZZ+DEPTH))/
!     &      SINH(KK*DEPTH)**4*COS(2.D0*OMEGA*TPS)
!            VBORL%R(IBORD)=0.D0
!            PBORL%R(IBORD)=DT*GRAV*HEIGHT*COS(OMEGA*TPS)/2.D0*
!     &                 (COSH(KK*(ZZ+DEPTH))/COSH(KK*DEPTH)-1.D0)
!!
!            UBORL%R(IBORD)=UBORL%R(IBORD)*MIN(1.D0,AT)
!            VBORL%R(IBORD)=VBORL%R(IBORD)*MIN(1.D0,AT)
!            PBORL%R(IBORD)=PBORL%R(IBORD)*MIN(1.D0,AT)
!          ENDDO
!        ENDIF
!      ENDDO
!
!     END OF PART SPECIFIC TO NONLINEARWAVE CASE
!
!-----------------------------------------------------------------------
!
!     BEGIN OF PART SPECIFIC TO VIOLLET CASE
!
!!     INITIALISES YADEB
!!
!      IF(NFRLIQ.GE.1) THEN
!        DO K=1,NFRLIQ
!          YADEB(K)=0
!        ENDDO
!      ENDIF
!!
!!     PRESCRIBED DISCHARGE GIVEN IN STEERING FILE (NDEBIT<>0)
!!     --------------------------------------------------------
!!
!      DO K=1,NPTFR2
!!
!!     A VELOCITY PROFILE IS SET HERE AND WILL BE CORRECTED LATER
!!     TO GET THE CORRECT DISCHARGE (CALL TO DEBIMP3D)
!!
!      IF(LIUBOL%I(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
!!
!!       INITIALISATION FOR PROFILE SPECIFICATION
!        S_MF1 = 0.D0
!        S_MF2 = 0.D0
!!       CONSTANT VALUES
!        H_MF  = 0.1D0
!        U_MF1 = 1.D0/30.D0
!        U_MF2 = 2.D0/30.D0
!        XI_S  = 0.0001D0
!        IPOIN2 = NBOR2%I(K)
!        DO NP=1,NPLAN
!          IJK=(NP-1)*NPTFR2+K
!          I3D=(NP-1)*NPOIN2+IPOIN2
!          IFRLIQ=NUMLIQ%I(K)
!!         CASE OF A VERTICAL PROFILE
!!         LOG PROFILE PRESCRIBED
!          IF(VERPROVEL(IFRLIQ).NE.1) THEN
!!            STEP PROFILE
!!            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
!!     &                       NP,INFOGR,VERPROVEL(IFRLIQ))
!            DZ=2.D0*H_MF/DBLE(NPLAN-1.D0)
!
!            IF(NP.GT.(NPLAN+1)/2) THEN
!              Z_MF=DBLE(NP-(NPLAN+1)/2)*DZ
!              S_MF1=S_MF1+LOG(Z_MF/XI_S)+8.5D0
!            ELSE
!              Z_MF=DBLE(NP-1.D0)*DZ
!              IF(NP.NE.1.AND.NP.NE.(NPLAN+1)/2) THEN
!                S_MF2 = S_MF2+LOG(MIN(Z_MF,H_MF-Z_MF)/XI_S)+8.5D0
!              ELSE
!                S_MF2 = S_MF2+LOG(DZ*0.1D0/XI_S)+8.5D0
!              ENDIF
!            ENDIF
!          ENDIF
!        ENDDO
!!
!!     FIXING UET1 AND UET2
!!
!        UET1=KARMAN*DBLE((NPLAN-1)/2)*U_MF1/S_MF1
!        UET2=KARMAN*DBLE((NPLAN-1)/2)*U_MF2/S_MF2
!!
!        DO NP=1,NPLAN
!          IJK=(NP-1)*NPTFR2+K
!          IF(NP.GT.(NPLAN+1)/2) THEN
!            Z_MF=DBLE(NP-(NPLAN+1)/2)*DZ
!            UBORL%R(IJK) = UET1/KARMAN*(LOG(Z_MF/XI_S)+8.5D0)
!          ELSEIF(NP.NE.1.AND.NP.NE.(NPLAN+1)/2) THEN
!            Z_MF=DBLE(NP-1.D0)*DZ
!            UBORL%R(IJK) = UET2/KARMAN*(LOG(MIN(Z_MF,H_MF-Z_MF)/XI_S)
!     &                                 +8.5D0)
!          ELSE
!            UBORL%R(IJK) = UET2/KARMAN*(LOG(DZ*0.1D0/XI_S)+8.5D0)
!          ENDIF
!          VBORL%R(IJK) = 0.D0
!!
!!          IF(VERPROVEL(IFRLIQ).NE.1) THEN
!!            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
!!     &                       NP,INFOGR,VERPROVEL(IFRLIQ))
!!            UBORL%R(IJK) = UBORL%R(IJK)*PROFZ
!!            VBORL%R(IJK) = VBORL%R(IJK)*PROFZ
!!         U AND V INITIALISED WITH PRESCRIBED VALUES (FOR DEBIMP3D)
!!         WILL BE CHANGED AGAIN AFTER DEBIMP3D
!          U%R(I3D)=UBORL%R(IJK)
!          V%R(I3D)=VBORL%R(IJK)
!        ENDDO
!        YADEB(NUMLIQ%I(K))=1
!      ENDIF
!!
!      ENDDO
!!
!!-----------------------------------------------------------------------
!!
!!     PRESCRIBED DISCHARGES: FINAL TREATMENT OF VELOCITIES
!!     ----------------------------------------------------
!!
!!     LOOP ON LIQUID BOUNDARIES
!!
!      IF(NFRLIQ.NE.0) THEN
!      DO IFRLIQ = 1 , NFRLIQ
!!
!      IF(NDEBIT.NE.0) THEN
!        MSK1=1
!        IF(NDEBIT.GE.IFRLIQ) THEN
!          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_MAX(YADEB(IFRLIQ))
!          IF(YADEB(IFRLIQ).EQ.1) THEN
!            CALL DEBIMP_3D(Q3(IFRLIQ,AT,INFOGR),
!     &                     UBORL%R,VBORL%R,
!     &                     U,V,NUMLIQ%I,NUMLIQ_ELM%I,IFRLIQ,T3_02,
!     &                     NPTFR2,NETAGE,MASK_3D%ADR(MSK1)%P,
!     &                     MESH3D,EQUA,IELM2V,SVIDE,MASKTR,
!     &                     MESH3D%NELEB)
!          ENDIF
!        ELSE
!          WRITE(LU,401) IFRLIQ
!401       FORMAT(1X,'USER_BORD3D: MORE PRESCRIBED FLOWRATES',/,
!     &           1X,'             ARE REQUIRED IN THE STEERING FILE',/,
!     &           1X,'             AT LEAST ',1I6,' MUST BE GIVEN')
!          CALL PLANTE(1)
!          STOP
!        ENDIF
!      ENDIF
!!
!      ENDDO ! IFRLIQ
!      ENDIF
!!
!!     RESETS BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
!!     AND OTHER SUBROUTINES BEFORE THE NEXT BOUNDARY CONDITIONS TREATMENT)
!!
!      DO K=1,NPTFR2
!        IF(LIUBOL%I(K).EQ.KENT) THEN
!          DO NP=1,NPLAN
!            IJK=(NP-1)*NPTFR2+K
!            U%R((NP-1)*NPOIN2+NBOR2%I(K))=UBORL%R(IJK)
!            V%R((NP-1)*NPOIN2+NBOR2%I(K))=VBORL%R(IJK)
!          ENDDO
!        ENDIF
!      ENDDO
!!
!!     EGR+BD MODIF TO AVOID TO ENTER IN FORTRAN FILE, TEMP0 =
!!     TEMPERATURE OF COFLOW DEPENDS ON FROUD
!!     TAKES THE VALUE DEFINED IN THE FILE FROUD.TXT
!      NFO1 = T3D_FILES(T3DFO1)%LU
!      REWIND NFO1
!      READ(NFO1,*) FROUD
!      TEMP0 = 20.D0
!      IF(ABS(FROUD-0.9D0).LT.1.D-5) THEN
!        TEMP1 = 25.3485028D0
!      ELSEIF (ABS(FROUD-1.6D0).LT.1.D-5) THEN
!        TEMP1 = 21.8663052D0
!      ELSEIF (ABS(FROUD-5.0D0).LT.1.D-5) THEN
!        TEMP1 = 20.2009931D0
!      ELSE
!        TEMP1 = 4.D0+SQRT((TEMP0-4.D0)**2
!     &                    +0.0333D0**2/(GRAV*7.D-6*0.1D0*FROUD**2))
!      ENDIF
!!
!      DO K=1,NPTFR2
!        DO NP=1,NPLAN
!          IBORD = (NP-1)*NPTFR2+K
!          IF(LITABL%ADR(1)%P%I(IBORD).EQ.KENT) THEN
!!           LINEAR STRATIFICATION AT THE ENTRANCE
!            IF(NP.GT.(NPLAN+1)/2) THEN
!              TABORL%ADR(1)%P%R(IBORD) = TEMP1
!            ENDIF
!          ENDIF
!        ENDDO
!      ENDDO
!!
!!     NEUMANN FOR EPS AT THE BOTTOM
!      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!        DO IPOIN2=1,NPOIN2
!          DZ=MESH3D%Z%R(IPOIN2+NPOIN2)-MESH3D%Z%R(IPOIN2)
!          BEBORF%R(IPOIN2) = 4.D0*UETCAR%R(IPOIN2)**1.5D0
!     &                           /KARMAN/DZ**2
!     &                           *VISCVI%ADR(3)%P%R(IPOIN2)
!          BEBORF%TYPR='Q'
!        ENDDO
!      ENDIF
!     INITIALISES YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
!     PRESCRIBED DISCHARGE GIVEN IN STEERING FILE (NDEBIT<>0)
!     --------------------------------------------------------
!
      DO K=1,NPTFR2
!
!     A VELOCITY PROFILE IS SET HERE AND WILL BE CORRECTED LATER
!     TO GET THE CORRECT DISCHARGE (CALL TO DEBIMP3D)
!
      IF(LIUBOL%I(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
!
!       INITIALISATION FOR PROFILE SPECIFICATION
        S_MF1 = 0.D0
        S_MF2 = 0.D0
!       CONSTANT VALUES
        H_MF  = 0.1D0
        U_MF1 = 1.D0/30.D0
        U_MF2 = 2.D0/30.D0
        XI_S  = 0.0001D0
        IPOIN2 = NBOR2%I(K)
        DO NP=1,NPLAN
          IJK=(NP-1)*NPTFR2+K
          I3D=(NP-1)*NPOIN2+IPOIN2
          IFRLIQ=NUMLIQ%I(K)
!         CASE OF A VERTICAL PROFILE
!         LOG PROFILE PRESCRIBED
          IF(VERPROVEL(IFRLIQ).NE.1) THEN
!            STEP PROFILE
!            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
!     &                       NP,INFOGR,VERPROVEL(IFRLIQ))
            DZ=2.D0*H_MF/DBLE(NPLAN-1.D0)

            IF(NP.GT.(NPLAN+1)/2) THEN
              Z_MF=DBLE(NP-(NPLAN+1)/2)*DZ
              S_MF1=S_MF1+LOG(Z_MF/XI_S)+8.5D0
            ELSE
              Z_MF=DBLE(NP-1.D0)*DZ
              IF(NP.NE.1.AND.NP.NE.(NPLAN+1)/2) THEN
                S_MF2 = S_MF2+LOG(MIN(Z_MF,H_MF-Z_MF)/XI_S)+8.5D0
              ELSE
                S_MF2 = S_MF2+LOG(DZ*0.1D0/XI_S)+8.5D0
              ENDIF
            ENDIF
          ENDIF
        ENDDO
!
!     FIXING UET1 AND UET2
!
        UET1=KARMAN*DBLE((NPLAN-1)/2)*U_MF1/S_MF1
        UET2=KARMAN*DBLE((NPLAN-1)/2)*U_MF2/S_MF2
!
        DO NP=1,NPLAN
          IJK=(NP-1)*NPTFR2+K
          IF(NP.GT.(NPLAN+1)/2) THEN
            Z_MF=DBLE(NP-(NPLAN+1)/2)*DZ
            UBORL%R(IJK) = UET1/KARMAN*(LOG(Z_MF/XI_S)+8.5D0)
          ELSEIF(NP.NE.1.AND.NP.NE.(NPLAN+1)/2) THEN
            Z_MF=DBLE(NP-1.D0)*DZ
            UBORL%R(IJK) = UET2/KARMAN*(LOG(MIN(Z_MF,H_MF-Z_MF)/XI_S)
     &                                 +8.5D0)
          ELSE
            UBORL%R(IJK) = UET2/KARMAN*(LOG(DZ*0.1D0/XI_S)+8.5D0)
          ENDIF
          VBORL%R(IJK) = 0.D0
!
!          IF(VERPROVEL(IFRLIQ).NE.1) THEN
!            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
!     &                       NP,INFOGR,VERPROVEL(IFRLIQ))
!            UBORL%R(IJK) = UBORL%R(IJK)*PROFZ
!            VBORL%R(IJK) = VBORL%R(IJK)*PROFZ
!         U AND V INITIALISED WITH PRESCRIBED VALUES (FOR DEBIMP3D)
!         WILL BE CHANGED AGAIN AFTER DEBIMP3D
          U%R(I3D)=UBORL%R(IJK)
          V%R(I3D)=VBORL%R(IJK)
        ENDDO
        YADEB(NUMLIQ%I(K))=1
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     PRESCRIBED DISCHARGES: FINAL TREATMENT OF VELOCITIES
!     ----------------------------------------------------
!
!     LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
      DO IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.NE.0) THEN
        MSK1=1
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_MAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
            CALL DEBIMP_3D(Q3(IFRLIQ,AT,INFOGR),
     &                     UBORL%R,VBORL%R,
     &                     U,V,NUMLIQ%I,NUMLIQ_ELM%I,IFRLIQ,T3_02,
     &                     NPTFR2,NETAGE,MASK_3D%ADR(MSK1)%P,
     &                     MESH3D,EQUA,IELM2V,SVIDE,MASKTR,
     &                     MESH3D%NELEB)
          ENDIF
        ELSE
          WRITE(LU,401) IFRLIQ
401       FORMAT(1X,'USER_BORD3D: MORE PRESCRIBED FLOWRATES',/,
     &           1X,'             ARE REQUIRED IN THE STEERING FILE',/,
     &           1X,'             AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      ENDDO ! IFRLIQ
      ENDIF
!
!     RESETS BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
!     AND OTHER SUBROUTINES BEFORE THE NEXT BOUNDARY CONDITIONS TREATMENT)
!
      DO K=1,NPTFR2
        IF(LIUBOL%I(K).EQ.KENT) THEN
          DO NP=1,NPLAN
            IJK=(NP-1)*NPTFR2+K
            U%R((NP-1)*NPOIN2+NBOR2%I(K))=UBORL%R(IJK)
            V%R((NP-1)*NPOIN2+NBOR2%I(K))=VBORL%R(IJK)
          ENDDO
        ENDIF
      ENDDO
!
!     EGR+BD MODIF TO AVOID TO ENTER IN FORTRAN FILE, TEMP0 =
!     TEMPERATURE OF COFLOW DEPENDS ON FROUD
!     TAKES THE VALUE DEFINED IN THE FILE FROUD.TXT
      NFO1 = T3D_FILES(T3DFO1)%LU
      REWIND NFO1
      READ(NFO1,*) FROUD
      TEMP0 = 20.D0
      IF(ABS(FROUD-0.9D0).LT.1.D-5) THEN
        TEMP1 = 25.3485028D0
      ELSEIF (ABS(FROUD-1.6D0).LT.1.D-5) THEN
        TEMP1 = 21.8663052D0
      ELSEIF (ABS(FROUD-5.0D0).LT.1.D-5) THEN
        TEMP1 = 20.2009931D0
      ELSE
        TEMP1 = 4.D0+SQRT((TEMP0-4.D0)**2
     &                    +0.0333D0**2/(GRAV*7.D-6*0.1D0*FROUD**2))
      ENDIF
!
      DO K=1,NPTFR2
        DO NP=1,NPLAN
          IBORD = (NP-1)*NPTFR2+K
          IF(LITABL%ADR(1)%P%I(IBORD).EQ.KENT) THEN
!           LINEAR STRATIFICATION AT THE ENTRANCE
            IF(NP.GT.(NPLAN+1)/2) THEN
              TABORL%ADR(1)%P%R(IBORD) = TEMP1
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!     NEUMANN FOR EPS AT THE BOTTOM
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
        DO IPOIN2=1,NPOIN2
          DZ=MESH3D%Z%R(IPOIN2+NPOIN2)-MESH3D%Z%R(IPOIN2)
          BEBORF%R(IPOIN2) = 4.D0*UETCAR%R(IPOIN2)**1.5D0
     &                           /KARMAN/DZ**2
     &                           *VISCVI%ADR(3)%P%R(IPOIN2)
          BEBORF%TYPR='Q'
        ENDDO
      ENDIF
!
!     END OF PART SPECIFIC TO VIOLLET CASE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
