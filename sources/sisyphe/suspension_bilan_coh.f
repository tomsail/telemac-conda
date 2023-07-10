!                   *******************************
                    SUBROUTINE SUSPENSION_BILAN_COH
!                   *******************************
!
     &(MESH,CST,HN,MASKEL,IELMT,ITRA,LT,NIT,DT,
     & XMVS,MS_VASE, NOMBLAY,NPOIN,
     & MASSOU,MASED0,MSK,ENTET,MASTEN,MASTOU,MASINI,T1,T2,
     & T3,MASFIN,MASDEPT,MASDEP,AGGLOT,
     & VOLU2D,NUMLIQ,NFRLIQ,NPTFR,FLBORTRA,SEDCO)
!
!***********************************************************************
! SISYPHE   V6P3                                   13/02/2013
!***********************************************************************
!
!brief    MASS-BALANCE FOR THE COHESIVE.
!
!history  C.VILLARET (EDF-LNHE)
!+        19/07/2011
!+        V6P2
!+   New subroutine: mass balance for cohesive sediments
!+
!
!history  P.TASSI PAT (EDF-LNHE)
!+        13/02/2013
!+        V6P3
!+   Correction bug NUMLIQ(NPTFR)
!+
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AGGLOT         |-->| COEFFICIENT OF MASS-LUMPING
!| CST            |<->| CONCENTRATION AT TIME T(N+1)
!| DT             |-->| TIME STEP IN SECONDS
!| ENTET          |<->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| FLBORTRA       |<->| FLUXES AT BOUNDARIES TRACER
!| HN             |-->| WATER DEPTH
!| IELMT          |-->| NUMBER OF ELEMENTS
!| ITRA           |-->| TRACER INDEX
!| LT             |-->| ITERATION
!| MASDEP         |<--| TOTAL DEPOSITED MASS
!| MASDEPT        |<--| DEPOSITED MASS DURING THE TIME STEP
!| MASED0         |<->| SUSPENDED MASS BALANCE
!| MASFIN         |<--| MASS AT THE END
!| MASINI         |<->| INITIAL MASS
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MASTEN         |<->| MASS ENTERED THROUGH LIQUID BOUNDARY
!| MASTOU         |<->| MASS CREATED BY SOURCE TERM
!| MESH           |<->| MESH STRUCTURE
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NIT            |-->| TOTAL NUMBER OF ITERATIONS
!| NOMBLAY        |-->| NUMBER OF VERTICAL BED LAYERS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| VOLU2D         |-->| INTEGRAL OF BASES
!| XMVS           |-->| SOLID DENSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_BILAN_COH
     &                    => SUSPENSION_BILAN_COH
      USE BIEF
! FOR MUD
      USE DECLARATIONS_SISYPHE, ONLY: MASV0, MASVT
! FOR SAND
      USE DECLARATIONS_SISYPHE, ONLY: MASS0, MASST
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,CST
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,FLBORTRA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: VOLU2D
      INTEGER,          INTENT(IN)    :: IELMT,ITRA,LT,NIT,NFRLIQ,NPTFR
      INTEGER,          INTENT(IN)    :: NUMLIQ(NPTFR), NOMBLAY,NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DT,XMVS
      DOUBLE PRECISION, INTENT(IN)    :: MASSOU,MASED0,AGGLOT
      LOGICAL,          INTENT(IN)    :: MSK,ENTET,SEDCO
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN,MASTOU,MASINI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2,T3,T1
      DOUBLE PRECISION, INTENT(INOUT) :: MASFIN,MASDEPT,MASDEP
      DOUBLE PRECISION, INTENT(IN)    :: MS_VASE(NPOIN, NOMBLAY)

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER IFRLIQ,I,J
      DOUBLE PRECISION            :: ERREUR, PERDUE, FLUXT, MASS_INIT
!     HERE 300 IS MAXFRO, THE MAXIMUM NUMBER OF LIQUID BOUNDARIES
      DOUBLE PRECISION FLT_BOUND(300)
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ************************************** !
      ! I - QUANTITY OF SEDIMENT IN SUSPENSION !
      ! ************************************** !
!
      IF(AGGLOT.GT.0.999999D0) THEN
!       ASSUMES HERE THAT AGGLOT=1.D0
        CALL OS('X=YZ    ',X=T2,Y=VOLU2D,Z=CST)
      ELSE
        CALL VECTOR(T2,'=','MASVEC          ',IELMT,
     &             1.D0-AGGLOT,CST,T3,T3,T3,T3,T3,MESH,MSK,MASKEL)
        CALL OS('X=X+CYZ ',X=T2,Y=VOLU2D,Z=CST,C=AGGLOT)
      ENDIF
!
! VOLUME OF SEDIMENTS IN SUSPENSION (M3) !
!
      MASFIN = DOTS(T2,HN)
      IF(NCSIZE.GT.1) MASFIN=P_DSUM(MASFIN)
!
      ! ************************** !
      ! II - TOTAL MASS OF DEPOSIT !
      ! ************************** !
!     CALL VECTOR(T2, '=', 'MASVEC          ',IELMT,CSF,ZFCL_S,HN,
!    &            HN,HN,HN,HN,MESH,MSK,MASKEL)

!      IF(AGGLOT.GT.0.999999D0) THEN
!       ASSUMES HERE THAT AGGLOT=1.D0
!        CALL OS('X=CYZ   ',X=T2,Y=VOLU2D,Z=ZFCL_S,C=CSF)
!      ELSE
!        CALL VECTOR(T2,'=','MASVEC          ',IELMT,
!     &             1.D0-AGGLOT,ZFCL_S,T3,T3,T3,T3,T3,MESH,MSK,MASKEL)
!        CALL OS('X=X+CYZ ',X=T2,Y=VOLU2D,Z=ZFCL_S,C=AGGLOT)
!        CALL OS('X=CX    ',X=T2,C=CSF)
!      ENDIF
!      MASDEPT = BIEF_SUM(T2)
!      IF(NCSIZE.GT.1) MASDEPT = P_DSUM(MASDEPT)
!
      ! **************************             !
      ! II - TOTAL MASS OF SEDIMENT BED  (KG)  !
      !                           ---> MASVT   !
      ! **************************             !
! SAVE RESULT PAS DE TEMPS PRECEDENT : MASS_INIT
      IF(SEDCO) THEN
        MASS_INIT = MASVT
      ELSE
        MASS_INIT = MASST
      ENDIF
!
      DO I=1,NPOIN
        T1%R(I)=0.D0
        DO J=1,NOMBLAY
          T1%R(I)= T1%R(I)+MS_VASE(I,J)
        ENDDO
      ENDDO
      IF(SEDCO) THEN
        MASVT=DOTS(T1,VOLU2D)
        IF(NCSIZE.GT.1) MASVT=P_DSUM(MASVT)
      ELSE
        MASST=DOTS(T1,VOLU2D)
        IF(NCSIZE.GT.1) MASST=P_DSUM(MASST)
      ENDIF
!


      ! ***************************************************     !
      ! III - TOTAL MASS OF DEPOSITED (OR ERODED) SEDIMENTS (KG)!
      !                           ---> MASDEP                   !
      ! ***************************************************     !
      IF(SEDCO)THEN
        MASDEP = MASVT-MASV0
        MASDEPT = MASVT - MASS_INIT
      ELSE
        MASDEP = MASST-MASS0
        MASDEPT = MASST - MASS_INIT
      ENDIF
!
!  MASS DEPOSITED DURING TIME STEP : MASDEPT
!
!
!=======================================================================
!
!     COMPUTES THE FLUXES (NO DIFFUSIVE FLUX,...TO INVESTIGATE)
!
!=======================================================================
!
      FLUXT=0.D0
!
      IF(NFRLIQ.GT.0) THEN
        DO IFRLIQ=1,NFRLIQ
          FLT_BOUND(IFRLIQ)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO I=1,NPTFR
!           NOTE: FLUX_BOUNDARIES COULD BE DEFINED BETWEEN 0 AND NFRLIQ
            IFRLIQ=NUMLIQ(I)
            IF(IFRLIQ.GT.0) THEN
              FLT_BOUND(IFRLIQ)=FLT_BOUND(IFRLIQ)+FLBORTRA%R(I)
            ENDIF
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO IFRLIQ=1,NFRLIQ
            FLT_BOUND(IFRLIQ)=P_DSUM(FLT_BOUND(IFRLIQ))
          ENDDO
        ENDIF
        DO IFRLIQ=1,NFRLIQ
          FLUXT=FLUXT+FLT_BOUND(IFRLIQ)
        ENDDO
      ENDIF
!
      ! **********************************************        !
      ! VII - QUANTITY ENTERED THROUGH LIQUID BOUNDARY (IN M3)!
      ! **********************************************        !
      MASTEN = MASTEN - FLUXT * DT
      ! **************************************       !
      ! VIII - QUANTITY CREATED BY SOURCE TERM IN(M3)!
      ! **************************************       !
      MASTOU = MASTOU + MASSOU
      ! ******************** !
      ! IX - ERROR IN MASS (KG) !
      ! ******************** !
!
      ERREUR = (MASINI -MASFIN)*XMVS! (MASSE EN SUSP + BED at T-DT)
!      *          + MASSOU *XMVS  ! (SOURCE TERM In KG)
     &          - DT*FLUXT*XMVS ! (FLUX IN KG)
     &          - MASDEPT    !(DEPOSITED MASS IN KG)
!
      ! *********** !
      ! X - LISTING !
      ! *********** !
      IF(ENTET) THEN
        WRITE(LU,2005) ITRA,MASINI*XMVS
        WRITE(LU,2100) ITRA,MASFIN*XMVS
        IF(NFRLIQ.GT.0) THEN
          DO IFRLIQ=1,NFRLIQ
            WRITE(LU,2110) IFRLIQ,ITRA,-FLT_BOUND(IFRLIQ)*XMVS
          ENDDO
        ENDIF
        IF(ABS(MASDEPT) > 1.D-8) WRITE(LU,2115) MASDEPT
        IF(ABS(MASSOU ) > 1.D-8) WRITE(LU,2116) MASSOU*XMVS
        WRITE(LU,2120) ERREUR
      ENDIF
      ! ************************************** !
      ! XI - LISTING OF THE FINAL MASS-BALANCE !
      ! ************************************** !
      IF(LT.EQ.NIT.AND.ENTET) THEN
        IF(SEDCO)THEN
          PERDUE = MASED0*XMVS + MASV0
     &         + MASTEN*XMVS
     &         - MASFIN*XMVS - MASVT
!              + MASTOU*XMVS
        ELSE
          PERDUE = MASED0*XMVS + MASS0
     &         + MASTEN*XMVS
     &         - MASFIN*XMVS - MASST
!              + MASTOU*XMVS
        ENDIF

        WRITE(LU,3100) ITRA
        WRITE(LU,2160) ITRA,MASED0*XMVS, MASFIN*XMVS
        IF(ABS(MASTEN) > 1.D-8) WRITE(LU,2161) MASTEN*XMVS
        IF(ABS(MASTOU) > 1.D-8) WRITE(LU,2164) MASTOU*XMVS
        IF(ABS(MASDEP) > 1.D-8) WRITE(LU,2167) MASDEP
        WRITE(LU,2166) PERDUE
      ENDIF
      ! *************************** !
      ! XII - UPDATES INITIAL MASS  !
      ! *************************** !

      MASINI = MASFIN

      !----------------------------------------------------------------!
2005  FORMAT(/,1X,'QUANTITY OF CLASS                 ',I2
     &         ,' IN SUSPENSION AT TIME T    : ',G20.11,' KG')
2100  FORMAT(1X,'QUANTITY OF CLASS                 ',I2
     &         ,' IN SUSPENSION AT TIME T+DT : ',G20.11,' KG')
2110  FORMAT(1X,'BOUNDARY ',1I3,' FLUX TRACER ',1I2,' = ',G20.11,
     &          ' ( >0 : ENTERING  <0 : EXITING )')
2115  FORMAT(1X,'VOLUME OF DEPOSIT           : ',G20.11,' KG')
2116  FORMAT(1X,'VOLUME CREATED BY SOURCE TERM       '
     &         ,'                            : ',G20.11,' KG')
2120  FORMAT(1X,'ERROR ON VOLUME             : ',G20.11,/)
2160  FORMAT(1X,'INITIAL QUANTITY OF ',I2,'           : ',G20.11
     &         ,' KG',
     &     /,1X,'FINAL QUANTITY                     : ',G20.11, ' KG')
2161  FORMAT(1X,'QUANTITY ENTERED THROUGH LIQ. BND. : ',G20.11, ' KG')
2164  FORMAT(1X,'QUANTITY CREATED BY SOURCE TERM    : ',G20.11, ' KG')
2166  FORMAT(1X,'TOTAL MASS LOST                : ',G20.11, ' KG',/)
2167  FORMAT(1X,'TOTAL MASS OF DEPOSIT              : ',G20.11, ' KG')
3100  FORMAT(/,1X,'      *** ','FINAL BALANCE FOR TRACER',I2,' ***')!
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
