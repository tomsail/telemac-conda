!                   ***************************
                    SUBROUTINE SUSPENSION_BILAN
!                   ***************************
!
     &(MESH,CST,HN,ZFCL_S,MASKEL,IELMT,ITRA,LT,NIT,DT,CSF,
     & MASSOU,MASED0,MSK,ENTET,MASTEN,MASTOU,MASINI,T2,
     & T3,MASFIN,MASDEPT,MASDEP,AGGLOT,
     & VOLU2D,NUMLIQ,NFRLIQ,NPTFR,FLBORTRA)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    MASS-BALANCE FOR THE SUSPENSION.
!
!history  C. MOULIN (LNH)
!+        13/12/2000
!+        V5P1
!+
!
!history  M. GONZALES DE LINARES
!+        **/05/2003
!+        V5P4
!+
!
!history  F. HUVELIN
!+        22/12/2004
!+        V5P6
!+
!
!history  J-M HERVOUET
!+        29/10/2007
!+        V5P8
!+   CORRECTIONS IN PARALLEL MODE
!
!history
!+        05/05/2008
!+
!+   COMPUTES THE MASS ACCOUNTING FOR MASS-LUMPING
!
!history
!+        28/05/2008
!+
!+   FLUX GIVEN BY BOUNDARIES
!
!history
!+        10/06/2008
!+
!+   TRACER FLUX GIVEN BY FLBORTRA (FROM CVDFTR)
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
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
!| CSF            |-->| VOLUME CONCENTRATION OF THE SEDIMENT BED
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
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NIT            |-->| TOTAL NUMBER OF ITERATIONS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| VOLU2D         |-->| INTEGRAL OF BASES
!| ZFCL_S         |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_BILAN => SUSPENSION_BILAN
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CST,HN,VOLU2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZFCL_S,MASKEL,FLBORTRA
      INTEGER,          INTENT(IN)    :: IELMT,ITRA,LT,NIT,NFRLIQ,NPTFR
      INTEGER,          INTENT(IN)    :: NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,CSF
      DOUBLE PRECISION, INTENT(IN)    :: MASSOU,MASED0,AGGLOT
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN,MASTOU,MASINI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2,T3
      DOUBLE PRECISION, INTENT(INOUT) :: MASFIN,MASDEPT,MASDEP
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER IFRLIQ,I
      DOUBLE PRECISION            :: ERREUR, PERDUE, FLUXT
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
      MASFIN = DOTS(T2,HN)
      IF(NCSIZE.GT.1) MASFIN=P_DSUM(MASFIN)
      ! ************************** !
      ! II - TOTAL MASS OF DEPOSIT !
      ! ************************** !
!     CALL VECTOR(T2, '=', 'MASVEC          ',IELMT,CSF,ZFCL_S,HN,
!    &            HN,HN,HN,HN,MESH,MSK,MASKEL)

      IF(AGGLOT.GT.0.999999D0) THEN
!       ASSUMES HERE THAT AGGLOT=1.D0
        CALL OS('X=CYZ   ',X=T2,Y=VOLU2D,Z=ZFCL_S,C=CSF)
      ELSE
        CALL VECTOR(T2,'=','MASVEC          ',IELMT,
     &             1.D0-AGGLOT,ZFCL_S,T3,T3,T3,T3,T3,MESH,MSK,MASKEL)
        CALL OS('X=X+CYZ ',X=T2,Y=VOLU2D,Z=ZFCL_S,C=AGGLOT)
        CALL OS('X=CX    ',X=T2,C=CSF)
      ENDIF
      MASDEPT = BIEF_SUM(T2)
      IF(NCSIZE.GT.1) MASDEPT = P_DSUM(MASDEPT)
      ! *************************************************** !
      ! III - TOTAL MASS OF DEPOSITED (OR ERODED) SEDIMENTS !
      ! *************************************************** !
      MASDEP = MASDEP + MASDEPT
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
      ! ********************************************** !
      ! VII - QUANTITY ENTERED THROUGH LIQUID BOUNDARY !
      ! ********************************************** !
      MASTEN = MASTEN - FLUXT * DT
      ! ************************************** !
      ! VIII - QUANTITY CREATED BY SOURCE TERM !
      ! ************************************** !
      MASTOU = MASTOU + MASSOU
      ! ******************** !
      ! IX - ERROR ON VOLUME !
      ! ******************** !
!
      ERREUR = MASINI + MASSOU - DT*FLUXT - MASDEPT - MASFIN
!
      ! *********** !
      ! X - LISTING !
      ! *********** !
      IF(ENTET) THEN
        WRITE(LU,2005) ITRA,MASINI
        WRITE(LU,2100) ITRA,MASFIN
        IF(NFRLIQ.GT.0) THEN
          DO IFRLIQ=1,NFRLIQ
            WRITE(LU,2110) IFRLIQ,ITRA,-FLT_BOUND(IFRLIQ)
          ENDDO
        ENDIF
        IF(ABS(MASDEPT) > 1.D-8) WRITE(LU,2115) MASDEPT
        IF(ABS(MASSOU ) > 1.D-8) WRITE(LU,2116) MASSOU
        WRITE(LU,2120) ERREUR
      ENDIF
      ! ************************************** !
      ! XI - LISTING OF THE FINAL MASS-BALANCE !
      ! ************************************** !
      IF(LT.EQ.NIT.AND.ENTET) THEN
        PERDUE = MASED0 + MASTEN + MASTOU - MASFIN - MASDEP
        WRITE(LU,3100) ITRA
        WRITE(LU,2160) ITRA,MASED0, MASFIN
        IF(ABS(MASTEN) > 1.D-8) WRITE(LU,2161) MASTEN
        IF(ABS(MASTOU) > 1.D-8) WRITE(LU,2164) MASTOU
        IF(ABS(MASDEP) > 1.D-8) WRITE(LU,2167) MASDEP
        WRITE(LU,2166) PERDUE
      ENDIF
      ! *************************** !
      ! XII - UPDATES INITIAL MASS  !
      ! *************************** !
      MASINI = MASFIN
      !----------------------------------------------------------------!
2005  FORMAT(/,1X,'QUANTITY OF CLASS                 ',I2
     &         ,' IN SUSPENSION AT TIME T    : ',G16.7,' M3')
2100  FORMAT(1X,'QUANTITY OF CLASS                 ',I2
     &         ,' IN SUSPENSION AT TIME T+DT : ',G16.7,' M3')
2110  FORMAT(1X,'BOUNDARY ',1I3,' FLUX TRACER ',1I2,' = ',G16.7,
     &          ' ( >0 : ENTERING  <0 : EXITING )')
2115  FORMAT(1X,'VOLUME OF DEPOSIT           : ',G16.7,' M3')
2116  FORMAT(1X,'VOLUME CREATED BY SOURCE TERM       '
     &         ,'                            : ',G16.7,' M3')
2120  FORMAT(1X,'ERROR ON VOLUME             : ',G16.7,/)
2160  FORMAT(1X,'INITIAL QUANTITY OF ',I2,'           : ',G16.7
     &         ,' M3',
     &     /,1X,'FINAL QUANTITY                     : ',G16.7, ' M3')
2161  FORMAT(1X,'QUANTITY ENTERED THROUGH LIQ. BND. : ',G16.7, ' M3')
2164  FORMAT(1X,'QUANTITY CREATED BY SOURCE TERM    : ',G16.7, ' M3')
2166  FORMAT(1X,'TOTAL QUANTITY LOST                : ',G16.7, ' M3',/)
2167  FORMAT(1X,'TOTAL MASS OF DEPOSIT              : ',G16.7, ' M3')
3100  FORMAT(/,1X,'      *** ','FINAL BALANCE FOR TRACER',I2,' ***')!
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
