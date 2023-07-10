!                   ************************
                    SUBROUTINE BILAN_SISYPHE
!                   ************************
!
     &(E,ESOMT,T1,VCUMU,DT,NPTFR,
     & INFO,ZFCL_C,ZFCL_S,ZFCL_MS,
     & NSICLA,VOLTOT,
     & NUMLIQ,NFRLIQ,FLBCLA,LT,NIT,NPOIN,VOLU2D,CSF_SABLE,MASDEP,
     & MASDEPT,CHARR,SUSP,SLIDE)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE MASS BALANCE.
!
!note     T2 IS NOT USED
!
!history  CMGDL
!+
!+        V5P9
!+   CHANGED FOR GRADED SEDIMENT
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
!+  Name of variables
!+
!
!history  J-M HERVOUET (EDF-LNHE)
!+        14/02/2012
!+        V6P2
!+  NSICLM and MAXFRO used instead of 10 and 300. New and compatible
!+  computation: flux given as argument, mass computed differently,
!+  and coefficient CSF_SABLE.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  R.KOPMANN (BAW)
!+        15/02/2019
!+        V7P2
!+   Adding mass balance per class
!+   incorporating nestor volumes in mass balance
!+   Adding mass balance with total volume (initial volume - final volume)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CSF_SABLE      |-->| 1-POROSITY
!| DT             |-->| TIME STEP
!| E              |-->| BED EVOLUTION AT A GIVEN TIME STEP
!| ESOMT          |-->| CUMULATED BED EVOLUTION
!| FLBCLA         |-->| BLOCK OF FLUXES AT BOUNDARY FOR EACH CLASS
!| INFO           |-->| IF YES : INFORMATION IS PRINTED
!| LT             |-->| CURRENT TIME STEP
!| MASDEP         |-->| VOLUME DEPOSITED ON THE BOTTOM FOR EACH CLASS
!|                |   | FROM THE BEGINNING
!| MASDEPT        |-->| VOLUME DEPOSITED ON THE BOTTOM FOR EACH CLASS
!|                |   | FOR THIS TIME STEP
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MESH           |<->| MESH STRUCTURE
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NIT            |-->| NUMBER OF TIME STEPS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY NODES
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| QSCLXC         |<->| TRANSPORT RATE FOR EACH CLASS X-DIRECTION
!| QSCLYC         |<->| TRANSPORT RATE FOR EACH CLASS Y-DIRECTION
!| S              |-->| VOID STRUCTURE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| VCUMU          |<->| VOLUME OF SEDIMENT ENTERING THE DOMAIN
!| VF             |-->| IF YES : FINITE VOLUMES IF NO : FINITE ELEMENTS
!| VOLTOT         |-->| VOLUME TOTAL PER CLASS OF SEDIMENT
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS (NOT ASSEMBLED IN //)
!| ZFCL_C         |<->| BEDLOAD EVOLUTION FOR EACH SEDIMENT CLASS
!| ZFCL_S         |<->| SUSPENDED LOAD EVOLUTION FOR EACH SEDIMENT CLASS
!| ZFCL_MS        |<->| SLIDE EVOLUTION FOR EACH SEDIMENT CLASS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NSICLM,MAXFRO
      USE DECLARATIONS_SISYPHE, ONLY : NESTOR,ZF,ZR,VOLINI,
     &     VCUMUCL,RMASCL,VOLNESTORCL,VOLNESTORCLA
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NPTFR,NFRLIQ,NSICLA,LT,NIT
      INTEGER, INTENT(IN)          :: NPOIN,NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: DT
      LOGICAL, INTENT(IN)          :: INFO,SUSP,SLIDE,CHARR
!
      DOUBLE PRECISION, INTENT(INOUT) :: VCUMU
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE,VOLTOT(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: MASDEP(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASDEPT(NSICLA)
!
!-----------------------------------------------------------------------
!
!     VECTOR STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: ZFCL_C
      TYPE(BIEF_OBJ), INTENT(IN)    :: E,ESOMT,VOLU2D,ZFCL_S
      TYPE(BIEF_OBJ), INTENT(IN)    :: ZFCL_MS
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,FLBCLA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IFRLIQ,IPTFR,ICLA,J
      DOUBLE PRECISION RMASSE,RCUMU,RMASCLA(NSICLM)
      DOUBLE PRECISION VCUMUCLA(NSICLM),FLUXT,FLUXTCLA,VOLDEP
      DOUBLE PRECISION FLT_BOUND(MAXFRO),VOLDEPC
      DOUBLE PRECISION VOLNESTOR,VOLLOSTPERC,VOLTOT1
      DOUBLE PRECISION VOLINI1,VCUMU1,VOLNESTOR1
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE EVOLUTION (E)
!
      RMASSE=0.D0
      DO I=1,NPOIN
        RMASSE=RMASSE+E%R(I)*VOLU2D%R(I)
      ENDDO
      IF(NCSIZE.GT.1) RMASSE = P_DSUM(RMASSE)
!
!=======================================================================
!
!     COMPUTES THE INTEGRAL OF EVOLUTION AT THE END (ESOMT)
!
      IF(LT.EQ.NIT) THEN
        RCUMU=0.D0
        DO I=1,NPOIN
          RCUMU=RCUMU+ESOMT%R(I)*VOLU2D%R(I)
        ENDDO
        IF(NCSIZE.GT.1) RCUMU = P_DSUM(RCUMU)
      ENDIF
!
!=======================================================================
!
!     COMPUTES THE FLUXES AT THE BOUNDARIES
!
      IF(CHARR) THEN
        CALL OS('X=Y     ',X=T1,Y=FLBCLA%ADR(1)%P)
        IF(NSICLA.GT.1) THEN
          DO I=2,NSICLA
            CALL OS('X=X+Y   ',X=T1,Y=FLBCLA%ADR(I)%P)
          ENDDO
        ENDIF
      ELSE
        CALL CPSTVC(FLBCLA%ADR(1)%P,T1)
        CALL OS('X=0     ',X=T1)
      ENDIF
!
      FLUXT=0.D0
!
      IF(NFRLIQ.GT.0) THEN
        DO IFRLIQ=1,NFRLIQ
          FLT_BOUND(IFRLIQ)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO IPTFR=1,NPTFR
            IFRLIQ=NUMLIQ(IPTFR)
            IF(IFRLIQ.GT.0) THEN
              FLT_BOUND(IFRLIQ)=FLT_BOUND(IFRLIQ)+T1%R(IPTFR)
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
      VCUMU = VCUMU - FLUXT*DT/CSF_SABLE
!
!     BALANCE IN EXTENDED GRANULOMETRY
!
      IF(NSICLA.GT.1) THEN
!
        DO ICLA=1,NSICLA
!
!       COMPUTES THE EVOLUTION PER CLASS
!
        RMASCLA(ICLA)=0.D0
        IF(SUSP.AND.SLIDE.AND.CHARR) THEN
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +( ZFCL_C%ADR(ICLA)%P%R(I)
     &                     +ZFCL_S%ADR(ICLA)%P%R(I)
     &                     +ZFCL_MS%ADR(ICLA)%P%R(I) )*VOLU2D%R(I)
          ENDDO
        ELSEIF(SLIDE.AND.CHARR) THEN
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +( ZFCL_C%ADR(ICLA)%P%R(I)
     &                     +ZFCL_MS%ADR(ICLA)%P%R(I) )*VOLU2D%R(I)
          ENDDO
        ELSEIF(SUSP.AND.CHARR) THEN
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +( ZFCL_C%ADR(ICLA)%P%R(I)
     &                     +ZFCL_S%ADR(ICLA)%P%R(I) )*VOLU2D%R(I)
          ENDDO
        ELSEIF(SUSP.AND.SLIDE) THEN
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +( ZFCL_S%ADR(ICLA)%P%R(I)
     &                     +ZFCL_MS%ADR(ICLA)%P%R(I) )*VOLU2D%R(I)
          ENDDO
        ELSEIF(SUSP) THEN
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +ZFCL_S%ADR(ICLA)%P%R(I)*VOLU2D%R(I)
          ENDDO
        ELSEIF(SLIDE) THEN
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +ZFCL_MS%ADR(ICLA)%P%R(I)*VOLU2D%R(I)
          ENDDO
        ELSEIF(CHARR) THEN
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +ZFCL_C%ADR(ICLA)%P%R(I)*VOLU2D%R(I)
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) RMASCLA(ICLA) = P_DSUM(RMASCLA(ICLA))
        RMASCL(ICLA) = RMASCL(ICLA)+RMASCLA(ICLA)

!
!       COMPUTES THE FREE FLUXES BY CLASS
!
        FLUXTCLA=0.D0
        IF(NFRLIQ.GT.0.AND.CHARR) THEN
          IF(NPTFR.GT.0) THEN
            DO IPTFR=1,NPTFR
              IFRLIQ=NUMLIQ(IPTFR)
              IF(IFRLIQ.GT.0) THEN
                FLUXTCLA=FLUXTCLA+FLBCLA%ADR(ICLA)%P%R(IPTFR)
              ENDIF
            ENDDO
          ENDIF
          IF(NCSIZE.GT.1) FLUXTCLA=P_DSUM(FLUXTCLA)
        ENDIF
!
        VCUMUCLA(ICLA) = - FLUXTCLA*DT/CSF_SABLE
        VCUMUCL(ICLA) = VCUMUCL(ICLA)- FLUXTCLA*DT/CSF_SABLE
!
        ENDDO
!
      ENDIF
!
!=======================================================================
!
!     GRAIN-FEEDING
!
      VOLDEPC=0.D0
      IF(SUSP) THEN
        DO I=1,NSICLA
          VOLDEPC=VOLDEPC+MASDEPT(I)
        ENDDO
        VOLDEPC=VOLDEPC/CSF_SABLE
      ENDIF
!
!     WRITES OUT THE BALANCE
!
      IF(INFO) THEN
!
!       GLOBAL BALANCE
!
        WRITE(LU,*)
        WRITE(LU,2000)
        WRITE(LU,2010) RMASSE
        IF(NFRLIQ.GT.0) THEN
          DO IFRLIQ=1,NFRLIQ
            WRITE(LU,2110) IFRLIQ,-FLT_BOUND(IFRLIQ)/CSF_SABLE
          ENDDO
          WRITE(LU,2111) -FLUXT/CSF_SABLE
          IF(SUSP) WRITE(LU,2112) VOLDEPC
        ENDIF
        VOLNESTOR = 0.D0
        IF(NESTOR) THEN
          DO I=1,NSICLA
            VOLNESTOR = VOLNESTOR + VOLNESTORCLA(I)
          END DO
          WRITE(LU,*)'NESTOR VOLUME CHANGE      =   ', VOLNESTOR
        ENDIF
        WRITE(LU,2033) RMASSE+DT*FLUXT/CSF_SABLE-VOLDEPC-VOLNESTOR
!       BALANCE PER CLASS
!
        IF(NSICLA.GT.1) THEN
          DO I=1,NSICLA
            WRITE(LU,*)
            WRITE(LU,*) 'MASS BALANCE FOR SEDIMENT CLASS :',I
            WRITE(LU,*) 'TOTAL VOLUME:',VOLTOT(I)
            WRITE(LU,3010) RMASCLA(I)
            WRITE(LU,3031) VCUMUCLA(I)
            WRITE(LU,*) 'NESTOR VOLUME PER CLASS: ',VOLNESTORCLA(I)
            IF(SUSP) THEN
              WRITE(LU,3034) MASDEPT(I)/CSF_SABLE
              WRITE(LU,2033) RMASCLA(I)-VCUMUCLA(I)
     &                                 -MASDEPT(I)/CSF_SABLE
            ELSE
              WRITE(LU,2033) RMASCLA(I)-VCUMUCLA(I)-VOLNESTORCLA(I)
            ENDIF
          ENDDO
        ENDIF
!
!       FINAL GLOBAL BALANCE
!
        IF(LT.EQ.NIT) THEN
          WRITE(LU,*)
          WRITE(LU,*)'---------------------------'
          WRITE(LU,*)'FINAL SEDIMENT MASS BALANCE'
          WRITE(LU,*)'---------------------------'
          VOLDEP=0.D0
          VOLNESTOR = 0.D0
          DO I=1,NSICLA
            VOLDEP=VOLDEP+MASDEP(I)
          ENDDO
          IF(NESTOR) THEN
            DO I=1,NSICLA
              VOLNESTOR=VOLNESTOR+VOLNESTORCL(I)
            ENDDO
          ENDIF
          VOLDEP=VOLDEP/CSF_SABLE
          WRITE(LU,*)
          WRITE(LU,2030) RCUMU
          WRITE(LU,2031) VCUMU
          IF(SUSP) WRITE(LU,2032) VOLDEP
          IF(NESTOR) WRITE(LU,*)'CUMULATED NESTOR VOLUME:        :',
     &        VOLNESTOR
          WRITE(LU,2033) RCUMU-VCUMU-VOLDEP-VOLNESTOR
          IF(NSICLA.EQ.1) THEN
            VOLINI1 = VOLINI(1)
            VOLTOT1 = 0.D0
            DO J=1,NPOIN
              VOLTOT1 = VOLTOT1 + (ZF%R(J)-ZR%R(J))*VOLU2D%R(J)
            ENDDO
            IF(NCSIZE>1)VOLTOT1 = P_DSUM(VOLTOT1)
            VOLNESTOR1 = VOLNESTOR
            VCUMU1 = VCUMU
          ELSE ! NSICLA >1
            VOLINI1 = 0.D0
            VOLTOT1 = 0.D0
            VOLNESTOR1 = 0.D0
            VCUMU1 = 0.D0
            DO I=1,NSICLA
              VOLINI1 = VOLINI1+VOLINI(I)
              VOLTOT1 = VOLTOT1+VOLTOT(I)
              VOLNESTOR1 = VOLNESTOR1+VOLNESTORCL(I)
              VCUMU1 = VCUMU1+VCUMUCL(I)
            END DO
          ENDIF

          WRITE(LU,*)'INITIAL VOLUME                  :',VOLINI1
          WRITE(LU,*)'FINAL VOLUME                    :',VOLTOT1
          IF(VOLINI1.GT.0.D0) THEN
            VOLLOSTPERC = (VOLTOT1-VOLINI1-VCUMU1-
     &              VOLNESTOR)*100.D0/VOLINI1
          ELSE
            VOLLOSTPERC = 0.D0
          ENDIF
          WRITE(LU,*)'TOTAL VOLUME LOST               :',
     &        VOLTOT1-VOLINI1-VCUMU1-VOLNESTOR
          WRITE(LU,*)'                                :'
     &               ,VOLLOSTPERC,'%'
          IF(NSICLA>1) THEN
            DO I=1,NSICLA
              WRITE(LU,*)
              WRITE(LU,*) 'MASS BALANCE FOR SEDIMENT CLASS :',I
              WRITE(LU,*)'VOLUME THAT ENTERED THE DOMAIN PER CLASS   ',
     &                    VCUMUCL(I)
              WRITE(LU,*)'SUM OF THE CUMULATED EVOLUTIONS PER CLASS  ',
     &                    RMASCL(I)
              WRITE(LU,*)'BALANCE FLUXES AND EVOLUTIONS PER CLASS    ',
     &                    RMASCL(I)-VCUMUCL(I)
              WRITE(LU,*) ''
              WRITE(LU,*)'INITIAL VOLUME PER CLASS OVER ALL LAYERS   ',
     &                    VOLINI(I)
              WRITE(LU,*)'FINAL VOLUME  PER CLASS OVER ALL LAYERS    ',
     &                    VOLTOT(I)
              IF(NESTOR)
     &           WRITE(LU,*)
     &          'DREDGED/DISPOSED VOLUME PER CLASS          ',
     &                    VOLNESTORCL(I)
              IF(VOLINI(I).GT.0.D0) THEN
                VOLLOSTPERC = (VOLTOT(I)-VOLINI(I)-VCUMUCL(I)-
     &           VOLNESTORCL(I))*100.D0/VOLINI(I)
              ELSE
                VOLLOSTPERC = 0.D0
              ENDIF
              WRITE(LU,*)'TOTAL VOLUME LOST PER CLASS OVER ALL LAYERS',
     &                    VOLTOT(I)-VOLINI(I)-VCUMUCL(I)-VOLNESTORCL(I)
              WRITE(LU,*)'                                           '
     &            ,VOLLOSTPERC,'%'
            END DO
          ENDIF
        ENDIF ! lt==nit
!
!       IF(LGRAFED) THEN
!         WRITE(LU, 4001) MASST
!         WRITE(LU, 4011) MASS_GF
!       ENDIF
!
      ENDIF
!
2000  FORMAT(1X,'MASS-BALANCE (IN VOLUME, INCLUDING VOID): ')
2010  FORMAT(1X,'SUM OF THE EVOLUTIONS : ',G16.7,' M3')
2030  FORMAT(1X,'SUM OF THE CUMULATED EVOLUTIONS : ',G16.7)
2031  FORMAT(1X,'VOLUME THAT ENTERED THE DOMAIN  : ',G16.7,' M3'
     &         ,'  ( IF <0 EXIT )')
2032  FORMAT(1X,'VOLUME DEPOSITED ON THE BOTTOM  : ',G16.7,' M3'
     &         ,'  ( IF <0 ERODED )')
2033  FORMAT(1X,'LOST VOLUME                     : ',G16.7,' M3'
     &         ,'  ( IF <0 EXIT )')
2110  FORMAT(1X,'BOUNDARY ',1I3,' BEDLOAD FLUX = ',G16.7,
     &          '  ( M3/S  >0 = ENTERING )')
2111  FORMAT(1X,'TOTAL        BEDLOAD FLUX = ',G16.7,
     &          '  ( M3/S  >0 = ENTERING )')
2112  FORMAT(1X,'DEPOSIT ON BOTTOM         = ',G16.7,
     &          '  ( M3/S )')
3010  FORMAT(1X,'SUM OF THE EVOLUTIONS FOR THIS CLASS: ',G16.7)
3031  FORMAT(1X,'VOLUME THAT ENTERED THE DOMAIN FOR THIS CLASS: '
     &       ,G16.7,' M3')
3034  FORMAT(1X,'VOLUME DEPOSITED ON BOTTOM FOR THIS CLASS:     '
     &       ,G16.7,' M3')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
