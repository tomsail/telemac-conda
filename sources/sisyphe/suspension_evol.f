!                   **************************
                    SUBROUTINE SUSPENSION_EVOL
!                   **************************
!
     &(ZFCL_S,FLUDP,FLUER,DT, NPOIN,XMVS, QFLUX,MS_VASE,ES_VASE,
     & CONC,NOMBLAY)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE EVOLUTION FOR MUD ACCORDING TO FLUDP
!+                AND FLUER; AND UPDATES THE MASS OF THE LAYERS +
!+                EACH LAYER THICKNESS + TOTAL THICKNESS.
!
!note     COMPUTE ES AGAIN AT THE END AND
!+         VERIFY THE CRITERION ELAY=ZF-ZR
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
!history  C.VILLARET (EDF-LNHE)
!+        21/08/2012
!+        V6P2
!+   Added new input argument (ELAY)
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AVAI           |<->| PERCENT OF MUD CLASS PER LAYER
!| CONC_VASE      |-->|  INPUT CONCENTRATION OF EACH LAYER (IN KG/M3)
!| DT             |-->| TIME STEP
!| ES             |<->| THICKNESS OF SEDIMENT BED LAYERS
!| FLUDP          |<->| DEPOSITION FLUX
!| FLUER          |<->| EROSION FLUX
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NOMBLAY        |-->| NUMBER OF VERTICAL BED LAYERS
!| NPOIN          |-->| NUMBER OF POINTS
!| QFLUX          |---| NET EROSION MINUS DEPOSITION RATE
!| XMVS           |-->| WATER DENSITY
!| ZFCL_S         |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE INTERFACE_SISYPHE,EX_SUSPENSION_EVOL => SUSPENSION_EVOL
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! 2/ GLOBAL VARIABLES
      TYPE (BIEF_OBJ),  INTENT(INOUT)   :: ZFCL_S,FLUDP,FLUER,QFLUX
      DOUBLE PRECISION, INTENT(IN)      :: DT, XMVS
      INTEGER, INTENT(IN)               :: NPOIN,NOMBLAY
      DOUBLE PRECISION, INTENT(INOUT)   :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT)  :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT)  :: ES_VASE(NPOIN,NOMBLAY)
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I,J
!
      DOUBLE PRECISION ZERO
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ZERO = 1.D-08

!
!     COMPUTES THE SEDIMENT FLUX DURING EACH TIMESTEP
!     QFLUX IS IN KG/M2 (MUD CONC ARE ALSO IN KG/M3)
!
      CALL OS('X=Y-Z   ', X=QFLUX, Y=FLUDP, Z=FLUER)
      CALL OS('X=CX    ', X=QFLUX, C=DT*XMVS)
!
      IF(NOMBLAY.EQ.1)  THEN
        DO I = 1, NPOIN
          ZFCL_S%R(I)=QFLUX%R(I)/CONC(I,1)
          ES_VASE(I,1)= ES_VASE(I,1)+ZFCL_S%R(I)
        ENDDO
!
      ELSE
        DO I = 1, NPOIN
!
!         DEPOSITION IN THE FIRST LAYER
!
          IF(QFLUX%R(I).GE.ZERO) THEN
!           ZFCL_S%R(I) = QFLUX%R(I) / CONC_VASE(1)
            ZFCL_S%R(I) = QFLUX%R(I) / CONC(I,1)
            ES_VASE(I,1)=ES_VASE(I,1)+ZFCL_S%R(I)
          ELSEIF(QFLUX%R(I).LT.ZERO) THEN
!
!           EROSION OF SUCCESSIVE LAYERS
!
!
            ZFCL_S%R(I) = 0.D0
!
            DO J = 1, NOMBLAY
!
!             CONC ARE IN KG/M3
!
              IF(-QFLUX%R(I).LE.CONC(I,J)*ES_VASE(I,J)) THEN
!               Last layer to be eroded
                ZFCL_S%R(I)= ZFCL_S%R(I)+QFLUX%R(I)/CONC(I,J)
                ES_VASE(I,J)=ES_VASE(I,J)-
     &                MAX(-QFLUX%R(I)/CONC(I,J),0.D0)
                GO TO 40
              ELSE
!               EROSION OF THE WHOLE LAYER
                QFLUX%R(I)=QFLUX%R(I)+CONC(I,J)*ES_VASE(I,J)
                ZFCL_S%R(I)=ZFCL_S%R(I) - ES_VASE(I,J)
                ES_VASE(I,J) = 0.D0
              ENDIF
!           END OF THE LOOP ON THE LAYERS
!
            ENDDO ! J
!
!
            WRITE(LU,*) 'BEWARE, ALL LAYERS EMPTY, NODE I=',I
            CALL PLANTE(1)
            STOP
!         END EROSION
          ENDIF
   40     CONTINUE
!
!       END OF THE LOOP ON THE NODES
!
        ENDDO ! I
      ENDIF
!
!     reactualisation of MS_VASE
!
      DO J = 1, NOMBLAY
        DO I = 1, NPOIN
          MS_VASE(I,J)=CONC(I,J)*ES_VASE(I,J)
        ENDDO
      ENDDO

!======================================================================!
!======================================================================!
!
      RETURN
      END
