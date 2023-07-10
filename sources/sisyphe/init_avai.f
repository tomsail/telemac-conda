!                   ********************
                    SUBROUTINE INIT_AVAI
!                   ********************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION AND LAYER THICKNESS.
!
!note     PHILOSOPHY: INIT_COMPO DEFINES THE STRATIFICATION CORRESPONDING
!+                TO THE BOTTOM INITIAL COMPOSITION; NCOUCHES CORRESPONDS
!+                TO THE NUMBER OF REAL INITIAL LAYERS.
!note         INIT_AVAI CORRECTS AND SUPPLEMENTS THIS STRATIFICATION
!+                IF PROBLEMS, AND ADDS THE ACTIVE LAYER; NLAYER CORRESPONDS
!+                TO THE NUMBER OF LAYERS USED IN THE COMPUTATION.
!
!history  BUI MINH DUC
!+        2002
!+
!+   INITIAL FRACTION DISTRIBUTION FOR NON-UNIFORM BED MATERIALS
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002/2003
!+
!+
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
!history  J.RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!!history  R.KOPMANN (BAW)
!+        Januar 2018
!+        V7P3
!+   Checks for bad layer thicknesses and fractions
!
!history  R.KOPMANN (BAW)
!+        15/02/2019
!+        V7P2
!+   Adding initial volume for mass balance
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      USE INTERFACE_SISYPHE, EX_INIT_AVAI=> INIT_AVAI
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      IMPLICIT NONE
!
!
!-----------------------------------------------------------------------
!
      INTEGER I,J,K,NMAXI
!
      DOUBLE PRECISION HAUTSED,TEST1
!
!-----------------------------------------------------------------------
!
      NMAXI = 0
!
!     THE INITIAL NUMBER OF LAYERS, THEIR THICKNESS AND THEIR COMPOSITION
!     ARE SET BY THE USER
!
!     NOTE: WHEN COMPUTATION CONTINUED INIT_COMPO MUST NOT
!           CHANGE ES AND AVAIL
!
      IF(DEBU) THEN
!       TODO: TENTATIVE VALUE, THIS IS TO CHECK
        DO J=1,NPOIN
          I=1
          IF(NOMBLAY.GE.2) THEN
            DO K=2,NOMBLAY
              IF(ES(J,K).GT.0.D0) I = I + 1
            ENDDO
          ENDIF
          NLAYER%I(J)=I
!         CHECKING ALL LAYERS AND CORRECTING AVAIL
!         DUE TO POSSIBLE SHIFT OF SINGLE PRECISION STORAGE
          DO K=1,NLAYER%I(J)
            TEST1=0.D0
            DO I=1,NSICLA
              TEST1=TEST1+AVAIL(J,K,I)
            ENDDO
            IF(TEST1.GT.1.D-10.AND.(TEST1-1.D0)**2.GT.1.D-10) THEN
              DO I=1,NSICLA
                AVAIL(J,K,I)=AVAIL(J,K,I)/TEST1
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ELSE
!
        CALL INIT_COMPO(IT1%I)
!
        DO J=1,NPOIN
!
!       NOMBLAY IS THE MAXIMUM NUMBER OF LAYERS ALLOWED
!
        NLAYER%I(J) = IT1%I(J)
        IF(NLAYER%I(J).GT.NOMBLAY) THEN
          WRITE(LU,1815) NOMBLAY
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       THE HEIGHT OF SEDIMENT (SUM OF ES) MUST NOT BE MORE THAN ZF-ZR
!       IF SO, THE HEIGHT OF THE LAST LAYER IS REDUCED
!       IF THERE ARE LAYERS UNDER ZR, THEY ARE NOT TAKEN INTO ACCOUNT
        HAUTSED = 0.D0
        DO K=1,IT1%I(J)
          IF(HAUTSED + ES(J,K) .GE. ZF%R(J) - ZR%R(J)) THEN
            ES(J,K) = ZF%R(J) - ZR%R(J) -  HAUTSED
            NLAYER%I(J) = K
            HAUTSED = HAUTSED + ES(J,K)
            EXIT
          ENDIF
          HAUTSED = HAUTSED + ES(J,K)
        ENDDO
!
!       OTHER LAYERS SET TO 0.D0
!
        IF(NLAYER%I(J).LT.NOMBLAY) THEN
          DO K=NLAYER%I(J)+1,NOMBLAY
            ES(J,K) = 0.D0
          ENDDO
        ENDIF
!
!       THE THICKNESS OF THE LAST LAYER IS ENLARGED SO THAT
!       THE HEIGHT OF SEDIMENT (SUM OF ES) IS EQUAL TO ZF-ZR
!
        IF(HAUTSED.LT.ZF%R(J)-ZR%R(J)) THEN
          ES(J,NLAYER%I(J))=ES(J,NLAYER%I(J))+ZF%R(J)-ZR%R(J)-HAUTSED
        ENDIF
!
        IF(NLAYER%I(J).GT.1) THEN
!         IT IS ASSUMED THAT ELAY0 IS SMALLER THAN THE FIRST STRATUM
!         NEED TO ADD THE CASE WHERE ELAY0 IS LARGER
          IF(ELAY0.GT.ES(J,1)) THEN
            WRITE(LU,*) ' ACTIVE LAYER TOO BIG/STRATIFICATION'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
!       THE FIRST STRATUM IS SEPARATED BETWEEN ACTIVE LAYER + ACTIVE STRATUM
!
        IF(ELAY0.LT.ES(J,1)) THEN
          NLAYER%I(J) = NLAYER%I(J) + 1
          IF(NLAYER%I(J).GT.NOMBLAY) THEN
            WRITE(LU,1815) NOMBLAY
            CALL PLANTE(1)
            STOP
          ENDIF
!         INDICES FOR ES AND AVAIL NEED TO BE OFFSET
          IF(NLAYER%I(J).GT.2) THEN
            DO K=NLAYER%I(J),3,-1
              ES(J,K) = ES(J,K-1)
            ENDDO
          ENDIF
          ES(J,2) = ES(J,1) - ELAY0
          ES(J,1) = ELAY0
          DO I=1,NSICLA
            DO K=NLAYER%I(J),2,-1
              AVAIL(J,K,I) = AVAIL(J,K-1,I)
            ENDDO
          ENDDO
        ENDIF
!
        ENDDO !J
!
      ENDIF
!
      NMAXI=0
      DO J=1,NPOIN
!
!       CHECKS FOR BAD LAYER THICKNESSES
        DO K=1,NOMBLAY
          IF(ES(J,K).LT.0.D0) THEN
            WRITE(LU,*)'BAD LAYER THICKNESS',J,K ,' :',ES(J,K)
            CALL PLANTE(1)
            STOP
          ENDIF
!       CHECKS FOR BAD AVAI
          DO I=1,NSICLA
            IF(AVAIL(J,K,I).LT.0.D0.OR.AVAIL(J,K,I).GT.1.D0) THEN
              WRITE(LU,*)'BAD FRACTIONS',J,K,I ,' :',AVAIL(J,K,I)
              CALL PLANTE(1)
              STOP
            ENDIF
          END DO
        END DO
!
        ELAY%R(J) = ES(J,1)
        IF(NLAYER%I(J).GT.1) THEN
          ESTRAT%R(J) = ES(J,2)
        ENDIF
!
!       UNUSED AVAIL ARE FILLED WITH ZEROS (IS IT USEFUL ???)
!
        IF(NLAYER%I(J).LT.NOMBLAY) THEN
          DO I = 1, NSICLA
            DO K = NLAYER%I(J)+1,NOMBLAY
              AVAIL(J,K,I) = 0.D0
            ENDDO
          ENDDO
        ENDIF
        IF(NLAYER%I(J).GT.NMAXI) NMAXI = NLAYER%I(J)
      ENDDO
!
!     COMPUTES THE TOTAL VOLUME OF SEDIMENTS IN EACH CLASS
!
      DO I=1,NSICLA
        VOLTOT(I)=0.D0
        DO J=1,NPOIN
          DO K=1,NLAYER%I(J)
            VOLTOT(I) = VOLTOT(I) + ES(J,K)*AVAIL(J,K,I)*VOLU2D%R(J)
          ENDDO
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
      VOLINI = VOLTOT
!
      WRITE(LU,*) 'MAXIMUM INITIAL NUMBER OF LAYERS :',NMAXI
      DO I=1,NSICLA
        WRITE(LU,*)'TOTAL VOLUME OF CLASS ',I ,' :',VOLTOT(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
1815  FORMAT(1X,'THERE ARE MORE THAN ',1I3,' LAYERS IN STRATIFICATION')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
