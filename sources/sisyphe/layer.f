!                   ****************
                    SUBROUTINE LAYER
!                   ****************
!
     &(ZFCL_W,NLAYER,ZR,ZF,ESTRAT,ELAY,MASBAS,ACLADM,NSICLA,NPOIN,
     & ELAY0,VOLTOT,ES,AVAIL,CONST_ALAYER,ESTRATNEW,NLAYNEW)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES AVAIL FOR EACH CLASS AND EACH LAYER;
!+                NEW STRATUM THICKNESS ESTRAT.
!+
!+            ACTIVE LAYER IS LAYER 1, IT IS KEPT AT A PRESCRIBED
!+                HEIGHT OF ELAY0  PROVIDED IT IS POSSIBLE
!+
!+            STRATUM IS LAYER 2 OF HEIGHT ESTRAT.
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
!+
!+
!
!history  JMH
!+        16/09/2009
!+
!+   AVAIL(NPOIN,10,NSICLA)
!
!history  JMH
!+        10/05/2010
!+        V6P0
!+   CASE WITH DEPOSITION REWRITTEN, TESTS CHANGED.
!
!history  N. DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N. DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (LNHE)
!+        12/04/2011
!+        V6P0
!+   One bug corrected in case of restart, and a formula made clearer
!+   Look for 12/04/2011...
!
!history  C. VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!history  C. VILLARET (EDF-LNHE)
!+        18/01/2012
!+        V6P1
!+  Dimension 10 of AVAIL changed into NOMBLAY
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  R.KOPMANN (BAW)
!+        Dezember 2017
!+        V7P2
!+   Zeroed the layer thicknesses if a layer was destroyed and setting
!+   the first class to 1 and all others to zero
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AVAIL          |<->| SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
!| CONST_ALAYER   |-->| CONSTANT ACTIVE LAYER THICKNESS OR NOT
!| ELAY           |<->| ACTIVE LAYER THICKNESS FOR EACH POINT
!| ELAY0          |<->| ACTIVE LAYER THICKNESS
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| ESTRAT         |<->| ACTIVE STRATUM THICKNESS FOR EACH POINT
!| ESTRATNEW      |<->| ACTIVE STRATUM THICKNESS AT TIME T+DT
!| MASBAS         |-->| INTEGRAL OF TEST FUNCTIONS
!| NLAYER         |<--| NUMBER OF LAYER FOR EACH POINT
!| NLAYNEW        |<->| NUMBER OF LAYER AT TIME T+DT
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| VOLTOT         |<->| TOTAL VOLUME OF SEDIMENT IN THE BED
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFCL_W         |-->| BED EVOLUTION FOR EACH SEDIMENT CLASS
!| ZR             |-->| NON ERODABLE BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE , ONLY : NOMBLAY
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM,P_ISUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZFCL_W,ZR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASBAS,ACLADM
      INTEGER,          INTENT(IN)    :: NSICLA,NPOIN
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: NLAYER,ESTRAT,ELAY
      DOUBLE PRECISION, INTENT(INOUT) :: ELAY0
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLTOT(NSICLA),ESTRATNEW(NPOIN)
      INTEGER         , INTENT(INOUT) :: NLAYNEW(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,ARRET,ARRET2
      DOUBLE PRECISION EVOL,HEIGH,TEST1,TEST2,AUX
!
!-----------------------------------------------------------------------
!
!     TO CHECK FRACTIONS IN THE RANGE [-ZERO,1+ZERO]
!
!     DATA             ZERO/1.D-10/
!     IN CASE OF RESTART, THE FIRST TIME STEP IS A BIT HARD BECAUSE OF
!     SINGLE PRECISION, WITHOUT RESTART 1.D-10 IS OK
      DOUBLE PRECISION, PARAMETER :: ZERO = 1.D-7
!
!-----------------------------------------------------------------------
!
      ARRET=0
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
!       ACTIVATE BEFORE INVESTIGATING PROBLEMS IN LAYER...
!       LOOK FOR 'ACTIVATE' TO SEE OTHER LINES LIKE THIS BELOW
!
!       IF(ELAY%R(J).LT.0.D0) THEN
!         WRITE(LU,*) 'NEGATIVE ELAY IN LAYER J=',J,' ELAY=',ELAY%R(J)
!         CALL PLANTE(1)
!         STOP
!       ENDIF
!
        IF(.NOT.CONST_ALAYER) ELAY0 = 3.D0 * ACLADM%R(J)
!
        NLAYNEW(J) = NLAYER%I(J)
!
!       TODO: QUESTION JMH, EVOLUTION HAS BEEN COMPUTED BEFORE IN ARRAY E, WHY NOT
!               EVOL=E(J) ?????
!               ELAY(J) = ES(J,1) WHY IS IT AN EXTRA ARRAY ??
!
!
        HEIGH = ZF%R(J)-ZR%R(J)
!
!       ACTIVATE BEFORE INVESTIGATING PROBLEMS IN LAYER...
!
!       IF(HEIGH.LT.0.D0) THEN
!         WRITE(LU,*) 'BAD DATA IN LAYER J=',J,' HEIGH=',HEIGH
!         CALL PLANTE(1)
!         STOP
!       ENDIF
!
!       HERE ELAY.NE.HEIGH BECAUSE ELAY IS THE ACTIVE LAYER THICKNESS
        EVOL  = 0.D0
        DO I=1,NSICLA
          EVOL = EVOL + ZFCL_W%ADR(I)%P%R(J)
        ENDDO
!
        IF(NLAYER%I(J).GT.1) THEN
!
          IF(EVOL.GE.0.D0) THEN
!
!           DEPOSITION
!
!           NEW HEIGHT OF LAYER 2 (IT RECEIVES EVOL TO KEEP LAYER 1 CONSTANT)
            ESTRATNEW(J) = ESTRAT%R(J) + EVOL
!
            DO I=1,NSICLA
!             THE OLD IMPLEMENTATION CONSISTED OF FIRST
!             GIVING EVOL TO LAYER 2, WITH OLD AVAIL, THEN OF RECEIVING
!             THE DEPOSITION, BUT IF A CLASS DISAPPEARS IN LAYER 1,
!             IT IS NOT POSSIBLE TO GIVE IT FIRST TO LAYER 2, SO NEW
!             FRACTIONS MUST BE COMPUTED BEFORE GIVING EVOL TO LAYER 2
!             THEN I SEE NO DIFFERENCE BETWEEN EVOLELAY0
!             SO BOTH ARE TREATED BELOW, UNLIKE RELEASE 5.9.
!
!             1) LAYER 1 RECEIVES ZFCL_W OF CLASS I, WE COMPUTE THE
!                PROVISIONAL NEW AVAIL(J,1,I) IN AUX
              AUX=(AVAIL(J,1,I)*ELAY0+ZFCL_W%ADR(I)%P%R(J))/
     &                        (ELAY0+EVOL)
!
!             2) LAYER 2 RECEIVES AUX*EVOL OF CLASS I (AUX MAY BE 0 HERE)
              AVAIL(J,2,I)=(AUX*EVOL+AVAIL(J,2,I)*ESTRAT%R(J))/
     &                               ESTRATNEW(J)
!
!             3) SEEN FROM LAYER 1, AUX*EVOL OF CLASS I HAS BEEN GIVEN
!                AND THE NEW LAYER THICKNESS IS ELAY0, HENCE THE NEW AVAIL
              AVAIL(J,1,I)=( AVAIL(J,1,I)*ELAY0-AUX*EVOL
     &                          +ZFCL_W%ADR(I)%P%R(J) )/ELAY0
!
! NOTE CV: CAN BE REPLACED BY
!              AVAIL(J,1,I)= AUX
!
!             OLD (AND WRONG) FORMULATION (IN IT -AVAIL*EVOL SHOULD BE -AUX*EVOL)
!             AVAIL(J,1,I)=( AVAIL(J,1,I)*(ELAY0-EVOL)
!    &                       +ZFCL_W%ADR(I)%P%R(J)     )/ELAY0
!
              IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
     &           AVAIL(J,1,I).LT.-ZERO) THEN
                WRITE(LU,*) 'ERROR IN LAYER CASE 1'
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDDO
!           NEW HEIGHT OF LAYER 1
            ELAY%R(J) = ELAY0
!
          ELSEIF(EVOL.GT.-ELAY0) THEN
! CV: I DON'T AGREE WITH THE COMMENT BELOW
!     WE'RE IN THE CASE : -ELAY0<EVOL<0 HENCE ELAY0>-EVOL>0
!     THICKNESS OF THE FIRST LAYER IS THEREFORE SUFFICIENT
!
!           EROSION GREATER THAN LAYER 1, WE HAVE TO DESTROY A STRATUM
!
            IF(-EVOL.GT.ESTRAT%R(J)) THEN
!
!  CV: USUALLY, LAYER 2 IS VERY BROAD AND TWO LAYERS ARE IN GENERAL SUFFICIENT
!      HERE LAYER 2 IS DESTROYED
!
!             USUAL CASE (NOTE JMH : WHY NOT .GE.2 ?
!
              IF(NLAYER%I(J).GT.2) THEN
!
                DO I=1,NSICLA
                  AVAIL(J,1,I) = (  AVAIL(J,1,I)*ELAY0
     &                            + ZFCL_W%ADR(I)%P%R(J)
     &                            + AVAIL(J,2,I)*ESTRAT%R(J)
     &                            - AVAIL(J,3,I)*(EVOL+ESTRAT%R(J))
     &                           )/ ELAY0
                  IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
     &               AVAIL(J,1,I).LT.-ZERO) THEN
                    WRITE(LU,*) 'ERROR IN LAYER CASE 2'
                    CALL PLANTE(1)
                    STOP
                  ENDIF
                  AVAIL(J,2,I) = AVAIL(J,3,I)
                  DO K=3,MIN(NOMBLAY-1,NLAYER%I(J))
                    AVAIL(J,K,I) = AVAIL(J,K+1,I)
                  ENDDO
                ENDDO
                ELAY%R(J) = ELAY0
                NLAYNEW(J) = NLAYER%I(J) - 1
                ESTRATNEW(J) = ESTRAT%R(J) + EVOL + ES(J,3)
                DO K=3,MIN(NOMBLAY-1,NLAYER%I(J))
                  ES(J,K) = ES(J,K+1)
                ENDDO
!               last layer must be zeroed, AVAIL: first class = 1, others = 0
                ES(J,NLAYER%I(J)) = 0.D0
                AVAIL(J,NLAYER%I(J),1) = 1.D0
                DO I=2,NSICLA
                  AVAIL(J,NLAYER%I(J),I) = 0.D0
                ENDDO


!
!             HERE NLAYER.GT.1 AND NOT .GT.2, SO 2 !
!
              ELSE
                DO I=1,NSICLA
                  IF(HEIGH.GT.0.D0) THEN
                    AVAIL(J,1,I) = (  AVAIL(J,1,I)*ELAY0
     &                              + ZFCL_W%ADR(I)%P%R(J)
     &                              + ESTRAT%R(J)*AVAIL(J,2,I)
     &                              )/(ELAY0+EVOL+ESTRAT%R(J))
!                   MODIF JMH 12/04/2011, WITH 2 LAYERS, LAYER 1
!                   HAS ELAY%R(J)=ELAY0, SO WHY ELAY IN DENOMINATOR
!                   AND ELAY0 IN NUMERATOR ? (VICIOUS!!)
!    &                              )/(ELAY%R(J)+EVOL+ESTRAT%R(J))
                    IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
     &                 AVAIL(J,1,I).LT.-ZERO) THEN
                      WRITE(LU,*) 'J=',J,' NLAYER%I(J)=',NLAYER%I(J)
                      WRITE(LU,*) 'AVAIL(J,1,I)=',AVAIL(J,1,I)
                      WRITE(LU,*) 'AVAIL(J,2,I)=',AVAIL(J,2,I)
                      WRITE(LU,*) 'ZFCL=',ZFCL_W%ADR(I)%P%R(J)
                      WRITE(LU,*) 'HEIGH=',HEIGH,' ELAY0=',ELAY0
                      WRITE(LU,*) 'ESTRAT%R(J)=',ESTRAT%R(J)
                      WRITE(LU,*) 'ELAY%R(J)=',ELAY%R(J)
                      WRITE(LU,*) 'EVOL=',EVOL
                      WRITE(LU,*) 'ERROR IN LAYER CASE 3'
                      CALL PLANTE(1)
                      STOP
                    ENDIF
                  ELSE
                    AVAIL(J,1,I) = 0.D0
! Fraction 1 should be 1, if all the others are zero
                    AVAIL(J,1,1) = 1.D0
                  ENDIF
                  AVAIL(J,2,I) = 0.D0
                ENDDO
! Fraction 1 should be 1, if all the others are zero
                AVAIL(J,2,1) = 1.D0
                NLAYNEW(J) = NLAYER%I(J) - 1
                ELAY%R(J) = HEIGH
                ESTRATNEW(J) = 0.D0
!           a layer was just canceled, the layer thickness of the canceled layer
!           should be set to zero for graphic printout and restart
                ES(J,NLAYNEW(J)+1) = 0.D0
              ENDIF
!
!           ONLY LAYER 1 ERODED
!
            ELSE
              DO I=1,NSICLA
                AVAIL(J,1,I) = (  AVAIL(J,1,I) * ELAY0
     &                          + ZFCL_W%ADR(I)%P%R(J)
     &                          - EVOL*AVAIL(J,2,I)    )/ELAY0
                IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
     &             AVAIL(J,1,I).LT.-ZERO) THEN
                  WRITE(LU,*) 'ERROR IN LAYER CASE 4'
                  CALL PLANTE(1)
                  STOP
                ENDIF
              ENDDO
              ELAY%R(J) = ELAY0
              ESTRATNEW(J) = ESTRAT%R(J) + EVOL
            ENDIF
!
          ELSE
!
!           STOPS IF EROSION IS GREATER THAN
!           THICKNESS OF THE ACTIVE LAYER!
!
            WRITE(LU,*) 'TOO MUCH EROSION AT POINT J=',J
            WRITE(LU,*) 'DECREASE DT OR INCREASE ELAY0'
            WRITE(LU,*) 'EVOL=', EVOL, 'ELAY0=',ELAY0
            CALL PLANTE(1)
            STOP
!
!         END OF TESTS ON EVOL
!
          ENDIF
!
! THERE WAS ONLY ONE LAYER
! ------------------------
!
        ELSE
!
!         IT IS NOW BIG ENOUGH TO MAKE TWO LAYERS
!
          IF(HEIGH.GT.ELAY0) THEN
            NLAYNEW(J) = 2
            ESTRATNEW(J) = HEIGH - ELAY0
            ELAY%R(J) = ELAY0
            DO I=1,NSICLA
              AVAIL(J,2,I) = AVAIL(J,1,I)
              AVAIL(J,1,I) = (AVAIL(J,1,I) * (ELAY0-EVOL)
     &                     + ZFCL_W%ADR(I)%P%R(J) )/ELAY0
              IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
     &          AVAIL(J,1,I).LT.-ZERO) THEN
                WRITE(LU,*) 'ERROR IN LAYER CASE 5'
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDDO
!
! IF THERE REMAINS ONLY ONE LAYER
! -------------------------------
!
          ELSE
!           NOTE JMH: THE TRICKIEST PART...
!           THE PROBLEM OF 0/0 CREATED BY THE CHOICE OF AVAIL
!           AS MAIN VARIABLE...
            IF(ELAY%R(J)+EVOL.GT.1.D-15) THEN
              DO I=1,NSICLA
!               AUX=AVAIL(J,1,I)
                AVAIL(J,1,I) = (AVAIL(J,1,I)*ELAY%R(J)+
     &                          ZFCL_W%ADR(I)%P%R(J))
     &                          / (ELAY%R(J)+EVOL)
!               IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
!    &            AVAIL(J,1,I).LT.-ZERO) THEN
!                 WRITE(LU,*) 'ERROR IN LAYER CASE 6'
!                 WRITE(LU,*) 'INITIAL AVAIL=',AUX
!                 WRITE(LU,*) 'J=',J,' CLASS ',I
!                 WRITE(LU,*) 'EVOL=',EVOL,' ELAY=',ELAY%R(J)
!                 WRITE(LU,*) 'EVOL+ELAY=',EVOL+ELAY%R(J)
!                 WRITE(LU,*) 'ZFCL=',ZFCL_W%ADR(I)%P%R(J)
!                 WRITE(LU,*) 'DENOMINATOR=',ELAY%R(J)+EVOL
!                 WRITE(LU,*) 'NUMERATOR=',AUX*ELAY%R(J)+
!    &                                     ZFCL_W%ADR(I)%P%R(J)
!               ENDIF
                AVAIL(J,2,I) = 0.D0
              ENDDO
              IF(ELAY%R(J)+EVOL.LT.1.D-5) THEN
!               PLAYING WITH ZEROES, RISK OF SUM NOT EQUAL TO 1
!               ONLY BECAUSE OF TRUNCATION ERRORS, WE NORMALISE
                TEST1=0.D0
                DO I=1,NSICLA
                  AVAIL(J,1,I)=MAX(0.D0,MIN(1.D0,AVAIL(J,1,I)))
                  TEST1=TEST1+AVAIL(J,1,I)
                ENDDO
                IF((TEST1-1.D0)**2.GT.ZERO) THEN
                  DO I=1,NSICLA
                    AVAIL(J,1,I)=AVAIL(J,1,I)/MAX(TEST1,1.D-21)
                  ENDDO
                ENDIF
              ENDIF
            ELSE
!             ACLADM BEING RECOMPUTED WITH AVAIL
              AVAIL(J,1,1)=1.D0
              AVAIL(J,2,1)=1.D0
              DO I=2,NSICLA
                AVAIL(J,1,I) = 0.D0
                AVAIL(J,2,I) = 0.D0
              ENDDO
            ENDIF
            ELAY%R(J) = HEIGH
            ESTRATNEW(J) = 0.D0
            NLAYNEW(J) = 1
!RK
            DO I=2,NOMBLAY
              ES(J,I) = 0.D0
            END DO
!
          ENDIF
        ENDIF
!
      NLAYER%I(J) = NLAYNEW(J)
      ESTRAT%R(J) = ESTRATNEW(J)
      ES(J,1) = ELAY%R(J)
!     IN CASE OF RESTART, ES(J,2)
!     WILL BE STORED IN A FILE AND LOOKED AT TO COUNT THE
!     NUMBER OF LAYERS
      ES(J,2) = ESTRAT%R(J)
!
      TEST1 = 0.D0
      TEST2 = 0.D0
!
      DO I=1,NSICLA
        DO K=1,NLAYER%I(J)
!         CHECKS THAT AVAIL IS IN THE RANGE (-ZERO,1+ZERO)
          IF(AVAIL(J,K,I).GT.1.D0+ZERO.OR.AVAIL(J,K,I).LT.-ZERO) THEN
            WRITE(LU,*) 'ERROR ON FRACTIONS'
            WRITE(LU,*) 'LAYER ',K,' CLASS ',I,' POINT ',J
            IF(AVAIL(J,K,I).LT.0.D0) THEN
              WRITE(LU,*) 'AVAIL=' ,AVAIL(J,K,I)
            ELSE
              WRITE(LU,*) 'AVAIL-1=' ,AVAIL(J,K,I)-1.D0
            ENDIF
            WRITE(LU,*) 'ZFCL=',ZFCL_W%ADR(I)%P%R(J)
            WRITE(LU,*) 'EVOL=',EVOL,' ELAY=',ELAY%R(J)
            ARRET=1
          ELSE
!           ONCE CHECKED THAT WE HAVE ONLY TRUNCATION ERRORS, CLIPS
            AVAIL(J,1,I)=MAX(AVAIL(J,1,I),0.D0)
            AVAIL(J,1,I)=MIN(AVAIL(J,1,I),1.D0)
          ENDIF
        ENDDO
        TEST1 = TEST1 + AVAIL(J,1,I)
        TEST2 = TEST2 + AVAIL(J,2,I)
      ENDDO
!
!     CHECKS THAT SUM OF AVAIL IS 1 FOR FIRST 2 LAYERS
!
      IF(TEST1.GT.ZERO.AND.(TEST1-1.D0)**2>ZERO) THEN
        WRITE(LU,*) ' PROBLEM IN LAYER: J,TEST1',J,TEST1
        WRITE(LU,*) ' IN LAYER 1 SUM OF FRACTIONS NOT 1'
        ARRET=1
      ENDIF
      IF(TEST2.GT.ZERO.AND.(TEST2-1.D0)**2>ZERO) THEN
        WRITE(LU,*) ' PROBLEM IN LAYER: J,TEST2',J,TEST2
        WRITE(LU,*) ' IN LAYER 2 SUM OF FRACTIONS IS NOT 1'
        ARRET=1
      ENDIF
!
!     END OF LOOP ON ALL POINTS
!
      ENDDO
!
!     COMPUTES THE TOTAL VOLUME OF SEDIMENTS IN THE DOMAIN
!
      DO I = 1, NSICLA
        VOLTOT(I) = 0.D0
      ENDDO
      DO I=1,NSICLA
        DO J=1,NPOIN
          DO K=1,NLAYER%I(J)
            VOLTOT(I) = VOLTOT(I) + ES(J,K)*AVAIL(J,K,I)*MASBAS%R(J)
          ENDDO
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CLEAN STOP FOR ALL PROCESSORS IF PROBLEM
!
      ARRET2=ARRET
      IF(NCSIZE.GT.1) ARRET2=P_ISUM(ARRET)
      IF(ARRET2.GT.0) THEN
        WRITE(LU,*) 'STOP AFTER AN ERROR IN LAYER'
        IF(ARRET.EQ.0) THEN
          WRITE(LU,*) 'IN ',ARRET2,' PROCESSOR(S)'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
