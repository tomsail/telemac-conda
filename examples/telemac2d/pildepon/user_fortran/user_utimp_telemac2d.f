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
      INTEGER ERR,LTT
      INTEGER I,ILOCAL,IFILE,IFILEM,ITRAC
      INTEGER ISEG,GLO1,GLO2,NSON_U,NSON_D
      DOUBLE PRECISION X1,Y1,X2,Y2,HM,LONG,DENOM,RELATI
      DOUBLE PRECISION PRESS_F1,PRESS_F2,CIR1,CIR2
      DOUBLE PRECISION GPRDTIME1,RESTE, PERDUE
      DOUBLE PRECISION, PARAMETER :: GAMMAW= 9801.D0
      DOUBLE PRECISION, PARAMETER :: EPSS=1.E-10
      INTEGER, ALLOCATABLE :: SEGM1(:),SEGM2(:)
      DOUBLE PRECISION, ALLOCATABLE :: LONGS1(:),LONGS2(:)
!
      SAVE NSON_U, NSON_D
      SAVE SEGM1,SEGM2,LONGS1,LONGS2
!
!-----------------------------------------------------------------------
!
      IF(LT.EQ.0) THEN
        ALLOCATE(SEGM1(MESH%NSEG),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'SEGM1')
        ALLOCATE(LONGS1(MESH%NSEG),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'LONGS1')
        ALLOCATE(SEGM2(MESH%NSEG),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'SEGM2')
        ALLOCATE(LONGS2(MESH%NSEG),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'LONGS2')
!
        DO ISEG=1, MESH%NSEG
          SEGM1(ISEG)=0
          LONGS1(ISEG)=0.D0
          SEGM2(ISEG)=0
          LONGS2(ISEG)=0.D0
        ENDDO
        NSON_U = 0
        NSON_D = 0
!
! DISTINCTION LINEAR AND QUADRATIC ELEMENTS
!
        IF(U%ELM.EQ.13) THEN
          DO ISEG=1, MESH%NSEG
            GLO1 = MESH%GLOSEG%I(ISEG)
            GLO2 = MESH%GLOSEG%I(ISEG+2*MESH%NSEG)
            X1 = MESH%X%R(GLO1)
            X2 = MESH%X%R(GLO2)
            Y1 = MESH%Y%R(GLO1)
            Y2 = MESH%Y%R(GLO2)
!
! FIND POINTS INSIDE THE CIRCUMFERENCE:
! UPPER CYLINDER
!
            CIR1=(X1+5.D0)**2+(Y1-4.D0)**2
            CIR2=(X2+5.D0)**2+(Y2-4.D0)**2
            IF(CIR1.LE.4.1D0) THEN
              IF(CIR2.LE.4.1D0) THEN
                NSON_U = NSON_U+1
                SEGM1(NSON_U) = ISEG
                LONG=SQRT((X1-X2)**2+(Y1-Y2)**2)
                LONGS1(NSON_U) = LONG
              ENDIF
            ENDIF
! FIND POINTS INSIDE THE CIRCUMFERENCE:
! LOWER CYLINDER
            CIR1=(X1+5.D0)**2+(Y1+4.D0)**2
            CIR2=(X2+5.D0)**2+(Y2+4.D0)**2
            IF(CIR1.LE.4.1D0) THEN
              IF(CIR2.LE.4.1D0) THEN
                NSON_D = NSON_D+1
                SEGM2(NSON_D) = ISEG
                LONG=SQRT((X1-X2)**2+(Y1-Y2)**2)
                LONGS2(NSON_D) = LONG
              ENDIF
            ENDIF
          ENDDO
        ELSEIF(U%ELM.EQ.11.OR.EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
          DO ISEG=1, MESH%NSEG
            GLO1 = MESH%GLOSEG%I(ISEG)
            GLO2 = MESH%GLOSEG%I(ISEG+MESH%NSEG)
            X1 = MESH%X%R(GLO1)
            X2 = MESH%X%R(GLO2)
            Y1 = MESH%Y%R(GLO1)
            Y2 = MESH%Y%R(GLO2)
!
! FIND POINTS INSIDE THE CIRCUMFERENCE:
! UPPER CYLINDER
!
            CIR1=(X1+5.D0)**2+(Y1-4.D0)**2
            CIR2=(X2+5.D0)**2+(Y2-4.D0)**2
            IF(CIR1.LE.4.1D0) THEN
              IF(CIR2.LE.4.1D0) THEN
                NSON_U = NSON_U+1
                SEGM1(NSON_U) = ISEG
                LONG=SQRT((X1-X2)**2+(Y1-Y2)**2)
                LONGS1(NSON_U) = LONG
              ENDIF
            ENDIF
! FIND POINTS INSIDE THE CIRCUMFERENCE:
! LOWER CYLINDER
            CIR1=(X1+5.D0)**2+(Y1+4.D0)**2
            CIR2=(X2+5.D0)**2+(Y2+4.D0)**2
            IF(CIR1.LE.4.1D0) THEN
              IF(CIR2.LE.4.1D0) THEN
                NSON_D = NSON_D+1
                SEGM2(NSON_D) = ISEG
                LONG=SQRT((X1-X2)**2+(Y1-Y2)**2)
                LONGS2(NSON_D) = LONG
              ENDIF
            ENDIF
          ENDDO
        ELSE
          IF(LNG.EQ.1) WRITE(LU,98) U%ELM
          IF(LNG.EQ.2) WRITE(LU,99) U%ELM
98        FORMAT(1X,'UTIMP: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99        FORMAT(1X,'UTIMP: IELM=',1I6,
     &          ' TYPE OF ELEMENT NOT AVAILABLE')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF !LT.EQ.0
!
! USING LIST OF SEGMENTS TO COMPUTE THE FORCE ON CYLINDER
!
      IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!
      IF(U%ELM.EQ.13) THEN
        IFILE = T2D_FILES(T2DRFO)%LU
        PRESS_F1=0.D0
        IF(NSON_U.GT.0) THEN
! SAMPLING AFTER t=600s ("REGULAR" SOLUTION)
          IF(AT.GE.600.D0) THEN
            DO I=1,NSON_U
              ISEG=SEGM1(I)
              GLO1=MESH%GLOSEG%I(ISEG)
              GLO2=MESH%GLOSEG%I(ISEG+2*MESH%NSEG)
              HM=(H%R(GLO1)+H%R(GLO2))/2.D0
              PRESS_F1=PRESS_F1+GAMMAW*HM**2*LONGS1(I)/2.D0
            ENDDO
          ENDIF
        ENDIF
        IF(NCSIZE.GT.0) PRESS_F1=P_SUM(PRESS_F1)
!
        PRESS_F2=0.D0
        IF(NSON_D.GT.0) THEN
          IF(AT.GE.600.D0) THEN
            DO I=1,NSON_D
              ISEG=SEGM2(I)
              GLO1=MESH%GLOSEG%I(ISEG)
              GLO2=MESH%GLOSEG%I(ISEG+2*MESH%NSEG)
              HM=(H%R(GLO1)+H%R(GLO2))/2.D0
              PRESS_F2=PRESS_F2+GAMMAW*HM**2*LONGS2(I)/2.D0
            ENDDO
          ENDIF
        ENDIF
        IF(NCSIZE.GT.0) PRESS_F2=P_SUM(PRESS_F2)
        IF (AT.GE.600.D0.AND.IPID.EQ.0) THEN
          WRITE (IFILE,*) PRESS_F1, PRESS_F2
        ENDIF
!
      ELSEIF(U%ELM.EQ.11) THEN
        IFILE = T2D_FILES(T2DRFO)%LU
        PRESS_F1=0.D0
        IF(NSON_U.GT.0) THEN
! SAMPLING AFTER t=600s ("REGULAR" SOLUTION)
          IF(AT.GE.600.D0) THEN
            DO I=1,NSON_U
              ISEG=SEGM1(I)
              GLO1=MESH%GLOSEG%I(ISEG)
              GLO2=MESH%GLOSEG%I(ISEG+MESH%NSEG)
              HM=(H%R(GLO1)+H%R(GLO2))/2.D0
              PRESS_F1=PRESS_F1+GAMMAW*HM**2*LONGS1(I)/2.D0
            ENDDO
          ENDIF
        ENDIF
        IF(NCSIZE.GT.0) PRESS_F1=P_SUM(PRESS_F1)
!
        PRESS_F2=0.D0
        IF(NSON_D.GT.0) THEN
          IF(AT.GE.600.D0) THEN
            DO I=1,NSON_D
              ISEG=SEGM2(I)
              GLO1=MESH%GLOSEG%I(ISEG)
              GLO2=MESH%GLOSEG%I(ISEG+MESH%NSEG)
              HM=(H%R(GLO1)+H%R(GLO2))/2.D0
              PRESS_F2=PRESS_F2+GAMMAW*HM**2*LONGS2(I)/2.D0
            ENDDO
          ENDIF
        ENDIF
        IF(NCSIZE.GT.0) PRESS_F2=P_SUM(PRESS_F2)
        IF (AT.GE.600.D0.AND.IPID.EQ.0) THEN
          WRITE (IFILE,*) PRESS_F1, PRESS_F2
        ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,100) U%ELM
        IF(LNG.EQ.2) WRITE(LU,101) U%ELM
100      FORMAT(1X,'UTIMP: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
101      FORMAT(1X,'UTIMP: IELM=',1I6,
     &        ' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
! WRITE FINAL MASS BALANCE INTO A FILE
!
      IF(LT.EQ.NIT.AND.IPID.EQ.0) THEN
        IF(NTRAC.GT.0) THEN
          CALL GET_FREE_ID(IFILEM)
! Get ID of    FORMATTED RESULTS FILE 1
          IFILEM = T2D_FILES(T2DRF1)%LU
          DO ITRAC=1,NTRAC
            PERDUE = MASTR0(ITRAC)+MASTEN(ITRAC)+MASTOU(ITRAC)
     &               -MASTR2(ITRAC)
            DENOM = MAX(ABS(MASTR0(ITRAC)),ABS(MASTR2(ITRAC)),
     &              ABS(MASTEN(ITRAC)),ABS(MASTOU(ITRAC)))
            IF(DENOM.GT.1.D-8) THEN
              RELATI = PERDUE / DENOM
            ENDIF
            IF(ITRAC.NE.NTRAC) THEN
! WRITE IN IT
              WRITE(UNIT=IFILEM,FMT=404) MASTR0(ITRAC),
     &              MASTR2(ITRAC),MASTEN(ITRAC)+MASTOU(ITRAC),
     &              PERDUE,RELATI
404           FORMAT(G16.7,'&',G16.7,'&',G16.7,'&',G16.7,'&',G16.7,'\\')
            ELSE
              WRITE(UNIT=IFILEM,FMT=405) MASTR0(ITRAC),MASTR2(ITRAC),
     &              MASTEN(ITRAC)+MASTOU(ITRAC),PERDUE,
     &              RELATI
405           FORMAT(G16.7,'&',G16.7,'&',G16.7,'&',G16.7,'&',G16.7)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
      ELSE
!
! FVM
!
! SAMPLING EVERY 0.1 S
        GPRDTIME1=0.1D0
        IF(GPRDTIME1.LT.EPSS)THEN
          WRITE(LU,*)'ERROR IN UTIMP'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(AT.GE.600.D0)THEN
!
          LTT=CEILING(AT/GPRDTIME1)
          RESTE=(LTT*GPRDTIME1-AT)/GPRDTIME1
          IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                 CASE WHERE RESTE=1
     &      LT.EQ.NIT)THEN
!
! WRITE IN THE FILE
            IFILE = T2D_FILES(T2DRFO)%LU
            PRESS_F1=0.D0
            IF(NSON_U.GT.0) THEN
              DO I=1,NSON_U
                ISEG=SEGM1(I)
                GLO1=MESH%GLOSEG%I(ISEG)
                GLO2=MESH%GLOSEG%I(ISEG+MESH%NSEG)
                HM=(H%R(GLO1)+H%R(GLO2))/2.D0
                PRESS_F1=PRESS_F1+GAMMAW*HM**2*LONGS1(I)/2.D0
              ENDDO
            ENDIF
            IF(NCSIZE.GT.0) PRESS_F1=P_SUM(PRESS_F1)
!
            PRESS_F2=0.D0
            IF(NSON_D.GT.0) THEN
              DO I=1,NSON_D
                ISEG=SEGM2(I)
                GLO1=MESH%GLOSEG%I(ISEG)
                GLO2=MESH%GLOSEG%I(ISEG+MESH%NSEG)
                HM=(H%R(GLO1)+H%R(GLO2))/2.D0
                PRESS_F2=PRESS_F2+GAMMAW*HM**2*LONGS2(I)/2.D0
              ENDDO
            ENDIF
            IF(NCSIZE.GT.0) PRESS_F2=P_SUM(PRESS_F2)
            IF(IPID.EQ.0) WRITE (IFILE,*) PRESS_F1, PRESS_F2
          ENDIF
        ENDIF !AT.GE.600.D0
!
      ENDIF !SAINT-VENANT VF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
