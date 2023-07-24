!                   *************************
                    SUBROUTINE SECTION_KHIONE
!                   *************************
     &(MESH,IKLE,NELMAX,IFABOR,F,U,V,QVC,CA,CV,H,S,LT)
!
!***********************************************************************
! TELEMAC2D   V8P2                                   23/06/2020
!***********************************************************************
!
!brief    Computes caracteristics of the clogged section
!+        Inspired by the algorithm of flusec_telemac2d
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CA             |-->| WET AREA OF THE SECTION
!| CV             |-->| AVERAGE CONCENTRATION ON THE SECTION
!| F              |-->| TRACER VALUES
!| H              |-->| WATER DEPTH
!| IFABOR         |-->| TABLE OF ELEMENT NEIGHBORS
!| IKLE           |-->| CONNECTIVITY TABLE
!| LT             |-->| NUMBER OF TIME STEPS
!| MESH           |-->| MESH STRUCTURE
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENT
!| QVC            |-->| COMPUTED FRAZIL FLUX ACROSS THE SECTION
!| S              |-->| VOID STRUCTURE
!| U              |-->| X-COMPONENT OF THE VELOCITY
!| V              |-->| Y-COMPONENT OF THE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_KHIONE, ONLY: NSECLOG,SECLOG,MSKSEC,T1,T2,T3,
     &                               BM1,BM2,CV1,CV2,CV3,CV4,IND_FRA,
     &                               CLOG_TLGTH,NFRCLOG,T4,T5,UN1,UN2,
     &                               NSEG_FS,LISTE_FS
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_KHIONE, EX_SECTION_KHIONE => SECTION_KHIONE
      USE INTERFACE_PARALLEL, ONLY : P_DSUM,P_DMIN,P_DMAX,P_IMIN
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NELMAX,LT
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),IFABOR(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: QVC(NSECLOG)
      DOUBLE PRECISION, INTENT(INOUT) :: CA(NSECLOG),CV(NSECLOG)
      TYPE(BIEF_OBJ), INTENT(IN) :: F,U,V,H,S
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, PARAMETER :: NSEMAX = 200
!
      INTEGER NCP,ISEC,DEP,ARR,PT,IELEM,I1,I2,I3,ELBEST,IGBEST,ILBEST
      INTEGER ILPREC,ISEG,J1,J2,J3,ERR,IEL,IELMH,IELMU,II
      DOUBLE PRECISION DIST,DIST1,DIST2,DIST3,X1,X2
      DOUBLE PRECISION Y1,Y2,NORM,L2(NSECLOG),DTMP1,DTMP2
!
      LOGICAL OK
!
!-----------------------------------------------------------------------
!
      CALL OS('X=C     ', X=T3, C=1.D0)
      CALL OS('X=Y     ', X=T2, Y=H )
      CALL OS('X=YZ    ', X=T1, Y=H, Z=F%ADR(IND_FRA)%P )
      NCP=NSECLOG*2
!
!  SEARCHES PATHS JOINING POINT PAIRS:
!
!
      IF(LT.EQ.1) THEN

        ALLOCATE(NSEG_FS(NCP)            ,STAT=ERR)
        ALLOCATE(LISTE_FS(NCP,NSEMAX,2)  ,STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, "SEC_KHIONE:LISTE_FS")
!
        DO ISEC =1,NSECLOG
!       IN THE SERIAL CASE, OR "CLASSICAL" IN PARALLEL,
!       FOLLOW THE ALGORITHM OF FINDING SEGMENT CHAINS
!
!       NOTE: IF YOU CHANGE THE ALGORITHM, CHANGE IT IN PARTEL AS WELL
!
          DEP = SECLOG(1+2*(ISEC-1))
          ARR = SECLOG(2+2*(ISEC-1))
          IF(NCSIZE.GT.1) THEN
            DEP=GLOBAL_TO_LOCAL_POINT(DEP,MESH)
            ARR=GLOBAL_TO_LOCAL_POINT(ARR,MESH)
          ENDIF
          IF(DEP.GT.0) THEN
            X1 = MESH%X%R(DEP)
            Y1 = MESH%Y%R(DEP)
          ELSE
            X1 = 0.D0
            Y1 = 0.D0
          ENDIF
          IF(ARR.GT.0) THEN
            X2 = MESH%X%R(ARR)
            Y2 = MESH%Y%R(ARR)
          ELSE
            X2 = 0.D0
            Y2 = 0.D0
          ENDIF
          X1 = P_DSUM(X1)
          X2 = P_DSUM(X2)
          Y1 = P_DSUM(Y1)
          Y2 = P_DSUM(Y2)
          NORM = SQRT((X1-X2)**2+(Y1-Y2)**2)
          IF(NORM.LE.0.D0) THEN
            WRITE(LU,*) 'SECTION_KHIONE: SECTION TOO SHORT             '
            WRITE(LU,*) '                PROBABLY BECAUSE N_START=N_END'
            CALL PLANTE(1)
            STOP
          ENDIF
          CLOG_TLGTH(ISEC+NFRCLOG) = NORM
          UN1(ISEC) = (Y1 - Y2) / NORM
          UN2(ISEC) = (X2 - X1) / NORM
          IF(NCSIZE.GT.1) THEN
            IF(DEP.EQ.0.AND.ARR.EQ.0) THEN
              NSEG_FS(ISEC)=0
              CYCLE
            ENDIF
            IF((DEP.EQ.0.AND.ARR.NE.0).OR.(DEP.NE.0.AND.ARR.EQ.0)) THEN
              NSEG_FS(ISEC)=-1
              CYCLE
            ENDIF
          ENDIF
          PT = DEP
          ISEG = 0
          DIST=(MESH%X%R(DEP)-MESH%X%R(ARR))**2+
     &         (MESH%Y%R(DEP)-MESH%Y%R(ARR))**2
!
10        CONTINUE
!
!
          DO IELEM =1,MESH%NELEM
!
            I1 = IKLE(IELEM,1)
            I2 = IKLE(IELEM,2)
            I3 = IKLE(IELEM,3)
!           IF THE ELEMENT CONTAINS THE SELECTED NODE:
            IF(PT.EQ.I1.OR.PT.EQ.I2.OR.PT.EQ.I3) THEN
              DIST1 = (MESH%X%R(I1)-MESH%X%R(ARR))**2 +
     &                (MESH%Y%R(I1)-MESH%Y%R(ARR))**2
              DIST2 = (MESH%X%R(I2)-MESH%X%R(ARR))**2 +
     &                (MESH%Y%R(I2)-MESH%Y%R(ARR))**2
              DIST3 = (MESH%X%R(I3)-MESH%X%R(ARR))**2 +
     &                (MESH%Y%R(I3)-MESH%Y%R(ARR))**2
              IF(DIST1.LT.DIST) THEN
                DIST = DIST1
                ELBEST = IELEM
                IGBEST = I1
                ILBEST = 1
                IF(I1.EQ.PT) ILPREC = 1
                IF(I2.EQ.PT) ILPREC = 2
                IF(I3.EQ.PT) ILPREC = 3
              ENDIF
              IF(DIST2.LT.DIST) THEN
                DIST = DIST2
                ELBEST = IELEM
                IGBEST = I2
                ILBEST = 2
                IF(I1.EQ.PT) ILPREC = 1
                IF(I2.EQ.PT) ILPREC = 2
                IF(I3.EQ.PT) ILPREC = 3
              ENDIF
              IF(DIST3.LT.DIST) THEN
                DIST = DIST3
                ELBEST = IELEM
                IGBEST = I3
                ILBEST = 3
                IF(I1.EQ.PT) ILPREC = 1
                IF(I2.EQ.PT) ILPREC = 2
                IF(I3.EQ.PT) ILPREC = 3
              ENDIF
            ENDIF
!
          ENDDO ! IELEM
!
          IF(IGBEST.EQ.PT) THEN
            WRITE(LU,33)
33          FORMAT(1X,'SECTION_KHIONE : ALGORITHM FAILED')
            CALL PLANTE(1)
            STOP
          ELSE
            PT = IGBEST
            ISEG = ISEG + 1
            IF(ISEG.GT.NSEMAX) THEN
              WRITE(LU,*) 'SECTION_KHIONE: TOO MANY SEGMENTS IN A   '
              WRITE(LU,*) '                  SECTION. INCREASE  NSEMAX'
              CALL PLANTE(1)
              STOP
            ENDIF
            LISTE_FS(ISEC,ISEG,1) = IKLE(ELBEST,ILPREC)
            LISTE_FS(ISEC,ISEG,2) = IKLE(ELBEST,ILBEST)
            IF(IGBEST.NE.ARR) GO TO 10
          ENDIF
!
          NSEG_FS(ISEC) = ISEG
!
!         IF COMPATIBLE COMPUTATION OF FLUXES=YES
          IF(NSEG_FS(ISEC).GE.1) THEN
!
!         LOOKING AT ALL ELEMENTS TOUCHING THE SECTION WITH 2 POINTS
!         MARKING THEM WITH 1 ON ONE SIDE AND -1 ON THE OTHER SIDE
!
          DO IEL=1,MESH%NELEM
            MSKSEC%ADR(ISEC)%P%R(IEL)=0.D0
            J1=IKLE(IEL,1)
            J2=IKLE(IEL,2)
            J3=IKLE(IEL,3)
            DO ISEG=1,NSEG_FS(ISEC)
              I1 = LISTE_FS(ISEC,ISEG,1)
              I2 = LISTE_FS(ISEC,ISEG,2)
!             LEFT SIDE
              IF    ( (J1.EQ.I1.AND.J2.EQ.I2) .OR.
     &                (J2.EQ.I1.AND.J3.EQ.I2) .OR.
     &                (J3.EQ.I1.AND.J1.EQ.I2)      ) THEN
                MSKSEC%ADR(ISEC)%P%R(IEL)=1.D0
!             RIGHT SIDE
              ELSEIF( (J1.EQ.I2.AND.J2.EQ.I1) .OR.
     &                (J2.EQ.I2.AND.J3.EQ.I1) .OR.
     &                (J3.EQ.I2.AND.J1.EQ.I1)      ) THEN
                MSKSEC%ADR(ISEC)%P%R(IEL)=-1.D0
              ENDIF
            ENDDO
          ENDDO
!
!         OTHER TRIANGLES WITH ONLY 1 POINT TOUCHING THE SECTION
!         LOOKING AT NEIGHBOURS TO FIND THEIR SIDE
!
          DO
            OK=.TRUE.
            DO IEL=1,MESH%NELEM
              J1=IKLE(IEL,1)
              J2=IKLE(IEL,2)
              J3=IKLE(IEL,3)
              DO ISEG=1,NSEG_FS(ISEC)
                I1 = LISTE_FS(ISEC,ISEG,1)
                I2 = LISTE_FS(ISEC,ISEG,2)
                IF((J1.EQ.I1.OR.J2.EQ.I1.OR.J3.EQ.I1.OR.
     &              J1.EQ.I2.OR.J2.EQ.I2.OR.J3.EQ.I2).AND.
     &              ABS(MSKSEC%ADR(ISEC)%P%R(IEL)).LT.0.5D0) THEN
!                 LOOKING AT NEIGHBOURS
                  IF(IFABOR(IEL,1).GT.0) THEN
                    IELEM=IFABOR(IEL,1)
                    IF(ABS(MSKSEC%ADR(ISEC)%P%R(IELEM)).GT.0.5D0) THEN
                      MSKSEC%ADR(ISEC)%P%R(IEL)=
     &                             MSKSEC%ADR(ISEC)%P%R(IELEM)
                    ENDIF
                  ENDIF
                  IF(IFABOR(IEL,2).GT.0) THEN
                    IELEM=IFABOR(IEL,2)
                    IF(ABS(MSKSEC%ADR(ISEC)%P%R(IELEM)).GT.0.5D0) THEN
                      MSKSEC%ADR(ISEC)%P%R(IEL)=
     &                             MSKSEC%ADR(ISEC)%P%R(IELEM)
                    ENDIF
                  ENDIF
                  IF(IFABOR(IEL,3).GT.0) THEN
                    IELEM=IFABOR(IEL,3)
                    IF(ABS(MSKSEC%ADR(ISEC)%P%R(IELEM)).GT.0.5D0) THEN
                      MSKSEC%ADR(ISEC)%P%R(IEL)=
     &                             MSKSEC%ADR(ISEC)%P%R(IELEM)
                    ENDIF
                  ENDIF
                  IF(ABS(MSKSEC%ADR(ISEC)%P%R(IEL)).LT.0.5D0) OK=.FALSE.
                ENDIF
              ENDDO
            ENDDO
            IF(OK) EXIT
          ENDDO
!
!         IF(COMFLU.AND.NSEG_FS(ISEC).GE.1) THEN
          ENDIF
!
        ENDDO ! ISEC
      ENDIF
!
!-----------------------------------------------------------------------
!
      IELMH=T2%ELM
      IELMU=U%ELM
!
      DO ISEC = 1 , NSECLOG
!
        QVC(ISEC) = 0.D0
        CA(ISEC) = 0.D0
        L2(ISEC)= 0.D0
        CV(ISEC) = 0.D0
!
      IF(NSEG_FS(ISEC).GE.1) THEN
!
      CALL OS('X=C     ', X=T4, C=UN1(ISEC))
      CALL OS('X=C     ', X=T5, C=UN2(ISEC))
!     COMPUTING THE FLUX AS IN THE CONTINUITY EQUATION
!     (HOWEVER IMPLICITATION SHOULD PERHAPS BE ALSO USED)
!
      CALL MATRIX(BM1,'M=N     ','MATFGR         X',IELMH,IELMU,
     &            1.D0,T1,S,S,S,S,S,MESH,.TRUE.,MSKSEC%ADR(ISEC)%P)
      CALL MATRIX(BM2,'M=N     ','MATFGR         Y',IELMH,IELMU,
     &            1.D0,T1,S,S,S,S,S,MESH,.TRUE.,MSKSEC%ADR(ISEC)%P)
!
      CALL MATVEC( 'X=AY    ',CV1,BM1,U,0.D0,MESH)
      CALL MATVEC( 'X=X+AY  ',CV1,BM2,V,0.D0,MESH)
      CALL MATVEC( 'X=AY    ',CV4,BM1,T4,0.D0,MESH)
      CALL MATVEC( 'X=X+AY  ',CV4,BM2,T5,0.D0,MESH)
!
      CALL MATRIX(BM1,'M=N     ','MATFGR         X',IELMH,IELMU,
     &            1.D0,T2,S,S,S,S,S,MESH,.TRUE.,MSKSEC%ADR(ISEC)%P)
      CALL MATRIX(BM2,'M=N     ','MATFGR         Y',IELMH,IELMU,
     &            1.D0,T2,S,S,S,S,S,MESH,.TRUE.,MSKSEC%ADR(ISEC)%P)
!
      CALL MATVEC( 'X=AY    ',CV2,BM1,T4,0.D0,MESH)
      CALL MATVEC( 'X=X+AY  ',CV2,BM2,T5,0.D0,MESH)
!
      CALL MATRIX(BM1,'M=N     ','MATFGR         X',IELMH,IELMU,
     &            1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MSKSEC%ADR(ISEC)%P)
      CALL MATRIX(BM2,'M=N     ','MATFGR         Y',IELMH,IELMU,
     &            1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MSKSEC%ADR(ISEC)%P)
!
      CALL MATVEC( 'X=AY    ',CV3,BM1,T4,0.D0,MESH)
      CALL MATVEC( 'X=X+AY  ',CV3,BM2,T5,0.D0,MESH)
!
!     SUMMING CV1 FOR ALL POINTS OF THE SECTION, THIS IS THE FLUX !
!     (BTAINED BY CONTINUITY EQUATION AND AN INTEGRATION BY PARTS)
!     CV1 (flux frazil), CV2 (wet area), CV3 (length), CV4
!     (concentration)
!
      DO ISEG = 1 , NSEG_FS(ISEC)
        I1   = LISTE_FS(ISEC,ISEG,1)
        QVC(ISEC) = QVC(ISEC) + CV1%R(I1)
        CA(ISEC) = CA(ISEC) + CV2%R(I1)
        L2(ISEC) = L2(ISEC) + CV3%R(I1)
        CV(ISEC) = CV(ISEC) + CV4%R(I1)
      ENDDO
!     LAST SEGMENT, ADDING THE LAST POINT
      I2   = LISTE_FS(ISEC,NSEG_FS(ISEC),2)
      QVC(ISEC) = QVC(ISEC) + CV1%R(I2)
      CA(ISEC) = CA(ISEC) + CV2%R(I2)
      L2(ISEC) = L2(ISEC) + CV3%R(I2)
      CV(ISEC) = CV(ISEC) + CV4%R(I2)
!
!     WHEN BOTH UPWIND AND DOWNSTREAM ELEMENTS ARE TAKEN INTO ACCOUNT
!     WITH DIFFERENT SIGNS, WE GET TWICE THE FLUX
!
      QVC(ISEC)=ABS(QVC(ISEC)*0.5D0)/CLOG_TLGTH(ISEC+NFRCLOG)
      L2(ISEC) = ABS(L2(ISEC)*0.5D0)
      CA(ISEC)=ABS(CA(ISEC)*0.5D0)/L2(ISEC)*CLOG_TLGTH(ISEC+NFRCLOG)
      CV(ISEC)=ABS(CV(ISEC)*0.5D0)/L2(ISEC)*CLOG_TLGTH(ISEC+NFRCLOG)
!
      ENDIF
      IF (NCSIZE.GT.1) THEN
        II = P_IMIN(NSEG_FS(ISEC))
        IF(II.GE.0) THEN
          DTMP1 = P_DMIN(QVC(ISEC))
          DTMP2 = P_DMAX(QVC(ISEC))
          QVC(ISEC) = DTMP1+DTMP2
          DTMP1 = P_DMIN(CA(ISEC))
          DTMP2 = P_DMAX(CA(ISEC))
          CA(ISEC) = DTMP1+DTMP2
          DTMP1 = P_DMIN(CV(ISEC))
          DTMP2 = P_DMAX(CV(ISEC))
          CV(ISEC) = DTMP1+DTMP2
        ENDIF
      ENDIF
!
      ENDDO ! ISEC
!
!-----------------------------------------------------------------------
!
      RETURN
      END
