!                   *************************
                    SUBROUTINE READ_BIN_FRLIQ
!                   *************************
!
     &(Q,VARNAME,AT,NFIC,FFORMAT,FOUND)
!
!***********************************************************************
! TELEMAC3D   V7P2                                        02/09/2016
!***********************************************************************
!
!brief    READS AND INTERPOLATES VALUES FROM A BINARY BOUNDARY DATA
!+        FILE, AS A 2D OR 3D SPECIAL BINARY FILE.
!+        INTERPOLATION IS BOTH IN TIME AND IN THE VERTICAL
!+        WHEN REQUIRED (EXCEPT WHEN NAME IS ELEVATION)
!+        ASSUMES THAT IPOBO (BND) HAS THE GLOBAL NODE NUMBER WHETHER
!+        SIMULATION IS IN SERIAL OR PARALLEL MODE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| FFORMAT        |-->| FORMAT OF THE FILE
!| NFIC           |-->| LOGICAL UNIT OF FILE
!| Q              |<--| VARIABLE READ AND INTERPOLATED
!| FOUND          |<--| IF FALSE: VARIABLE NOT FOUND
!| VARNAME        |-->| NAME OF THE VARIABLE Q
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY : NPOIN3,NPLAN,NPOIN2,MESH2D,
     &                                   NBOR2,NPTFR2,Z,NUMLIQ
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE INTERFACE_HERMES
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16), INTENT(IN)      :: VARNAME
      DOUBLE PRECISION, INTENT(IN)       :: AT
      DOUBLE PRECISION, INTENT(INOUT)    :: Q(NPOIN3)
      INTEGER         , INTENT(IN)       :: NFIC
      CHARACTER(LEN=8), INTENT(IN)       :: FFORMAT
      LOGICAL         , INTENT(OUT)      :: FOUND
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES RELATIVE TO THE BINARY FILE
      INTEGER :: NPOINBND,NPOIN2BND,NPLANBND,NVAR,IZ
      INTEGER :: J,JINF,JSUP,IVAR,KK,IERR
      INTEGER :: IPOIN2,IPOIN3,IPOB,IPTFR,JSF,JJ,N
      DOUBLE PRECISION ZINF,ZSUP,COEFF,ZSF,ZF
      CHARACTER(LEN=16), ALLOCATABLE :: BNAMES(:)
      CHARACTER(LEN=16), ALLOCATABLE :: BUNIT(:)
      CHARACTER(LEN=16) :: ZNAME
!
      INTEGER, ALLOCATABLE :: IPOBO(:)
      DOUBLE PRECISION, ALLOCATABLE :: VARVALUE(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZVALUE(:)
!
!-----------------------------------------------------------------------
!
!     INITIALISE THE ARRAY TO BE FILLED
      DO J=1,NPOIN3
        Q(J) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     READ THE BINARY BOUNDARY DATA FILE
!     AND STORE VARIABLE VALUE AND Z VALUE
!
!-----------------------------------------------------------------------
!
!     GET THE NUMBER OF POINTS IN THE 3-D BOUNDARY FILE
!
      CALL GET_MESH_NPOIN(FFORMAT,NFIC,QUADRANGLE_ELT_TYPE,
     &                    NPOINBND,IERR)
      CALL CHECK_CALL(IERR,"READ_BIN_FRILQ: GET_MESH_NPOIN")

!-----------------------------------------------------------------------
!
!     ALLOCATE TWO ARRAYS TO STORE THE VARIABLE VALUES AND THE
!     ELEVATION VALUES
!
      ALLOCATE(VARVALUE(NPOINBND),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"READ_BIN_FRILQ: VARVALUE")
      ALLOCATE(ZVALUE(NPOINBND),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"READ_BIN_FRILQ: ZVALUE")

!-----------------------------------------------------------------------
!
!     STORE THE VARIABLE VALUES IN VARVALUE
!
      CALL FIND_VARIABLE(FFORMAT,NFIC,VARNAME,VARVALUE,NPOINBND,IERR,
     &                   TIME=AT,EPS_TIME=1.D-6,OFFSET=TEL_OFFSET)
      IF(IERR.EQ.HERMES_VAR_UNKNOWN_ERR) THEN
        WRITE(LU,103) VARNAME
103     FORMAT(1X,'WARNING:',/,
     &    1X,'   READ_BIN_FRLIQ: THE VARIABLE ',A16,/,
     &    1X,'   IS MISSING IN THE BINARY BOUNDARY DATA FILE.',/,
     &    1X,'   ITS VALUE IS TAKEN IN THE ASCII FILE')
        FOUND = .FALSE.
      ELSE
        FOUND = .TRUE.
      ENDIF
      CALL CHECK_CALL(IERR,"READ_BIN_FRILQ: FIND_VARIABLE")
!
!-----------------------------------------------------------------------
!
!     GET THE IPOBO VALUES IN THE BOUNDARY FILE
!     IN THE BOUNDARY FILE, IPOBO IS THE GLOBAL NUMBER OF THE BOTTOM
!     POINT AT THE SAME 2D LOCATION ON EVERY LAYER
!
      ALLOCATE(IPOBO(NPOINBND),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"READ_BIN_FRILQ: IPOBO")
      CALL GET_BND_IPOBO(FFORMAT,NFIC,NPOINBND,NPOINBND,
     &                   POINT_BND_ELT_TYPE,IPOBO,IERR)
      CALL CHECK_CALL(IERR,"READ_BIN_FRILQ: GET_BND_IPOBO")
!
!-----------------------------------------------------------------------
!
!     GET THE NUMBER OF PLANES IN THE BOUNDARY FILE
!
      CALL GET_MESH_NPLAN(FFORMAT,NFIC,NPLANBND,IERR)
      CALL CHECK_CALL(IERR,"READ_BIN_FRILQ: GET_MESH_NPLAN")
!
!-----------------------------------------------------------------------
!
!     DEDUCE THE 2D NUMBER OF POINTS IN THE BOUNDARY FILE
!
      NPOIN2BND = NPOINBND/NPLANBND
!
!-----------------------------------------------------------------------
!
!     GET THE NUMBER OF VARIABLES IN THE BOUNDARY FILE
!     THEN ALLOCATE TWO ARRAYS AND STORE THE VARIABLES
!     NAMES AND UNITS IN THEM
!
      CALL GET_DATA_NVAR(FFORMAT,NFIC,NVAR,IERR)
      CALL CHECK_CALL(IERR,"READ_BIN_FRILQ: GET_DATA_NVAR")

      ALLOCATE(BNAMES(NVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"READ_BIN_FRILQ: BNAMES")
      ALLOCATE(BUNIT(NVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"READ_BIN_FRILQ: BUNIT")

      CALL GET_DATA_VAR_LIST(FFORMAT,NFIC,NVAR,BNAMES,BUNIT,IERR)
      CALL CHECK_CALL(IERR,"READ_BIN_FRILQ: GET_DATA_VAR_LIST")

!-----------------------------------------------------------------------
!
!     GET THE INDEX OF THE VARIABLE ELEVATION, CHECK IT WAS FOUND
!     AND STORE IT IN THE ZVALUE ARRAY
!
      IZ = 0
      DO IVAR=1,NVAR
        IF(BNAMES(IVAR).EQ.'ELEVATION Z     ') THEN
          IZ = IVAR
          ZNAME = 'ELEVATION Z'
        ENDIF
        IF(BNAMES(IVAR).EQ.'COTE Z          ') THEN
          IZ = IVAR
          ZNAME = 'COTE Z'
          EXIT
        ENDIF
      ENDDO

      IF( IZ.EQ.0 ) THEN
        WRITE(LU,*) "READ_BIN_FRILQ : "//ZNAME//" Z IS MISSING"
        CALL PLANTE(1)
        STOP
      ENDIF
      CALL FIND_VARIABLE(FFORMAT,NFIC,ZNAME,ZVALUE,NPOINBND,IERR,
     &                   TIME=AT,EPS_TIME=1.D-6,OFFSET=TEL_OFFSET)
      CALL CHECK_CALL(IERR,"READ_BIN_FRILQ: FIND_VARIABLE Z")

!-----------------------------------------------------------------------
!
!     IN CASE VARNAME IS THE ELEVATION, GET
!     THE VALUES OF ELEVATION AT THE FREE-SURFACE ONLY
!
      IF(VARNAME(1:11).EQ.'ELEVATION Z'.AND.IZ.NE.0) THEN
        ! LOOP ON THE 2D BOUNDARY POINTS
        DO IPTFR=1,NPTFR2
          IF(NUMLIQ%I(IPTFR).EQ.0) CYCLE
          IPOIN2 = NBOR2%I(IPTFR)
          N=IPOIN2
          IF(NCSIZE.GT.1) N=MESH2D%KNOLG%I(IPOIN2) ! 2D NODE NUMBER
          ! FIND THE CORRESPONDING BOTTOM POINT IPOB
          ! THE BOUNDARY FILE: IT HAS THE FIRST MATCHING IPOBO
          IPOB = 0
          DO KK = 1,NPOIN2BND
            IF(IPOBO(KK).EQ.N) THEN
              IPOB = KK
              EXIT
            ENDIF
          ENDDO
          IF( IPOB.EQ.0 ) THEN
            WRITE(LU,105) N
105         FORMAT(1X,'READ_BIN_FRLIQ: NODE',I8,/,
     &      1X,'   MISSING IN',/,
     &      1X,'   THE BINARY BOUNDARY DATA FILE')
            CALL PLANTE(1)
            STOP
          ENDIF
          ! STORE THE VALUE OF THE ELEVATION ON THE FREE-SURFACE LAYER
          ! OF THE BOUNDARY POINTS
          IPOIN3 = (NPLAN-1)*NPOIN2+IPOIN2
          Q(IPOIN3) = VARVALUE(IPOB + (NPLANBND-1)*NPOIN2BND)
        ENDDO
      ELSE
!-----------------------------------------------------------------------
!
!     INTERPOLATE THE VARIABLE BETWEEN THE PLANES OF THE BOUNDARY FILE
!
        DO IPTFR=1,NPTFR2
          IF(NUMLIQ%I(IPTFR).EQ.0) CYCLE
          IPOIN2 = NBOR2%I(IPTFR)
          N=IPOIN2
          IF(NCSIZE.GT.1) N=MESH2D%KNOLG%I(IPOIN2) ! 2D NODE NUMBER
          ! FIND THE CORRESPONDING BOTTOM POINT IPOB
          ! THE BOUNDARY FILE: IT HAS THE FIRST MATCHING IPOBO
          IPOB = 0
          DO KK = 1,NPOIN2BND
            IF(IPOBO(KK).EQ.N) THEN
              IPOB = KK
              EXIT
            ENDIF
          ENDDO
          IF( IPOB.EQ.0 ) THEN
            WRITE(LU,107) N
107         FORMAT(1X,'READ_BIN_FRLIQ: NODE',I8,/,
     &      1X,'   MISSING IN',/,
     &      1X,'   THE BINARY BOUNDARY DATA FILE')
            CALL PLANTE(1)
            STOP
          ENDIF
          ! STORE THE VALUE OF THE VARIABLE ON ALL THE BOUNDARY POINTS
          DO J = 1,NPLAN
            COEFF = 0.D0
            IPOIN3 = (J-1)*NPOIN2+IPOIN2
            ! THE NUMBER OF PLANES IN THE BINARY FILE
            ! MAY NOT BE THE SAME AS IN OUR MESH
            ! SO WE NEED TO INTERPOLATE ON THE VERTICAL
            IF( NPLANBND.GT.1 ) THEN
              ZF = ZVALUE(IPOB)
              JSF = IPOB + (NPLANBND-1)*NPOIN2BND
              ZSF = ZVALUE(JSF)
              ! CLIP ON THE BOTTOM
              IF( Z(IPOIN3).LE.ZF ) THEN
                Q(IPOIN3) = VARVALUE(IPOB)
              ! CLIP ON THE FREE-SURFACE
              ELSEIF( Z(IPOIN3).GE.ZSF ) THEN
                Q(IPOIN3) = VARVALUE(JSF)
              ! INTERPOLATION
              ELSE
                DO JJ = 1,NPLANBND-1
                  JINF = IPOB + (JJ-1)*NPOIN2BND
                  ZINF = ZVALUE(JINF)
                  JSUP = IPOB + JJ*NPOIN2BND
                  ZSUP = ZVALUE(JSUP)
                  IF( Z(IPOIN3).LE.ZSUP.AND.Z(IPOIN3).GE.ZINF ) THEN
                    COEFF = ( Z(IPOIN3)-ZINF )/( ZSUP-ZINF )
                    Q(IPOIN3) = (1.D0-COEFF)*VARVALUE(JINF)
     &                        + COEFF*VARVALUE(JSUP)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF ! NPLANBND.GT.1
          ENDDO
        ENDDO
      ENDIF

      DEALLOCATE(VARVALUE)
      DEALLOCATE(ZVALUE)
      DEALLOCATE(BNAMES)
      DEALLOCATE(BUNIT)
      DEALLOCATE(IPOBO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
