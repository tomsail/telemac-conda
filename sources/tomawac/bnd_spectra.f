!                   ******************
                    MODULE BND_SPECTRA
!                   ******************
!
!
!***********************************************************************
!     TOMAWAC   V7P3                                   23/02/2017
!***********************************************************************
!
!brief    MODULE TO IMPOSE SPECTRA ON OPEN BOUNDARIES FROM AN
!         EXTERNAL MESH FILE
!
!     history  A. JOLY (EDF - LNHE)
!     +        23/02/2017
!     +        V7P3
!     +   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      IMPLICIT NONE
!
      PRIVATE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      PUBLIC :: IMPOSE_BND_SPECTRA
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ! TIME VARIABLES
      DOUBLE PRECISION :: TV1,TV2
      INTEGER :: RECORD1,RECORD2
      DOUBLE PRECISION :: TIME1,TIME2
      !ERROR INTEGER WHEN READING THE MESH
      INTEGER :: IERR
      ! BOUNDARY SPECTRA VARIABLES
      CHARACTER(LEN=80) :: BC_TITLE ! TITLE
      INTEGER :: BC_NVAR   ! NUMBER OF VARIABLES
      INTEGER :: BC_NPOIN  ! NUMBER OF MESH NODES
      INTEGER :: BC_TYP_ELEM  ! TYPE OF ELEMENT
      INTEGER :: BC_NELEM  ! NUMBER OF ELEMENTS
      INTEGER :: BC_NDP    ! NUMBER OF ELEMENT FACES
      INTEGER :: BC_NPTFR  ! NUMBER OF BOUNDARY NODES
      INTEGER :: BC_NPTIR  ! NUMBER OF INTERFACES
      INTEGER :: BC_NDIM  ! DIMENSION OF THE MESH
      CHARACTER(LEN=16), ALLOCATABLE :: BC_VARLIST(:)
      CHARACTER(LEN=16), ALLOCATABLE :: BC_UNITLIST(:)
      INTEGER :: BC_NT  ! NUMBER TIME STEPS
      DOUBLE PRECISION, ALLOCATABLE :: BC_X(:),BC_Y(:) !X AND Y COORDINATES ON SPE MESH
      DOUBLE PRECISION, ALLOCATABLE :: BC_FREQ(:),BC_TETA(:) !F AND THETA COORDINATES OF SPECTRUM
      DOUBLE PRECISION, ALLOCATABLE :: BC_VAL(:)  ! TEMPORY DATA FOR READING
      DOUBLE PRECISION, ALLOCATABLE :: BC_ALL1(:,:), BC_ALL2(:,:) !VALUES A TIME TV1 AND TV2
      !FOR POINT OF THE SPECTRA FILE
      INTEGER, ALLOCATABLE :: BC_CORR(:) ! INDEX OF THE CLOSEST SPECTRUM TO A LIQUID BOUNDARY NODE
      ! OPTIONAL ARGUMENTS
      INTEGER NNELEBD, TYP_BND
!

      INTEGER :: NPTFR_LOC
      INTEGER, ALLOCATABLE :: IND_PTFR(:) ! INDEX INTO VARLIST AND BOUNDARY NUMBER
!
      SAVE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!***********************************************************************
      CONTAINS
!***********************************************************************

!                   *****************************
                    SUBROUTINE IMPOSE_BND_SPECTRA
!                   *****************************
!
     &     (IMP_FILE,LT,AT,FBOR,NPTFR,NDIRE,NF)
!
!***********************************************************************
!     TOMAWAC   V7P3                                   23/02/2017
!***********************************************************************
!
!     brief    READS SPECTRA SAVED IN A MESH FILE AND IMPOSES THEM ON
!     +        OPEN BOUNDARY POINTS
!
!     history  A. JOLY (EDF - LNHE)
!     +        23/02/2017
!     +        V7P3
!     +   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IMP_FILE       |-->| MESH FILE WITH THE IMPOSED SPECTRA
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| AT             |-->| COMPUTATION TIME
!| FBOR           |<->| SPECTRAL VARIANCE DENSITY AT THE BOUNDARIES
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NF             |-->| NUMBER OF FREQUENCIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE BIEF_DEF, ONLY : BIEF_FILE
      USE DECLARATIONS_TOMAWAC, ONLY : NPSPE, XSPE, YSPE, UNITSPE,
     &                     PHASSPE, X, Y, NBOR, LIFBOR, F1, RAISF, DEUPI
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      INTEGER, INTENT(IN)            :: LT
      DOUBLE PRECISION, INTENT(IN)   :: AT
      INTEGER, INTENT(IN)            :: NPTFR,NDIRE,NF
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPTFR, ISPE, IPOIN, IDIR, IFF,ITMP
      INTEGER X_ORIG, Y_ORIG
      DOUBLE PRECISION DIST_SPE,DIST
      DOUBLE PRECISION COEFT
      DOUBLE PRECISION FCL1,FCL2
      INTEGER BC_NF,BC_NDIRE,ITRAV
      DOUBLE PRECISION BC_F1,BC_F2,BC_RAISF
      DOUBLE PRECISION F_TEMP
      DOUBLE PRECISION, PARAMETER :: EPS = 1.D-6
!
!-----------------------------------------------------------------------
! DURING THE FIRST TIME STEP
!-----------------------------------------------------------------------
      IF(LT.EQ.0)THEN
! 1/ FIND THE CLOSEST SPECTRUM FOR EACH LIQUID BOUNDARY NODE
        IF(ALLOCATED(BC_CORR))DEALLOCATE(BC_CORR)
        ALLOCATE(BC_CORR(NPTFR))
        DO IPTFR=1,NPTFR
          IF(LIFBOR(IPTFR).EQ.KENT)THEN
            DIST_SPE=1.D99
            DO ISPE=1,NPSPE
              DIST=SQRT((XSPE(ISPE)-X(NBOR(IPTFR)))**2+
     &                  (YSPE(ISPE)-Y(NBOR(IPTFR)))**2)
              IF(DIST.LE.DIST_SPE)THEN
                BC_CORR(IPTFR)=ISPE
                DIST_SPE=DIST
              ENDIF
            ENDDO
          ELSE
            BC_CORR(IPTFR)=0
          ENDIF
        END DO
! 2/ READ THE MESH THE SPECTRA TO BE IMPOSED ON THE BOUNDARY
        ! MESH INFO
        CALL READ_MESH_INFO(IMP_FILE%FMT,IMP_FILE%LU,
     &          BC_TITLE,BC_NVAR,
     &          BC_NPOIN,BC_TYP_ELEM,BC_NELEM,BC_NPTFR,BC_NPTIR,
     &          BC_NDP,ITRAV,X_ORIG,Y_ORIG,TYP_BND,NNELEBD)
        CALL GET_MESH_DIMENSION(IMP_FILE%FMT,IMP_FILE%LU,
     &          BC_NDIM,IERR)
        ! READ THE NAME OF THE VARIABLES
        IF(ALLOCATED(BC_VARLIST))DEALLOCATE(BC_VARLIST)
        IF(ALLOCATED(BC_UNITLIST))DEALLOCATE(BC_UNITLIST)
        ALLOCATE(BC_VARLIST(BC_NVAR))
        ALLOCATE(BC_UNITLIST(BC_NVAR))
        CALL GET_DATA_VAR_LIST(IMP_FILE%FMT,IMP_FILE%LU,
     &          BC_NVAR,BC_VARLIST,BC_UNITLIST,IERR)
! 3/ READ THE NUMBER OF TIME STEPS AND THE FIRST TIME
        ! GET NUMBER OF TIME STEPS
        CALL GET_DATA_NTIMESTEP(IMP_FILE%FMT,IMP_FILE%LU,
     &          BC_NT,IERR)
        ! READ THE FIRST TIME
        RECORD1 = 0
        RECORD2 = 1
        CALL GET_DATA_TIME(IMP_FILE%FMT,IMP_FILE%LU,
     &          RECORD1,TIME1,IERR)
        TV1=(TIME1-PHASSPE)*UNITSPE

        IF(TV1.GT.AT) THEN
          WRITE(LU,*)'*********************************************'
          WRITE(LU,*)'THE FIRST RECORDING OF THE FILE'
          WRITE(LU,*)'  ',TV1,' IS OLDER THAN THE BEGINNING'
          WRITE(LU,*)'  OF THE COMPUTATION',AT
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF

        DO
        CALL GET_DATA_TIME(IMP_FILE%FMT,IMP_FILE%LU,
     &          RECORD2,TIME2,IERR)
        TV2=(TIME2-PHASSPE)*UNITSPE
        IF(TV2.LT.AT) THEN
          RECORD1 = RECORD2
          RECORD2 = RECORD2 + 1
          TV1 = TV2
          IF (RECORD2.GT.BC_NT) THEN
            WRITE(LU,*)'*****************************************'
            IF(LNG.EQ.1) THEN
              WRITE(LU,*)'LA FIN DU FICHIER DE CONDITION LIMITE'
              WRITE(LU,*)'EST ATTEINTE AU TEMPS : ',AT
            ELSE
              WRITE(LU,*)'THE END OF THE BOUNDARY CONDITION FILE'
              WRITE(LU,*)'IS REACHED AT TIME: ',AT
            ENDIF
            WRITE(LU,*)'*****************************************'
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          EXIT
        ENDIF
      ENDDO


! 4/ READ X AND Y COORDINATES
        IF(ALLOCATED(BC_X))DEALLOCATE(BC_X)
        IF(ALLOCATED(BC_Y))DEALLOCATE(BC_Y)
        IF(ALLOCATED(BC_FREQ))DEALLOCATE(BC_FREQ)
        IF(ALLOCATED(BC_TETA))DEALLOCATE(BC_TETA)
        ALLOCATE(BC_X(BC_NPOIN))
        ALLOCATE(BC_Y(BC_NPOIN))
        ALLOCATE(BC_FREQ(BC_NPOIN))
        ALLOCATE(BC_TETA(BC_NPOIN))
!
        CALL GET_MESH_COORD(IMP_FILE%FMT,IMP_FILE%LU,
     &            1,BC_NDIM,BC_NPOIN,BC_X,IERR)
        CALL GET_MESH_COORD(IMP_FILE%FMT,IMP_FILE%LU,
     &          2,BC_NDIM,BC_NPOIN,BC_Y,IERR)
! 5/ CHECK THAT THE MESH READ HAS THE SAME THE FREQUENCIES AND
!    DIRECTIONS AS IN THE SIMULATION
        BC_NF=0
        BC_NDIRE=0
        BC_F1=10.D10
        BC_F2=10.D10
        BC_RAISF=0.D0
        DO IPOIN=1,BC_NPOIN
          IF((ABS(BC_X(IPOIN)).LE.EPS).AND.(BC_Y(IPOIN).GE.0.0))THEN
            BC_NF=BC_NF+1
            F_TEMP=BC_Y(IPOIN)
            IF(F_TEMP.LT.BC_F1)THEN
              BC_F2=BC_F1
              BC_F1=F_TEMP
            ELSEIF(F_TEMP.LT.BC_F2)THEN
              BC_F2=F_TEMP
            ENDIF
          ENDIF
        END DO
        BC_RAISF=BC_F2/BC_F1
        BC_NDIRE=BC_NPOIN/BC_NF
        IF(ABS(BC_F1-F1).GT.1.D-6) THEN
          WRITE(LU,*)'*********************************************'
          WRITE(LU,*)'THE MINIMAL FREQUENCY OF THE SPECTRA READ'
          WRITE(LU,*)'IS NOT EQUAL TO THE FREQUENCY OF THE'
          WRITE(LU,*)'SIMULATION.'
          WRITE(LU,*)'- FREQUENCY OF THE SIMULATION:',F1
          WRITE(LU,*)'- FREQUENCY READ:',BC_F1
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(ABS(BC_RAISF-RAISF).GT.1.D-6) THEN
          WRITE(LU,*)'*********************************************'
          WRITE(LU,*)'THE FREQUENTIAL RATIO OF THE SPECTRA READ'
          WRITE(LU,*)'IS NOT EQUAL TO THE RATIO OF THE SIMULATION.'
          WRITE(LU,*)'- FREQUENTIAL RATIO OF THE SIMULATION:',RAISF
          WRITE(LU,*)'- FREQUENTIAL RATIO READ:',BC_RAISF
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(BC_NF.NE.NF) THEN
          WRITE(LU,*)'*********************************************'
          WRITE(LU,*)'THE NUMBER OF FREQUENCY OF THE SPECTRA READ'
          WRITE(LU,*)'IS NOT EQUAL TO THE NUMBER OF FREQUENCY OF THE'
          WRITE(LU,*)'SIMULATION.'
          WRITE(LU,*)'- NUMBER OF FREQUENCY OF THE SIMULATION:',NF
          WRITE(LU,*)'- NUMBER OF FREQUENCY READ:',BC_NF
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(BC_NDIRE.NE.NDIRE) THEN
          WRITE(LU,*)'*********************************************'
          WRITE(LU,*)'THE NUMBER OF DIRECTIONS OF THE SPECTRA READ'
          WRITE(LU,*)'IS NOT EQUAL TO THE NUMBER OF DIRECTIONS OF THE'
          WRITE(LU,*)'SIMULATION.'
          WRITE(LU,*)'- NUMBER OF DIRECTIONS OF THE SIMULATION:',NDIRE
          WRITE(LU,*)'- NUMBER OF DIRECTIONS READ:',BC_NDIRE
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
! 6/ CALCULATE FREQ AND DIR
        DO IPOIN=1,BC_NPOIN
          BC_TETA(IPOIN)=ATAN2(BC_X(IPOIN),BC_Y(IPOIN))+DEUPI
          BC_TETA(IPOIN)=MOD(BC_TETA(IPOIN),DEUPI)
          IF(SIN(BC_TETA(IPOIN)).NE.0.D0)THEN
            BC_FREQ(IPOIN)=BC_X(IPOIN)/SIN(BC_TETA(IPOIN))
          ELSE
            BC_FREQ(IPOIN)=BC_Y(IPOIN)/COS(BC_TETA(IPOIN))
          ENDIF
        ENDDO
! 6.5/ CHECK NUMBER OF POINTS ON PROCESS
        NPTFR_LOC  = 0
        ALLOCATE(IND_PTFR(NPTFR))
        DO IPTFR=1,NPTFR
          IF(LIFBOR(IPTFR).EQ.KENT)THEN
            NPTFR_LOC = NPTFR_LOC+1
            IND_PTFR(NPTFR_LOC) = IPTFR
          ENDIF
        ENDDO


! 7/ ALLOCATE THE TEMPORARY SPECTRUM AT THE TWO TIME STEPS
        IF(ALLOCATED(BC_VAL))DEALLOCATE(BC_VAL)
        ALLOCATE(BC_VAL(BC_NPOIN))

        IF(ALLOCATED(BC_ALL1))DEALLOCATE(BC_ALL1)
        IF(ALLOCATED(BC_ALL2))DEALLOCATE(BC_ALL2)
        ALLOCATE(BC_ALL1(BC_NPOIN,NPTFR_LOC))
        ALLOCATE(BC_ALL2(BC_NPOIN,NPTFR_LOC))

! 8/ READ FIRST TIME
        DO IPTFR=1,NPTFR_LOC
          ITMP = IND_PTFR(IPTFR)
          ! READ DATA BEFORE
          CALL GET_DATA_VALUE(IMP_FILE%FMT,IMP_FILE%LU,
     &       RECORD1,BC_VARLIST(BC_CORR(ITMP)),BC_VAL,BC_NPOIN,IERR)
          DO IPOIN = 1,BC_NPOIN
            BC_ALL1(IPOIN,IPTFR)= BC_VAL(IPOIN)
          ENDDO
          ! READ DATA AFTER
          CALL GET_DATA_VALUE(IMP_FILE%FMT,IMP_FILE%LU,
     &       RECORD2,BC_VARLIST(BC_CORR(ITMP)),BC_VAL,BC_NPOIN,IERR)
          DO IPOIN = 1,BC_NPOIN
            BC_ALL2(IPOIN,IPTFR)= BC_VAL(IPOIN)
          ENDDO
        ENDDO


!-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------
! FOR ALL TIME STEPS
!-----------------------------------------------------------------------
! 8/ READ THE SECOND TIME STEP

      ! RETURN IF NO BOUNDARY DATA
      IF (NPTFR_LOC.EQ.0) THEN
            RETURN
      ENDIF

! 8.5/ UPDATE DATA IF NEEDED
      IF (AT.GT.TV2) THEN
        DO
          RECORD2 = RECORD2+1
          TV1 = TV2
          CALL GET_DATA_TIME(IMP_FILE%FMT,IMP_FILE%LU,
     &            RECORD2,TIME2,IERR)
          TV2=(TIME2-PHASSPE)*UNITSPE
          IF(TV2.LT.AT) THEN
            IF (RECORD2.GT.BC_NT) THEN
              WRITE(LU,*)'*****************************************'
              IF(LNG.EQ.1) THEN
                WRITE(LU,*)'LA FIN DU FICHIER DE CONDITION LIMITE'
                WRITE(LU,*)'EST ATTEINTE AU TEMPS : ',AT
              ELSE
                WRITE(LU,*)'THE END OF THE BOUNDARY CONDITION FILE'
                WRITE(LU,*)'IS REACHED AT TIME: ',AT
              ENDIF
              WRITE(LU,*)'*****************************************'
              CALL PLANTE(1)
              STOP
            ENDIF
          ELSE
            EXIT
          ENDIF
        ENDDO

        IF (RECORD1.LT.RECORD2-1) THEN
            ! IN CASE TIME STEP IN SPECTRAL FILE IS SMALLER THAN MODEL TIME STEP
            ! ALSO UPDATE RECORD 1
            RECORD1 = RECORD2-1
            DO IPTFR=1,NPTFR_LOC
              ITMP = IND_PTFR(IPTFR)
              CALL GET_DATA_VALUE(IMP_FILE%FMT,IMP_FILE%LU,
     &           RECORD1,BC_VARLIST(BC_CORR(ITMP)),BC_VAL,
     &           BC_NPOIN,IERR)
              DO IPOIN = 1,BC_NPOIN
                BC_ALL1(IPOIN,IPTFR)= BC_VAL(IPOIN)
              ENDDO
            ENDDO
        ELSE
          ! COPY DATA
          RECORD1 = RECORD2
          DO IPTFR=1,NPTFR_LOC
            DO IPOIN = 1,BC_NPOIN
              BC_ALL2(IPOIN,IPTFR)= BC_VAL(IPOIN)
            ENDDO
          ENDDO
        ENDIF
        ! UPDATE NEW TIME
        DO IPTFR=1,NPTFR_LOC
          ITMP = IND_PTFR(IPTFR)
          CALL GET_DATA_VALUE(IMP_FILE%FMT,IMP_FILE%LU,
     &       RECORD2,BC_VARLIST(BC_CORR(ITMP)),BC_VAL,BC_NPOIN,IERR)
          DO IPOIN = 1,BC_NPOIN
            BC_ALL2(IPOIN,IPTFR)= BC_VAL(IPOIN)
          ENDDO
        ENDDO
      ENDIF
!
      COEFT=(AT-TV1)/(TV2-TV1)
! 9/ IMPOSE THE SPECTRA
        !TODO; USE BETTER INTERPOLATION
      DO IPTFR=1,NPTFR_LOC
          ITMP = IND_PTFR(IPTFR)
          DO IDIR=1,NDIRE
            DO IFF=1,NF
              IPOIN=(IDIR+NDIRE*(IFF-1))
              FCL1=BC_ALL1(IPOIN,IPTFR)
              FCL2=BC_ALL2(IPOIN,IPTFR)
              FBOR(ITMP,IDIR,IFF)=FCL1+(FCL2-FCL1)*COEFT
            ENDDO
          ENDDO
      ENDDO

!
      RETURN
      END SUBROUTINE IMPOSE_BND_SPECTRA
!
      END MODULE BND_SPECTRA
