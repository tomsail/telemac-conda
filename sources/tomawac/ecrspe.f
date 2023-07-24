!                   *****************
                    SUBROUTINE ECRSPE
!                   *****************
!
     &( F     , NDIRE , NF    , NPOIN2, LT ,    AUXIL ,
     &  NOLEO , NLEO  , DEBRES, DATE  , TIME  , KNOLG , MESH)
!
!***********************************************************************
! TOMAWAC   V6P3                                   15/06/2011
!***********************************************************************
!
!brief    WRITES OUT THE DIRECTIONAL VARIANCE SPECTRUM
!+                AT SELECTED NODES.
!+                (SERAPHIN BINARY FORMAT).
!
!history  OPTIMER
!+        28/08/2000
!+        V5P0
!+   CREATED
!
!history
!+        07/06/2001
!+        V5P2
!+
!
!history  M. BENOIT
!+        13/07/2004
!+        V5P5
!+   CORRECTED A BUG IN THE DECLARATION OF IPOBO WHEN PASSED
!
!history
!+
!+        V6P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  A. LAUGEL & J-M HERVOUET (EDF - LNHE)
!+        22/11/2012
!+        V6P3
!+   Parallelism treated with files.
!
!history  E. GAGNAIRE-RENOU (EDF - LNHE)
!+        12/03/2013
!+        V6P3
!+   Print out the 1D frequential spectrum at (same) selected nodes.
!+   Scopgene format.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history A JOLY (LNHE)
!+       16/02/2017
!+       V7P3
!+   In some instances, PROXIM could find a node in only one processor
!+   domain (and therefore NOLEO), but MESH%ELTCAR was in another.
!+   This case is now taken into account.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AUXIL          |<->| DIRECTIONAL SPECTRUM WORK TABLE
!| DATE           |-->| START DATE
!| DEBRES         |-->| LOGICAL INDICATING THE FIRST TIME STEP TO PRINT
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| ISLEO          |-->| ARRAY OF LOGICAL
!| KNOLG          |-->| ARRAY LINKING LOCAL TO GLOBAL INDEXES IN PARALL
!| NF             |-->| NUMBER OF FREQUENCIES
!| NK             |-->| DUMMY VARIABLE
!| NLEO           |-->| NUMBER OF SPECTRUM PRINTOUT POINTS
!| NOLEO          |-->| INDEX ARRAY OF SPECTRUM PRINTOUT POINTS
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TIME           |-->| START TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, STRA40,STRA41, TETA, FREQ,
     &    TITCAS, AT, LUSPE, NAMSPE, LULEO, FMTLEO, NAMLEO
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_SUM
      USE INTERFACE_TOMAWAC, EX_ECRSPE => ECRSPE
      
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NLEO,NF,NDIRE, LT
      INTEGER, INTENT(IN)             :: KNOLG(*),  NOLEO(NLEO)
      INTEGER, INTENT(IN)             :: DATE(3),TIME(3)
      DOUBLE PRECISION, INTENT(INOUT) :: AUXIL(NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      LOGICAL, INTENT(IN)             :: DEBRES
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  ISTAT , II    , JF    , K    , II_ALL
      INTEGER  KAMP1 , KAMP2 , KAMP3 , KAMP4 , KAMP5 , KAMP6 , ILEO
      INTEGER  IBID(1), NELEM, NPSPE
      CHARACTER(LEN=72) C
      CHARACTER(LEN=32) TEXTE(NLEO)
      CHARACTER(LEN=6)  NUM
      CHARACTER(LEN=5)  CC
      CHARACTER(LEN=1)  C1,C2,C3,C4,C5,C6
      INTEGER NUM1, NUM2, NUM3, NUM4, NUM5
      TYPE(BIEF_MESH) MESHF
      LOGICAL         SORLEO(NLEO)
      DOUBLE PRECISION DTETAR
      REAL W(1)
      CHARACTER(LEN=11) EXTENS
      EXTERNAL          EXTENS
      INTEGER :: ID
!
      DOUBLE PRECISION, ALLOCATABLE :: F_INTF(:,:)
!
!-----------------------------------------------------------------------
!
      DTETAR=DEUPI/DBLE(NDIRE)
      NPSPE=NF*NDIRE
      NELEM=(NF-1)*NDIRE
!     SORLEO = .FALSE.
      DO ILEO=1,NLEO
        KAMP1=NOLEO(ILEO)
        IF(NCSIZE.GT.1) THEN
          IF(KAMP1.GT.0) KAMP1=KNOLG(NOLEO(ILEO))
          KAMP1=P_MAX(KAMP1)
        ENDIF
        KAMP2=MOD(KAMP1,100000)
        KAMP3=MOD(KAMP2,10000)
        KAMP4=MOD(KAMP3,1000)
        KAMP5=MOD(KAMP4,100)
        KAMP6=MOD(KAMP5,10)
        C1=CHAR(48+KAMP1/100000)
        C2=CHAR(48+KAMP2/10000)
        C3=CHAR(48+KAMP3/1000)
        C4=CHAR(48+KAMP4/100)
        C5=CHAR(48+KAMP5/10)
        C6=CHAR(48+KAMP6)
        NUM=C1//C2//C3//C4//C5//C6
        ! Number of frequence
        NUM1=ILEO
        NUM2=MOD(NUM1,10000)
        NUM3=MOD(NUM2,1000)
        NUM4=MOD(NUM3,100)
        NUM5=MOD(NUM4,10)
        C1=CHAR(48+NUM1/10000)
        C2=CHAR(48+NUM2/1000)
        C3=CHAR(48+NUM3/100)
        C4=CHAR(48+NUM4/10)
        C5=CHAR(48+NUM5)
        CC=C1//C2//C3//C4//C5
        TEXTE(ILEO)='F'//CC//'PT2D'//NUM//'UNITE SI       '
        SORLEO(ILEO) = .TRUE.
      ENDDO
!
!     FOR THE FIRST PRINTED TIME STEP, WRITES OUT THE HEADER TO THE FILE
!
      ALLOCATE(F_INTF(NLEO,NF))
      IF(DEBRES) THEN
!
!     IN PARALLEL ONLY PROCESSOR 0 CREATES THE FILE
!
        IF(IPID.EQ.0) THEN
!
!       CREATES MESHF, MESH ASSOCIATED WITH DISCRETISATION
!       IN FREQUENCY AND DIRECTION
!
          ALLOCATE(MESHF%TYPELM)
          ALLOCATE(MESHF%NELEM)
          ALLOCATE(MESHF%NPOIN)
          ALLOCATE(MESHF%IKLE)
          ALLOCATE(MESHF%IKLE%I(4*NELEM))
          ALLOCATE(MESHF%X)
          ALLOCATE(MESHF%Y)
          ALLOCATE(MESHF%NPTFR)
          ALLOCATE(MESHF%NBOR)
          ALLOCATE(MESHF%NBOR%I(2*NDIRE))
          ALLOCATE(MESHF%DIM1)
          ALLOCATE(MESHF%KNOLG)
          ALLOCATE(MESHF%KNOLG%I(NDIRE*NF))
          ALLOCATE(MESHF%X_ORIG)
          ALLOCATE(MESHF%Y_ORIG)
!
!
          MESHF%NAME = 'MESH'
          MESHF%TYPELM = QUADRANGLE_ELT_TYPE !TRIANGLE 2D MESH
          MESHF%NELEM  = NELEM
          MESHF%NPOIN  = NPSPE
          MESHF%DIM1   = 2
          MESHF%X_ORIG = 0
          MESHF%Y_ORIG = 0
          II=0
          DO JF=1,NF-1
            DO K=1,NDIRE
              II=II+1
              MESHF%IKLE%I(II)=MOD(II,NDIRE)+1+(JF-1)*NDIRE
            ENDDO
          ENDDO
          DO II=1,NELEM
            MESHF%IKLE%I(II+NELEM)=II
            MESHF%IKLE%I(II+2*NELEM)=II+NDIRE
            MESHF%IKLE%I(II+3*NELEM)=MESHF%IKLE%I(II)+NDIRE
          ENDDO
!
!       WRITES OUT THE ARRAYS X AND Y
!
          ALLOCATE(MESHF%X%R(NDIRE*NF))
          ALLOCATE(MESHF%Y%R(NDIRE*NF))
          MESHF%NPTFR = 2*NDIRE
          DO JF=1,NF
            DO II=1,NDIRE
              MESHF%X%R(II+NDIRE*(JF-1))=FREQ(JF)*SIN(TETA(II))
              MESHF%Y%R(II+NDIRE*(JF-1))=FREQ(JF)*COS(TETA(II))
            ENDDO
          ENDDO
          MESHF%NBOR%I=0
          DO II = 1,NDIRE
            MESHF%NBOR%I(II) = II
          ENDDO
          DO II = NDIRE+1,2*NDIRE
            MESHF%NBOR%I(II)=NDIRE+1+NPSPE-II
          ENDDO
          MESHF%KNOLG%I = 0
          ALLOCATE(MESHF%NDS(0:81,7))
          MESHF%TYPELM = QUADRANGLE_ELT_TYPE
          MESHF%NDS(MESHF%TYPELM+1,3) = 4

!
!         CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES.
!         THE DATA ARE CREATED IN THE FILE: NRES, AND IS
!         CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
!         CONTAINED IN THE FILE.
!
          IF (NAMLEO(1:1).NE.' ') THEN
            CALL WRITE_HEADER(FMTLEO, ! RESULTS FILE FORMAT
     &           LULEO,          ! LU FOR RESULTS FILE
     &           TITCAS,        ! TITLE
     &           NLEO,          ! MAX NUMBER OF OUTPUT VARIABLES
     &           TEXTE,         ! NAMES OF OUTPUT VARIABLES
     &           SORLEO)        ! PRINT TO FILE OR NOT

!
!         WRITES THE MESH IN THE OUTPUT FILE
!
            CALL WRITE_MESH(FMTLEO, ! RESULTS FILE FORMAT
     &           LULEO,          ! LU FOR RESULTS FILE
     &           MESHF,
     &           1,             ! NUMBER OF PLANES
     &           DATE,          ! START DATE
     &           TIME,          ! START TIME
     &           STRA40,STRA41, !
     &           .FALSE., 0) ! PARALL, NPTIR
          ENDIF
!
          IF(NAMSPE(1:1).NE.' ') THEN
            WRITE(LUSPE,'(A1,A72)') '/', TITCAS
            WRITE(LUSPE,'(I3)') NLEO
            DO ILEO=1,NLEO
              WRITE(LUSPE,'(A32)') TEXTE(ILEO)
            ENDDO
            WRITE(LUSPE,'(A19)') '0 0 0 0 0 0 0 0 0 0'
          ENDIF
          DEALLOCATE(MESHF%TYPELM)
          DEALLOCATE(MESHF%NELEM)
          DEALLOCATE(MESHF%NPOIN)
          DEALLOCATE(MESHF%IKLE%I)
          DEALLOCATE(MESHF%IKLE)
          DEALLOCATE(MESHF%X%R)
          DEALLOCATE(MESHF%X)
          DEALLOCATE(MESHF%Y%R)
          DEALLOCATE(MESHF%Y)
          DEALLOCATE(MESHF%NPTFR)
          DEALLOCATE(MESHF%NBOR%I)
          DEALLOCATE(MESHF%NBOR)
          DEALLOCATE(MESHF%DIM1)
          DEALLOCATE(MESHF%KNOLG%I)
          DEALLOCATE(MESHF%KNOLG)
          DEALLOCATE(MESHF%NDS)
!
        ENDIF

      ENDIF
!
!     RECORDS THE CURRENT TIME STEP
!
      IF (NAMSPE(1:1).NE.' ') THEN
        IF(IPID.EQ.0) THEN
          WRITE(LUSPE,1008) AT
        ENDIF
      ENDIF
1008  FORMAT('TIME  = ',F13.5)
!
      IF(NCSIZE.GT.1) THEN
        CALL GET_FREE_ID(ID)
!
!       1) EVERY PROCESSOR WRITES ITS OWN POINTS
!       MESH%ELTCAR IS USED AS FOR THE CHARACTERISTICS
!
        DO ILEO=1,NLEO
          II=NOLEO(ILEO)
          II_ALL=P_SUM(II)
          IF(II.GT.0) THEN
            IF((MESH%ELTCAR%I(II).NE.0).OR.
     &          (II.EQ.II_ALL)) THEN
              DO JF=1,NF
                DO K=1,NDIRE
                  AUXIL(K,JF)=F(II,K,JF)
                ENDDO
              ENDDO
              OPEN(ID,FILE=EXTENS(NLEO,ILEO),
     &             FORM='UNFORMATTED',STATUS='NEW')
              CALL ECRI2(AUXIL,IBID,C,NPSPE,'R8',ID,'STD',ISTAT)
              CLOSE(ID)
            ENDIF
          ENDIF
        ENDDO
!
!       WAITING COMPLETION OF THE WORK BY ALL PROCESSORS
!
        CALL P_SYNC
!
!       2) PROCESSOR 0 READS ALL FILES AND MERGES IN THE FINAL FILE
!
        IF(IPID.EQ.0) THEN
          IF (NAMLEO(1:1).NE.' ') THEN
            DO ILEO=1,NLEO
              OPEN(ID,FILE=EXTENS(NLEO,ILEO),
     &                FORM='UNFORMATTED',STATUS='OLD')
              CALL LIT(AUXIL,W,IBID,C,NPSPE,'R8',ID,'STD',ISTAT)
              CALL ADD_DATA(FMTLEO,LULEO,TEXTE(ILEO),AT,LT,ILEO.EQ.1,
     &                      AUXIL,NPSPE,ISTAT)
              CALL CHECK_CALL(ISTAT,'ECRSPE:ADD_DATA')
              CLOSE(ID,STATUS='DELETE')
              DO JF=1,NF
                F_INTF(ILEO,JF)=0.D0
                DO K=1,NDIRE
                  F_INTF(ILEO,JF)=F_INTF(ILEO,JF)+AUXIL(K,JF)*DTETAR
                ENDDO
              ENDDO
            ENDDO
          ENDIF
          IF (NAMSPE(1:1).NE.' ') THEN
            DO JF=1,NF
              WRITE(LUSPE,'(100(E10.4,2X))') FREQ(JF),
     &             (F_INTF(ILEO,JF),ILEO=1,NLEO)
            ENDDO
          ENDIF
        ENDIF
      ELSE
        IF (NAMLEO(1:1).NE.' ') THEN
          DO ILEO=1,NLEO
            II=NOLEO(ILEO)
            DO JF=1,NF
              DO K=1,NDIRE
                AUXIL(K,JF)=F(II,K,JF)
              ENDDO
            ENDDO
            CALL ADD_DATA(FMTLEO,LULEO,TEXTE(ILEO),AT,LT,ILEO.EQ.1,
     &                    AUXIL,NPSPE,ISTAT)
            CALL CHECK_CALL(ISTAT,'ECRSPE:ADD_DATA')
          ENDDO
        ENDIF
        IF (NAMSPE(1:1).NE.' ') THEN
          DO ILEO=1,NLEO
            II=NOLEO(ILEO)
            DO JF=1,NF
              F_INTF(ILEO,JF)=0.D0
              DO K=1,NDIRE
                F_INTF(ILEO,JF)=F_INTF(ILEO,JF)+F(II,K,JF)*DTETAR
              ENDDO
              IF(ABS(F_INTF(ILEO,JF)).LT.1.D-90) F_INTF(ILEO,JF)=0.D0
            ENDDO
          ENDDO
          DO JF=1,NF
            WRITE(LUSPE,'(100(E10.4,2X))') FREQ(JF),
     &            (F_INTF(ILEO,JF),ILEO=1,NLEO)
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(F_INTF)
      RETURN
      END
