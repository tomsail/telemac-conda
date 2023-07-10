!                   ****************
                    SUBROUTINE SOR3D
!                   ****************
!
     &(F,NDIRE,NF,NPOIN2,VENT,COURAN,MAREE,TITRE,TRA01,MESH3D)
!
!***********************************************************************
! TOMAWAC   V6P3                                   28/06/2011
!***********************************************************************
!
!brief    WRITES DATA NECESSARY TO RESUME COMPUTATION
!+                AT A LATER DATE.
!
!history  F MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        26/11/2012
!+        V6P3
!+   Correction of bugs and double precision.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| FMTR3D         |-->| GLOBAL RESULT FILE FORMAT
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| MAREE          |-->| LOGICAL INDICATING CONSIDERATION OF TIDE
!| MESH3D         |-->| MESH STRUCTURE IN 3D (I.E. INCLUDING DIRECTIONS)
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NR3D           |-->| LOGICAL UNIT NUMBER OF GLOBAL RESULT FILE
!| TITRE          |-->| TITLE
!| TRA01          |-->| DOUBLE PRECISION WORK TABLE OF SIZE NPOIN2*NDIRE
!| VENT           |-->| INDICATES IF WIND IS TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_TOMAWAC, ONLY : STRA40,STRA41,LUGEO,FMTGEO,LURBI,
     & FMTRBI,AT, UC, VC, UV, VV, DEPTH
      USE INTERFACE_TOMAWAC, EX_SOR3D => SOR3D
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2*NDIRE)
      LOGICAL, INTENT(IN)             :: COURAN,VENT,MAREE
      CHARACTER(LEN=80), INTENT(IN)   :: TITRE
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISTAT,I,IIF
!
      INTEGER, PARAMETER :: NFMAX = 999
      LOGICAL, ALLOCATABLE :: SORLEO(:)
      CHARACTER(LEN=32), ALLOCATABLE :: TEXTE(:)
!
      INTEGER :: DATE(3),TIME(3)
      PARAMETER ( DATE = (/ 0,0,0 /) )
      PARAMETER ( TIME = (/ 0,0,0 /) )
!
!***********************************************************************
!
      ALLOCATE(SORLEO(NF+2))
      ALLOCATE(TEXTE(NF+2))
      DO I=1,NF
        SORLEO(I)=.TRUE.
        TEXTE(I)='FREQUENCY 000                   '
        IF(I.LT.10) THEN
          WRITE(TEXTE(I)(13:13),'(I1)') I
        ELSEIF(I.LT.100) THEN
          WRITE(TEXTE(I)(12:13),'(I2)') I
        ELSEIF(I.LT.NFMAX+1) THEN
          WRITE(TEXTE(I)(11:13),'(I3)') I
        ELSE
          WRITE(LU,*) 'SOR3D: PARAMETER NFMAX MUST BE'
          WRITE(LU,*) '       INCREASED TO AT LEAST ',NF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
      DO I=1,NPOIN2*NDIRE
        MESH3D%Z%R(I)=0.D0
      ENDDO
      SORLEO(NF+1)=.TRUE.
      TEXTE(NF+1)='DEPTH                           '
      TEXTE(NF+2)='CURRENT-WIND                    '
      IF(COURAN.OR.VENT) THEN
        SORLEO(NF+2)=.TRUE.
      ELSE
        SORLEO(NF+2)=.FALSE.
      ENDIF
!
!     CREATES THE DATA FILE USING A GIVEN FILE FORMAT
!     THE DATA ARE CREATED IN THE FILE: NRES, AND IS
!     CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
!     CONTAINED IN THE FILE.
!
      CALL WRITE_HEADER(FMTRBI, ! RESULTS FILE FORMAT
     &                  LURBI,       ! LU FOR RESULTS FILE
     &                  TITRE,      ! TITLE
     &                  NF+2,       ! MAX NUMBER OF OUTPUT VARIABLES
     &                  TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                  SORLEO)     ! PRINT TO FILE OR NOT
!
!     WRITES THE MESH IN THE OUTPUT FILE
!
      CALL WRITE_MESH(FMTRBI,     ! RESULTS FILE FORMAT
     &                LURBI,       ! LU FOR RESULTS FILE
     &                MESH3D,
     &                NDIRE,      ! NUMBER OF PLANES
     &                DATE,       ! START DATE
     &                TIME,       ! START TIME
     &                STRA40,STRA41,
     &                NCSIZE.GT.1, NPTIR,
     &                NGEO=LUGEO,
     &                GEOFORMAT=FMTGEO)
!
! WRITES TIME
!
!     WRITES DATA INFORMATION
!
! WRITES F
!
      DO IIF=1,NF
        CALL ADD_DATA(FMTRBI,LURBI,TEXTE(IIF),AT,0,IIF.EQ.1,F(1,1,IIF),
     &                NPOIN2*NDIRE,ISTAT)
      ENDDO
!
!     WRITES DEPTH
!
      IF(MAREE) THEN
        DO I=1,NPOIN2
          TRA01(I)=DEPTH(I)
        ENDDO
      ENDIF
!
!     HERE TRA01 MAY BE WRITTEN FOR NOTHING (AND NOT INITIALISED)
!     THIS IS NECESSARY TO HAVE A REAL SERAFIN FORMAT
!
      CALL ADD_DATA(FMTRBI,LURBI,TEXTE(NF+1),AT,0,.FALSE.,TRA01,
     &                NPOIN2*NDIRE,ISTAT)
!
!     WRITES U,V,UV,VV (IF HAS TO)
!
      IF(VENT.AND.NDIRE.LT.4) THEN
        WRITE(LU,*) 'SOR3D: NDIRE MUST BE GREATER THAN 3'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(COURAN.AND.NDIRE.LT.2) THEN
        WRITE(LU,*) 'SOR3D: NDIRE MUST BE GREATER THAN 1'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(COURAN) THEN
        DO I=1,NPOIN2
          TRA01(I)=UC(I)
          TRA01(I+NPOIN2)=VC(I)
        ENDDO
      ENDIF
!
      IF(VENT) THEN
        DO I=1,NPOIN2
          TRA01(I+2*NPOIN2)=UV(I)
          TRA01(I+3*NPOIN2)=VV(I)
        ENDDO
      ENDIF
!
      IF(COURAN.OR.VENT) THEN
        CALL ADD_DATA(FMTRBI,LURBI,TEXTE(NF+2),AT,0,.FALSE.,TRA01,
     &                NPOIN2*NDIRE,ISTAT)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
