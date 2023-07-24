!                     **************************
                      SUBROUTINE OUTPUT3D_KHIONE
!                     **************************
!
     &(TITLE,AT,LT,MESH,DATE,MARTIM,ISHEAD,ISMESH,ISVARS,
     & TN,NPOIN3,NPLAN)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    "WRAPPER" FOR WRITE_HEADER, WRITE_MESH AND DESIMP SO THAT
!+            OUTPUTS CAN BE DONE FROM WITHIN TELEMAC-3D WHEN USING
!+            THE COUPLED MODEL RATHER THAN CONFLICTING PRINTOUT
!+            PERIODS, VARSOR, MAXVAR, ETC.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| CURRENT TIME IN SECONDS
!| DATE           |-->| START DATE
!| ISHEAD         !-->| 1: WRITE HEADER; 0: DOES NOT WRITE HEADER
!| ISMESH         !-->| 1: WRITE MESH; 0: DOES NOT WRITE MESH
!| ISVARS         !-->| 1: WRITE VARS; 0: DOES NOT WRITE VARS
!| LT             |-->| CURRENT NUMBER OF OF TIME STEP
!| MARTIM         |-->| START TIME
!| M3D            |-->| TRUE IF CALLED BY T3D
!| MESH           |-->| MESH STRUCTURE
!| TITLE          |-->| STUDY TITLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_KHIONE, EX_OUTPUT3D_KHIONE => OUTPUT3D_KHIONE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=72),     INTENT(IN) :: TITLE
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
      TYPE(BIEF_OBJ),        INTENT(IN) :: TN
      INTEGER, DIMENSION(3), INTENT(IN) :: DATE
      INTEGER, DIMENSION(3), INTENT(IN) :: MARTIM
      DOUBLE PRECISION,      INTENT(IN) :: AT
      INTEGER,               INTENT(IN) :: LT
      LOGICAL,               INTENT(IN) :: ISHEAD,ISMESH,ISVARS
      INTEGER,               INTENT(IN) :: NPOIN3,NPLAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     PREPARES THE RESULTS FILE FOR KHIONE
!
      IF( ICE_FILES(ICER3D)%NAME(1:1).EQ.' ' ) RETURN
!     ______________________________________________________________
!     HEADER
      IF( ISHEAD ) THEN
!
        CALL WRITE_HEADER(
     &    ICE_FILES(ICER3D)%FMT,  ! RESULTS FILE FORMAT
     &    ICE_FILES(ICER3D)%LU,   ! LU FOR RESULTS FILE
     &    TITLE,                  ! TITLE
     &    MAXVAR,                 ! MAX NUMBER OF OUTPUT VARIABLES
     &    TEXTE3,                  ! NAMES OF OUTPUT VARIABLES
     &    SORLEO3)                 ! PRINT TO FILE OR NOT
!
      ENDIF
!
!     ______________________________________________________________
!     MESH
      IF( ISMESH ) THEN
!
        CALL WRITE_MESH(
     &    ICE_FILES(ICER3D)%FMT,  ! RESULTS FILE FORMAT
     &    ICE_FILES(ICER3D)%LU,   ! LU FOR RESULTS FILE
     &    MESH,
     &    NPLAN,                  ! NUMBER OF PLANES /NA/
     &    DATE,                   ! START DATE
     &    MARTIM,                 ! START TIME
     &    T1_3,T2_3,                  ! TEMPORARY ARRAYS
     &    NCSIZE.GT.1, NPTIR)
!
      ENDIF
!
!     ______________________________________________________________
!     TIME VARIABLES
      IF( ISVARS ) THEN
!
        CALL PRERES3D_KHIONE(LT,TN,NPOIN3,MESH)
!
!       OUTPUTS A STANDARD TIME STEP
        CALL BIEF_DESIMP(
     &    ICE_FILES(ICER3D)%FMT,  ! RESULTS FILE FORMAT
     &    VARSO3,                 ! POINTERS TO CORE VARIABLES
     &    NPOIN3,                  ! NUMBER OF POINTS IN THE LOCAL MESH
     &    ICE_FILES(ICER3D)%LU,   ! LU FOR RESULTS FILE
     &    AT,LT,
     &    LISPRD,LEOPRD,
     &    SORLEO3,SORIMP3,
     &    MAXVAR,TEXTE3,0,0 )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
