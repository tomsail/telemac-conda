!                       ******************
                        SUBROUTINE ALLSPEC
!                       ******************
!
     &(SPEC,NOM)
!
!***********************************************************************
! ARTEMIS   V8P0                                     Jan 2019
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR A SPECTRAL DATA STRUCTURE : SPEC.
!
!history  N.DURAND (HRW)
!+        09/03/01
!+
!+   Original
!
!history  SRIRAM VENKATACHALAM (HRW)
!+        13/08/08
!+
!+   Modified to be compatible with Artemis v5.8
!+   After executing allvec, the newer version will fill the arrays with
!+   huge numbers in order to check whether the array is initialised or
!+   not. Hence, we need to re-initialise the array with zeros.
!
!history  N.DURAND (HRW)
!+        20/06/14
!+        V7P0
!+   bief_allvec not used anymore as support is not MESH
!+   Streamlined for V7P0
!
!history  N.DURAND (HRW)
!+        Feb 2017
!+        V7P2
!+   Updated for V7P2
!
!history  N.DURAND (HRW)
!+        Jan 2019
!+        V8P0
!+   Tidied up
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SPEC           |<--| STRUCTURE TO BE ALLOCATED
!| NOM            |-->| NAME OF THE STRUCTURE SPECTRUM
!| OUTERMODEL     |-->| IS THE STRUCTURE FOR THE OUTER MODEL OR ARTEMIS?
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
      CHARACTER(LEN=6) , INTENT(IN)           :: NOM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IERR
      INTEGER I,II,JJ
!
!-----------------------------------------------------------------------
!  HEADER COMMON TO ALL OBJECTS IN SPEC
!-----------------------------------------------------------------------
!
      SPEC%NAME  = NOM
!
!     NUMBER OF OBJECTS IN SPEC
      SPEC%N = NSPEC
!
!     DIRECTION COMPONENTS IN SPEC
!     +1 TO CLOSE THE LOOP
      ALLOCATE (SPEC%DIR(NDIR+1),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ALLSPEC:SPEC%DIR')
      CALL OV('X=C     ',SPEC%DIR,SPEC%DIR,SPEC%DIR,0.D0,NDIR+1)
!
!     FREQUENCY COMPONENTS
      ALLOCATE (SPEC%FRE(NF),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ALLSPEC:SPEC%FRE')
      CALL OV('X=C     ',SPEC%FRE,SPEC%FRE,SPEC%FRE,0.D0,NF)
!
!_______________________________________________________________________
!
!     NODE NUMBER
      ALLOCATE (SPEC%NOUTER(NSPEC),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ALLSPEC:SPEC%NOUTER')
      DO I=1,NSPEC
        SPEC%NOUTER(I) = 0
      ENDDO
!     X COORDINATE
      ALLOCATE (SPEC%XOUTER(NSPEC),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ALLSPEC:SPEC%XOUTER')
      CALL OV
     & ('X=C     ',SPEC%XOUTER,SPEC%XOUTER,SPEC%XOUTER,0.D0,NSPEC)
!     Y COORDINATE
      ALLOCATE (SPEC%YOUTER(NSPEC),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ALLSPEC:SPEC%YOUTER')
      CALL OV
     & ('X=C     ',SPEC%YOUTER,SPEC%YOUTER,SPEC%YOUTER,0.D0,NSPEC)
!
!-----------------------------------------------------------------------
!     ALLOCATES THE POINTERS ARRAY ADR
!-----------------------------------------------------------------------
!
      ALLOCATE(SPEC%ADR(SPEC%N),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ALLSPEC:SPEC%ADR')
      WRITE(LU,*) 'SPECTRUM: ',NOM,' ALLOCATED'
!
!_______________________________________________________________________
!
      DO I=1,NSPEC
        ALLOCATE(SPEC%ADR(I)%SOUTER(NF,NDIR+1),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'ALLSPEC:SPEC%ADR(I)%SOUTER')
        DO II=1,NF
          DO JJ=1,NDIR+1
            SPEC%ADR(I)%SOUTER(II,JJ)=0.D0
          ENDDO
        ENDDO
      ENDDO
!
!_______________________________________________________________________
!
      RETURN
      END
