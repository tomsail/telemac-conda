!                   *********************************
                    SUBROUTINE GET_TOMSPEC_DIMENSIONS
!                   *********************************
!
     &(NSPEC1,NDIR1,NF1)
!
!***********************************************************************
! ARTEMIS   V7P2                                     Feb 2017
!***********************************************************************
!
!brief    READS OR COMPUTES THE VALUES OF NSPEC1, NDIR1, NF1
!+        IN TOMAWAC ENERGY SPECTRA.
!
!history  N.DURAND (HRW)
!+        June 2014
!+        V7P0
!+
!
!history  N.DURAND (HRW)
!+        Feb 2017
!+        V7P2
!+   Revisited to use the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NSPEC1         |<--| NUMBER OF TOMAWAC SPECTRA IN FILE
!| NDIR1          |<--| NUMBER OF DISCRETE DIRECTIONS
!| NF1            |<--| NUMBER OF DISCRETE FREQUENCIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, DUMMY => GET_TOMSPEC_DIMENSIONS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(OUT)         :: NSPEC1  ! NUMBER OF SPECTRA
      INTEGER, INTENT(OUT)         :: NDIR1   ! NUMBER OF DIRECTIONS
      INTEGER, INTENT(OUT)         :: NF1     ! NUMBER OF FREQUENCIES
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                      :: NFIC, IERR
      INTEGER                      :: TYP,NPOIN1,NNELEM1
!
      CHARACTER(LEN=8)             :: FFORMAT
!
!-----------------------------------------------------------------------
!
      NFIC = ART_FILES(WACSPE)%LU
      FFORMAT = ART_FILES(WACSPE)%FMT
!     THE SUPPORT MESH FOR TOMAWAC SPECTRA IS MADE OF QUADRANGLES
      TYP = QUADRANGLE_ELT_TYPE
!
!-----------------------------------------------------------------------
!
!     CHOSE NOT TO CALL READ_MESH_INFO HERE BECAUSE IT CONTAINS EXTRA
!     INFORMATION WHICH IS NOT RELEVANT IN THIS CASE
!     => CALLED INDIVIDUAL ROUTINES INSTEAD
!
!     NUMBER OF SPECTRA/POINTS IN THE TOMAWAC SPECTRAL FILE
      CALL GET_DATA_NVAR(FFORMAT,NFIC,NSPEC1,IERR)
      CALL CHECK_CALL(IERR,'GET_TOMSPEC_DIMENSIONS:GET_DATA_NVAR')
!
!     NUMBER OF POINTS DISCRETISING THE SPECTRUM IE NF1*NDIR1
!
      CALL GET_MESH_NPOIN(FFORMAT,NFIC,TYP,NPOIN1,IERR)
      CALL CHECK_CALL(IERR,'GET_TOMSPEC_DIMENSIONS:GET_MESH_NPOIN')
!
!     NUMBER OF DIRECTIONS DISCRETISING THE SPECTRUM
!
      CALL GET_MESH_NELEM(FFORMAT,NFIC,TYP,NNELEM1,IERR)   ! NNELEM1 is NPOIN1-NDIR1
      CALL CHECK_CALL(IERR,
     &          'GET_TOMSPEC_DIMENSIONS:GET_MESH_NELEM:QUADRANGLE')
      NDIR1 = NPOIN1-NNELEM1
!
!     NUMBER OF FREQUENCIES DISCRETISING THE SPECTRUM
!
      NF1 = NPOIN1/NDIR1
!
!-----------------------------------------------------------------------
!
!     PRINTOUT:
!
      WRITE(LU,301) NSPEC1,NDIR1,NF1
      WRITE(LU,*) ' '
!
301   FORMAT(1X,//,1X,'GET_TOMSPEC_DIMENSIONS :',
     &       1X, 'NUMBER OF SPECTRA:',1I12,/,
     &       26X,'NUMBER OF DIRECTIONS:',1I9,/,
     &       26X,'NUMBER OF FREQUENCIES:',1I8,/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
