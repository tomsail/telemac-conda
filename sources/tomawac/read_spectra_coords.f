!                   ******************************
                    SUBROUTINE READ_SPECTRA_COORDS
!                   ******************************
!
     &     (FID,NP,XP,YP)
!
!***********************************************************************
!     TOMAWAC   V7P3                                   21/02/2017
!***********************************************************************
!
!     brief    READS A LIST OF COORDINATES FROM AN EXTERNAL TEXT FILE
!     +        AND ADDS THEM TO THE EXISTING COORDINATES
!
!     history  A. JOLY (EDF - LNHE)
!     +        21/02/2017
!     +        V7P3
!     +   CREATED

!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FID            |-->| ID OF THE TEXT FILE TO BE READ
!| NP             |<->| NUMPER OF POINTS READ
!| XP,YP          |<->| COORDINATES OF THE SPECTRUM POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_READ_SPECTRA_COORDS=>READ_SPECTRA_COORDS
!
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)                         :: FID
      INTEGER, INTENT(INOUT)                      :: NP
      DOUBLE PRECISION,ALLOCATABLE, INTENT(INOUT) :: XP(:),YP(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      INTEGER I_GLO
      DOUBLE PRECISION NE,ZP
!
!-----------------------------------------------------------------------
!
! CHECK TO SEE IF THE PRINTOUT POINTS ARE ONLY DEFINED ONE WAY
!
      IF(NP.GT.0)THEN
        WRITE(LU,*) ''
        WRITE(LU,*) '**************************************'
        WRITE(LU,*) '   ERROR : SPECTRA PRINTOUT POINTS'
        WRITE(LU,*) '   THE PRINTOUT POINTS ARE DEFINED'
        WRITE(LU,*) '   USING BOTH KEYWORDS AND A FILE'
        WRITE(LU,*) '**************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
! READ THE NUMBER OF POINTS
      READ(FID,*) NP,NE
      IF(ALLOCATED(XP))DEALLOCATE(XP)
      ALLOCATE(XP(NP))
      IF(ALLOCATED(YP))DEALLOCATE(YP)
      ALLOCATE(YP(NP))
! READ ALL THE POINTS
      DO I=1,NP
        READ(FID,*) I_GLO,XP(I),YP(I),ZP
      ENDDO
!
      RETURN
      END
