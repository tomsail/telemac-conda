!                       **********************
                        SUBROUTINE CVSP_P_GAIA
!                       **********************
!
     &(PATH_PRE,FILE_PRE,JG)
!
!***********************************************************************
! GAIA   V8P1                                   16/05/2017
!***********************************************************************
!
!>@brief   CSV-FILE OUTPUT OF A VERTICAL SORTING PROFILE IN POINT J
!
!>@history UWE MERKEL
!!        20/07/2011
!!        V6P3
!
!>@history  P. A. TASSI (EDF R&D, LNHE)
!!        12/03/2013
!!        V6P3
!!   Cleaning, cosmetic
!
!>@history  J-M HERVOUET (EDF R&D, LNHE)
!!        02/01/2014
!!        V7P0
!!   Use of KNOGL replaced by GLOBAL_TO_LOCAL_POINT.
!
!>@history UWE MERKEL, R. KOPMANN (BAW)
!!        2016 / 2017
!!        V6P3 / V7P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] PATH_PRE Where to save
!>@param[in] FILE_PRE Filenametrunk
!>@param[in] JG Global point number
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_GAIA
      USE BIEF
      USE BIEF_DEF
      USE CVSP_OUTPUTFILES_GAIA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER     , INTENT(IN)  :: JG
      CHARACTER(*), INTENT(IN) :: PATH_PRE
      CHARACTER(*), INTENT(IN) :: FILE_PRE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=100) DEBUGFILE
      CHARACTER(LEN=7) OCSTR
      INTEGER I, K, J
      DOUBLE PRECISION AT, BSUM
      INTEGER :: ID
!
!-----------------------------------------------------------------------
!
      AT = DT*LT/PERCOU
      J = JG
      OUTPUTCOUNTER = OUTPUTCOUNTER + 1
!
!     GLOBAL NUMBERS TO LOCAL NUMBERS
!
      IF(NCSIZE.GT.1) THEN
        J = GLOBAL_TO_LOCAL_POINT(JG,MESH)
      ENDIF
!
      WRITE(UNIT=OCSTR, FMT='(I7)') OUTPUTCOUNTER
      DO I=1,7
        IF(OCSTR(I:I)==' ') OCSTR(I:I)='0'
      ENDDO
!
      WRITE(UNIT=DEBUGFILE, FMT='(A,A,A,A,I8,A,G15.8,A)')
     &     PATH_PRE,OCSTR,'_',FILE_PRE,
     &     JG,'_T_',AT,'_VSP.CSV'

      DO I=1,LEN_TRIM(DEBUGFILE)
        IF(DEBUGFILE(I:I)==' ') DEBUGFILE(I:I)='_'
      ENDDO
!
      IF(J > 0) THEN
        CALL GET_FREE_ID(ID)
        OPEN(ID, FILE=DEBUGFILE, STATUS='UNKNOWN' )
        REWIND ID
        WRITE(ID,*)
     &"J K FD50(I) AT PRO_D(K_I) PRO_F(K_I) X Y D50 ALT TOB HEIGHT"
!
        DO K=1,PRO_MAX(J)
          BSUM = 0.D0
          DO I=1,NSICLA
            BSUM = DCLA(I)*PRO_F(J,PRO_MAX(J)+1-K,I) + BSUM
          ENDDO
!
          DO I=1,NSICLA
            IF(K.EQ.1) THEN
! FULL OUTPUT WITH COORDINATES ETC. ON SURFACE
              WRITE (ID,'(I8,1X,I4,1X,10(G20.12,1X))')
     &              JG,PRO_MAX(J)+1-K,DCLA(I),AT,
     &              PRO_D(J,PRO_MAX(J)+1-K,I),
     &              PRO_F(J,PRO_MAX(J)+1-K,I),X(J),Y(J),
     &              BSUM,ES(J,1),TOB%R(J), Z%R(J)
            ELSE
! FOLLOWING SECTIONS
              WRITE (ID,'(I8,1X,I4,1X,5(G20.12,1X))')
     &              JG,PRO_MAX(J)+1-K,DCLA(I),AT,
     &              PRO_D(J,PRO_MAX(J)+1-K,I),
     &              PRO_F(J,PRO_MAX(J)+1-K,I)
            ENDIF
          ENDDO
        ENDDO
!
        BSUM = 0.D0
        DO I=1,NSICLA
          BSUM = DCLA(I)*PRO_F(J,1,I) + BSUM
        ENDDO
!
        CLOSE(ID)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_P_GAIA
