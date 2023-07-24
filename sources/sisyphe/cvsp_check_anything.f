!                   ***************************
                    SUBROUTINE CVSP_CHECK_ANYTHING
!                   ***************************
!
!
!***********************************************************************
! SISYPHE   V7P2                                   12/02/2015
!***********************************************************************
!
!brief  CHECKS fractions for all points, all layers HIRANO and CVSP this
!is expensive and it partially checks things that are already beeing
!checked before.
!
!history U.MERKEL
!+        2015
!+        V6P3

      USE INTERFACE_SISYPHE
      USE BIEF_DEF
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_SPECIAL
      USE CVSP_OUTPUTFILES, ONLY: CP


      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL RET
      INTEGER  J,K

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
! Over all Points
!-----------------------------------------------------------------------

      IF(CP) THEN
        WRITE(LU,*) "----------------------------------------------"
        WRITE(LU,*) "CVSP Checking Anything.... EXPENSIVE DEBUGGING"
      ENDIF

      DO J=1,NPOIN

        DO K=1,NOMBLAY
          RET =  CVSP_CHECK_L(J,K,'Anything Hirano: ')
        ENDDO

        DO K=1,PRO_MAX(J)
          RET =  CVSP_CHECK_F(J,K,'Anything CVSM: ')
        ENDDO

        CALL CVSP_CHECK_STEADY(J)

      ENDDO
      IF(CP) WRITE(LU,*) "---------------------------------------------"

!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE CVSP_CHECK_ANYTHING

