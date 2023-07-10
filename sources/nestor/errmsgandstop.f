!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ErrMsgAndStop                  !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (  MsgA, MsgB, MsgC, MsgD, mark, SR, ipid  )
!
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  Lu
!
!
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  my_FLUSH
#endif
!
!
      IMPLICIT NONE
      INTEGER  , INTENT(IN)      :: mark                    ! position of error e.g.: number of line
      INTEGER  , INTENT(IN)      :: ipid                    ! number of parallel thread where the error occured
      CHARACTER          (len=*) :: MsgA, MsgB, MsgC, MsgD  ! message strings
      TYPE(t_String_Length)      :: SR                      ! subroutine where the error occured
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
      INTEGER         :: U, LenT
      INTEGER         :: LenA, LenB, LenC, LenD  ! length of string A, of string B, of ...
      CHARACTER (512) :: StrT
      CHARACTER   (8) :: char_ipid   ! to store the value of ipid as string
!
669   FORMAT(' ?>',9(/,' ?> error:'))       ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
668   FORMAT(5(' ?> error:',/))             ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
661   FORMAT(1(' ?> error:'))               ! write 1 pseudo empty lines like " ?> error:         "                                                                  !
!
!
670   FORMAT( 1(' ?> error:'),60('='), '+' )
!     671   FORMAT( 1(' ?> error:'),60('-'), '+' )
672   FORMAT( 1(' ?> error:'),60(' '), '|' )
673   FORMAT( 1(' ?> error:'),11(' '),29A,27(' '),'|' )
!
!
      U = Lu   ! output will appear in the output file of the particular thread
!     U = 6    ! output of all threads will appear mixed up in the the output file of thread 0
!
      CALL my_FLUSH(U)
!
!
      WRITE( char_ipid, '(I8)') ipid
!
!      dbug WRITE(U,*)'?>-------  SR ErrMsgAndStop ----------------'
      StrT = "error in dredge module Nestor"
      StrT = StrT(1:29)//"                    |"
      LenT = 29 + 24
      WRITE(U,669)
      WRITE(U,670)
      WRITE(U,672)
      WRITE(U,673)   StrT(1:LenT)
      WRITE(U,672)
      WRITE(U,672)
!
      LenA = len_trim( MsgA )
      LenB = len_trim( MsgB )
      LenC = len_trim( MsgC )
      LenD = len_trim( MsgD )
      SR%i = len_trim( SR%s )
!
      StrT = " ?> error:  occured in Subroutine       "//SR%s(1:SR%i)
      LenT = 40 + SR%i
      WRITE(U,'(A,1X)' ) StrT(1:LenT)
!
      StrT = " ?> error:  occured in parallel thread  "
      WRITE(U,'(A,A8)' ) StrT(1:40), adjustl(char_ipid)
      WRITE(U,661)
!
      StrT = " ?> error:  "//MsgA(1:LenA)
      LenT = 13 + LenA
      WRITE(U,'(A,1X)' ) StrT(1:LenT)
      StrT = " ?> error:  "//MsgB(1:LenB)
      LenT = 13 + LenB
      WRITE(U,'(A,1X)' ) StrT(1:LenT)
      StrT = " ?> error:  "//MsgC(1:LenC)
      LenT = 13 + LenC
      WRITE(U,'(A,1X)' ) StrT(1:LenT)
      StrT = " ?> error:  "//MsgD(1:LenD)
      LenT = 13 + LenD
!
      IF( mark == -1 ) THEN
        WRITE(U,'(A,1X)' ) StrT(1:LenT)
      ELSE
        WRITE(U,'(A,1X,I5)' ) StrT(1:LenT), mark
      ENDIF
      WRITE(U,661)
      WRITE(U,661)
      WRITE(U,661)
      WRITE(U,672)
      WRITE(U,672)
      WRITE(U,670)
      WRITE(U,668)
!
      CALL my_FLUSH(U)
!
!      IF( ParallelComputing ) CALL P_SYNC()
!
      STOP
!
!      dbug WRITE(U,*)'?>-------  SR ErrMsgAndStop End ------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE ErrMsgAndStop               !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
