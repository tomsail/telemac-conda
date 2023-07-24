!
!
      SUBROUTINE  my_FLUSH
     &(ID)
!
#if defined NO_STD_FLUSH || NAGFOR
      USE F90_UNIX_IO, ONLY: FLUSH
#endif
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ID
!
#ifndef NESTOR_INTERFACES
      !--------------------- local ----------
!
#if defined(NO_STD_FLUSH)
      CALL FLUSH(ID)
#else
      FLUSH(ID)
#endif
!
#endif
!
      END SUBROUTINE my_FLUSH
