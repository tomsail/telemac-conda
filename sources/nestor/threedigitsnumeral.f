!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      FUNCTION  ThreeDigitsNumeral               !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( checkStr )
!
      !> check if the first three digits in a character String are numerals
      !  and if the three digits value is bigger or equal 100 (first digit is not 0)
!
      IMPLICIT NONE
!
      CHARACTER (3) :: checkStr
!
      LOGICAL       :: ThreeDigitsNumeral
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
!      dbug WRITE(6,*)'?>-------  SR ThreeDigitsNumeral -----------'
!
      IF(   (     checkStr(1:1) == '1'
     &       .OR. checkStr(1:1) == '2'
     &       .OR. checkStr(1:1) == '3'
     &       .OR. checkStr(1:1) == '4'
     &       .OR. checkStr(1:1) == '5'
     &       .OR. checkStr(1:1) == '6'
     &       .OR. checkStr(1:1) == '7'
     &       .OR. checkStr(1:1) == '8'
     &       .OR. checkStr(1:1) == '9'
     &      )        .AND.
     &      (     checkStr(2:2) == '0'
     &       .OR. checkStr(2:2) == '1'
     &       .OR. checkStr(2:2) == '2'
     &       .OR. checkStr(2:2) == '3'
     &       .OR. checkStr(2:2) == '4'
     &       .OR. checkStr(2:2) == '5'
     &       .OR. checkStr(2:2) == '6'
     &       .OR. checkStr(2:2) == '7'
     &       .OR. checkStr(2:2) == '8'
     &       .OR. checkStr(2:2) == '9'
     &      )        .AND.
     &      (     checkStr(3:3) == '0'
     &       .OR. checkStr(3:3) == '1'
     &       .OR. checkStr(3:3) == '2'
     &       .OR. checkStr(3:3) == '3'
     &       .OR. checkStr(3:3) == '4'
     &       .OR. checkStr(3:3) == '5'
     &       .OR. checkStr(3:3) == '6'
     &       .OR. checkStr(3:3) == '7'
     &       .OR. checkStr(3:3) == '8'
     &       .OR. checkStr(3:3) == '9'
     &      )     )  THEN
        ThreeDigitsNumeral = .TRUE.
      ELSE
        ThreeDigitsNumeral = .FALSE.
      ENDIF
!
!      dbug WRITE(6,*)'?>-------  SR ThreeDigitsNumeral End -------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END FUNCTION   ThreeDigitsNumeral          !********************************************
!     END SUBROUTINE ThreeDigitsNumeral          !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************