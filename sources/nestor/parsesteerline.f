!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ParseSteerLine                 !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (line, KeyWord, valueStr)
!
!
      USE m_TypeDefs_Nestor
!
#ifndef  NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  my_FLUSH
#endif
!
!
      IMPLICIT NONE
      CHARACTER (128), INTENT(IN)  :: line
      CHARACTER (128), INTENT(OUT) :: KeyWord
      CHARACTER (128), INTENT(OUT) :: valueStr
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER                      :: sepPos  !position of seperator "="
      CHARACTER (128)              :: str
!
      TYPE(t_String_Length) :: SRname     ! subroutine where the error occured
!
!      dbug WRITE(6,*)'?>-------  SR ParseSteerLine ---------------'
      SRname%s = "ParseSteerLine"     ! subroutine name
!
      !WRITE(6,*) 'line = >',line, '<'
      str    = line
      sepPos = INDEX(line,"/")
      IF( sepPos > 0) str = line(1:sepPos-1) !    "Keywordbalbla   =  valueblabla    / a comment    "
                                             ! -> "Keywordbalbla   =  valueblabla    "
      !WRITE(6,*)'str = >', str,'<'
!
      sepPos = INDEX(str,"=")
!
      KeyWord  = str(1:sepPos-1)              ! -> "Keywordbalbla   "
      valueStr = str(sepPos+1:LEN_TRIM(str))  ! -> "  valueblabla"
      valueStr = ADJUSTL(valueStr)            ! -> "valueblabla  "
!
      !KeyWord  = KeyWord(1:LEN_TRIM(KeyWord))
!
!
      !WRITE(6,*) 'KeyWord = >', KeyWord,'<'
      !WRITE(6,*) 'KeyWord = >', KeyWord(1:LEN_TRIM(KeyWord)),'<'
      !WRITE(6,*) 'valueStr = >', valueStr(1:LEN_TRIM(valueStr)),'<'
!
      CALL my_FLUSH(6)
!
!
!      dbug WRITE(6,*)'?>-------  SR ParseSteerLine End -----------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END SUBROUTINE ParseSteerLine              !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
