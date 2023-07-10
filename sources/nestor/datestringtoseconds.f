!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      FUNCTION  DateStringToSeconds              !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( timeString, lineCount )
!
      !   Convert a date given in a Character string to seconds since a "time-zero-point"
      !>  The "time-zero-point" is the date that is defined in the sis.cas file under
      !>  the key words   ORIGINAL DATE OF TIME    and   ORIGINAL HOUR OF TIME .
      !>  The "time-zero-point" is passed to this function as module variable "SisStart"
!
      USE m_TypeDefs_Nestor
      USE m_Nestor, ONLY : SisStart, ipid     ! "time-zero-point"
!
#ifndef NESTOR_INTERFACES
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop, Diff_Time
#endif
!
      IMPLICIT NONE
!
      CHARACTER (128), INTENT(IN) :: timeString !> date given in a character string
      INTEGER        , INTENT(IN) :: lineCount  !> current line number of DigAction file
                                                !  used only for error message
      REAL (KIND =R8) :: DateStringToSeconds    !> seconds since a "time-zero-point" (function name)
!
#ifndef NESTOR_INTERFACES
      !--------------------- local variables ---------------
!
      INTEGER         :: stat
      INTEGER         :: diffDays ,diffMinutes ,diffHours ,diffSeconds
      INTEGER(KIND=K8):: diffDays8,diffMinutes8,diffHours8,diffSeconds8
!
      TYPE( t_DateTime )    :: T
!
      INTEGER, DIMENSION(8) :: arrT0, arrT
!
      CHARACTER (128)       :: charTmp
      TYPE(t_String_Length) :: SRname     ! subroutine where the error occured
!
!      dbug WRITE(6,*)'?>-------  SR DateStringToSeconds ----------'
      SRname%s = "DateStringToSeconds"
!
      charTmp = ADJUSTL( timeString )  ! delete leading white spaces in string
      !WRITE(6,*)'?> charTmp2 =',charTmp     ! debug
!
      READ(charTmp, '(I4,5(1x,I2))',IOSTAT=stat)
     &   T%year, T%month ,T%day ,T%hour, T%minutes, T%seconds
!
      IF(stat /= 0 .OR. LEN_TRIM(charTmp) /= 19 ) THEN
        Call ErrMsgAndStop( "while read the Action file     "
     &  ,"         reason: format conflict, date-time format"
     &  ,"                 must be:   yyyy.mm.dd-hh:mm:ss   "
     &  ,"occured in line: ", lineCount, SRname, ipid)
      ENDIF
!
!
      arrT(1) =  T%year
      arrT(2) =  T%month
      arrT(3) =  T%day
      arrT(4) =  0
      arrT(5) =  T%hour
      arrT(6) =  T%minutes
      arrT(7) =  T%seconds
      arrT(8) =  0
!
      arrT0(1) = SisStart%year
      arrT0(2) = SisStart%month
      arrT0(3) = SisStart%day
      arrT0(4) = SisStart%zone
      arrT0(5) = SisStart%hour
      arrT0(6) = SisStart%minutes
      arrT0(7) = SisStart%seconds
      arrT0(8) = SisStart%milliseconds
!
!
      !  calculate the time differenze ( diffDays, diffHours, ...)
      !> between arrT0 ("time-zero-point") and arrT ( a given date (timeString) )
      CALL Diff_Time(  arrT0, arrT
     &            , diffDays, diffHours, diffMinutes, diffSeconds )
!
      WRITE(charTmp,*) diffDays       !  convert integer kind=4
      READ (charTmp,*) diffDays8      !>   to    integer kind=8
      WRITE(charTmp,*) diffHours      !  convert integer kind=4
      READ (charTmp,*) diffHours8     !>   to    integer kind=8
      WRITE(charTmp,*) diffMinutes    !  convert integer kind=4
      READ (charTmp,*) diffMinutes8   !>   to    integer kind=8
      WRITE(charTmp,*) diffSeconds    !  convert integer kind=4
      READ (charTmp,*) diffSeconds8   !>   to    integer kind=8
!
      diffSeconds8 =    diffSeconds8
     &                + diffMinutes8  * 60
     &                + diffHours8    * 3600
     &                + diffDays8     * 86400
!
      DateStringToSeconds = DBLE( diffSeconds8 )  ! return value of the function
!
      !WRITE(6,*) ' T0:',(arrT0(i), i=1,8)
      !WRITE(6,*) ' T :',(arrT(i),  i=1,8)
      !WRITE(6,*)'diffSeconds8 = ',diffSeconds8
!
!      dbug WRITE(6,*)'?>-------  SR DateStringToSeconds End ------'
!
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif
      END FUNCTION   DateStringToSeconds         !********************************************
!     END SUBROUTINE DateStringToSeconds         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
