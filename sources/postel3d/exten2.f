!                       ***************************
                        CHARACTER(LEN=7) FUNCTION EXTEN2
!                       ***************************
!
     &(N,IPID)
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IPID,N
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IF(N.GT.0) THEN
!
        EXTEN2='000-000'
!
        IF(N.LT.10) THEN
          WRITE(EXTEN2(03:03),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTEN2(02:03),'(I2)') N
        ELSE
          WRITE(EXTEN2(01:03),'(I3)') N
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(EXTEN2(07:07),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(EXTEN2(06:07),'(I2)') IPID
        ELSE
          WRITE(EXTEN2(05:07),'(I3)') IPID
        ENDIF
!
      ELSE
!
        EXTEN2='       '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION
!
