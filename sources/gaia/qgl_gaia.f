!                   **********************************
                    DOUBLE PRECISION FUNCTION QGL_GAIA
!                   **********************************
!
     &(I,AT)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Prescribes the solid discharge for  imposed liquid boundaries.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] I  Number of liquid boundary
!>@param[in] N  Global number of point.
!!              In parallel number in the original mesh
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
!
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(IN):: AT
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
!
!-----------------------------------------------------------------------
!
!     IF THE LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
      IF(OKQGL(I).AND.GAI_FILES(GAILIQ)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE QGL_GAIA(1), QGL_GAIA(2), ETC, QGL_GAIA(9), DEPENDING ON I
        FCT='QG(      '
        IF(I.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:5)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:6)=')'
        ELSE
          WRITE(LU,*)'I=',I
          WRITE(LU,*) 'QGL_GAIA NOT PROGRAMMED FOR MORE THAN 99
     &    BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        CALL READ_FIC_CONC_GAIA(QGL_GAIA,FCT,AT,GAI_FILES(GAILIQ)%LU,
     &                     ENTET,OKQGL(I))
!
      ENDIF
!
      IF(.NOT.OKQGL(I).OR.GAI_FILES(GAILIQ)%NAME(1:1).EQ.' ') THEN
!       PROGRAMMABLE PART
!       SL IS READ FROM THE STEERING FILE, BUT MAY BE CHANGED
        WRITE(LU,101) I
101     FORMAT(1X,/,1X,'QG: MORE PRESCRIBED SOLID DISCHARGES '
     &           ,/,1X,'     ARE REQUIRED IN THE PARAMETER FILE'
     &           ,/,1X,'     AT LEAST ',1I6,' MUST BE GIVEN')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
