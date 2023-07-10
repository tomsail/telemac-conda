!                   **********************
                    SUBROUTINE USER_CORPOR
!                   **********************
!
     &(POROS)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER MODIFIES THE POROSITY OF ELEMENTS.
!
!history  J-M HERVOUET (LNHE)
!+        01/03/1990
!+        V5P2
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| POROS          |<->| POROSITY TO BE MODIFIED.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: POROS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSOM(4),YSOM(4),XX1,YY1
      INTEGER NSOM,IELEM
!
!-----------------------------------------------------------------------
!
!
      NSOM = 4
      XSOM(1) = -50.D0
      XSOM(2) =  50.D0
      XSOM(3) =  50.D0
      XSOM(4) = -50.D0
      YSOM(1) = -21.D0
      YSOM(2) = -21.D0
      YSOM(3) =  21.D0
      YSOM(4) =  21.D0
!
!-----------------------------------------------------------------------
!
      CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )
!
!--------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        XX1 = (  X(IKLE%I(IELEM)          )+
     &           X(IKLE%I(IELEM+NELMAX)   )+
     &           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0
        YY1 = (  Y(IKLE%I(IELEM)          )+
     &           Y(IKLE%I(IELEM+NELMAX)   )+
     &           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!
        IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
          POROS%R(IELEM) = 0.5D0 * ( 1.D0 + ABS(XX1/50.D0) )
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
