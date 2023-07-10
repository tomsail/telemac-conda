!                   ******************************
                    SUBROUTINE COMPLETE_EBE_FLUINT
!                   ******************************
!
     &(EBE_FLUINT,NELEM2,NPLAN)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EBE_FLUINT     |<->| INTERIOR FLUXES TO BE COMPLETED
!| NELEM2         |-->| NUMBER OF 2D ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES ON THE VERTICAL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM2,NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: EBE_FLUINT(NELEM2,NPLAN-1,6)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ILAYER,NLAYER,IELEM2
!
!-----------------------------------------------------------------------
!
      NLAYER=NPLAN-1
!
!-----------------------------------------------------------------------
!
!     UPPER LAYER: INITIALISING WITH COEFFICIENTS OF LOWER PLANE
!
      DO IELEM2=1,NELEM2
        EBE_FLUINT(IELEM2,NLAYER,4)=-EBE_FLUINT(IELEM2,NLAYER,1)
        EBE_FLUINT(IELEM2,NLAYER,5)=-EBE_FLUINT(IELEM2,NLAYER,2)
        EBE_FLUINT(IELEM2,NLAYER,6)=-EBE_FLUINT(IELEM2,NLAYER,3)
      ENDDO
!
!     OTHER LAYERS
!
      IF(NLAYER.GE.2) THEN
        DO ILAYER=1,NLAYER-1
          DO IELEM2=1,NELEM2
            EBE_FLUINT(IELEM2,NLAYER,4)=EBE_FLUINT(IELEM2,NLAYER,4)
     &                                 -EBE_FLUINT(IELEM2,ILAYER,1)
     &                                 -EBE_FLUINT(IELEM2,ILAYER,4)
            EBE_FLUINT(IELEM2,NLAYER,5)=EBE_FLUINT(IELEM2,NLAYER,5)
     &                                 -EBE_FLUINT(IELEM2,ILAYER,2)
     &                                 -EBE_FLUINT(IELEM2,ILAYER,5)
            EBE_FLUINT(IELEM2,NLAYER,6)=EBE_FLUINT(IELEM2,NLAYER,6)
     &                                 -EBE_FLUINT(IELEM2,ILAYER,3)
     &                                 -EBE_FLUINT(IELEM2,ILAYER,6)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
