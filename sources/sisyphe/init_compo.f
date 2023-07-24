!                   *********************
                    SUBROUTINE INIT_COMPO
!                   *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE   V7P3
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        2016
!+        V7P2
!+   Checking coherence of data: ZR+sediment height=ZF
!
!history  R.KOPMANN (BAW)
!+        30/01/2018
!+        V7P2
!+   Correct initialisation of all layers and consistency with cas-file
!+   by default
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K
      DOUBLE PRECISION EPAI,HEIGHT
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
!       BY DEFAULT : UNIFORM BED COMPOSITION
!
        NCOUCHES(J) = NOMBLAY
        DO K=1,NOMBLAY
          DO I = 1, NSICLA
            AVAIL(J,K,I) = AVA0(I)
          ENDDO
        END DO
        ES(J,1) = MIN(ELAY0,(ZF%R(J)-ZR%R(J)))
        DO K = 2, NOMBLAY
          HEIGHT = (ZF%R(J)-ZR%R(J)-ELAY0)
          IF(HEIGHT.GT.0.D0) THEN
            ES(J,K)=HEIGHT/(NOMBLAY-1)
          ELSE
            ES(J,K) = 0.D0
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      ! USER FUNCTION
      CALL USER_INIT_COMPO(NCOUCHES)
!
!-----------------------------------------------------------------------
!
!     CHECKING THE CONSISTENCY OF DATA
!     THE FORMULA USED HERE ZR+SED. HEIGHT = ZF CAN BE USED TO GIVE THE
!     HEIGHT OF THE LAST LAYER.
!
      DO J=1,MESH%NPOIN
        EPAI=0.D0
        DO I=1,NCOUCHES(J)
          EPAI=EPAI+ES(J,I)
        ENDDO
        IF(ABS(ZR%R(J)+EPAI-ZF%R(J)).GT.1.D-6) THEN
          WRITE(LU,*) 'INIT_COMPO, ERROR:'
          WRITE(LU,*) 'ZR+SEDIMENT HEIGHT=',ZR%R(J)+EPAI
          WRITE(LU,*) 'ZF=',ZF%R(J),' ZR=',ZR%R(J),
     &                ' SEDIMENT HEIGHT=',EPAI
          WRITE(LU,*) 'AT POINT ',J
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
