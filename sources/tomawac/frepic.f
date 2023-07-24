!                   *****************
                    SUBROUTINE FREPIC
!                   *****************
!
     &( FPIC, F, NF, NDIRE, NPOIN2 )
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE PEAK FREQUENCY FOR ALL THE NODES IN THE
!+                2D MESH. THIS PEAK FREQUENCY IS DEFINED AS THE
!+                DISCRETISED FREQUENCY FOR WHICH E(F) IS GREATEST.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FPIC           |<--| PEAK FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_FREPIC => FREPIC
      USE DECLARATIONS_TOMAWAC, ONLY : FREQ
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER,INTENT(IN)             :: NF    , NDIRE , NPOIN2
      DOUBLE PRECISION,INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION,INTENT(INOUT) :: FPIC(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION E, EMAX
!
      DO IP = 1,NPOIN2
        FPIC(IP) = 1.D-20
        EMAX = 0.D0
!
!.....LOOP OVER DISCRETISED FREQUENCIES
!     """""""""""""""""""""""""""""""""""""""""""""
        DO JF = 1,NF
!
!.......INTEGRATES WRT DIRECTIONS TO GET E(F)
!       """""""""""""""""""""""""""""""""""""""""""""""""
          E = 0.D0
          DO JP = 1, NDIRE
            E = E + F(IP,JP,JF)
          ENDDO                ! JP
!
!.......KEEPS THE MAXIMUM VALUE FOR E(F) AND ASSOCIATED FREQUENCY
!       """""""""""""""""""""""""""""""""""""""""""""""""""""
          IF (E.GT.EMAX) THEN
            EMAX = E
            FPIC(IP) = FREQ(JF)
          ENDIF
        ENDDO                   ! JF
      ENDDO                     ! IP
!
!
      RETURN
      END
