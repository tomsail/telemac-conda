!                   *************************
                    SUBROUTINE ENTETE_GAIA
!                   *************************
!
     &(IETAPE,AT,LT)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Writes headings to the listing at various stages of the program.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] IETAPE  Show computation steps
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: AT
!
      INTEGER, INTENT(IN):: LT,IETAPE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J,H,M
      DOUBLE PRECISION S
      INTRINSIC INT
      CHARACTER(LEN=32) :: FR(15),GB(15)
!
!-----------------------------------------------------------------------
!
      PARAMETER ( FR = (/
     &     '                                ' ,
     &     '                                ' ,
     &     '     ETAPE DE CONVECTION        ' ,
     &     '       MODELE K-EPSILON         ' ,
     &     '  ETAPE DE DIFFUSION DU TRACEUR ' ,
     &     ' ETAPE DE DIFFUSION-PROPAGATION ' ,
     &     '      BILAN DE VOLUME D''EAU     ' ,
     &     ' BILAN FINAL DE VOLUME D''EAU    ' ,
     &     '  TEMPS :                       ' ,
     &     ' SECONDES                       ' ,
     &     'ITERATION                       ' ,
     &     '     DERIVE DE FLOTTEUR(S)      ' ,
     &     '   DERIVE(S) LAGRANGIENNE(S)    ' ,
     &     '     GLISSEMENT DU SEDIMENT     ' ,
     &     '      TASSEMENT DU SEDIMENT     '  /) )
      PARAMETER ( GB = (/
     &     '                                ' ,
     &     '                                ' ,
     &     '        ADVECTION STEP          ' ,
     &     '        K-EPSILON MODEL         ' ,
     &     '   DIFFUSION OF TRACER STEP     ' ,
     &     '  DIFFUSION-PROPAGATION STEP    ' ,
     &     '     BALANCE OF WATER VOLUME    ' ,
     &     ' FINAL BALANCE OF WATER VOLUME  ' ,
     &     '    TIME:                       ' ,
     &     ' SECONDS                        ' ,
     &     'ITERATION                       ' ,
     &     '       DRIFT OF DROGUE(S)       ' ,
     &     '      LAGRANGIAN DRIFT(S)       ' ,
     &     '         SEDIMENT SLIDE         ' ,
     &     '          CONSOLIDATION         '  /) )
!
!-----------------------------------------------------------------------
!
!  DECOMPOSITION OF TIME IN DAYS, HOURS, MINUTES AND SECONDS
!
      S = AT
      J = INT(AT/86400.D0)
      S = S - 86400.D0 * J
      H = INT(S/3600.D0)
      S = S - 3600.D0 * H
      M = INT(S/60.D0)
      S = S - 60.D0 * M
!
!-----------------------------------------------------------------------
!
!   WRITES OUT: TIME AND ITERATIONS
!
      IF (IETAPE.EQ.1.OR.IETAPE.EQ.2) THEN
!
        IF(J.NE.0) THEN
          WRITE(LU,11) GB(11),LT,GB(9),J,H,M,S,AT
        ELSEIF(H.NE.0) THEN
          WRITE(LU,20) GB(11),LT,GB(9),H,M,S,AT
        ELSEIF(M.NE.0) THEN
          WRITE(LU,30) GB(11),LT,GB(9),M,S,AT
        ELSE
          WRITE(LU,40) GB(11),LT,GB(9),S
        ENDIF
!       AN EMPTY LINE
        WRITE(LU,*)
!
!   WRITES OUT: TITLES OF THE STAGES
!
      ELSE
!
        WRITE(LU,200) GB(IETAPE)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
11     FORMAT(/,80('='),/,1X,A10,I8,A10,
     &     1I4,' D ',1I2,' H ',1I2,' MN ',F8.4,' S',5X,'(',F15.4,' S)')
20     FORMAT(/,80('='),/,1X,A10,I8,A10,1I2,' H ',1I2,' MIN ',F8.4,' S',
     &                                               5X,'(',F12.4,' S)')
30     FORMAT(/,80('='),/,1X,A10,I8,A10,1I2,' MN ',F8.4,' S',
     &                                               5X,'(',F12.4,' S)')
40     FORMAT(/,80('='),/,1X,A10,I8,A10,F8.4,' S')
200    FORMAT(80('-'),/,18X,A32)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
