!                   *************************
                    SUBROUTINE ENTETE_SISYPHE
!                   *************************
!
     &(IETAPE,AT,LT)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    WRITES HEADINGS TO THE LISTING
!+                AT VARIOUS STAGES OF THE PROGRAM.
!
!history  J-M HERVOUET (LNHE)
!+        06/06/2008
!+        V5P9
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IETAPE         |-->| SHOW COMPUTATION STEPS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: AT
!
      INTEGER, INTENT(IN):: LT,IETAPE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J,H,M
      DOUBLE PRECISION S
      INTRINSIC INT
      CHARACTER(LEN=32) :: FR(15),GB(15)
!
!-----------------------------------------------------------------------
!
!     AD: NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
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
          IF(LNG.EQ.LNG_FR) WRITE(LU,10) FR(11),LT,FR(9),J,H,M,S,AT
          IF(LNG.EQ.LNG_EN) WRITE(LU,11) GB(11),LT,GB(9),J,H,M,S,AT
        ELSEIF(H.NE.0) THEN
          IF(LNG.EQ.LNG_FR) WRITE(LU,20) FR(11),LT,FR(9),H,M,S,AT
          IF(LNG.EQ.LNG_EN) WRITE(LU,20) GB(11),LT,GB(9),H,M,S,AT
        ELSEIF(M.NE.0) THEN
          IF(LNG.EQ.LNG_FR) WRITE(LU,30) FR(11),LT,FR(9),M,S,AT
          IF(LNG.EQ.LNG_EN) WRITE(LU,30) GB(11),LT,GB(9),M,S,AT
        ELSE
          IF(LNG.EQ.LNG_FR) WRITE(LU,40) FR(11),LT,FR(9),S
          IF(LNG.EQ.LNG_EN) WRITE(LU,40) GB(11),LT,GB(9),S
        ENDIF
!       AN EMPTY LINE
        WRITE(LU,*)
!
!   WRITES OUT: TITLES OF THE STAGES
!
      ELSE
!
        IF(LNG.EQ.LNG_FR) WRITE(LU,200) FR(IETAPE)
        IF(LNG.EQ.LNG_EN) WRITE(LU,200) GB(IETAPE)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
10     FORMAT(/,80('='),/,1X,A10,I8,A10,
     &     1I4,' J ',1I2,' H ',1I2,' MIN ',F8.4,' S',5X,'(',F15.4,' S)')
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
