!                   *************************
                    SUBROUTINE NOMVAR_ARTEMIS
!                   *************************
!
     &(TEXTE,TEXTPR,MNEMO)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (TEXT) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (TEXTPR).
!+
!+                TEXT AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!history  D. AELBRECHT (LNH)
!+        13/01/1998
!+        V5P1
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
!| MNEMO          |---| ALIASES FOR VARIALBLES
!| TEXTE          |<--| NAME OF VARIABLES
!| TEXTPR         |<--| VARIABLES NAME OF PREVIOUS CALCULATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_NOMVAR_ARTEMIS => NOMVAR_ARTEMIS
      USE DECLARATIONS_ARTEMIS, ONLY : MAXVAR,NVAR_ART
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      CHARACTER(LEN=8), INTENT(INOUT) :: MNEMO(MAXVAR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.LNG_EN) THEN
!
      TEXTE (1 ) = 'WAVE HEIGHT     M               '
      TEXTE (2 ) = 'WAVE PHASE      RAD             '
      TEXTE (3 ) = 'U0 SURFACE      M/S             '
      TEXTE (4 ) = 'V0 SURFACE      M/S             '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'STILL WATER H   M               '
      TEXTE (8 ) = 'PHASE VELOCITY  M/S             '
      TEXTE (9 ) = 'GROUP VELOCITY  M/S             '
      TEXTE (10) = 'WAVE NUMBER     1/M             '
      TEXTE (11) = 'REAL POTENTIAL  M2/S            '
      TEXTE (12) = 'IMAG POTENTIAL  M2/S            '
      TEXTE (13) = 'PRIVATE 1       UNIT   ??       '
      TEXTE (14) = 'PRIVATE 2       UNIT   ??       '
      TEXTE (15) = 'PRIVATE 3       UNIT   ??       '
      TEXTE (16) = 'PRIVATE 4       UNIT   ??       '
      TEXTE (17) = 'T01             S               '
      TEXTE (18) = 'T02             S               '
      TEXTE (19) = 'TM              S               '
      TEXTE (20) = 'FORCE_FX        M/S2            '
      TEXTE (21) = 'FORCE_FY        M/S2            '
      TEXTE (22) = 'WAVE INCIDENCE  DEG             '
      TEXTE (23) = 'QB                              '
      TEXTE (24) = 'STRESS_SXX      M3/S2           '
      TEXTE (25) = 'STRESS_SXY      M3/S2           '
      TEXTE (26) = 'STRESS_SYY      M3/S2           '
!
! TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE UNLESS ANOTHER CODE WAS USED TO
! GENERATE THE PREVIOUS RESULT, IN WHICH CASE THE OUTPUT
! VARIABLE NAMES HAVE TO BE WRITTEN HERE.
!
      TEXTPR (1 ) = 'WAVE HEIGHT     M               '
      TEXTPR (2 ) = 'WAVE PHASE      RAD             '
      TEXTPR (3 ) = 'U0 SURFACE      M/S             '
      TEXTPR (4 ) = 'V0 SURFACE      M/S             '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'STILL WATER H   M               '
      TEXTPR (8 ) = 'PHASE VELOCITY  M/S             '
      TEXTPR (9 ) = 'GROUP VELOCITY  M/S             '
      TEXTPR (10) = 'WAVE NUMBER     1/M             '
      TEXTPR (11) = 'REAL POTENTIAL  M2/S            '
      TEXTPR (12) = 'IMAG POTENTIAL  M2/S            '
      TEXTPR (13) = 'PRIVATE 1       UNIT   ??       '
      TEXTPR (14) = 'PRIVATE 2       UNIT   ??       '
      TEXTPR (15) = 'PRIVATE 3       UNIT   ??       '
      TEXTPR (16) = 'PRIVATE 4       UNIT   ??       '
      TEXTPR (17) = 'T01             S               '
      TEXTPR (18) = 'T02             S               '
      TEXTPR (19) = 'TM              S               '
      TEXTPR (20) = 'FORCE_FX        M/S2            '
      TEXTPR (21) = 'FORCE_FY        M/S2            '
      TEXTPR (22) = 'WAVE INCIDENCE  DEG             '
      TEXTPR (23) = 'QB                              '
      TEXTPR (24) = 'STRESS_SXX      M3/S2           '
      TEXTPR (25) = 'STRESS_SXY      M3/S2           '
      TEXTPR (26) = 'STRESS_SYY      M3/S2           '
!
!-----------------------------------------------------------------------
!
!  FRENCH OR OTHER
!
      ELSE
!
      TEXTE (1 ) = 'HAUTEUR HOULE   M               '
      TEXTE (2 ) = 'PHASE HOULE     RAD             '
      TEXTE (3 ) = 'U0 SURFACE      M/S             '
      TEXTE (4 ) = 'V0 SURFACE      M/S             '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'H EAU REPOS     M               '
      TEXTE (8 ) = 'VITESSE PHASE   M/S             '
      TEXTE (9 ) = 'VITESSE GROUPE  M/S             '
      TEXTE (10) = 'NOMBRE ONDE     1/M             '
      TEXTE (11) = 'POTENTIEL REEL  M2/S            '
      TEXTE (12) = 'POTENTIEL IMAG  M2/S            '
      TEXTE (13) = 'PRIVE 1         UNITES ??       '
      TEXTE (14) = 'PRIVE 2         UNITES ??       '
      TEXTE (15) = 'PRIVE 3         UNITES ??       '
      TEXTE (16) = 'PRIVE 4         UNITES ??       '
      TEXTE (17) = 'T01             S               '
      TEXTE (18) = 'T02             S               '
      TEXTE (19) = 'TM              S               '
      TEXTE (20) = 'FORCE_FX        M/S2            '
      TEXTE (21) = 'FORCE_FY        M/S2            '
      TEXTE (22) = 'INCIDENCE HOULE DEG             '
      TEXTE (23) = 'QB                              '
      TEXTE (24) = 'CONTRAINTE_SXX  M3/S2           '
      TEXTE (25) = 'CONTRAINTE_SXY  M3/S2           '
      TEXTE (26) = 'CONTRAINTE_SYY  M3/S2           '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
      TEXTPR (1 ) = 'HAUTEUR HOULE   M               '
      TEXTPR (2 ) = 'PHASE HOULE     RAD             '
      TEXTPR (3 ) = 'U0 SURFACE      M/S             '
      TEXTPR (4 ) = 'V0 SURFACE      M/S             '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'H EAU REPOS     M               '
      TEXTPR (8 ) = 'VITESSE PHASE   M/S             '
      TEXTPR (9 ) = 'VITESSE GROUPE  M/S             '
      TEXTPR (10) = 'NOMBRE ONDE     1/M             '
      TEXTPR (11) = 'POTENTIEL REEL  M2/S            '
      TEXTPR (12) = 'POTENTIEL IMAG  M2/S            '
      TEXTPR (13) = 'PRIVE 1         UNITES ??       '
      TEXTPR (14) = 'PRIVE 2         UNITES ??       '
      TEXTPR (15) = 'PRIVE 3         UNITES ??       '
      TEXTPR (16) = 'PRIVE 4         UNITES ??       '
      TEXTPR (17) = 'T01             S               '
      TEXTPR (18) = 'T02             S               '
      TEXTPR (19) = 'TM              S               '
      TEXTPR (20) = 'FORCE_FX        M/S2            '
      TEXTPR (21) = 'FORCE_FY        M/S2            '
      TEXTPR (22) = 'INCIDENCE HOULE DEG             '
      TEXTPR (23) = 'QB                              '
      TEXTPR (24) = 'CONTRAINTE_SXX  M3/S2           '
      TEXTPR (25) = 'CONTRAINTE_SXY  M3/S2           '
      TEXTPR (26) = 'CONTRAINTE_SYY  M3/S2           '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIASES FOR THE VARIABLES IN THE STEERING FILE
!
!     USES THE OLD : LETVAR/&#039 DATED; APUVSBHCGKIJDEFOLMNWXT???? ' /
!
      MNEMO(1)    = 'HS      '
      MNEMO(2)    = 'PHAS    '
      MNEMO(3)    = 'U0      '
      MNEMO(4)    = 'V0      '
      MNEMO(5)    = 'ZS      '
      MNEMO(6)    = 'ZF      '
      MNEMO(7)    = 'HW      '
      MNEMO(8)    = 'C       '
      MNEMO(9)    = 'CG      '
      MNEMO(10)   = 'K       '
      MNEMO(11)   = 'PHIR    '
      MNEMO(12)   = 'PHII    '
      MNEMO(13)   = 'D       '
      MNEMO(14)   = 'E       '
      MNEMO(15)   = 'F       '
      MNEMO(16)   = 'G       '
      MNEMO(17)   = 'T01     '
      MNEMO(18)   = 'T02     '
      MNEMO(19)   = 'TM      '
      MNEMO(20)   = 'FX      '
      MNEMO(21)   = 'FY      '
      MNEMO(22)   = 'INC     '
      MNEMO(23)   = 'QB      '
      MNEMO(24)   = 'SXX     '
      MNEMO(25)   = 'SXY     '
      MNEMO(26)   = 'SYY     '
      NVAR_ART = 26
!
!-----------------------------------------------------------------------
!
      RETURN
      END
