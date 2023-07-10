!                   *****************
                    SUBROUTINE CORFON
!                   *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!history  J-M HERVOUET (LNHE)
!+        01/03/1990
!+        V5P2
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
      INTEGER I,J
      DOUBLE PRECISION ZM
!     REMARQUE JMH : 5023 POINTS DANS LE FICHIER MAIS NPOIN = 5007 ?????
      DOUBLE PRECISION ZZM(5023)
      LOGICAL MAS
      INTEGER ID
!
!-----------------------------------------------------------------------
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(LISFON.EQ.0) THEN
        WRITE(LU,*)
        WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
        WRITE(LU,*)
      ELSE
        WRITE(LU,*)
        WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CALCUL DU DEMI-MARNAGE EN CHAQUE POINT DU MAILLAGE
!
      ID = T2D_FILES(T2DFO2)%LU
      REWIND ID
!
!  LECTURE DES DEMI-MARNAGES DONNES PAR LE FICHIER
!  INITIALISATIONS DES TABLEAUX ET DES VARIABLES
!  CALCUL DE LA COTE DU FOND AU POINT I
!
      IF(NCSIZE.GT.1) THEN
!       NOMBRE DE POINTS DU MAILLAGE NON DECOUPE (5023 DANS FICHIER)
        DO I=1,5023
          READ(ID,*) J,ZZM(I)
          IF(I.NE.J) STOP 'PROBLEME DANS FICHIER 27'
        ENDDO
        DO I=1,NPOIN
          ZF%R(I)=ZF%R(I)-ZZM(MESH%KNOLG%I(I))*12.D0/7.D0
        ENDDO
      ELSE
        DO I=1,NPOIN
          READ(ID,*) J,ZM
          ZF%R(I)=ZF%R(I)-ZM*12.D0/7.D0
        ENDDO
      ENDIF
!
! ON LISSE 5 FOIS LE FOND (PB DE PENTE DU TALUS)
!
      MAS = .TRUE.
      LISFON = 5
      CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
