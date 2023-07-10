!                       *****************
                        SUBROUTINE FASPDA
!                       *****************
!
     &(X,Y,VARINT,NPOIN,NPTFR,NBOR,XRELV,YRELV,VRELV,NP)
!
!***********************************************************************
!
! BIEF VERSION 4.0              17/08/94  J-C GALLAND   01 30 87 78 13
!                                         J-M HERVOUET  01 30 87 80 18
! MODIFIE POUR ARTEMIS 5.0      22/08/00  D AELBRECHT   01 30 87 74 12
!
!***********************************************************************
!
!   FONCTION : INTERPOLATION D'UNE VARIABLE SUR LES POINTS DU MAILLAGE A
!              PARTIR DE POINTS RELEVES D'UN AUTRE MAILLAGE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    X,Y         | -->|  COORDONNEES DU MAILLAGE
! |    VARINT      | <--|  VARIABLE INTERPOLEE EN X,Y
! |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE.
! |    XRELV       | -->|  ABCISSES DES POINTS RELEVES
! |    YRELV       | -->|  ORDONNEES DES POINTS RELEVES
! |    VRELV       | -->|  VALEURS DE LA VARIABLE AUX POINTS RELEVES
! |    NP          | -->|  NOMBRE DE POINTS RELEVES
! |    NBOR        | -->|  NUMEROTATION GLOBALE DES POINTS DE BORD
! |    NPTFR       | -->|  NOMBRE DE POINTS DE BORD.
! |    DM          | -->|  DISTANCE MINIMUM A LA COTE TOLEREE POUR
! |                |    |  ACCEPTER UN POINT RELEVE.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE: CROSFR
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER NP,N,NPOIN,NPTFR,INUM,I,IFR
      INTEGER, INTENT(IN) ::  NBOR(NPTFR)
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),XRELV(NP),YRELV(NP),VRELV(NP)
      DOUBLE PRECISION DIST1,DIST2,DIST3,DIST4
      DOUBLE PRECISION ZCADR1,ZCADR2,ZCADR3,ZCADR4
      DOUBLE PRECISION DIFX,DIFY,DIST
!     DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,X4,Y4
      DOUBLE PRECISION ZNUM,ZDEN,VARINT(NPOIN)
!
      LOGICAL OK1,OK2,OK3,OK4
!
!-----------------------------------------------------------------------
!
!  BOUCLE SUR LES POINTS DU MAILLAGE :
!
      DO IFR = 1 , NPTFR
!
      I = NBOR(IFR)
!
!     FOND INTERPOLE A PARTIR DE 4 QUADRANTS
!
! ---->  INITIALISATIONS:
!
      DIST1=1.D12
      DIST2=1.D12
      DIST3=1.D12
      DIST4=1.D12
!
      OK1 = .FALSE.
      OK2 = .FALSE.
      OK3 = .FALSE.
      OK4 = .FALSE.
!
      ZCADR1=0.D0
      ZCADR2=0.D0
      ZCADR3=0.D0
      ZCADR4=0.D0
!
! --------->  BOUCLE SUR LES POINTS RELEVES (IL Y EN A NP):
      DO N=1,NP
        DIFX = XRELV(N)-X(I)
        DIFY = YRELV(N)-Y(I)
        DIST = DIFX*DIFX + DIFY*DIFY
!
        IF ( DIST.LT.1.D-6 ) DIST=1.D-6
!         ->QUADRANT 1 :
          IF( DIFX.LE.0.D0.AND.DIFY.LE.0.D0) THEN
            IF(DIST.LE.DIST1)THEN
!             X1=XRELV(N)
!             Y1=YRELV(N)
              DIST1=DIST
              ZCADR1=VRELV(N)
              OK1 = .TRUE.
            ENDIF
!         ->QUADRANT 2 :
          ELSE IF( DIFX.GE.0.D0.AND.DIFY.LE.0.D0) THEN
            IF(DIST.LE.DIST2)THEN
!             X2=XRELV(N)
!             Y2=YRELV(N)
              DIST2=DIST
              ZCADR2=VRELV(N)
              OK2 = .TRUE.
            ENDIF
!         ->QUADRANT 3 :
          ELSE IF( DIFX.GE.0.D0.AND.DIFY.GE.0.D0) THEN
            IF(DIST.LE.DIST3)THEN
!             X3=XRELV(N)
!             Y3=YRELV(N)
              DIST3=DIST
              ZCADR3=VRELV(N)
              OK3 = .TRUE.
            ENDIF
!         ->QUADRANT 4 :
          ELSE IF( DIFX.LE.0.D0.AND.DIFY.GE.0.D0) THEN
            IF(DIST.LE.DIST4)THEN
!             X4=XRELV(N)
!             Y4=YRELV(N)
              DIST4=DIST
              ZCADR4=VRELV(N)
              OK4 = .TRUE.
            ENDIF
          ENDIF
      ENDDO
!
! --------->  FIN DE LA BOUCLE SUR LES POINTS RELEVES.
!
      ZNUM = 0.D0
      ZDEN = 0.D0
      INUM = 0
      IF(OK1) THEN
        ZNUM = ZNUM + ZCADR1/DIST1
        ZDEN = ZDEN + 1.D0/DIST1
        INUM = INUM + 1
      ENDIF
      IF(OK2) THEN
        ZNUM = ZNUM + ZCADR2/DIST2
        ZDEN = ZDEN + 1.D0/DIST2
        INUM = INUM + 1
      ENDIF
      IF(OK3) THEN
        ZNUM = ZNUM + ZCADR3/DIST3
        ZDEN = ZDEN + 1.D0/DIST3
        INUM = INUM + 1
      ENDIF
      IF(OK4) THEN
        ZNUM = ZNUM + ZCADR4/DIST4
        ZDEN = ZDEN + 1.D0/DIST4
        INUM = INUM + 1
      ENDIF
!
      IF(INUM.NE.0) THEN
!       VARINT : VARIABLE AU POINT
        VARINT(I)=ZNUM/ZDEN
      ELSE
        WRITE(*,*) 'INUM = ', INUM
        WRITE(*,*) 'PAS DE POINT TROUVE POUR INTERPOLER '
        WRITE(*,*) 'IGLB = ', I
        VARINT(I) = 0.D0
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

