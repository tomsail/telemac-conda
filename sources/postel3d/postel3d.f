!                       *******************
                        SUBROUTINE POSTEL3D
!                       *******************
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!       PPPP    OOO    SSSS  TTTTT  EEEEE  L         3333   DDDD
!       P   P  O   O  S        T    E      L             3  D   D
!       PPPP   O   O   SSS     T    EEEE   L     ---  333   D   D
!       P      O   O      S    T    E      L             3  D   D
!       P       OOO   SSSS     T    EEEEE  LLLLL     3333   DDDD
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : HOMERE_POSTEL3D
! SOUS-PROGRAMME APPELES : LIT, PRE2DH, PRE2DV, LECR3D, COUPEH, COUPEV
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!-----------------------------------------------------------------------
!                    DECLARATION DES TYPES ET DIMENSIONS
!-----------------------------------------------------------------------
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_POSTEL3D
      USE INTERFACE_HERMES
      USE INTERFACE_POSTEL3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
! TABLEAUX DE REELS
!
      DOUBLE PRECISION SHP(IM,3,NC2DV)
!
! TABLEAUX D'ENTIERS
!
      INTEGER IKLES(3,NELEM2) , PLINF(NPOIN2)
      INTEGER IPOBO(NPOIN2)
      INTEGER ELEM(IM,NC2DV)
      INTEGER N,NPRE
!
! VARIABLES LOCALES
!
      INTEGER I, K ,  IMSEG(49,9)
      DOUBLE PRECISION AT
      INTEGER :: IENRE
!
! VARIABLES BIDON POUR LIT
!
      DOUBLE PRECISION, ALLOCATABLE :: VAR(:)
      DOUBLE PRECISION, ALLOCATABLE :: SHZ(:)
      INTEGER ERR, IERR
      CHARACTER(LEN=8) PRE_FMT, VER_FMT, HOR_FMT
      CHARACTER(LEN=16),ALLOCATABLE :: VAR_NAME(:), VAR_UNIT(:)
      INTEGER, ALLOCATABLE :: NHOR(:)
      INTEGER, ALLOCATABLE :: NVER(:)
!
!***********************************************************************
! allocate a (simple) REAL vector
!
      ALLOCATE(VAR(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'POSTEL3D:VAR')
!
      ALLOCATE(SHZ(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'POSTEL3D:SHZ')
!
!***********************************************************************
!
!     LECTURE DES DONNEES RELATIVES AU MAILLAGE
!     DANS LE FICHIER DE RESULTATS 3D
!
      NPRE = POS_FILES(POSPRE)%LU
      PRE_FMT = POS_FILES(POSPRE)%FMT
      ALLOCATE(NHOR(MAX(NC2DH,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"NHOR")
      NHOR(1) = POS_FILES(POSHOR)%LU
      HOR_FMT = POS_FILES(POSHOR)%FMT
      ALLOCATE(NVER(MAX(NC2DV,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"NVER")
      NVER(1) = POS_FILES(POSVER)%LU
      VER_FMT = POS_FILES(POSVER)%FMT
!
!
      CALL GET_DATA_NVAR(PRE_FMT,NPRE,NVA3,IERR)
      CALL CHECK_CALL(IERR, 'POSTEL3D:GET_DATA_NVAR')
!
!     LEC/ECR 3 : NOMS ET UNITES DES VARIABLES
!
      IF(NVA3.GE.1) THEN
        ALLOCATE(VAR_NAME(NVA3),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'POSTEL3D:VAR_NAME')
        ALLOCATE(VAR_UNIT(NVA3),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'POSTEL3D:VAR_UNIT')
        CALL GET_DATA_VAR_LIST(PRE_FMT,NPRE,NVA3,VAR_NAME,VAR_UNIT,IERR)
        CALL CHECK_CALL(IERR, 'POSTEL3D:GET_DATA_VAR_LIST')
        DO I=1,NVA3
          TEXTLU(I)(1:16) = VAR_NAME(I)
          TEXTLU(I)(17:32) = VAR_UNIT(I)
        ENDDO
        DEALLOCATE(VAR_NAME,VAR_UNIT)
      ENDIF
!
!
      CALL GET_MESH_COORD(PRE_FMT,NPRE,1,2,NPOIN3,X,IERR)
      CALL CHECK_CALL(IERR, 'POSTEL3D:GET_MESH_COORD:X')
      CALL GET_MESH_COORD(PRE_FMT,NPRE,2,2,NPOIN3,Y,IERR)
      CALL CHECK_CALL(IERR, 'POSTEL3D:GET_MESH_COORD:X')

      CALL GET_MESH_ORIG(PRE_FMT,NPRE,X_ORIG,Y_ORIG,IERR)
      CALL CHECK_CALL(IERR, 'POSTEL3D:GET_MESH_ORIG')
!
! *****************
! fin de l'en-tete
! *****************
!
! INVERSION DE IKLE3 EN IKLES
!
      DO K = 1,NELEM2
        IKLES(1,K) = IKLE2%I(K)
        IKLES(2,K) = IKLE2%I(K+NELEM2)
        IKLES(3,K) = IKLE2%I(K+2*NELEM2)
      ENDDO
      ! Cancelling the opening done in bief_open_file as multiple files will be reopen

      IF(NC2DH.GE.1) THEN
        CALL CLOSE_MESH(HOR_FMT,NHOR(1),IERR)
        CALL CHECK_CALL(IERR,'POSTEL3D:CLOSE_MESH')
      ENDIF
      IF(NC2DV.GE.1) THEN
        CALL CLOSE_MESH(VER_FMT,NVER(1),IERR)
        CALL CHECK_CALL(IERR,'POSTEL3D:CLOSE_MESH')
      ENDIF
!
! PREPARATION DES DONNEES POUR LES COUPES HORIZONTALES
!
      IF(NC2DH.GE.1) THEN
        DO K = 1,NPOIN2
          IPOBO(K) = 0
        ENDDO
        CALL PRE2DH(X,Y,IKLES,IPOBO,NPOIN2,NELEM2,NC2DH,NHOR,
     &     TITCAS,HOR_FMT,NVA3,TEXTLU,X_ORIG,Y_ORIG)
      ENDIF
!
! PREPARATION DES DONNEES POUR LES COUPES VERTICALES
!
      IF(NC2DV.GE.1) THEN
        CALL PRE2DV(X,Y,SHP,NSEG,IMSEG,X2DV,Y2DV,
     &   IKLES,ELEM,NPOIN2,NELEM2,IM,NC2DV)
      ENDIF
!
!-----------------------------------------------------------------------
!
! LECTURE ECRITURE DES RESULTATS RELATIFS AU IEME ENREGISTREMENT
!
!
      DO K = 1,NENRE
        IF (K.GE.NUPRSO.AND.MOD(K-NUPRSO,PESOGR).EQ.0) THEN
          IENRE = (K-NUPRSO)/PESOGR
!
! LA ON SAIT QUE CET ENREGISTREMENT EST A TRANSCRIRE
!
          CALL LECR3D(K-1,AT,Z,U%R,V%R,W%R,NPOIN3,NPOIN2,NPLAN,
     &                NPRE,PRE_FMT,NVA3,TAB)
!
          IF(NC2DH.GE.1) THEN
            CALL COUPEH (IENRE,AT,Z,U%R,V%R,W%R,
     &       HREF,NPLREF,PLINF,NC2DH,NPOIN2,NPLAN,NHOR,HOR_FMT,
     &       VAR,SHZ,NVA3,TAB,TEXTLU)
          ENDIF
!
          IF (NC2DV.GE.1) THEN
            DO N=1,NC2DV
              CALL COUPEV(AT,Z,U%R,V%R,W%R,SHP,
     &           IMSEG,X2DV,Y2DV,DISTOR,IKLES,ELEM,NC2DV,NPOIN2,
     &           NELEM2,NVER,VER_FMT,IM,JM,TITCAS,NVA3,TAB,TEXTLU,
     &           IENRE)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
!
      IF (NC2DH.GE.2) THEN
        DO I=2,NC2DH
          CALL CLOSE_MESH(HOR_FMT,NHOR(I),IERR)
          CALL CHECK_CALL(IERR,'POSTEL3D:CLOSE_MESH')
        ENDDO
      ENDIF
      POS_FILES(POSHOR)%LU = NHOR(1)
      ! Reopening the file for bief_close_mesh
      CALL OPEN_MESH(VER_FMT,'POSVER',POS_FILES(POSVER)%LU,
     &               'WRITE    ',IERR)
!
      DEALLOCATE(VAR)
      DEALLOCATE(SHZ)
      DEALLOCATE(NHOR)
      DEALLOCATE(NVER)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
