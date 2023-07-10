!                       *****************
                        SUBROUTINE PRE2DH
!                       *****************
!
     &(X,Y,IKLES,IPOBO,NPOIN2,NELEM2,NC2DH,NCOU,TITCAS,
     & FFORMAT,NVA3,TEXTLU,X_ORIG,Y_ORIG)
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : PREPARATION DES FICHIERS DES COUPES HORIZONTALES
!                      CONVENTIONS DU LOGICIEL SELAFIN.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   X,Y          ! -->! COORDONNEES DU MAILLAGE CURVILIGNE           !
! !   IKLES        ! -->! TABLE DE CONNECTIVITE                        !
! !   IPOBO        ! -->! INDICATEUR DE LA NATURE DES POINTS           !
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NELEM2       ! -->! NOMBRE D'ELEMENTS DU MAILLAGE 2D             !
! !   NC2DH        ! -->! NOMBRE DE COUPES HORIZONTALES                !
! !   NCOU         ! -->! NUMERO DE CANAL - 1 DE LA PREMIERE COUPE     !
! !   TITCAS       ! -->! TITRE A PORTER SUR CHAQUE COUPE              !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : POSTEL3D
! SOUS-PROGRAMME APPELES : ECRDEB , ECRI2
!
!***********************************************************************
!
!     - DOCUMENTATION : NOTICE SELAFIN
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!***********************************************************************
!
      USE BIEF
      USE INTERFACE_HERMES
      USE INTERFACE_POSTEL3D, EX_PRE2DH => PRE2DH
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) ::NPOIN2,NELEM2,NC2DH
      INTEGER, INTENT(INOUT) :: NCOU(NC2DH)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN2),Y(NPOIN2)
      CHARACTER(LEN=72), INTENT(IN) :: TITCAS
      INTEGER , INTENT(INOUT) :: IKLES(3,NELEM2),IPOBO(NPOIN2)
      CHARACTER(LEN=32), INTENT(IN) :: TEXTLU(100)
      CHARACTER(LEN=8),INTENT(INOUT) :: FFORMAT
      INTEGER,INTENT(IN) :: NVA3
      INTEGER, INTENT(IN) :: X_ORIG,Y_ORIG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER IC,I
      INTEGER N,IELEM
!
      INTEGER, ALLOCATABLE :: IKLE(:)
!
      INTEGER :: DATE(3), TIME(3), IERR
!-----------------------------------------------------------------------
      N=0
!
!
!-----------------------------------------------------------------------
!
!  POUR CHAQUE COUPE HORIZONTALE FAIRE :
!
      DO IC = 1,NC2DH
!
!
!     OUVERTURE DU FICHIER + ENREGISTREMENT DES PREMIERS PARAMETRES
!     -------------------------------------------------------------
!
        CALL ECRDEB(NCOU(IC),FFORMAT,TITCAS,NVA3,.TRUE.,
     &               TEXTLU,IC,N)
!
!     ENREGISTREMENT DES AUTRES PARAMETRES DE L'ENTETE
!     ------------------------------------------------
!
        DATE = (/0,0,0/)
        TIME = (/0,0,0/)
        ALLOCATE(IKLE(NELEM2*3),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:TMP0')
        DO I = 1,3
          DO IELEM = 1,NELEM2
            IKLE((I-1)*NELEM2 + IELEM) = IKLES(I,IELEM)
          ENDDO
        ENDDO
        CALL SET_MESH(FFORMAT,NCOU(IC),2,TRIANGLE_ELT_TYPE,3,0,0,
     &                NELEM2,NPOIN2,IKLE,IPOBO,IPOBO,X,Y,0,
     &                DATE,TIME,X_ORIG,Y_ORIG,IERR)
        CALL CHECK_CALL(IERR,'PRED2H:SET_MESH')
        DEALLOCATE(IKLE)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
