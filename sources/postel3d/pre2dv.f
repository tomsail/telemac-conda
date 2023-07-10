!                       *****************
                        SUBROUTINE PRE2DV
!                       *****************
!
     &(X,Y,SHP,NSEG,IMSEG,X2DV,Y2DV,IKLES,ELEM,
     & NPOIN2,NELEM2,IM,NC2DV)
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : PREPARATION DES FICHIERS DES COUPES VERTICALES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   X,Y          ! -->! COORDONNEES DU MAILLAGE CURVILIGNE           !
! !   SHP          !<-- ! COORDONNEES BARYCENTRIQUES DES PTS DE COUPE  !
! !   NSEG         ! -->! NOMBRE DE SEGMENTS CONSTITUANT CHAQUE COUPE  !
! !   IMSEG        ! -->! NOMBRE DE POINTS PAR SEGMENTS                !
! !   X2DV         ! -->! ABSCISSES DES SOMMETS DES COUPES VERTICALES  !
! !   Y2DV         ! -->! ORDONNEES DES SOMMETS DES COUPES VERTICALES  !
! !   IKLES        ! -->! TABLE DE CONNECTIVITE                        !
! !   ELEM         !<-- ! NUMERO DES ELEMENTS CONTENANT LES PTS DE COUPE
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NELEM2       ! -->! NOMBRE D'ELEMENTS DU MAILLAGE 2D             !
! !   IM (LU)      ! -->! NOMBRE DE PTS DE COUPE SUIVANT L'HORIZONTALE !
! !   NC2DV        ! -->! NOMBRE DE COUPES VERTICALES                  !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : POSTEL
!
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER, INTENT(IN) :: NPOIN2,NELEM2,IM,NC2DV
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION , INTENT(INOUT) :: SHP(IM,3,NC2DV)
      DOUBLE PRECISION, INTENT(IN) :: X2DV(50,NC2DV),Y2DV(50,NC2DV)
      INTEGER, INTENT(INOUT) :: IKLES(3,NELEM2)
      INTEGER, INTENT(INOUT) :: ELEM(IM,NC2DV)
      INTEGER, INTENT(INOUT) :: NSEG(NC2DV),IMSEG(49,NC2DV)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: XM,YM,A1,A2,A3,SURDET,LGTOT
      INTEGER IMTOT,IMMAX,NSEGMA,ISEG,IDSEG,IFSEG
      INTEGER IC,N1,N2,N3,I,N
      DOUBLE PRECISION :: LGSEG(49)
!
      LOGICAL FLAG
!
!***********************************************************************
!
! PARAMETRES IDENTIQUES A TOUS LES PAS DE TEMPS
!
      DO IC = 1,NC2DV
!
        LGTOT = 0.D0
        DO I = 1,NSEG(IC)
          LGSEG(I) = SQRT((X2DV(I+1,IC)-X2DV(I,IC))**2
     &                   +(Y2DV(I+1,IC)-Y2DV(I,IC))**2)
          LGTOT = LGTOT + LGSEG(I)
        ENDDO
        LGTOT = MAX(LGTOT,1D-6)
!
        IMTOT = 0
        IMMAX = 0
        NSEGMA = 1
        DO I = 1,NSEG(IC)
          IMSEG(I,IC) = MAX(NINT(LGSEG(I)*FLOAT(IM-1)/LGTOT),1)
          IMTOT = IMTOT + IMSEG(I,IC)
          IF (IMSEG(I,IC).GT.IMMAX) THEN
            IMMAX = IMSEG(I,IC)
            NSEGMA = I
          ENDIF
        ENDDO
        IMSEG(NSEGMA,IC) = IMMAX + IM-1 - IMTOT
!
        FLAG = .TRUE.
        ISEG = 0
        IFSEG = 1
!
        DO I = 1,IM
!
          IF (I.GT.IFSEG.OR.I.EQ.1) THEN
            ISEG = ISEG + 1
            IDSEG = IFSEG
            IFSEG = IFSEG + IMSEG(ISEG,IC)
          ENDIF
!
          XM = ((IFSEG-I)*X2DV(ISEG,IC) + (I-IDSEG)*X2DV(ISEG+1,IC))
     &       / FLOAT(IFSEG-IDSEG)
          YM = ((IFSEG-I)*Y2DV(ISEG,IC) + (I-IDSEG)*Y2DV(ISEG+1,IC))
     &       / FLOAT(IFSEG-IDSEG)
!
          ELEM(I,IC) = 1
          SHP(I,1,IC) = 1.
          SHP(I,2,IC) = 0.
          SHP(I,3,IC) = 0.
!
          DO N = 1,NELEM2
            N1 = IKLES(1,N)
            N2 = IKLES(2,N)
            N3 = IKLES(3,N)
            A1 = (XM-X(N3))*(Y(N2)-Y(N3)) - (YM-Y(N3))*(X(N2)-X(N3))
            A2 = (XM-X(N1))*(Y(N3)-Y(N1)) - (YM-Y(N1))*(X(N3)-X(N1))
            A3 = (XM-X(N2))*(Y(N1)-Y(N2)) - (YM-Y(N2))*(X(N1)-X(N2))
            IF (A1.GE.0..AND.A2.GE.0..AND.A3.GE.0.) THEN
              FLAG = .FALSE.
              SURDET = 1. / ((X(N2)-X(N1))*(Y(N3)-Y(N1)) -
     &                       (Y(N2)-Y(N1))*(X(N3)-X(N1)))
              ELEM(I,IC) = N
              SHP(I,1,IC) = A1 * SURDET
              SHP(I,2,IC) = A2 * SURDET
              SHP(I,3,IC) = A3 * SURDET
            ENDIF
          ENDDO !N
!
        ENDDO !N
!
        IF (FLAG) THEN
          WRITE(LU,102) IC
        ENDIF
!
      ENDDO !IC
!
!-----------------------------------------------------------------------
!
102   FORMAT('ATTENTION, YOUR VERTICAL CROSS SECTION NUMBER',I2,/,
     &       'HAS NO INTERSECTION WITH THE COMPUTATIONAL DOMAIN')
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
