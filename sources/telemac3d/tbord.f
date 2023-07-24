!                   ****************
                    SUBROUTINE TBORD
!                   ****************
!
     & (AUBORL,
     &  RUGOL,DISBOR,NELBOR,NULONE,IKLE,NELMAX2,
     &  U,V,W,NBOR,NPOIN2,NPLAN,NPTFR,
     &  DNUVIH,DNUVIV,KARMAN,LISRUL,KFROTL,
     &  UETCAL,NONHYD,UTANG,MESH2D)
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    COMPUTES U STAR AND AUBOR.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  V. BOYER UMIST
!+        26/06/2008
!+        V5P9
!+   PARALLELISM
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
!+        02/04/2015
!+        V7P1
!+   Avoiding divisions by 0 in the smooth regime section.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/09/2017
!+        V7P3
!+   Adding the argument NELMAX2 for dimensioning IKLE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AUBORL         |---|
!| DISBOR         |-->| DISTANCE AU BORD DES POINTS VOISINS DU BORD
!| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE
!| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE
!| IKLE           |---|
!| KARMAN         |-->| CONSTANTE DE KARMAN
!| KFROTL         |-->| LOI DE FROTTEMENT
!| LISRUL         |-->| REGIME DE TURBULENCE 1: LISSE
!|                |   | DES BORDS            2: RUGUEUX
!| MESH2D         |---|
!| NBOR           |-->| ADRESSES DES POINTS DE BORD
!| NELBOR         |---|
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN THE 2D MESH.
!| NONHYD         |---|
!| NPLAN          |-->| NOMBRE DE PLANS  DU MAILLAGE 3D
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
!| NULONE         |---|
!| RUGOL          |-->| RUGOSITY ON LATERAL BOUNDARIES
!| U              |-->| COMPOSANTES X DE LA VITESSE AU TEMPS N
!| UETCAL         |---|
!| UTANG          |---|
!| V              |-->| COMPOSANTES Y DE LA VITESSE AU TEMPS N
!| W              |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR,NPLAN,NPOIN2,NELMAX2
      INTEGER, INTENT(IN) :: LISRUL,KFROTL
!
      INTEGER, INTENT(IN) :: NBOR(NPTFR),NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX2,3)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: W(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: UETCAL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(INOUT) :: AUBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: UTANG(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: DISBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: RUGOL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV, KARMAN
!
      LOGICAL, INTENT(IN) :: NONHYD
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN2,ITER,I3,IELEM
!
      DOUBLE PRECISION UETUTA,DIST,PROPNU,VNORM
      DOUBLE PRECISION YPLUS
      DOUBLE PRECISION, PARAMETER :: TESTREICH = 1.D-4
      DOUBLE PRECISION UETREICH, TEST
      INTEGER, PARAMETER :: MAXITEREICH = 30
!
      INTRINSIC SQRT,MAX,LOG
!
!-----------------------------------------------------------------------
!
      PROPNU = (2*DNUVIH + DNUVIV) /3.D0
!
!=======================================================================
!
!                /* LOOP ON LATERAL BOUNDARIES */
!
!  COMPUTES THE FRICTION VELOCITY ON THE WALL, COMPUTES AUBOR
!
!=======================================================================
!
!
!        COMPUTES UETOIL FOR THE SOLID BOUNDARIES
!        ----------------------------------------
!
!
!        SMOOTH FRICTION TURBULENCE REGIME
!
!
!     ********************
      IF(LISRUL.EQ.1) THEN
!     ********************
!
!               3
!               **         IT IS ASSUMED HERE THAT POINT 3 IS IN
!              *  *        THE LOGARITHMIC LAYER. DISBOR IS THE DISTANCE
!             *    *       BETWEEN 3 AND SEGMENT 1-2
!            *      *
!           *        *
!          *          *
!       1 *            * 2
!  ************************************
!
!
      DO IPLAN=1,NPLAN
        DO IPTFR=1,NPTFR
!         TANGENTIAL VELOCITY AT POINT 3 (IT IS ASSUMED THAT THE POINT IS
!         CLOSE ENOUGH TO THE BOUNDARY SO THAT THE VELOCITY IS TANGENTIAL)
          DIST = DISBOR(IPTFR)
          IELEM=NELBOR(IPTFR)
          IF(IELEM.GT.0) THEN
!           UTANG IS THE TANGENTIAL VELOCITY INSIDE THE DOMAIN,
!           ASSUMED TO BE CLOSE ENOUGH TO THE BOUNDARY THAT IT CAN BE
!           CONSIDERED TANGENTIAL
            I3 = IKLE(IELEM,MOD(NULONE(IPTFR)+2,3)+1)
            UTANG(IPTFR)=SQRT( U(I3,IPLAN)**2
     &                        +V(I3,IPLAN)**2
     &                        +W(I3,IPLAN)**2 )
          ELSE
            UTANG(IPTFR)=0.D0
          ENDIF
        ENDDO
!       FINDS THE TRUE VALUE OF UTANG IF IN ANOTHER
!       SUBDOMAIN
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM_BORD(UTANG,3,MESH2D)
        ENDIF
!
        DO IPTFR=1,NPTFR
!         INITIAL GUESS
          UETUTA= 6.D-2
          UETREICH= 6.D-2
          DO ITER=1,MAXITEREICH
            YPLUS = DIST*UETUTA*UTANG(IPTFR)/PROPNU
            IF(YPLUS.GT.1.D-10) THEN
              UETUTA = 1.D0/(LOG(1.D0+KARMAN*YPLUS)/KARMAN + 7.8D0*
     &                 (1.D0-EXP(-YPLUS/11.D0) - YPLUS/11.D0
     &                                         *EXP(-0.33D0* YPLUS)))
            ELSE
              UETUTA = 0.D0
              GO TO 44
            ENDIF
            TEST = ABS(UETUTA-UETREICH)/UETREICH
            IF(TEST.LT.TESTREICH) THEN
              GOTO 44
            ELSE
              UETREICH = UETUTA
            ENDIF
          ENDDO
44        CONTINUE
          UETCAL(IPTFR,IPLAN) = (UETUTA*UTANG(IPTFR))**2
        ENDDO
!
      ENDDO
!
!     ROUGH FRICTION TURBULENCE REGIME
!
!     ************************
      ELSEIF(LISRUL.EQ.2) THEN
!     ************************
!
        IF(KFROTL.EQ.0) THEN
!
!         NO FRICTION NO STRESS
!
          DO IPTFR=1,NPTFR
            DO IPLAN=1,NPLAN
              UETCAL(IPTFR,IPLAN) = 0.D0
            ENDDO
          ENDDO
!
        ELSEIF(KFROTL.EQ.5) THEN
!
        DO IPLAN=1,NPLAN
          DO IPTFR=1,NPTFR
            DIST = DISBOR(IPTFR)
            IELEM=NELBOR(IPTFR)
            IF(IELEM.GT.0) THEN
              I3 = IKLE(IELEM,MOD(NULONE(IPTFR)+2,3)+1)
              UTANG(IPTFR)=SQRT( U(I3,IPLAN)**2
     &                          +V(I3,IPLAN)**2
     &                          +W(I3,IPLAN)**2 )
            ELSE
              UTANG(IPTFR)=0.D0
            ENDIF
          ENDDO
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM_BORD(UTANG,3,MESH2D)
          ENDIF
          DO IPTFR=1,NPTFR
!           NIKURADSE LAW
            UETUTA=1.D0/(8.5D0+LOG(DIST/RUGOL(IPTFR,IPLAN))/KARMAN)
            UETCAL(IPTFR,IPLAN) = (UETUTA*UTANG(IPTFR))**2
          ENDDO
        ENDDO
!
        ELSE
!
          WRITE(LU,202) KFROTL
202       FORMAT('TBORD: UNKNOWN FRICTION ON BOUNDARIES : ',I6)
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!     ****
      ELSE
!     ****
!
        WRITE(LU,102) LISRUL
102     FORMAT('TBORD: UNKNOWN TURBULENCE MODEL : ',I6)
        CALL PLANTE(1)
        STOP
!
!     *****
      ENDIF
!     *****
!
!-----------------------------------------------------------------------
!
!     COMPUTES AUBORL, AVBORL AND AWBORL
!
!     USING       : NUT * DU/DN = UETOIL**2 = -AUBOR*U(N+1)
!     TURNED INTO : NUT * DU/DN = UETOIL**2  *  U(N+1) / U(N)
!                               = UETOIL * (UETOIL/UTANG) * U(N+1)
!                               = UETOIL *  UETUTA        * U(N+1)
!
!
      IF(KFROTL.NE.0) THEN
!
      IF(NONHYD) THEN
!
        DO IPTFR=1,NPTFR
          IPOIN2 = NBOR(IPTFR)
          DO IPLAN=1,NPLAN
            VNORM=SQRT(  U(IPOIN2,IPLAN)**2
     &                  +V(IPOIN2,IPLAN)**2
     &                  +W(IPOIN2,IPLAN)**2 )
            AUBORL(IPTFR,IPLAN) = - UETCAL(IPTFR,IPLAN)/MAX(1.D-4,VNORM)
          ENDDO
        ENDDO
!
      ELSE
!
        DO IPTFR=1,NPTFR
          IPOIN2 = NBOR(IPTFR)
          DO IPLAN=1,NPLAN
            VNORM=SQRT( U(IPOIN2,IPLAN)**2+V(IPOIN2,IPLAN)**2)
            AUBORL(IPTFR,IPLAN) = - UETCAL(IPTFR,IPLAN)/MAX(1.D-4,VNORM)
          ENDDO
        ENDDO
!
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
