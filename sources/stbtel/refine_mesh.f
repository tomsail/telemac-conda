!                       **********************
                        SUBROUTINE REFINE_MESH
!                       **********************
!
     &(RLEVELS,MESHINIT,NNELMAX,NPTFRMAX,NTRAC,EXTEND_LIM,
     & CORRESP,LIHBOR,LIUBOR,LIVBOR,LITBOR,HBOR,UBOR,VBOR,
     & CHBORD,TBOR,ATBOR,BTBOR,ZF)
!
!***********************************************************************
! STBTEL  V7P3
!***********************************************************************
!
!brief    Refines RLEVELS times a mesh by successive divisions of
!         all the triangle elements by four
!
!history  A. LEROY (EDF)
!+        24/03/2016
!+        V7P2
!+        Creation of the file.
!
!history  J.-M. HERVOUET (JUBILADO)
!+        27/10/2016
!+        V7P2
!+        Adapting to new optimised subroutine DIVISE.
!
!history  J.-M. HERVOUET (JUBILADO)
!+        27/09/2017
!+        V7P3
!+        Resetting here the new dimension of TTILD, TN, etc. is no
!+        longer useful. Idem for ZF%DIM1 and H%DIM1.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   CORRESP      |<-->| ARRAY OF CORRESPONDANCE BETWEEN THE ELEMENTS
! |                |    | OF THE COARSE MESH AND THOSE OF THE FINE MESH
! |                |    | FOR EACH REFINEMENT LEVEL
! |   MESHINIT     |<-->| INITIAL COARSE MESH TO BE REFINED
! |   RLEVELS      | -->| NUMBER OF REFINEMENT LEVELS: EACH DIVIDES BY
! |                |    | FOUR
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : HOMERE_TELEMAC2D
! APPEL DE   : DIVISE,VERIFI,VOISIN,RANBO
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_STBTEL
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_STBTEL, EX_REFINE_MESH => REFINE_MESH
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESHINIT
      INTEGER        , INTENT(IN)    :: RLEVELS
      INTEGER        , INTENT(IN)    :: NNELMAX
      INTEGER        , INTENT(IN)    :: NTRAC
      INTEGER        , INTENT(IN)    :: NPTFRMAX
      LOGICAL        , INTENT(IN)    :: EXTEND_LIM
      INTEGER, INTENT(INOUT), OPTIONAL:: CORRESP(NNELMAX,RLEVELS)
      INTEGER,INTENT(INOUT) ,OPTIONAL :: LIHBOR(NPTFRMAX)
      INTEGER,INTENT(INOUT) ,OPTIONAL :: LIUBOR(NPTFRMAX)
      INTEGER,INTENT(INOUT) ,OPTIONAL :: LIVBOR(NPTFRMAX)
      TYPE(BIEF_OBJ),INTENT(INOUT),OPTIONAL :: LITBOR
      DOUBLE PRECISION,INTENT(INOUT),OPTIONAL:: UBOR(NPTFRMAX,2)
      DOUBLE PRECISION,INTENT(INOUT),OPTIONAL:: VBOR(NPTFRMAX,2)
      DOUBLE PRECISION,INTENT(INOUT),OPTIONAL:: HBOR(NPTFRMAX)
      DOUBLE PRECISION,INTENT(INOUT),OPTIONAL:: CHBORD(NPTFRMAX)
      TYPE(BIEF_OBJ),INTENT(INOUT),OPTIONAL:: ZF
      TYPE(BIEF_OBJ),INTENT(INOUT),OPTIONAL:: TBOR,ATBOR,BTBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM,I,J,K,I1,I2,I3,IELEM,NPOINMAX,ERR
      INTEGER OLD_NPTFR
!     FAKE ARRAY (ONLY USED BY VOISIN IN PARALLELISM)
      INTEGER NACHB(1)
!
      DOUBLE PRECISION, ALLOCATABLE :: WORK(:),SHP(:,:)
      INTEGER, ALLOCATABLE :: TRAV1(:),TRAV2(:),TRAV3(:)
      INTEGER, ALLOCATABLE :: IKLE(:,:),IFABOR(:,:),TRAV4(:,:)
      INTEGER, ALLOCATABLE :: NBOR(:),KP1BOR(:),NCOLOR(:),NCOLFR(:)
      INTEGER, ALLOCATABLE :: OLD_NBOR(:)
!
!     TODO: CHECK THAT THE MESH IS COMPOSED OF TRIANGLES, OTHERWISE
!     THIS DOES NOT WORK
      MAX_SEG_PER_POINT = 11
!
!     NPOINMAX IS OVERDIMENSIONNED HERE
!     NNELMAX = NELEM*4**RLEVELS
      NPOINMAX  = 3*MESHINIT%NELMAX
      NSOM2 = 0
!
!     ALLOCATION OF REAL ARRAYS
      ALLOCATE(WORK(MESHINIT%NPOIN) ,STAT=ERR)
!     ALLOCATION DYNAMIQUE DES TABLEAUX ENTIERS
!     NPTFR REMPLACE PAR NPOINMAX (VALEUR PAR EXCES)
      ALLOCATE(TRAV1(4*MESHINIT%NELMAX)  ,STAT=ERR)
      ALLOCATE(TRAV2(4*MESHINIT%NELMAX)  ,STAT=ERR)
      ALLOCATE(TRAV3(NPOINMAX),STAT=ERR)
      ALLOCATE(TRAV4(NPOINMAX,2),STAT=ERR)
      ALLOCATE(IFABOR(MESHINIT%NELMAX,4) ,STAT=ERR)
      ALLOCATE(IKLE(MESHINIT%NELMAX,3) ,STAT=ERR)
      ALLOCATE(NBOR(NPOINMAX) ,STAT=ERR)
      ALLOCATE(KP1BOR(NPOINMAX),STAT=ERR)
      ALLOCATE(NCOLOR(NPOINMAX),STAT=ERR)
      ALLOCATE(NCOLFR(NPOINMAX),STAT=ERR)
      ALLOCATE(SHP(NPOINMAX,3),STAT=ERR)
      ALLOCATE(OLD_NBOR(NPOINMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'REFINE_MESH')
!
!=======================================================================
! DIVISION BY 4 OF THE WHOLE MESH
! THE COLUMN NUMBER LEVEL OF CORRESP IS FILLED
!=======================================================================
!
      DO J=1,MESHINIT%NELEM
        DO K=1,3
          IKLE(J,K) = MESHINIT%IKLE%I((K-1)*MESHINIT%NELMAX+J)
        ENDDO
      ENDDO
      DO J= 1,MESHINIT%NPOIN
        NCOLOR(J) = 0
      ENDDO
!
      DO I=1,RLEVELS
        WRITE(LU,*) 'CALL TO DIVISE NUMBER ',I
        CALL DIVISE(MESHINIT%X%R,MESHINIT%Y%R,
     &              IKLE,NCOLOR,MESHINIT%NPOIN,
     &              MESHINIT%NELEM,MESHINIT%NELMAX,NSOM2,
     &              TRAV1,TRAV2,SHP,TRAV3,NPOINMAX,
     &              CORR=CORRESP,LEVEL=I)
!       BUILDING XEL AND YEL OF NEW MESH
        DO IELEM=1,MESHINIT%NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          MESHINIT%XEL%R(IELEM                  )=MESHINIT%X%R(I1)
          MESHINIT%YEL%R(IELEM                  )=MESHINIT%Y%R(I1)
          MESHINIT%XEL%R(IELEM+  MESHINIT%NELMAX)=MESHINIT%X%R(I2)
          MESHINIT%YEL%R(IELEM+  MESHINIT%NELMAX)=MESHINIT%Y%R(I2)
          MESHINIT%XEL%R(IELEM+2*MESHINIT%NELMAX)=MESHINIT%X%R(I3)
          MESHINIT%YEL%R(IELEM+2*MESHINIT%NELMAX)=MESHINIT%Y%R(I3)
        ENDDO
      ENDDO
!
!=======================================================================
! EVERYTHING BELOW DOES NOT AFFECT CORRESP BECAUSE IT ONLY INVOLVES
! THE MESH POINTS, NOT THE ELEMENTS
!=======================================================================
!
!     BUILDING ARRAY IFABOR
!
      IELM = 11
!
!     HERE NBOR NOT INITIALISED, BUT USED ONLY IF NCSIZE.GT.1
!     THUS NOT A PROBLEM SINCE REFINEMENT PROCEDURE DOES NOT
!     WORK IN PARALLEL
      CALL VOISIN(IFABOR,MESHINIT%NELEM,MESHINIT%NELMAX,IELM,IKLE,
     &            MESHINIT%NELMAX,MESHINIT%NPOIN,NACHB,NBOR,
     &            MESHINIT%NPTFR,TRAV1,TRAV2)
!
!     NUMBERING BOUNDARY POINTS
!     COUNTERCLOCKWISE FOR CONTOUR, CLOCKWISE FOR ISLANDS
!
      OLD_NPTFR = MESHINIT%NPTFR
      CALL RANBO(NBOR,KP1BOR,IFABOR,IKLE,NCOLOR,TRAV4,MESHINIT%NPTFR,
     &           MESHINIT%X%R,MESHINIT%Y%R,NCOLFR,3,MESHINIT%NPOIN,
     &           MESHINIT%NELEM,MESHINIT%NELMAX,3)
      IF(EXTEND_LIM) THEN
        MESHINIT%NSEG = MESHINIT%NPOIN+MESHINIT%NELEM-1
        MESHINIT%NELEB= MESHINIT%NELEB*3
        CALL BIEF_ININDS(MESHINIT%NPOIN,MESHINIT%NPTFR,MESHINIT%NELEM,
     &                   MESHINIT%NPMAX,MESHINIT%NPTFRX,MESHINIT%NELMAX,
     &                   1,MESHINIT%NPTFR,MESHINIT%NDS,MESHINIT%NELEB)
        ! Dirty work around so that fill_lim is not reordering
        DO I=1,OLD_NPTFR
          OLD_NBOR(I) = NBOR(2*I-1)
        ENDDO
        CALL FILL_LIM(OLD_NPTFR,NPTFRMAX,NTRAC,LIHBOR,
     &                LIUBOR,LIVBOR,LITBOR,HBOR,UBOR,VBOR,
     &                CHBORD,TBOR,ATBOR,BTBOR,NBOR,OLD_NBOR,KP1BOR)
!       TODO: SHOULD BE GENERIC
!       COULD BE DONE ONCE AFTER THE LAST REFINEMENT, WITH FINEMESH%NPOIN
        DO I=1,MESHINIT%NPOIN
          ZF%R(I) = 0.D0
        ENDDO
        WRITE(LU,*) 'IN STBTEL REFINE_MESH', MESHINIT%NPOIN
      ENDIF
!
!     RENUMBERING NODES FOR OPTIMISING ASSEMBLY
!
      IF(OPTASS) THEN
        WRITE(LU,*) 'CALL RENUM'
        CALL RENUM(MESHINIT%X%R,MESHINIT%Y%R,WORK,IKLE,NBOR,
     &             TRAV1,TRAV2,TRAV3,NCOLOR,COLOR,MESHINIT%NPTFR)
      ENDIF
!
      WRITE(LU,*) 'COPY IKLE AND IFABOR'
      DO J=1,MESHINIT%NELEM
        DO K=1,3
          MESHINIT%IKLE%I((K-1)*MESHINIT%NELMAX+J) = IKLE(J,K)
          MESHINIT%IFABOR%I((K-1)*MESHINIT%NELMAX+J) = IFABOR(J,K)
        ENDDO
      ENDDO
      WRITE(LU,*) 'COPY NBOR AND KP1BOR'
      DO J=1,MESHINIT%NPTFR
        MESHINIT%NBOR%I(J) = NBOR(J)
        MESHINIT%KP1BOR%I(J) = KP1BOR(J)
      ENDDO
!
      DEALLOCATE(WORK)
      DEALLOCATE(TRAV1)
      DEALLOCATE(TRAV2)
      DEALLOCATE(TRAV3)
      DEALLOCATE(TRAV4)
      DEALLOCATE(IFABOR)
      DEALLOCATE(IKLE)
      DEALLOCATE(KP1BOR)
      DEALLOCATE(NCOLFR)
      DEALLOCATE(SHP)
      DEALLOCATE(OLD_NBOR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

