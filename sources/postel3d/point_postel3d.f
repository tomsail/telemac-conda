!                       *************************
                        SUBROUTINE POINT_POSTEL3D
!                       *************************
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_POSTEL3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!***********************************************************************
!
      INTEGER CFG(2),NGEO
      NGEO=POS_FILES(POSGEO)%LU
!
!-----------------------------------------------------------------------
!
      WRITE(LU,21)
 21   FORMAT(1X,/,1X,'POINT_TELEMAC3D: MEMORY ALLOCATION',/)
!
!-----------------------------------------------------------------------
! discretisation types are declared here
!
      IELM0 = 10*(IELMH/10) ! FOR TELEMAC2D
      IELM1 = IELM0 + 1     ! FOR TELEMAC2D
!
! Telemac3D discretisation types: 3D, 2D horizontal boundary,
! 2D vertical boundary
!
      IELM3  = 41     ! TELEMAC3D PRISMS
      IELM2H = 11     ! PRISM TRIANGULAR BOTTOM AND SURFACE
      IELM2V = 21     ! PRISM QUADRILATERAL LATERAL BOUNDARIES
!
      IELMX=MAX(IELMU,IELM2H,IELMH) ! IT WILL BE MAX. DISCR. IN 2D
!
      CFG(1) = 1
      CFG(2) = 1
!
!=======================================================================
!
!                     *********************
!                     *  MESH - GEOMETRY  *
!                     *********************
!
! TWO meshes are allocated: (1) 2D base mesh, (2) 3D sigma-mesh
!
! allocation of the 2D mesh structure for Telemac2D
! discretisation ielmh given in lecdon
! ielmx = ielmu if quasi-bubble element required, otherwise ielmh
!
      EQUA = 'NO_EQUATION_IS_GIVEN'
!
      CALL ALMESH(MESH2D,'MESH2D',IELMX,SPHERI,CFG,
     &            POS_FILES(POSGEO)%FMT,NGEO,
     &            EQUA,0,NPLAN=1)
!
! aliases for certain components of the 2D mesh structure
!
      X2      => MESH2D%X
      Y2      => MESH2D%Y
      Z2      => MESH2D%Z
      XNEBOR2 => MESH2D%XNEBOR
      YNEBOR2 => MESH2D%YNEBOR
      XSGBOR2 => MESH2D%XSGBOR
      YSGBOR2 => MESH2D%YSGBOR
      IKLE2   => MESH2D%IKLE
      NBOR2   => MESH2D%NBOR   ! PREV. SIMPLY NBOR
!
!
      NELEM2  => MESH2D%NELEM
      NELMAX2 => MESH2D%NELMAX  ! PREVIOUSLY NELMA2 (ADAPTIVITY OUTLOOK)
      NPTFR2  => MESH2D%NPTFR   ! PREVIOUSLY SIMPLY NPTFR
      NPTFRX2 => MESH2D%NPTFRX
      DIM2    => MESH2D%DIM1
      TYPELM2 => MESH2D%TYPELM
      NPOIN2  => MESH2D%NPOIN
      NPMAX2  => MESH2D%NPMAX
      MXPTVS2 => MESH2D%MXPTVS
      MXELVS2 => MESH2D%MXELVS
!
!-----------------------------------------------------------------------
! ALLOCATION OF THE 3D MESH STRUCTURE (EQUA=EMPTY) (READ AGAIN?)
!
      EQUA = 'NO_EQUATION_IS_GIVEN'
!
      CALL ALMESH(MESH3D,'MESH3D',IELM3,SPHERI,CFG,
     &            POS_FILES(POSGEO)%FMT,NGEO,
     &            EQUA,0,NPLAN=NPLAN)
!
! alias for certain components of the 3D mesh structure
! they are defined in declarations
!
      X       => MESH3D%X%R  ! REAL VALUE!!!
      Y       => MESH3D%Y%R
      Z       => MESH3D%Z%R
      X3      => MESH3D%X    ! POINTERS
      Y3      => MESH3D%Y
      Z3      => MESH3D%Z
      XNEBOR3 => MESH3D%XNEBOR
      YNEBOR3 => MESH3D%YNEBOR
      ZNEBOR3 => MESH3D%ZNEBOR
      XSGBOR3 => MESH3D%XSGBOR
      YSGBOR3 => MESH3D%YSGBOR
      ZSGBOR3 => MESH3D%ZSGBOR
      IKLE3   => MESH3D%IKLE
      NBOR3   => MESH3D%NBOR
!
      NELEM3  => MESH3D%NELEM
      NELMAX3 => MESH3D%NELMAX   ! PREVIOUSLY NELMA3 (ADAPTIVITY?)
      NELEB   => MESH3D%NELEB
      NELEBX  => MESH3D%NELEBX
      NPTFR3  => MESH3D%NPTFR
      NPTFRX3 => MESH3D%NPTFRX
      DIM3    => MESH3D%DIM1
      TYPELM3 => MESH3D%TYPELM
      NPOIN3  => MESH3D%NPOIN
      NPMAX3  => MESH3D%NPMAX
      MXPTVS3 => MESH3D%MXPTVS
      MXELVS3 => MESH3D%MXELVS
!
!
      WRITE(LU,32)
     &           TYPELM2,NPOIN2,NELEM2,NPTFR2,TYPELM3,NPOIN3,NELEM3,
     &           NPLAN,NELEB,NPTFR3+2*NPOIN2,NPTFR3,NPOIN2,NPOIN2
!
 32   FORMAT(/,' 2D MESH',/,
     &         ' -------',//,
     &         ' 2D ELEMENT TYPE                : ',I8,/,
     &         ' NUMBER OF 2D NODES             : ',I8,/,
     &         ' NUMBER OF 2D ELEMENTS          : ',I8,/,
     &         ' NUMBER OF 2D BOUNDARY NODES    : ',I8,///,
     &         ' 3D MESH',/,
     &         ' -------',//,
     &         ' 3D ELEMENT TYPE                : ',I8,/,
     &         ' NUMBER OF 3D NODES             : ',I8,/,
     &         ' NUMBER OF 3D ELEMENTS          : ',I8,/,
     &         ' NUMBER OF LEVELS               : ',I8,/,
     &         ' NUMBER OF BOUNDARY ELEMENTS    : ',I8,/,
     &         ' TOTAL NUMBER OF BOUNDARY NODES : ',I8,/,
     &         ' INCLUDING   LATERAL BOUNDARIES : ',I8,/,
     &         '                        SURFACE : ',I8,/,
     &         '                         BOTTOM : ',I8,/)
!
!
! DEFINITION DES POINTEURS
!
      CALL BIEF_ALLVEC(1, U,      'U     ', IELM3,  1,1,MESH3D)
      CALL BIEF_ALLVEC(1, V,      'V     ', IELM3,  1,1,MESH3D)
      CALL BIEF_ALLVEC(1, W,      'W     ', IELM3,  1,1,MESH3D)
!
      CALL ALLBLO(TAB,'TAB   ')
      IF (NVA3.GT.4) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(TAB,NVA3-4,1,'TAB   ',IELM3,
     &                            1,1,MESH3D)
      ENDIF
!
!=======================================================================
!
! IMPRESSIONS :
!
      WRITE(LU,23)
23    FORMAT(1X,///,21X,'*************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION:    *',/,
     &21X,              '*************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
