!                   *****************
                    SUBROUTINE PREDIV
!                   *****************
!
     & ( PD, UP, VP, WP, INFO , BC , OPT, DIRSUR, DIRBOT, DIRLAT)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    SOLVES THE PRESSURE POISSON EQUATION
!+               (FOR THE DYNAMIC PRESSURE):
!+
!+            CASE.
!code
!+                           ----->
!+    NABLA^2 ( PD ) = DIV ( U_DIFF )
!+         ----->
!+   DIV ( U_DIFF )  IS HERE THE COMPUTED SOURCE TERM
!+
!+ DIFF_MATRIX (PD, VISC=1) = MASS_MATRIX (DIVU)  (AND BC'S)
!+
!+ THE PHYSICAL DYNAMIC PRESSURE IS OBTAINED BY MULTIPLYING: PD*(RHO/DT)
!
!note     S0P AND S1P ARE NOT USED.
!
!history  JMH
!+        02/09/2002
!+
!+   Tidal flats
!
!history  J-M HERVOUET (LNHE)     ; JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!+        21/05/2010
!+        V6P0
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
!history  J-M HERVOUET (LNHE)
!+        07/12/2011
!+        V6P2
!+   Removing preconditioning 17 in case of 2 planes.
!
!history  J-M HERVOUET (LNHE)
!+        16/06/2015
!+        V7P1
!+   Changing %FAC%R into %IFAC%I.
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        28/08/2015
!+        V7P1
!+   Add the option OPTSOU to treat sources as a dirac (OPTSOU=2) or
!+   not (OPTSOU=1).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BC             |-->| LOGICAL, IF YES, BOUNDARY CONDITIONS
!|                |   | ARE TAKEN INTO ACCOUNT (SEE CALL FLUPRI...)
!|                |   | AND DIRICHLET VALUES ARE CONSIDERED
!|                |   | IF NO, DIRICHLET VALUES ARE SET TO 0
!|                |   | AND BOUNDARY CONDITIONS ARE DISCARDED
!| DIRBOT         |-->| LOGICAL FOR DIRICHLET VALUES ON THE BOTTOM
!| DIRLAT         |-->| LOGICAL FOR DIRICHLET VALUES ON LATERAL BOUNDARIES
!| DIRSUR         |-->| LOGICAL FOR DIRICHLET VALUES AT THE SURFACE
!| INFO           |-->| INFORMATION ASKED ON SOLVER ITERATIONS
!| OPT            |-->| OPTION 1: DIVERGENCE IN THE REAL MESH
!|                |   | 2: DIVERGENCE OBTAINED BY SUM OF FLUXES
!|                |   | 3: DIVERGENCE OBTAINED BY SUM OF FLUXES
!|                |   | AND LAPLACIAN IN THE TRANSFORMED MESH
!|                |   | THAT WILL ALLOW A SPLITTING BETWEEN
!|                |   | VERTICAL FLUXES AND FLUXES TANGENT
!|                |   | TO PLANES
!| PD             |<->| DYNAMIC PRESSURE
!| UP             |-->| INTERMEDIATE VELOCITY FIELD
!| VP             |-->| INTERMEDIATE VELOCITY FIELD
!| WP             |-->| INTERMEDIATE VELOCITY FIELD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: OPT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PD
      TYPE(BIEF_OBJ), INTENT(IN)      :: UP, VP, WP
      LOGICAL, INTENT(IN)             :: INFO,BC,DIRSUR,DIRBOT,DIRLAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN3,IPOIN2,IPTFR3,IPLAN,IS,IIS,SIZD,SIZX,ELMD
      CHARACTER(LEN=16) FORMUL
!
!=======================================================================
! RIGHT HAND SIDE VECTOR SEM3D = - DIV (INTERMEDIATE VELOCITY)
!=======================================================================
!
      IF(OPT.EQ.1) THEN
!                     2 FILTERING CRUSHED ELEMENTS
        FORMUL='GRADF 2         '
        CALL VECTOR
     &   (SEM3D, '=',FORMUL(1:15)//'X', IELM3,-1.D0,UP, SVIDE,SVIDE,
     &    SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
        CALL VECTOR
     &   (SEM3D, '+',FORMUL(1:15)//'Y', IELM3,-1.D0,VP, SVIDE,SVIDE,
     &    SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
        CALL VECTOR
     &   (SEM3D, '+',FORMUL(1:15)//'Z', IELM3,-1.D0,WP, SVIDE,SVIDE,
     &    SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
!
        IF(BC) THEN
          CALL FLUPRI(SEM3D%R,1.D0,UP%R,VP%R,WP%R,
     &                X,Y,Z,IKLE3%I,
     &                MESH3D%NELMAX,NELEM2,NPOIN2,NPOIN3,
     &                MESH2D%W%R(         1:  NELEM2),
     &                MESH2D%W%R(  NELEM2+1:2*NELEM2),
     &                MESH2D%W%R(2*NELEM2+1:3*NELEM2)  )
!         TERME EN D(ZS)/DT (MAIS ATTENTION DSSUDT=(H-HN)/DT)
          CALL VECTOR(T2_01,'=', 'MASVEC          ',IELM2H,-1.D0,DSSUDT,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
          CALL OV('X=X+Y   ', X=SEM3D%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       Y=T2_01%R, DIM1=NPOIN2)
!         MANQUE LE TERME EN D(ZF)/DT
        ENDIF
!
!       SOURCES INSIDE THE DOMAIN : DIV(U) WILL NOT BE 0
!       WITH OPTIONS 2 AND 3, THIS IS ALREADY IN RHS_PRESSURE
!
        IF(NSCE.GE.1) THEN
          IF(OPTSOU.EQ.1) THEN
          ! SOURCE NOT CONSIDERED AS A DIRAC
            DO IS=1,NSCE
              IIS=IS
!             HERE SOURCES IN THE NON ASSEMBLED (PARCOM) FORM
!             SEE SOURCES_SINKS
              IF(NCSIZE.GT.1) IIS=IS+NSCE
              CALL OS('X=X+Y   ',X=SEM3D,Y=SOURCES%ADR(IIS)%P)
            ENDDO
          ELSE IF(OPTSOU.EQ.2) THEN
          ! SOURCE CONSIDERED AS A DIRAC
            IIS=1
!           HERE SOURCES IN THE NON ASSEMBLED (PARCOM) FORM
!           SEE SOURCES_SINKS
            IF(NCSIZE.GT.1) IIS=2
            CALL OS('X=X+Y   ',X=SEM3D,Y=SOURCES%ADR(IIS)%P)
          ENDIF
        ENDIF
!
!       BED FLUXES : DIV(U) WILL NOT BE 0
!       WITH OPTIONS 2 AND 3, THIS IS ALREADY IN RHS_PRESSURE
!
        IF(BEDBOU) THEN
          DO IPOIN2=1,NPOIN2
            SEM3D%R(IPOIN2)=SEM3D%R(IPOIN2)+BEDFLU%R(IPOIN2)
          ENDDO
        ENDIF
!
      ELSEIF(OPT.EQ.2.OR.OPT.EQ.3) THEN
!
        CALL RHS_PRESSURE(SEM3D,UP,VP,WP,IELM3,DM1,GRAZCO,SVIDE,
     &                    MESH3D,MSK,MASKEL,FLUEXT,NSCE,RAIN,PLUIE,
     &                    SOURCES,GRADZF,VOLU2D,DSSUDT,
     &                    NPOIN2,NPOIN3,NPLAN,OPTSOU)
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN OPTION IN PREDIV: ',OPT
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!=======================================================================
! DIFFUSION MATRIX AND BOUNDARY TERMS (BC)
!=======================================================================
!
      CALL CPSTVC(SEM3D,T3_01)
      CALL OS('X=C     ', X=T3_01,C=1.D0)
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        FORMUL='MATDIF       MON'
      ELSE
        FORMUL='MATDIF          '
      ENDIF
!
      IF(OPT.EQ.3) THEN
        FORMUL='MATDIF*  1234   '
        IF(SIGMAG.OR.OPTBAN.EQ.1) FORMUL(7:7)='2'
      ENDIF
!
      CALL MATRIX(MDIFF,'M=N     ',FORMUL,IELM3,IELM3,1.D0,
     &            T3_01,T3_01,T3_01,
     &            SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
!
!=======================================================================
! IMPOSED (DIRICHLET) BC + PRECONDITIONING + LINEAR EQUATION SOLVER
!=======================================================================
!
! IMPOSED BC
!
      DO IPOIN3 = 1,NPOIN3
        IT1%I(IPOIN3) = KDDL
        IT2%I(IPOIN3) = 0
        T3_03%R(IPOIN3) = 0.D0
      ENDDO
!
!     BORDS LATERAUX
!
      IF(DIRLAT) THEN
      IF(BC) THEN
        DO IPTFR3 = 1,NPTFR3
          IF(LIPBOL%I(IPTFR3).EQ.KENT.OR.LIPBOL%I(IPTFR3).EQ.KADH) THEN
            IT1%I(NBOR3%I(IPTFR3)) = KDIR
            T3_03%R(NBOR3%I(IPTFR3)) = PBORL%R(IPTFR3)
          ENDIF
        ENDDO
      ELSE
!       PBORL REPLACED BY 0
        DO IPTFR3 = 1,NPTFR3
          IF(LIPBOL%I(IPTFR3).EQ.KENT.OR.LIPBOL%I(IPTFR3).EQ.KADH) THEN
            IT1%I(NBOR3%I(IPTFR3)) = KDIR
            T3_03%R(NBOR3%I(IPTFR3)) = 0.D0
          ENDIF
        ENDDO
      ENDIF
      ENDIF
!
!     BOTTOM
!
      IF(DIRBOT) THEN
      IF(BC) THEN
        DO IPOIN2 = 1,NPOIN2
          IF(LIPBOF%I(IPOIN2).EQ.KENT.OR.LIPBOF%I(IPOIN2).EQ.KADH) THEN
            IT1%I(IPOIN2) = KDIR
            T3_03%R(IPOIN2) = PBORF%R(IPOIN2)
          ENDIF
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIPBOF%I(IPOIN2).EQ.KENT.OR.LIPBOF%I(IPOIN2).EQ.KADH) THEN
            IT1%I(IPOIN2) = KDIR
            T3_03%R(IPOIN2) = 0.D0
          ENDIF
        ENDDO
      ENDIF
      ENDIF
!
!     FREE SURFACE
!
      IF(DIRSUR) THEN
      IF(NCSIZE.GT.1) THEN
        DO IPOIN2 = 1,NPOIN2
          IF(LIPBOS%I(IPOIN2).EQ.KENT.OR.LIPBOS%I(IPOIN2).EQ.KADH) THEN
            IPOIN3=NPOIN3-NPOIN2+IPOIN2
            IT1%I(IPOIN3) = KDIR
            T3_03%R(IPOIN3) = PBORS%R(IPOIN2)
            MDIFF%D%R(IPOIN3)=MESH3D%IFAC%I(IPOIN3)
          ENDIF
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIPBOS%I(IPOIN2).EQ.KENT.OR.LIPBOS%I(IPOIN2).EQ.KADH) THEN
            IPOIN3=NPOIN3-NPOIN2+IPOIN2
            IT1%I(IPOIN3) = KDIR
            T3_03%R(IPOIN3) = PBORS%R(IPOIN2)
            MDIFF%D%R(IPOIN3)=1.D0
          ENDIF
        ENDDO
      ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CRUSHED ELEMENTS AND TIDAL FLATS: FIRST TREATED AS DIRICHLET
!     A CRUSHED POINT HAS NO DIAGONAL
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=T3_02,Y=MDIFF%D)
!         T3_02 WILL BE THE ASSEMBLED DIAGONAL
          CALL PARCOM(T3_02,2,MESH3D)
          DO IPOIN3=1,NPOIN3
            IF(T3_02%R(IPOIN3).LT.1.D-10) THEN
              MDIFF%D%R(IPOIN3)= MESH3D%IFAC%I(IPOIN3)
              IT1%I(IPOIN3)    = KDIR
              IT2%I(IPOIN3)    = 1
              T3_03%R(IPOIN3)  = PD%R(IPOIN3)
            ENDIF
          ENDDO
        ELSE
          DO IPOIN3=1,NPOIN3
            IF(MDIFF%D%R(IPOIN3).LT.1.D-10) THEN
              MDIFF%D%R(IPOIN3)= 1.D0
              IT1%I(IPOIN3)    = KDIR
              IT2%I(IPOIN3)    = 1
              T3_03%R(IPOIN3)  = PD%R(IPOIN3)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL DIRI01(PD,MDIFF,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &            KDIR,MSK,MASKPT)
!
!     SOLVE THE LINEAR EQUATION SYSTEM MDIFF * PD = SEM3D
!
      IF(NPLAN.EQ.2.AND.MDIFF%STO.EQ.3.AND.
     &   MDIFF%ELMLIN.EQ.41.AND.MDIFF%ELMCOL.EQ.41) THEN
!
!       AS THE UPPER PLANE IS DIRICHLET, THIS IS A 2D PROBLEM
!
!       SEM3D AND PD TURNED INTO 2D
        CALL CPSTVC(U2D,PD)
        CALL CPSTVC(U2D,SEM3D)
!
!       PROPERTIES OF MATRIX MODIFIED
!       THE FIRST SEGMENTS IN A 3D MATRIX ARE THE HORIZONTAL
!       SEGMENTS OF THE BOTTOM, HENCE JUST LIKE A 2D MATRIX
!       THE FIRST POINTS IN THE DIAGONAL ARE THE BOTTOM POINTS
!
        SIZD=MDIFF%D%DIM1
        SIZX=MDIFF%X%DIM1
        ELMD=MDIFF%D%ELM
        MDIFF%D%DIM1=NPOIN2
        MDIFF%X%DIM1=MESH2D%NELEM
        MDIFF%ELMLIN=11
        MDIFF%ELMCOL=11
        MDIFF%D%ELM=11
!       PRECONDITIONING 17 WILL NOT BE ACCEPTED IN 2D
        IS=SLVPOI%PRECON
        IF(17*(IS/17).EQ.IS) SLVPOI%PRECON=IS/17
!
#if defined COMPAD
        CALL AD_SOLVE(PD,MDIFF,SEM3D,TRAV2,SLVPOI,INFO,MESH2D,
     &             MAT2D%ADR(1)%P)
#else
        CALL SOLVE(PD,MDIFF,SEM3D,TRAV2,SLVPOI,INFO,MESH2D,
     &             MAT2D%ADR(1)%P)
#endif
!
!       RESTORING POSSIBLE PRECONDITIONING 17
        SLVPOI%PRECON=IS
!
!       OLD PROPERTIES RESTORED
        MDIFF%ELMLIN=41
        MDIFF%ELMCOL=41
        MDIFF%D%DIM1=SIZD
        MDIFF%X%DIM1=SIZX
        MDIFF%D%ELM=ELMD
        CALL CPSTVC(U,PD)
        CALL CPSTVC(U,SEM3D)
!
      ELSE
!
!       REAL 3D PROBLEM
!
#if defined COMPAD
        CALL AD_SOLVE(PD,MDIFF,SEM3D,TRAV3,SLVPOI,INFO,MESH3D,MTRA2)
#else
        CALL SOLVE(PD,MDIFF,SEM3D,TRAV3,SLVPOI,INFO,MESH3D,MTRA2)
#endif
!
      ENDIF
!
!=======================================================================
!
!   POINTS WITH NO DIAGONAL ARE EQUAL TO THE POINT ABOVE
!   THEY HAD BEEN TREATED ABOVE AS DIRICHLET.
!
!=======================================================================
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        DO IPLAN=NPLAN-1,1,-1
          DO IPOIN2=1,NPOIN2
            IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
            IF(IT2%I(IPOIN3).EQ.1) THEN
!             VALUE OF THE UPPER POINT IS COPIED
              PD%R(IPOIN3)=PD%R(IPOIN3+NPOIN2)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

