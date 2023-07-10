!                   *****************
                    SUBROUTINE TRIDW2
!                   *****************
!
     &(WSCONV,VOLU,VOLUN,SEM2D,FLUINT,FLUEXT,SOURCES,NSCE,NETAGE,NPOIN2,
     & DT,UNSV2D,MESH2D,OPTSOU)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    COMPUTES AN AVERAGED VALUE OF H * WSTAR IN A WAY
!+                COMPATIBLE WITH THE PSI SCHEME.
!code
!+      IN WSCONV WE FIRST PUT (LOOP 1) :
!+
!+      STARTING FROM THE FIRST PLANE (BOTTOM) :
!+
!+      (H WSTAR) LEVEL 3/2 - (H WSTAR) LEVEL 1/2     (THE LATTER=0)
!+      (H WSTAR) LEVEL 5/2 - (H WSTAR) LEVEL 3/2
!+      ......
!+
!+      (H WSTAR) LEVEL NPLAN - 1/2   -  (H WSTAR) LEVEL NPLAN - 3/2
!+
!+      THE FOLLOWING IS NOT SOLVED
!+      (WOULD ONLY GIVE (H WSTAR) LEVEL NPLAN + 1/2 = 0
!+
!+      (H WSTAR) LEVEL NPLAN + 1/2   -  (H WSTAR) LEVEL NPLAN - 1/2
!+
!+
!+      THEN BY SUCCESSIVE SUMS WE GET IN WSCONV (LOOP 2):
!+
!+      (H WSTAR) LEVEL 3/2
!+      (H WSTAR) LEVEL 5/2
!+      ......
!+
!+      (H WSTAR) LEVEL NPLAN - 1/2
!
!reference  PHD OF J-M HERVOUET, EQUATION 5.58
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history
!+        25/10/2004
!+
!+   DONE WITH MASS-LUMPING, SEE ALSO MT14PP FOR COMPATIBILITY
!
!history  JMH
!+        14/11/2008
!+
!+   CALLS TO OV REPLACED BY LOOPS
!
!history  J-M HERVOUET  (LNHE)
!+        04/12/09
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
!| DT             |-->| TIME STEP
!| FLUINT         |-->| INTERNAL FLUXES
!| FLUEXT         |-->| EXTERNAL FLUXES
!| MESH2D         |-->| 2D MESH STRUCTURE
!| NETAGE         |-->| NPLAN-1
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NSCE           |-->| NUMBER OF SOURCES
!| SEM2D          |<->| RIGHT-HAND SIDE OF 2D EQUATION
!| SOURCES        |-->| FLUXES FROM SOURCES
!| UNSV2D         |-->| INVERSE OF 2D VOLUMES = 1/(INTEGRAL OF BASES)
!| VOLU           |-->| VOLUMES (INTEGRAL OF BASES) AT NEW TIME STEP
!| VOLUN          |-->| VOLUMES (INTEGRAL OF BASES) AT OLD TIME STEP
!| WSCONV         |<->| VERTICAL VELOCITY COMPONENT WSTAR
!|                |   | IN TRANSFORMED MESH AT TIME N+1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, ONLY:BEDBOU,BEDFLU
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NSCE,NETAGE,NPOIN2,OPTSOU
      DOUBLE PRECISION, INTENT(IN)  :: DT
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WSCONV,SEM2D
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU,VOLUN,FLUINT,FLUEXT
      TYPE(BIEF_OBJ), INTENT(IN)    :: SOURCES,UNSV2D
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IETAGE, I,IAD1,IAD2,IAD3,IS
!
      DOUBLE PRECISION :: SURDT
!
!=======================================================================
!
!   SOLVES THE LINEAR SYSTEM
!
!=======================================================================
!
      SURDT=1.D0/DT
!
!-----------------------------------------------------------------------
!
!     LOOP 1
!
!     WSCONV OF LAST LEVEL WILL BE 0 (CHECKED,IT WORKS !)
!     BECAUSE SUM ON THE VERTICAL=2D CONTINUITY EQUATION
!     HENCE LAST LEVEL NOT SOLVED, SO LOOP UP TO NETAGE
!     A CONSEQUENCE IS THAT RAIN AND EVAPORATION IS NOT SEEN HERE
!
      IAD1=0
      IAD3=0
      DO IETAGE = 1,NETAGE
!
        DO I=1,NPOIN2
          IAD1=IAD1+1
          SEM2D%ADR(1)%P%R(I) = FLUINT%R(IAD1)-FLUEXT%R(IAD1)
     &                         +SURDT*(VOLUN%R(IAD1)-VOLU%R(IAD1))
        ENDDO
!
!       WITH BED FLUXES
!
        IF(BEDBOU.AND.IETAGE.EQ.1) THEN
          CALL OS('X=X+Y   ',X=SEM2D%ADR(1)%P,Y=BEDFLU)
        ENDIF
!
!       PARALLELISM
!
        IF(NCSIZE.GT.1) CALL PARCOM(SEM2D%ADR(1)%P,2,MESH2D)
!
!       WITH SOURCES (DONE AFTER CALL PARCOM BECAUSE
!       CALL PARCOM ON SOURCES IS ALREADY DONE IN SOURCES_SINKS)
!
        IF(NSCE.GT.0) THEN
!         WITH SOURCES
          IF(OPTSOU.EQ.1) THEN
          ! SOURCE NOT CONSIDERED AS A DIRAC
            DO IS=1,NSCE
              DO I=1,NPOIN2
                IAD2=(IETAGE-1)*NPOIN2+I
                SEM2D%ADR(1)%P%R(I) = SEM2D%ADR(1)%P%R(I)
     &                              + SOURCES%ADR(IS)%P%R(IAD2)
              ENDDO
            ENDDO
          ELSE IF(OPTSOU.EQ.2) THEN
          ! SOURCE CONSIDERED AS A DIRAC
            DO I=1,NPOIN2
              IAD2=(IETAGE-1)*NPOIN2+I
              SEM2D%ADR(1)%P%R(I) = SEM2D%ADR(1)%P%R(I)
     &                            + SOURCES%ADR(1)%P%R(IAD2)
            ENDDO
          ENDIF
        ENDIF
!
!       SOLVES THE SYSTEM (JUST A DIVISION BY DIAGONAL)
!
        DO I=1,NPOIN2
          IAD3=IAD3+1
          WSCONV%R(IAD3)=SEM2D%ADR(1)%P%R(I)*UNSV2D%R(I)
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP 2
!
      IF(NETAGE.GT.1) THEN
        IAD1=0
        IAD2=NPOIN2
        DO IETAGE = 2,NETAGE
          DO I=1,NPOIN2
            IAD1=IAD1+1
            IAD2=IAD2+1
            WSCONV%R(IAD2)=WSCONV%R(IAD2)+WSCONV%R(IAD1)
          ENDDO
        ENDDO
      ENDIF
!
!  TODO: LAST LEVEL : WSCONV = 0 (NOT USEFUL, BECAUSE INITIALISED AT 0,
!                           AT THE BEGINNING OF TELEMAC3D.F
!                           HOWEVER WSCONV IS MODIFIED AFTER BECAUSE WSTAR
!                           IS COPIED INTO IT, BUT IN FACT SET TO 0. ALSO)
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
