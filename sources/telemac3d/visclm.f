!                   *****************
                    SUBROUTINE VISCLM
!                   *****************
!
     &(VISCVI,VISCTA,RI,U,V,DELTAR,Z,HN,TRAV1,TRAV2,TRAV3,
     & TRAV4,TRAV5,TRAV7,MESH3D,IELM3,GRAV,
     & NPLAN,NPOIN3,NPOIN2,NTRAC,MSK,MASKEL,MIXING,
     & DAMPING,DNUVIV,DNUTAV,KARMAN,PRANDTL,KFROT,
     & RUGOF,ZF,LINLOG,IPBOT)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    INITIALISES VISCOSITIES.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  C. VILLARET, P. TASSI
!+        02/02/2011
!+        V6P1
!+   Introducing logarithmic derivatives of velocities.
!+
!
!history  J-M HERVOUET (LNHE)
!+        01/12/2011
!+        V6P2
!+   Treatment of tidal flats in case of logarithmic derivatives.
!+   LINLOG as a new parameter for vertical velocity derivatives.
!+   Hardcoded option:
!+   Option 1 for taking advantage of verticals (previous versions)
!+   Option 2 for pure finite elements
!
!history  C. VILLARET
!+        15/12/2012
!+        V6P2
!+   Treatment of tidal flats in case of logarithmic derivatives
!+   and Nikuradse: specific treatment for first plane.
!+
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/06/2015
!+        V7P1
!+   DNUTAV is now an array.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DAMPING        |-->| NUMBER FOR CHOICE OF DAMPING FUNCION
!| DELTAR         |-->| (RHO-RHO0)/RHO0
!| DNUTAV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF TRACER
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| GRAV           |-->| GRAVITY ACCELERATION
!| HN             |-->| WATER DEPTH
!| IELM3          |-->| TYPE OF 3D DISCRETISATION
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| KARMAN         |-->| KARMAN CONSTANT
!| KFROT          |-->| LAW OF BOTTOM FRICTION
!| LINLOG         |-->| 1: LINEAR VERTICAL DERIVATIVES OF VELOCITY
!|                |   | 2: LOGARITHMIC VERTICAL DERIVATIVES OF VELOCITY
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |---| 3D MESH
!| MIXING         |-->| MIXING LENGTH MODEL
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| PRANDTL        |-->| PRANDTL NUMBER
!| RI             |<->| RICHARDSON NUMBER
!| RUGOF          |-->| FRICTION COEFFICIENT
!| TRAV1          |<->| WORK ARRAY
!| TRAV2          |<->| WORK ARRAY
!| TRAV3          |<->| WORK ARRAY
!| TRAV4          |<->| WORK ARRAY
!| TRAV5          |<->| WORK ARRAY
!| TRAV7          |<->| WORK ARRAY
!| U              |-->| HORIZONTAL COMPONENT OF VELOCITY
!| V              |-->| HORIZONTAL COMPONENT OF VELOCITY
!| VISCTA         |<->| DYNAMIC VISCOSITY COEFFICIENTS FOR TRACERS
!| VISCVI         |<->| DYNAMIC VISCOSITY COEFFICIENTS FOR VELOCITIES
!| Z              |-->| MESH COORDINATES
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_VISCLM => VISCLM
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN3, NPOIN2,NPLAN,KFROT
      INTEGER, INTENT(IN)            :: NTRAC,DAMPING,LINLOG
      INTEGER, INTENT(IN)            :: IELM3, MIXING
      INTEGER, INTENT(IN)            :: IPBOT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: GRAV,DNUVIV,DNUTAV(NTRAC),KARMAN
      DOUBLE PRECISION, INTENT(IN)   :: PRANDTL
      LOGICAL, INTENT(IN)            :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI, VISCTA
      TYPE (BIEF_OBJ), INTENT(IN)    :: RUGOF,ZF
      TYPE (BIEF_OBJ), INTENT(INOUT) :: RI
      TYPE (BIEF_OBJ), INTENT(IN)    :: U, V, DELTAR, Z, HN
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV1, TRAV2, TRAV3, TRAV4
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV5, TRAV7
      TYPE (BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE (BIEF_MESH)               :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,I,IPLAN,I3D,OPTION
      DOUBLE PRECISION SDELTAZ,AUX,AUX1,AUX2,DENOM
!
!-----------------------------------------------------------------------
!
!     SETTING DISCRETISATION OF WORK ARRAYS AS VELOCITY U
!
      CALL CPSTVC(U,TRAV1)
      CALL CPSTVC(U,TRAV2)
      CALL CPSTVC(U,TRAV3)
      CALL CPSTVC(U,TRAV4)
      CALL CPSTVC(U,TRAV5)
!
!     OPTION 1 : TAKING ADVANTAGE OF VERTICALS (ONLY WITH PRISMS)
!     OPTION 2 : PURE FINITE ELEMENT PROGRAMMING (PRISMS OR TETRAHEDRONS)
!
      OPTION = 1
!
!-----------------------------------------------------------------------
!
      IF(OPTION.EQ.1.AND.IELM3.EQ.41) THEN
!
!     DISCRETISATION OF VISCOSITES ON VERTICAL
!     DECLARED AS P1 TRIANGLES WITH SECOND DIMENSION NPLAN-1
!     TO HAVE A DISCRETISATION P0 ON VERTICAL AND LINEAR ON HORIZONTAL
!
!     THIS IS DONE WITH COMPONENT DIMDISC SET TO 4111
!     THAT IS SEEN BY MT02PP (AND FORBIDDEN IN MT02TT)
!
      VISCVI%ADR(3)%P%DIMDISC=4111
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          VISCTA%ADR(ITRAC)%P%ADR(3)%P%DIMDISC=4111
        ENDDO
      ENDIF
!
!     COMPUTING DISTANCE TO BOTTOM OF MIDDLE OF EVERY LAYER (TRAV7)
!
      IF(KFROT.NE.5) THEN
        DO IPLAN= 1, NPLAN-1
          DO I = 1, NPOIN2
            I3D=I+NPOIN2*(IPLAN-1)
            TRAV7%R(I3D)=(Z%R(I3D+NPOIN2)+Z%R(I3D))*0.5D0-ZF%R(I)
            TRAV7%R(I3D)=MAX(TRAV7%R(I3D),1.D-8)
          ENDDO
        ENDDO
      ELSE
!       THIS OPTION WORKS ONLY WITH NIKURADSE LAW, HENCE RUGOF
!       IS HERE THE GRAIN SIZE
!       THE REAL BOTTOM IS CONSIDERED TO BE AT RUGOF/30 UNDER
!       THE FIRST PLANE (SO THAT FIRST PLANE CORRESPONDS TO U=0)
!       HERE THE ELEVATION OF TRUE BOTTOM CONSIDERED IS THUS
!       ZF%R(I)-RUGOF%R(I)/30.D0, HENCE THE FORMULA FOR TRAV7
        DO I = 1, NPOIN2
          DO IPLAN= 1, NPLAN-1
          I3D=I+NPOIN2*(IPLAN-1)
          IF(IPLAN.LE.IPBOT(I)+1) THEN
            TRAV7%R(I3D)=(Z%R(I3D+NPOIN2)-ZF%R(I)
     &                   + RUGOF%R(I)/30.D0)*0.5D0
          ELSE
            TRAV7%R(I3D)=(Z%R(I3D+NPOIN2)+Z%R(I3D))*0.5D0-ZF%R(I)
          ENDIF
!         THIS WILL TREAT TIDAL FLATS FOR WHICH Z=ZF
          TRAV7%R(I3D)=MAX(TRAV7%R(I3D),RUGOF%R(I)/60.D0)
          ENDDO
        ENDDO
      ENDIF
!
!     UPPER PLANE (USELESS HERE... BUT COMPUTATION DONE IN LONGML)
!
      DO I=1,NPOIN2
        I3D=I+NPOIN2*(NPLAN-1)
        TRAV7%R(I3D)=Z%R(I3D)-ZF%R(I)
        TRAV7%R(I3D)=MAX(TRAV7%R(I3D),1.D-8)
      ENDDO
!
      IF(LINLOG.EQ.1) THEN
!       LINEAR DERIVATIVE
        DO I=1,NPOIN3-NPOIN2
          SDELTAZ=1.D0/MAX(Z%R(I+NPOIN2)-Z%R(I),1.D-8)
          TRAV1%R(I)=(     U%R(I+NPOIN2)-     U%R(I))*SDELTAZ
          TRAV2%R(I)=(     V%R(I+NPOIN2)-     V%R(I))*SDELTAZ
          TRAV3%R(I)=(DELTAR%R(I+NPOIN2)-DELTAR%R(I))*SDELTAZ
        ENDDO
      ELSEIF(LINLOG.EQ.2) THEN
!       LOGARITHMIC DERIVATIVE
!       DU/DZ =DU/D(LOG(Z))/Z
!
        IF(KFROT.NE.5) THEN
          WRITE(LU,*) 'NIKURADSE LAW MANDATORY'
          WRITE(LU,*) 'WITH LOGARITHMIC DERIVATIVES'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO IPLAN=1,NPLAN-1
        DO I=1,NPOIN2
          I3D=I+NPOIN2*(IPLAN-1)
          SDELTAZ=1.D0/MAX(Z%R(I3D+NPOIN2)-Z%R(I3D),1.D-8)
          AUX1=MAX(Z%R(I3D+NPOIN2)-ZF%R(I),RUGOF%R(I)/30.D0)
          AUX2=MAX(Z%R(I3D       )-ZF%R(I),RUGOF%R(I)/30.D0)
          DENOM=LOG(AUX1)-LOG(AUX2)
          IF(TRAV7%R(I3D).LT.0.2D0*HN%R(I)) THEN
!           LOGARITHMIC DERIVATIVE IN LOGARITHMIC PROFILE ZONE
            AUX=1.D0/(MAX(DENOM*TRAV7%R(I3D),1.D-8))
          ELSE
!           LINEAR DERIVATIVE
            AUX=SDELTAZ
          ENDIF
!         BOTTOM OR FIRST PLANE WITH FREE WATER ABOVE
          IF(IPLAN.LE.IPBOT(I)+1) THEN
!           VELOCITY AT BOTTOM ASSUMED TO BE 0 EVEN IF IT IS NOT
            TRAV1%R(I3D)=(U%R(I3D+NPOIN2)         )*AUX
            TRAV2%R(I3D)=(V%R(I3D+NPOIN2)         )*AUX
          ELSE
            TRAV1%R(I3D)=(U%R(I3D+NPOIN2)-U%R(I3D))*AUX
            TRAV2%R(I3D)=(V%R(I3D+NPOIN2)-V%R(I3D))*AUX
          ENDIF
!         LINEAR DERIVATIVE FOR DELTAR
          TRAV3%R(I3D)=(DELTAR%R(I3D+NPOIN2)-DELTAR%R(I3D))*SDELTAZ
        ENDDO
        ENDDO
      ELSE
        WRITE(LU,*) 'UNKNOWN VERTICAL VELOCITY DERIVATIVES:',LINLOG
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     SURFACE VALUE SET TO 0, IT IS ACTUALLY NOT USED
!     EXCEPT IN LONGML OR LONGMB (HERE FOR NOTHING)
!
      DO I=NPOIN3-NPOIN2+1,NPOIN3
        TRAV1%R(I)=0.D0
        TRAV2%R(I)=0.D0
        TRAV3%R(I)=0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF((OPTION.EQ.2.AND.IELM3.EQ.41).OR.IELM3.EQ.51) THEN
!
!     COMPUTING DISTANCE TO BOTTOM (TRAV7, WILL BE USED BY LONGML)
!
      DO IPLAN= 1, NPLAN
        DO I = 1, NPOIN2
          I3D=I+NPOIN2*(IPLAN-1)
          TRAV7%R(I3D)=MAX(Z%R(I3D)-ZF%R(I),1.D-8)
        ENDDO
      ENDDO
!
      IF(LINLOG.EQ.1) THEN
!
!       LINEAR DERIVATIVE
!
        CALL VECTOR(TRAV1,'=','GRADF          Z',IELM3,1.D0,U,
     &              U,U,U,U,U,MESH3D,MSK,MASKEL)
        CALL VECTOR(TRAV2,'=','GRADF          Z',IELM3,1.D0,V,
     &              U,U,U,U,U,MESH3D,MSK,MASKEL)
        CALL VECTOR(TRAV3,'=','GRADF          Z',IELM3,1.D0,DELTAR,
     &              U,U,U,U,U,MESH3D,MSK,MASKEL)
        CALL VECTOR(TRAV4,'=','MASBAS          ',IELM3,1.D0,
     &              U,U,U,U,U,U,MESH3D,MSK,MASKEL)
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(TRAV1,2,MESH3D)
          CALL PARCOM(TRAV2,2,MESH3D)
          CALL PARCOM(TRAV3,2,MESH3D)
          CALL PARCOM(TRAV4,2,MESH3D)
        ENDIF
!
        DO I=1,NPOIN3
          IF(TRAV4%R(I).GT.1.D-6) THEN
            TRAV4%R(I)=1.D0/TRAV4%R(I)
            TRAV1%R(I)=TRAV1%R(I)*TRAV4%R(I)
            TRAV2%R(I)=TRAV2%R(I)*TRAV4%R(I)
            TRAV3%R(I)=TRAV3%R(I)*TRAV4%R(I)
          ELSE
            TRAV1%R(I)=0.D0
            TRAV2%R(I)=0.D0
            TRAV3%R(I)=0.D0
          ENDIF
        ENDDO
!
      ELSE
!
        WRITE(LU,*) 'VERTICAL VELOCITY DERIVATIVES:',LINLOG
        WRITE(LU,*) 'NOT IMPLEMENTED WITH OPTION 2'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN OPTION OR ELEMENT IN VISCLM'
        WRITE(LU,*) 'OPTION: ',OPTION
        WRITE(LU,*) 'ELEMENT DISCRETISATION: ',IELM3
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE RICHARDSON NUMBER
!
      DO I=1,NPOIN3
        TRAV4%R(I)=TRAV1%R(I)**2+TRAV2%R(I)**2
        RI%R(I)=-GRAV*TRAV3%R(I)/MAX(TRAV4%R(I),1.D-10)
        TRAV1%R(I)=SQRT(TRAV4%R(I))
      ENDDO
!
!-----------------------------------------------------------------------
!
!     SQUARE OF THE MIXING LENGTH : IN TRAV2
!
      IF(MIXING.EQ.1.OR.MIXING.EQ.3.OR.MIXING.EQ.5.OR.MIXING.EQ.6) THEN
!
!                           TRAV7 IS STILL THE HEIGHT ABOVE BOTTOM
        CALL LONGML(TRAV2%R,TRAV7%R,HN%R,NPOIN3,NPOIN2,
     &              NPLAN,MIXING,KARMAN)
!
      ELSE
        WRITE(LU,*) 'UNKNOWN MODEL IN VISCLM : ',MIXING
        IF(MIXING.EQ.4) WRITE(LU,*) 'THE JET MODEL HAS BEEN REMOVED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     STANDARD VISCOSITY WITHOUT DAMPING FUNCTION : TRAV4
!
      CALL OS('X=YZ    ',X=TRAV4,Y=TRAV1,Z=TRAV2)
!
!-----------------------------------------------------------------------
!
! DAMPING FUNCTION FOR
! THE VERTICAL TURBULENT VISCOSITY OF THE VELOCITIES : TRAV5
!
      IF(NTRAC.GT.0) THEN
!
      IF(DAMPING.EQ.1) THEN
!       USER DEFINED
        CALL DRIUTI(TRAV5%R,RI%R,1,0,NPOIN3)
      ELSEIF(DAMPING.EQ.2) THEN
!       MODELE ALGEBRIQUE
        CALL DRIALG(TRAV5%R,TRAV3%R,RI%R,NPOIN3)
      ELSEIF(DAMPING.EQ.3) THEN
!       MUNK ET ANDERSON
        CALL DRICV(TRAV5%R,TRAV3%R,RI%R,NPOIN3)
      ELSEIF(DAMPING.NE.0) THEN
        WRITE(LU,*) 'UNKNOWN DUMPING FUNCTION IN VISCLM: ',
     &                DAMPING
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES VISCVI(1,3): VERTICAL VISCOSITY OF THE VELOCITIES
!
!-----------------------------------------------------------------------
!
      IF(DAMPING.EQ.0.OR.NTRAC.EQ.0) THEN
!       SANS FONCTION D'AMORTISSEMENT
        CALL OS('X=Y     ',X=VISCVI%ADR(3)%P,Y=TRAV4)
      ELSE
!       AVEC FONCTION D'AMORTISSEMENT
        CALL OS('X=YZ    ',X=VISCVI%ADR(3)%P,Y=TRAV5,Z=TRAV4)
      ENDIF
!
!     ADDING THE MOLECULAR VISCOSITY
!
      CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
!     COMPUTES VISCTA(1,3,ITRAC): VERTICAL VISCOSITY OF THE TRACERS
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
          IF(DAMPING.EQ.1) THEN
!           USER DEFINED
            CALL DRIUTI(TRAV3%R,RI%R,2,ITRAC,NPOIN3)
          ELSEIF(DAMPING.EQ.2) THEN
!           VIOLLET (SEE CALL DRIALG ABOVE)
          ELSEIF(DAMPING.EQ.3) THEN
!           CV (SEE CALL DRICV ABOVE)
          ENDIF
!
          IF(DAMPING.EQ.0) THEN
            CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,Y=TRAV4)
          ELSE
            CALL OS('X=YZ    ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                         Y=TRAV3,Z=TRAV4)
          ENDIF
!
!         DIVIDING BY THE PRANDTL NUMBER
!
          IF(ABS(PRANDTL-1.D0).GT.1.D-4) THEN
            CALL OS('X=CX    ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                         C=1.D0/PRANDTL)
          ENDIF
!
!         ADDING THE MOLECULAR VISCOSITY
!
          CALL OS('X=X+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                       C=DNUTAV(ITRAC))
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
