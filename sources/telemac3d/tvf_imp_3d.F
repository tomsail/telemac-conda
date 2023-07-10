!                   *********************
                    SUBROUTINE TVF_IMP_3D
!                   *********************
!
     &(F,FC,FXMAT,FXMATPAR,DT,VOLUNP1MT,NSEG3,NPOIN3,FLUEXT,FLUEXTPAR,
     & NPTFR3,NBOR3,SIZGLO,GLOSEG,
     & TETAF,PSIEXP,FBOR,MESH3D,ICOR,NCOR,PREDICTOR,CORRECTOR,
     & INFOGT,SF,SM,AM2,SLVPSI,BB,RAIN,TRAIN,
     & PLUIE,NPOIN2,OPTSOU,NSCE,ISCE,KSCE,FSCE,SOURCES)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief Equivalent of TVF_IMP (bief) but in 3D. Computes the solution of
!      the locally implicit distributive scheme.
!
!history S. PAVAN (LHSV) & J-M HERVOUET (EDF LAB, LNHE)
!+     01/09/2016
!+     V7P2
!+  First version
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+     15/09/2016
!+     V7P2
!+  Imposing a highest maximum value in case of evaporation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AM2            |<->| WORKING MATRIX FOR LHS
!| DT             |-->| TIME-STEP
!| F              |<--| VALUES OF F AT TIME N+1 OF SUB-ITERATION
!| FBOR           |-->| VALUES OF F AT THE PRESCRIBED BOUNDARIES
!| FC             |-->| VALUES OF F AT TIME N OF SUB-ITERATION
!| FLUEXT         |-->| FLUXES ON BOUNDARIES
!| FLUEXTPAR      |-->| FLUXES ON BOUNDARIES (DEFINED ON ALL DOMAIN
!|                |   | AND ASSEMBLED IN PARALLEL)
!| FXMAT          |-->| FLUXES (NON ASSEMBLED IN PARALLEL)
!| FXMATPAR       |-->| FLUXES (ASSEMBLED IN PARALLEL)
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT.
!| ICOR           |-->| CURRENT CORRECTION NUMBER
!| IELM3          |-->| TYPE OF ELEMENT (41:PRISM, ETC.)
!| INFOGT         |-->| IF YES, PRINT INFORMATION ON SOLVER
!| MESH3D         |-->| MESH STRUCTURE
!| NBOR3          |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOR           |-->| TOTAL NUMBER OF CORRECTIONS ASKED
!| NPOIN3         |-->| NUMBER OF POINTS
!| NPTFR3         |-->| NUMBER OF BOUNDARY POINTS
!| NSEG3          |-->| NUMBER OF SEGMENTS
!| PREDICTOR      |-->| IF YES, PREDICTOR STEP
!| CORRECTOR      |-->| IF YES, CORRECTOR STEP
!| SF             |<->| BIEF_OBJ STRUCTURE OF F
!| SLVPSI         |<->| SOLVER CONFIGURATION FOR PSI
!| SM             |<->| BIEF_OBJ WORKING VARIABLE FOR SECOND MEMBER
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| TETAF          |-->| IMPLICITATION COEFFICIENT ON F
!| VOLUNP1MT      |-->| INTERMEDIATE VOLUME V(N+1-TETA)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG3,NPOIN3,NPTFR3,NPOIN2
      INTEGER, INTENT(IN)             :: SIZGLO,OPTSOU,NSCE
      INTEGER, INTENT(IN)             :: ICOR,NCOR
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2),NBOR3(NPTFR3)
      INTEGER, INTENT(IN)             :: ISCE(NSCE),KSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)    :: VOLUNP1MT(NPOIN3),DT,TRAIN
      DOUBLE PRECISION, INTENT(IN)    :: FLUEXT(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FLUEXTPAR(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: TETAF(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG3),FXMATPAR(NSEG3)
      DOUBLE PRECISION, INTENT(IN)    :: PSIEXP(NPOIN3),FBOR(NPTFR3)
      DOUBLE PRECISION, INTENT(IN)    :: FC(NPOIN3),PLUIE(*)
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN3)
      LOGICAL, INTENT(IN)             :: PREDICTOR,CORRECTOR,INFOGT,RAIN
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: SF,SM,AM2,BB
      TYPE(BIEF_OBJ),  INTENT(IN)     :: SOURCES
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
      TYPE(SLVCFG),    INTENT(INOUT)  :: SLVPSI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N,I1,I2,IP,IIS,IS
      DOUBLE PRECISION NORMR,NORMB,FMIN,FMAX
!
      TYPE(BIEF_OBJ), POINTER :: SURDIAG,BB1,R
!
      INTRINSIC SQRT,MIN
!
!-----------------------------------------------------------------------
!
      BB1    =>BB%ADR(1)%P
      R      =>BB%ADR(2)%P
      SURDIAG=>BB%ADR(3)%P
      CALL CPSTVC(SF,BB1)
      CALL CPSTVC(SF,R)
      CALL CPSTVC(SF,SURDIAG)
!
!-----------------------------------------------------------------------
!
!     COMPUTING GLOBAL EXTREMA, FOR CLIPPING TRUNCATION ERRORS
!
      FMIN=FC(1)
      FMAX=FC(1)
      DO I=1,NPOIN3
        FMIN=MIN(FMIN,FC(I))
        FMAX=MAX(FMAX,FC(I))
      ENDDO
      DO I=1,NPTFR3
        N=NBOR3(I)
        IF(FLUEXTPAR(N).LT.0.D0) THEN
          FMIN=MIN(FMIN,FBOR(I))
          FMAX=MAX(FMAX,FBOR(I))
        ENDIF
      ENDDO
      IF(NSCE.GT.0) THEN
        DO I=1,NSCE
          FMIN=MIN(FMIN,FSCE(I))
          FMAX=MAX(FMAX,FSCE(I))
        ENDDO
      ENDIF
      IF(RAIN) THEN
        FMIN=MIN(FMIN,TRAIN)
        FMAX=MAX(FMAX,TRAIN)
      ENDIF
      IF(NCSIZE.GT.1) THEN
        FMIN=P_MIN(FMIN)
        FMAX=P_MAX(FMAX)
      ENDIF
!
!     IF RISK OF EVAPORATION, ARBITRARY GLOBAL MAXIMUM
!
      IF(RAIN) FMAX=MAX(FMAX,1000.D0)
!
!-----------------------------------------------------------------------
!
      IF(PREDICTOR) THEN
        DO I=1,NPOIN3
          F(I)=FC(I)
        ENDDO
      ENDIF
!
!     BUILDING THE MATRIX AND THE RIGHT-HAND SIDE
!     THE MATRIX IS NOT DONE WITH A CALL MATRIX
!     SO ITS FEATURES HAVE TO BE HARDCODED HERE
!
      AM2%X%DIM1=2*NSEG3
      AM2%X%DIM2=1
      AM2%TYPEXT='Q'
      AM2%TYPDIA='Q'
      AM2%ELMLIN=41
      AM2%ELMCOL=41
      CALL CPSTVC(SF,AM2%D)
!
!     DIAGONAL OF MATRIX
!
      DO I=1,NPOIN3
        AM2%D%R(I)=VOLUNP1MT(I)
      ENDDO
!
!     RIGHT HAND SIDE
!
!     TERM FROM THE DERIVATIVE IN TIME
      IF(PREDICTOR) THEN
        DO I=1,NPOIN3
          SM%R(I)=AM2%D%R(I)*FC(I)
        ENDDO
      ELSEIF(CORRECTOR) THEN
!       THE PREDICTOR IS TAKEN, AT THIS LEVEL IT IS STILL F
        DO I=1,NPOIN3
          SM%R(I)=AM2%D%R(I)*F(I)
        ENDDO
      ELSE
        WRITE(LU,*) 'TVF_IMP_3D, CHECK ARGUMENTS PREDICTOR, CORRECTOR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     IMPLICIT BOUNDARY TERM
!
      DO I=1,NPTFR3
        N=NBOR3(I)
!       WE CONSIDER THAT IF THE FLUX IS NEGATIVE (ENTERING),
!       WE ARE IN A DIRICHLET POINT
        IF(FLUEXTPAR(N).LT.0.D0) THEN
          AM2%D%R(N)=AM2%D%R(N)-DT*TETAF(N)*FLUEXT(N)
!       ELSE
!         NO CONTRIBUTION ON EXITS
        ENDIF
      ENDDO
!
!     DIAGONAL AND OFF-DIAGONAL TERMS
!
      IF(ICOR.LT.NCOR) THEN
!       SYSTEM SIMPLIFIED, MASS SPOILED, BUT IT WILL BE CORRECTED AFTER
!       BY THE LAST CORRECTION
!       HERE EXTRA-DIAGONAL TERMS NOT BUILT
        DO I=1,NSEG3
          I1=GLOSEG(I,1)
          I2=GLOSEG(I,2)
          IF(FXMATPAR(I).LT.0.D0) THEN
            AM2%D%R(I1) = AM2%D%R(I1) - DT*TETAF(I1)*FXMAT(I)
          ELSE
            AM2%D%R(I2) = AM2%D%R(I2) + DT*TETAF(I2)*FXMAT(I)
          ENDIF
        ENDDO
      ELSE
!       NO SIMPLIFICATION, REAL MATRIX
        DO I=1,NSEG3
          I1=GLOSEG(I,1)
          I2=GLOSEG(I,2)
          IF(FXMATPAR(I).LT.0.D0) THEN
            AM2%D%R(I1) = AM2%D%R(I1) - DT*TETAF(I1)*FXMAT(I)
            AM2%X%R(I)=DT*TETAF(I2)*FXMAT(I)
            AM2%X%R(I+NSEG3)=0.D0
          ELSE
            AM2%D%R(I2) = AM2%D%R(I2) + DT*TETAF(I2)*FXMAT(I)
            AM2%X%R(I)=0.D0
            AM2%X%R(I+NSEG3)=-DT*TETAF(I1)*FXMAT(I)
          ENDIF
        ENDDO
!
      ENDIF
!
!     TERMES BII * CIN ET BIJ * CJN
!
!     ASSEMBLED CONTRIBUTION, DONE BY FLUX_EF_VF_3, POSSIBLY
!     WITH DERIVATIVE IN TIME.
      DO I=1,NPOIN3
        SM%R(I)=SM%R(I)-DT*PSIEXP(I)
      ENDDO
!
!     ADD FLUX ON BOUNDARY AND OTHER TERMS
!
      DO I=1,NPTFR3
        N=NBOR3(I)
!       DIRICHLET POINT AS BEFORE
        IF(FLUEXTPAR(N).LT.0.D0) THEN
          SM%R(N)=SM%R(N)+DT*FLUEXT(N)*((1.D0-TETAF(N))*FC(N)-FBOR(I))
!       ELSE
!         NO CONTRIBUTION ON SM FOR EXITS
        ENDIF
      ENDDO
!
!     SOURCES IN CONTINUITY EQUATION
!
      IF(NSCE.GT.0) THEN
        IF(OPTSOU.EQ.1) THEN
!         SOURCE NOT CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            IIS=IS
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
!           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IP=1,NPOIN3
!             TEST ON ASSEMBLED VALUE
              IF(SOURCES%ADR(IS)%P%R(IP).GT.0.D0) THEN
                AM2%D%R(IP)=AM2%D%R(IP)
!                                        NON ASSEMBLED VALUE
     &                     +DT*TETAF(IP)*SOURCES%ADR(IIS)%P%R(IP)
                SM%R(IP)=SM%R(IP)-DT*SOURCES%ADR(IIS)%P%R(IP)
     &                  *((1.D0-TETAF(IP))*FC(IP)-FSCE(IS))
              ENDIF
            ENDDO
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
!         SOURCE CONSIDERED AS A DIRAC
          IIS = 1
!         HERE IN PARALLEL SOURCES WITHOUT PARCOM
!         ARE STORED AT ADRESSES 2 (SEE SOURCES_SINKS.F)
          IF(NCSIZE.GT.1) IIS=2
          DO IS=1,NSCE
            IF(ISCE(IS).GT.0) THEN
              IP=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
!             TEST ON ASSEMBLED VALUE
              IF(SOURCES%ADR(1)%P%R(IP).GT.0.D0) THEN
                AM2%D%R(IP)=AM2%D%R(IP)
!                                        NON ASSEMBLED VALUE
     &                     +DT*TETAF(IP)*SOURCES%ADR(IIS)%P%R(IP)
                SM%R(IP)=SM%R(IP)-DT*SOURCES%ADR(IIS)%P%R(IP)
     &                  *((1.D0-TETAF(IP))*FC(IP)-FSCE(IS))
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!     BED FLUXES (NOT READY, WHAT IS THE VALUE OF TRACER AT ENTRANCES?)
!     IF(BEDBOU) THEN
!       DO IP=1,NPOIN2
!         IF(BEDFLU%R(IP).GT.0.D0) THEN
!           AM2%D%R(IP)=AM2%D%R(IP)+DT*TETAF(IP)*BEDFLU%R(IP)
!           SM%R(IP)=SM%R(IP)+DT*BEDFLU%R(IP)
!                                             ????????
!    &              *((1.D0-TETAF(IP))*FC(IP)-FBED(IP))
!         ENDIF
!       ENDDO
!     ENDIF
!
!     RAIN-EVAPORATION
!
      IF(RAIN) THEN
        DO N=1,NPOIN2
          I=NPOIN3-NPOIN2+N
          IF(PLUIE(N).GT.0.D0) THEN
!           REAL RAIN, VALUE IN RAIN CONSIDERED...
            SM%R(I)=SM%R(I)-DT*PLUIE(N)*((1.D0-TETAF(I))*FC(I)-TRAIN)
            AM2%D%R(I)=AM2%D%R(I)+DT*TETAF(I)*PLUIE(N)
          ELSE
!           EVAPORATION, VALUE IN RAIN NOT CONSIDERED...
!           VAPOR DOES NOT TAKE AWAY THE TRACER...
!           HENCE EXTRA TERMS HERE, UNLIKE OTHER SOURCES
!           THIS IS WRONG FOR TEMPERATURE
            SM%R(I)=SM%R(I)-DT*PLUIE(N)*(1.D0-TETAF(I))*FC(I)
            AM2%D%R(I)=AM2%D%R(I)+DT*TETAF(I)*PLUIE(N)
          ENDIF
        ENDDO
      ENDIF
!
      CALL OS('X=Y     ',X=BB1,Y=AM2%D)
      IF(NCSIZE.GT.1) CALL PARCOM(BB1,2,MESH3D)
!
!     FOR TIDAL FLATS
!
      DO I=1,NPOIN3
!       SEE PRECD1 EPSILON HERE MUST BE GREATER THAN 1.D-20, OR PRECD1 WILL
!       DO THE CLIPPING ITSELF, IN A LESS CONSISTANT WAY.
!       THE TEST MUST BE DONE ON THE ASSEMBLED DIAGONAL
        IF(BB1%R(I).LT.1.D-15) THEN
!         VOLUNP1MT IS A NON ASSEMBLED COEFFICIENT, ANY SUCH COEFFICIENT WOULD WORK...
!         DRY POINT THAT RECEIVES NO WATER, F=FC IS GIVEN AS EQUATION
          AM2%D%R(I)=MAX(1.D-15,VOLUNP1MT(I))
          SM%R(I)   =MAX(1.D-15,VOLUNP1MT(I))*FC(I)
        ENDIF
      ENDDO
!
!     SOLVING THE FINAL LINEAR SYSTEM
!
      IF(ICOR.LT.NCOR) THEN
!       HERE ONE ITERATION OF JACOBI, OFF-DIAGONAL TERMS BUILT ON THE SPOT
        DO I=1,NSEG3
          I1=GLOSEG(I,1)
          I2=GLOSEG(I,2)
          IF(FXMATPAR(I).LT.0.D0) THEN
            SM%R(I1)=SM%R(I1)-DT*TETAF(I2)*FXMAT(I)*F(I2)
          ELSE
            SM%R(I2)=SM%R(I2)+DT*TETAF(I1)*FXMAT(I)*F(I1)
          ENDIF
        ENDDO
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(AM2%D,2,MESH3D)
          CALL PARCOM(SM,2,MESH3D)
        ENDIF
        DO I=1,NPOIN3
          F(I)=SM%R(I)/AM2%D%R(I)
        ENDDO
      ELSE
        IF(SLVPSI%SLV.EQ.7.OR.SLVPSI%SLV.EQ.8) THEN
#if defined COMPAD
          CALL AD_SOLVE(SF,AM2,SM,BB,SLVPSI,INFOGT,MESH3D,AM2)
#else
          CALL SOLVE(SF,AM2,SM,BB,SLVPSI,INFOGT,MESH3D,AM2)
#endif
        ELSE
!         IF NOT GMRES OR DIRECT, JACOBI SOLUTION
          N=0
          CALL OS('X=Y     ',X=BB1,Y=SM)
          CALL OS('X=Y     ',X=SURDIAG,Y=AM2%D)
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM(SURDIAG,2,MESH3D)
            CALL PARCOM(BB1,2,MESH3D)
          ENDIF
          CALL OS('X=1/Y   ',X=SURDIAG,Y=SURDIAG)
          NORMB=SQRT(P_DOTS(BB1,BB1,MESH3D))
100       CONTINUE
          N=N+1
!         ONE ITERATION OF JACOBI
          CALL OS('X=Y     ',X=BB1,Y=SM)
          DO I=1,NSEG3
            I1=GLOSEG(I,1)
            I2=GLOSEG(I,2)
            IF(FXMATPAR(I).LT.0.D0) THEN
              BB1%R(I1)=BB1%R(I1)-AM2%X%R(I      )*F(I2)
            ELSE
              BB1%R(I2)=BB1%R(I2)-AM2%X%R(I+NSEG3)*F(I1)
            ENDIF
          ENDDO
          IF(NCSIZE.GT.1) CALL PARCOM(BB1,2,MESH3D)
!         NEW SOLUTION IN BB1
          DO I=1,NPOIN3
            BB1%R(I)=BB1%R(I)*SURDIAG%R(I)
          ENDDO
!         END OF ONE ITERATION, COMPUTING THE RESIDUAL
!         AX-B = (A%D+A%X)X-B = A%X*(NEW F - OLD F)
          CALL OS('X=0     ',X=R)
          DO I=1,NSEG3
            I1=GLOSEG(I,1)
            I2=GLOSEG(I,2)
            IF(FXMATPAR(I).LT.0.D0) THEN
              R%R(I1)=R%R(I1)-AM2%X%R(I)*(F(I2)-BB1%R(I2))
            ELSE
              R%R(I2)=R%R(I2)-AM2%X%R(I+NSEG3)*(F(I1)-BB1%R(I1))
            ENDIF
          ENDDO
          IF(NCSIZE.GT.1) CALL PARCOM(R,2,MESH3D)
          NORMR=SQRT(P_DOTS(R,R,MESH3D))
!         COPY OF NEW SOLUTION ON F
          DO I=1,NPOIN3
!           COPY OF NEW SOLUTION ON F, WITH CLIPPING WITH GLOBAL EXTREMA
!           TO COPE WITH TRUNCATION ERRORS. IF CLIPPING TRUE ERRORS IT
!           WILL DO MASS ERRORS
            F(I)=MAX(MIN(BB1%R(I),FMAX),FMIN)
          ENDDO
          IF(N.LT.SLVPSI%NITMAX.AND.NORMR.GT.SLVPSI%EPS*NORMB) THEN
            GO TO 100
          ENDIF
!         DONE !
          IF(INFOGT) THEN
            IF(NORMR.GT.SLVPSI%EPS*NORMB) THEN
              IF(NORMB.GT.1.D0) THEN
                WRITE(LU,*)
     &            'JACOBI: MAXIMUM ITERATIONS REACHED ',N,
     &            ' ITERATIONS, RELATIVE PRECISION = ',NORMR/NORMB
              ELSE
                WRITE(LU,*)
     &            'JACOBI: MAXIMUM ITERATIONS REACHED ',N,
     &            ' ITERATIONS, ABSOLUTE PRECISION = ',NORMR
              ENDIF
            ELSE
              IF(NORMB.GT.1.D0) THEN
                WRITE(LU,*) 'JACOBI:',N,
     &            ' ITERATIONS, RELATIVE PRECISION = ',NORMR/NORMB
              ELSE
                WRITE(LU,*) 'JACOBI:',N,
     &            ' ITERATIONS, ABSOLUTE PRECISION = ',NORMR
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

