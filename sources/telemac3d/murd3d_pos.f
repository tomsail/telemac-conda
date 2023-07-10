!                   *********************
                    SUBROUTINE MURD3D_POS
!                   *********************
!
     &(FC,FN,VOLU,VOLUN,SVOLUN,VOLU2,SVOLU2,RMASS,
     & TRA01,TRA02,TRA03,STRA01,STRA02,STRA03,MESH2,MESH3,
     & NPOIN3,DT,SCHCF,INFOR,CALFLU,FLUXB,FLUX,FLUEXT,
     & FLUEXTPAR,FBORL,NPTFR3,NBOR3,
     & S0F,NSCE,SOURCES,FSCE,RAIN,PLUIE,TRAIN,NPOIN2,
     & FLOPAR,GLOSEG,DIMGLO,NSEG,NPLAN,
     & T5,FLUX_REMOVED,SAVED_VOLU2,SAVED_F,OPTION,IELM3,NITMAX,OPTSOU)
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    ADVECTION OF A TRACER WITH THE NERD SCHEME
!
!+
!+        SEE FLUX3D (HORIZONTAL FLUXES BASED ON FLUINT)
!+            AND PRECON (VERTICAL FLUXES BASED ON WSCONV)
!
!history  J-M HERVOUET (LNHE)
!+        19/04/2010
!+        V6P0
!+   First version.
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
!+        28/10/2011
!+        V6P2
!+   Updated for element 51 (prisms cut into tetrahedra). Better memory
!+   allocation of INDIC_MURD3D_POS.
!
!history  J-M HERVOUET (LNHE)
!+        23/04/2012
!+        V6P2
!+   Values of tracers in rain taken into account. Option 1 removed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/06/2015
!+        V7P1
!+   Sharing at interfaces done differently, one subdomain receives all.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        28/08/2015
!+        V7P1
!+   Add the option OPTSOU to treat sources as a dirac (OPTSOU=2) or
!+   not (OPTSOU=1).
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        15/09/2016
!+        V7P2
!+   Better treatment of evaporation, with a highest possible value.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/04/2017
!+        V7P3
!+   Stricter allowances, and more prints in TESTING mode. In 3D to
!+   avoid mass losses, the NERD scheme should transmit all the fluxes.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        31/08/2017
!+        V7P3
!+   Divisions by 0 secured, due to the fact that some compilers handle
!+   numbers smaller than TINY(1.D0).
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        04/09/2017
!+        V7P3
!+   Now two options, 1 as before, 2: final volumes changed to get an
!+   exact mass-conservation of tracer. 2 should not be mixed with other
!+   advection schemes (they would give mass errors).
!+   The sum of the new volumes is equal to the sum of the old volumes,
!+   so the mass balance of water is unchanged (and anyway is computed
!+   with the depths which are unchanged).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CALFLU         |-->| INDICATE IF FLUX IS CALCULATED FOR BALANCE
!| DIMGLO         |-->| FIRST DIMENSION OF ARRAY GLOSEG
!| DT             |-->| TIME STEP
!| FBORL          |<->| DIRICHLET CONDITIONS ON F ON LATERAL BOUNDARIES
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FLOPAR         |-->| FLUXES BY SEGMENT, ASSEMBLED IN PARALLEL
!| FLUEXT         |-->| OUTPUT FLUX BY NODE
!| FLUEXTPAR      |-->| OUTPUT FLUX BY NODE ASSEMBLED IN //
!| FLUX_REMOVED   |<->| TOTAL FLUX REMOVED OF EACH POINT
!| FLUX           |<->| GLOBAL FLUXES TO BE CHANGED
!| FLUXB          |<->| FLUX FOR F FOR BALANCE
!| FN             |-->| VARIABLE AT TIME N
!| FSCE           |-->| DIRICHLET BOUNDARY CONDITIONS OF F
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| IELM3          |-->| TYPE OF ELEMENT (41: PRISM, ETC.)
!| INFOR          |-->| INFORMATIONS FOR SOLVERS
!| MESH2          |<->| 2D MESH
!| MESH3          |<->| 3D MESH
!| NBOR3          |-->| GLOBAL ADDRESS OF LATERAL BOUNDARY NODES
!| NITMAX         |-->| MAXIMUM NUMBER OF ITERATIONS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NPTFR3         |-->| NUMBER OF LATERAL BOUNDARY POINTS
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NSEG           |-->| NUMBER OF SEGMENTS
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| RMASS          |<->| REMAINING MASSES
!| S0F            |-->| EXPLICIT SOURCE TERM
!| SAVED_F        |<->| TRACER SAVED
!| SAVED_VOLU2    |<->| VOLUME VOLU2 SAVED
!| SCHCF          |-->| ADVECTION SCHEME FOR F
!| SOURCES        |-->| SOURCES
!| STRA01         |<->| STRUCTURE OF TRA01
!| STRA02         |<->| STRUCTURE OF TRA02
!| STRA03         |<->| STRUCTURE OF TRA03
!| SVOLU2         |<->| STRUCTURE OF VOLU2
!| SVOLUN         |-->| STRUCTURE OF VOLUN
!| T5             |<->| WORK ARRAY
!| TRA01          |<->| WORK ARRAY OF DIMENSION NPOIN3 EQUIVALENT TO
!|                |   | VOLU2 FOR CURRENT FINAL TIME
!| TRA02          |<->| WORK ARRAY OF DIMENSION NPOIN3
!| TRA03          |<->| WORK ARRAY OF DIMENSION NPOIN3
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| VOLU           |-->| CONTROL VOLUME AT TIME N+1
!| VOLU2          |<->| LIKE VOLU, BUT ASSEMBLED IN PARALLEL
!| VOLUN          |-->| CONTROL VOLUME AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, ONLY : DEJA_MURD3D_POS,
     &                                   INDIC_MURD3D_POS,
     &                                   SIZEINDIC_MURD3D_POS,
     &                                   BEDBOU,BEDFLU,T2_18,KSCE,ISCE,
     &                                   MAXFRO,NUMLIQ
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: SCHCF,NPOIN3,NPOIN2
      INTEGER, INTENT(IN)             :: NSCE,NSEG,NPLAN,DIMGLO
      INTEGER, INTENT(IN)             :: OPTION,IELM3
      INTEGER, INTENT(IN)             :: NITMAX,OPTSOU,NPTFR3
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2),NBOR3(NPTFR3)
!
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FN(NPOIN3),PLUIE(*)
!
      DOUBLE PRECISION, INTENT(IN)    :: FLUEXT(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FLUEXTPAR(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLU(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLU2(NPOIN3),FBORL(NPTFR3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3),FLUX
      DOUBLE PRECISION, INTENT(INOUT) :: FLUXB(*)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA03(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: DT,TRAIN,FSCE(NSCE)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SVOLU2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUX_REMOVED
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SAVED_VOLU2,SAVED_F,T5
      TYPE(BIEF_OBJ), INTENT(IN)      :: SOURCES,S0F,SVOLUN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: STRA01,STRA02,STRA03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2,MESH3
!
!     DIMENSION OF FLOPAR=NSEG2D*NPLAN+NPOIN2*NETAGE
      DOUBLE PRECISION, INTENT(IN)    :: FLOPAR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: RMASS(*)
!                                        SIZE IN MEMORY = 30*NELEM
!                                        THIS IS ENOUGH
!
      LOGICAL, INTENT(IN)             :: INFOR,CALFLU,RAIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,NITER,IS,IIS,I,NSEGH,NSEGV,OPT,IR,ILIQ
      INTEGER I1,I2,IPLAN,ISEG3D,I2D,I3D,IPTFR
      INTEGER REMAIN_SEG,NEWREMAIN,REMAIN_TOT
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION RINIT,RFLUX,RFLUX_OLD
      DOUBLE PRECISION VOLSEG1,VOLSEG2
!
      DOUBLE PRECISION, PARAMETER :: EPS = 1.D-6
      DOUBLE PRECISION, PARAMETER :: ALLOW = 1.D-6
      DOUBLE PRECISION, PARAMETER :: REDUC = 1.D-10
      DOUBLE PRECISION, PARAMETER :: EPS_VOLUME = 1.D-8
      DOUBLE PRECISION :: TMP1
!
      LOGICAL, PARAMETER :: TESTING = .FALSE.
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      IF(OPTION.NE.1.AND.OPTION.NE.2) THEN
        WRITE(LU,*) 'UNKNOWN OPTION IN MURD3D_POS: ',OPTION
        WRITE(LU,*) 'OPTION 1 TAKEN INSTEAD'
        WRITE(LU,*) 'FOR THE KEYWORD:'
        WRITE(LU,*) 'SCHEME OPTION FOR ADVECTION OF TRACERS'
        WRITE(LU,*) 'AVAILABLE OPTIONS: 1 AND 2'
!       CALL PLANTE(1)
!       STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL CPSTVC(SVOLU2,STRA01)
      CALL CPSTVC(SVOLU2,STRA02)
      CALL CPSTVC(SVOLU2,STRA03)
!
      NSEGH=NSEG*NPLAN
      NSEGV=NPOIN2*(NPLAN-1)
      IF(SCHCF.EQ.ADV_LPO_TF) THEN
!       HORIZONTAL AND VERTICAL SEGMENTS
        REMAIN_SEG=NSEGH+NSEGV
        OPT=1
      ELSEIF(SCHCF.EQ.ADV_NSC_TF) THEN
!       ALL SEGMENTS
        IF(IELM3.EQ.41) THEN
          REMAIN_SEG=NSEGH+NSEGV+2*NSEG*(NPLAN-1)
        ELSEIF(IELM3.EQ.51) THEN
          REMAIN_SEG=NSEGH+NSEGV+NSEG*(NPLAN-1)
        ELSE
          WRITE(LU,*) 'UNKNOWN ELEMENT IN MURD3D_POS:',IELM3
          CALL PLANTE(1)
          STOP
        ENDIF
        OPT=2
      ELSE
        WRITE(LU,*) 'UNKNOWN SCHEME IN MURD3D_POS:',SCHCF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(.NOT.DEJA_MURD3D_POS) THEN
        ALLOCATE(INDIC_MURD3D_POS(REMAIN_SEG))
        SIZEINDIC_MURD3D_POS=REMAIN_SEG
        DEJA_MURD3D_POS=.TRUE.
      ELSE
        IF(REMAIN_SEG.GT.SIZEINDIC_MURD3D_POS) THEN
!         LARGER SIZE OF INDIC_MURD3D_POS REQUIRED (CASE OF SEVERAL CALLS WITH
!         DIFFERENT SCHEMES THAT IMPLY DIFFERENT NUMBERS OF SEGMENTS
!         LIKE SCHEMES LPO_TF AND NSC_TF IN PRISMS
          DEALLOCATE(INDIC_MURD3D_POS)
          ALLOCATE(INDIC_MURD3D_POS(REMAIN_SEG))
          SIZEINDIC_MURD3D_POS=REMAIN_SEG
        ENDIF
      ENDIF
!
      CALL CPSTVC(SVOLU2,FLUX_REMOVED)
!
!***********************************************************************
!
!     COPIES FLUXES FROM FLOPAR TO ARRAY RMASS (REMAINING MASSES
!     TO BE TRANSFERRED AND THEIR ADDRESS IN INDIC_MURD3D_POS)
!
      DO I=1,REMAIN_SEG
        INDIC_MURD3D_POS(I)=I
        RMASS(I)=-DT*FLOPAR(I)
      ENDDO
!
!     SHARES ASSEMBLED FLUXES ON INTERFACE SEGMENTS BY:
!     DIVIDING BY 2 ON INTERFACE HORIZONTAL AND CROSSED SEGMENTS
!     MULTIPLYING BY FAC ON VERTICAL FLUXES
!     THIS WILL GIVE THE SAME UPWINDING INFORMATION
!
      IF(NCSIZE.GT.1) THEN
        CALL SHARE_3D_FLUXES(RMASS,NPLAN,MESH2,MESH3,OPT)
      ENDIF
!
!     REMAINING FLUXES (SPLIT INTO POSITIVE AND NEGATIVE
!                       SO THAT THEY SUM CORRECTLY IN PARALLEL
!                       ABSOLUTE VALUES WOULD NOT SUM CORRECTLY)
!
      RFLUX_OLD=0.D0
      DO I=1,REMAIN_SEG
        RFLUX_OLD=RFLUX_OLD+ABS(RMASS(I))
      ENDDO
      IF(NCSIZE.GT.1) RFLUX_OLD=P_SUM(RFLUX_OLD)
      RINIT=RFLUX_OLD
      IF(TESTING) WRITE(LU,*) 'INITIAL SUM OF ABS(FLUX)=',RINIT/DT
!
!     INITIAL VALUE OF TRACER = FN
!
      CALL OV('X=Y     ', X=FC, Y=FN, DIM1=NPOIN3)
!
!     VOLU2 WILL BE THE VOLUME CHANGING PROGRESSIVELY FROM VOLUN TO VOLU
!
      CALL OS('X=Y     ', X=SVOLU2, Y=SVOLUN)
!
!     VOLU2 ASSEMBLED IN PARALLEL
!
      IF(NCSIZE.GT.1) CALL PARCOM(SVOLU2,2,MESH3)
!
!-----------------------------------------------------------------------
!
!     ENTERING POSITIVE SOURCES (WITH FC = FSCE)
!     IT WILL ALWAYS GIVE POSITIVE VOLUMES
!
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          IF(OPTSOU.EQ.1) THEN
!           THE SOURCES ARE NOT CONSIDERED AS A DIRAC
            DO IPOIN=1,NPOIN3
!             HERE VERSION OF SOURCES ASSEMBLED IN PARALLEL
              IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                VOLU2(IPOIN)=VOLU2(IPOIN)+DT*SOURCES%ADR(IS)%P%R(IPOIN)
                FC(IPOIN)=FC(IPOIN)+DT*(FSCE(IS)-FC(IPOIN))
     &                         *SOURCES%ADR(IS)%P%R(IPOIN)/VOLU2(IPOIN)
              ENDIF
            ENDDO
          ELSEIF(OPTSOU.EQ.2) THEN
!           THE SOURCES ARE CONSIDERED AS A DIRAC
            IF(ISCE(IS).GT.0) THEN
              IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
!             HERE VERSION OF SOURCES ASSEMBLED IN PARALLEL
              IF(SOURCES%ADR(1)%P%R(IPOIN).GT.0.D0) THEN
                VOLU2(IPOIN)=VOLU2(IPOIN)+DT*SOURCES%ADR(1)%P%R(IPOIN)
                FC(IPOIN)=FC(IPOIN)+DT*(FSCE(IS)-FC(IPOIN))
     &                         *SOURCES%ADR(1)%P%R(IPOIN)/VOLU2(IPOIN)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF(CALFLU) THEN
!         CORRESPONDING FLUXES
          DO IS=1,NSCE
            IF(OPTSOU.EQ.1) THEN
!             THE SOURCES ARE NOT CONSIDERED AS A DIRAC
              IIS=IS
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=IIS+NSCE
              DO IPOIN=1,NPOIN3
                IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                  FLUX=FLUX-DT*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
                  FLUXB(MAXFRO+NSCE+1)=FLUXB(MAXFRO+NSCE+1)-DT*FSCE(IS)
     &                                      *SOURCES%ADR(IIS)%P%R(IPOIN)
                ENDIF
              ENDDO
            ELSE IF(OPTSOU.EQ.2) THEN
!             THE SOURCES ARE CONSIDERED AS A DIRAC
              IIS=1
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESS 2 (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=2
              IF(ISCE(IS).GT.0) THEN
                IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
                IF(SOURCES%ADR(1)%P%R(IPOIN).GT.0.D0) THEN
                  FLUX=FLUX-DT*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
                  FLUXB(MAXFRO+NSCE+1)=FLUXB(MAXFRO+NSCE+1)-DT*FSCE(IS)
     &                                      *SOURCES%ADR(IIS)%P%R(IPOIN)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!     ENTERING BEDFLUXES
      IF(BEDBOU) THEN
!         DO IPOIN=1,NPOIN2
!         HERE VERSION OF BEDFLUXES ASSEMBLED IN PARALLEL
!           IF(BEDFLU%R(IPOIN).GT.0.D0) THEN
!             VOLU2(IPOIN)=VOLU2(IPOIN)+DT*BEDFLU%R(IPOIN)
!             FC(IPOIN)=FC(IPOIN)-DT*FC(IPOIN)
!      &                     *BEDFLU%R(IPOIN)/VOLU2(IPOIN)
!           ENDIF
!         ENDDO
!       STORE BEDFLU IN T2_18 AS IT NEEDS TO BE ASSEMBLED
        CALL CPSTVC(BEDFLU,T2_18)
        CALL OS('X=Y     ',X=T2_18,Y=BEDFLU)
        IF(NCSIZE.GT.1) CALL PARCOM(T2_18,2,MESH2)
        DO IPOIN=1,NPOIN2
!         HERE VERSION OF BEDFLUXES ASSEMBLED IN PARALLEL
          IF(T2_18%R(IPOIN).GT.0.D0) THEN
            VOLU2(IPOIN)=VOLU2(IPOIN)+DT*T2_18%R(IPOIN)
            FC(IPOIN)=FC(IPOIN)-DT*FC(IPOIN)
     &                     *T2_18%R(IPOIN)/VOLU2(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
!     RAIN-EVAPORATION (HERE ONLY RAIN, NOT EVAPORATION, HENCE
!                       VALUE OF TRACER IN RAIN TAKEN INTO ACCOUNT)
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
          IF(PLUIE(IPOIN).GT.0.D0) THEN
            IS=NPOIN3-NPOIN2+IPOIN
!           ASSEMBLED FORM OF PLUIE NEEDED HERE
            VOLU2(IS)=VOLU2(IS)+DT*PLUIE(IPOIN)
!           DILUTION EFFECT FOR ALL TRACERS
            FC(IS)=FC(IS)+DT*(TRAIN-FC(IS))*PLUIE(IPOIN)/VOLU2(IS)
          ENDIF
        ENDDO
      ENDIF
!
!     BOUNDARY CONDITIONS: TAKES INTO ACCOUNT ENTERING EXTERNAL FLUXES
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NPTFR3
!         ASSEMBLED VALUE HERE
          IPOIN=NBOR3(I)
          IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
            VOLU2(IPOIN)=VOLU2(IPOIN)-DT*FLUEXTPAR(IPOIN)
            FC(IPOIN)=FC(IPOIN)-DT*(FBORL(I)-FC(IPOIN))
     &                            *FLUEXTPAR(IPOIN)/VOLU2(IPOIN)
          ENDIF
        ENDDO
      ELSE
        DO I=1,NPTFR3
!         FLUEXT SHARED IN PARALLEL (HENCE SAME SIGN)
          IPOIN=NBOR3(I)
          IF(FLUEXT(IPOIN).LT.0.D0) THEN
            VOLU2(IPOIN)=VOLU2(IPOIN)-DT*FLUEXT(IPOIN)
            FC(IPOIN)=FC(IPOIN)-DT*(FBORL(I)-FC(IPOIN))
     &                            *FLUEXT(IPOIN)/VOLU2(IPOIN)
          ENDIF
        ENDDO
      ENDIF
      IF(CALFLU) THEN
        IF(NCSIZE.GT.1) THEN
          DO I = 1,NPTFR3
            IPOIN=NBOR3(I)
            IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
              FLUX = FLUX + DT*FBORL(I)*FLUEXT(IPOIN)
            ENDIF
          ENDDO
        ELSE
          DO I = 1,NPTFR3
            IPOIN=NBOR3(I)
            FLUX = FLUX + DT*FBORL(I)*MIN(FLUEXT(IPOIN),0.D0)
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1) THEN
          DO I = 1,NPTFR3
            IPOIN=NBOR3(I)
            ILIQ=NUMLIQ%I(I)
            IF(ILIQ.GT.0) THEN
              IF(FLUEXTPAR(IPOIN).LT.0.D0) THEN
                FLUXB(ILIQ)=FLUXB(ILIQ)+DT*FBORL(I)*FLUEXT(IPOIN)
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO I = 1,NPTFR3
            IPOIN=NBOR3(I)
            ILIQ=NUMLIQ%I(I)
            IF(ILIQ.GT.0) THEN
              FLUXB(ILIQ)=FLUXB(ILIQ)
     &                   +DT*FBORL(I)*MIN(FLUEXT(IPOIN),0.D0)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
      NITER = 0
!
777   CONTINUE
!
      NITER = NITER + 1
!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FOR DISTRIBUTING THE VOLUMES BETWEEN SEGMENTS
!
!       FLUX_REMOVED (T6)    : TOTAL FLUX REMOVED OF EACH POINT
!       SAVED_VOLU2 (T8)     : VOLUME VOLU2 SAVED
!       SAVED_F (T9)         : TRACER SAVED
!
      IF(NITER.EQ.1) THEN
        DO I=1,NPOIN3
          FLUX_REMOVED%R(I)=0.D0
          SAVED_VOLU2%R(I)=VOLU2(I)
          SAVED_F%R(I)=FC(I)
          T5%R(I)=FC(I)*VOLU2(I)
        ENDDO
        IF(NCSIZE.GT.1) THEN
!         SHARES AFTER SUMMING
          DO IPTFR=1,NPTIR
            I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            DO IPLAN=1,NPLAN
              I3D=I2D+(IPLAN-1)*NPOIN2
              VOLU2(I3D)=VOLU2(I3D)*MESH3%IFAC%I(I3D)
              T5%R(I3D) = T5%R(I3D)*MESH3%IFAC%I(I3D)
            ENDDO
          ENDDO
        ENDIF
      ELSE
!       NOT ALL THE POINTS NEED TO BE INITIALISED NOW
        DO IR=1,REMAIN_SEG
          I=INDIC_MURD3D_POS(IR)
          I1=GLOSEG(I,1)
          I2=GLOSEG(I,2)
          FLUX_REMOVED%R(I1)=0.D0
          FLUX_REMOVED%R(I2)=0.D0
!         SAVING THE DEPTH AND TRACER
          SAVED_VOLU2%R(I1)=VOLU2(I1)
          SAVED_VOLU2%R(I2)=VOLU2(I2)
          SAVED_F%R(I1)=FC(I1)
          SAVED_F%R(I2)=FC(I2)
          T5%R(I1)=FC(I1)*VOLU2(I1)
          T5%R(I2)=FC(I2)*VOLU2(I2)
        ENDDO
!       CANCELLING INTERFACE POINTS (SOME MAY BE ISOLATED IN A SUBDOMAIN
!       AT THE TIP OF AN ACTIVE SEGMENT WHICH IS ELSEWHERE)
        IF(NCSIZE.GT.1) THEN
          DO IPTFR=1,NPTIR
            I2D=MESH3%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            DO IPLAN=1,NPLAN
              I3D=(IPLAN-1)*NPOIN2+I2D
              FLUX_REMOVED%R(I3D)=0.D0
!             SAVING THE VOLUME AND TRACER
              SAVED_VOLU2%R(I3D)=VOLU2(I3D)
              SAVED_F%R(I3D)=FC(I3D)
              VOLU2(I3D)=VOLU2(I3D)*MESH3%IFAC%I(I3D)
              T5%R(I3D) = T5%R(I3D)*MESH3%IFAC%I(I3D)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
      DO I=1,REMAIN_SEG
        ISEG3D=INDIC_MURD3D_POS(I)
        I1=GLOSEG(ISEG3D,1)
        I2=GLOSEG(ISEG3D,2)
!       POSITIVE FLUXES FROM 1 TO 2 !!!
        IF(RMASS(ISEG3D).GT.EPS_VOLUME) THEN
          FLUX_REMOVED%R(I1)=FLUX_REMOVED%R(I1)+RMASS(ISEG3D)
          VOLU2(I1)=0.D0
          T5%R(I1)=0.D0
        ELSEIF(RMASS(ISEG3D).LT.-EPS_VOLUME) THEN
          FLUX_REMOVED%R(I2)=FLUX_REMOVED%R(I2)-RMASS(ISEG3D)
          VOLU2(I2)=0.D0
          T5%R(I2)=0.D0
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) CALL PARCOM(FLUX_REMOVED,2,MESH3)
!
!     FOR ISOLATED POINTS CONNECTED TO AN ACTIVE SEGMENT
!     THAT IS IN ANOTHER SUBDOMAIN
      IF(NCSIZE.GT.1) THEN
        DO IPTFR=1,NPTIR
          I2D=MESH3%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=(IPLAN-1)*NPOIN2+I2D
            IF(FLUX_REMOVED%R(I3D).GT.EPS_VOLUME) THEN
!             ALL VOLUME SHARED
              VOLU2(I3D)=0.D0
              T5%R(I3D)=0.D0
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!     FROM HERE RMASS IS THE REMAINING MASSES TO BE PASSED BETWEEN POINTS
!
!     COMPUTES THE NEW VOLUME WITH FV SCHEME + DB
!
!     HORIZONTAL AND VERTICAL FLUXES TREATED TOGETHER
!
      RFLUX=0.D0
      NEWREMAIN=0
!
      DO IR=1,REMAIN_SEG
        I=INDIC_MURD3D_POS(IR)
        IF(RMASS(I).GT.EPS_VOLUME) THEN
          I1=GLOSEG(I,1)
!         FLUX FROM 1 TO 2 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          IF(SAVED_VOLU2%R(I1).GT.0.D0) THEN
            I2=GLOSEG(I,2)
!           SHARING ON DEMAND: RMASS(I)/FLUX_REMOVED%R(I1) IS A PERCENTAGE
!                              PARENTHESIS ADDED TO FORCE THE ORDER
            VOLSEG1=SAVED_VOLU2%R(I1)*(RMASS(I)/FLUX_REMOVED%R(I1))
!           END OF SHARING ON DEMAND
            IF(RMASS(I).GE.VOLSEG1) THEN
!             ALL VOLSEG1 WILL BE TRANSFERED TO POINT2
!             VOLSEG1 > 0, HENCE VOLU2(I2) ALSO
              RMASS(I) =RMASS(I) -VOLSEG1
              VOLU2(I2)=VOLU2(I2)+VOLSEG1
!             GROUPING H*F
              T5%R(I2)=T5%R(I2)+VOLSEG1*SAVED_F%R(I1)
!             THIS MAY BE DONE SEVERAL TIMES FOR THE SAME POINT
!             BUT THE LAST ONE WILL BE THE GOOD ONE
              FC(I2)=T5%R(I2)/MAX(VOLU2(I2),1.D-300)
              RFLUX=RFLUX+RMASS(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC_MURD3D_POS(NEWREMAIN)=I
            ELSE
              VOLSEG1=VOLSEG1-RMASS(I)
!             GATHERING VOLUMES (HERE VOLU2(I2) WILL REMAIN POSITIVE)
              VOLU2(I2)=VOLU2(I2)+RMASS(I)
              VOLU2(I1)=VOLU2(I1)+VOLSEG1
              T5%R(I1)=T5%R(I1)+VOLSEG1*SAVED_F%R(I1)
              T5%R(I2)=T5%R(I2)+RMASS(I)*SAVED_F%R(I1)
              FC(I1)=T5%R(I1)/VOLU2(I1)
              FC(I2)=T5%R(I2)/VOLU2(I2)
            ENDIF
          ELSE
            RFLUX=RFLUX+RMASS(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC_MURD3D_POS(NEWREMAIN)=I
          ENDIF
        ELSEIF(RMASS(I).LT.-EPS_VOLUME) THEN
          I2=GLOSEG(I,2)
!         FLUX FROM 2 TO 1 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          IF(SAVED_VOLU2%R(I2).GT.0.D0) THEN
            I1=GLOSEG(I,1)
!           SHARING ON DEMAND
            VOLSEG2=-SAVED_VOLU2%R(I2)*(RMASS(I)/FLUX_REMOVED%R(I2))
!           END OF SHARING ON DEMAND
            IF(-RMASS(I).GE.VOLSEG2) THEN
!             ALL VOLSEG2 WILL BE TRANSFERED TO POINT 1
!             VOLSEG2 > 0, HENCE VOLU2(I1) ALSO
              VOLU2(I1)=VOLU2(I1)+VOLSEG2
              RMASS(I) =RMASS(I) +VOLSEG2
              T5%R(I1)=T5%R(I1)+VOLSEG2*SAVED_F%R(I2)
              FC(I1)=T5%R(I1)/MAX(VOLU2(I1),1.D-300)
              RFLUX=RFLUX-RMASS(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC_MURD3D_POS(NEWREMAIN)=I
            ELSE
              VOLSEG2=VOLSEG2+RMASS(I)
!             GATHERING VOLUMES (HERE VOLU2(I1) WILL REMAIN POSITIVE)
              VOLU2(I1)=VOLU2(I1)-RMASS(I)
              VOLU2(I2)=VOLU2(I2)+VOLSEG2
              T5%R(I1)=T5%R(I1)-RMASS(I)*SAVED_F%R(I2)
              T5%R(I2)=T5%R(I2)+VOLSEG2*SAVED_F%R(I2)
              FC(I1)=T5%R(I1)/VOLU2(I1)
              FC(I2)=T5%R(I2)/VOLU2(I2)
            ENDIF
          ELSE
            RFLUX=RFLUX-RMASS(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC_MURD3D_POS(NEWREMAIN)=I
          ENDIF
        ENDIF
      ENDDO
!
      REMAIN_SEG=NEWREMAIN
!
!     MERGES VOLUMES AND FC AT INTERFACE POINTS
!
      IF(NCSIZE.GT.1) THEN
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
!           ARRAY WITH VOLUME*FC AT INTERFACE POINTS
            TRA01(I3D)=VOLU2(I3D)*FC(I3D)
          ENDDO
        ENDDO
!       SUMS VOLUME*FC AT INTERFACE POINTS
        CALL PARCOM(STRA01,2,MESH3)
!       SUMS THE NEW POSITIVE PARTIAL VOLUMES OF INTERFACE POINTS
        CALL PARCOM(SVOLU2,2,MESH3)
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
!           ARRAY WITH VOLUME*F AT INTERFACE POINTS
            IF(VOLU2(I3D).GT.0.D0) THEN
              FC(I3D)=TRA01(I3D)/VOLU2(I3D)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      REMAIN_TOT=REMAIN_SEG
      IF(NCSIZE.GT.1) THEN
        RFLUX=P_SUM(RFLUX)
!       WILL NOT SUM CORRECTLY IN PARALLEL, BUT ONLY TEST IF .EQ.0
        REMAIN_TOT=P_SUM(REMAIN_TOT)
        IF(TESTING) THEN
          WRITE(LU,*) 'NITER=',NITER,' SUM OF ABS(FLUX)=',RFLUX/DT,
     &                ' REMAINING SEGMENTS: ',REMAIN_TOT
        ENDIF
      ENDIF
!
!     4 POSSIBLE REASONS FOR STOPPING:
!
!     1) THERE IS NO REMAINING FLUX
!     2) REMAINING FLUXES DO NOT CHANGE (POSITIVE AND NEGATIVE)
!        WITH SOME ABSOLUTE ALLOWANCE OR REDUCTION COEFFICIENT
!     3) ALL SEGMENTS HAVE BEEN TREATED
!     4) MAXIMUM NUMBER OF ITERATIONS IS REACHED
!
      IF(RFLUX.NE.0.D0                                   .AND.
     &   ABS(RFLUX-RFLUX_OLD).GT.MIN(RINIT*REDUC,ALLOW)  .AND.
     &              REMAIN_TOT.NE.0                      .AND.
     &              NITER.LT.NITMAX                   ) THEN
        RFLUX_OLD=RFLUX
        GO TO 777
      ENDIF
!
!     TAKES INTO ACCOUNT EXITING SOURCES
!
      IF(CALFLU) THEN
!       EXITING SOURCES
        IF(NSCE.GT.0) THEN
          IF(OPTSOU.EQ.1) THEN
!           SOURCE NOT CONSIDERED AS A DIRAC
            DO IS=1,NSCE
              IIS=IS
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=IIS+NSCE
              DO IPOIN=1,NPOIN3
                IF(SOURCES%ADR(IS)%P%R(IPOIN).LT.0.D0) THEN
                  FLUX=FLUX-DT*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)-DT*FC(IPOIN)
     &                                      *SOURCES%ADR(IIS)%P%R(IPOIN)
                ENDIF
              ENDDO
            ENDDO
          ELSE IF(OPTSOU.EQ.2) THEN
!           SOURCE CONSIDERED AS A DIRAC
            DO IS=1,NSCE
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=2
              IF(ISCE(IS).GT.0) THEN
                IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
                IF(SOURCES%ADR(1)%P%R(IPOIN).LT.0.D0) THEN
                  FLUX=FLUX-DT*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
                  FLUXB(MAXFRO+IS)=FLUXB(MAXFRO+IS)-DT*FC(IPOIN)
     &                                      *SOURCES%ADR(IIS)%P%R(IPOIN)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
!       EXITING BEDFLUXES
        IF(BEDBOU) THEN
!         HERE IN PARALLEL BEDFLUXES WITHOUT PARCOM
          DO IPOIN=1,NPOIN2
            IF(BEDFLU%R(IPOIN).LT.0.D0) THEN
              FLUX=FLUX-DT*FC(IPOIN)*BEDFLU%R(IPOIN)
              FLUXB(MAXFRO+NSCE+1)=FLUXB(MAXFRO+NSCE+1)
     &                            -DT*FC(IPOIN)*BEDFLU%R(IPOIN)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     UPDATING VOLUME (FOR CONTROLS AND EVAPORATION THAT COMES AFTER)
!
!     EXITING SOURCES (WITH VOLU2 CHANGED AND FC UNCHANGED)
      IF(NSCE.GT.0) THEN
        IF(OPTSOU.EQ.1) THEN
!         SOURCE NOT CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            DO IPOIN=1,NPOIN3
!             HERE VERSION OF SOURCES ASSEMBLED IN PARALLEL
              IF(SOURCES%ADR(IS)%P%R(IPOIN).LT.0.D0) THEN
                VOLU2(IPOIN)=VOLU2(IPOIN)+
     &                      DT*SOURCES%ADR(IS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
!         SOURCE CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            IF(ISCE(IS).GT.0) THEN
              IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
!             HERE VERSION OF SOURCES ASSEMBLED IN PARALLEL
              IF(SOURCES%ADR(1)%P%R(IPOIN).LT.0.D0) THEN
                VOLU2(IPOIN)=VOLU2(IPOIN)+
     &                      DT*SOURCES%ADR(1)%P%R(IPOIN)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!     EXITING BEDFLUXES (WITH FC UNCHANGED)
      IF(BEDBOU) THEN
        DO IPOIN=1,NPOIN2
!         HERE VERSION OF BEDFLU ASSEMBLED IN PARALLEL
!           IF(BEDFLU%R(IPOIN).LT.0.D0) THEN
!             VOLU2(IPOIN)=VOLU2(IPOIN)+
!      &                  DT*BEDFLU%R(IPOIN)
!           ENDIF
          IF(T2_18%R(IPOIN).LT.0.D0) THEN
            VOLU2(IPOIN)=VOLU2(IPOIN)+DT*T2_18%R(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
!     RAIN-EVAPORATION (HERE ONLY EVAPORATION, NOT RAIN, HENCE
!                       VALUE OF TRACER IN RAIN NOT TAKEN INTO ACCOUNT
!                       AND ASSUMED TO BE 0)
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
          IF(PLUIE(IPOIN).LT.0.D0) THEN
            IS=NPOIN3-NPOIN2+IPOIN
            VOLU2(IS)=VOLU2(IS)+DT*PLUIE(IPOIN)
!           CONCENTRATION EFFECT FOR ALL TRACERS (WRONG FOR TEMPERATURE ??)
            FC(IS)=FC(IS)*(1.D0-DT*PLUIE(IPOIN)/MAX(VOLU2(IS),1.D-10))
!           ARBITRARY CLIPPING OF HIGH VALUES
            FC(IS)=MIN(FC(IS),1000.D0)
          ENDIF
        ENDDO
      ENDIF
!
!     EXITING BOUNDARY FLUXES
!
      IF(CALFLU) THEN
        IF(NCSIZE.GT.1) THEN
          DO I = 1,NPTFR3
            IPOIN=NBOR3(I)
            IF(FLUEXTPAR(IPOIN).GT.0.D0) THEN
              FLUX = FLUX + DT*FC(IPOIN)*FLUEXT(IPOIN)
            ENDIF
          ENDDO
        ELSE
          DO I = 1,NPTFR3
            IPOIN=NBOR3(I)
            FLUX = FLUX + DT*FC(IPOIN)*MAX(FLUEXT(IPOIN),0.D0)
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPTFR3
            IPOIN=NBOR3(I)
            ILIQ=NUMLIQ%I(I)
            IF(ILIQ.GT.0) THEN
              IF(FLUEXTPAR(IPOIN).GT.0.D0) THEN
                FLUXB(ILIQ)=FLUXB(ILIQ)+DT*FC(IPOIN)*FLUEXT(IPOIN)
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO I=1,NPTFR3
            IPOIN=NBOR3(I)
            ILIQ=NUMLIQ%I(I)
            IF(ILIQ.GT.0) THEN
              FLUXB(ILIQ)=FLUXB(ILIQ)
     &                   +DT*FC(IPOIN)*MAX(FLUEXT(IPOIN),0.D0)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     EXITING FLUXES (NO EFFECT ON TRACER, JUST TO GET THE RIGHT VOLUME)
!
      IF(NCSIZE.GT.1) THEN
        DO I = 1,NPTFR3
          IPOIN=NBOR3(I)
          VOLU2(IPOIN)=VOLU2(IPOIN)-DT*MAX(FLUEXTPAR(IPOIN),0.D0)
        ENDDO
      ELSE
        DO I = 1,NPTFR3
          IPOIN=NBOR3(I)
          VOLU2(IPOIN)=VOLU2(IPOIN)-DT*MAX(FLUEXT(IPOIN),0.D0)
        ENDDO
      ENDIF
!
!     CORRECTING FINAL VOLUMES
!
      DO I=1,NPOIN3
        TRA02(I)=VOLUN(I)
        TRA03(I)=VOLU(I)
      ENDDO
!     TRA02: VOLUN ASSEMBLED IN // TRA03: VOLU ASSEMBLED IN //.
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(STRA02,2,MESH3)
        CALL PARCOM(STRA03,2,MESH3)
      ENDIF
!
!     WATCHOUT DYNAMITE !!!!!!!
!     WITH THIS OPTION THE DEPTHS WILL NO LONGER CORRESPOND TO THE VOLUMES !!!
!
      IF(OPTION.EQ.2) THEN
!
!       RETRIEVING A NON ASSEMBLED NEW VOLU BASED ON THE ASSEMBLED VOLU2
!
        DO I=1,NPOIN3
          IF(TRA03(I).GT.1.D-20) THEN
            VOLU(I)=VOLU(I)*(VOLU2(I)/TRA03(I))
          ELSEIF(TRA02(I).GT.1.D-20) THEN
            VOLU(I)=VOLU2(I)*(VOLUN(I)/TRA02(I))
          ELSE
!           IFAC USED, HERE A 3D IFAC WOULD BE USEFUL
            IPOIN=1+MOD(I-1,NPOIN2)
            VOLU(I)=VOLU2(I)*MESH2%IFAC%I(IPOIN)
          ENDIF
        ENDDO
!
      ENDIF
!
!     CONTROL OF FINAL VOLUMES
!
      IF(TESTING) THEN
        DO I=1,NPOIN3
          TRA03(I)=VOLU(I)
        ENDDO
        IF(NCSIZE.GT.1) CALL PARCOM(STRA03,2,MESH3)
        CALL OS('X=X-Y   ',X=STRA03,Y=SVOLU2)
!       CHECKS EQUALITY OF ASSEMBLED VOLUMES
        TMP1 = P_DOTS(STRA03,STRA03,MESH3)
        WRITE(LU,*) 'ERROR ON VOLUMES = ',TMP1
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERMS
!
      IF(S0F%TYPR.NE.'0') THEN
        DO IPOIN=1,NPOIN3
          IF(VOLU2(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DT*S0F%R(IPOIN)/VOLU2(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IMPLICIT SOURCE TERMS ???
!
!-----------------------------------------------------------------------
!
      IF(INFOR) THEN
        WRITE(LU,102) SCHCF,NITER
      ENDIF
!
      IF(NITER.EQ.NITMAX) THEN
        WRITE(LU,103) SCHCF,NITER
      ENDIF
!
102   FORMAT(1X,'MURD3D_POS SCHEME: ',1I4,'  ',1I4,' ITERATIONS')
103   FORMAT(1X,'MURD3D_POS SCHEME: ',1I4,'  ',1I4,' ITERATIONS',
     &  ' = MAXIMUM',
     &  /,'INCREASE MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES')
!
!-----------------------------------------------------------------------
!
      RETURN
      END

