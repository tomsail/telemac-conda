!                   ************************
                    SUBROUTINE MAXSLOPE_GAIA
!                   ************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Collapse of sand with a slope greater than a
!!       stability criterion.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in/out] EVOL   Work array
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_MAXSLOPE => MAXSLOPE_GAIA
      USE BIEF
      USE DECLARATIONS_GAIA
      USE INTERFACE_PARALLEL, ONLY: P_MAX
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!

!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I,IG1,IG2,IR1,IR2
      INTEGER IPOIN,ILAYER,ISAND,IMUD,ICLA,FIRST_LAYER
      DOUBLE PRECISION X2,X3,Y2,Y3,Z2,Z3,A,B,L,ZC,DEUXSURF,TANSL
      DOUBLE PRECISION QGLI(3),QG1,QG2,QR1,QR2
      DOUBLE PRECISION FAC1,FAC2,FAC3
      DOUBLE PRECISION QELEM(NELEM,3)
      DOUBLE PRECISION POURCENTAGE_EPAI_TRANSFERT
      DOUBLE PRECISION EPAI_GLISS,EPAI_GLISS_LAYER,FLUX_LOC
      DOUBLE PRECISION TEST_GLISS
      INTEGER NB_PDT_GLISS ! passer en mot clé ?
      TYPE(BIEF_OBJ),POINTER :: EVOL
!
      LOGICAL CASE2
!
      INTRINSIC SQRT,MIN,MAX,TAN
!
!-----------------------------------------------------------------------
!
      EVOL => TB%ADR(1)%P ! WORK ARRAY
!
      PI = 4.D0 * ATAN( 1.D0 )
      TANSL = TAN( PI*PHISED/180.D0 )
!>      @todo make keyword
      NB_PDT_GLISS = 10
!
      CALL CPSTVC(UNSV2D,EVOL)
!
!======================================================================
!    TOTAL MASS OF SEDIMENT IN EACH POINT BEFORE SLIDING
!    JUST FOR CHECK THAT EVOLUTION OF MASS = 0
!>@todo can be removed if it works
!     INITIALISES
      CALL OS('X=0     ',X=EVOL_MM)
!    EVOL_MM%R() CONTAINED -MASS OF SEDIMENT BEFORE SLIDING
!    LATER WE ADD MASS OF SEDIMENT AFTER SLIDING TO HAVE EVOLUTION
!
      IF(NSAND.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO ISAND = 1,NSAND
              EVOL_MM%R(IPOIN) = EVOL_MM%R(IPOIN)
     &        - MASS_SAND(ISAND,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(NMUD.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO IMUD = 1,NMUD
              EVOL_MM%R(IPOIN) = EVOL_MM%R(IPOIN)
     &        - MASS_MUD(IMUD,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!-------------------------------------------------------------------------
!
      DO IPOIN = 1,NPOIN
        IF(NMUD.GE.1) THEN
          DO ILAYER = 1,NOMBLAY
            DO IMUD = 1,NMUD
              FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)=0.D0
            ENDDO
          ENDDO
        ENDIF
        IF(NSAND.GE.1) THEN
          DO ILAYER = 1,NOMBLAY
            DO ISAND = 1,NSAND
              FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)=0.D0
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
! SLIDING IS BASED ON ZF BUT IS APPLIED LAYER BY LAYER.
! SLIDING IS FIRST APPLIED ON THE FIRST LAYER, A NEW ZF IS COMPUTED.
! THEN, IF NECESSARY, SLIDING IS APPLIED ON THE OTHER LAYERS.
      IF(BED_MODEL.EQ.2.AND.HIRANO)THEN
! IN THIS CASE, NO SLIDING FOR ACTIVE LAYER BECAUSE THE CONCENTRATION
! OF MUD IS NOT CONSTANT IN SPACE: FURTHER DEVELOPMENTS ARE NECESSARY
          FIRST_LAYER = 2
      ELSE
          FIRST_LAYER = 1
      ENDIF
!
      DO ILAYER= FIRST_LAYER,NOMBLAY
!
        CALL OS('X=0     ',X=EVOL)
!
!       TO CHECK IF ANY SLIDING IS DONE AND SHORTCUT IF NOT
        TEST_GLISS = 0.D0
!
!       FIRST LOOP ON ELEMENTS. THIS FIRST LOOP ENABLES TO COMPUTE THE
!       TRANSFERS OF VOLUMES BETWEEN THE 3 POINTS INSIDE EACH ELEMENT,
!       AND TO COMPUTE THE MAXIMUM POSSIBLE EROSION FOR EACH POINTS,
!       IN ORDER TO ENABLE LIMITATION OF FLUXES ON RIGID BEDS.
!       THE ACTUAL LIMITATION OF FLUXES ON RIGID BEDS, AND THE APPLYING
!       OF FLUXES TO MASSES OF THE LAYERS ARES DONE IN THE SECOND LOOP
!       ON ELEMENTS.
        DO IELEM=1,NELEM
!
          I1=MESH%IKLE%I(IELEM)
          I2=MESH%IKLE%I(IELEM+NELEM)
          I3=MESH%IKLE%I(IELEM+2*NELEM)
!
          X2=MESH%XEL%R(IELEM+NELEM)
          X3=MESH%XEL%R(IELEM+2*NELEM)
          Y2=MESH%YEL%R(IELEM+NELEM)
          Y3=MESH%YEL%R(IELEM+2*NELEM)
          Z2=ZF%R(I2)-ZF%R(I1)
          Z3=ZF%R(I3)-ZF%R(I1)
!
!         TWICE THE TRIANGLE AREA
!
          DEUXSURF=X2*Y3-X3*Y2
!
!         AVERAGE BOTTOM IN THE ELEMENT
!
          ZC=(ZF%R(I1)+ZF%R(I2)+ZF%R(I3))/3.D0
!
!         COMPONENTS OF BOTTOM GRADIENT
!
          A=(Z2*Y3-Z3*Y2)/DEUXSURF
          B=(Z3*X2-Z2*X3)/DEUXSURF
!
!         CORRECTING FACTOR ON SLOPE
!
          L=MIN(1.D0,TANSL/MAX(SQRT(A**2+B**2),1.D-8))
!
!         HERE THE EVOLUTIONS ARE MULTIPLIED BY SURFAC/3
!         BECAUSE THE REAL EVOLUTION TAKING INTO ACCOUNT OTHER ELEMENTS
!         WILL NEED A FACTOR (SURFAC/3)/(INTEGRAL OF BASIS)
!
          QELEM(IELEM,1)=(1.D0/NB_PDT_GLISS)*
     &                   (1.D0-L)*(ZC-ZF%R(I1))*DEUXSURF/6.D0
          QELEM(IELEM,2)=(1.D0/NB_PDT_GLISS)*
     &                   (1.D0-L)*(ZC-ZF%R(I2))*DEUXSURF/6.D0
          QELEM(IELEM,3)=(1.D0/NB_PDT_GLISS)*
     &                   (1.D0-L)*(ZC-ZF%R(I3))*DEUXSURF/6.D0
!!
! QELEM AND EVOL ARE VOLUMES.
! EVOL IS USED ONLY FOR FLUX LIMITATION ON RIGID BEDS
! ONLY QELEM CORRESPONDING TO EROSIONS ARE ADDED TO EVOL (PESSIMISTIC),
! OTHERWISE THE LIMITATION IS NOT ENOUGH
!
          IF(QELEM(IELEM,1).LE.0.D0) THEN
            EVOL%R(I1)=EVOL%R(I1)+QELEM(IELEM,1)
          ENDIF
          IF(QELEM(IELEM,2).LE.0.D0) THEN
            EVOL%R(I2)=EVOL%R(I2)+QELEM(IELEM,2)
          ENDIF
          IF(QELEM(IELEM,3).LE.0.D0) THEN
            EVOL%R(I3)=EVOL%R(I3)+QELEM(IELEM,3)
          ENDIF
!
        ENDDO ! END OF FIRST LOOP ON ELEMENTS
!
!       ADDING EVOL IN PARALLEL
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(EVOL,2,MESH)
        ENDIF
!
        DO I=1,NPOIN
          TEST_GLISS=TEST_GLISS+ABS(EVOL%R(I))
        ENDDO
!
!       GET PARALLEL MAX TO MAKE SURE EACH PARTITION DOES THE SAME (BECAUSE THER ARE MORE PARCOM CALLS BELOW)
        TEST_GLISS = P_MAX(TEST_GLISS)
!
!       IF NO SLIDING NEEDED, END OF SUBROUTINE
        IF(TEST_GLISS.LE.1.D-8) GOTO 100
!

!MDS refaire une boucle sur les éléments pour appliquer le facteur ES(ILAYER)/EVOL aux evolutions
!MDS et pour appliquer les Q en terme de masse.
!
!       SECOND LOOP ON ELEMENTS
!       HERE THE LIMITATION OF FLUXES ON RIGID BED IS MADE, AND
!       THE FLUXES (QELEM, VOLUMETRIC) ARE CONVERTED IN FLUXES OF MASS
        DO IELEM=1,NELEM
!
          I1=MESH%IKLE%I(IELEM)
          I2=MESH%IKLE%I(IELEM+NELEM)
          I3=MESH%IKLE%I(IELEM+2*NELEM)
!
!         LIMITATION FACTOR LESS THAN 1 WHEN LIMITATION OVER RIGID BED

          FAC1=1.D0
          IF(QELEM(IELEM,1).LE.0.D0) THEN
            FAC1=MAX(0.D0,
     &      MIN(1.D0,
     &      ES(I1,ILAYER)/MAX(-EVOL%R(I1)*UNSV2D%R(I1),1.D-15)))
          ENDIF
!
          FAC2=1.D0
          IF(QELEM(IELEM,2).LE.0.D0) THEN
            FAC2=MAX(0.D0,
     &      MIN(1.D0,
     &      ES(I2,ILAYER)/MAX(-EVOL%R(I2)*UNSV2D%R(I2),1.D-15)))
          ENDIF
!
          FAC3=1.D0
          IF(QELEM(IELEM,3).LE.0.D0) THEN
            FAC3=MAX(0.D0,
     &      MIN(1.D0,
     &      ES(I3,ILAYER)/MAX(-EVOL%R(I3)*UNSV2D%R(I3),1.D-15)))
          ENDIF
!
          QGLI(1)=QELEM(IELEM,1)*FAC1*FAC2*FAC3
          QGLI(2)=QELEM(IELEM,2)*FAC1*FAC2*FAC3
          QGLI(3)=QELEM(IELEM,3)*FAC1*FAC2*FAC3
!
!         IG1 AND IG2 : POINTS THAT GIVE
!         IR1 AND IR2 : POINTS THAT RECEIVE
!         CASE2: TWO POINTS GIVE TO THE THIRD ONE (THE OTHER CASE IS
!                ONE POINT GIVES TO THE TWO OTHERS)
          CASE2=.FALSE.
!
!         PARAMETERISING TO REDUCE THE 6 CASES TO 2
!
!! Q ARE VOLUMES
! FLUX LEAVING IS NEGATIVE.
          IF(QGLI(1).GT.0.D0) THEN
            IF(QGLI(2).GT.0.D0) THEN
!             3 GIVES TO 1 AND 2
              IG1=I3
              QG1=QGLI(3)
              IR1=I1
              QR1=QGLI(1)
              IR2=I2
              QR2=QGLI(2)
            ELSE
              IF(QGLI(3).GT.0.D0) THEN
!               2 GIVES TO 1 AND 3
                IG1=I2
                QG1=QGLI(2)
                IR1=I1
                QR1=QGLI(1)
                IR2=I3
                QR2=QGLI(3)
              ELSE
!               2 AND 3 GIVE TO 1
                IG1=I2
                QG1=QGLI(2)
                IG2=I3
                QG2=QGLI(3)
                IR1=I1
                QR1=QGLI(1)
                CASE2=.TRUE.
              ENDIF
            ENDIF
          ELSE
            IF(QGLI(2).GT.0.D0) THEN
              IF(QGLI(3).GT.0.D0) THEN
!               1 GIVES TO 2 AND 3
                IG1=I1
                QG1=QGLI(1)
                IR1=I2
                QR1=QGLI(2)
                IR2=I3
                QR2=QGLI(3)
              ELSE
!               1 AND 3 GIVE TO 2
                IG1=I1
                QG1=QGLI(1)
                IG2=I3
                QG2=QGLI(3)
                IR1=I2
                QR1=QGLI(2)
                CASE2=.TRUE.
              ENDIF
            ELSE
!             1 AND 2 GIVE TO 3
              IG1=I1
              QG1=QGLI(1)
              IG2=I2
              QG2=QGLI(2)
              IR1=I3
              QR1=QGLI(3)
              CASE2=.TRUE.
            ENDIF
          ENDIF
!
!
          IF(CASE2) THEN
!
!           THE TWO DONORS CASE : IG1 AND IG2 GIVE TO IR1
!           FILLS FLUX_MASS_SAND AND FLUX_MASS_MUD
!           WHICH ARE IN DECLARATION_GAIA BUT USED ONLY LOCALLY.
!           WE DO NOT NEED ANY INFO ON QR1.

! FIRST TRANSFER OF MASS : FROM IG1 TO IR1
            EPAI_GLISS = -QG1*UNSV2D%R(IG1)
            IF(EPAI_GLISS.GT.0.D0) THEN
!           IF EPAI_GLISS > ES THE WHOLE LAYER WILL SLIDE. OTHERWISE ONLY A PERCENTAGE OF THE LAYER
              POURCENTAGE_EPAI_TRANSFERT=
     &        MAX(0.D0,MIN(1.D0,EPAI_GLISS/ES(IG1,ILAYER)))
!
              IF(NMUD.GE.1) THEN
                DO IMUD = 1,NMUD
                  FLUX_LOC=-MIN(MAX(0.D0,POURCENTAGE_EPAI_TRANSFERT*
     &              MASS_MUD(IMUD,ILAYER,IG1)),
     &              MASS_MUD(IMUD,ILAYER,IG1))
                  FLUX_MASS_MUD(IMUD,ILAYER,IG1)=
     &              FLUX_MASS_MUD(IMUD,ILAYER,IG1)
     &              +FLUX_LOC
                  FLUX_MASS_MUD(IMUD,ILAYER,IR1)=
     &              FLUX_MASS_MUD(IMUD,ILAYER,IR1)
     &              -FLUX_LOC*V2DPAR%R(IG1)/V2DPAR%R(IR1)
                ENDDO
              ENDIF
              IF(NSAND.GE.1) THEN
                DO ISAND = 1,NSAND
                  FLUX_LOC=-MIN(MAX(0.D0,POURCENTAGE_EPAI_TRANSFERT*
     &              MASS_SAND(ISAND,ILAYER,IG1)),
     &              MASS_SAND(ISAND,ILAYER,IG1))
                  FLUX_MASS_SAND(ISAND,ILAYER,IG1)=
     &              FLUX_MASS_SAND(ISAND,ILAYER,IG1)
     &              +FLUX_LOC
                  FLUX_MASS_SAND(ISAND,ILAYER,IR1)=
     &              FLUX_MASS_SAND(ISAND,ILAYER,IR1)
     &              -FLUX_LOC*V2DPAR%R(IG1)/V2DPAR%R(IR1)
                ENDDO
              ENDIF
            ENDIF ! ENDIF EPAI_GLISS > 0
!           END OF TRANSFER FROM IG1 TO IR1
!
!!! 2) FROM IG2 TO IR1
            EPAI_GLISS = -QG2*UNSV2D%R(IG2)
            IF(EPAI_GLISS.GT.0.D0) THEN
              POURCENTAGE_EPAI_TRANSFERT =
     &         MAX(0.D0,MIN(1.D0,EPAI_GLISS/ES(IG2,ILAYER)))
              EPAI_GLISS_LAYER =
     &        POURCENTAGE_EPAI_TRANSFERT*ES(IG2,ILAYER)
!
              IF(NMUD.GE.1) THEN
                DO IMUD = 1,NMUD
                  FLUX_LOC=-MIN(MAX(0.D0,POURCENTAGE_EPAI_TRANSFERT*
     &              MASS_MUD(IMUD,ILAYER,IG2)),
     &              MASS_MUD(IMUD,ILAYER,IG2))
                  FLUX_MASS_MUD(IMUD,ILAYER,IG2) =
     &              FLUX_MASS_MUD(IMUD,ILAYER,IG2)
     &              +FLUX_LOC
                  FLUX_MASS_MUD(IMUD,ILAYER,IR1) =
     &              FLUX_MASS_MUD(IMUD,ILAYER,IR1)
     &              -FLUX_LOC*V2DPAR%R(IG2)/V2DPAR%R(IR1)
                ENDDO
              ENDIF
              IF(NSAND.GE.1) THEN
                DO ISAND = 1,NSAND
                  FLUX_LOC=-MIN(MAX(0.D0,POURCENTAGE_EPAI_TRANSFERT*
     &              MASS_SAND(ISAND,ILAYER,IG2)),
     &              MASS_SAND(ISAND,ILAYER,IG2))
                  FLUX_MASS_SAND(ISAND,ILAYER,IG2)=
     &              FLUX_MASS_SAND(ISAND,ILAYER,IG2)
     &              +FLUX_LOC
                  FLUX_MASS_SAND(ISAND,ILAYER,IR1)=
     &              FLUX_MASS_SAND(ISAND,ILAYER,IR1)
     &              -FLUX_LOC*V2DPAR%R(IG2)/V2DPAR%R(IR1)
                ENDDO
              ENDIF
            ENDIF ! ENDIF EPAI_GLISS > 0
!           END OF TRANSFER FROM IG1 TO IR2
!
          ELSE
!
!           THE ONE DONOR CASE : IG1 GIVES TO IR1 AND IR2
!           QR1 AND QR2 ONLY USED TO COMPUTE THE RATIO TO KNOW
!           THE REPARTITION OF QG1 BETWWEN IR1 AND IR2
            EPAI_GLISS = -QG1*UNSV2D%R(IG1)
            IF(EPAI_GLISS.GT.0.D0) THEN
              POURCENTAGE_EPAI_TRANSFERT =
     &          MAX(0.D0,MIN(1.D0,EPAI_GLISS/ES(IG1,ILAYER)))
              EPAI_GLISS_LAYER =
     &          POURCENTAGE_EPAI_TRANSFERT*ES(IG1,ILAYER)
              IF(NMUD.GE.1) THEN
                DO IMUD = 1,NMUD
                  FLUX_LOC=-MIN(MAX(0.D0,POURCENTAGE_EPAI_TRANSFERT*
     &              MASS_MUD(IMUD,ILAYER,IG1)),
     &              MASS_MUD(IMUD,ILAYER,IG1))
                  FLUX_MASS_MUD(IMUD,ILAYER,IG1) =
     &              FLUX_MASS_MUD(IMUD,ILAYER,IG1)
     &              +FLUX_LOC
                  FLUX_MASS_MUD(IMUD,ILAYER,IR1) =
     &              FLUX_MASS_MUD(IMUD,ILAYER,IR1)
     &              -QR1/(QR1+QR2)*FLUX_LOC*V2DPAR%R(IG1)/V2DPAR%R(IR1)
                  FLUX_MASS_MUD(IMUD,ILAYER,IR2) =
     &              FLUX_MASS_MUD(IMUD,ILAYER,IR2)
     &              -QR2/(QR1+QR2)*FLUX_LOC*V2DPAR%R(IG1)/V2DPAR%R(IR2)
                ENDDO
              ENDIF
              IF(NSAND.GE.1) THEN
                DO ISAND = 1,NSAND
                  FLUX_LOC=-MIN(MAX(0.D0,POURCENTAGE_EPAI_TRANSFERT*
     &              MASS_SAND(ISAND,ILAYER,IG1)),
     &              MASS_SAND(ISAND,ILAYER,IG1))
                  FLUX_MASS_SAND(ISAND,ILAYER,IG1) =
     &              FLUX_MASS_SAND(ISAND,ILAYER,IG1)
     &              +FLUX_LOC
                  FLUX_MASS_SAND(ISAND,ILAYER,IR1) =
     &              FLUX_MASS_SAND(ISAND,ILAYER,IR1)
     &              -QR1/(QR1+QR2)*FLUX_LOC*V2DPAR%R(IG1)/V2DPAR%R(IR1)
                  FLUX_MASS_SAND(ISAND,ILAYER,IR2) =
     &              FLUX_MASS_SAND(ISAND,ILAYER,IR2)
     &              -QR2/(QR1+QR2)*FLUX_LOC*V2DPAR%R(IG1)/V2DPAR%R(IR2)
                ENDDO
              ENDIF
            ENDIF ! ENDIF EPAI_GLISS > 0
! END OF CASE IG1 > IR1 AND IR2
!
          ENDIF ! CASE2
!
        ENDDO ! END OF SECOND LOOP ON ELEMENTS
!
!       ADDING VOLUMES IN PARALLEL
        IF(NCSIZE.GT.1) THEN
          IF(NMUD.GE.1) THEN
            DO IMUD=1,NMUD
              DO IPOIN = 1,NPOIN
                T2%R(IPOIN) = FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)
              ENDDO
              CALL PARCOM(T2,2,MESH)
              DO IPOIN = 1,NPOIN
                FLUX_MASS_MUD(IMUD,ILAYER,IPOIN) = T2%R(IPOIN)
              ENDDO
            ENDDO
          ENDIF
          IF(NSAND.GE.1) THEN
            DO ISAND=1,NSAND
              DO IPOIN = 1,NPOIN
                T2%R(IPOIN) = FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)
              ENDDO
              CALL PARCOM(T2,2,MESH)
              DO IPOIN = 1,NPOIN
                FLUX_MASS_SAND(ISAND,ILAYER,IPOIN) = T2%R(IPOIN)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
!
! UPDATING THE MASSES
!
        DO IPOIN = 1,NPOIN
          IF(NMUD.GE.1) THEN
            DO IMUD = 1,NMUD
              MASS_MUD(IMUD,ILAYER,IPOIN) =
     &             MASS_MUD(IMUD,ILAYER,IPOIN)
     &             +FLUX_MASS_MUD(IMUD,ILAYER,IPOIN)
            ENDDO
          ENDIF
          IF(NSAND.GE.1) THEN
            DO ISAND = 1,NSAND
              MASS_SAND(ISAND,ILAYER,IPOIN) =
     &             MASS_SAND(ISAND,ILAYER,IPOIN)
     &             +FLUX_MASS_SAND(ISAND,ILAYER,IPOIN)
            ENDDO
          ENDIF
        ENDDO
!
        IF(VSMTYPE==0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*)'BED1_UPDATE'
          CALL BED1_UPDATE(ZR,ZF,VOLU2D)
          IF(DEBUG.GT.0) WRITE(LU,*)'END BED1_UPDATE'
        ELSEIF(VSMTYPE==1) THEN
! CVSM
!       !==== conversion of mass to thickness and fraction =============!
!       EVCL_MB  and  MASS_SAND   given in [ kg/m**2 ]
          DO ICLA = 1,NSICLA  ! convert for layer-1 for each sand class,
            DO I = 1,NPOIN
              ZFCL_C%ADR(ICLA)%P%R(I)                              !>  mass evolution to thickness evolution
     &           = EVCL_MB%ADR(ICLA)%P%R(I) * MPA2T(ICLA)        !   thickness evolution
              IF(ES(I,1).GT.0.D0) THEN
                AVAIL(I,1,ICLA)                                      !>  for layer-1
     &           = MASS_SAND(ICLA,1,I) * MPA2T(ICLA) / ES(I,1)   !   mass to fraction
              ELSE
                AVAIL(I,1,ICLA) = 0.D0
              ENDIF
              ZF%R(I) = ZF%R(I)+ZFCL_C%ADR(ICLA)%P%R(I)  ! new ZF
            ENDDO
          ENDDO
          IF(DEBUG.GT.0) WRITE(LU,*)'CVSP_MAIN_GAIA'
          CALL CVSP_MAIN_GAIA(ZFCL_C,ZF,NSICLA,NPOIN)
          IF(DEBUG.GT.0) WRITE(LU,*)'CVSP_MAIN_GAIA'
!

!       !==== conversion of thickness to mass  ========================!
        DO ICLA = 1,NSICLA  ! convert for each class thickness evolution to mass evolution
          DO I = 1,NPOIN
            EVCL_MB%ADR(ICLA)%P%R(I)
     &         =  ZFCL_C%ADR(ICLA)%P%R(I) / MPA2T(ICLA)
            MASS_SAND(ICLA,1,I) = AVAIL(I,1,ICLA) / MPA2T(ICLA)*ES(I,1)
          ENDDO
        ENDDO

        ENDIF !VSMTYPE

      ENDDO ! ENDLOOP ON NOMBLAY
!-----------------------------------------------------------------------
!
100   CONTINUE
!
!----------------------------------------------------------------------
!    TOTAL MASS OF SEDIMENT IN EACH POINT AFTER SLIDING
!    JUST FOR CHECK THAT EVOLUTION OF MASS = 0
!    EVOLUTIONS FOR EACH CLASS ARE ADDED: TOTAL MASS EVOLUTION
!    EVOL_MM%R() CONTAINED -MASS OF SEDIMENT BEFORE SLIDING
!    WE ADD MASS OF SEDIMENT AFTER SLIDING TO HAVE EVOLUTION
!>@todo can be removed if it works
      IF(NSAND.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO ISAND = 1,NSAND
              EVOL_MM%R(IPOIN) = EVOL_MM%R(IPOIN)
     &        + MASS_SAND(ISAND,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF(NMUD.GE.1) THEN
        DO IPOIN = 1,NPOIN
          DO ILAYER = 1,NOMBLAY
            DO IMUD = 1,NMUD
              EVOL_MM%R(IPOIN) =EVOL_MM%R(IPOIN)
     &        + MASS_MUD(IMUD,ILAYER,IPOIN)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END
