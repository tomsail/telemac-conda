!                   ******************************
                    SUBROUTINE CORRECTION_DEPTH_3D
!                   ******************************
!
     &(GLOSEG,DIMGLO)
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    APPLIES VARIOUS TECHNIQUES TO TREAT NEGATIVE DEPTHS.
!
!history  J.M. HERVOUET (LNHE)
!+        28/07/2009
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
!history  J.M. HERVOUET (LNHE)
!+        26/08/2011
!+        V6P2
!+   Call to FLUX_EF_VF_3D changed
!
!history  J.M. HERVOUET (LNHE)
!+        23/02/2016
!+        V7P2
!+   Adapting to new treatment of negative depths = 3, necessary for
!+   scheme ERIA=15.
!
!history  J.M. HERVOUET (LNHE)
!+        15/09/2016
!+        V7P2
!+   Adding dummy RAIN and PLUIE to call of positive_depths.
!+   Retrieving SMH without rain in the case of rain.
!+   Updating array PLUIE when there is a imitation of evaporation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D,
     &                     EX_CORRECTION_DEPTH_3D => CORRECTION_DEPTH_3D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER IOPT, ISEG, ISEG3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16) :: FORMUL
      INTEGER I,OPTPOS
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!-----------------------------------------------------------------------
!
      IF(OPT_HNEG.LT.0.OR.OPT_HNEG.GT.3) THEN
        WRITE(LU,*) 'CORRECTION_DEPTH_3D:'
        WRITE(LU,*) 'TREATMENT OF NEGATIVE DEPTHS'
        WRITE(LU,*) 'MUST BE BETWEEN 0 AND 3'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1) THEN
!
        IF(OPT_HNEG.GE.2) THEN
!
!         FOR THE TIME BEING, FLODEL IS A WORKING ARRAY, HENCE
!         YAFLODEL=.FALSE.; TO USE FLODEL IN TEL4DEL WOULD REQUIRE
!         TRANSFORMING IT INTO 3D.
!
          FORMUL = 'VGRADP 2     HOR'
          SAVEZ     =>MESH3D%Z%R
          MESH3D%Z%R=>ZPROP%R
!
!         HERE T3_01 IS NOT REALLY USED, WE USE THE NON ASSEMBLED
!         FORM MESH3D%W, WHICH IS HERE ALSO W3D, HENCE LEGO=.FALSE.
!
          CALL VECTOR(T3_01,'=',FORMUL,IELM3,-1.D0,DM1,GRAZCO,GRAZCO,
     &                UCONV,VCONV,VCONV,MESH3D,MSK,MASKEL,LEGO=.FALSE.)
!
          MESH3D%Z%R=>SAVEZ
!
!         TODO: CALCULATES 3D FLODEL (NOTE JMH: THIS WILL BE REDONE IN FLUX3D
!                                         OPTIMISATION MAYBE POSSIBLE  )
          IOPT=2
          CALL FLUX_EF_VF_3D(FLODEL%R,MESH2D%W%R,MESH3D%W%R,
     &                       MESH2D%NSEG,MESH2D%NELEM,
     &                       MESH2D%NELMAX,
     &                       MESH2D,.TRUE.,IOPT,1,
     &                       IELM3,NPLAN,MESH3D%IKLE%I,MESH3D%NELMAX,
     &                       MESH3D%KNOLG%I)
!
!         CALCULATES 2D FLODEL (PUT IN FIRST PLANE OF 3D FLODEL)
!
!         IT IS DIFFERENT FROM THE METHOD CONSISTING OF SUMMING THE
!         FLUXES ON THE VERTICAL FIRST AND THEN CALLING FLUX_EF_VF
!
          IF(IELM3.EQ.41) THEN
!
            DO ISEG = 1,MESH2D%NSEG
              DO I = 2,NPLAN
                ISEG3D = ISEG + (I-1)*MESH2D%NSEG
                FLODEL%R(ISEG) = FLODEL%R(ISEG) + FLODEL%R(ISEG3D)
              ENDDO
            ENDDO
!
          ELSEIF(IELM3.EQ.51) THEN
!
!           NOTHING TO DO, THIS WAS DONE IN FLUX_EF_VF_3D
!
          ELSE
            WRITE(LU,*) 'CORRECTION_DEPTH_3D: UNKNOWN ELEMENT:',IELM3
            CALL PLANTE(1)
            STOP
          ENDIF
!
          OPTPOS=2
          IF(OPT_HNEG.EQ.3) OPTPOS=1
!
!         PROVISIONAL!!!! SPLITTING SOURCES AND RAIN FOR POSITIVE DEPTHS
!         BECAUSE POSITIVE AND NEGATIVE SOURCES ARE NOT TREATED AT THE SAME TIME
!         ORIGINAL RAIN WITHOUT *VOLU2D OR *V2DPAR PUT INTO T2_05
!         (EQUIVALENT OF "PLUIE" IN TELEMAC-2D)
!
          IF(RAIN) THEN
            IF(NCSIZE.GT.1) THEN
              CALL OS('X=Y-Z   ',X=SMH,Y=SMH,Z=PARAPLUIE)
              CALL OS('X=YZ    ',X=T2_06,Y=PARAPLUIE,Z=UNSV2D)
            ELSE
              CALL OS('X=Y-Z   ',X=SMH,Y=SMH,Z=PLUIE)
              CALL OS('X=YZ    ',X=T2_06,Y=PLUIE,Z=UNSV2D)
            ENDIF
            CALL POSITIVE_DEPTHS(T2_01,T2_02,T2_03,T2_04,H,HN,
     &                           MESH2D,FLODEL,.FALSE.,
     &                           FLBOR,DT,UNSV2D,NPOIN2,
     &                           GLOSEG(1:DIMGLO,1),
     &                           GLOSEG(1:DIMGLO,2),
     &                           MESH2D%NBOR%I,NPTFR2,
!                                     YASMH PLUIE      OPTSOU
     &                           SMH,.TRUE.,T2_06,RAIN,2,
!                                SMH IN PROJECTED FORM IN T3D
     &                           FLULIM%R,LIHBOR%I,HBOR%R,KENT,INFOGR,
     &                           MESH2D%W%R,NAMECODE,OPTPOS,MAXADV)
!                                                    OPTION FOR POSITIVE DEPTH ALGORITHM
!
!           UPDATING SMH AND PLUIE IN CASE EVAPORATION HAS BEEN LIMITED
!
            CALL OS('X=YZ    ',X=PLUIE,Y=T2_06,Z=VOLU2D)
            IF(NCSIZE.GT.1) THEN
              CALL OS('X=Y     ',X=PARAPLUIE,Y=PLUIE)
              CALL PARCOM(PARAPLUIE,2,MESH2D)
              CALL OS('X=Y-Z   ',X=SMH,Y=SMH,Z=PARAPLUIE)
            ELSE
              CALL OS('X=YZ    ',X=T2_06,Y=PLUIE,Z=UNSV2D)
              CALL OS('X=Y-Z   ',X=SMH,Y=SMH,Z=PLUIE)
            ENDIF
!
          ELSE
            CALL POSITIVE_DEPTHS(T2_01,T2_02,T2_03,T2_04,H,HN,
     &                           MESH2D,FLODEL,.FALSE.,
     &                           FLBOR,DT,UNSV2D,NPOIN2,
     &                           GLOSEG(1:DIMGLO,1),
     &                           GLOSEG(1:DIMGLO,2),
     &                           MESH2D%NBOR%I,NPTFR2,
!                                           PLUIE (NOT INITIALISED BUT NOT USED)
     &                           SMH,.TRUE.,T2_06,RAIN,2,
!                                     YASMH OPTSOU
!                                SMH IN PROJECTED FORM IN T3D
     &                           FLULIM%R,LIHBOR%I,HBOR%R,KENT,INFOGR,
     &                           MESH2D%W%R,NAMECODE,OPTPOS,MAXADV)
!                                                    OPTION FOR POSITIVE DEPTH ALGORITHM
          ENDIF
!
        ELSEIF(OPT_HNEG.EQ.1) THEN
!
!         CONSERVATIVE SMOOTHING OF NEGATIVE DEPTHS
!
!         1) PUTS NEGATIVE VALUES IN T2_01 AND REMOVES THEM FROM H
!
          CALL OS( 'X=-(Y,C)' , X=T2_01 , Y=H     , C=0.D0 )
          CALL OS( 'X=X-Y   ' , X=H     , Y=T2_01 )
!
!         2) SMOOTHES NEGATIVE VALUES (TWO LOOPS HERE)
!            AND MASKS TO NOT AFFECT THE TIDAL FLATS
!
          IF(OPTBAN.EQ.1) THEN
            CALL FILTER(T2_01,.TRUE.,T2_02,T2_03,
     &                  MAT2D%ADR(1)%P,'MATMAS          ',
     &                  1.D0,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                  MESH2D,MSK,MASKEL,2)
          ENDIF
!
!         3) PUTS BACK IN H THE SMOOTHED NEGATIVE VALUES
!
          CALL OS( 'X=X+Y   ' , X=H , Y=T2_01 )
!
        ENDIF
!
      ENDIF
!
! CLIPS H AND COMPUTES Z
!
      IF(OPTBAN.EQ.2) THEN
        CALL CLIP(H,HMIN,.TRUE., 1.D6, .FALSE., 0)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

