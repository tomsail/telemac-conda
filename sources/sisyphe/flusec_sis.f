!                   *********************
                    SUBROUTINE FLUSEC_SIS
!                   *********************
!
     &(GLOSEG,DIMGLO,DT,MESH,FLODEL,ICLA,DOPLOT)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief  COMPUTES FLUXES OVER LINES (FLUXLINES/CONTROL SECTIONS) VIA
!+      FLODEL
!+
!+      THE FLUXES OF THE SEGMENTS ARE ALLREADY COMPUTED IN THE POSITIVE
!+      DEPTHS ROUTINE (BIEF)
!+
!+      IN A FIRST STEP WE SEARCH AND SAVE ALL NECESSARY SEGMENTS
!+      (ONE NODE IS ON THE LEFT SIDE , THE OTHER ON THE RIGHT SIDE OF THE
!+      FLUXLINE.
!+
!+      DURING LATER CALLS WE SUMM UP THE FLUXES FOR EACH SEGMENT AND USE
!+      FLUXPR_TELEMAC2D TO WRITE OUT THE FLUXES
!+
!
!history  L. STADLER (BAW)
!+        15/03/2016
!+        V7P2
!+   New way of computing discharges through control sections.
!+   First version.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME_FLUSEC2 STEP
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| FLODEL         |<--| FLUXES BETWEEN POINTS (PER SEGMENT)
!| GLOSEG         |-->| GLOBAL NUMBERS OF APICES OF SEGMENTS
!| ICLA           |-->| SEDIMENT CLASS
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| MESH           |-->| MESH STRUCTURE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_SISYPHE, ONLY: SIS_FILES, SISFLX,
     &                                FLUXLINEDATA_FLUSEC2,
     &                                DEJA_FLUSEC2,VOLFLUX_FLUSEC2,
     &                                FLUX_FLUSEC2,
     &                                NUMBEROFLINES_FLUSEC2,
     &                                TIME_FLUSEC2
!
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ICLA
      INTEGER, INTENT(IN)    :: DIMGLO
      INTEGER, INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN) :: DT
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN) :: FLODEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VOLFLUX_FLUSEC2: CUMULATED VOLUME THROUGH SECTIONS
!     FLUX_FLUSEC2: FLUX_FLUSEC2 THROUGH CONTROL SECTIONS
!
      INTEGER, PARAMETER :: MAXEDGES=500
!
      INTEGER IERR
      LOGICAL DOPLOT
!
      INTEGER I,INP
      INTEGER ISEC
      INTEGER MYPOS
      INTEGER MAXNUMBEROFCLASSES
!
      DOUBLE PRECISION, DIMENSION (2)   :: SEG1,SEG2
      DOUBLE PRECISION :: SEGMENTFLUX
      DOUBLE PRECISION                  :: SIGN1,SIGN2
!
      DOUBLE PRECISION :: SUMFLUX, SUMVOLFLUX
!
      DOUBLE PRECISION :: SEGXMIN,SEGXMAX
      DOUBLE PRECISION :: SEGYMIN,SEGYMAX
      DOUBLE PRECISION,ALLOCATABLE :: FLUXLINES (:,:)
!
!----------------------------------------------------------------------
!
!     PART I
!
!     SEARCH AND SAVE SEGMENTS (FIRST RUN ONLY)
!
!----------------------------------------------------------------------
!
      IF(.NOT.DEJA_FLUSEC2) THEN
!
        INP=SIS_FILES(SISFLX)%LU
        MAXNUMBEROFCLASSES = 20
        TIME_FLUSEC2 = 0.0D0
!
!------- OPEN FLUXLINE FILE
!
        READ(INP,*) NUMBEROFLINES_FLUSEC2
!       ALLOCATE THE FLUXLINES
        IF (.NOT.ALLOCATED(FLUXLINES)) THEN
          ALLOCATE (FLUXLINES(NUMBEROFLINES_FLUSEC2,9), STAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(LU,*)'FLUSEC_SIS: ERROR OF ALLOCATION:',IERR
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!       READ NODES INTO FLUXLINE
        DO I = 1,NUMBEROFLINES_FLUSEC2
          READ(INP,*) ( FLUXLINES(I,ISEC), ISEC=1,9 )
        ENDDO
!
        WRITE(LU,*) "FLUXLINES FOUND ",NUMBEROFLINES_FLUSEC2,
     &              "CLASSES",ICLA
!
!------- DYNAMIC ALLOCATION OF FLUX_FLUSEC2, VOLFLUX_FLUSEC2,...
!
        ALLOCATE(FLUX_FLUSEC2(NUMBEROFLINES_FLUSEC2,
     &                        MAXNUMBEROFCLASSES),STAT=IERR)
        ALLOCATE(VOLFLUX_FLUSEC2(NUMBEROFLINES_FLUSEC2,
     &                        MAXNUMBEROFCLASSES),STAT=IERR)
        ALLOCATE(FLUXLINEDATA_FLUSEC2(NUMBEROFLINES_FLUSEC2),STAT=IERR)
        DO I = 1,NUMBEROFLINES_FLUSEC2
          ALLOCATE(FLUXLINEDATA_FLUSEC2(I)%SECTIONIDS(MAXEDGES),
     &                        STAT=IERR)
          ALLOCATE(FLUXLINEDATA_FLUSEC2(I)%DIRECTION(MAXEDGES),
     &                        STAT=IERR)
        ENDDO
!
        IF(IERR.NE.0) THEN
          WRITE(LU,200) IERR
200       FORMAT(1X,'FLUSEC_SIS: ERROR DURING ALLOCATION
     &              OF MEMORY: ',/,1X,'ERROR CODE: ',1I6)
        ENDIF
!
!------ CLEANUP
!
        DO ISEC =1,NUMBEROFLINES_FLUSEC2
          DO I = 1,MAXNUMBEROFCLASSES
            VOLFLUX_FLUSEC2(ISEC,I) = 0.0D0
          ENDDO
          FLUXLINEDATA_FLUSEC2(ISEC)%NOFSECTIONS = 0
        ENDDO
!
!-------LOOP OVER ALL MESH SEGMENTS TO STORE THEM FOR EACH FLUXLINE
!
        DO I = 1,MESH%NSEG
!
          SEG1(1) = MESH%X%R(GLOSEG(I,1))
          SEG1(2) = MESH%Y%R(GLOSEG(I,1))
          SEG2(1) = MESH%X%R(GLOSEG(I,2))
          SEG2(2) = MESH%Y%R(GLOSEG(I,2))
!         LOOP OVER ALL FLUXLINES
          DO ISEC =1,NUMBEROFLINES_FLUSEC2
!
!----------------------------------------------------------
!
! SIGN IS USED TO LOOK ON WHICH SIDE OF THE LINE A NODE IS
!
!  - SIGN IS NEGATIVE IF WE ARE ON THE RIGHT SIDE
!  - SIGN IS POSITIVE IF WE ARE ON THE LEFT SIDE
!  - SIGN IS ZERO IF WE ARE ON A POINT
!
!---------------------------------------------------------
!
            SIGN1 = (SEG1(1) - FLUXLINES(ISEC,3))*
     &              (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4)) -
     &              (SEG1(2) - FLUXLINES(ISEC,4)) *
     &              (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))

            SIGN2 = (SEG2(1) - FLUXLINES(ISEC,3))*
     &              (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4)) -
     &              (SEG2(2) - FLUXLINES(ISEC,4)) *
     &              (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
!
!---------------------------------------------------------
!
! THE FLUXLINE SHOULD NEVER CROSS A NODE (BE ZERO)
! IF THIS HAPPENS WE SHIFT THE NODE (RIGHT AND UPWARDS)
!
!---------------------------------------------------------
!
            IF(SIGN1.EQ.0.D0) THEN
              SIGN1 = (SEG1(1)+0.001D0 - FLUXLINES(ISEC,3)) *
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG1(2)+0.001D0 - FLUXLINES(ISEC,4)) *
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
!
            IF(SIGN2.EQ.0.D0) THEN
              SIGN2 = (SEG2(1)+0.001D0 - FLUXLINES(ISEC,3)) *
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG2(2)+0.001D0 - FLUXLINES(ISEC,4)) *
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
!           ADD THE SEGMENT ID TO THE NODES
            IF(SIGN1*SIGN2.LT.0.D0) THEN

              SEGXMIN = MIN(SEG1(1),SEG2(1))
              SEGXMAX = MAX(SEG1(1),SEG2(1))
              SEGYMIN = MIN(SEG1(2),SEG2(2))
              SEGYMAX = MAX(SEG1(2),SEG2(2))
!
              IF((SEGXMIN > FLUXLINES(ISEC,5).AND.(SEGXMAX <
     &            FLUXLINES(ISEC,7))).AND.
     &           (SEGYMIN > FLUXLINES(ISEC,6)).AND.(SEGYMAX <
     &              FLUXLINES(ISEC,8))) THEN
!
                    MYPOS = FLUXLINEDATA_FLUSEC2(ISEC)%NOFSECTIONS + 1
                    IF(MYPOS.EQ.MAXEDGES) THEN
                      WRITE(LU,53)
53                    FORMAT(/,1X,'SISYPHE IS STOPPED : ',/
     &                ,1X,' REACHED MAXIMUM LIMIT OF EDGES')
                      CALL PLANTE(1)
                      STOP
                    ENDIF
!
                    FLUXLINEDATA_FLUSEC2(ISEC)%SECTIONIDS(MYPOS) = I
                    IF(SIGN1.GT.0.D0) THEN
                      FLUXLINEDATA_FLUSEC2(ISEC)%DIRECTION(MYPOS) = 1
                    ELSE
                      FLUXLINEDATA_FLUSEC2(ISEC)%DIRECTION(MYPOS) = -1
                    ENDIF
                    FLUXLINEDATA_FLUSEC2(ISEC)%NOFSECTIONS = MYPOS
                ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!     END SEARCH SEGEMENT (DEJA_FLUSEC2)
      DEJA_FLUSEC2 = .TRUE.
!
!----------------------------------------------------------------------
!
!     PART II
!
!     ADD THE FLUXES (FLODEL FROM POSITIVE DEPTHS) FOR SEGMENTS
!
!     TODO WE SHOULD THINK ABOUT HOW WE CAN HANDLE THIS IN THE PARALLEL
!          CASE! IF A SEGMENT IS SHARED WE NEED THE HALF FLUX_FLUSEC2?
!
!----------------------------------------------------------------------
!
!     ONLY INCREASE THE TIME_FLUSEC2 SORRY, THIS INCLUDES THE MORPHOLOGIC FACTOR!
      IF(ICLA.EQ.1)THEN
        TIME_FLUSEC2 = TIME_FLUSEC2 + DT
      ENDIF
!     LOOP OVER ALL FLUXLINES
      DO ISEC =1,NUMBEROFLINES_FLUSEC2
!       ICLA ARE THE CLASSES
        FLUX_FLUSEC2(ISEC,ICLA) = 0.0D0
!       LOOP OVER SEGMENT
        DO I = 1,FLUXLINEDATA_FLUSEC2(ISEC)%NOFSECTIONS
          SEGMENTFLUX = FLUXLINEDATA_FLUSEC2(ISEC)%DIRECTION(I) *
     &                FLODEL%R(FLUXLINEDATA_FLUSEC2(ISEC)%SECTIONIDS(I))
          FLUX_FLUSEC2(ISEC,ICLA) = FLUX_FLUSEC2(ISEC,ICLA)
     &                              + SEGMENTFLUX
          VOLFLUX_FLUSEC2(ISEC,ICLA) = VOLFLUX_FLUSEC2(ISEC,ICLA)
     &                                 + (SEGMENTFLUX*DT)
        ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!
!     PART IIb
!
!     SCRIPTING AREA ;-)
!     ADD A MASSBOX IF YOU LIKE (ADVANCED USERS ONLY) !
!     WARNING THIS PART IS NOT PARALLEL
!----------------------------------------------------------------------
!      IF (DOPLOT) THEN
!        MASS = 0.0D0
!        MASSTOTAL = 0.0D0
!        DO I=1,NPOIN
!            POINT(1) = MESH%X%R(I)
!            POINT(2) = MESH%Y%R(I)
!            IF (POINT(2).GE.31.2) THEN
!               MASS = MASS + HZ%R(I) * (1.0D0/ UNSV2D%R(I))
!            ENDIF
!            MASSTOTAL = MASSTOTAL + HZ%R(I) * (1.0D0/ UNSV2D%R(I))
!        ENDDO
!
!        IF(NCSIZE.GT.1) THEN
!            SUMBOX = P_DSUM(MASS)
!            SUMGLOBAL = P_DSUM(MASSTOTAL)
!            WRITE(6,FMT=1001) "FLUXLINE_MASSTOTAL",SUMGLOBAL,TIME_FLUSEC2,DT
!            WRITE(6,FMT=1001) "FLUXLINE_MASSBOX  ",SUMBOX,TIME_FLUSEC2,DT
!        ELSE
!            WRITE(6,FMT=1001) "FLUXLINE_MASSTOTAL",MASSTOTAL,TIME_FLUSEC2,DT
!            WRITE(6,FMT=1001) "FLUXLINE_MASSBOX  ",MASS,TIME_FLUSEC2,DT
!        ENDIF
!
!1001  FORMAT (A18,ES22.14,ES22.14,ES22.14)
!      ENDIF
!----------------------------------------------------------------------
!
!     PART III
!
!     SEND THE RESULTS TO FLUXPR_TELEMAC2D
!
!----------------------------------------------------------------------
!
      IF(DOPLOT) THEN
        IF(NCSIZE.GT.1) THEN
!         PARALLEL CASE
!         PREPARE SINGLE DATA FOR SENDING
          DO I=1,NUMBEROFLINES_FLUSEC2
            SUMFLUX = P_DSUM(FLUX_FLUSEC2(I,ICLA))
            SUMVOLFLUX = P_DSUM(VOLFLUX_FLUSEC2(I,ICLA))
            WRITE(LU,FMT=1000) 'FLUXLINE',I,'MYCLASS',ICLA,
     &      SUMFLUX,SUMVOLFLUX,TIME_FLUSEC2,DT
          ENDDO
        ELSE
!         SERIAL CASE
          DO I=1,NUMBEROFLINES_FLUSEC2
            WRITE(LU,FMT=1000) 'FLUXLINE',I,'MYCLASS',ICLA,
     &      FLUX_FLUSEC2(I,ICLA),VOLFLUX_FLUSEC2(I,ICLA),TIME_FLUSEC2,DT
          ENDDO
        ENDIF
      ENDIF
!
1000  FORMAT (A8,' ',I2,' ',A7,' ',I2,' ',ES22.14,ES22.14,ES22.14,
     &        ES22.14)
!
!----------------------------------------------------------------------
!
      RETURN
      END
