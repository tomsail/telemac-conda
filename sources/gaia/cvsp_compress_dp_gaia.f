!                     ***************************
                      SUBROUTINE CVSP_COMPRESS_DP_GAIA
!                     ***************************
!
     &(J, THRESHOLD)
!
!***********************************************************************
! GAIA   V8P1                                   14/03/2013
!***********************************************************************
!
!>@brief   COMPRESSES A VERTICAL SORTING PROFILE IN POINT J TO PREVENT
!!        EXTENSIV GROTH OF SECTION / NODE NUMBERS
!!         WITH A DOUGLAS PEUKER LIKE ALGORITHM
!!         THE ALGORITHM IS MODIFIED: INSTEAD FROM "POINT TO LINE DISTANCES"
!!          TO THE "SUM OF FRACTION ERRORS" !
!
!>@history UWE MERKEL
!!        20/07/2011
!!        V6P2
!
!>@history  P. A. TASSI (EDF R&D, LNHE)
!!        12/03/2013
!!        V6P3
!!   Cleaning, cosmetic
!
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] J Index of a point in mesh
!>@param[in] THRESHOLD Share of a fraction we are willing to round of
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA_BEDLOAD, EX => CVSP_COMPRESS_DP_GAIA
      USE DECLARATIONS_GAIA
!
      USE CVSP_OUTPUTFILES_GAIA, ONLY: CP

      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)    :: J
      DOUBLE PRECISION,  INTENT(IN)    :: THRESHOLD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K, MARKERMAX, MARKERCNT,  TTT, NNN, JG
      INTEGER MAXPOS, M, MARKERMAXOLD, MARKERMAXVERYOLD
      INTEGER MARKER(PRO_MAX_MAX), MARKERTEMP(PRO_MAX_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     USED TO MARK NODES THAT WILL BE KEPT
!
      DOUBLE PRECISION LOSS(PRO_MAX_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! STORES THE FRACTION ERRORS THAT WILL OCCURE IF THE POINT IS ELEMINATED FROM CURRENT PROFILE
!
      DOUBLE PRECISION MAXDIST
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! STORES THE MAXIMUM DISTANCE OF ANY NODE IN THE CURRENT LOOP
!
      DOUBLE PRECISION  FI, FJ, FK, DI, DJ, DK, THRESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
! PARALLEL: LOCAL TO GLOBAL
!-----------------------------------------------------------------------
!
      JG = J
      IF (NCSIZE.GT.1) JG = MESH%KNOLG%I(J)
!
!--------------------------------------------------------------------------
! DEBUG: INIT OUTPUT
!-----------------------------------------------------------------------
!
      IF(CVSP_DB_GAIA(JG,0)) CALL CVSP_P_GAIA('./','V_H',JG)

!--------------------------------------------------------------------------
! INIT
!-----------------------------------------------------------------------
!
      IF(PRO_MAX(J) <= 2) RETURN
!
!-----------------------------------------------------------------------
!FIRST AND LAST POINT WILL ALWAY BE KEPT
!-----------------------------------------------------------------------
!
      MARKERMAX = 2             !MAXIMUM USED INDEX IN MARKER ARRAY
      MARKER(1) = 1             !FIRST WILL ALWAYS BE KEPT
      MARKER(2) = PRO_MAX(J)    !LAST  WILL ALWAYS BE KEPT
      MARKERCNT = 1             !MAXIMUM USED INDEX IN MARKERTEMP ARRAY
!
      THRESH = THRESHOLD
!
!-------------------------------------------------------------------------
! EXTEND THRESHOLD IF NECESSARY
!-----------------------------------------------------------------------
!
      DO NNN = 1,1              !4
        IF (NNN > 1) WRITE(LU,*) 'COMPRESS', J, NNN
!
        THRESH = THRESH * (10**(1-NNN))
        MARKERMAXVERYOLD = MARKERMAX
!
!--------------------------------------------------------------------------
! ITERATE UNTIL NOTHING CHANGES ANYMORE
!-----------------------------------------------------------------------
!
        DO TTT = 1, PRO_MAX(J) - 2 ! THEROTICAL MAXIMUM NUMBER OF ITERATIONS
          MARKERMAXOLD = MARKERMAX
          MARKERCNT = 1
          MARKERTEMP(MARKERCNT) = 1
!
!--------------------------------------------------------------------------
! LOOP OVER ALL SECTIONS BETWEEN 2 MARKED NODES
!-----------------------------------------------------------------------
!
          DO I = 1, MARKERMAX-1
            MAXDIST = 0      !INITS THE MAXIMUM FRACTION ERROR
            MAXPOS = -1      !INITS THE NODE WHICH PRODUCES THE MAXIMUM FRACTION ERROR
            IF (MARKER(I+1)-MARKER(I) >= 2 ) THEN
!
!--------------------------------------------------------------------------
! LOOP OVER ALL UNMARKED NODES IN BETWEEN 2 MARKED NODES
!-----------------------------------------------------------------------
!
              DO M = MARKER(I) + 1 , MARKER(I + 1) - 1
!
!--------------------------------------------------------------------------
! HOW MUCH VOLUME=FRACTION IS LOST IF WE ELIMINATE THIS PROFILEPOINT
! USING "VOLUME" !!! ORIGINAL DOUGLAS-PEUKER: DISTANCE TO INTERCONNECTION !!!
! "ERROR TRIANGLE VOLUME" IS CALCULATED BY GAUSSIAN POLYGON FORMULA!
!-----------------------------------------------------------------------
!
                LOSS(M) = 0.D0
!
                DO K = 1, NSICLA
                  IF (NNN.GE.5) THEN
                    FI = PRO_F(J,M-1,K)
                    FJ = PRO_F(J,M+1,K)
                    FK = PRO_F(J,M,K)
                    DI = PRO_D(J,M-1,K)
                    DJ = PRO_D(J,M+1,K)
                    DK = PRO_D(J,M,K)
                  ELSE
                    FI = PRO_F(J,MARKER(I),K)
                    FJ = PRO_F(J,MARKER(I+1),K)
                    FK = PRO_F(J,M,K)
                    DI = PRO_D(J,MARKER(I),K)
                    DJ = PRO_D(J,MARKER(I+1),K)
                    DK = PRO_D(J,M,K)
                  ENDIF
!
                  LOSS(M) = LOSS(M) +
     &                 ABS(0.5D0 * ((FI+FJ) * (DI-DJ) +
     &                 (FJ+FK) * (DJ-DK) +
     &                 (FK+FI) * (DK-DI)))

                ENDDO      !K
!
                IF(LOSS(M).GT.MAXDIST) THEN
                  MAXDIST = LOSS(M)
                  MAXPOS = M
                ENDIF
!
              ENDDO         !M
!
!-----------------------------------------------------------------------
! IF ANY POINT IS TO FAR OUT OF RANGE: ADD IT TO THE MARKER LIST
!-----------------------------------------------------------------------
!
              IF(MAXPOS > -1 .AND. MAXDIST > THRESH) THEN
                MARKERCNT = MARKERCNT + 1
                MARKERTEMP(MARKERCNT) = MAXPOS
              ENDIF

            ENDIF
!
!-----------------------------------------------------------------------
! ADD THE ENDPOINT OF THIS SECTION
!-----------------------------------------------------------------------
!
            MARKERCNT = MARKERCNT + 1
            MARKERTEMP(MARKERCNT) = MARKER(I+1)
!
          ENDDO               !I
!
          DO I = 1, MARKERCNT
            MARKER(I) = MARKERTEMP(I)
          ENDDO
          MARKERMAX = MARKERCNT
!
          IF (MARKERMAX - MARKERMAXOLD == 0 ) EXIT !STOP ITERATION, AS NOTHING CHANGED!
        ENDDO                  ! TTT
        IF (MARKERMAX - MARKERMAXVERYOLD == 0 ) EXIT !STOP ITERATION, AS NOTHING CHANGED!
      ENDDO                     ! NNN
!
!--------------------------------------------------------------------------
! RECREATE THE SORTING PROFILE WITH LESSER NUMBER OF SECTIONS
!-----------------------------------------------------------------------
!
      DO K = 1, NSICLA
        DO I = 1, MARKERMAX
          PRO_F(J,I,K) = PRO_F(J,MARKER(I),K)
          PRO_D(J,I,K) = PRO_D(J,MARKER(I),K)
        ENDDO                  !I
      ENDDO                     !K
!
      PRO_MAX(J) = MARKERMAX
!
!--------------------------------------------------------------------------
! BRUTFORCE COMPRESSION IN CASE OF EXCEPTIONAL FRAGMENTATION
!-----------------------------------------------------------------------
!
      IF(PRO_MAX(J) > PRO_MAX_MAX-4*NSICLA-4) THEN
        IF(CP)
     & WRITE(LU,*) 'CVSP_COMPRESS_DP RESIGNS AND CALLS COMPRESS_BRUT:'
        CALL CVSP_COMPRESS_BRUT_GAIA(J)
      ENDIF
!
!--------------------------------------------------------------------------
! DEBUG: FINAL OUTPUT
!-----------------------------------------------------------------------
!
      IF(CVSP_DB_GAIA(JG,0)) CALL CVSP_P_GAIA('./','V_I',JG)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_COMPRESS_DP_GAIA
