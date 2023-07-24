!                   ********************************
                    SUBROUTINE READ_SECTIONS_SISYPHE
!                   ********************************
!
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    - READS SECTIONS INPUT FILE IN SCALAR AND PARALLEL MODES
!+
!+            - DEFINES THE CONTROL SECTIONS, OR...
!+
!+            - ...RE-DEFINES THE ONES DECLARED PREVIOUSLY IN THE STEERING FILE
!+
!+            - SECTIONS ARE DEFINED BY GLOBAL NODE NUMBERS OR,
!+                BY END POINT COORDINATES (THEN NEAREST NODE FOUND)
!+
!+            - IN PARALLEL MODE, TWO OPTIONS:
!+
!+                 -> TAKES THE "SCALAR" FILE (AS "PREVIOUSLY")
!+
!+                 -> TAKES A PARTITIONED FILE - COMPUTING FLUXES THROUGH SECTIONS
!+                     - CROSSING NUMEROUS MESH PARTITIONS IS POSSIBLE
!+
!+            - MODIFIES CTRLSC AND NCP
!
!history  JAJ PINXIT JACEK.JANKOWSKI@BAW.DE
!+        15/02/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY: NCSIZE
      USE DECLARATIONS_SISYPHE, ONLY: MESH, CHAIN, NCP, CTRLSC,
     &                                SIS_FILES, SISSEC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER :: NSEC, IHOWSEC, I, N, ERR, INP
      DOUBLE PRECISION :: XA, YA, DISTB, DISTE, DMINB, DMINE
!
!-----------------------------------------------------------------------
!
!      WRITE(LU,*) '-> ENTERING READ_SECTIONS_SISYPHE'
      INP=SIS_FILES(SISSEC)%LU
      READ (INP,*) ! THE NECESSARY COMMENT LINE
      READ (INP,*) NSEC, IHOWSEC
      IF (.NOT.ALLOCATED(CHAIN)) THEN
        ALLOCATE (CHAIN(NSEC), STAT=ERR)
        IF (ERR/=0) THEN
          WRITE(LU,*)
     &      'READ_SECTIONS: ERROR BY REALLOCATING CHAIN:',ERR
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
      SELECT CASE (IHOWSEC)
      CASE (:-1) ! SECTION END POINTS PROVIDED AS GLOBAL NODES
        DO N=1,NSEC
          READ (INP,*) CHAIN(N)%DESCR
          READ (INP,*) CHAIN(N)%NPAIR(:)
          IF (NCSIZE>1) THEN
            CHAIN(N)%XYBEG(:)=0.0D0
            CHAIN(N)%XYEND(:)=0.0D0
          ELSE
            CHAIN(N)%XYBEG(:)= (/MESH%X%R(CHAIN(N)%NPAIR(1)),
     &                           MESH%Y%R(CHAIN(N)%NPAIR(1))/)
            CHAIN(N)%XYEND(:)= (/MESH%X%R(CHAIN(N)%NPAIR(2)),
     &                           MESH%Y%R(CHAIN(N)%NPAIR(2))/)
          ENDIF
          CHAIN(N)%NSEG=-1
          NULLIFY(CHAIN(N)%LISTE)
        END DO
!        WRITE(LU,'(A)') ' -> SECTION, TERMINAL COORDINATES:'
!        DO N=1,NSEC
!          WRITE(LU,'(I9,4(1X,1PG13.6))') N,
!     &          CHAIN(N)%XYBEG, CHAIN(N)%XYEND
!        END DO
      CASE (0) ! SECTION END POINTS PROVIDED BY COORDINATES
        DO N=1,NSEC
          READ (INP,*) CHAIN(N)%DESCR
!         AD: AVOID PART-REF WITH NON-ZERO RANK IN MODE T1V
          READ (INP,*)
     &         ( CHAIN(N)%XYBEG(I), I=1, SIZE(CHAIN(N)%XYBEG,1) ),
     &         ( CHAIN(N)%XYEND(I), I=1, SIZE(CHAIN(N)%XYEND,1) )
          CHAIN(N)%NPAIR(:)=0
          CHAIN(N)%NSEG=-1
          NULLIFY(CHAIN(N)%LISTE)
        END DO
        DO N=1,NSEC         ! FIND NEAREST NODES
          XA=MESH%X%R(1)
          YA=MESH%Y%R(1)
          DMINB = SQRT( (CHAIN(N)%XYBEG(1)-XA)**2
     &                + (CHAIN(N)%XYBEG(2)-YA)**2 )
          DMINE = SQRT( (CHAIN(N)%XYEND(1)-XA)**2
     &                + (CHAIN(N)%XYEND(2)-YA)**2 )
          CHAIN(N)%NPAIR(1)=1
          CHAIN(N)%NPAIR(2)=1
          DO I=2,MESH%NPOIN ! COMPUTATIONALLY INTENSIVE
            XA=MESH%X%R(I)
            YA=MESH%Y%R(I)
            DISTB = SQRT( (CHAIN(N)%XYBEG(1)-XA)**2
     &                  + (CHAIN(N)%XYBEG(2)-YA)**2 )
            DISTE = SQRT( (CHAIN(N)%XYEND(1)-XA)**2
     &                 + (CHAIN(N)%XYEND(2)-YA)**2 )
            IF ( DISTB < DMINB ) THEN
              CHAIN(N)%NPAIR(1)=I
              DMINB=DISTB
            ENDIF
            IF ( DISTE < DMINE ) THEN
              CHAIN(N)%NPAIR(2)=I
              DMINE=DISTE
            ENDIF
          END DO
!          WRITE(LU,'(A,3(1X,I9))')
!     &          ' -> SECTION, TERMINAL NODES: ', N, CHAIN(N)%NPAIR(:)
        END DO
      CASE (1:) ! PARTITIONED, INSTEAD OF END POINTS, READY CHAINS PROVIDED
        DO N=1,NSEC
          READ (INP,*) CHAIN(N)%DESCR
          READ (INP,*) CHAIN(N)%NSEG
          IF (CHAIN(N)%NSEG>0) THEN
            ALLOCATE (CHAIN(N)%LISTE(CHAIN(N)%NSEG,2), STAT=ERR)
            IF (ERR/=0) THEN
              WRITE(LU,*) 'READ_SECTIONS: ',
     &         ' ERROR BY REALLOCATING CHAIN(N)%LISTE, N, ERR:',N,ERR
              CALL PLANTE(1)
              STOP
            ENDIF
            DO I=1,CHAIN(N)%NSEG
              READ(INP,*) CHAIN(N)%LISTE(I,:)
              CHAIN(N)%NPAIR=-1 ! HM...
              CHAIN(N)%XYBEG=0.0D0
              CHAIN(N)%XYEND=0.0D0
            END DO
          ELSE
            NULLIFY(CHAIN(N)%LISTE)
          ENDIF
        END DO
      END SELECT
!
!-----------------------------------------------------------------------
!
!      WRITE(LU,*) 'SECTIONS SUMMARY:'
!      WRITE(LU,*) 'NSEC,IHOWSEC: ',NSEC,IHOWSEC
!      SELECT CASE (IHOWSEC)
!      CASE(:0) ! SERIAL CASE, OR "CLASSICAL CASE" IN PARALLEL (DEVEL)
!        DO N=1,NSEC
!          WRITE(LU,*) CHAIN(N)%DESCR
!          WRITE(LU,*) CHAIN(N)%XYBEG(:), CHAIN(N)%XYEND(:)
!          WRITE(LU,*) CHAIN(N)%NPAIR(:)
!        END DO
!      CASE (1:) ! PARTITIONED, READY SEGMENT CHAINS GIVEN
!        DO N=1,NSEC
!          WRITE(LU,*) 'NAME: ', CHAIN(N)%DESCR
!          WRITE(LU,*) 'NSEG: ', CHAIN(N)%NSEG
!          DO I=1,CHAIN(N)%NSEG
!            WRITE(LU,*) CHAIN(N)%LISTE(I,:)
!          END DO
!        END DO
!      END SELECT
!
!-----------------------------------------------------------------------
! TRANSFER TO THE GLOBAL TELEMAC OR SISYPHE VARIABLES
! NCP IS 2 * NUMBER OF SECTIONS
! CTRLSC IS THE LIST OF THE SECTION END NODES
! CTRLSC HAS TO BE RE-ALLOCATED CAREFULLY
!
!      WRITE (LU,*) 'ARRANGING SECTIONS FOR SISYPHE'
!      WRITE (LU,*) 'SISYPHE NCP WAS: ',NCP
      NCP = 2*NSEC
      IF (ALLOCATED(CTRLSC)) THEN
        DEALLOCATE(CTRLSC, STAT=ERR)
        IF (ERR/=0) THEN
          WRITE(LU,*)
     &    'READ_SECTIONS_SISYPHE: ERROR BY DEALLOCATING CTRLSC:',ERR
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
      ALLOCATE (CTRLSC(NCP), STAT=ERR)
      IF (ERR/=0) THEN
        WRITE(LU,*)
     &    'READ_SECTIONS_SISYPHE: ERROR BY REALLOCATING CTRLSC:',ERR
        CALL PLANTE(1)
        STOP
      ENDIF
      I=1
      DO N=1,NSEC
        CTRLSC(I)   = CHAIN(N)%NPAIR(1)
        CTRLSC(I+1) = CHAIN(N)%NPAIR(2)
        I=I+2
      END DO
!      WRITE (LU,*) 'NCP@SISYPHE: ',NCP
!      WRITE (LU,*) 'CTRLSC@SISYPHE: ',CTRLSC
!
!-----------------------------------------------------------------------
!
!      WRITE(LU,*) '-> LEAVING READ_SECTIONS_SISYPHE'
      RETURN
      END SUBROUTINE READ_SECTIONS_SISYPHE
