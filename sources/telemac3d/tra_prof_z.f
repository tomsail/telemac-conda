!                   ************************************
                    DOUBLE PRECISION FUNCTION TRA_PROF_Z
!                   ************************************
!
     &( I , IPOIN2 , IPLAN , IOPT , ITRAC )
!
!***********************************************************************
! TELEMAC3D   V8P4
!***********************************************************************
!
!brief    GIVES THE VERTICAL PROFILE FOR TRACERS.
!
!history  J-M HERVOUET (LNHE)
!+        12/09/2007
!+        V5P8
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/06/2015
!+        V5P8
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY
!| IOPT           |-->| OPTION : 0 : USER DEFINED
!|                |   | 2 : ROUSE PROFILE FOR SEDIMENT
!|                |   | 3 : MODIFIED ROUSE PROFILE (VISCOSITY)
!| IPLAN          |-->| PLAN NUMBER
!| IPOIN2         |-->| 2D GLOBAL NUMBER OF POINT CONSIDERED
!| ITRAC          |-->| TRACER NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA, ONLY: NSUSP_TEL, TYPE_SED, CSTAEQ, XWC0,
     &                             NUM_ISUSP_ICLA
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: I,IPOIN2,IPLAN,IOPT
      INTEGER          , INTENT(IN) :: ITRAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION USTAR,HH,DELTAZ,ROUSE
      DOUBLE PRECISION CMEAN,B,BROUSE,ZREFE
      INTEGER IPOIN3
!!    FIXME SET ZREFE TO 2*DMOY
!
!-----------------------------------------------------------------------
!
!
!     NOT SEDIMENT: SO FAR PROFILE = 1.D0
      TRA_PROF_Z = 1.D0
!
      IF(ITRAC.GE.IND_SED.AND.ITRAC.LE.(IND_SED+NSUSP_TEL-1)) THEN
!       THIS TRACER IS A SEDIMENT TRACER
        ZREFE = 0.01D0
        ISUSP=ITRAC-IND_SED+1
        IF(TYPE_SED(NUM_ISUSP_ICLA(ISUSP)).EQ.'NCO') THEN

          IF(IOPT.EQ.0) THEN
!
!         USER DEFINED
          CALL USER_TRA_PROF_Z
     &    ( TRA_PROF_Z, I , IPOIN2 , IPLAN , IOPT , ITRAC )
!
          ELSEIF(IOPT.EQ.2) THEN
!
!           HERE VALID ONLY FOR SEDIMENT : ROUSE PROFILE
!               divide by cref
            IF(IPLAN.EQ.1) THEN
              TRA_PROF_Z = 1.D0
            ELSE
              IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
              USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
              ROUSE= XWC0(NUM_ISUSP_ICLA(ISUSP))*PRANDTL/KARMAN/USTAR
!             ZREFE=KSPRATIO*DMOY%R(IPOIN2)
              HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &               -MESH3D%Z%R(IPOIN2)  , 1.D-4)
              DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
!             bug correction CV
              TRA_PROF_Z=(ZREFE/(HH-ZREFE)*(HH-DELTAZ)/DELTAZ)**ROUSE
            ENDIF
            TRA_PROF_Z=CSTAEQ%R(IPOIN2)*TRA_PROF_Z
!
          ELSEIF(IOPT.EQ.3) THEN
!
!           Normalised Rouse concentration profile
!           CV 14/01/2014
!
!           HERE VALID ONLY FOR SEDIMENT : ROUSE PROFILE
            HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &             -MESH3D%Z%R(IPOIN2)  , 1.D-4)
            USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
            ROUSE= XWC0(NUM_ISUSP_ICLA(ISUSP))*PRANDTL/KARMAN/USTAR
            B=ZREFE/HH
!           CMEAN : Mean value (cf Sisyphe User Manual,
!                   subroutine suspension_Rouse)
            IF(ABS(ROUSE-1.D0).LE.1.D-04)THEN
              CMEAN= -LOG(B)
            ELSE
              BROUSE=MAX(B,1.D-04)**(ROUSE-1.D0)
              CMEAN= 1.D0/(ROUSE-1.D0)*(1.D0-BROUSE)
            ENDIF
            IF(IPLAN.EQ.1) THEN
              TRA_PROF_Z= 1.D0/MAX(CMEAN,1.D-08)
            ELSE
              IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
              DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
              TRA_PROF_Z= (HH/DELTAZ)**ROUSE/MAX(CMEAN,1.D-08)
            ENDIF
!
          ELSEIF(IOPT.EQ.4) THEN
!
!           Modified Rouse profile with eddy viscosity accounted for
!
!           HERE VALID ONLY FOR SEDIMENT :
!           MODIFIED ROUSE PROFILE FOR LAMINAR VISCOSITY
            IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
            USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
!            ROUSE=-WCHU%R(IPOIN2)/KARMAN/USTAR
            ROUSE= WCHU%R(IPOIN2)/KARMAN/USTAR
            HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &             -MESH3D%Z%R(IPOIN2)      ,1.D-4)
            DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
            TRA_PROF_Z=((HH-DELTAZ)/(DELTAZ+DNUTAV(ITRAC)/
     &                                           KARMAN/USTAR))**ROUSE
            TRA_PROF_Z= CSTAEQ%R(IPOIN2)*TRA_PROF_Z
     &       *(DNUTAV(ITRAC)/KARMAN/USTAR/HH)**ROUSE
!
          ELSE
            WRITE(LU,*) 'TRA_PROF_Z: UNKNOWN OPTION FOR THE PROFILE'
            WRITE(LU,*) 'IOPT=',IOPT,' 0 AND 2 ONLY ARE POSSIBLE'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDIF
!
      IF(S3D_SEDI) THEN
        IF(IOPT.EQ.0) THEN
!
          CALL USER_TRA_PROF_Z(TRA_PROF_Z,I,IPOIN2,IPLAN,IOPT,ITRAC)
!
        ELSEIF(IOPT.EQ.2) THEN
!
!         NOT SEDIMENT : SO FAR PROFILE = 1.D0
!
          IF(ITRAC.NE.NTRAC.OR..NOT.S3D_SEDI) THEN
            TRA_PROF_Z=1.D0
          ELSE
!
!         HERE VALID ONLY FOR SEDIMENT : ROUSE PROFILE
!           divide by cref
            IF(IPLAN.EQ.1) THEN
              TRA_PROF_Z= 1.D0
            ELSE
              IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
              USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
              ROUSE= S3D_WCHU0*PRANDTL/KARMAN/USTAR
!             ZREFE=KSPRATIO*DMOY%R(IPOIN2)
              ZREFE=ZREF%R(IPOIN2)
              HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &             -MESH3D%Z%R(IPOIN2)  , 1.D-4)
              DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
!             bug correction CV
              TRA_PROF_Z=(ZREFE/(HH-ZREFE)*(HH-DELTAZ)/DELTAZ)**ROUSE
            ENDIF
            TRA_PROF_Z=S3D_CREF%R(IPOIN2)*TRA_PROF_Z
!
          ENDIF
!
        ELSEIF(IOPT.EQ.3) THEN
!
!         Normalised Rouse concentration profile
!         CV 14/01/2014
!
          IF(ITRAC.NE.NTRAC.OR..NOT.S3D_SEDI) THEN
            TRA_PROF_Z=1.D0
          ELSE
!           HERE VALID ONLY FOR SEDIMENT : ROUSE PROFILE
            HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &           -MESH3D%Z%R(IPOIN2)  , 1.D-4)
            USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
            ROUSE= S3D_WCHU0*PRANDTL/KARMAN/USTAR
            ZREFE=ZREF%R(IPOIN2)
            B=ZREFE/HH
!           CMEAN : Mean value (cf Sisyphe User Manual,
!                   subroutine suspension_Rouse)
            IF(ABS(ROUSE-1.D0).LE.1.D-04)THEN
              CMEAN= -LOG(B)
            ELSE
              BROUSE=MAX(B,1.D-04)**(ROUSE-1.D0)
              CMEAN= 1.D0/(ROUSE-1.D0)*(1.D0-BROUSE)
            ENDIF
            IF(IPLAN.EQ.1) THEN
              TRA_PROF_Z= 1.D0/MAX(CMEAN,1.D-08)
            ELSE
              IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
              DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
              TRA_PROF_Z= (HH/DELTAZ)**ROUSE/MAX(CMEAN,1.D-08)
            ENDIF
          ENDIF
!
        ELSEIF(IOPT.EQ.4) THEN
!
!         Modified Rouse profile with eddy viscosity accounted for
!
          IF(ITRAC.NE.NTRAC.OR..NOT.S3D_SEDI) THEN
            TRA_PROF_Z=1.D0
          ELSE
!           HERE VALID ONLY FOR SEDIMENT :
!           MODIFIED ROUSE PROFILE FOR LAMINAR VISCOSITY
            IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
            USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
!            ROUSE=-S3D_WCHU%R(IPOIN2)/KARMAN/USTAR
            ROUSE= S3D_WCHU%R(IPOIN2)/KARMAN/USTAR
            HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &           -MESH3D%Z%R(IPOIN2)                  , 1.D-4)
            DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
            TRA_PROF_Z=((HH-DELTAZ)/(DELTAZ+DNUTAV(ITRAC)/
     &                                         KARMAN/USTAR))**ROUSE
            TRA_PROF_Z= S3D_CREF%R(IPOIN2)*TRA_PROF_Z
     &     *(DNUTAV(ITRAC)/KARMAN/USTAR/HH)**ROUSE
          ENDIF
!
        ELSE
          WRITE(LU,*) 'TRA_PROF_Z: UNKNOWN OPTION FOR THE PROFILE'
          WRITE(LU,*) 'IOPT=',IOPT,' 0 AND 2 ONLY ARE POSSIBLE'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(.NOT.(ITRAC.GE.IND_SED.AND.ITRAC.LE.(IND_SED+NSUSP_TEL-1))
     &   .AND..NOT.S3D_SEDI) THEN
        IF(IOPT.EQ.0) THEN
!         USER DEFINED
          CALL USER_TRA_PROF_Z(TRA_PROF_Z,I,IPOIN2,IPLAN,IOPT,ITRAC)
        ELSE
          WRITE(LU,*) 'TRA_PROF_Z: UNKNOWN OPTION FOR THE PROFILE'
          WRITE(LU,*) 'IOPT=',IOPT,
     &                ' 0 AND 1 ONLY ARE POSSIBLE IF NO SEDIMENT'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
