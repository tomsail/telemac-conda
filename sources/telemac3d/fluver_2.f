!                   *******************
                    SUBROUTINE FLUVER_2
!                   *******************
!
     &(FLUVER2,UP,VP,WP,GRADZF,VOLU2D,DSSUDT,NPLAN,NPOIN2)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE VERTICAL FLUXES IN THE REAL MESH
!+                WE HAVE FLUVER=FLUVER2+(VOLU-VOLUN)/DT
!+                SEE RELEASE NOTES 6.1
!
!
!history  J-M HERVOUET (LNHE)
!+        21/08/2010
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DSSUDT         |-->| (H-HN)/DT
!| FLUVER2        |<->| RESULT
!| GRADZF         |-->| BOTTOM GRADIENT
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| VOLU2D         |-->| 2D VOLUME
!| UP             |-->| INTERMEDIATE VELOCITY FIELD
!| VP             |-->| INTERMEDIATE VELOCITY FIELD
!| WP             |-->| INTERMEDIATE VELOCITY FIELD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPLAN,NPOIN2
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLUVER2
      TYPE(BIEF_OBJ), INTENT(IN)    :: UP,VP,WP,GRADZF,VOLU2D,DSSUDT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,IPOIN2,ILEVEL,IUPPER
      DOUBLE PRECISION FLUXMID,NPX,NPY,UMID,VMID,WMID
!
!-----------------------------------------------------------------------
!
!     CONTRIBUTION OF BOUNDARY CONDITIONS: BOTTOM (FLUBOT)
!     THIS INITIALISES THE FIRST PLANE
!
      DO IPOIN2=1,NPOIN2
        FLUVER2%R(IPOIN2)=0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     FOR ALL PLANES BUT FREE SURFACE (HENCE ALL LAYERS)
!     COMPUTATION OF INTERMEDIATE FLUX
!     IT WILL BE ADDED TO LOWER PLANE AND SUBTRACTED FROM UPPER PLANE
!
      DO IPLAN=1,NPLAN-1
!
        DO IPOIN2=1,NPOIN2
          ILEVEL=(IPLAN-1)*NPOIN2+IPOIN2
          IUPPER=ILEVEL+NPOIN2
          NPX=0.5D0*(  GRADZF%ADR(1)%P%R(IUPPER)
     &                +GRADZF%ADR(1)%P%R(ILEVEL) )
          NPY=0.5D0*(  GRADZF%ADR(2)%P%R(IUPPER)
     &                +GRADZF%ADR(2)%P%R(ILEVEL) )
          UMID=0.5D0*(UP%R(IUPPER)+UP%R(ILEVEL))
          VMID=0.5D0*(VP%R(IUPPER)+VP%R(ILEVEL))
          WMID=0.5D0*(WP%R(IUPPER)+WP%R(ILEVEL))
          FLUXMID=VOLU2D%R(IPOIN2) * (-UMID*NPX-VMID*NPY+WMID)
!
!         FLUXMID IS DOWN FOR UPPER LEVEL (NOT YET INITIALISED)
          FLUVER2%R(IUPPER)=                 +FLUXMID
!         FLUXMID IS UP FOR LEVEL (ALREADY INITIALISED)
          FLUVER2%R(ILEVEL)=FLUVER2%R(ILEVEL)-FLUXMID
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CONTRIBUTION OF BOUNDARY CONDITIONS: FREE SURFACE (FLUSUR)
!
!     FLUSUR
!
      DO IPOIN2=1,NPOIN2
        ILEVEL=(NPLAN-1)*NPOIN2+IPOIN2
        FLUVER2%R(ILEVEL)=FLUVER2%R(ILEVEL)
     &                   -VOLU2D%R(IPOIN2)*DSSUDT%R(IPOIN2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
