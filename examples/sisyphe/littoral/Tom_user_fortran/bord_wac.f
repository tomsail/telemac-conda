!                 *******************
                  SUBROUTINE BORD_WAC
!                 *******************
!
     &(F,NDIRE,NF,NPOIN2,IP)
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !    F           ! <->!  DENSITE SPECTRALE                           !
! !    NDIRE       ! -->!  NOMBRE DE DIRECTIONS                        !
! !    NF          ! -->!  NOMBRE DE FREQUENCES                        !
! !    NPOIN2      ! -->!  NOMBRE DE POINTS 2D                         !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : LIMWAC
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
      INTEGER NDIRE,NF,NPOIN2
!
      DOUBLE PRECISION F(NPOIN2,NDIRE,NF)
!
      INTEGER IFF,IPLAN
      INTEGER IP, IMIL, IDRO, IGAU
      DOUBLE PRECISION DUMMY(100,100)
!
!***********************************************************************
!
      IMIL=1117+IP-1
      IF (IMIL.EQ.1156) IMIL=116
      IGAU=180-IP+1
      IDRO= 52+IP-1
!
      IMIL=GLOBAL_TO_LOCAL_POINT(IMIL,MESH)
      IF(IMIL.EQ.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NDIRE
            DUMMY(IPLAN,IFF)=0.D0
          ENDDO
        ENDDO
      ELSE
        DO IFF=1,NF
          DO IPLAN = 1,NDIRE
            DUMMY(IPLAN,IFF)=F(IMIL,IPLAN,IFF)
          ENDDO
        ENDDO
      ENDIF
!
      DO IFF=1,NF
        DO IPLAN=1,NDIRE
          DUMMY(IPLAN,IFF) = P_MAX(DUMMY(IPLAN,IFF))
!     &                    + P_MIN(DUMMY(IPLAN,IFF))
        ENDDO
      ENDDO
!
      IGAU=GLOBAL_TO_LOCAL_POINT(IGAU,MESH)
      IDRO=GLOBAL_TO_LOCAL_POINT(IDRO,MESH)
      IF(IGAU.GT.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NDIRE
            F(IGAU,IPLAN,IFF) = DUMMY(IPLAN,IFF)
          ENDDO
        ENDDO
      ENDIF
      IF(IDRO.GT.0) THEN
        DO IFF=1,NF
          DO IPLAN = 1,NDIRE
            F(IDRO,IPLAN,IFF) = DUMMY(IPLAN,IFF)
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END

