!                     **************************
                      SUBROUTINE NOMVAR3D_KHIONE
!                     **************************
!
     &(TEXTE,TEXTPR,MNEMO)
!
!***********************************************************************
! KHIONE
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (IN TEXTPR).
!+
!+                TEXTE AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO        |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| TEXTE        |<--| SEE ABOVE
!| TEXTPR       |<--| SEE ABOVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY: NC_FRA, SALINITY, THERMAL_BUDGET,
     &                               DYN_ICOVER
      USE INTERFACE_KHIONE, EX_NOMVAR3D_KHIONE => NOMVAR3D_KHIONE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2) CHAR2
      INTEGER I,ILAST,INEXT
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.LNG_EN) THEN
!
        TEXTE (1 ) = 'PARTICLES NUMBER                '
        TEXTE (2 ) = 'TOTAL CONCENTRATION OF FRAZIL   '
        TEXTPR(1 ) = 'PARTICLES NUMBER                '
        TEXTPR(2 ) = 'TOTAL CONCENTRATION OF FRAZIL   '
        TEXTE (3 ) = 'ELEVATION Z                     '
        TEXTPR(3 ) = 'ELEVATION Z                     '

      ELSE
!
        TEXTE (1 ) = 'PARTICLES NUMBER                '
        TEXTE (2 ) = 'TOTAL CONCENTRATION OF FRAZIL   '
        TEXTPR(1 ) = 'PARTICLES NUMBER                '
        TEXTPR(2 ) = 'TOTAL CONCENTRATION OF FRAZIL   '
        TEXTE (3 ) = 'ELEVATION Z                     '
        TEXTPR(3 ) = 'ELEVATION Z                     '

      ENDIF
!
!     TOTAL NUMBER OF PARTICLES
      MNEMO(1 )  = 'NTOT    '
!     TOTAL CONCENTRATION
      MNEMO(2 )  = 'CTOT    '
!     TOTAL CONCENTRATION
      MNEMO(3 )  = 'Z       '
!
      ILAST = 3
!
!     FRAZIL CONCENTRATIONS
!
      IF(THERMAL_BUDGET) THEN
        IF(NC_FRA.EQ.1) THEN
          TEXTE(ILAST+1) = 'FRAZIL       '
          TEXTPR(ILAST+1) = 'FRAZIL       '
          MNEMO(ILAST+1) = 'F       '
          TEXTE(ILAST+2) = 'NB PARTICLE  '
          TEXTPR(ILAST+2) = 'NB PARTICLE  '
          MNEMO(ILAST+2) = 'N       '
        ELSE
          DO I=1,NC_FRA
            WRITE(CHAR2,'(I2)') I
            TEXTE(ILAST+I)  = 'FRAZIL CLASS '
     &                        //ADJUSTL(CHAR2)//''
            TEXTPR(ILAST+I) = 'FRAZIL CLASS '
     &                        //ADJUSTL(CHAR2)//''
            MNEMO(ILAST+I)  = 'F'//ADJUSTL(CHAR2)//'   '
            TEXTE(ILAST+I+NC_FRA)='NB PARTICLE '
     &                            //ADJUSTL(CHAR2)//' '
            TEXTPR(ILAST+I+NC_FRA)  = 'NB PARTICLE '
     &                                //ADJUSTL(CHAR2)//' '
            MNEMO(ILAST+I+NC_FRA)  = 'N'//ADJUSTL(CHAR2)//'   '
          ENDDO
        ENDIF
        TEXTE(ILAST+2*NC_FRA+1) = 'TEMPERATURE        '
        TEXTPR(ILAST+2*NC_FRA+1) = 'TEMPERATURE        '
        MNEMO(ILAST+2*NC_FRA+1) = 'TEMP    '
        IF(SALINITY) THEN
          TEXTE(ILAST+2*NC_FRA+2) = 'SALINITY        '
          TEXTPR(ILAST+2*NC_FRA+2) = 'SALINITY        '
          MNEMO(ILAST+2*NC_FRA+2) = 'SAL     '
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
