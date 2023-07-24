MODULE M_CALC_PC_CONFLU_I_B
  IMPLICIT NONE

  INTERFACE 
      SUBROUTINE CALC_PC_CONFLU_B(pcsing, pcsingb, z, zb, q, qb, x, zref&
&       , profil, profilplan, confluent, abaque, idt, xdt, connect, &
&       unitelisting, erreur)
!============================== Instructions ================================
! type DOUBLE
        USE M_PRECISION
! EPS3
        USE M_PARAMETRE_C
! Liste des messages d'erreur
        USE M_MESSAGE_C
! type CONNECT_T
        USE M_CONNECT_T
! type CONFLUENT_T
        USE M_CONFLUENT_T
! type ERREUR_T
        USE M_ERREUR_T
! type PROFIL_T
        USE M_PROFIL_T
! type PROFIL_T
        USE M_PROFIL_PLAN_T
! Traitement des erreurs
        USE M_TRAITER_ERREUR_I
! sous-programme RHSBP_SECTION_S
        USE M_RHSBP_S_B
        USE M_INTERPOLATION_S_B
! Arguments
        DOUBLE PRECISION, DIMENSION(:) :: pcsing
        DOUBLE PRECISION, DIMENSION(:) :: pcsingb
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: z
        DOUBLE PRECISION, DIMENSION(:) :: zb
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: q
        DOUBLE PRECISION, DIMENSION(:) :: qb
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
        TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
        TYPE(CONFLUENT_T), DIMENSION(:), INTENT(IN) :: confluent
        DOUBLE PRECISION, DIMENSION(:, :, :), INTENT(IN) :: abaque
        INTEGER, DIMENSION(:), INTENT(IN) :: idt
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
        TYPE(CONNECT_T), INTENT(IN) :: connect
        INTEGER, INTENT(IN) :: unitelisting
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE CALC_PC_CONFLU_B
  END INTERFACE

END MODULE M_CALC_PC_CONFLU_I_B

