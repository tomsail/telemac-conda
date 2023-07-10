MODULE M_PERSAR_I_D
  INTERFACE
      SUBROUTINE PERSAR_D(z, zd, q, qd, x, zref, cf1, cf1d, cf2, cf2d, &
&       pcsing, pcsingd, idt, xdt, profil, profilplan, f1, qinjec, &
&       connect, singularite, extremite, modelelit, &
&       confluent, abaque, impression, unitelisting, temps, algorithme, &
&       loifrottement, pertechargeconfluent, cqmv, decentrement, erreur)
        USE M_PRECISION
        USE M_MESSAGE_C
        USE M_CONFLUENT_T
        USE M_CONNECT_T
        USE M_EXTREMITE_T
        USE M_PROFIL_T
        USE M_PROFIL_PLAN_T
        USE M_ERREUR_T
        USE M_SINGULARITE_T
        USE M_CALC_PC_CONFLU_I_D
        USE M_PERMAT_I_D
        USE M_QNODE_I_D
        USE M_QREPAR_I_D
        USE M_TRAITER_ERREUR_I
        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: z
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: zd
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: q
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: qd
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1d
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2d
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsing
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsingd
        INTEGER, DIMENSION(:), INTENT(IN) :: idt
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
        TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
        TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
        DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: f1
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: qinjec
        TYPE(CONNECT_T), INTENT(IN) :: connect
        TYPE(SINGULARITE_T), DIMENSION(:), INTENT(IN) :: singularite
        TYPE(EXTREMITE_T), DIMENSION(:), INTENT(IN) :: extremite
        INTEGER, INTENT(IN) :: modelelit
        TYPE(CONFLUENT_T), DIMENSION(:), INTENT(IN) :: confluent
        DOUBLE PRECISION, DIMENSION(:, :, :), INTENT(IN) :: abaque
        LOGICAL, INTENT(IN) :: impression
        INTEGER, INTENT(IN) :: unitelisting
        DOUBLE PRECISION, INTENT(IN) :: temps
        INTEGER, DIMENSION(:), INTENT(IN) :: algorithme
        INTEGER, INTENT(IN) :: loifrottement
        INTEGER, INTENT(IN) :: cqmv
        LOGICAL, INTENT(IN) :: pertechargeconfluent
        LOGICAL, INTENT(IN) :: decentrement
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE PERSAR_D
  END INTERFACE
END MODULE M_PERSAR_I_D

