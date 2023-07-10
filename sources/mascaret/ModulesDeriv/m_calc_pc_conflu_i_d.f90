MODULE M_CALC_PC_CONFLU_I_D
  INTERFACE 
      SUBROUTINE CALC_PC_CONFLU_D(pcsing, pcsingd, z, zd, q, qd, x, zref&
&       , profil, profilplan, confluent, abaque, idt, xdt, connect, &
&       unitelisting, erreur)
        USE M_PRECISION
        USE M_PARAMETRE_C
        USE M_MESSAGE_C
        USE M_CONNECT_T
        USE M_CONFLUENT_T
        USE M_ERREUR_T
        USE M_PROFIL_T
        USE M_PROFIL_PLAN_T
        USE M_TRAITER_ERREUR_I
        USE M_RHSBP_S_D
        USE M_INTERPOLATION_S_D
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: pcsing
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: pcsingd
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: z
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zd
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: q
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: qd
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
      END SUBROUTINE CALC_PC_CONFLU_D
  END INTERFACE
END MODULE M_CALC_PC_CONFLU_I_D

