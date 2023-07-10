MODULE M_QNODE_I_B
  IMPLICIT NONE

  INTERFACE 
      SUBROUTINE QNODE_B(q, qb, z, zb, numconfluence, numpassage, &
&       connect, erreur)
!
!***********************************************************************
!
!  FONCTION :
!  --------
!
!        REPARTITION DES DEBITS A UN NOEUD AVEC EGALITE DES COTES :
!
!          CAS AVEC UNE SEULE BRANCHE AVAL
!                   ---------
!
!-----------------------------------------------------------------------
!                              ARGUMENTS
! ._______________.____.____._______________________________________________
! !    NOM        !TYPE!MODE!                   ROLE
! !_______________!____!____!______________________________________________
! ! Q             ! R  !<-->! TABLEAU DES DEBITS
! ! Z             ! R  !<-->! TABLEAU DES COTES DE LA SURFACE LIBRE
! ! NumConfluence ! I  ! -->! Numero de la confluence a traiter
! ! NumPassage    ! I  ! -->! VARIABLE INDICATRICE :
! !               !    !    !  = 1  CALCUL DU DEBIT DANS LA BRANCHE AVAL
! !               !    !    !  = 2  CALCUL DES COTES DANS LES BRANCHES AMONT
! ! Connect       ! T  ! -->! Structure contenant la table de connectivite
! ! Erreur        ! T  !<-->! ERREUR
! !_______________!____!____!______________________________________________
!
!                         VARIABLES LOCALES
! ._______________.____.____.______________________________________________
! !               !    !    !
! !_______________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!   SOUS PROGRAMME APPELANT :  PERSAR
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!
!   COMMENTAIRE:
!   -----------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************
!============================= Declarations ===========================
!.. Modules importes ..
!----------------------
        USE M_PRECISION
! Type CONNECT_T
        USE M_CONNECT_T
! Type ERREUR_T
        USE M_ERREUR_T
! Traitement de l'erreur
        USE M_TRAITER_ERREUR_I
        IMPLICIT NONE
!.. Arguments ..
!---------------
! TABLEAUX DIMENSIONNES A NbSection
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qb
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zb
        INTEGER, INTENT(IN) :: numconfluence
        INTEGER, INTENT(IN) :: numpassage
        TYPE(CONNECT_T), INTENT(IN) :: connect
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE QNODE_B
  END INTERFACE

END MODULE M_QNODE_I_B

