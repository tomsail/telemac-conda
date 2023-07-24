! Arrete le programme si erreur d'allocation memoire
subroutine err_alloc(name)
    use M_ERREUR_T               ! Type ERREUR_T
    use M_MESSAGE_C              ! Messages d'erreur
    use M_TRAITER_ERREUR_I       ! Traitement de l'errreur
    character(len=*) :: name
    type(ERREUR_T)   :: Erreur
    Erreur%Numero = 5
    Erreur%ft     = err_5
    Erreur%ft_c   = err_5c
    call TRAITER_ERREUR( Erreur , name )
    write(*,321)
    print * , Erreur%Message 
    stop 1
    return   
    321 format(/,"===========",/,"=> ERROR <=",/,"===========",/)
end subroutine err_alloc
