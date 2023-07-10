"fid=Identificateur du fichier." \
"comm=Communicateur mpi du groupe de processus participant à des appels parallèles sur le fichier MED." \
"info=Le paramètre mpi_info obtenu de la biblitohèque MPI ." \
"fidDes=résultat i.e. l'identificateur entier (ID) retourné sera utilisé par les routines de l'API pour accéder au contenu du fichier." \
"filename=Nom du fichier." \
"filenamesize=Taille du Nom du fichier (avec le chemin d'accès)." \
"comment=Descripteur du fichier." \
"accessmode=Mode d'acces au fichier." \
"hdfok=Indicateur booléen indiquant si la bibliothèque HDF est compatible avec cette bibliothèque MED." \
"medok=Indicateur booléen indiquant si le fichier est un fichier MED compatible avec la bibliothèque." \
"major=Numéro de version majeur." \
"minor=Numéro de version mineur." \
"release=Numéro de release." \
"medversion=Numéro de version défini dans une chaîne de caractères." \
"mountfilename=Nom du fichier à monter." \
"mid=Identificateur du fichier à démonter." \
"medclass=Type d'objet de haut niveau MED (champ ou maillage)". \
"mountId=Identificateur du fichier monté." \
"chfid=Identificateur du fichier à monter (fichier enfant)." \
"chpath=Chemin à partir duquel se trouve la structure au format MED à monter dans le fichier hôte (parent)." \
"error=retour négatif en cas d'erreur, Zéro sinon." \
"memfile=fichier mémoire." \
"filesync=synchronise ou non le fichier mémoire avec un fichier disque." \
"objectname=nom de l'objet a traiter de type medclass." \
"objectexist=indique l'existence d'un objet de type medclass." \
"fileexist=indique l'existence d'un fichier." \
"accessok=indique l'adéquation des droits effectifs avec le mode d'accès demandé ." \
\
"MEDfileOpenBrief=Ouverture d'un fichier MED." \
"MEDfileOpenDetails=\MEDfileOpenBrief" \
"MEDfileOpenNote= \
 \li Si aucun fichier de nom \a filename n'existe, il est crée dans les modes #MED_ACC_RDWR et #MED_ACC_RDEXT. \
 \li En mode #MED_ACC_CREAT un nouveau fichier \a filename est crée qu'il en existe déjà un ou non." \
"MEDfileVersionOpenBrief=Ouverture d'un fichier MED en indiquant la version du modèle à utiliser en cas de création d'un nouveau fichier." \
"MEDfileVersionOpenDetails=\MEDfileOpenBrief" \
"MEDfileVersionOpenNote= \
 \li Le numéro \a major ne peut être que le numéro majeur utilisé par la bibliothèque. \
 \li Le numéro \a minor ne peut être que dans la plage des versions mineurs des bibliothèques distribuées avec le numéro majeur égal à celui de la bibliothèque utilisé. \
 \li Le numéro \a release n'est pas pris en compte. Le numéro release utilisé sera le numéro le plus élévé associé à la version mineure demandée. \
 \MEDfileOpenNote" \
"MEDfileNameBrief=Renvoi le nom de fichier MED à partir d'un med_idt." \
"MEDfileNameDetails=Renvoi le nom de fichier/la taille du nom de fichier MED à partir d'un med_idt. \
Le nom de fichier contient le chemin d'accès. \
\li Si \a filename == NULL, la taille du nom de fichier est renvoyée. \
\li Si \a filename != NULL, le nom du fichier avec son chemin d'accès est copié dans filename à concurrence de \a filenamesize caractères" \
"MEDfileNameRem = Le paramètre \a filenamesize ne comptabilise pas le caractère '\\0' mais \a filename doit pouvoir l'accueillir." \
"MEDparfileOpenBrief=Ouverture d'un fichier MED pour une utilisation parallèle." \
"MEDparfileOpenDetails=Cette routine permet d'ouvrir un fichier MED pour une utilisation parallèle selon le mode d'accès souhaité." \
"MEDparfileOpenNote=\li Tous les appels à l'API MED via un descripteur de fichier parallèle doit être exécuté par tous les processus du comminicateur. \
\li Si un processus veut travailler indépendament des autres il doit également ouvrir le fichier via \ref MEDfileOpen pour avoir un descripteur séquentiel. " \
"MEDfileCloseBrief=Fermeture d'un fichier MED." \
"MEDfileCloseDetails=Cette routine permet de fermer un fichier MED. En cas d'accès en écriture, la garantie que les données sont physiquement écrites dans le fichier, n'est donnée que suite à l'exécution de cette routine." \
"MEDfileCommentWrDetails=Cette routine permet d'écrire un descripteur dans un fichier MED. Ce descripteur est une chaîne de caratères de MED_COMMENT_SIZE (200) caractères" \
"MEDfileCommentWrBrief=Ecriture d'un descripteur dans un fichier MED." \
"MEDfileCommentRdDetails=Cette routine permet de lire un descripteur dans un fichier MED. Ce descripteur est une chaîne de caratères de MED_COMMENT_SIZE (200) caractères" \
"MEDfileCommentRdBrief=Lecture d'un descripteur dans un fichier MED." \
"MEDfileCompatibilityDetails=Cette routine permet de vérifier la compatibilité du fichier passé en argument avec les bibliothèques HDF et MED utilisées : <ul><li>Le fichier doit être un fichier HDF et dans une version d'HDF utilisable par la bibliothèque MED pour que \a hdfok soit vrai.</li><li>Le fichier doit être dans une version MED utilisable par la bibliothèque MED (Majeur du fichier égal à celui de la bibliothèque et mineur du fichier inférieur ou égal à celui de la bibliothèque quelque soit la valeur release).</li></ul>" \
"MEDfileCompatibilityBrief=Vérification de la compatibilité d'un fichier avec HDF et MED." \
"MEDfileNumVersionRdDetails=Cette routine lit le numéro de version de la bibliothèque MED qui a été utilisée pour créer le fichier auquel on accède via le paramètre fid. Le numéro de version est renvoyé sous la forme de trois entiers : numéro de version majeur, numéro de version mineur, numéro de relase" \
"MEDfileNumVersionRdBrief=Lecture du numéro de version de la bibliothèque MED utilisée pour créer le fichier." \
"MEDfileStrVersionRdDetails=Cette routine lit le numéro de version de la bibliothèque MED qui a été utilisée pour créer le fichier auquel on accède via le paramètre fid. Le numéro de version est renvoyé sous la forme d'une chaîne de 9 caractères sous la forme "MED-M.m.r". Exemple de chaîne renvoyé : 'MED-3.0.0'." \
"MEDfileStrVersionRdBrief=Lecture du numéro de version de la bibliothèque MED utilisée pour créer le fichier (renvoyé sous la forme d'une chaîne de caractères)." \
"MEDfileObjectsMountDetails=Cette routine permet de monter dans le fichier courant un type de données (exemples les maillages, les champs) d'un autre fichier MED. Cette routine est utile par exemple quand les champs et les maillages d'une étude se situent dans des fichiers différents. Une fois le montage effectué, l'accès aux données montées est transparent." \
"MEDfileObjectsMountBrief=Cette routine permet de monter dans le fichier courant un type de données (exemples les maillages, les champs) d'un autre fichier MED." \
"MEDfileObjectsUnmountDetails=Cette routine permet désactiver un point de montage." \
"MEDfileObjectsUnmountBrief=Une fois le démontage effectué, les données précédemment montées ne sont plus accessibles." \
"MEDfileObjectsMountByIdBrief=Cette routine permet le montage d'une collection d'objets de type \a medclass dans le fichier associé à \a fid." \
"MEDfileObjectsMountByIdDetails=\MEDfileObjectsMountByIdBrief La collection au format MED est encapsulée dans un fichier HDF associé à \a chfid à partir du chemin \a chpath." \
\
"MEDfileObjectExistBrief=Interroge le fichier \a fid pour tester l'existence de l'objet \a objectname de type \a medclass." \
"MEDfileObjectExistDetails=\MEDfileObjectExistBrief" \
\
"MEDfileExistBrief=Interroge l'existence d'un fichier de nom \a filename et la possibilité de l'ouvrir selon le mode d'accès \a accessmode." \
"MEDfileExistDetails=\MEDfileExistBrief \
Si le fichier n'existe pas, cette fonction prend en compte les droits du répertoire contenu dans le chemin d'accès \a filename pour savoir s'il est possible de créer le fichier. \
" \
\
\
\
"MEDmemFileOpenBrief=Ouverture d'un fichier MED pour une utilisation en mémoire." \
"MEDmemFileOpenDetails=Cette routine permet de créer, lire ou modifier un fichier mémoire au format MED. Le contenu du fichier mémoire peut-être crée par les appels habituels de l'API MED ou initialisé à partir d'un fichier MED existant.\
Une fois le fichier mémoire fermé par \ref MEDfileClose, le fichier mémoire reste disponible pour être transmis et de nouveau ouvert/modifié par un nouvel appel à MEDmemFileOpen." \
\
"MEDmemFileOpenNote1=\par Gestion de la mémoire:\n\
\li La structure \a memfile doit impérativement être initialisée à #MED_MEMFILE_INIT. \
\li Une fois \a memfile initialisé à #MED_MEMFILE_INIT, la gestion de l'allocation mémoire peut être laissée à la charge de MED. La désallocation de \a memfile.app_image_ptr reste à la charge de l'utilisateur. \
\li Si (\a memfile.app_image_ptr == 0 ) :\
	La place mémoire utilisée par les différents appels medfichier sera allouée par medfichier et accessible par le pointeur \a medfile.image_ptr. L'emprise mémoire utilisée par l'ensemble des appels à medfichier est indiquée dans \a memfile.app_image_size au fil des appels. La désallocation est à la charge de l'utilisateur.\n\
\li Si (\a memfile.app_image_ptr <> 0) et (\a memfile.app_image_size <> 0) :\
	L'utilisateur a pré-alloué un emplacement d'acceuil au fichier mémoire, cet emplacement sera utilisé par MED dans les différentes fonctions de l'API. Si la taille réservée est insuffisante, MED réallouera la taille nécessaire et mettra à jour le champ \a memfile.app_image_size. Ce mécanisme  suppose que l'utilisateur n'utilise pas d'alias du pointeur \a memfile.app_image_ptr de façon concurrente à MED.\n Il est de la responsabilité de l'utilisateur de prendre connaissance des eventuels changements de taille et de valeur du pointeur.\n\
	Après l'appel à \ref MEDfileClose, MED  n'utilise plus l'image mémoire. Il est possible de vérifier qu'il n'existe plus d'accès à l'image mémoire en s'assurant que \a memfile.fapl_ref_count et \a memfile.vfd_ref_count sont tous les deux nuls." \
\
"MEDmemFileOpenNote2=\par Gestion des droits et fichier disque:\n\
\
\n	Si le paramètre \a syncfile est à #MED_FALSE, \ref MEDmemFileOpen ne s'occupera pas de la présence d'un fichier disque de même nom que l'image mémoire.\
\n	Si le paramètre syncfile est à #MED_TRUE, \ref MEDmemFileOpen gardera la cohérence entre le fichier disque crée ou déjà présent et l'image mémoire en fonction du mode d'accès demandé.\
\
\li Mode #MED_ACC_RDEXT : Mode interdit.\
\li Mode #MED_ACC_CREAT :\
\
\n Si \a filesync == #MED_FALSE : Un nouvel accès \a fid au fichier mémoire \a memfile est crée, aucun fichier disque n'est utilisé (un fichier disque de même nom peut exister mais ne sera pas utilisé). Si le fichier mémoire possédait une image MED valide, elle sera réinitialisée.\
\
\n Si \a filesync == #MED_TRUE : Un nouvel accès \a fid au fichier mémoire \a memfile est crée, un nouveau fichier disque de même nom est également crée (un fichier disque de même nom peut déjà exister, il sera alors écrasé). Tous les appels MED seront effectués en mémoire et sur le fichier jusqu'à l'appel de \ref MEDfileClose (pas forcément de façon synchrone jusqu'à la fermeture).\
\
\li Mode #MED_ACC_RDWR :\
\
\n Si \a filesync == #MED_FALSE : Un nouvel accès \a fid au fichier mémoire \a memfile existant et valide est crée qu'un fichier disque de même nom existe ou non. Les appels MED seront effectués uniquement en mémoire.\
\
\n Si \a filesync == #MED_TRUE : Un nouvel accès \a fid au fichier mémoire \a memfile est crée.\
S'il n'existe pas de fichier disque de même nom, l'image memfile doit exister et être valide; un fichier disque de même nom est crée et maintenu en cohérence (il faut au moins un appel MED en écriture/création pour que la synchronisation du fichier se fasse).\
S'il existe un fichier disque de même nom, l'image \a memfile est réinitialisée par le contenu du fichier. La cohérence est maintenue entre fichier mémoire et fichier disque.\
\
\li Mode #MED_ACC_RDONLY :\
\
\n \a filesync = #MED_TRUE | #MED_FALSE : Le fichier mémoire est initialisé à partir d' un fichier disque de même nom (forcément existant). Les appels MED en lecture seront effectués uniquement en mémoire, le fichier restera dans son état intial. Les appels MED en écriture échoueront." \
"meshname=Nom du maillage, de longueur maximum #MED_NAME_SIZE ." \
"nentity=Nombre d'entités de même type géométrique constituant globalement le maillage." \
"relativenumbering=numérotation MED relative à un type géométrique d'élément commence à 1." \
"switchmode=Mode d'entrelacement utilisé pour le stockage de valeurs \ref med_switch_mode." \
"entitype=Type d'entité (\ref med_entity_type)." \
"geotype=Type géométrique de l'entité (\ref med_geometry_type)." \
"geotypeit=Itérateur sur les types géométriques disponibles (\ref med_geometry_type)." \
"geotypename=Nom du type géométrique de l'entité (de taille #MED_NAME_SIZE, \ref med_geometry_type)." \
"geodim=Dimensions de l'espace utilisées pour décrire un type géométrique." \
"componentselect=Numéro de composante sélectionnée (#MED_ALL_CONSTITUENT pour désigner toutes les composantes)." \
"isolatednodes=Nombre de noeuds isolés au sein du maillage." \
"verticesnodes=Nombre de noeuds sommets dans le maillage." \
"cellmaxnodes=Nombre de noeuds maximum par maille du maillage." \
"numdt1=Numéro de pas de temps de la séquence de calcul précédente." \
"numit1=Numéro d'itération de la séquence de calcul précédente." \
"numdt2=Numéro de pas de temps de la séquence de calcul à créer." \
"numit2=Numéro d'itération de la séquence de calcul à créer." \
"dt2=Valeur du pas de temps." \
"description_=Description, chaîne de caractères de taille maximum #MED_COMMENT_SIZE caractères." \
"description=\description_" \
"spacedim=Dimension de l'espace de calcul." \
"meshdim=Dimension du maillage." \
"meshtype=Type du maillage (non structuré ou structuré)." \
"sortingtype=Ordre de tri des séquences de calcul dans le maillage (#MED_SORT_DTIT ou #MED_SORT_ITDT)." \
"axistype=Type du repère des coordonnées (cartésien #MED_CARTESIAN , cylindrique #MED_CYLINDRICAL ou sphérique #MED_SPHERICAL )." \
"axisname=Noms des axes du repère des coordonnées. Chaque nom est de taille #MED_SNAME_SIZE caractères." \
"axisunit=Unités des axes du repère des coordonnées. Chaque unité est un nom de taille #MED_SNAME_SIZE caractères." \
"cmode=Mode de connectivité (nodale ou descendante)." \
"connectivity=Tableau des connectivités du type géométrique d'élément du maillage." \
"dimselect=Composant (ou dimension) sélectionné, #MED_ALL_CONSTITUENT pour les sélectionner tous." \
"number_=Tableau des numéros." \
"number=\number_" \
"elementnumber=\number_" \
"nodenumber=\number_" \
"famnumber=Tableau des numéros de famille." \
"name_=Tableau des noms. Chaque nom est sur #MED_SNAME_SIZE caractères." \
"name=\name_" \
"elementname=\name_" \
"nodename=\name_" \
"axis=Numéro de l'axe du repère." \
"naxis=Nombre d'axe dans le repère de coordonnées." \
"gridindex=Tableau des coordonnées selon l'axe considéré." \
"indexsize=Taille du tableau." \
"gridstruct=Structure (nombre de points sur chaque axe du repère) d'un maillage structuré. " \
"gridtype=Type de maillage structuré." \
"meshit=Itérateur sur les maillages. Cet itérateur commence à 1." \
"nstep=Nombre de séquence de calcul." \
"coordinate=Tableau des coordonnées." \
"polyindex=Tableau d'index des polygones." \
"polytype=Type de polygon #MED_POLYGON | #MED_POLYGON2." \
"faceindexsize=Taille du tableau d'index des faces des polyèdres." \
"faceindex=Tableau d'index des faces des polyèdres." \
"nodeindexsize=Taille du tableau d'index des noeuds des faces des polyèdres." \
"nodeindex=Tableau d'index des noeuds des faces des polyèdres." \
"univname=Nom universel de taille #MED_LNAME_SIZE caractères." \
"nent=Nombre d'entité à lire." \
"nnode=Nombre de noeuds." \
"datatype=Type de la donnée." \
"changement=Indicateur de changement par rapport à la séquence de calcul précédente." \
"transformation=Indicateur de transformation par rapport à la séquence de calcul précédente." \
"withname_=Indicateur booléen de présence des noms." \
"withname=\withname_" \
"withelementname=\withname_" \
"withnodename=\withname_" \
"withnumber_=booléen de présence des numéros optionnels." \
"withnumber=\withnumber_" \
"withelementnumber=\withnumber_" \
"withnodenumber=\withnumber_" \
"withfamnumber=Indicateur booléen de présence des numéros de famille." \
"nmesh=Nombre de maillage(s) dans le fichier." \
"coordinatetrsf=Paramètre de translation rotation de l'ensembles des noeuds (7 paramètres)." \
\
\
\
\
"MEDmeshAttributeRdBrief=Cette routine permet la lecture des attributs optionnels d'un maillage." \
"MEDmeshAttributeRdDetails=Cette routine permet la lecture des attributs optionnels d'un maillage :\
 nombre de noeuds isolés, nombre de noeuds sommets, nombre de noeuds maximum par maille.\
 La présence de ces attributs est optionnelle, s'ils n'existent pas, il ne s'agit donc pas d'une erreur." \
\
"MEDmeshAttributeWrBrief=Cette routine permet l'écriture des attributs optionnels d'un maillage." \
"MEDmeshAttributeWrDetails=Cette routine permet l'écriture des attributs optionnels d'un maillage :\
 nombre de noeuds isolés, nombre de noeuds sommets, nombre de noeuds maximum par maille. L'écriture de ces attributs est optionnelle." \
\
"MEDmeshComputationStepCrBrief=Cette routine permet de créer une nouvelle séquence de calcul dans un maillage." \
"MEDmeshComputationStepCrDetails=Cette routine permet de créer une nouvelle séquence de calcul dans un maillage.\
 Une séquence de calcul est identifiée par un couple numéro de pas de temps / numéro d'itération. Une date est associée au pas de temps. \
 \li Si les deux pas de temps et numéro d'itération passés en paramètres sont différents,\
 la première séquence passée en paramètre correspond à la séquence de calcul précédant la séquence de calcul à créer.\
 La séquence de calcul à créer s'insère alors entre deux séquences de calcul existantes. \
 \li Si les deux pas de temps et numéro d'itération passés en paramètres sont identiques,\
 la séquence de calcul à créer s'insère alors en tant que dernière séquence (si les valeurs sont cohérentes). \
 Si la séquence de calcul à créer existe déjà, une erreur est renvoyée." \
\
"MEDmeshComputationStepInfoBrief= Cette routine permet de lire les informations relatives à une séquence de calcul d'un maillage." \
"MEDmeshComputationStepInfoDetails= Cette routine permet de lire les informations relatives à une séquence de calcul d'un maillage. L'accès à la séquence de calcul se fait via un itérateur. Les informations lues sont : le numéro de pas de temps, le numéro d'itération, la valeur du pas de temps. \
\li Une séquence de calcul est identifiée par les paramètres <i>numdt</i> et <i>numit</i> (les valeurs renvoyées peuvent être #MED_NO_DT et #MED_NO_IT). \
\li Si le numéro de pas de temps est différent de #MED_NO_DT, une date est associée au pas de temps." \
\
"MEDmeshCrBrief=Cette routine permet de créer un maillage dans un fichier." \
"MEDmeshCrDetails=Cette routine permet de créer un maillage dans un fichier. Un maillage est caractérise par : \
\li son nom  ; \
\li sa dimension  : cette dimension est inférieure ou égale à celle de la dimension de l'espace de calcul\
 (on peut avoir un maillage 2D dans un espace de calcul 3D) ; \
\li son type : structuré ou non structuré ; \
\li le repère des coordonnées définies selon la dimension de l'espace de calcul : cartésien, sphérique, cylindrique. \
\li le mode de tri des séquence de calcul (ordre d'accès aux séquences de calcul) :\
 en privilégiant les pas de temps sur les numéro d'itération (#MED_SORT_DTIT) ou inversement (#MED_SORT_ITDT). \
Le choix du mode de tri est obligatoire, même si le maillage ne contient que l'étape de calcul par défaut ( #MED_NO_DT , #MED_NO_IT )." \
\
"MEDmeshElementConnectivityAdvancedRdBrief=Cette routine permet de lire dans un maillage le tableau des connectivités pour un type géométrique d'un élément,\
 selon une séquence de calcul et un filtre donnés." \
"MEDmeshElementConnectivityAdvancedRdDetails=Cette routine permet de lire dans un maillage le tableau des connectivités pour un type géométrique d'un élément,\
 selon une séquence de calcul et un filtre donnés.\
 Cette routine est une routine dite avancée car le paramètre correspondant au filtre permet de sélectionner finement les données lues en mode séquentiel ou parallèle : avec ou sans profil, mode d'entrelacement, par blocs, etc.\
 A noter que le type de l'entite est soit une maille (connectivité nodale ou descendante), une face (connectivité descendante uniquement) ou une arête (connectivité descendante uniquement).\
 Quelque soit le mode de connectivité (nodale/descendante), la numérotation optionnelle n'est jamais utilisée dans la définition des connectivités. " \
\
"MEDmeshElementConnectivityAdvancedWrBrief=Cette routine permet d'écrire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul et un filtre donnés." \
"MEDmeshElementConnectivityAdvancedWrDetails=Cette routine permet d'écrire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul et un filtre donnés.\
 Cette routine est une routine dite avancée car le paramètre correspondant au filtre permet de sélectionner finement les données lues en mode séquentiel ou parallèle :\
 avec ou sans profil, mode d'entrelacement, par blocs, etc.\
 A noter que le type de l'entite est soit une maille (connectivité nodale ou descendante), une face (connectivité descendante uniquement) ou une arête (connectivité descendante uniquement).\
 Quelque soit le mode de connectivité (nodale/descendante), la numérotation optionnelle n'est jamais utilisée dans la définition des connectivités.  " \
\
"MEDmeshElementConnectivityRdBrief=Cette routine permet de lire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul donnée." \
"MEDmeshElementConnectivityRdDetails=Cette routine permet de lire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul donnée. A noter que le : \
\li Le type de l'entite est soit une maille (connectivité nodale ou descendante), une face (connectivité descendante uniquement) ou une arête (connectivité descendante uniquement). \
\li Quelque soit le mode de connectivité (nodale/descendante), la numérotation optionnelle n'est jamais utilisée dans la définition des connectivités. \
\li Le mode de stockage du tableau de connectivité en mémoire est soit entrelacé soit non entrelacé." \
\
"MEDmeshElementConnectivityWrBrief=Cette routine permet d'écrire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul donnée." \
"MEDmeshElementConnectivityWrDetails=Cette routine permet d'écrire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul donnée. A noter que : \
\li Le type de l'entite est soit une maille (connectivité nodale ou descendante), une face (connectivité descendante uniquement) ou une arête (connectivité descendante uniquement). \
\li Quelque soit le mode de connectivité (nodale/descendante), la numérotation optionnelle n'est jamais utilisée dans la définition des connectivités. \
\li Le mode de stockage du tableau de connectivité en mémoire est soit entrelacé ou non entrelacé." \
"MEDmeshElementConnectivityRdBrief=Cette routine permet de lire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul donnée." \
\
"MEDmeshElementConnectivityWithProfileRdBrief=Cette routine permet de lire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul et un profil donnés." \
"MEDmeshElementConnectivityWithProfileRdDetails=Cette routine permet de lire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul et un profil donnés.\
 Le profil est identifié par un nom et le mode de stockage des données en mémoire peut être paramétré : compact ou global. A noter que le : \
\li Le type de l'entite est soit une maille (connectivité nodale ou descendante), une face (connectivité descendante uniquement) ou une arête (connectivité descendante uniquement). \
\li Quelque soit le mode de connectivité (nodale/descendante), la numérotation optionnelle n'est jamais utilisée dans la définition des connectivités. \
\li Le mode de stockage du tableau de connectivité en mémoire est soit entrelacé ou non entrelacé." \
\
"MEDmeshElementConnectivityWithProfileWrBrief=Cette routine permet d'écrire dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul et un profil donnés." \
"MEDmeshElementConnectivityWithProfileWrDetails=Cette routine permet d'écrier dans un maillage le tableau des connectivités pour un type géométrique d'un élément, selon une séquence de calcul et un profil donnés.\
 Le profil est identifié par un nom et le mode de stockage des données en mémoire peut être paramétré : compact ou global. A noter que le : \
\li Le type de l'entite est soit une maille (connectivité nodale ou descendante), une face (connectivité descendante uniquement) ou une arête (connectivité descendante uniquement). \
\li Quelque soit le mode de connectivité (nodale/descendante), la numérotation optionnelle n'est jamais utilisée dans la définition des connectivités. \
\li Le mode de stockage du tableau de connectivité en mémoire est soit entrelacé ou non entrelacé." \
\
"MEDmeshEntityFamilyNumberRdBrief=Cette routine permet la lecture des numéros de famille d'un type d'entité d'un maillage." \
"MEDmeshEntityFamilyNumberRdDetails=Cette routine permet la lecture des numéros de famille d'un type d'entité d'un maillage.\
 A noter que lorsque tous les numéros de familles d'une type d'entité donné (exemple : les noeuds) sont tous à 0,\
 la présence du tableau n'est pas obligatoire." \
"MEDmeshEntityFamilyNumberWrBrief=Cette routine permet l'écriture des numéros de famille d'un type d'entité d'un maillage." \
"MEDmeshEntityFamilyNumberWrDetails=Cette routine permet l'écriture des numéros de famille d'un type d'entité d'un maillage.\
 A noter que lorsque tous les numéros de familles d'une type d'entité donné (exemple : les noeuds) sont tous à 0,\
 la présence du tableau n'est pas obligatoire." \
\
"MEDmeshEntityFamilyNumberRdDetails=Cette routine permet la lecture des numéros de famille d'un type d'entité d'un maillage.\
 A noter que lorsque tous les numéros de familles d'une type d'entité donné (exemple : les noeuds) sont tous à 0,\
 la présence du tableau n'est pas obligatoire." \
\
"MEDmeshEntityNameRdBrief=Cette routine permet de lire les noms d'un type d'entité d'un maillage." \
"MEDmeshEntityNameRdDetails=Cette routine permet de lire les noms d'un type d'entité d'un maillage. La présence des noms est optionnelle. " \
\
"MEDmeshEntityNameWrBrief=Cette routine permet d'écrire les noms d'un type d'entité d'un maillage." \
"MEDmeshEntityNameWrDetails=Cette routine permet d'écrire les noms d'un type d'entité d'un maillage. La présence des noms est optionnelle. " \
\
"MEDmeshEntityNumberRdBrief=Cette routine permet de lire les numéros d'un type d'entité d'un maillage." \
"MEDmeshEntityNumberRdDetails=Cette routine permet de lire les numéros d'un type d'entité d'un maillage. La présence des numéros est optionnelle. " \
\
"MEDmeshEntityNumberWrBrief=Cette routine permet d'écrire les numéros d'un type d'entité d'un maillage." \
"MEDmeshEntityNumberWrDetails=Cette routine permet d'écrire les numéros d'un type d'entité d'un maillage. La présence des numéros est optionnelle. " \
\
"MEDmeshGeotypeNameBrief=Cette routine renvoie le nom associé à un type géométrique." \
"MEDmeshGeotypeNameDetails=Cette routine renvoie le nom associé au géométrique \a geotype dans le paramètre \a geotypename.\
 Le type géométrique \a geotype peut être celui d'un élément de structure local au fichier identifié par \fid." \
\
"MEDmeshGeotypeParameterBrief=Cette routine renvoie les caractéristiques d'un type géométrique de maille." \
"MEDmeshGeotypeParameterDetails=Cette routine renvoie les caractéristiques d'une maille de type géométrique \a geotype en connectivité nodale. \
 Le type géométrique \a geotype se décrit avec \a geodim dimensions et se compose de \a nnode noeuds.\
 Le type géométrique \a geotype peut être celui d'un élément de structure local au fichier identifié par \fid." \
\
"MEDmeshGlobalNumberWrBrief=Cette routine permet l'écriture d'une numérotation globale sur un maillage pour un type d'entité, un type géométrique et une séquence de calcul donnés." \
"MEDmeshGlobalNumberWrDetails=Cette routine permet l'écriture d'une numérotation globale sur un maillage pour un type d'entité, un type géométrique et une séquence de calcul donnés.\
 Les numéros globaux sont obligatoirement supérieur à 1. Un maillage distribué est entièrement déterminé par la donnée des maillages affectés à chacun des sous-domaines,\
 par la définition de « joints » - raccord entre les maillages de sous-domaines voisins et par une numérotation globale optionnelle des entités. " \
\
"MEDmeshGlobalNumberRdBrief=Cette routine permet la lecture d'une numérotation globale sur un maillage pour un type d'entité, un type géométrique et une séquence de calcul donnés." \
"MEDmeshGlobalNumberRdDetails=Cette routine permet la lecture d'une numérotation globale sur un maillage pour un type d'entité, un type géométrique et une séquence de calcul donnés.\
 Les numéros globaux sont obligatoirement supérieur à 1. Un maillage distribué est entièrement déterminé par la donnée des maillages affectés à chacun des sous-domaines,\
 par la définition de « joints » - raccord entre les maillages de sous-domaines voisins et par une numérotation globale optionnelle des entités. " \
\
"MEDmeshGridIndexCoordinateRdBrief= Cette routine permet la lecture des coordonnées des noeuds d'un maillage structuré selon un axe du repère des coordonnées." \
"MEDmeshGridIndexCoordinateRdDetails=Cette routine permet la lecture des coordonnées des noeuds d'un maillage structuré selon un axe du repère des coordonnées.\
 Pour N axes dans le repère des coordonnées, les numéros des axes vont de 1 à N. Pour lire les coordonnées d'un maillage structuré de type #MED_CURVILINEAR_GRID,\
 il faut utiliser la même routine de lecture des coordonnées que pour les maillages non structurés." \
\
"MEDmeshGridIndexCoordinateWrBrief= Cette routine permet l'écriture des coordonnées des noeuds d'un maillage structuré selon un axe du repère des coordonnées." \
"MEDmeshGridIndexCoordinateWrDetails=Cette routine permet l'écriture des coordonnées des noeuds du maillage structuré #MED_STRUCTURED_MESH \a meshname de type #MED_CARTESIAN_GRID ou #MED_POLAR_GRID  selon un axe du repère des coordonnées. Pour N axes dans le repère des coordonnées, les numéros des axes vont de 1 à N. Pour écrire les coordonnées d'un maillage structuré de type #MED_CURVILINEAR_GRID, il faut utiliser la routine d'écriture des coordonnées des maillages non structurés \ref MEDmeshNodeCoordinateWr." \
"MEDmeshGridIndexCoordinateWrRem= \
\li Une grille #MED_CARTESIAN_GRID doit utiliser un système de coordonnées #MED_CARTESIAN \
\li Une grille #MED_POLAR_GRID peut utiliser un système de coordonnées #MED_CYLINDRICAL ou #MED_SPHERICAL \
\li Cette routine ne permet pas l'écriture des noeuds d'un maillage structuré #MED_STRUCTURED_MESH de type #MED_CURVILINEAR_GRID." \
\
"MEDmeshGridStructRdBrief=Cette routine permet la lecture de la structure (nombre de points sur chaque axe du repère) d'un maillage structuré de type #MED_CURVILINEAR_GRID." \
"MEDmeshGridStructRdDetails=Cette routine permet la lecture de la structure d'un maillage structuré (nombre de points sur chaque axe du repère) de type #MED_CURVILINEAR_GRID.\
 Par exemple une grille 5x3 (15 noeuds, 8 quadrangles) a une structure égale au tableau [5,3]." \
\
"MEDmeshGridStructRem=\
\li Une grille #MED_CURVILINEAR_GRID peut utiliser un système de coordonnées quelconque" \
\
"MEDmeshGridStructWrBrief=Cette routine définit la structure (nombre de points sur chaque axe du repère) d'un maillage structuré de type #MED_CURVILINEAR_GRID." \
"MEDmeshGridStructWrDetails=Cette routine définit la structure d'un maillage structuré (nombre de points sur chaque axe du repère) de type #MED_CURVILINEAR_GRID.\
 Par exemple une grille 5x3 (15 noeuds, 8 quadrangles) a une structure égale au tableau [5,3]." \
\
"MEDmeshGridTypeRdBrief=Cette routine permet de lire le type d'un maillage structuré (#MED_STRUCTURED_MESH)." \
"MEDmeshGridTypeRdDetails=Cette routine permet de lire le type d'un maillage structuré : #MED_CARTESIAN_GRID, #MED_POLAR_GRID ou #MED_CURVILINEAR_GRID." \
\
"MEDmeshGridTypeWrBrief=Cette routine permet de définir le type d'un maillage structuré (#MED_STRUCTURED_MESH)." \
"MEDmeshGridTypeWrDetails=Cette routine permet de définir le type d'un maillage structuré :  #MED_CARTESIAN_GRID, #MED_POLAR_GRID ou #MED_CURVILINEAR_GRID." \
\
"MEDmeshInfoBrief=Cette routine permet de lire les informations relatives à un maillage dans un fichier." \
"MEDmeshInfoDetails=Cette routine permet de lire les informations relatives à un maillage dans un fichier. L'accès au maillage se fait via un itérateur. Un maillage est caractérise par : \
\li son nom  ; \
\li sa dimension  : cette dimension est inférieure ou égale à celle de la dimension de l'espace de calcul (on peut avoir un maillage 2D dans un espace de calcul 3D) ; \
\li son type : structuré ou non structuré ; \
\li le repère des coordonnées définies selon la dimension de l'espace de calcul : cartésien, sphérique, cylindrique. \
\li le mode de tri des séquence de calcul (ordre d'accès aux séquences de calcul) : en privilégiant les pas de temps sur les numéro d'itération (#MED_SORT_DTIT) ou inversement (#MED_SORT_ITDT) ; \
\li le nombre de séquences de calcul présentes dans le maillage. " \
\
"MEDmeshInfoByNameBrief=Cette routine permet de lire les informations relatives à un maillage en précisant son nom." \
"MEDmeshInfoByNameDetails=Cette routine permet de lire les informations relatives à un maillage dans un fichier. L'accès au maillage se fait directement via son nom. Un maillage est caractérise par : \
\li son nom  ; \
\li sa dimension  : cette dimension est inférieure ou égale à celle de la dimension de l'espace de calcul (on peut avoir un maillage 2D dans un espace de calcul 3D) ; \
\li son type : structuré ou non structuré ; \
\li le repère des coordonnées définies selon la dimension de l'espace de calcul : cartésien, sphérique, cylindrique. \
\li le mode de tri des séquence de calcul (ordre d'accès aux séquences de calcul) : en privilégiant les pas de temps sur les numéro d'itération (#MED_SORT_DTIT) ou inversement (#MED_SORT_ITDT) ; \
\li le nombre de séquences de calcul présentes dans le maillage. " \
\
"MEDmeshNodeCoordinateAdvancedRdBrief=Cette routine permet de lire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul et un filtre donnés." \
"MEDmeshNodeCoordinateAdvancedRdDetails=Cette routine permet de lire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul et un filtre donnés. Cette routine est une routine dite avancée car le paramètre correspondant au filtre permet de sélectionner finement les données lues en mode séquentiel ou parallèle : avec ou sans profil, mode d'entrelacement, par blocs, etc." \
\
"MEDmeshNodeCoordinateAdvancedWrBrief=Cette routine permet d'écrire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul et un filtre donnés." \
"MEDmeshNodeCoordinateAdvancedWrDetails=Cette routine permet d'écrire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul et un filtre donnés. Cette routine est une routine dite avancée car le paramètre correspondant au filtre permet de sélectionner finement les données lues en mode séquentiel ou parallèle : avec ou sans profil, mode d'entrelacement, par blocs, etc." \
"MEDmeshNodeCoordinateWrnogridRem= \
\li Cette routine ne permet pas l'écriture des coordonnées des noeuds des maillages structurés #MED_STRUCTURED_MESH quelqu'en soit le type #med_grid_type." \
\
"MEDmeshNodeCoordinateRdBrief=Cette routine permet de lire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul donnée." \
"MEDmeshNodeCoordinateRdDetails=Cette routine permet de lire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul donnée. Le mode de stockage du tableau de coordonnées en mémoire est entrelacé ou non entrelacé." \
"MEDmeshNodeCoordinateWrBrief=Cette routine permet d'écrire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul donnée." \
"MEDmeshNodeCoordinateWrDetails=Cette routine permet d'écrire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul donnée. Le mode de stockage du tableau de coordonnées en mémoire est entrelacé ou non entrelacé." \
\
"MEDmeshNodeCoordinateWithProfileWrBrief=Cette routine permet d'écrire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul donnée et un profil donnés." \
"MEDmeshNodeCoordinateWithProfileWrDetails=Cette routine permet d'écrire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul et un profil donnés. Le profil est identifié par un nom et le mode de stockage des données en mémoire peut être paramétré : compact ou global. Le mode de stockage du tableau de coordonnées en mémoire est entrelacé ou non entrelacé." \
"MEDmeshNodeCoordinateWrgridRem1=\
\li Cette routine est également utilisée pour écrire les noeuds des maillages structurés #MED_STRUCTURED_MESH de type #MED_CURVILINEAR_GRID. \
\li Pour les autres types de maillages structurés (#MED_CARTESIAN_GRID, #MED_POLAR_GRID) utiliser la routine \ref MEDmeshGridIndexCoordinateWr qui permet l'écriture des noeuds principaux selon les axes choisis." \
\
"MEDmeshNodeCoordinateWithProfileRdBrief=Cette routine permet de lire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul donnée et un profil donnés." \
"MEDmeshNodeCoordinateWithProfileRdDetails=Cette routine permet de lire dans un maillage le tableau des coordonnées des noeuds, selon une séquence de calcul et un profil donnés. Le profil est identifié par un nom et le mode de stockage des données en mémoire peut être paramétré : compact ou global. Le mode de stockage du tableau de coordonnées en mémoire est entrelacé ou non entrelacé." \
\
"MEDmeshPolygonRdBrief=Cette routine permet la lecture des connectivités de polygones." \
"MEDmeshPolygonRdDetails=Cette routine permet la lecture des connectivités de polygones (polygones à nombre de noeuds quelconques non référencés dans les éléments géométriques de base)." \
"MEDmeshPolygonRem= \
\li Le mode de stockage ne dépend pas des types géométriques spécifiques aux différents polygones (le type géométrique spécifique est donné par le nombre de sommets du polygone). \
\li Exemple : si on a 2 polygones à 5 sommets (P5) et 1 polygone à 6 sommets (P6), on peut stocker les connectivités de ces éléments de la manière suivante : P5, P6, P5. \
\li On accède à la connectivité de chaque polygone par l'intermédiaire du tableau d'index \a polyindex. \
\li En connectivité nodale (#MED_NODAL), les entiers stockés dans le tableau de connectivités correspondent à des numéros de noeuds. \
\li En connectivité descendante (#MED_DESCENDING), les entiers stockés dans le tableau de connectivités correspondent à des numéros d'arêtes. \
\li Dans notre exemple, en mode #MED_NODAL cela revient à avoir les 2 tableaux suivants (par convention les indexes MED débutent à 1) : \n \
\n \image html exemple_connectivite_polygones.png \n \n \
\li Quelque soit le mode de connectivité (nodale/descendante), la numérotation optionnelle n'est jamais utilisée dans la définition des connectivités. \
\li Le seul mode de stockage du tableau \a connectivity possible est le mode non entrelacé." \
\
"MEDmeshPolygon2RdBrief= \MEDmeshPolygonRdBrief " \
"MEDmeshPolygon2RdDetails=Cette routine permet la lecture des connectivités de polygones simple et quadratique." \
"MEDmeshPolygon2Rem= \
\MEDmeshPolygonRem \
\li Polygones Quadratiques :\n \
Exemples de deux polygones quadratiques : poly1 : 12,1,2,3,4,5 + 6,7,8,9,10,11 et poly2 : 1,14,3,2 + 13,15,8,7 \
\n \n \image html exemple_connectivite+index_polygones2.png \n \
\li Les polygones quadratiques s'utilisent comme les polygones simples mais uniquement en connectivité #MED_NODAL . \
\li Pour les polygones quadratiques l' \a indexsize est de même taille que pour les polygones simples. \
\li Pour chaque polygones quadratiques, les noeuds milieux apparaissent après l'ensemble des noeuds du polygone simple associé." \
\
"MEDmeshPolygonWrBrief=Cette routine permet l'écriture des connectivités de polygones." \
"MEDmeshPolygonWrDetails=Cette routine permet l'écriture des connectivités de polygones (polygones à nombre de noeuds quelconques non référencés dans les éléments géométriques de base)." \
\
"MEDmeshPolygon2WrBrief= \MEDmeshPolygonWrBrief " \
"MEDmeshPolygon2WrDetails=Cette routine permet l'écriture des connectivités de polygones simple et quadratique." \
\
"MEDmeshPolyhedronWrBrief=Cette routine permet l'écriture dans un maillage des connectivités de polyèdres." \
"MEDmeshPolyhedronWrDetails=Cette routine permet l'écriture dans un maillage des connectivités de polyèdres (polyèdres quelconques non référencés dans les éléments géométriques de base). " \
"MEDmeshPolyhedronRem= \
\li Pour le stockage des données en mémoire en connectivité nodale, on accède aux connectivités via un système de double indexation :  le premier tableau <i>faceindex</i> renvoie à la liste des faces de chaque polyèdre, \
le second tableau <i>nodeindex</i> renvoie pour chaque face à la liste des noeuds qui la compose. \
\li Les faces communes sont décrites 2 fois (mêmes listes de noeuds mais orientations différentes). La normale des faces doit être extérieure. \
\n \n \image html exemple_connectivite_nodale_polyedres.svg \n \n \
\li Pour le stockage des données en mémoire en connectivité descendante, un seul niveau d'indexation suffit (<i>faceindex</i>). Le tableau des connectivités contient les numéros des faces. Le tableau <i>nodeindex</i> contient alors le type géométrique de chaque face (exemple : #MED_TRIA3). \
\li Les numéros des faces en connectivité descendante se base sur la numérotation locale à chaque type géométrique (exemple : 1..nq pour les #MED_QUAD4, 1..nt pour les #MED_TRIA3...). \
\n \n \image html exemple_connectivite_descendante_polyedres.svg \n \n \
\li Quelque soit le type de connectivité (nodale/descendante), la numérotation optionnelle n'est jamais utilisée dans la définition des connectivités." \
\
"MEDmeshPolyhedronRdBrief=Cette routine permet la lecture dans un maillage des connectivités de polyèdres." \
"MEDmeshPolyhedronRdDetails=Cette routine permet la lecture dans un maillage des connectivités de polyèdres\
 (polyèdres quelconques non référencés dans les éléments géométriques de base)." \
\
"MEDmeshUniversalNameRdBrief=Cette routine permet la lecture du nom universel d'un maillage." \
"MEDmeshUniversalNameRdDetails=Cette routine permet la lecture du nom universel d'un maillage. La présence du nom universel est optionnelle.\
 Le nom universel est un nom unique : deux maillages ne peuvent avoir le même nom universel. " \
\
"MEDmeshUniversalNameWrBrief=Cette routine permet l'écriture du nom universel d'un maillage." \
"MEDmeshUniversalNameWrDetails=Cette routine permet l'écriture du nom universel d'un maillage. La présence du nom universel est optionnelle.\
 Le nom universel est un nom unique : deux maillages ne peuvent avoir le même nom universel. Le nom universel d'un maillage est généré automatiquement à partir du système d'exploitation de la machine." \
\
"MEDmeshnAxisBrief=Cette routine permet de lire dans un maillage le nombre d'axes du repère des coordonnées des noeuds." \
"MEDmeshnAxisDetails=Cette routine permet de lire dans un maillage le nombre d'axes du repère des coordonnées des noeuds. le nombre d'axe correspond\
 à la dimension de l'espace de calcul. L'accès au maillage se fait via un itérateur." \
\
"MEDmeshnAxisByNameBrief=Cette routine permet de lire dans un maillage le nombre d'axes du repère des coordonnées des noeuds avec accès direct." \
"MEDmeshnAxisByNameDetails=Cette routine permet de lire dans un maillage le nombre d'axes du repère des coordonnées des noeuds. le nombre d'axe\
 correspond à la dimension de l'espace de calcul. L'accès au maillage se fait directement via son nom." \
\
"MEDmeshnEntityBrief=Cette routine permet de lire le nombre d'entités dans un maillage pour une séquence de calcul donnée." \
"MEDmeshnEntityDetails2=Cette routine retourne selon la valeur des paramètres : \
\li Le nombre de noeuds/mailles/faces/arêtes d'un maillage non structuré. \
\li Le nombre de noeuds d'un maillage structuré correspondant à une grille curviligne (#MED_CURVILINEAR_GRID). \
\li Le nombre de points de coordonnées d'un maillage  structuré correspondant à une grille cartésienne ou polaire (#MED_CURVILINEAR_GRID,#MED_POLAR_GRID). \
\li La taille des tableaux d'index des polygones (\a entitype==#MED_CELL, \a geotype==#MED_POLYGON, \a datatype==#MED_INDEX_NODE) .\n  \
\li La taille des tableaux d'index des faces des polyèdres (\a entitype==#MED_CELL, \a geotype==#MED_POLYHEDRON, \a datatype==#MED_INDEX_FACE) .\n \
\li La taille des tableaux d'index des polyèdres (\a entitype==#MED_CELL, \a geotype==#MED_POLYHEDRON, \a datatype==#MED_INDEX_NODE) .\n" \
"MEDmeshnEntityDetails=\MEDmeshnEntityBrief \
L'indicateur \a changement indique un changement dans le maillage par rapport à la séquence de calcul précédente (exemple : nouvelles coordonnées des noeuds).\
 Si cet indicateur est à #MED_TRUE, l'indicateur \a transformation indique pour la séquence de calcul considérée et le type d'entité concerné un changement\
 géométrique (exemple : modification des connectivités des mailles). \
\MEDmeshnEntityDetails2 " \
"MEDmeshnEntityWithProfileBrief=Cette routine permet de lire le nombre d'entités dans un maillage pour une séquence de calcul et un profil donnés." \
"MEDmeshnEntityWithProfileDetails=\MEDmeshnEntityWithProfileBrief \
Le profil est identifié par un nom et le mode de stockage des données en mémoire peut être paramétré : #MED_COMPACT_STMODE ou #MED_GLOBAL_STMODE .\
 L'indicateur \a changement indique un changement dans le maillage par rapport à la séquence de calcul précédente (exemple : nouvelles coordonnées des noeuds).\
 Si cet indicateur est à #MED_TRUE, l'indicateur \a tranbsformation indique pour la séquence de calcul considérée et le type d'entité concerné un changement\
 géométrique (exemple : modification des connectivités des mailles). \
Cette routine retourne selon la valeur des paramètres et en tenant compte du mode de stockage du profil : \
\MEDmeshnEntityDetails2 " \
\
"MEDmeshNodeWrBrief=Cette routine permet l'écriture des noeuds d'un maillage non structuré pour une séquence de calcul donnée." \
"MEDmeshNodeWrDetails=Cette routine permet l'écriture des noeuds d'un maillage non structuré pour une séquence de calcul donnée.\
 Les données écrites portent sur les coordonnées des noeuds, les noms des noeuds (optionnel), les numéros des noeuds (optionnel),\
 les numéros de familles des noeuds (optionnel si tous égaux à 0). Les booléens associés aux tableaux permettent d'indiquer la présence\
 des données optionnelles." \
"MEDmeshNodeRdBrief=Cette routine permet la lecture des noeuds d'un maillage non structuré pour une séquence de calcul donnée." \
"MEDmeshNodeRdDetails=Cette routine permet la lecture des noeuds d'un maillage non structuré pour une séquence de calcul donnée.\
 Les données lues sont les coordonnées des noeuds, les noms des noeuds (optionnel), les numéros des noeuds (optionnel),\
 les numéros de familles des noeuds (optionnel si tous égaux à 0). Les booléens associés aux tableaux permettent d'indiquer la présence\
 des données optionnelles." \
"MEDmeshNodeWrDetails=Cette routine permet l'écriture des noeuds d'un maillage non structuré pour une séquence de calcul donnée.\
 Les données écrites portent sur les coordonnées des noeuds, les noms des noeuds (optionnel), les numéros des noeuds (optionnel),\
 les numéros de familles des noeuds (optionnel si tous égaux à 0). Les booléens associés aux tableaux permettent d'indiquer la présence\
 des données optionnelles." \
\
"MEDmeshElementWrBrief=Cette routine permet l'écriture d'un type d'élément d'un maillage non structuré pour une séquence de calcul donnée." \
"MEDmeshElementWrDetails=Cette routine permet l'écriture d'un type d'entité d'un maillage non structuré pour une séquence de calcul donnée. Les données écrites sont le tableau des connectivités, les noms (optionnel), les numéros (optionnel), les numéros de familles (optionnel si tous égaux à 0). Les booléens associés aux tableaux permettent d'indiquer la présence des données optionnelles." \
"MEDmeshElementRdBrief=Cette routine permet la lecture d'un type d'élément d'un maillage non structuré pour une séquence de calcul donnée." \
"MEDmeshElementRdDetails=Cette routine permet la lecture d'un type d'entité d'un maillage non structuré pour une séquence de calcul donnée. Les données lues sont le tableau des connectivités, les noms (optionnel), les numéros (optionnel), les numéros de familles (optionnel si tous égaux à 0). Les booléens associés aux tableaux permettent d'indiquer la présence des données optionnelles." \
\
"MEDnMeshBrief=Cette routine permet de lire le nombre de maillages dans un fichier." \
"MEDnMeshDetails=Cette routine permet de lire le nombre de maillages dans un fichier tous types de maillages confondus." \
\
"MEDmeshEntityInfoBrief=Cette routine indique de façon itérative les types géométriques disponibles dans un maillage." \
"MEDmeshEntityInfoDetails=Cette routine indique de façon itérative les types géométriques disponibles pour les entités de type \a enttype au pas de temps \a numdt, \a numit du maillage \a meshname. Le nom du type géométrique est également renvoyé dans \a geotypename." \
\
"MEDmeshNodeCoordinateTrsfWrBrief=Cette routine définit les paramètres de translation rotation à appliquer aux noeuds de la séquence de calcul \a numdt \a numo du maillage \a meshname." \
"MEDmeshNodeCoordinateTrsfDetails= Les trois premiers paramètres définissent la translation à appliquer selon l'ordre des axes définis pour le maillage.\
 Les quatres suivants définissent une rotation phi par le quarternion (p4,p5-7) où p4 est le scalaire et p5-7 le vecteur décrit suivant\
 l'ordre des axes définis pour le maillage" \
"MEDmeshNodeCoordinateTrsfWrDetails=\MEDmeshNodeCoordinateTrsfWrBrief \
\MEDmeshNodeCoordinateTrsfDetails" \
"MEDmeshNodeCoordinateTrsfRm1=Si un profil est défini, la transformation s'applique à tous ses noeuds." \
"MEDmeshNodeCoordinateTrsfRm2=La définition d'une transformation est exclusive avec la définition de nouvelles coordonnées." \
"MEDmeshNodeCoordinateTrsfRm3=S'il y a moins de trois axes définis, les paramètres inutiles à la transformation doivent être à zéro." \
"MEDmeshNodeCoordinateTrsfRdBrief=Cette routine lit les paramètres de translation rotation à appliquer aux noeuds de la séquence de calcul \a numdt \a numo\
 du maillage \a meshname." \
"MEDmeshNodeCoordinateTrsfRdDetails=\MEDmeshNodeCoordinateTrsfRdBrief \MEDmeshNodeCoordinateTrsfDetails" \
\
"MEDmeshComputationStepDtRdBrief=Cette routine lit dans un maillage la valeur d'un pas de temps pour un pas de temps et un numéro d'ordre donné." \
"MEDmeshComputationStepDtRdDetails=Cette routine lit dans un maillage la valeur d'un pas de temps pour un pas de temps et un numéro d'ordre donné." \
\
"MEDmeshSortingTypeRdBrief=Cette routine lit l'ordre de tri des séquences évolutives du maillage." \
"MEDmeshSortingTypeRdDetails=Cette routine lit l'ordre de tri des séquences évolutives du maillage.\
 Le mode de tri (ordre d'accès aux séquences de calcul) consiste à privilégier les pas de temps sur les numéro d'itération (#MED_SORT_DTIT) ou inversement (#MED_SORT_ITDT)." \
\
"MEDmeshEntityAttributeAdvancedBrief{1}=Cette routine permet \1 les attributs optionnels d'entités d'un maillage en utilisant un filtre." \
"MEDmeshEntityAttributeAdvancedDetails{1}=Cette routine permet \1 les attributs optionnels (noms,numéros,numéros de famille) d'entités d'un maillage en utilisant un filtre. " \
"MEDmeshEntityAttributeAdvancedRem=Le type d'attribut optionnel concerné est indiqué par le paramètre datatype qui peut prendre les valeurs suivantes : \n \
\li #MED_NAME : Noms optionnels \
\li #MED_NUMBER : Numéros optionnels \
\li #MED_FAMILY_NUMBER : Numéros de familles " \
\
"MEDsupportMeshCrBrief=Cette routine permet de créer un maillage support." \
"MEDsupportMeshCrDetails=Cette routine permet de créer un maillage support à la définition d'un modèle d'éléments de structure. \
	\li La dimension de l'espace \a spacedim dans lequel évolue le maillage doit être égale à la dimension \a mdim de l'élément de structure \
	\li Le maillage \a meshname est un maillage non structuré dont la dimension \a meshdim est égale à la plus grande dimension des éléments finis support \
	\li Un maillage support peut contenir des noeuds et des mailles d'un seul type géométrique \
	\li La connectivité écrite est forcément nodale \
	\li Le maillage ne doit pas utiliser d'autre séquence de calcul que ( #MED_NO_DT , #MED_NO_IT ) \
	\li les attributs suivants sont optionnels : les numéros de famille (tout les éléments ont un numéro de famille 0 par défaut), la numérotation optionnelle, les noms optionnels \
	\li Les  maillages support ne peuvent pas utiliser les polygones/polyhedres\n " \
"MEDsupportMeshInfoByNameBrief=Cette routine permet de lire les informations relatives à un maillage support en précisant son nom." \
"MEDsupportMeshInfoByNameDetails=Cette routine permet de lire les informations relatives à un maillage support dans un fichier. L'accès au maillage se fait directement via son nom." \
"MEDsupportMeshInfoBrief=Cette routine permet de lire les informations relatives à un maillage support dans un fichier." \
"MEDsupportMeshInfoDetails=Cette routine permet de lire les informations relatives à un maillage support dans un fichier. L'accès au maillage se fait via un itérateur." \
"MEDnSupportMeshBrief=Cette routine permet de lire le nombre de maillages support dans un fichier." \
"MEDnSupportMeshDetails=\MEDnSupportMeshBrief." \
"nvaluesperentity=Nombre de valeurs par entité." \
"nvaluesperentityMEDfilterCm=Utiliser la valeur 1 pour un filtre d'éléments de maillage." \
"nvaluesperentityMEDfilterEx=Cela peut être le nombre de points d'intégration utilisé dans un champ résultat." \
\
"nconstituentpervalue=Nombre de constituants par valeur." \
"nconstituentpervalueMEDfilterEx=Cela peut être le nombre de coordonnées des noeuds,\
 le nombre de noeuds d'une connectivité, le nombre de composantes d'un champ résultat." \
\
"constituentselect=Numéro de constituant des valeurs à filtrer (commence à 1).\
 Le mot clé #MED_ALL_CONSTITUENT permet de selectionner tous les constituants." \
"constituentselectMEDfilterEx=Cela peut être le numéro de coordonnées des noeuds, le numéro de noeuds d'une connectivité,\
 le numéro de composantes d'un champ résultat." \
\
"filterarraysize=Nombre d'entités à filtrer et taille du tableau \a filterarray." \
"filterarraysizeMEDfilterCm=Ne prend pas en compte \a nvaluesperentity et \a nconstituentpervalue." \
\
"filterarray=Tableau d'index du profil des numéros d'entités associées aux valeurs à sélectionner." \
\
"filter=Filtre sur entités (\ref med_filter) appliqué en lecture/écriture de valeurs." \
"filterMEDfilterCm=La désallocation est à la charge de l'utilisateur (cf. \ref MEDfilterClose \ref MEDfilterDeAllocate)." \
"nentityMEDfilterCm=Ce paramètre ne doit pas prendre en compte \a nvaluesperentity et \a nconstituentpervalue." \
\
"start=Index de départ du premier bloc. L'index est relatif à un profil." \
"stride=Ecart constant en nombre d'entités entre les blocs (en comptant à partir de l'indice de départ du bloc précédent). \
 En particulier, écart entre \a start et l'index de départ du bloc suivant." \
"count=Nombre de blocs à sélectionner (0 est possible)." \
"blocksize=Taille de chaque bloc en nombre d'entités." \
"lastblocksize=Taille en nombre d'entités du dernier bloc du dernier processus utilisé (inutilisé si \a count < 2 )." \
"nfilter=Nombre de filtres." \
\
\
\
"MEDfilterBrief=Crée une selection d'entités grâce a un tableau d'index \a filterarray de taille \a filterarraysize. \
 Initialisé en sortie de fonction, le filtre \a filter sera utilisé pour lire/écrire des valeurs associées à ces entités. \
 Ces valeurs peuvent être des coordonnées, des connectivités des valeurs de champs résultats \
 mais aussi des numéros de familles, des noms ou numéros optionnels. \
" \
\
"MEDfilterDetails=\MEDfilterBrief \
 \n \
 \li Les \a nvaluesperentity valeurs associées à chaque entité seront sélectionnées (ex : le nombre de points d'intégration des valeurs d'un champ résultat).\
 \li Il est possible de sélectionner l'ensemble des constituants par valeur ( #MED_ALL_CONSTITUENT )\
 ou uniquement les constituants numéro \a constituentselect (ex : coordonnées n°i de noeuds, composantes n°i des valeurs d'un champ résultat). \
 Les constituants sont lus/écrits en mode #MED_FULL_INTERLACE|#MED_NO_INTERLACE (\ref med_switch_mode).\
 \li Le rangement des valeurs en mémoire peut être en mode #MED_GLOBAL_STMODE ou #MED_COMPACT_STMODE (\ref med_storage_mode).\
 \li Les index définis dans le tableau de filtre (\a filterarray) sont des index relatifs à un tableau profil\
 (eventuellement le tableau virtuel #MED_ALLENTITIES_PROFILE ou #MED_NO_PROFILE ).\
 La numérotation des index commence à 1, comme la numérotation implicite des éléments.\n\
 \li Index et profils :\n \
 Les valeurs à lire/écrire peuvent être définies sur un profil d'entités. \
 L'utilisation d'un profil d'entités permet de restreindre \
 le stockage des valeurs aux seules entités dont l'évolution présente un intérêt. \
 Si les valeurs concernées ne sont pas définies sur un profil d'entités, les index utilisés sont directement les numéros d'entités (équivaut aux index d'un profil #MED_ALLENTITIES_PROFILE). \
 Si les valeurs concernées sont définies sur un profil, les index sont appliqués au profil pour sélectionner indirectement les entités. \
 \
 \li Utilisation sans profils et sans tableau d'index :\n \
 Il est possible d'initialiser un filtre sans utiliser de profil et sans définir un tableau d'index.\
 Le filtre sert alors à selectionner les constituants de numéro \a constituentselect des valeurs.\
"\
 \
"MEDfilterNote=\
 \li Les numéros d'entités selon la numérotation MED implicite sont relatifs au type géométrique de l'entité \
 (la numérotation implicite commence à 1 pour chaque type géométrique différent). \
 \li Les numéros d'entités/index de profil doivent apparaître en ordre croissant. \
 \li Un numéro d'entité/index de profil peut aparaître plusieurs fois (ils sont alors consécutifs).\
 \li Le filtre créé prend en compte les paramètres \a nvaluesperentity et \a nconstituentpervalue\
 pour selectionner les emplacements en mémoire en fonction du mode de stockage utilisé (\ref med_storage_mode).\
 \li La sélection des emplacements mémoire lus/écrits s'opère dans l'espace mémoire d'un seul processus.\
 \li Le filtre créé s'utilise uniquement <b>en mode séquentiel</b> (le \a fid utilisé lors de la lecture/écriture doit provenir d'une ouverture séquentielle).\
" \
\
\
\
"MEDfilterBlockOfEntityBrief=Crée un filtre en selectionnant par blocs les entités\
 pour lesquelles on veut lire/écrire des valeurs.\
 Cette sélection permet une lecture/écriture avancée vers/depuis les emplacements mémoire sélectionnés.\
 Elle s'utilise aussi bien en mode séquentiel qu'en mode parallèle (un ou plusieurs processus)." \
\
"MEDfilterBlockOfEntityDetails=La sélection des emplacements mémoire lus/écrits s'opère dans l'espace\
 mémoire du processus appelant.\
 Les entités sélectionnées le sont avec toutes leurs valeurs (\a nvaluesperentity).\
 Il est possible de sélectionner l'ensemble des constituants par valeur ( #MED_ALL_CONSTITUENT )\
 ou uniquement les constituants n°\a constituentselect . \
 Les constituants sont lus/écrits en mode #MED_FULL_INTERLACE|#MED_NO_INTERLACE (\ref med_switch_mode).\
 Le mode de stockage peut être #MED_GLOBAL_STMODE ou #MED_COMPACT_STMODE (\ref med_storage_mode) :\
 \li En mode #MED_GLOBAL_STMODE, chacun des processus possède l'espace mémoire des valeurs associés à la totalité des entités d'un même type.\
 Dans ce mode, il n'est pas possible d'utiliser un profil utilisateur (car la sélection ne serait plus par bloc).\
 Les seuls profils disponibles sont #MED_ALLENTITIES_PROFILE ou #MED_NO_PROFILE .\
 \li En mode #MED_COMPACT_STMODE, chacun des processus possède un espace mémoire contigü pour lire/écrire\
 les valeurs des entités selectionnées par des blocks d'index.\
 Les blocks décrivent les parties du profil à utiliser.\
 Autrement dit, les blocks d'index créés indiquent les numéros d'entités à selectionner dans le tableau \a profil.\
 La numérotation des index commence à 1.\
 Il est possible d'utiliser un profil utilisateur ou #MED_ALLENTITIES_PROFILE.\
 Si le profil #MED_ALLENTITIES_PROFILE est utilisé, les index sont directement les numéros des entités." \
\
"MEDfilterBlockOfEntityNote=\
 \li Le filtre créé prend en compte les paramètres \a nvaluesperentity et \a nconstituentpervalue\
 pour sélectionner les emplacements en mémoire.\
 \li Si \a count vaut 0 ou 1, \a lastblocksize n'est pas utilisé, le paramètre \a blocksize peut avoir des valeurs différentes par processus.\
 \li Si \a count > 2, chaque processus sélectionne \a count blocks de taille \a blocksize. En utilisant le \a stride\
 il est possible de mettre en oeuvre une sélection par block cyclique. Seul le processus de plus haut rang utilisé\
 (dernier processus utilisé) peut définir une valeur non nulle de \a lastblocksize. Ce paramètre vaudra alors \a blocksize auquel\
 il sera ajouté un résidu non nul. Par exemple : lastblocksize= blocksize + (nentities - blocksize*nproc*nblocks_pproc).\
 \li Il est possible de ne pas faire participer certains processus en indiquant un \a count == 0. Cependant tous les processus du\
 communicateur utilisé doivent faire appel aux même fonctions MED. Il s'agit d'opérations collectives." \
\
\
"MEDfilterAllocateBrief=Alloue un tableau de filtres de taille \a nfilter." \
"MEDfilterAllocateDetails=" \
"MEDfilterAllocateNote=\li Ne pas oublier d'appeler \ref MEDfilterDeAllocate pour libérer les ressources hdf et la mémoire utilisée. \
\li En C, il est possible de déclarer directement une variable de type \ref med_filter que l'on initialise à #MED_FILTER_INIT. \
Il sera alors necessaire d'appeler  \ref MEDfilterClose pour libérer correctement les ressources détenues par le filtre." \
\
\
"MEDfilterDeAllocateBrief=Desalloue un tableau de filtre de taille \a nfilter." \
"MEDfilterDeAllocateDetails=" \
"MEDfilterDeAllocateNote=MEDfilterDeAllocate appelle \ref MEDfilterClose." \
\
\
"MEDfilterCloseBrief=Désalloue les ressources hdf détenues par un filtre." \
"MEDfilterCloseDetails=" \
"MEDfilterCloseNote=MEDfilterClose est surtout destinée à être appelée en C, pour une variable de type \ref med_filter déclarée directement et initialisée à #MED_FILTER_INIT." \
\
\
\
\
\
\
"MEDfilterNoIGNote=Pour cette fonction, la sélection s'opère dans un espace mémoire ou les éléments \
sont stockés uniquement en mode #MED_NO_INTERLACE et #MED_GLOBAL_STMODE." \
\
"MEDfilterFullIGNote=Pour cette fonction, la sélection s'opère dans un espace mémoire ou les éléments \
sont stockés uniquement en mode #MED_FULL_INTERLACE et #MED_GLOBAL_STMODE." \
\
"MEDfilterNoICNote=Pour cette fonction, la sélection s'opère dans un espace mémoire ou les éléments \
sont stockés uniquement en mode #MED_NO_INTERLACE et #MED_COMPACT_STMODE." \
\
"MEDfilterFullIGNote=Pour cette fonction, la sélection s'opère dans un espace mémoire ou les éléments \
sont stockés uniquement en mode #MED_FULL_INTERLACE et #MED_COMPACT_STMODE." \
"hdfversion=Version de la bibliothèque HDF5 codée dans une chaîne de caractères" \
"MEDlibraryStrVersionBrief=Renvoie le numéro de version de la librairie MED dans une chaîne de caractères." \
"MEDlibraryStrVersionDetails=Cette routine retourne le numéro de version de la bibliothèque MED. Le numéro de version est renvoyé sous la forme d'une chaîne de caractères." \
"MEDlibraryNumVersionBrief=Renvoie les 3 numéros de version de la librairie MED." \
"MEDlibraryNumVersionDetails=Cette routine retourne le numéro de version de la bibliothèque MED. Le numéro de version est renvoyé sous la forme de trois entiers (majeur, mineur, release)." \
"MEDlibraryHdfNumVersionBrief=Renvoie les 3 numéros de version de la librairie HDF5 utilisée par MED." \
"MEDlibraryHdfNumVersionDetails=Cette routine retourne le numéro de version de la bibliothèque HDF5 utilisée par MED. Le numéro de version est renvoyé sous la forme de trois entiers (majeur, mineur, release)." \
"MEDlibraryHdfStrVersionBrief=Renvoie le numéro de version de la librairie HDF utilisée par la bibliothèque MED dans une chaîne de caractères." \
"MEDlibraryHdfStrVersionDetails=Cette routine retourne le numéro de version de la bibliothèque HDF utilisée avec la bibliothèque MED. Le numéro de version est renvoyé sous la forme d'une chaîne de caractères." \
"MEDlibraryCloseBrief=Cette routine force l'écriture des données sur disque, nettoie la mémoire et ferme tous les fichiers MED ouverts (de même que tous les fichiers HDF)." \
"MEDlibraryCloseDetails=Cette routine force l'écriture des données sur disque, nettoie la mémoire et ferme tous les fichiers MED ouverts (de même que tous les fichiers HDF). Attention donc à l'emploi de cette routine qui doit utilisée en cas de force majeure dans un contexte d'E/S parallèles." \
"familyname=familyname Nom de la famille de longueur maximum #MED_NAME_SIZE ." \
"familynumber=familynumber Numéro de la famille." \
"ngroup=ngroup Nombre de groupe." \
"groupname=groupname Nom(s) de(s) groupe(s). Chaque nom de groupe est une chaîne de de longueur maximum #MED_LNAME_SIZE caractères." \
"famit=famit Itérateur sur les familles du maillage." \
"attributenumber=attributenumber Liste des identificateurs des attributs." \
"attributevalue=attributevalue Liste des valeurs des attributs." \
"attributedes=attributedes Liste des descripteurs des attributs." \
"nfamily=Nombre de famille dans un maillage." \
"natt=Nombre d'attribut de la famille." \
"ngroup=Nombre de groupe de la famille." \
"MEDfamilyCrBrief=Cette routine permet la création d'une famille portant sur les entités d'un maillage." \
"MEDfamilyCrDetails=Cette routine permet la création d'une famille portant sur les entités d'un maillage. Une famille est composée d'une liste de noms de groupes (éventuellement vide). Chaque nom de groupes comprend #MED_LNAME_SIZE caractères. Les familles sont identifiées par un nom et un numéro avec les caractéristiques suivantes : \
\li Le nom d'une famille est une chaîne d'au plus #MED_NAME_SIZE caractères. Une famille de noeuds peut porter le même nom qu'un famille d'éléments. Par contre les familles d'éléments (respectivement de noeuds) doivent toutes avoir des noms différents. \
\li Le numéro d'une famille de noeuds doit être positif ou nul, le numéro d'une famille d'éléments doit être négatif ou nul. Par convention, la famille de numéro 0 ne comporte aucun groupe. La création de numéro 0 est obligatoire car elle constitue la famille de référence pour tous les noeuds et les éléments qui n'appartiennent à aucun groupe." \
"MEDfamilyInfoBrief=Cette routine permet de lire les informations relatives à une famille d'un maillage." \
"MEDfamilyInfoDetails=Cette routine permet de lire les informations relatives à une famille d'un maillage.  L'accès à la famille se fait via un itérateur. Les informations lues sont : \
\li Le nom de la famille qui est une chaîne d'au plus #MED_NAME_SIZE caractères. Une famille de noeuds peut porter le même nom qu'un famille d'éléments. Par contre les familles d'éléments (respectivement de noeuds) doivent toutes avoir des noms différents. \
\li Le numéro de la famille qui oit être positif ou nul (le numéro d'une famille d'éléments doit être négatif ou nul, par convention la famille de numéro 0 ne comporte aucun groupe).\
\li La liste de groupe de la famille (éventuellement vide). Chaque nom de groupes comprend #MED_LNAME_SIZE caractères. " \
"MEDfamily23InfoBrief=Cette routine permet de lire les informations relatives à une famille d'un maillage créé avec MED 2.3 ou MED 2.2, -i.e. pouvant comporter une liste d'attributs en plus de la liste de groupes." \
"MEDfamily23InfoDetails=Cette routine permet de lire les informations relatives à une famille d'un maillage créé avec MED 2.3 ou MED 2.2, -i.e. pouvant comporter une liste d'attributs en plus de la liste de groupes (la notion d'attribut a disparu avec MED 3.0). L'accès à la famille se fait via un itérateur et les informations lues sont : \
\li Le nom de la famille qui est une chaîne d'au plus #MED_NAME_SIZE caractères. Une famille de noeuds peut porter le même nom qu'un famille d'éléments. Par contre les familles d'éléments (respectivement de noeuds) doivent toutes avoir des noms différents. \
\li Les attributs de la famille fournis sous la forme de 3 listes distinctes : liste des descripteurs entiers (un descripteur entier correspond à un numéro d'ordre dans la liste), liste des valeurs des attributs (un attribut porte une valeur entière), liste des descripteurs (un descripteur est une chaîne de #MED_COMMENT_SIZE caractères). \
\li Le numéro de la famille qui oit être positif ou nul (le numéro d'une famille d'éléments doit être négatif ou nul, par convention la famille de numéro 0 ne comporte aucun groupe). \
\li La liste de groupe de la famille (éventuellement vide). Chaque nom de groupes comprend #MED_LNAME_SIZE caractères." \
"MEDnFamilyBrief=Cette routine permet de lire le nombre de famille dans un maillage." \
"MEDnFamilyDetails=Cette routine permet de lire le nombre de famille dans un maillage." \
"MEDnFamily23AttributeBrief=Cette routine permet de lire le nombre d'attribut dans une famille dans un maillage créé avec MED 2.2 ou 2.3." \
"MEDnFamily23AttributeDetails=Cette routine permet de lire le nombre d'attribut dans une famille dans un maillage créé avec MED 2.2 ou 2.3, -i.e. pouvant comporter une liste d'attributs en plus de la liste de groupes (la notion d'attribut a disparu avec MED 3.0). L'accès à la famille se fait via un itérateur." \
"MEDnFamilyGroupBrief=Cette routine permet de lire le nombre de groupe dans une famille." \
"MEDnFamilyGroupDetails=Cette routine permet de lire le nombre d'attribut dans une famille. L'accès à la famille se fait via un itérateur." \
"equivname=Nom de l'équivalence de longueur maximum #MED_NAME_SIZE ." \
"equivit=Itérateur sur les équivalences du maillage." \
"nocstpncorrespondence=Nombre de tableaux de correspondances sur la séquence de calcul." \
"nentityMEDequiv=Nombre de correspondances." \
"corit=Itérateur sur les tableaux de correspondances. L'itérateur commence à 1." \
"correspondence=Tableau de correspondances sur les entités." \
"ncorrespondence=Nombre de correspondance." \
"equivdescription=\description_" \
"nequiv=Nombre d'équivalence" \
"MEDequivalenceCrBrief=Cette routine permet la création d'une équivalence portant sur les entités d'un maillage." \
"MEDequivalenceCrDetails=Cette routine permet la création d'une équivalence portant sur les entités d'un maillage. Une équivalence est identifiée par son nom et se voit associée une description." \
"MEDequivalenceInfoBrief=Cette routine permet lire les informations d'une équivalence portant sur les entités d'un maillage." \
"MEDequivalenceInfoDetails=Cette routine permet lire les informations d'une équivalence portant sur les entités d'un maillage. L'accès à chaque équivalence se fait via un itérateur. Les informations lues sont : \
\li Le nom de l'équivalence (une équivalence est identifiée par son nom), \
\li La description de l'équivalence, \
\li Le nombre de séquence de calcul dans le maillage sur lesquelles sont dédinies des tableaux de correspondance pour cette équivalence, \
\li Le nombre de tableau de correspondances sur la première séquence de calcul." \
"MEDequivalenceComputingStepInfoBrief=Cette routine permet de lire les informations relatives à une équivalence pour une séquence de calcul donnée." \
"MEDequivalenceComputingStepInfoDetails=Cette routine permet de lire les informations relatives à une équivalence pour une séquence de calcul donnée. L'accès à la séquence de calcul se fait via un itérateur. Les informations lues sont : \
\li Le numéro de pas de temps de la séquence de calcul, \
\li Le numéro d'ordre de la séquence de calcul, \
\li Le nombre de tableau de correspondances dans cette séquence de calcul." \
"MEDequivalenceCorrespondenceSizeBrief=Cette routine permet de lire le nombre de correspondances dans une équivalence pour une séquence de calcul et un type d'entité donnés." \
"MEDequivalenceCorrespondenceSizeDetails=Cette routine permet de lire le nombre de correspondances dans une équivalence pour une séquence de calcul et un type d'entité donnés. Le type géométrique peut être : \
\li Pour les noeuds (MED_NODE) : MED_NONE. \
\li Pour les mailles (MED_CELL) : MED_POINT1, MED_SEG2, MED_SEG3, MED_TRIA3, MED_TRIA6, MED_QUAD4, MED_QUAD8, MED_POLYGON. \
\li Pour les faces (MED_DESCENDING_FACE) : MED_TRIA3, MED_TRIA6, MED_QUAD4, MED_QUAD8, MED_POLYGON. \
\li Pour les arêtes (MED_DESCENDING_EDGE) : MED_SEG2 et MED_SEG3." \
"MEDequivalenceCorrespondenceSizeInfoBrief= Cette routine permet de lire les informations relatives à un tableau de correspondances dans une équivalence pour une séquence de calcul donnée." \
"MEDequivalenceCorrespondenceSizeInfoDetails= Cette routine permet de lire les informations relatives à un tableau de correspondances dans une équivalence pour une séquence de calcul donnée. L'accès au tableau de correspondances se fait via un itérateur, les informations lues sont : le type d'entité, le type géométrique de l'entité, le nombre de correspondances.  Le type géométrique peut être : \
\li Pour les noeuds (MED_NODE) : MED_NONE. \
\li Pour les mailles (MED_CELL) : MED_POINT1, MED_SEG2, MED_SEG3, MED_SEG4, MED_TRIA3, MED_TRIA6, MED_TRIA7, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les faces (MED_DESCENDING_FACE) : MED_TRIA3, MED_TRIA6, MED_TRIA7, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les arêtes (MED_DESCENDING_EDGE) : MED_SEG2, MED_SEG3, MED_SEG4." \
"MEDequivalenceCorrespondenceRdBrief=Cette routine permet de lire un tableau de correspondances entre les entités d'un maillage dans une équivalence pour une séquence de calcul et un type d'entité donnés." \
"MEDequivalenceCorrespondenceRdDetails=Cette routine permet de lire un tableau de correspondances entre les entités d'un maillage dans une équivalence pour une séquence de calcul et un type d'entité donnés. Le tableau des correspondances est un tableau à 1 dimension où les correspondances sont rangées 2 à 2. Le type géométrique peut être : \
\li Pour les noeuds (MED_NODE) : MED_NONE. \
\li Pour les mailles (MED_CELL) : MED_POINT1, MED_SEG2, MED_SEG3, MED_SEG4, MED_TRIA3, MED_TRIA6, MED_TRIA7, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les faces (MED_DESCENDING_FACE) : MED_TRIA3, MED_TRIA6, MED_TRIA7, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les arêtes (MED_DESCENDING_EDGE) : MED_SEG2, MED_SEG3, MED_SEG4." \
"MEDequivalenceCorrespondenceWrBrief=Cette routine permet d'écrire un tableau de correspondances entre les entités d'un maillage dans une équivalence pour une séquence de calcul et un type d'entité donnés." \
"MEDequivalenceCorrespondenceWrDetails=Cette routine permet d'écrire un tableau de correspondances entre les entités d'un maillage dans une équivalence pour une séquence de calcul et un type d'entité donnés. Le tableau des correspondances est un tableau à 1 dimension où les correspondances sont rangées 2 à 2. Le type géométrique peut être : \
\li Pour les noeuds (MED_NODE) : MED_NONE. \
\li Pour les mailles (MED_CELL) : MED_POINT1, MED_SEG2, MED_SEG3, MED_SEG4, MED_TRIA3, MED_TRIA6, MED_TRIA7, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les faces (MED_DESCENDING_FACE) : MED_TRIA3, MED_TRIA6, MED_TRIA7, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les arêtes (MED_DESCENDING_EDGE) : MED_SEG2, MED_SEG3, MED_SEG4." \
"MEDnEquivalenceBrief=Cette routine permet de lire le nombre d'équivalence dans un fichier." \
"MEDnEquivalenceDetails=Cette routine permet de lire le nombre d'équivalence dans un fichier." \
"fieldname=Nom du champ, de longueur maximum #MED_NAME_SIZE ." \
"csit=Itérateur sur les séquences de calcul. L'itérateur commence à 1." \
"numdt=Numéro de pas de temps de l'étape de calcul (#MED_NO_DT si pas de numéro de pas de temps)." \
"numit=Numéro d'itération de l'étape de calcul (#MED_NO_IT si pas de numéro d'itération)." \
"meshnumdt=Numéro de pas de temps de l'étape de calcul du maillage associé (#MED_NO_DT si pas de pas de temps)." \
"meshnumit=Numéro d'itération de l'étape de calcul du maillage associé  (#MED_NO_IT si pas de numéro d'itération)." \
"dt=Date du pas de temps si le numéro de pas de temps est différent de #MED_NO_DT." \
"fieldtype=Type numérique des composantes du champ." \
"ncomponent=Nombre de composantes." \
"componentname=Nom des composantes du champ. Les noms des composantes sont définis dans une chaîne de \a ncomponent * #MED_SNAME_SIZE caractères." \
"ind=Itérateur. L'itérateur commence à 1." \
"componentunit=Unité des composantes du champ. Les noms des unités des composantes sont définis dans une chaîne de \a ncomponent * #MED_SNAME_SIZE caractères." \
"dtunit=Unité des dates des séquences de calcul du champ. Elle est définie dans une chaîne de taille #MED_SNAME_SIZE ." \
"localmesh=Indicateur de localisation du maillage : #MED_TRUE si le maillage est dans le même fichier que le champ, #MED_FALSE si le maillage est dans un autre fichier." \
"ncstp=Nombre de séquences de calcul dans le champ." \
"value=Tableau des valeurs." \
"nvalue=Nombre de valeurs." \
"nfield=Nombre de champs." \
"ncomponent=Nombre de composantes." \
"nprofile=Nombre de profils." \
"defaultlocalizationname=Nom de fonction de localisation par défaut, de longueur maximum #MED_NAME_SIZE , (#MED_NO_LOCALIZATION si pas de fonction de localisation)". \
"nintegrationpoint=Nombre de points d'intégation (1 par défaut)" \
"localizationname=Nom de fonction de localisation, de longueur maximum #MED_NAME_SIZE , (#MED_NO_LOCALIZATION si pas de fonction de localisation)". \
\
\
"MEDfieldComputingStepInfoBrief=Cette fonction permet de lire les informations caractérisant une séquence de calcul : numéro de pas de temps, numéro d'ordre." \
"MEDfieldComputingStepInfoDetails=\MEDfieldComputingStepInfoBrief Une fois le nombre d'étapes de calcul connu par appel à \ref MEDfieldInfo ou \ref MEDfieldInfoByName, il est possible de lire les informations caractérisant chaque étape en itérant sur séquences de calcul successives. Une séquence de calcul est identifiée par un couple : \
\li numéro de pas de temps \a numdt (#MED_NO_DT si pas de pas de temps)\
\li numéro d'itération \a numit (#MED_NO_IT si pas de numéro d'itération).\
\n\
" \
"MEDfieldComputingStepInfoRem1=L'ordre d'apparition des étapes de calcul au cours des itérations est celui de leur création." \
\
"MEDfieldComputingStepMeshInfoBrief=\MEDfieldComputingStepInfoBrief Elle indique également l'étape de calcul utilisée par le maillage associé." \
"MEDfieldComputingStepMeshInfoDetails=\MEDfieldComputingStepMeshInfoBrief \
\li numéro de pas de temps (#MED_NO_DT si pas de pas de temps)\
\li numéro d'itération (#MED_NO_IT si pas de numéro d'itération).\
\n\
" \
\
"MEDfieldComputingStepMeshWrBrief=Cette fonction permet de définir l'étape de calcul  ( \a meshnumdit , \a meshnumit ) à utiliser pour le maillage \
associé au champ résultat à l'étape de calcul ( \a numdit , \a numit )." \
"MEDfieldComputingStepMeshWrDetails=\MEDfieldComputingStepMeshWrBrief \
\li numéro de pas de temps (#MED_NO_DT si pas de pas de temps)\
\li numéro d'itération (#MED_NO_IT si pas de numéro d'itération).\
\n\
" \
\
"MEDfieldCrBrief=Cette fonction crée un champ dans un fichier." \
"MEDfieldCrDetails=\MEDfieldCrBrief  Un champ est composé d'une ou plusieurs composantes scalaires. A chaque composante est associé un nom et une unité. Le type des valeurs des composantes peut être au choix (\ref med_field_type) : \
\li #MED_FLOAT64 : flottant 64 bits, \
\li #MED_FLOAT32 : flottant 32 bits, \
\li #MED_INT32 : entier 32 bits, \
\li #MED_INT64 : entier 64 bits.\
\li #MED_INT   : entier #MED_INT32 ou #MED_INT64 selon la configuration du #med_int.\
\n\
" \
"MEDfieldCrRem=\
\
     <b>Depuis la 3.3.0</b> en plus des types #MED_FLOAT64, #MED_INT32 et #MED_INT64, les types #MED_FLOAT32 et #MED_INT sont autorisés. \
     Aux types med_int et #med_float64 utilisés en C sont ajoutés les types #med_float32, #med_int32 et #med_int64 \
     (si la plateforme possède des entiers 64bits testé à la configuration). \
<ul>									\
<li>   A l'écriture :							\
       <ul>								\
      <li>si #med_int = int  les champs #MED_INT32 sont toujours  stockés              en 32bits  (utiliser #med_int32 ou #med_int   ) \
      <li>si #med_int = int  les champs #MED_INT64 sont désormais autorisés et stockés en 64bits  (utiliser #med_int64              ) \
      <li>si #med_int = int  les champs #MED_INT   sont désormais acceptés  et stockés en 32bits  (utiliser #med_int   ou #med_int32 ) \
      <li>si #med_int = long les champs #MED_INT32 sont désormais stockés              en 32bits  (utiliser #med_int32) \
      <li>si #med_int = long les champs #MED_INT64 sont toujours  autorisés et stockés en 64bits  (utiliser #med_int64 ou #med_int ) \
      <li>si #med_int = long les champs #MED_INT   sont désormais acceptés  et stockés en 64bits  (utiliser #med_int ou #med_int64 ) \
       </ul>								\
									\
<li>   A la lecture :							\
       <ul>								\
      <li>si #med_int = int  les champs #MED_INT32 sont toujours    lus                en 32bits                 (utiliser #med_int32 ou #med_int) \
      <li>si #med_int = int  les champs #MED_INT64 sont acceptés et lus                en 64bits sans conversion (utiliser #med_int64) \
      <li>si #med_int = int  les champs #MED_INT   sont acceptés et lus                en 32bits avec conversion si necessaire (0 si > maxint32 , utiliser #med_int ou #med_int32) \
      <li>si #med_int = long les champs #MED_INT32 sont toujours    lus                en 32bits sans conversion (utiliser #med_int32) \
      <li>si #med_int = long les champs #MED_INT64 sont toujours    lus                en 64bits sans conversion (utiliser #med_int64 ou #med_int) \
      <li>si #med_int = long les champs #MED_INT   sont acceptés et lus                en 64bits avec conversion  si necessaire (utiliser #med_int ou #med_int64) \
       </ul>								\
</ul>									\
<br>									\
   Sur un Unix 32 bits sur architecture 64bits, il est possible d'utiliser des #MED_INT64, l'étape de configuration vérifie l'existence ou définie le type C int64_t. \
   A lecture d'un fichier < 3.3.0 avec une bibliothèque >= 3.3.0 configurée avec #med_int = long : \
   <ul>									\
     <li>Si le fichier lu contient un champ #MED_INT32, les bibliothèques < 3.3.0 relisaient en se basant sur la taille 64 bits des med_int. \
         Les tableaux étaient donc alloués en fonction de la taille du med_int, ceci n'est plus le cas pour les champs #MED_INT32 ou #MED_INT64. \
   </ul>								\
   En Fortran pour les champs MED_INT64 utiliser le type integer*8 et pour les champs MED_INT32 utiliser le type integer*4. \
 <br>									\
 <br>									\
  <b>Avant la 3.3.0</b> seuls les types : #MED_FLOAT64, #MED_INT32 et #MED_INT64 étaient autorisés dans MEDfieldCr et seuls les types #med_int et #med_float64 pouvaient être utilisés en C. \
     La configuration du #med_int était prédominante sur le choix du type de champ pour définir la taille du stockage à utiliser. \
     Il faut garder à l'esprit que les étapes d'écriture et de lecture ne se font pas forcément avec la même configuration de #med_int.	\
 <ul>									\
 <li>  A l'écriture :							\
      <ul>								\
      <li>si #med_int = int  les champs #MED_INT32 sont stockés   en 32bits \
      <li>si #med_int = int  les champs #MED_INT64 sont interdits	\
      <li>si #med_int = long les champs #MED_INT32 sont stockés   en 64bits \
      <li>si #med_int = long les champs #MED_INT64 sont stockés   en 64bits \
      </ul>								\
									\
<li>   A la lecture :							\
       <ul>								\
      <li>si #med_int = int  les champs #MED_INT32 sont lus       en 32bits avec conversion 64->32 s'il avait été stocké en 64bits (configuration écriture #med_int = long) \
      <li>si #med_int = int  les champs #MED_INT64 ne pouvaient pas être lu (pour prevenir la perte d'information) \
      <li>si #med_int = long les champs #MED_INT32 sont lus       en 64bits avec conversion 32->64 s'il avait été stocké en 32bits (configuration écriture #med_int = int) \
      <li>si #med_int = long les champs #MED_INT64 sont lus       en 64bits \
       </ul>								\
</ul>									\
									\
									\
									\
" 									\
"MEDfieldInfoBrief=Cette fonction permet de lire les informations concernant le champ d'indice \a ind ." \
"MEDfieldInfoDetails=\MEDfieldInfoBrief Les informations lues sont : \
\li Nom du champ, \
\li Nom du maillage associé, \
\li Localisation du maillage : dans le même fichier ou non (\ref med_bool ), \
\li Type des valeurs des composantes du champ (\ref med_field_type ), \
\li Nom et unité des composantes, \ 
\li Unité des pas de temps, \
\li Nombre de séquences de calcul.\
\n\
" \
\
"MEDfieldInfoByNameBrief=Cette fonction permet de lire les informations concernant le champ de nom \a fieldname." \
"MEDfieldInfoByNameDetails=\MEDfieldInfoByNameBrief. Les informations lues sont : \
\li Nom du maillage associé, \
\li Localisation du maillage : dans le même fichier ou non (\ref med_bool ), \
\li Type des valeurs des composantes du champ (\ref med_field_type ), \
\li Nom et unité des composantes, \ 
\li Unité des pas de temps, \
\li Nombre de séquences de calcul.\
\n\
" \
\
"MEDfieldValueAdvancedRdBrief=Cette fonction permet de lire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul et selon un filtre donnés." \
"MEDfieldValueAdvancedRdDetails=Cette fonction permet de lire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul et selon un filtre donnés. Cette fonction est une fonction dite avancée car le paramètre correspondant au filtre permet de sélectionner finement les données lues en mode séquentiel ou parallèle : avec ou sans profil, mode d'entrelacement, par blocs, etc. " \
"MEDfieldValueAdvancedRdRem=\MEDfieldCrRem " \
\
"MEDfieldValueAdvancedWrBrief=Cette fonction permet d'écire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul et selon un filtre donnés." \
"MEDfieldValueAdvancedWrDetails=Cette fonction permet d'écrire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul et selon un filtre donnés. Cette fonction est une fonction dite avancée car le paramètre correspondant au filtre permet de sélectionner finement les données lues en mode séquentiel ou parallèle : avec ou sans profil, mode d'entrelacement, par blocs, etc. " \
"MEDfieldValueAdvancedWrRem=\MEDfieldCrRem " \
\
"MEDfieldValueRdBrief=Cette fonction permet de lire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul donnée (pas de gestion de profil)." \
"MEDfieldValueRdDetails=Cette fonction permet de lire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul donnée (pas de gestion de profil)." \
"MEDfieldValueRdRem=\MEDfieldCrRem " \
\
"MEDfieldValueWrBrief=Cette fonction permet d'écrire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul donnée (pas de gestion de profil)." \
"MEDfieldValueWrDetails=Cette fonction permet d'écrire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul donnée (pas de gestion de profil)." \
"MEDfieldValueWrRem=\MEDfieldCrRem " \
\
"MEDfieldValueWithProfileRdBrief=Cette fonction permet de lire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul et un profil donnés." \
"MEDfieldValueWithProfileRdDetails=Cette fonction permet de lire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul et un profil donnés. Le profil est identifié par un nom et le mode de stockage des données en mémoire peut être paramétré : compact ou global." \
\
"MEDfieldValueWithProfileWrBrief=Cette fonction permet d'écrire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul et un profil donnés." \
"MEDfieldValueWithProfileWrDetails=Cette fonction permet d'écrire les valeurs d'un champ définies sur des entités d'un maillage pour une séquence de calcul et un profil donnés. Le profil est identifié par un nom et le mode de stockage des données en mémoire peut être paramétré : compact ou global." \
\
"MEDfieldnComponentBrief=Cette fonction lit le nombre de composantes d'un champ." \
"MEDfieldnComponentDetails=Cette fonction lit le nombre de composantes d'un champ. L'indice correspond à l'indice du champ dans le fichier." \
\
"MEDfieldnComponentByNameBrief=Cette fonction lit le nombre de composantes d'un champ (accès direct à partir du nom du champ)." \
"MEDfieldnComponentByNameDetails=Cette fonction lit le nombre de composantes d'un champ. L'accès direct au champ se fait à partir de son nom." \
\
"MEDfieldnProfileBrief=Cette fonction permet de lire le nombre de profils référencés dans un champ pour une séquence de calcul, et un type d'entité donnés." \
"MEDfieldnProfileDetails=Cette fonction permet de lire le nombre de profils référencés dans un champ pour une séquence de calcul, et un type d'entité donnés. Si un seul nom de profil et un seul nom de localisation d'intégration sont présents, on accède directement à ces noms par l'intermédiaire des deux noms par défaut qui sont renvoyés." \
\
"MEDfieldnValueBrief=Cette fonction permet de lire le nombre de valeurs dans un champ pour une séquence de calcul, et un type d'entité donnés (pas de gestion des profils)." \
"MEDfieldnValueDetails=Cette fonction permet de lire le nombre de valeurs dans un champ pour une séquence de calcul, et un type d'entité donnés (pas de gestion des profils). Ce nombre de valeurs permet de calculer la zône mémoire à allouer en vue de lire ces données (à savoir le nombre de valeurs * nombre de composantes du champ)." \
\
"MEDfieldnValueWithProfileBrief=Cette fonction permet de lire le nombre de valeurs à lire dans un champ pour une séquence de calcul, et un type d'entité donnés pour un profil donné." \
"MEDfieldnValueWithProfileDetails=Cette fonction permet de lire le nombre de valeurs à lire dans un champ pour une séquence de calcul, et un type d'entité donnés selon un profil donné. Ce nombre de valeurs permet de calculer la zône mémoire à allouer en vue de lire ces données (à savoir le nombre de valeurs * nombre de composantes du champ * nombre de point d'integration)." \
\
"MEDfieldnValueWithProfileByNameBrief=Cette fonction permet de lire le nombre de valeurs à lire dans un champ pour une séquence de calcul, et un type d'entité donnés pour un profil donné (accès direct au champ via son nom)." \
"MEDfieldnValueWithProfileByNameDetails=Cette fonction permet de lire le nombre de valeurs à lire dans un champ pour une séquence de calcul, et un type d'entité donnés selon un profil donné (accès direct au champ via son nom). Ce nombre de valeurs permet de calculer la zône mémoire à allouer en vue de lire ces données (à savoir le nombre de valeurs * nombre de composantes du champ * nombre de point d'integration)." \
\
"MEDnFieldBrief=Cette fonction permet de lire le nombre de champs dans un fichier." \
"MEDnFieldDetails=Cette fonction permet de lire le nombre de champs dans un fichier." \
"link=Lien vers le fichier contenant le maillage." \
"linkit=Itérateur sur les liens du fichier. Un itérateur commence à 1." \
"linksize=Taille de la chaîne de caractères correspondant au lien." \
"nlink=Nombre de lien dans le fichier." \
"MEDlinkWrBrief=Cette routine permet d'écrire un lien dans un fichier MED." \
"MEDlinkWrDetails=Cette routine permet d'écrire un lien dans un fichier MED. Dans MED, un champ et un maillage peuvent être dans deux fichiers différents. Dans le fichier contenant le champ, il est nécessaire de créer un lien spécifiant le chemin d'accès et le nom du fichier MED contenant le maillage. Le lien porte le nom du maillage." \
"MEDlinkInfoBrief=Cette routine permet de lire les informations sur un lien dans un fichier MED." \
"MEDlinkInfoDetails=Cette routine permet de lire les informations sur un lien dans un fichier MED. L'accès au lien se fait via un itérateur. Les informations lues sont le nom du lien (nom du maillage dans le fichier référencé par le lien) ainsi que la taille de la chaîne de caractères correspondant au lien." \
"MEDlinkInfoByNameBrief=Cette routine permet de lire les informations sur un lien dans un fichier MED." \
"MEDlinkInfoByNameDetails=Cette routine permet de lire les informations sur un lien dans un fichier MED. L'accès au lien se fait via le nom du lien (nom du maillage dans le fichier référencé par le lien). Le résultat renvoyé correspond à la taille de la chaîne de caractères correspondant au lien." \
"MEDlinkRdBrief=Cette routine permet de lire un lien dans un fichier MED." \
"MEDlinkRdDetails=Cette routine permet de lire un lien dans un fichier MED. Dans MED, un champ et un maillage peuvent être dans deux fichiers différents. Dans le fichier contenant le champ, il est nécessaire de créer un lien spécifiant le chemin d'accès et le nom du fichier MED contenant le maillage. Le lien porte le nom du maillage." \
"MEDnLinkBrief=Cette routine permet la lecture du nombre de lien dans un fichier MED." \
"MEDnLinkDetails=Cette routine permet la lecture du nombre de lien dans un fichier MED. Dans MED, un champ et un maillage peuvent être dans deux fichiers différents. Dans le fichier contenant le champ, il est nécessaire de créer un lien spécifiant le chemin d'accès et le nom du fichier MED contenant le maillage. Le lien porte le nom du maillage." \
"nJoint=Nombre de joint." \
"localmeshname=Nom du maillage local, de longueur maximum #MED_NAME_SIZE ." \
"jointname=Nom du joint, de longueur maximum #MED_NAME_SIZE ." \
"domainnumber=Numéro du sous-domaine distant." \
"remotemeshname=Nom du maillage distant, de longueur maximum #MED_NAME_SIZE ." \
"jointit=Itérateur sur les joints. Un itérateur a pour valeur initiale 1." \
"localentitype=Type des entités du sous-domaine local mises en correspondance " \
"localgeotype=Type géométrique des entités du sous-domaine local." \
"remoteentitype=Type des entités du sous-domaine en vis à vis mises en correspondance." \
"remotegeotype=Type géométrique des entités du sous-domaine en vis à vis." \
"nentitycor=Nombre d'entités en correspondance." \
"MEDnSubdomainJointBrief=Cette routine permet la lecture du nombre de joint dans un maillage." \
"MEDnSubdomainJointDetails=Cette routine permet la lecture du nombre de joint dans un maillage." \
"MEDsubdomainJointCrBrief=Cette routine permet de créer un joint dans un maillage." \
"MEDsubdomainJointCrDetails=Cette routine permet de créer un joint dans un maillage. Un joint est identifié par son nom. On précise à la création du joint les noms des maillages local et distant auquel il se rapporte ainsi que le numéro du domaine distant." \
"MEDsubdomainJointInfoBrief=Cette routine permet de lire les informations sur un joint dans un maillage." \
"MEDsubdomainJointInfoDetails=Cette routine permet de lire les informations sur un joint dans un maillage. L'accès au joint se fait via un itérateur. Les informations lues sont : le nom du joint, la description associée au joint, le numéro du domaine distant, le nom du maillage distant, le nombre de séquence de calcul, le nombre de types d'entités en correspondance pour la première séquence de calcul." \
"MEDsubdomainCorrespondenceWrBrief=Cette routine permet l'écriture d'une correspondance dans un joint pour un type de couple d'entité en regard et une séquence de calcul donnés." \
"MEDsubdomainCorrespondenceWrDetails=Cette routine permet l'écriture d'un tableau de correspondance dans un joint pour un type de couple d'entité en regard, les correspondances y sont rangées 2 à 2. Le type géométrique peut être : \
\li Pour les noeuds (MED_NODE) : MED_NONE. \
\li Pour les mailles (MED_CELL) : MED_POINT1, MED_SEG2, MED_SEG3, MED_SEG4, MED_TRIA3, MED_TRIA7, MED_TRIA6, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les faces (MED_DESCENDING_FACE) : MED_TRIA3, MED_TRIA6, MED_TRIA7, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les arêtes (MED_DESCENDING_EDGE) : MED_SEG2, MED_SEG3, MED_SEG4." \
"MEDsubdomainCorrespondenceRdBrief=Cette routine permet la lecture d'une correspondance dans un joint pour un type de couple d'entité en regard et une séquence de calcul donnés." \
"MEDsubdomainCorrespondenceRdDetails=Cette routine permet la lecture d'un tableau de correspondance dans un joint pour un type de couple d'entité en regard, les correspondances y sont rangées 2 à 2. Le type géométrique peut être : \
\li Pour les noeuds (MED_NODE) : MED_NONE. \
\li Pour les mailles (MED_CELL) : MED_POINT1, MED_SEG2, MED_SEG3, MED_SEG4, MED_TRIA3, MED_TRIA7, MED_TRIA6, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les faces (MED_DESCENDING_FACE) : MED_TRIA3, MED_TRIA6, MED_TRIA7, MED_QUAD4, MED_QUAD8, MED_QUAD9, MED_POLYGON. \
\li Pour les arêtes (MED_DESCENDING_EDGE) : MED_SEG2, MED_SEG3, MED_SEG4." \
"MEDsubdomainComputingStepInfoBrief=Cette routine permet de lire les informations sur les correspondances entre types d'entités dans un maillage pour chaque séquence de calcul." \
"MEDsubdomainComputingStepInfoDetails=Cette routine permet de lire les informations sur les correspondances entre types d'entités dans un maillage pour chaque séquence de calcul. L'accès à chaque correspondance se fait via un itérateur, les informations lues sont : le numéro d'itération, le pas de temps et le nombre de correspondances." \
"MEDsubdomainCorrespondenceSizeBrief=Cette routine permet la lecture du nombre d'entités en correspondance dans un joint pour un couple d'entités et une séquence de calcul donnés." \
"MEDsubdomainCorrespondenceSizeDetails=Cette routine permet la lecture du nombre d'entités en correspondance dans un joint pour un couple d'entités et une séquence de calcul donnés." \
"MEDsubdomainCorrespondenceSizeInfoBrief=Cette routine permet de lire les informations sur les couples d'entités en correspondance dans un joint pour une séquence de calcul donnée." \
"MEDsubdomainCorrespondenceSizeInfoDetails=Cette routine permet de lire les informations sur les couples d'entités en correspondance dans un joint pour une séquence de calcul donnée. L'accès aux correspondances se fait via un itérateur, les informations lues sont : les types d'entités en correspondance entre les sous-domaines et le nombre d'entités en correspondance." \
"MEDjointDef=Du point de vue du stockage, un maillage distribué a la même structure qu'un maillage MED classique mais ses composantes (entités géométrique, familles, groupes) peuvent être réparties sur plusieurs sous-domaines affectés à des processeurs disjoints. Lors de cette distribution certains sommets, faces, arêtes ou mailles se retrouvent sur la frontière commune de deux sous-domaines. L'ensemble de ces éléments communs à deux sous-domaines constitue un joint. Dans un cadre très général, les éléments communs à deux sous-domaines peuvent apparaître comme : \
\li La jointure de deux maillages qui se correspondent parfaitement : on parle alors de  raccordement conforme, \
\li La jointure de deux maillages de pavage différent :  raccordement non conforme, \ 
\li Le recouvrement de deux maillages qu'il soit conforme ou non. " \
"localizationname=Nom de la localisation, de longueur maximum #MED_NAME_SIZE ." \
"weight=Poids des points d'intégration." \
"spacedimension=Dimension de l'espace des coordonnées des points d'intégration." \
"elementcoordinate=Coordonnées des noeuds de l'éléments de référence." \
"nipoint=Nombre de points d'intégration dans l'élément de référence." \
"ipointcoordinate=Coordonnées des points d'intégration." \
"localizationit=Itérateur sur les localisations. La valeur initiale d'un itérateur est 1." \
"geointerpname=Nom de la fonction de transformation géométrique (#MED_NO_INTERPOLATION si pas de transformation)." \
"sectionmeshname=Nom du maillage support sectionnant l'élément de structure à chaque point d'intégration (#MED_NO_MESH_SUPPORT si pas de section)." \
"nsectionmeshcell=Nombre de maille dans le maillage support section (0 si pas de section)." \
"sectiongeotype=Type géométrique des mailles du maillage support section (#MED_UNDEF_GEOTYPE si pas de section). " \
"nlocalization=Nombre de localisations de points d'intégration dans le fichier." \
\
\
\
\
"MEDlocalizationWrBrief=Cette routine permet l'écriture d'une localisation \a localizationname de\
 points d'intégration dans/autour d'un élément de référence de dimension \a spacedimension." \
\
"MEDlocalizationDetails=\
 L'élément de référence de type \a geotype est décrit par les coordonnées \a elementcoordinate de ses noeuds\
 dans un repère absolu de dimension \a spacedimension.\n\
 Les \a nipoints points d'intégrations sont positionnés dans l'élément de référence par leurs coordonnées\
 \a ipointcoordinate. Les poids associés à chacun des points d'intégration est spécifié par le tableau \a weight.\n" \
\
"MEDlocalizationWrDetails= \MEDlocalizationWrBrief \n \MEDlocalizationDetails" \
\
"MEDlocalizationRdBrief=Cette routine permet la lecture d'une localisation \a localizationname de\
 points d'intégration dans/autour d'un élément de référence de dimension \a spacedimension." \
\
"MEDlocalizationRdDetails=\MEDlocalizationRdBrief \n\
 L'élément de référence est décrit par les coordonnées \a elementcoordinate de ses noeuds\
 dans un repère absolu de dimension \a spacedimension.\n\
 Les points d'intégrations sont positionnés dans l'élément de référence par leurs coordonnées\
 \a ipointcoordinate. Les poids associés à chacun des points d'intégration est spécifié par le tableau \a weight.\n" \
\
"MEDlocalizationInfoByNameBrief=Cette routine permet d'obtenir la description d'une localisation de points d'intégration nommée \a localizationname." \
"MEDlocalizationInfoByNameDetails=\MEDlocalizationInfoByNameBrief \n\
 Les données lues sont :\n\
 \li le type géométrique de l'élément \li la dimension de l'espace du repère des coordonnées \li le nombre de points d'intégration\
 \li le nom de la transformation géométrique \li le nom de maillage support à la section d'élément de structure\
 \li le nombre de maille de ce maillage support et le type géométrique de ces mailles." \
\
"MEDlocalizationInfoBrief=Cette routine permet d'obtenir la description de la localisation de points d'intégration n° \a localizationit." \
"MEDlocalizationInfoDetails=\MEDlocalizationInfoBrief \n\
 Les données lues sont :\n\
 \li le nom de la localisation \li le type géométrique de l'élément \li la dimension de l'espace du repère des coordonnées \li le nombre de points d'intégration\
 \li le nom de la transformation géométrique \li le nom de maillage support à la section d'élément de structure\
 \li le nombre de maille de ce maillage support et le type géométrique de ces mailles." \
\
"MEDnLocalizationBrief=Cette routine permet de lire le nombre de localisations de points d'intégration contenues dans un fichier." \
"MEDnLocalizationDetails=\MEDnLocalizationBrief." \
\
\
\
\
"MEDlocalizationDef=\
Dans le cadre des échanges de champs de résultats exprimés sur des points d'intégration,\
 MED permet la localisation de ces points dans des éléments de référence en des lieux définis par la modélisation numérique choisie.\
 Pour chaque type de modélisation, il est possible de spécifier nominativement cette localisation sur des éléments de référence.\
 Chaque point d'intégration est localisé au sein d'un élément de référence par ses coordonnées et se voit associer un poids.\
" \
\
"MEDlocalizationRem=\
\li Si les points d'intégration se confondent avec les noeuds de l'élément, il est inutile de créer une localisation factice avec des poids qui ne signifient rien\
 et des coordonnées des points d'intégration identiques à celles des noeuds.\
 Dans ce cas de figure, il faut utiliser mot clé réservé #MED_GAUSS_ELNO comme nom de localisation à l'écriture des valeurs d'un champ.\
 \li Il est possible d'associer à une localisation de points d'intégration une fonction de transformation/ d'interpolation géométrique qui projette\
 les points de la maille de référence vers ceux de la maille réelle.\
 \li Si le type géométrique \a geotype utilisé est celui d'un #MED_STRUCT_ELEMENT, il est possible d'indiquer l'utilisation\
 d'un maillage support définissant une section du modèle d'élément de structure. Ce maillage support est alors utilisé comme\
 section de l'élément de structure à chaque point d'intégration.\
 Auquel cas un champ utilisant cette localisation définira autant de valeur par élément qu'il y a de mailles dans le maillage section\
 de chaque point d'intégration.\
" \
\
\
\
\
"MEDlocalizationRdWrRem=\
\li Si le type géométrique \a geotype utilisé est celui d'un #MED_STRUCT_ELEMENT, les coordonnées \a elementcoordinate des noeuds de l'élément de référence\
 sont celles des noeuds du maillage support du modèle de cet élément.\
 En effet l'élément de référence est déjà décrit à la définition de l'élément de structure.\
" \
"MEDlocalizationRdRem=\MEDlocalizationRdWrRem \
 Le tableau \a elementcoordinate renvoie les coordonnées des noeuds du mailage support utilisé par le modèle d'élément de structure.\
 Les coordonnées des points d'intégration y sont relatives." \
"MEDlocalizationWrRem=\MEDlocalizationRdWrRem \
 Il n'est pas necessaire de renseigner le tableau \a elementcoordinate.\
 Les coordonnées des points d'intégration sont relatives aux coordonnées des noeuds du mailage support utilisé par le modèle d'élément de structure.\
" \
"modelname=Nom du modèle d'éléments de structure (de taille maximum #MED_NAME_SIZE)" \
"modeldim=La dimension du modèle d'élément de structure" \
"supportmeshname=Nom du maillage support utilisé ou #MED_NO_NAME" \
"sentitytype= #MED_CELL si des mailles sont présentes dans \a supportmeshname, #MED_NODE sinon" \
"sgeotype=Type géométrique des mailles utilisées dans \a supportmeshname ou #MED_NO_GEOTYPE" \
"mgeotype=Type géométrique associé au modèle d'éléments de structure" \
"snnode=Nombre de noeuds du maillage support" \
"sncell=Nombre de mailles du maillage support" \
"constattname=Nom de l'attribut caractéristique constant (de taille maximum #MED_NAME_SIZE)" \
"varattname=Nom de l'attribut caractéristique variable (de taille maximum #MED_NAME_SIZE)" \
"constatttype=Type MED de l'attribut caractéristique constant" \
"varatttype=Type MED de l'attribut caractéristique variable" \
"atttype=Type MED de l'attribut caractéristique" \
"sizeofatttype=Taille du type d'attribut" \
"nStructElement=Nombre de modèles d'éléments de structure" \
"nconstantattribute=Nombre d'attributs caractéristiques constants" \
"anyprofile=Présence d'un profil quelconque" \
"nvariableattribute=Nombre d'attributs caractéristiques variables" \
"mit=Itérateur sur les modèles d'éléments de structure" \
"attit=Itérateur sur les attributs caractéristiques" \
\
"MEDstructElementCrBrief=Cette routine permet de créer un nouveau modèle d'éléments de structure dans un fichier MED." \
\
"MEDstructElementCrDetails= \
	Cette routine permet la création d'un nouveau modèle d'élément de structure nommé \a modelname et de type géométrique associé \a mgeotype. \
La création d'un nouveau modèle d'éléments de structure permet de définir par la suite des éléments de ce type dans les maillages de calcul ( \ref MEDmeshElementConnectivityWr ). \
Le type d'entité de maillage des éléments de structure est #MED_STRUCT_ELEMENT. Le type géométrique de ces éléments est le type  géométrique du modèle utilisé tel que retourné par MEDstructElementCr. \remarks \li Le type géométrique crée est associé au nom du modèle \a modelname, sa valeur est locale au fichier MED." \
\
"MEDstructElementCrmodelnameCm=Les noms de modèles MED_.* sont réservés au modèle MED. " \
\
"MEDstructElementCrsupportmeshnameCm1=Le maillage support MED_NO_NAME indique que le support est constitué d'un noeud du maillage de calcul (implique \a sentitytype==MED_NODE.) La connectivité implicite des éléments de ce type est constitué des noeuds du maillage de calcul (cf. MED_PARTICLE). " \
\
"MEDstructElementCrsupportmeshnameCm2=Pour un maillage support constitué uniquement de n noeuds (implique \a sentitytype==MED_NODE), la connectivité des éléments de ce type est constitué de n noeuds pour chaque élément de structure du maillage de calcul. " \
\
"MEDstructElementCrsupportmeshnameCm3=Pour un maillage support constitué de m mailles (implique \a sentitytype==MED_CELL), la connectivité des éléments de ce type dans le maillag de calcul est constitué de m numéros de mailles (du maillage de calcul) pour chaque élément de structure défini. Ces numéros de mailles apparaissent dans le même ordre que celui défini dans la connectivité du modèle. " \
\
"MEDstructElementConstAttWrBrief=Cette routine définit un attribut caractéristique constant d'un modèle d'éléments de structure." \
\
"MEDstructElementConstAttWithProfileWrBrief=\MEDstructElementConstAttWrBrief" \
\
"MEDstructElementConstAttWrDetails=Cette routine définit l'attribut \a constattname de valeur constante \a value à \a ncomponent composantes."  \
\
"MEDstructElementConstAttWithProfileWrDetails=\MEDstructElementConstAttWrDetails \
	Cette valeur est affectée aux entités du type \a sentitytype du modèle \a modelname choisies selon le profil \a profilename. Si l'attribut caractéristique possède une valeur différente sur certaines entités du maillage support il est necessaire de créer une suite de nom d'attributs avec des profils différents." \
"MEDstructElementConstAttswitchCm=L'entrelacement des valeurs est toujours en mode #MED_FULL_INTERLACE." \
\
"MEDstructElement=Cette routine renvoie le nombre de modèles d'éléments de structure définis dans le fichier." \
"MEDstructElementInfoByNameBrief=Cette routine décrit les caractéristiques d'un modèle d'élément de structure à partir de son nom." \
"MEDstructElementInfosupportCm=Elle renseigne également le nom du maillage support utilisé \a supportmeshname et les caractéristiques générales de ce maillage. Le maillage support est constitué de \a sncell mailles de type \a sgeotype  et de \a snnode noeuds." \
"MEDstructElementInfoattributsCm= Ce modèle d'élément possède \a nconstantattribute attributs constants dont au moins un est décrit en utilisant des profils si \a anyprofile est vrai. Le maillage de calcul peut contenir jusqu'à \a nvariableattribute attributs variables pour les éléments de ce type géométrique. " \
"MEDstructElementInfoByNameDetails=\MEDstructElementInfoByNameBrief \
	A partir du nom du modèle d'élément de structure \a modelname, la routine indique le type géométrique \a mgeotype associé et la dimension \a mdim du modèle. \MEDstructElementInfosupportCm \MEDstructElementInfoattributsCm "  \
"MEDstructElementInfoBrief=Cette routine décrit les caractéristiques d'un modèle d'élément de structure par itération." \
"MEDstructElementInfoDetails=\MEDstructElementInfoBrief \
	A chaque itération \a mit (>0) la routine décrit le  modèle d'élément de structure \a modelname en indiquant le type géométrique \a mgeotype associé et la dimension \a mdim du modèle. \MEDstructElementInfosupportCm \MEDstructElementInfoattributsCm " \
"MEDstructElementConstAttInfoBrief=Cette routine décrit les caractéristiques d'un attribut constant de modèle d'élément de structure par itération." \
"MEDstructElementConstAttInfoDetails=\MEDstructElementConstAttInfoBrief \
	A chaque itération \a attit (>0) la routine décrit l'attribut constant \a constattname du  modèle d'élément de structure \a modelname en indiquant le type MED de l'attribut \a constatttype et son nombre de composantes \a ncomponent. Les entités du maillage support concernées sont de type  \a sentitytype et éventuellement énumérées par un profile \a profilename de taille \a profilesize. " \
"MEDstructElementConstAttInfoByNameBrief=Cette routine décrit les caractéristiques d'un attribut constant de modèle d'élément de structure à partir de son nom." \
"MEDstructElementConstAttInfoByNameDetails=\MEDstructElementConstAttInfoByNameBrief \
	A partir du nom de l'attribut constant \a constattname du  modèle d'élément de structure \a modelname, la routine indique le type MED de l'attribut \a constatttype et son nombre de composantes \a ncomponent. Les entités du maillage support concernées sont de type  \a sentitytype et éventuellement énumérées par un profile \a profilename de taille \a profilesize. " \
"MEDstructElementConstAttRdBrief=Cette routine lit la valeur d'un attribut caractéristique constant d'un modèle d'éléments de structure." \
"MEDstructElementConstAttRdDetails=Cette routine lit la valeur de l'attribut caractéristique constant \a constattname attaché à un (sous)ensemble d'entités de type #MED_CELL ou #MED_NODE du maillage support du modèle d'éléments de structure \a modelname." \
\
"MEDstructElementAttSizeofBrief=Cette routine renvoie la taille en octets du type élémentaire \a atttype." \
"MEDstructElementAttSizeofDetails=\MEDstructElementAttSizeofBrief" \
"MEDnStructElementBrief=Cette routine renvoie le nombre de modèles d'éléments de structure." \
"MEDnStructElementDetails=\MEDnStructElementBrief" \
"MEDstructElementVarAttCrBrief=Cette routine déclare la présence d'un attribut caractéristique variable attaché aux éléments de type \a modelname." \
"MEDstructElementVarAttCrDetails=Cette routine déclare la présence d'un attribut caractéristique variable \a varattname attaché aux éléments de type \a modelname. \
	Cet attribut est de type \a varaattype et possède \a nbcomponent. L'écriture effective des valeurs de cet attribut sur les éléments de maillage de calcul se fait par appel à \ref MEDmeshStructElementVarAttWr . Si la connectivité de ces éléments du maillage de calcul est écrite en suivant un profil, les éléments concernés par l'écriture de l'attribut sont ceux de ce profil." \
"MEDstructElementVarAttInfoByNameBrief=Cette routine décrit les caractéristiques d'un attribut variable de modèle d'élément de structure à partir de son nom." \
"MEDstructElementVarAttInfoByNameDetails= \
	A partir du nom de l'attribut variable \a varattname du modèle d'élément de structure \a modelname, la routine indique le type MED \a varatttype de l'attribut et son nombre de composantes \a ncomponent. " \
"MEDstructElementVarAttInfoBrief=Cette routine décrit les caractéristiques d'un attribut variable de modèle d'élément de structure par itération." \
"MEDstructElementVarAttInfoDetails=\MEDstructElementVarAttInfoBrief \
	A chaque itération \a attit (>0) la routine décrit l'attribut variable \a varattname du modèle d'élément de structure \a modelname. La routine indique le type MED \a varatttype de l'attribut et son nombre de composantes \a ncomponent. " \
"MEDmeshStructElementVarAttWrBrief=Cette routine écrit les valeurs d'un attribut caractéristique variable sur les éléments de structure d'un maillage de calcul." \
"MEDmeshStructElementVarAttWrDetails=Cette routine écrit les valeurs de l'attribut caractéristique variable \a varattname sur les éléments de structure de type \a mgeotype du maillage de calcul \a meshname . Si la connectivité de ces éléments est écrite en suivant un profil, les éléments concernés par l'écriture de l'attribut sont ceux du profil. Si le type de l'attribut est #MED_ATT_NAME , chaque chaîne de caractères est de taille #MED_NAME_SIZE ." \
"MEDmeshStructElementVarAttRdBrief=Cette routine lit les valeurs d'un attribut caractéristique variable sur les éléments de structure d'un maillage de calcul." \
"MEDmeshStructElementVarAttRdDetails=Cette routine lit les valeurs de l'attribut caractéristique variable \a varattname sur les éléments de structure de type \a mgeotype du maillage de calcul \a meshname . Si la connectivité de ces éléments est écrite en suivant un profil, les éléments concernés par la lecture de l'attribut sont ceux du profil. Si le type de l'attribut est #MED_ATT_NAME , chaque chaîne de caractères est de taille #MED_NAME_SIZE ." \
"MEDstructElementNameBrief=Cette routine renvoie le nom du modèle d'éléments de structure associé au type \a mgeotype." \
"MEDstructElementNameDetails=\MEDstructElementNameBrief" \
"MEDstructElementGeotypeBrief=Cette routine renvoie le type géométrique \a mgeotype associé au modèle d'éléments de structure de nom \a modelname." \
"MEDstructElementGeotypeDetails=\MEDstructElementGeotypeBrief" \
"MEDsupportMeshnAxisBrief=Cette routine permet de lire dans un maillage support le nombre d'axes du repère des coordonnées des noeuds." \
"MEDsupportMeshnAxisDetails=Cette routine permet de lire dans un maillage support le nombre d'axes du repère des coordonnées des noeuds. le nombre d'axe correspond à la dimension de l'espace de calcul. L'accès au maillage support se fait via un itérateur." \
"MEDsupportMeshnAxisByNameBrief=Cette routine permet de lire dans un maillage support le nombre d'axes du repère des coordonnées des noeuds avec accès direct." \
"MEDsupportMeshnAxisByNameDetails=Cette routine permet de lire dans un maillage support le nombre d'axes du repère des coordonnées des noeuds. le nombre d'axe correspond à la dimension de l'espace de calcul. L'accès au maillage support se fait directement via son nom." \
"interpname=Nom de la fonction d'interpolation" \
"ninterp=Nombre d'interpolations." \
"interpit=Iterateur sur les fonctions d'interpolations" \
"cellnode=Indique si les points de construction de l'interpolation sont aux noeuds de la maille de référence" \
"nvariable=Nombre de variables différentes apparaissant dans les polynômes (fonctions de formes/fonctions de base) \
	C'est aussi égal à la dimension de l'espace de la maille de construction" \ 
"ncoef=Nombre de coefficients (non null) du polynômes. Nombre de monômes de la fonction de base" \
"maxdegree=Degré maximum de l'ensemble des polynômes (fonctions de forme/fonctions de base)" \
"nmaxcoef=Nombre maximum de coefficients de l'ensemble des polynômes (fonctions de forme/fonctions de base)" \
"nbasisfunc=Nombre de fonctions de forme/base d'une interpolation" \
"basisfuncit=Itérateur sur les fonctions de base/forme (>0)." \
"power=Tableau des puissances des monômes d'un polynôme (taille minimum \a ncoef * \a nvariable)" \
"coefficient=Tableau contenant le coefficient multiplicateur de chaque monôme (taille minimum \a ncoef)" \
"MEDinterpCrBrief=Cette routine permet de créer une nouvelle fonction d'interpolation polynômiale nommée \a interpname." \
"MEDinterpCrDetails=\MEDinterpCrBrief Cette interpolation est adaptée à des champs reposants sur des éléments de type géométrique \a geotype. L'ensemble de ses fonctions de base utilisent un maximum de \a nvariable variables et un maximum de \a nmaxcoef coefficients et sont d'un degrée maximum \a maxdegree." \
"MEDinterpCrcellnodeCm1=L'utilisation directe de l'interpolation ainsi définie pour calculer la valeur d'un champ en tout point de n'importe quel élément réel n'est possible que si les valeurs du champ résultat sont données aux points de construction de la fonction d'interpolation ( \a cellnode doit valoir #MED_TRUE ). Ce n'est généralement pas le cas lorsque le champ résultat est donnée aux points d'intégrations (cf. \ref MEDlocalization )." \
"MEDinterpCrcellnodeCm2= Lorsque \a cellnode vaut #MED_FALSE, l'interpolation peut par exemple être utilisée pour le calcul de l'intégrale du champ." \
"MEDinterpBaseFunctionWrBrief=Cette routine permet l'écriture d'une fonction de base/forme de l'interpolation \a interpname." \
"MEDinterpBaseFunctionWrDetails=Cette routine permet l'écriture de la fonction de base/forme n° \a basisfuncit de l'interpolation \a interpname. Cette fonction de base est un polynôme qui possède \a ncoef monômes dont les coefficients sont donnés dans le tableau \a coefficient et les puissances dans le tableau \a power." \
"MEDinterpBaseFunctionWrCm1=Lorsque la fonction d'interpolation est contruite au noeuds d'un élément de référence, le numéro \a basisfuncit (>0) doit décrire le polynôme associé au noeud \a basisfuncit de la maille en suivant l'ordre de parcours des mailles MED." \
"MEDinterpBaseFunctionWrCm2=L'ordre d'apparition des variables dans le tableau \a power suit l'ordre des axes du repère de l'espace." \
"MEDnInterpBrief=Cette routine renvoie le nombre d'interpolations disponibles dans le fichier." \
"MEDnInterpDetails=\MEDnInterpBrief" \
"MEDinterpInfoBrief=Cette fonction informe des caractéristiques de la fonction d'interpolation n° \a interpit." \
"MEDinterpInfoDetails=\MEDinterpInfoBrief La fonction \a interpname opère sur des champs résultats qui reposent sur des éléments de type \a geotype. Cette fonction est constituée d'un ensemble de \a nbasisfunc fonctions de base/forme de degrée maximum \a maxdegree avec un maximum de \nmaxcoefficient monômes a \a nvariable variables. Si les points de construction de la fonction d'interpolation sont les noeuds de l'élément de référence le paramètre \a cellnode vaut #MED_TRUE." \
"MEDinterpInfoByNameBrief=Cette fonction informe des caractéristiques de la fonction d'interpolation nommée \a interpname." \
"MEDinterpInfoByNameDetails=\MEDinterpInfoByNameBrief La fonction \a interpname opère sur des champs résultats qui reposent sur des éléments de type \a geotype. Cette fonction est constituée d'un ensemble de \a nbasisfunc fonctions de base/forme de degrée maximum \a maxdegree avec un maximum de \nmaxcoefficient monômes a \a nvariable variables. Si les points de construction de la fonction d'interpolation sont les noeuds de l'élément de référence le paramètre \a cellnode vaut #MED_TRUE." \
"MEDinterpBaseFunctionRdBrief=Cette routine permet la lecture d'une fonction de base/forme de l'interpolation \a interpname." \
"MEDinterpBaseFunctionRdDetails=Cette routine lit la fonction de base/forme n° \a basisfuncit de la fonction d'inerpolation \a interpname. Cette fonction possède \a ncoef monômes dont les coefficients sont lus dans \a coefficient et les puissances dans \a power." \
"MEDinterpBaseFunctionRdCm1=L'allocation des tableaux power et coefficient peut être faite une fois à la plus grande taille  en utilisant les paramètres \a nmaxcoef et \a nvariable (cf. \ref MEDinterpInfo)." \
"MEDinterpBaseFunctionRdCm2=L'allocation des tableaux power et coefficient peut être faite \a nbasisfunc fois au plus juste en appelant au préalable \ref MEDinterpBaseFunctionCoefSize." \
"MEDinterpBaseFunctionCoefSizeBrief=Cette routine retourne ne nombre de coefficients/monômes de la fonction de base/forme n° \a basisfunctit de l'interpolation \a interpname." \
"MEDinterpBaseFunctionCoefSizeDetails=\MEDinterpBaseFunctionCoefSizeBrief" \
"MEDfieldInterpWrBrief=Cette routine associe une fonction d'interpolation \a interpname au champ résultat \a fieldname" \
"MEDfieldInterpWrDetails=\MEDfieldInterpWrBrief" \
"MEDfieldnInterpBrief=Cette routine renvoie le nombre de fonctions d'interpolation associées au champ résultat \a fieldname." \
"MEDfieldnInterpDetails=\MEDfieldnInterpBrief" \
"MEDfieldInterpInfoBrief=Cette routine indique le nom \a interpname de la \a interpit ème fonction d'interpolation associées au champ résultat \a fieldname." \
"MEDfieldInterpInfoDetails=\MEDfieldInterpInfoBrief" \
"paramname=Nom du paramètre de longueur maximum #MED_NAME_SIZE ." \
"paramtype=Type du paramètre." \
"nparam=Nombre de paramètre." \
"paramit=Itérateur sur les paramètres du fichier. La valeur de l'itérateur commence à 1" \
"MEDparameterCrBrief=Cette routine permet la création d'un paramètre numérique scalaire." \
"MEDparameterCrDetails=Cette routine permet la création d'un paramètre numérique scalaire. Un paramètre est identifié par son nom et se voit associer un type, une description et un ordre de tri des séquences de calcul." \
"MEDparameterValueWrBrief=Cette routine permet l'écriture de la valeur d'un paramètre numérique scalaire." \
"MEDparameterValueWrDetails=Cette routine permet l'écriture de la valeur d'un paramètre numérique scalaire pour une séquence de calcul donnée." \
"MEDparameterValueRdBrief=Cette routine permet la lecture de la valeur d'un paramètre numérique scalaire." \
"MEDparameterValueRdDetails=Cette routine permet la lecture de la valeur d'un paramètre numérique scalaire pour une séquence de calcul donnée." \
"MEDparameterInfoBrief=Cette routine permet la lecture des informations relatives à un paramètre scalaire via un itérateur." \
"MEDparameterInfoDetails=Cette routine permet la lecture des informations relatives à un paramètre scalaire via un itérateur. Les informations lues sont le nom, la description, le type, l'unité des pas de temps, le mode de tri des séquences de calcul, le nombre de séquence." \
"MEDparameterInfoByNameBrief=Cette routine permet la lecture des informations relatives à un paramètre scalaire." \
"MEDparameterInfoByNameDetails=Cette routine permet la lecture des informations relatives à un paramètre scalaire. Le scalaire est identifié par son nom. Les informations lues sont la description, le type, l'unité des pas de temps, le mode de tri des séquences de calcul, le nombre de séquence." \
"MEDnParameterBrief=Cette routine permet la lecture du nombre de paramètre numérique scalaire dans un fichier." \
"MEDnParameterDetails=Cette routine permet la lecture du nombre de paramètre numérique scalaire dans un fichier." \
"MEDparameterComputationStepInfoBrief=Cette routine permet la lecture des informations relatives à une séquence de calcul du paramètre numérique scalaire." \
"MEDparameterComputationStepInfoDetails=Cette routine permet la lecture des informations relatives à une séquence de calcul d'un paramètre numérique scalaire. L'accès à la séquence se fait par un numéro d'itération. Les informations lues sont le num d'ordre, le numéro de pas de temps et la valeur du pas de temps." \
"storagemode=Indique le mode de stockage en mémoire \ref med_storage_mode des valeurs associées au profil utilisé." \
"profilename=Nom du profil utilisé (de taille maximum #MED_NAME_SIZE ) ou (#MED_NO_PROFILE | #MED_ALLENTITIES_PROFILE ) s'il n'y a pas de profil." \
"profilearray=Tableau des numéros d'entités associées aux valeurs à traiter." \
"profilesize=Taille du profil." \
"defaultprofilename=Nom du profil par défaut (de taille maximum #MED_NAME_SIZE ) ou #MED_NO_PROFILE s'il n'y a pas de profil." \
"profileit=Itérateur sur le profil. La valeur initiale de l'itérateur est 1." \
"nProfile=Nombre de profil." \
"MEDprofileInfoBrief=Cette routine permet de lire les informations sur un profil dans un fichier MED." \
"MEDprofileInfoDetails=Cette routine permet de lire les informations sur un profil dans un fichier MED. L'accès au profil se fait via un itérateur. Les informations lues sont : le nom du profil ainsi que le nombre d'entités référencées dans le profil." \
"MEDprofileSizeByNameBrief=Cette routine permet de lire la taille d'un profil dont on connait le nom." \
"MEDprofileSizeByNameDetails=Cette routine permet de lire la taille d'un profil dont on connait le nom." \
"MEDprofileDef=Un profil est un tableau de numéros d'entités (\relativenumbering ) associés aux valeurs à traiter. Un profil permet de sélectionner les entités d'un maillage lors de la lecture ou l'écriture d'un champ ou d'un maillage. La définition d'un profil se fait selon les conventions suivantes : \ 
\li Les numéros d'entité utilisés pour définir un profil sont ceux de la numérotation implicite (ordre d'apparition des entités par ordre croissant). \
\li Il s'agit d'une liste compacte : on ne met que les numéros représentatifs. Exemple : sur un maillage de 30 noeuds, si on a un champ portant sur les noeuds de numéros de référence 4, 5 et 12, le profil correspondant sera la liste (4,5,12)." \
"MEDnProfileBrief=Cette routine permet de lire le nombre de profil dans un fichier MED." \
"MEDnProfileDetails=Cette routine permet de lire le nombre de profil dans un fichier MED." \
"MEDprofileWrBrief=Cette routine permet d'écrire un profil dans un fichier MED." \
"MEDprofileWrDetails=Cette routine permet d'écrire un profil dans un fichier MED. Un profil est identifié par un nom." \
"MEDprofileRdBrief=Cette routine permet de lire un profil dans un fichier MED." \
"MEDprofileRdDetails=Cette routine permet de lire un profil dans un fichier MED. Un profil est identifié par un nom."
