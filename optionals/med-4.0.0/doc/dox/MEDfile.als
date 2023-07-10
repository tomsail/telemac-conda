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