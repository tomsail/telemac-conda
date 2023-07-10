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