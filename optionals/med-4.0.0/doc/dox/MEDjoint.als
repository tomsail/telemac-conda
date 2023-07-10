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