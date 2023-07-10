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