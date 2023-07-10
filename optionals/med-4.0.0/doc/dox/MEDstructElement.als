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