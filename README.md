# Bienvenue dans l'application CST-Ecophyto II+
*Développée par Maxime Garnault, UMR AGIR, INRAe Toulouse-Occitanie, FRANCE*

<br/>
<br/>

Cette application permet aux membres du comité d'accéder et de réaliser des représentations graphiques (principalement des cartographies) de données d'intérêt dans le cadre du plan Ecophyto II+.
L'application met à disposition plusieurs outils de requêtage vers des bases de données telles que : la base météorologique SAFRAN, la base de vente des pesticides BNV-d, etc.
Elle met également à disposition un outil de création manuel de cartographie à partir de données personalisées.

<br/>

## 1) Requêtage et cartographie de données météorologiques SAFRAN
Accèder à l'onglet "Météorologie SAFRAN"

Choisir une plage temporelle d'étude : l'application se chargera d'extraire toutes les données inclues dans la plage temporelle sélectionnée. L'application ne parmet pas de remonter au dela de l'année 1959.

Choisir une variable climatique parmi celles proposée

Choisir une méthode d'aggrégation temporelle : l'application se chargera d'éxécuter la fonction demandée pour chaque maille (8km * 8km) sur la plage temporelle sélectionnée

Choisir une échelle d'aggrégation spatiale : l'application se chargera de réaliser une moyenne pondérée (par les surfaces interceptantes) de la variable climatique à l'échelle spatiale demandée. Il est recommandé de toujours commencer par l'échelle de la maille safran brute, puisqu'elle seule permet de voir quelles sont les mailles considérées
