# APS
Développement d'un petit langage de programmation en OCaml

# Installation 
Pour pouvoir vous aurez besoin de : 
 - OCaml 
 - Dune 

# Build 
POur build un des APS vous devrez allez dans le fichier associez à ce APS par exemple avec APS0 : 
```
cd /APS0
```
puis allez dans le bin : 
```
cd /bin
```
puis compiler le tout. 

Vous aurez alors deux chose que vous pouvez compiler soit l'evaluateur, soit le typeur pour cela vous devrez allez dans le fichier dune ce trouvant dans le fichier bin et modifier ces lignes : 

```
(executable
 (modules _) <----- remplacer par _ par inter ou eval
 (libraries APS2)
 (name _)) <----- remplacer par _ par inter ou eval

(ocamlyacc
 (modules parser))

(library
 (name APS2)
 (public_name APS2)
 (modules ast parser lexer ))

(ocamllex
 (modules lexer))
```

Après avoir fait cela vous aurez uniquement de build projet en faisant : 
```
dune build
```

# Evaluateur 

Pour l'evaluateur vous pourrez tester les différents code se trouvant dans le fichier sample en faisant : 

```
dune exec ./eval.exe ..\Samples\prog0.aps
```
(en remplacent le prog0.aps par le prog que vous voulez tester)


# Typage 

Pour le typage vous devrez faire la commande : 
```
dune exec ./inter.exe ..\Samples\prog0.aps | swipl typage.pl 
```
(en remplacent le prog0.aps par le prog que vous voulez tester)