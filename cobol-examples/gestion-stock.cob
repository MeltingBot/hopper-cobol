       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCK.
      *===============================================
      * GESTION DES STOCKS
      * Interface ecran IBM 3270
      *===============================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUITS ASSIGN TO 'PRODUITS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRD-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD PRODUITS.
       01 PRODUIT-REC.
           05 PRD-CODE PIC X(8).
           05 PRD-NOM PIC X(20).
           05 PRD-QTE PIC 9(5).
           05 PRD-SEUIL PIC 9(5).
           05 PRD-PRIX PIC 9(5)V99.
       WORKING-STORAGE SECTION.
       01 WS-CHOIX PIC 9 VALUE 0.
       01 WS-QTE PIC 9(5) VALUE 0.
       01 WS-FIN PIC 9 VALUE 0.
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-LIGNE PIC 99 VALUE 0.
       01 WS-COUNT PIC 999 VALUE 0.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O PRODUITS.
           PERFORM UNTIL WS-FIN = 1
               PERFORM AFFICHER-MENU
               ACCEPT WS-CHOIX LINE 16 POSITION 25
               EVALUATE WS-CHOIX
                   WHEN 1 PERFORM ENTREE-STOCK
                   WHEN 2 PERFORM SORTIE-STOCK
                   WHEN 3 PERFORM CONSULTER
                   WHEN 4 PERFORM ALERTES
                   WHEN 5 PERFORM INVENTAIRE
                   WHEN 9 MOVE 1 TO WS-FIN
               END-EVALUATE
           END-PERFORM.
           CLOSE PRODUITS.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY 'AU REVOIR' LINE 12 POSITION 35 HIGHLIGHT.
           STOP RUN.

       AFFICHER-MENU.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '+' LINE 2 POSITION 22.
           DISPLAY '------------------------------------'
               LINE 2 POSITION 23.
           DISPLAY '+' LINE 2 POSITION 58.
           DISPLAY '|' LINE 3 POSITION 22.
           DISPLAY '       GESTION DES STOCKS          '
               LINE 3 POSITION 24 HIGHLIGHT.
           DISPLAY '|' LINE 3 POSITION 58.
           DISPLAY '+' LINE 4 POSITION 22.
           DISPLAY '------------------------------------'
               LINE 4 POSITION 23.
           DISPLAY '+' LINE 4 POSITION 58.
           DISPLAY '1.' LINE 6 POSITION 28.
           DISPLAY 'Entree stock' LINE 6 POSITION 31.
           DISPLAY '2.' LINE 8 POSITION 28.
           DISPLAY 'Sortie stock' LINE 8 POSITION 31.
           DISPLAY '3.' LINE 10 POSITION 28.
           DISPLAY 'Consulter produit' LINE 10 POSITION 31.
           DISPLAY '4.' LINE 12 POSITION 28.
           DISPLAY 'Alertes stock bas' LINE 12 POSITION 31.
           DISPLAY '5.' LINE 14 POSITION 28.
           DISPLAY 'Inventaire complet' LINE 14 POSITION 31.
           DISPLAY '9.' LINE 16 POSITION 28.
           DISPLAY 'Quitter' LINE 16 POSITION 31.
           DISPLAY 'Votre choix:' LINE 16 POSITION 12.
           DISPLAY '[_]' LINE 16 POSITION 25 REVERSE-VIDEO.

       ENTREE-STOCK.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== ENTREE STOCK ===' LINE 2 POSITION 30 HIGHLIGHT.
           DISPLAY 'Code produit:' LINE 5 POSITION 15.
           DISPLAY '[________]' LINE 5 POSITION 30 REVERSE-VIDEO.
           ACCEPT PRD-CODE LINE 5 POSITION 31.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT INCONNU' LINE 12 POSITION 32
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY '+--------------------------+'
                       LINE 7 POSITION 20
                   DISPLAY '|' LINE 8 POSITION 20
                   DISPLAY 'Produit:' LINE 8 POSITION 22
                   DISPLAY PRD-NOM LINE 8 POSITION 31 HIGHLIGHT
                   DISPLAY '|' LINE 8 POSITION 47
                   DISPLAY '|' LINE 9 POSITION 20
                   DISPLAY 'Stock actuel:' LINE 9 POSITION 22
                   DISPLAY PRD-QTE LINE 9 POSITION 36 HIGHLIGHT
                   DISPLAY '|' LINE 9 POSITION 47
                   DISPLAY '+--------------------------+'
                       LINE 10 POSITION 20
                   DISPLAY 'Quantite a ajouter:' LINE 12 POSITION 15
                   DISPLAY '[_____]' LINE 12 POSITION 35 REVERSE-VIDEO
                   ACCEPT WS-QTE LINE 12 POSITION 36
                   ADD WS-QTE TO PRD-QTE
                   REWRITE PRODUIT-REC
                   DISPLAY '*** STOCK MIS A JOUR ***'
                       LINE 15 POSITION 28 HIGHLIGHT
                   DISPLAY 'Nouveau stock:' LINE 16 POSITION 22
                   DISPLAY PRD-QTE LINE 16 POSITION 37 HIGHLIGHT
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 20 POSITION 49.

       SORTIE-STOCK.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== SORTIE STOCK ===' LINE 2 POSITION 30 HIGHLIGHT.
           DISPLAY 'Code produit:' LINE 5 POSITION 15.
           DISPLAY '[________]' LINE 5 POSITION 30 REVERSE-VIDEO.
           ACCEPT PRD-CODE LINE 5 POSITION 31.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT INCONNU' LINE 12 POSITION 32
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY '+--------------------------+'
                       LINE 7 POSITION 20
                   DISPLAY '|' LINE 8 POSITION 20
                   DISPLAY 'Produit:' LINE 8 POSITION 22
                   DISPLAY PRD-NOM LINE 8 POSITION 31 HIGHLIGHT
                   DISPLAY '|' LINE 8 POSITION 47
                   DISPLAY '|' LINE 9 POSITION 20
                   DISPLAY 'Stock actuel:' LINE 9 POSITION 22
                   DISPLAY PRD-QTE LINE 9 POSITION 36 HIGHLIGHT
                   DISPLAY '|' LINE 9 POSITION 47
                   DISPLAY '+--------------------------+'
                       LINE 10 POSITION 20
                   DISPLAY 'Quantite a retirer:' LINE 12 POSITION 15
                   DISPLAY '[_____]' LINE 12 POSITION 35 REVERSE-VIDEO
                   ACCEPT WS-QTE LINE 12 POSITION 36
                   IF WS-QTE > PRD-QTE
                       DISPLAY '*** STOCK INSUFFISANT ***'
                           LINE 15 POSITION 27 HIGHLIGHT
                   ELSE
                       SUBTRACT WS-QTE FROM PRD-QTE
                       REWRITE PRODUIT-REC
                       DISPLAY '*** STOCK MIS A JOUR ***'
                           LINE 15 POSITION 28 HIGHLIGHT
                       DISPLAY 'Nouveau stock:' LINE 16 POSITION 22
                       DISPLAY PRD-QTE LINE 16 POSITION 37 HIGHLIGHT
                       IF PRD-QTE < PRD-SEUIL
                           DISPLAY '!!! ALERTE SEUIL !!!'
                               LINE 17 POSITION 30 BLINK
                       END-IF
                   END-IF
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 20 POSITION 49.

       CONSULTER.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== CONSULTER PRODUIT ==='
               LINE 2 POSITION 27 HIGHLIGHT.
           DISPLAY 'Code produit:' LINE 5 POSITION 15.
           DISPLAY '[________]' LINE 5 POSITION 30 REVERSE-VIDEO.
           ACCEPT PRD-CODE LINE 5 POSITION 31.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT INCONNU' LINE 12 POSITION 32
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY '+------------------------------+'
                       LINE 7 POSITION 18
                   DISPLAY '|' LINE 8 POSITION 18
                   DISPLAY 'Code:' LINE 8 POSITION 21
                   DISPLAY PRD-CODE LINE 8 POSITION 30
                   DISPLAY '|' LINE 8 POSITION 49
                   DISPLAY '|' LINE 9 POSITION 18
                   DISPLAY 'Nom:' LINE 9 POSITION 21
                   DISPLAY PRD-NOM LINE 9 POSITION 30 HIGHLIGHT
                   DISPLAY '|' LINE 9 POSITION 49
                   DISPLAY '|' LINE 10 POSITION 18
                   DISPLAY 'Stock:' LINE 10 POSITION 21
                   DISPLAY PRD-QTE LINE 10 POSITION 30 HIGHLIGHT
                   DISPLAY '|' LINE 10 POSITION 49
                   DISPLAY '|' LINE 11 POSITION 18
                   DISPLAY 'Seuil:' LINE 11 POSITION 21
                   DISPLAY PRD-SEUIL LINE 11 POSITION 30
                   DISPLAY '|' LINE 11 POSITION 49
                   DISPLAY '|' LINE 12 POSITION 18
                   DISPLAY 'Prix:' LINE 12 POSITION 21
                   DISPLAY PRD-PRIX LINE 12 POSITION 30
                   DISPLAY 'EUR' LINE 12 POSITION 40
                   DISPLAY '|' LINE 12 POSITION 49
                   DISPLAY '+------------------------------+'
                       LINE 13 POSITION 18
                   IF PRD-QTE < PRD-SEUIL
                       DISPLAY '!!! STOCK SOUS SEUIL !!!'
                           LINE 15 POSITION 28 BLINK
                   END-IF
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 20 POSITION 49.

       ALERTES.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '*** PRODUITS SOUS SEUIL ***'
               LINE 2 POSITION 26 HIGHLIGHT.
           DISPLAY 'CODE' LINE 5 POSITION 5 UNDERLINE.
           DISPLAY 'NOM' LINE 5 POSITION 16 UNDERLINE.
           DISPLAY 'STOCK' LINE 5 POSITION 38 UNDERLINE.
           DISPLAY 'SEUIL' LINE 5 POSITION 48 UNDERLINE.
           DISPLAY '---------------------------------------------------'
               LINE 6 POSITION 5.
           MOVE LOW-VALUES TO PRD-CODE.
           START PRODUITS KEY >= PRD-CODE.
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-COUNT.
           MOVE 7 TO WS-LIGNE.
           PERFORM UNTIL WS-EOF = 1 OR WS-LIGNE > 17
               READ PRODUITS NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF PRD-QTE < PRD-SEUIL
                           DISPLAY PRD-CODE LINE WS-LIGNE POSITION 5
                           DISPLAY PRD-NOM LINE WS-LIGNE POSITION 16
                           DISPLAY PRD-QTE LINE WS-LIGNE POSITION 38
                               HIGHLIGHT
                           DISPLAY '/' LINE WS-LIGNE POSITION 44
                           DISPLAY PRD-SEUIL LINE WS-LIGNE POSITION 46
                           ADD 1 TO WS-LIGNE
                           ADD 1 TO WS-COUNT
                       END-IF
               END-READ
           END-PERFORM.
           DISPLAY '---------------------------------------------------'
               LINE 18 POSITION 5.
           IF WS-COUNT = 0
               DISPLAY 'Aucun produit sous seuil'
                   LINE 19 POSITION 28 HIGHLIGHT
           ELSE
               DISPLAY WS-COUNT LINE 19 POSITION 30 HIGHLIGHT
               DISPLAY 'produit(s) en alerte' LINE 19 POSITION 35
           END-IF.
           DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 22 POSITION 49.

       INVENTAIRE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== INVENTAIRE COMPLET ==='
               LINE 2 POSITION 27 HIGHLIGHT.
           DISPLAY 'CODE' LINE 5 POSITION 5 UNDERLINE.
           DISPLAY 'NOM' LINE 5 POSITION 16 UNDERLINE.
           DISPLAY 'STOCK' LINE 5 POSITION 40 UNDERLINE.
           DISPLAY 'PRIX' LINE 5 POSITION 50 UNDERLINE.
           DISPLAY '---------------------------------------------------'
               LINE 6 POSITION 5.
           MOVE LOW-VALUES TO PRD-CODE.
           START PRODUITS KEY >= PRD-CODE.
           MOVE 0 TO WS-EOF.
           MOVE 0 TO WS-COUNT.
           MOVE 7 TO WS-LIGNE.
           PERFORM UNTIL WS-EOF = 1 OR WS-LIGNE > 17
               READ PRODUITS NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       DISPLAY PRD-CODE LINE WS-LIGNE POSITION 5
                       DISPLAY PRD-NOM LINE WS-LIGNE POSITION 16
                       DISPLAY PRD-QTE LINE WS-LIGNE POSITION 40
                       DISPLAY PRD-PRIX LINE WS-LIGNE POSITION 48
                       ADD 1 TO WS-LIGNE
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM.
           DISPLAY '---------------------------------------------------'
               LINE 18 POSITION 5.
           DISPLAY 'Total:' LINE 19 POSITION 25.
           DISPLAY WS-COUNT LINE 19 POSITION 32 HIGHLIGHT.
           DISPLAY 'produits' LINE 19 POSITION 38.
           DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 22 POSITION 49.

