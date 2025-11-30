       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCK.
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
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O PRODUITS.
           PERFORM UNTIL WS-FIN = 1
               DISPLAY ' '
               DISPLAY '=== GESTION STOCK ==='
               DISPLAY '1. ENTREE STOCK'
               DISPLAY '2. SORTIE STOCK'
               DISPLAY '3. CONSULTER PRODUIT'
               DISPLAY '4. ALERTES STOCK BAS'
               DISPLAY '5. INVENTAIRE COMPLET'
               DISPLAY '9. QUITTER'
               DISPLAY 'CHOIX:'
               ACCEPT WS-CHOIX
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
           STOP RUN.
       ENTREE-STOCK.
           DISPLAY 'CODE PRODUIT:'.
           ACCEPT PRD-CODE.
           READ PRODUITS
               INVALID KEY DISPLAY 'PRODUIT INCONNU'
               NOT INVALID KEY
                   DISPLAY PRD-NOM ' - QTE ACTUELLE: ' PRD-QTE
                   DISPLAY 'QUANTITE A AJOUTER:'
                   ACCEPT WS-QTE
                   ADD WS-QTE TO PRD-QTE
                   REWRITE PRODUIT-REC
                   DISPLAY 'NOUVEAU STOCK: ' PRD-QTE
           END-READ.
       SORTIE-STOCK.
           DISPLAY 'CODE PRODUIT:'.
           ACCEPT PRD-CODE.
           READ PRODUITS
               INVALID KEY DISPLAY 'PRODUIT INCONNU'
               NOT INVALID KEY
                   DISPLAY PRD-NOM ' - QTE ACTUELLE: ' PRD-QTE
                   DISPLAY 'QUANTITE A RETIRER:'
                   ACCEPT WS-QTE
                   IF WS-QTE > PRD-QTE
                       DISPLAY '*** STOCK INSUFFISANT ***'
                   ELSE
                       SUBTRACT WS-QTE FROM PRD-QTE
                       REWRITE PRODUIT-REC
                       DISPLAY 'NOUVEAU STOCK: ' PRD-QTE
                       IF PRD-QTE < PRD-SEUIL
                           DISPLAY '!!! ALERTE SEUIL !!!'
                       END-IF
                   END-IF
           END-READ.
       CONSULTER.
           DISPLAY 'CODE PRODUIT:'.
           ACCEPT PRD-CODE.
           READ PRODUITS
               INVALID KEY DISPLAY 'PRODUIT INCONNU'
               NOT INVALID KEY
                   DISPLAY 'NOM:    ' PRD-NOM
                   DISPLAY 'STOCK:  ' PRD-QTE
                   DISPLAY 'SEUIL:  ' PRD-SEUIL
                   DISPLAY 'PRIX:   ' PRD-PRIX
           END-READ.
       ALERTES.
           DISPLAY '*** PRODUITS SOUS SEUIL ***'.
           MOVE LOW-VALUES TO PRD-CODE.
           START PRODUITS KEY >= PRD-CODE.
           MOVE 0 TO WS-EOF.
           PERFORM UNTIL WS-EOF = 1
               READ PRODUITS NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF PRD-QTE < PRD-SEUIL
                           DISPLAY PRD-CODE ' ' PRD-NOM
                               ' QTE:' PRD-QTE '/' PRD-SEUIL
                       END-IF
               END-READ
           END-PERFORM.
       INVENTAIRE.
           DISPLAY '=== INVENTAIRE COMPLET ==='.
           MOVE LOW-VALUES TO PRD-CODE.
           START PRODUITS KEY >= PRD-CODE.
           MOVE 0 TO WS-EOF.
           PERFORM UNTIL WS-EOF = 1
               READ PRODUITS NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       DISPLAY PRD-CODE ' | ' PRD-NOM ' | ' PRD-QTE
               END-READ
           END-PERFORM.