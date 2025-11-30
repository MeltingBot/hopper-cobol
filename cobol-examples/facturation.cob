       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTURE.
      *===============================================
      * CREATION DE FACTURES
      * Interface ecran IBM 3270
      *===============================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARTICLES ASSIGN TO 'ARTICLES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ART-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD ARTICLES.
       01 ARTICLE-REC.
           05 ART-CODE PIC X(6).
           05 ART-LIBELLE PIC X(25).
           05 ART-PRIX PIC 9(5)V99.
           05 ART-STOCK PIC 9(4).
       WORKING-STORAGE SECTION.
       01 WS-CODE PIC X(6).
       01 WS-QTE PIC 9(3) VALUE 0.
       01 WS-LIGNE-FACT PIC 9(6)V99 VALUE 0.
       01 WS-TOTAL-HT PIC 9(8)V99 VALUE 0.
       01 WS-TVA PIC 9(6)V99 VALUE 0.
       01 WS-TOTAL-TTC PIC 9(8)V99 VALUE 0.
       01 WS-NUM-FACT PIC 9(6) VALUE 0.
       01 WS-CONTINUER PIC X VALUE 'O'.
       01 WS-LIGNE PIC 99 VALUE 0.
       01 WS-NB-ARTICLES PIC 99 VALUE 0.
       01 WS-CHOIX PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT ARTICLES.
           PERFORM AFFICHER-ENTETE-FACTURE.
           MOVE 10 TO WS-LIGNE.
           PERFORM UNTIL WS-CONTINUER = 'N' OR WS-LIGNE > 16
               PERFORM SAISIR-ARTICLE
           END-PERFORM.
           PERFORM CALCULER-TOTAUX.
           PERFORM AFFICHER-TOTAUX.
           CLOSE ARTICLES.
           STOP RUN.

       AFFICHER-ENTETE-FACTURE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '+' LINE 2 POSITION 15.
           DISPLAY '------------------------------------------------'
               LINE 2 POSITION 16.
           DISPLAY '+' LINE 2 POSITION 64.
           DISPLAY '|' LINE 3 POSITION 15.
           DISPLAY '           CREATION DE FACTURE                  '
               LINE 3 POSITION 17 HIGHLIGHT.
           DISPLAY '|' LINE 3 POSITION 64.
           DISPLAY '+' LINE 4 POSITION 15.
           DISPLAY '------------------------------------------------'
               LINE 4 POSITION 16.
           DISPLAY '+' LINE 4 POSITION 64.
           DISPLAY 'Numero de facture:' LINE 6 POSITION 20.
           DISPLAY '[______]' LINE 6 POSITION 40 REVERSE-VIDEO.
           ACCEPT WS-NUM-FACT LINE 6 POSITION 41.
           DISPLAY 'CODE' LINE 8 POSITION 5 UNDERLINE.
           DISPLAY 'ARTICLE' LINE 8 POSITION 14 UNDERLINE.
           DISPLAY 'P.U.' LINE 8 POSITION 40 UNDERLINE.
           DISPLAY 'QTE' LINE 8 POSITION 48 UNDERLINE.
           DISPLAY 'TOTAL' LINE 8 POSITION 55 UNDERLINE.
           DISPLAY '----------------------------------------------'
               LINE 9 POSITION 5.

       SAISIR-ARTICLE.
           DISPLAY 'Code (FIN=000000):' LINE WS-LIGNE POSITION 5.
           DISPLAY '[______]' LINE WS-LIGNE POSITION 24 REVERSE-VIDEO.
           ACCEPT WS-CODE LINE WS-LIGNE POSITION 25.
           IF WS-CODE NOT = '000000'
               MOVE WS-CODE TO ART-CODE
               READ ARTICLES
                   INVALID KEY
                       DISPLAY 'ARTICLE INCONNU' LINE WS-LIGNE
                           POSITION 35 HIGHLIGHT
                   NOT INVALID KEY
                       DISPLAY ART-CODE LINE WS-LIGNE POSITION 5
                       DISPLAY ART-LIBELLE LINE WS-LIGNE POSITION 14
                       DISPLAY ART-PRIX LINE WS-LIGNE POSITION 38
                       DISPLAY '[___]' LINE WS-LIGNE POSITION 47
                           REVERSE-VIDEO
                       ACCEPT WS-QTE LINE WS-LIGNE POSITION 48
                       MULTIPLY ART-PRIX BY WS-QTE
                           GIVING WS-LIGNE-FACT
                       ADD WS-LIGNE-FACT TO WS-TOTAL-HT
                       ADD 1 TO WS-NB-ARTICLES
                       DISPLAY WS-LIGNE-FACT LINE WS-LIGNE POSITION 53
                           HIGHLIGHT
                       ADD 1 TO WS-LIGNE
               END-READ
           ELSE
               MOVE 'N' TO WS-CONTINUER
           END-IF.

       CALCULER-TOTAUX.
           COMPUTE WS-TVA = WS-TOTAL-HT * 0.20.
           COMPUTE WS-TOTAL-TTC = WS-TOTAL-HT + WS-TVA.

       AFFICHER-TOTAUX.
           ADD 1 TO WS-LIGNE.
           DISPLAY '----------------------------------------------'
               LINE WS-LIGNE POSITION 5.
           ADD 1 TO WS-LIGNE.
           DISPLAY '+--------------------------------+'
               LINE WS-LIGNE POSITION 20.
           ADD 1 TO WS-LIGNE.
           DISPLAY '|' LINE WS-LIGNE POSITION 20.
           DISPLAY 'FACTURE N.' LINE WS-LIGNE POSITION 22 HIGHLIGHT.
           DISPLAY WS-NUM-FACT LINE WS-LIGNE POSITION 33 HIGHLIGHT.
           DISPLAY '|' LINE WS-LIGNE POSITION 53.
           ADD 1 TO WS-LIGNE.
           DISPLAY '|' LINE WS-LIGNE POSITION 20.
           DISPLAY '--------------------------------' LINE WS-LIGNE
               POSITION 21.
           DISPLAY '|' LINE WS-LIGNE POSITION 53.
           ADD 1 TO WS-LIGNE.
           DISPLAY '|' LINE WS-LIGNE POSITION 20.
           DISPLAY 'Articles:' LINE WS-LIGNE POSITION 24.
           DISPLAY WS-NB-ARTICLES LINE WS-LIGNE POSITION 40.
           DISPLAY '|' LINE WS-LIGNE POSITION 53.
           ADD 1 TO WS-LIGNE.
           DISPLAY '|' LINE WS-LIGNE POSITION 20.
           DISPLAY 'Total HT:' LINE WS-LIGNE POSITION 24.
           DISPLAY WS-TOTAL-HT LINE WS-LIGNE POSITION 38 HIGHLIGHT.
           DISPLAY 'EUR' LINE WS-LIGNE POSITION 49.
           DISPLAY '|' LINE WS-LIGNE POSITION 53.
           ADD 1 TO WS-LIGNE.
           DISPLAY '|' LINE WS-LIGNE POSITION 20.
           DISPLAY 'TVA 20%:' LINE WS-LIGNE POSITION 24.
           DISPLAY WS-TVA LINE WS-LIGNE POSITION 40.
           DISPLAY 'EUR' LINE WS-LIGNE POSITION 49.
           DISPLAY '|' LINE WS-LIGNE POSITION 53.
           ADD 1 TO WS-LIGNE.
           DISPLAY '|' LINE WS-LIGNE POSITION 20.
           DISPLAY '--------------------------------' LINE WS-LIGNE
               POSITION 21.
           DISPLAY '|' LINE WS-LIGNE POSITION 53.
           ADD 1 TO WS-LIGNE.
           DISPLAY '|' LINE WS-LIGNE POSITION 20.
           DISPLAY 'TOTAL TTC:' LINE WS-LIGNE POSITION 24 HIGHLIGHT.
           DISPLAY WS-TOTAL-TTC LINE WS-LIGNE POSITION 36 HIGHLIGHT.
           DISPLAY 'EUR' LINE WS-LIGNE POSITION 49.
           DISPLAY '|' LINE WS-LIGNE POSITION 53.
           ADD 1 TO WS-LIGNE.
           DISPLAY '+--------------------------------+'
               LINE WS-LIGNE POSITION 20.
           DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 22 POSITION 49.

