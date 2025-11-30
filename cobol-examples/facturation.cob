       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTURE.
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
       01 WS-LIGNE PIC 9(6)V99 VALUE 0.
       01 WS-TOTAL-HT PIC 9(8)V99 VALUE 0.
       01 WS-TVA PIC 9(6)V99 VALUE 0.
       01 WS-TOTAL-TTC PIC 9(8)V99 VALUE 0.
       01 WS-NUM-FACT PIC 9(6) VALUE 0.
       01 WS-CONTINUER PIC X VALUE 'O'.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT ARTICLES.
           DISPLAY '================================'.
           DISPLAY '      CREATION FACTURE          '.
           DISPLAY '================================'.
           DISPLAY 'NUMERO FACTURE:'.
           ACCEPT WS-NUM-FACT.
           DISPLAY ' '.
           DISPLAY 'FACTURE N. ' WS-NUM-FACT.
           DISPLAY '--------------------------------'.
           DISPLAY 'CODE   | ARTICLE       | TOTAL '.
           DISPLAY '--------------------------------'.
           PERFORM UNTIL WS-CONTINUER = 'N'
               DISPLAY 'CODE ARTICLE (FIN=000000):'
               ACCEPT WS-CODE
               IF WS-CODE NOT = '000000'
                   MOVE WS-CODE TO ART-CODE
                   READ ARTICLES
                       INVALID KEY
                           DISPLAY 'ARTICLE INCONNU'
                       NOT INVALID KEY
                           DISPLAY 'QUANTITE:'
                           ACCEPT WS-QTE
                           MULTIPLY ART-PRIX BY WS-QTE
                               GIVING WS-LIGNE
                           ADD WS-LIGNE TO WS-TOTAL-HT
                           DISPLAY ART-CODE ' | '
                               ART-LIBELLE ' | ' WS-LIGNE
                   END-READ
               ELSE
                   MOVE 'N' TO WS-CONTINUER
               END-IF
           END-PERFORM.
           COMPUTE WS-TVA = WS-TOTAL-HT * 0.20.
           COMPUTE WS-TOTAL-TTC = WS-TOTAL-HT + WS-TVA.
           DISPLAY '--------------------------------'.
           DISPLAY 'TOTAL HT:  ' WS-TOTAL-HT.
           DISPLAY 'TVA 20%:   ' WS-TVA.
           DISPLAY 'TOTAL TTC: ' WS-TOTAL-TTC.
           DISPLAY '================================'.
           CLOSE ARTICLES.
           STOP RUN.