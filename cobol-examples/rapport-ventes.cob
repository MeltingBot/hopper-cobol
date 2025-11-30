       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAPVENTES.
      *===============================================
      * RAPPORT DES VENTES
      * Interface ecran IBM 3270
      *===============================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTES ASSIGN TO 'VENTES.DAT'
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD VENTES.
       01 VENTE-REC.
           05 VTE-DATE PIC 9(8).
           05 VTE-VENDEUR PIC X(15).
           05 VTE-CLIENT PIC X(20).
           05 VTE-MONTANT PIC 9(7)V99.
       WORKING-STORAGE SECTION.
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-TOTAL PIC 9(9)V99 VALUE 0.
       01 WS-COUNT PIC 9(5) VALUE 0.
       01 WS-MOYENNE PIC 9(7)V99 VALUE 0.
       01 WS-MAX PIC 9(7)V99 VALUE 0.
       01 WS-MIN PIC 9(7)V99 VALUE 999999.99.
       01 WS-VENDEUR-PREC PIC X(15) VALUE SPACES.
       01 WS-TOTAL-VENDEUR PIC 9(8)V99 VALUE 0.
       01 WS-COUNT-VENDEUR PIC 9(4) VALUE 0.
       01 WS-LIGNE PIC 99 VALUE 0.
       01 WS-CHOIX PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT VENTES.
           PERFORM AFFICHER-ENTETE.
           MOVE 8 TO WS-LIGNE.
           PERFORM UNTIL WS-EOF = 1 OR WS-LIGNE > 18
               READ VENTES
                   AT END
                       MOVE 1 TO WS-EOF
                       IF WS-COUNT-VENDEUR > 0
                           PERFORM AFFICHER-TOTAL-VENDEUR
                       END-IF
                   NOT AT END
                       IF VTE-VENDEUR NOT = WS-VENDEUR-PREC
                           IF WS-COUNT-VENDEUR > 0
                               PERFORM AFFICHER-TOTAL-VENDEUR
                           END-IF
                           MOVE VTE-VENDEUR TO WS-VENDEUR-PREC
                           MOVE 0 TO WS-TOTAL-VENDEUR
                           MOVE 0 TO WS-COUNT-VENDEUR
                       END-IF
                       PERFORM AFFICHER-LIGNE-VENTE
                       ADD VTE-MONTANT TO WS-TOTAL
                       ADD VTE-MONTANT TO WS-TOTAL-VENDEUR
                       ADD 1 TO WS-COUNT
                       ADD 1 TO WS-COUNT-VENDEUR
                       IF VTE-MONTANT > WS-MAX
                           MOVE VTE-MONTANT TO WS-MAX
                       END-IF
                       IF VTE-MONTANT < WS-MIN
                           MOVE VTE-MONTANT TO WS-MIN
                       END-IF
                       ADD 1 TO WS-LIGNE
               END-READ
           END-PERFORM.
           IF WS-COUNT > 0
               COMPUTE WS-MOYENNE = WS-TOTAL / WS-COUNT
           END-IF.
           PERFORM AFFICHER-SYNTHESE.
           CLOSE VENTES.
           STOP RUN.

       AFFICHER-ENTETE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '+' LINE 2 POSITION 15.
           DISPLAY '------------------------------------------------'
               LINE 2 POSITION 16.
           DISPLAY '+' LINE 2 POSITION 64.
           DISPLAY '|' LINE 3 POSITION 15.
           DISPLAY '           RAPPORT DES VENTES                   '
               LINE 3 POSITION 17 HIGHLIGHT.
           DISPLAY '|' LINE 3 POSITION 64.
           DISPLAY '+' LINE 4 POSITION 15.
           DISPLAY '------------------------------------------------'
               LINE 4 POSITION 16.
           DISPLAY '+' LINE 4 POSITION 64.
           DISPLAY 'DATE' LINE 6 POSITION 5 UNDERLINE.
           DISPLAY 'VENDEUR' LINE 6 POSITION 16 UNDERLINE.
           DISPLAY 'CLIENT' LINE 6 POSITION 34 UNDERLINE.
           DISPLAY 'MONTANT' LINE 6 POSITION 56 UNDERLINE.
           DISPLAY '------------------------------------------------'
               LINE 7 POSITION 5.

       AFFICHER-LIGNE-VENTE.
           DISPLAY VTE-DATE LINE WS-LIGNE POSITION 5.
           DISPLAY VTE-VENDEUR LINE WS-LIGNE POSITION 16.
           DISPLAY VTE-CLIENT LINE WS-LIGNE POSITION 34.
           DISPLAY VTE-MONTANT LINE WS-LIGNE POSITION 56.

       AFFICHER-TOTAL-VENDEUR.
           ADD 1 TO WS-LIGNE.
           IF WS-LIGNE < 19
               DISPLAY '>>' LINE WS-LIGNE POSITION 8 HIGHLIGHT
               DISPLAY 'TOTAL' LINE WS-LIGNE POSITION 11 HIGHLIGHT
               DISPLAY WS-VENDEUR-PREC LINE WS-LIGNE POSITION 17
                   HIGHLIGHT
               DISPLAY ':' LINE WS-LIGNE POSITION 33
               DISPLAY WS-TOTAL-VENDEUR LINE WS-LIGNE POSITION 35
                   HIGHLIGHT
               DISPLAY '(' LINE WS-LIGNE POSITION 48
               DISPLAY WS-COUNT-VENDEUR LINE WS-LIGNE POSITION 49
               DISPLAY 'vtes)' LINE WS-LIGNE POSITION 54
               ADD 1 TO WS-LIGNE
           END-IF.

       AFFICHER-SYNTHESE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '+' LINE 3 POSITION 20.
           DISPLAY '--------------------------------------'
               LINE 3 POSITION 21.
           DISPLAY '+' LINE 3 POSITION 59.
           DISPLAY '|' LINE 4 POSITION 20.
           DISPLAY '          SYNTHESE VENTES            '
               LINE 4 POSITION 22 HIGHLIGHT.
           DISPLAY '|' LINE 4 POSITION 59.
           DISPLAY '+' LINE 5 POSITION 20.
           DISPLAY '--------------------------------------'
               LINE 5 POSITION 21.
           DISPLAY '+' LINE 5 POSITION 59.
           DISPLAY '|' LINE 7 POSITION 20.
           DISPLAY 'Nombre de ventes:' LINE 7 POSITION 24.
           DISPLAY WS-COUNT LINE 7 POSITION 45 HIGHLIGHT.
           DISPLAY '|' LINE 7 POSITION 59.
           DISPLAY '|' LINE 8 POSITION 20.
           DISPLAY '|' LINE 8 POSITION 59.
           DISPLAY '|' LINE 9 POSITION 20.
           DISPLAY 'Total general:' LINE 9 POSITION 24.
           DISPLAY WS-TOTAL LINE 9 POSITION 42 HIGHLIGHT.
           DISPLAY 'EUR' LINE 9 POSITION 55.
           DISPLAY '|' LINE 9 POSITION 59.
           DISPLAY '|' LINE 10 POSITION 20.
           DISPLAY '|' LINE 10 POSITION 59.
           DISPLAY '|' LINE 11 POSITION 20.
           DISPLAY 'Vente moyenne:' LINE 11 POSITION 24.
           DISPLAY WS-MOYENNE LINE 11 POSITION 45.
           DISPLAY 'EUR' LINE 11 POSITION 55.
           DISPLAY '|' LINE 11 POSITION 59.
           DISPLAY '|' LINE 12 POSITION 20.
           DISPLAY '|' LINE 12 POSITION 59.
           DISPLAY '|' LINE 13 POSITION 20.
           DISPLAY 'Plus grande vente:' LINE 13 POSITION 24.
           DISPLAY WS-MAX LINE 13 POSITION 45 HIGHLIGHT.
           DISPLAY 'EUR' LINE 13 POSITION 55.
           DISPLAY '|' LINE 13 POSITION 59.
           DISPLAY '|' LINE 14 POSITION 20.
           DISPLAY 'Plus petite vente:' LINE 14 POSITION 24.
           DISPLAY WS-MIN LINE 14 POSITION 45.
           DISPLAY 'EUR' LINE 14 POSITION 55.
           DISPLAY '|' LINE 14 POSITION 59.
           DISPLAY '|' LINE 15 POSITION 20.
           DISPLAY '|' LINE 15 POSITION 59.
           DISPLAY '+' LINE 16 POSITION 20.
           DISPLAY '--------------------------------------'
               LINE 16 POSITION 21.
           DISPLAY '+' LINE 16 POSITION 59.
           DISPLAY 'Appuyez sur ENTREE...' LINE 20 POSITION 29 BLINK.
           ACCEPT WS-CHOIX LINE 20 POSITION 51.

