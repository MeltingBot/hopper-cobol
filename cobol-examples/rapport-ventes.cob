       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAPVENTES.
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
       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT VENTES.
           DISPLAY '========================================'.
           DISPLAY '        RAPPORT DES VENTES              '.
           DISPLAY '========================================'.
           DISPLAY ' '.
           DISPLAY 'DATE     | VENDEUR    | CLIENT     | MT'.
           DISPLAY '----------------------------------------'.
           PERFORM UNTIL WS-EOF = 1
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
                       DISPLAY VTE-DATE ' | ' VTE-VENDEUR
                           ' | ' VTE-CLIENT ' | ' VTE-MONTANT
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
               END-READ
           END-PERFORM.
           IF WS-COUNT > 0
               COMPUTE WS-MOYENNE = WS-TOTAL / WS-COUNT
           END-IF.
           DISPLAY '========================================'.
           DISPLAY '           SYNTHESE                     '.
           DISPLAY '========================================'.
           DISPLAY 'NOMBRE DE VENTES: ' WS-COUNT.
           DISPLAY 'TOTAL GENERAL:    ' WS-TOTAL.
           DISPLAY 'VENTE MOYENNE:    ' WS-MOYENNE.
           DISPLAY 'PLUS GRANDE:      ' WS-MAX.
           DISPLAY 'PLUS PETITE:      ' WS-MIN.
           DISPLAY '========================================'.
           CLOSE VENTES.
           STOP RUN.
       AFFICHER-TOTAL-VENDEUR.
           DISPLAY '  >> TOTAL ' WS-VENDEUR-PREC ': '
               WS-TOTAL-VENDEUR ' (' WS-COUNT-VENDEUR ' vtes)'.