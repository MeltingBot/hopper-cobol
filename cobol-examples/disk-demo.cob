       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISK-DEMO.
      *================================================================*
      * DEMONSTRATION VISUELLE DU DISQUE HOPPER 33XX                   *
      * Gestion clients + journal d'activite                           *
      * Remplit, lit a l'envers, supprime, recommence                  *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTS ASSIGN TO "CLIENTS"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-ID.

           SELECT JOURNAL ASSIGN TO "JOURNAL"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD CLIENTS.
       01 CLIENT-REC.
           05 CLI-ID        PIC 9(3).
           05 CLI-NOM       PIC X(12).
           05 CLI-SOLDE     PIC 9(6).

       FD JOURNAL.
       01 JOURNAL-REC.
           05 JRN-TIME      PIC 9(6).
           05 JRN-OP        PIC X(6).
           05 JRN-CLI       PIC 9(3).
           05 JRN-INFO      PIC X(15).

       WORKING-STORAGE SECTION.
       01 WS-I              PIC 9(3).
       01 WS-J              PIC 9(2).
       01 WS-CYCLE          PIC 9 VALUE 0.
       01 WS-MAX            PIC 9(3) VALUE 30.
       01 WS-TIME           PIC 9(6) VALUE 100000.

       01 WS-NOMS.
           05 FILLER PIC X(12) VALUE "DUPONT".
           05 FILLER PIC X(12) VALUE "MARTIN".
           05 FILLER PIC X(12) VALUE "DURAND".
           05 FILLER PIC X(12) VALUE "MOREAU".
           05 FILLER PIC X(12) VALUE "LAURENT".
           05 FILLER PIC X(12) VALUE "SIMON".
           05 FILLER PIC X(12) VALUE "MICHEL".
           05 FILLER PIC X(12) VALUE "LEROY".
           05 FILLER PIC X(12) VALUE "ROUX".
           05 FILLER PIC X(12) VALUE "DAVID".
       01 WS-NOM-TBL REDEFINES WS-NOMS.
           05 WS-NOM PIC X(12) OCCURS 10.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "======================================".
           DISPLAY "  DEMO VISUELLE HOPPER 33XX".
           DISPLAY "  Clients + Journal d'activite".
           DISPLAY "======================================".
           DISPLAY " ".

           OPEN OUTPUT JOURNAL.

           PERFORM DEMO-CYCLE 2 TIMES.

           CLOSE JOURNAL.

           DISPLAY " ".
           DISPLAY "======================================".
           DISPLAY "  FIN DE LA DEMONSTRATION".
           DISPLAY "======================================".
           STOP RUN.

       DEMO-CYCLE.
           ADD 1 TO WS-CYCLE.
           DISPLAY " ".
           DISPLAY "============ CYCLE " WS-CYCLE " ============".

      *--- CREATION CLIENTS ---
           DISPLAY " ".
           DISPLAY ">> CREATION DE " WS-MAX " CLIENTS".
           OPEN OUTPUT CLIENTS.

           MOVE 1 TO WS-J.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MAX
               MOVE WS-I TO CLI-ID
               MOVE WS-NOM(WS-J) TO CLI-NOM
               MULTIPLY WS-I BY 1000 GIVING CLI-SOLDE
               WRITE CLIENT-REC
               DISPLAY "  +CLI " CLI-ID " " CLI-NOM
               PERFORM LOG-WRITE
               ADD 1 TO WS-J
               IF WS-J > 10
                   MOVE 1 TO WS-J
               END-IF
           END-PERFORM.

           CLOSE CLIENTS.

      *--- LECTURE INVERSE ---
           DISPLAY " ".
           DISPLAY ">> CONSULTATION INVERSE".
           OPEN INPUT CLIENTS.

           PERFORM VARYING WS-I FROM WS-MAX BY -1 UNTIL WS-I < 1
               MOVE WS-I TO CLI-ID
               READ CLIENTS KEY IS CLI-ID
                   NOT INVALID KEY
                       DISPLAY "  ?CLI " CLI-ID " " CLI-NOM
                               " $" CLI-SOLDE
                       PERFORM LOG-READ
               END-READ
           END-PERFORM.

           CLOSE CLIENTS.

      *--- MISE A JOUR SOLDES ---
           DISPLAY " ".
           DISPLAY ">> MISE A JOUR SOLDES (x10)".
           OPEN I-O CLIENTS.

           PERFORM VARYING WS-I FROM 5 BY 5 UNTIL WS-I > WS-MAX
               MOVE WS-I TO CLI-ID
               READ CLIENTS KEY IS CLI-ID
                   NOT INVALID KEY
                       MULTIPLY 10 BY CLI-SOLDE
                       REWRITE CLIENT-REC
                       DISPLAY "  *CLI " CLI-ID " -> $" CLI-SOLDE
                       PERFORM LOG-UPDATE
               END-READ
           END-PERFORM.

           CLOSE CLIENTS.

      *--- SUPPRESSION PAIRS ---
           DISPLAY " ".
           DISPLAY ">> SUPPRESSION CLIENTS PAIRS".
           OPEN I-O CLIENTS.

           PERFORM VARYING WS-I FROM 2 BY 2 UNTIL WS-I > WS-MAX
               MOVE WS-I TO CLI-ID
               DELETE CLIENTS
               DISPLAY "  -CLI " CLI-ID
               PERFORM LOG-DELETE
           END-PERFORM.

           CLOSE CLIENTS.

      *--- LOG OPERATIONS ---
       LOG-WRITE.
           ADD 1 TO WS-TIME.
           MOVE WS-TIME TO JRN-TIME.
           MOVE "CREATE" TO JRN-OP.
           MOVE CLI-ID TO JRN-CLI.
           MOVE CLI-NOM TO JRN-INFO.
           WRITE JOURNAL-REC.

       LOG-READ.
           ADD 1 TO WS-TIME.
           MOVE WS-TIME TO JRN-TIME.
           MOVE "READ" TO JRN-OP.
           MOVE CLI-ID TO JRN-CLI.
           MOVE CLI-NOM TO JRN-INFO.
           WRITE JOURNAL-REC.

       LOG-UPDATE.
           ADD 1 TO WS-TIME.
           MOVE WS-TIME TO JRN-TIME.
           MOVE "UPDATE" TO JRN-OP.
           MOVE CLI-ID TO JRN-CLI.
           MOVE "SOLDE MAJ" TO JRN-INFO.
           WRITE JOURNAL-REC.

       LOG-DELETE.
           ADD 1 TO WS-TIME.
           MOVE WS-TIME TO JRN-TIME.
           MOVE "DELETE" TO JRN-OP.
           MOVE CLI-ID TO JRN-CLI.
           MOVE "SUPPRIME" TO JRN-INFO.
           WRITE JOURNAL-REC.
