       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSVEXPORT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DONNEES ASSIGN TO 'DONNEES.DAT'
               ORGANIZATION IS SEQUENTIAL.
           SELECT SORTIE ASSIGN TO 'EXPORT.CSV'
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DONNEES.
       01 DATA-REC.
           05 DAT-CODE PIC X(6).
           05 DAT-NOM PIC X(20).
           05 DAT-VALEUR PIC 9(7)V99.
           05 DAT-DATE PIC 9(8).
       FD SORTIE.
       01 CSV-REC PIC X(100).
       WORKING-STORAGE SECTION.
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-COUNT PIC 9(5) VALUE 0.
       01 WS-LIGNE PIC X(100) VALUE SPACES.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT DONNEES.
           OPEN OUTPUT SORTIE.
           MOVE 'CODE;NOM;VALEUR;DATE' TO CSV-REC.
           WRITE CSV-REC.
           PERFORM UNTIL WS-EOF = 1
               READ DONNEES
                   AT END
                       MOVE 1 TO WS-EOF
                   NOT AT END
                       PERFORM FORMATER-LIGNE
                       WRITE CSV-REC FROM WS-LIGNE
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM.
           DISPLAY '=== EXPORT TERMINE ==='.
           DISPLAY 'LIGNES EXPORTEES: ' WS-COUNT.
           DISPLAY 'FICHIER: EXPORT.CSV'.
           CLOSE DONNEES.
           CLOSE SORTIE.
           STOP RUN.
       FORMATER-LIGNE.
           STRING DAT-CODE DELIMITED SIZE
               ';' DELIMITED SIZE
               DAT-NOM DELIMITED SPACE
               ';' DELIMITED SIZE
               DAT-VALEUR DELIMITED SIZE
               ';' DELIMITED SIZE
               DAT-DATE DELIMITED SIZE
               INTO WS-LIGNE.