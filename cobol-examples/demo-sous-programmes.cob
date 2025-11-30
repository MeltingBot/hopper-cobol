       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO-SOUS-PROGRAMMES.
      *===============================================
      * DEMONSTRATION CALL/COPY - SOUS-PROGRAMMES
      *===============================================
      * Ce programme demontre les fonctionnalites
      * CALL et COPY disponibles dans HOPPER COBOL:
      * - CALL "paragraphe" pour appeler un sous-prog
      * - USING BY REFERENCE (modifiable)
      * - USING BY CONTENT (lecture seule)
      * - ON EXCEPTION pour gerer les erreurs
      * - COPY pour inclure des copybooks
      *===============================================
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *--- Variables de travail ---
       01 WS-MONTANT-HT      PIC 9(6)V99 VALUE 1000.00.
       01 WS-TAUX-TVA        PIC 9V99 VALUE 0.20.
       01 WS-MONTANT-TVA     PIC 9(6)V99 VALUE 0.
       01 WS-MONTANT-TTC     PIC 9(7)V99 VALUE 0.
       01 WS-REMISE          PIC 9(4)V99 VALUE 0.
       01 WS-TAUX-REMISE     PIC 9V99 VALUE 0.10.

      *--- Compteurs pour demonstration ---
       01 WS-COMPTEUR        PIC 99 VALUE 0.
       01 WS-TOTAL           PIC 9(4) VALUE 0.
       01 WS-I               PIC 99 VALUE 0.

      *--- Variables pour affichage ---
       01 WS-DISPLAY-HT      PIC Z(5)9.99.
       01 WS-DISPLAY-TVA     PIC Z(5)9.99.
       01 WS-DISPLAY-TTC     PIC Z(6)9.99.
       01 WS-DISPLAY-REMISE  PIC Z(3)9.99.

      *--- Inclusion du copybook DATE-VARS ---
       COPY DATE-VARS.

       PROCEDURE DIVISION.
       PROGRAMME-PRINCIPAL.
           DISPLAY "================================================".
           DISPLAY "  DEMONSTRATION CALL/COPY - SOUS-PROGRAMMES".
           DISPLAY "================================================".
           DISPLAY " ".

      *--- Demo 1: CALL simple ---
           DISPLAY ">>> DEMO 1: CALL simple".
           DISPLAY "    Appel du sous-programme AFFICHER-TITRE".
           CALL "AFFICHER-TITRE".
           DISPLAY " ".

      *--- Demo 2: CALL avec USING BY REFERENCE ---
           DISPLAY ">>> DEMO 2: CALL avec USING BY REFERENCE".
           DISPLAY "    Le sous-programme peut modifier les valeurs".
           MOVE 1500.00 TO WS-MONTANT-HT.
           MOVE WS-MONTANT-HT TO WS-DISPLAY-HT.
           DISPLAY "    Montant HT avant : " WS-DISPLAY-HT.
           CALL "CALCULER-TVA" USING WS-MONTANT-HT
                                     WS-TAUX-TVA
                                     WS-MONTANT-TVA
                                     WS-MONTANT-TTC.
           MOVE WS-MONTANT-TVA TO WS-DISPLAY-TVA.
           MOVE WS-MONTANT-TTC TO WS-DISPLAY-TTC.
           DISPLAY "    TVA calculee    : " WS-DISPLAY-TVA.
           DISPLAY "    Montant TTC     : " WS-DISPLAY-TTC.
           DISPLAY " ".

      *--- Demo 3: CALL avec BY CONTENT (lecture seule) ---
           DISPLAY ">>> DEMO 3: CALL avec BY CONTENT".
           DISPLAY "    Le montant original est preserve".
           MOVE 2000.00 TO WS-MONTANT-HT.
           MOVE WS-MONTANT-HT TO WS-DISPLAY-HT.
           DISPLAY "    Montant avant : " WS-DISPLAY-HT.
           CALL "APPLIQUER-REMISE" USING BY CONTENT WS-MONTANT-HT
                                         BY REFERENCE WS-REMISE
                                         WS-TAUX-REMISE.
           MOVE WS-MONTANT-HT TO WS-DISPLAY-HT.
           MOVE WS-REMISE TO WS-DISPLAY-REMISE.
           DISPLAY "    Montant apres : " WS-DISPLAY-HT
                   " (inchange car BY CONTENT)".
           DISPLAY "    Remise calc.  : " WS-DISPLAY-REMISE.
           DISPLAY " ".

      *--- Demo 4: CALL avec ON EXCEPTION ---
           DISPLAY ">>> DEMO 4: CALL avec ON EXCEPTION".
           DISPLAY "    Gestion d'erreur si sous-prog inexistant".
           CALL "SOUS-PROG-INEXISTANT"
               ON EXCEPTION
                   DISPLAY "    ! Erreur : sous-programme non trouve"
               NOT ON EXCEPTION
                   DISPLAY "    Appel reussi"
           END-CALL.
           DISPLAY " ".

      *--- Demo 5: CALL multiple avec boucle ---
           DISPLAY ">>> DEMO 5: CALL dans une boucle".
           MOVE 0 TO WS-TOTAL.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               CALL "INCREMENTER" USING WS-TOTAL WS-I
               DISPLAY "    Iteration " WS-I " - Total: " WS-TOTAL
           END-PERFORM.
           DISPLAY " ".

      *--- Demo 6: Utilisation du COPYBOOK ---
           DISPLAY ">>> DEMO 6: Utilisation du COPYBOOK DATE-VARS".
           MOVE 2024 TO WS-YEAR.
           MOVE 12 TO WS-MONTH.
           MOVE 25 TO WS-DAY.
           MOVE 14 TO WS-HOUR.
           MOVE 30 TO WS-MINUTE.
           STRING WS-DAY "/" WS-MONTH "/" WS-YEAR
               DELIMITED BY SIZE INTO WS-DATE-FORMATTED.
           DISPLAY "    Date formatee : " WS-DATE-FORMATTED.
           DISPLAY "    Heure         : " WS-HOUR ":" WS-MINUTE.
           DISPLAY " ".

           DISPLAY "================================================".
           DISPLAY "  FIN DE LA DEMONSTRATION".
           DISPLAY "================================================".
           STOP RUN.

      *===============================================
      * SOUS-PROGRAMMES
      *===============================================
       AFFICHER-TITRE.
           DISPLAY "    +--------------------------+".
           DISPLAY "    |   SYSTEME DE FACTURATION |".
           DISPLAY "    |      HOPPER COBOL        |".
           DISPLAY "    +--------------------------+".

       CALCULER-TVA.
      *    Calcule la TVA et le TTC
           COMPUTE WS-MONTANT-TVA = WS-MONTANT-HT * WS-TAUX-TVA.
           COMPUTE WS-MONTANT-TTC = WS-MONTANT-HT + WS-MONTANT-TVA.

       APPLIQUER-REMISE.
      *    Calcule une remise sur le montant
           COMPUTE WS-REMISE = WS-MONTANT-HT * WS-TAUX-REMISE.

       INCREMENTER.
      *    Ajoute une valeur au total
           ADD WS-I TO WS-TOTAL.
