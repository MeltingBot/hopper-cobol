       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO-ECRAN.
      *===============================================
      * DEMONSTRATION DES EXTENSIONS SCREEN CONTROL
      * Compatible IBM 3270 / MicroFocus COBOL
      *===============================================
      * Ce programme demontre toutes les fonctionnalites
      * d'affichage ecran disponibles dans HOPPER COBOL:
      * - LINE / POSITION (positionnement curseur)
      * - ERASE EOS / EOL / SCREEN (effacement)
      * - HIGHLIGHT (texte lumineux)
      * - BLINK (clignotement)
      * - REVERSE-VIDEO (inversion couleurs)
      * - UNDERLINE (soulignement)
      *===============================================
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DUMMY PIC X VALUE SPACE.
       01 WS-NOM PIC X(20) VALUE SPACES.
       01 WS-AGE PIC 99 VALUE 0.
       01 WS-COMPTEUR PIC 99 VALUE 0.
       PROCEDURE DIVISION.
       DEBUT.
      *--- Ecran 1: Presentation des attributs ---
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '+' LINE 1 POSITION 1.
           DISPLAY '---------------------------------------'
               LINE 1 POSITION 2.
           DISPLAY '+' LINE 1 POSITION 41.
           DISPLAY '|' LINE 2 POSITION 1.
           DISPLAY '  DEMO ATTRIBUTS ECRAN IBM 3270  ' LINE 2
               POSITION 4 HIGHLIGHT.
           DISPLAY '|' LINE 2 POSITION 41.
           DISPLAY '+' LINE 3 POSITION 1.
           DISPLAY '---------------------------------------'
               LINE 3 POSITION 2.
           DISPLAY '+' LINE 3 POSITION 41.

           DISPLAY 'Texte normal' LINE 6 POSITION 5.
           DISPLAY 'Texte HIGHLIGHT' LINE 7 POSITION 5 HIGHLIGHT.
           DISPLAY 'Texte BLINK' LINE 8 POSITION 5 BLINK.
           DISPLAY 'Texte REVERSE-VIDEO' LINE 9 POSITION 5
               REVERSE-VIDEO.
           DISPLAY 'Texte UNDERLINE' LINE 10 POSITION 5 UNDERLINE.
           DISPLAY 'Combinaison HIGHLIGHT + UNDERLINE' LINE 11
               POSITION 5 HIGHLIGHT UNDERLINE.

           DISPLAY 'Appuyez ENTREE pour continuer...' LINE 20
               POSITION 5 BLINK.
           ACCEPT WS-DUMMY.

      *--- Ecran 2: Formulaire interactif ---
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '========================================='
               LINE 2 POSITION 20 HIGHLIGHT.
           DISPLAY '      FORMULAIRE DE SAISIE       '
               LINE 3 POSITION 20 REVERSE-VIDEO.
           DISPLAY '========================================='
               LINE 4 POSITION 20 HIGHLIGHT.

           DISPLAY 'Nom:' LINE 8 POSITION 15.
           DISPLAY '[                    ]' LINE 8 POSITION 25
               REVERSE-VIDEO.
           ACCEPT WS-NOM LINE 8 POSITION 26.

           DISPLAY 'Age:' LINE 10 POSITION 15.
           DISPLAY '[  ]' LINE 10 POSITION 25 REVERSE-VIDEO.
           ACCEPT WS-AGE LINE 10 POSITION 26.

           DISPLAY 'Vous avez saisi:' LINE 14 POSITION 15 UNDERLINE.
           DISPLAY 'Nom: ' LINE 16 POSITION 15.
           DISPLAY WS-NOM LINE 16 POSITION 20 HIGHLIGHT.
           DISPLAY 'Age: ' LINE 17 POSITION 15.
           DISPLAY WS-AGE LINE 17 POSITION 20 HIGHLIGHT.

           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 30.
           ACCEPT WS-DUMMY.

      *--- Ecran 3: Animation simple ---
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY 'ANIMATION - Compteur' LINE 3 POSITION 30
               HIGHLIGHT.
           DISPLAY 'Valeur:' LINE 10 POSITION 30.
           PERFORM VARYING WS-COMPTEUR FROM 1 BY 1
               UNTIL WS-COMPTEUR > 10
               DISPLAY WS-COMPTEUR LINE 10 POSITION 38
                   HIGHLIGHT BLINK
           END-PERFORM.
           DISPLAY '  ' LINE 10 POSITION 38.
           DISPLAY 'TERMINE!' LINE 10 POSITION 38 REVERSE-VIDEO.

           DISPLAY 'Appuyez ENTREE pour terminer...' LINE 20
               POSITION 25.
           ACCEPT WS-DUMMY.

      *--- Ecran final ---
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY 'Merci d avoir utilise cette demo!'
               LINE 12 POSITION 23 HIGHLIGHT.
           DISPLAY '(c) HOPPER COBOL Emulator' LINE 14 POSITION 28.
           STOP RUN.
