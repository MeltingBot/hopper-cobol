/**
 * COBOL Explanations Module
 * Provides pedagogical explanations for COBOL statements in French
 */

import { NodeType } from './parser.js';

/**
 * Get a pedagogical explanation for a COBOL statement
 * @param {object} stmt - The AST node for the statement
 * @param {object} context - Optional execution context (variables, etc.)
 * @returns {object} Explanation with title, description, and tips
 */
export function explainStatement(stmt, context = {}) {
    if (!stmt || !stmt.type) {
        return null;
    }

    const explanations = {
        [NodeType.DISPLAY]: explainDisplay,
        [NodeType.ACCEPT]: explainAccept,
        [NodeType.MOVE]: explainMove,
        [NodeType.ADD]: explainAdd,
        [NodeType.SUBTRACT]: explainSubtract,
        [NodeType.MULTIPLY]: explainMultiply,
        [NodeType.DIVIDE]: explainDivide,
        [NodeType.COMPUTE]: explainCompute,
        [NodeType.IF]: explainIf,
        [NodeType.EVALUATE]: explainEvaluate,
        [NodeType.PERFORM]: explainPerform,
        [NodeType.GO_TO]: explainGoTo,
        [NodeType.STOP_RUN]: explainStopRun,
        [NodeType.EXIT]: explainExit,
        [NodeType.INITIALIZE]: explainInitialize,
        [NodeType.OPEN]: explainOpen,
        [NodeType.CLOSE]: explainClose,
        [NodeType.READ]: explainRead,
        [NodeType.WRITE]: explainWrite,
        [NodeType.REWRITE]: explainRewrite,
        [NodeType.DELETE]: explainDelete,
        [NodeType.START]: explainStart,
        [NodeType.STRING]: explainString,
        [NodeType.UNSTRING]: explainUnstring,
        [NodeType.INSPECT]: explainInspect,
        [NodeType.SEARCH]: explainSearch,
        [NodeType.SET]: explainSet,
        [NodeType.CALL]: explainCall,
        [NodeType.CANCEL]: explainCancel,
        [NodeType.SORT]: explainSort,
        [NodeType.MERGE]: explainMerge,
        [NodeType.RELEASE]: explainRelease,
        [NodeType.RETURN]: explainReturn,
    };

    const explainer = explanations[stmt.type];
    if (explainer) {
        return explainer(stmt, context);
    }

    return {
        title: stmt.type,
        description: `Instruction ${stmt.type}`,
        category: 'general'
    };
}

// ============================================================================
// Individual Statement Explanations
// ============================================================================

function explainDisplay(stmt, context) {
    const items = stmt.items?.length || 0;
    const hasScreenControl = stmt.line || stmt.position || stmt.erase ||
                            stmt.highlight || stmt.blink || stmt.reverse;

    let description = `Affiche ${items} élément(s) à l'écran.`;

    if (hasScreenControl) {
        const controls = [];
        if (stmt.line) controls.push(`ligne ${stmt.line}`);
        if (stmt.position) controls.push(`colonne ${stmt.position}`);
        if (stmt.erase) controls.push(`effacement ${stmt.erase}`);
        if (stmt.highlight) controls.push('surbrillance');
        if (stmt.blink) controls.push('clignotant');
        if (stmt.reverse) controls.push('vidéo inverse');
        description += ` Contrôle écran: ${controls.join(', ')}.`;
    }

    return {
        title: '📺 DISPLAY - Affichage',
        description,
        syntax: 'DISPLAY valeur [WITH options]',
        tip: 'DISPLAY affiche des valeurs sur la sortie standard. Avec les extensions IBM 3270, vous pouvez contrôler la position et l\'apparence du texte.',
        category: 'io',
        example: 'DISPLAY "Total: " WS-TOTAL.'
    };
}

function explainAccept(stmt, context) {
    const varName = stmt.variable || '?';
    const hasScreenControl = stmt.line || stmt.position || stmt.secure;

    let description = `Attend une saisie utilisateur et la stocke dans ${varName}.`;

    if (stmt.secure) {
        description += ' (Mode sécurisé: saisie masquée)';
    }
    if (hasScreenControl && (stmt.line || stmt.position)) {
        description += ` Position: ligne ${stmt.line || 'courante'}, colonne ${stmt.position || 'courante'}.`;
    }

    return {
        title: '⌨️ ACCEPT - Saisie',
        description,
        syntax: 'ACCEPT variable [FROM source]',
        tip: 'ACCEPT lit une valeur depuis l\'entrée standard (clavier). La valeur est automatiquement convertie selon le PIC de la variable destination.',
        category: 'io',
        example: 'ACCEPT WS-NOM.'
    };
}

function explainMove(stmt, context) {
    const source = formatValue(stmt.source);
    const dests = stmt.destinations?.map(d => d.name || d).join(', ') || '?';

    return {
        title: '📋 MOVE - Affectation',
        description: `Copie ${source} vers ${dests}.`,
        syntax: 'MOVE source TO destination-1 [destination-2 ...]',
        tip: 'MOVE copie une valeur avec conversion automatique. Pour les alphanumériques, le texte est cadré à gauche et complété par des espaces. Pour les numériques, la valeur est cadrée à droite et complétée par des zéros.',
        category: 'data',
        example: 'MOVE "DUPONT" TO WS-NOM.'
    };
}

function explainAdd(stmt, context) {
    const values = stmt.values?.map(v => formatValue(v)).join(' + ') || '?';
    const to = stmt.to?.name || stmt.giving?.name || '?';

    let description = `Additionne ${values}`;
    if (stmt.giving) {
        description += ` et stocke le résultat dans ${stmt.giving.name}`;
    } else if (stmt.to) {
        description += ` à ${to}`;
    }
    description += '.';

    return {
        title: '➕ ADD - Addition',
        description,
        syntax: 'ADD valeur-1 [valeur-2] TO variable [GIVING résultat]',
        tip: 'ADD additionne des valeurs numériques. Avec GIVING, le résultat va dans une nouvelle variable. Sans GIVING, la somme s\'ajoute à la variable TO.',
        category: 'arithmetic',
        example: 'ADD 1 TO WS-COMPTEUR.'
    };
}

function explainSubtract(stmt, context) {
    const values = stmt.values?.map(v => formatValue(v)).join(' + ') || '?';
    const from = stmt.from?.name || '?';

    let description = `Soustrait ${values} de ${from}`;
    if (stmt.giving) {
        description += ` et stocke dans ${stmt.giving.name}`;
    }
    description += '.';

    return {
        title: '➖ SUBTRACT - Soustraction',
        description,
        syntax: 'SUBTRACT valeur FROM variable [GIVING résultat]',
        tip: 'SUBTRACT soustrait une ou plusieurs valeurs d\'une variable. Le résultat peut être stocké dans une autre variable avec GIVING.',
        category: 'arithmetic',
        example: 'SUBTRACT WS-REMISE FROM WS-TOTAL.'
    };
}

function explainMultiply(stmt, context) {
    const value = formatValue(stmt.value);
    const by = stmt.by?.name || '?';

    let description = `Multiplie ${value} par ${by}`;
    if (stmt.giving) {
        description += ` et stocke dans ${stmt.giving.name}`;
    }
    description += '.';

    return {
        title: '✖️ MULTIPLY - Multiplication',
        description,
        syntax: 'MULTIPLY valeur BY variable [GIVING résultat]',
        tip: 'MULTIPLY effectue une multiplication. Utilisez GIVING pour stocker le résultat sans modifier les opérandes d\'origine.',
        category: 'arithmetic',
        example: 'MULTIPLY WS-QTE BY WS-PRIX GIVING WS-MONTANT.'
    };
}

function explainDivide(stmt, context) {
    const dividend = formatValue(stmt.dividend);
    const divisor = formatValue(stmt.divisor);

    let description = `Divise ${dividend} par ${divisor}`;
    if (stmt.giving) {
        description += ` → quotient dans ${stmt.giving.name}`;
    }
    if (stmt.remainder) {
        description += `, reste dans ${stmt.remainder.name}`;
    }
    description += '.';

    return {
        title: '➗ DIVIDE - Division',
        description,
        syntax: 'DIVIDE dividende BY diviseur [GIVING quotient] [REMAINDER reste]',
        tip: 'DIVIDE effectue une division entière ou décimale. REMAINDER capture le reste de la division. Attention à la division par zéro !',
        category: 'arithmetic',
        example: 'DIVIDE WS-TOTAL BY 12 GIVING WS-MENSUEL.'
    };
}

function explainCompute(stmt, context) {
    const target = stmt.target?.name || '?';

    return {
        title: '🔢 COMPUTE - Calcul',
        description: `Évalue une expression arithmétique et stocke le résultat dans ${target}.`,
        syntax: 'COMPUTE variable = expression arithmétique',
        tip: 'COMPUTE permet d\'écrire des expressions complexes avec +, -, *, /, ** (puissance) et parenthèses. C\'est souvent plus lisible que plusieurs ADD/SUBTRACT/MULTIPLY.',
        category: 'arithmetic',
        example: 'COMPUTE WS-TTC = WS-HT * (1 + WS-TVA / 100).'
    };
}

function explainIf(stmt, context) {
    const hasElse = stmt.else && stmt.else.length > 0;

    return {
        title: '❓ IF - Condition',
        description: `Teste une condition${hasElse ? ' avec une branche alternative' : ''}.`,
        syntax: 'IF condition THEN instructions [ELSE instructions] END-IF',
        tip: 'IF évalue une condition booléenne. Les opérateurs: = (égal), > (supérieur), < (inférieur), NOT, AND, OR. Les 88-levels peuvent être testés directement par leur nom.',
        category: 'control',
        example: 'IF WS-AGE >= 18 THEN DISPLAY "Majeur" END-IF.'
    };
}

function explainEvaluate(stmt, context) {
    const cases = stmt.whens?.length || 0;
    const hasOther = stmt.whenOther && stmt.whenOther.length > 0;

    return {
        title: '🔀 EVALUATE - Aiguillage',
        description: `Structure de sélection multiple avec ${cases} cas${hasOther ? ' et un cas par défaut' : ''}.`,
        syntax: 'EVALUATE sujet WHEN valeur-1 ... WHEN valeur-2 ... [WHEN OTHER ...] END-EVALUATE',
        tip: 'EVALUATE est le "switch/case" du COBOL. Plus lisible que plusieurs IF imbriqués. WHEN OTHER capture tous les cas non traités.',
        category: 'control',
        example: 'EVALUATE WS-JOUR\n  WHEN 1 DISPLAY "Lundi"\n  WHEN 2 DISPLAY "Mardi"\n  WHEN OTHER DISPLAY "Autre"\nEND-EVALUATE.'
    };
}

function explainPerform(stmt, context) {
    let description = 'Exécute ';

    if (stmt.paragraph) {
        description += `le paragraphe "${stmt.paragraph}"`;
        if (stmt.thru) {
            description += ` jusqu'à "${stmt.thru}"`;
        }
    } else if (stmt.inline) {
        description += 'un bloc d\'instructions';
    } else {
        description += 'des instructions';
    }

    if (stmt.times) {
        description += ` ${formatValue(stmt.times)} fois`;
    } else if (stmt.until) {
        description += ' jusqu\'à ce que la condition soit vraie';
    } else if (stmt.varying) {
        description += ` en itérant ${stmt.varying.variable?.name || 'un index'}`;
    }
    description += '.';

    return {
        title: '🔄 PERFORM - Exécution/Boucle',
        description,
        syntax: 'PERFORM paragraphe [TIMES n | UNTIL condition | VARYING var FROM x BY y UNTIL condition]',
        tip: 'PERFORM est la structure de boucle principale en COBOL. Il peut appeler un paragraphe ou exécuter des instructions en ligne. "PERFORM UNTIL" teste AVANT chaque itération.',
        category: 'control',
        example: 'PERFORM TRAITEMENT 10 TIMES.'
    };
}

function explainGoTo(stmt, context) {
    const target = stmt.target || '?';

    return {
        title: '➡️ GO TO - Saut',
        description: `Transfère l'exécution au paragraphe "${target}".`,
        syntax: 'GO TO paragraphe',
        tip: '⚠️ GO TO est considéré comme une mauvaise pratique (code spaghetti). Préférez PERFORM pour une structure plus claire. GO TO reste utile pour sortir d\'une boucle en cas d\'erreur.',
        category: 'control',
        example: 'GO TO FIN-PROGRAMME.'
    };
}

function explainStopRun(stmt, context) {
    return {
        title: '🛑 STOP RUN - Arrêt',
        description: 'Termine l\'exécution du programme.',
        syntax: 'STOP RUN',
        tip: 'STOP RUN arrête définitivement le programme et rend le contrôle au système d\'exploitation. C\'est la fin normale d\'un programme COBOL.',
        category: 'control',
        example: 'STOP RUN.'
    };
}

function explainExit(stmt, context) {
    let target = 'du paragraphe';
    if (stmt.exitType === 'PARAGRAPH') target = 'du paragraphe';
    else if (stmt.exitType === 'SECTION') target = 'de la section';
    else if (stmt.exitType === 'PERFORM') target = 'de la boucle PERFORM';

    return {
        title: '🚪 EXIT - Sortie',
        description: `Sort ${target} courant.`,
        syntax: 'EXIT [PARAGRAPH | SECTION | PERFORM]',
        tip: 'EXIT seul ne fait rien (juste un point d\'ancrage). EXIT PARAGRAPH sort du paragraphe courant. Utile pour créer des points de sortie explicites.',
        category: 'control',
        example: 'EXIT PARAGRAPH.'
    };
}

function explainInitialize(stmt, context) {
    const targets = stmt.targets?.map(t => t.name || t).join(', ') || '?';

    return {
        title: '🔄 INITIALIZE - Réinitialisation',
        description: `Réinitialise ${targets} aux valeurs par défaut.`,
        syntax: 'INITIALIZE variable [REPLACING type BY valeur]',
        tip: 'INITIALIZE remet les champs alphanumériques à SPACES et les numériques à ZEROS. Plus propre que de MOVE SPACES/ZEROS à chaque champ.',
        category: 'data',
        example: 'INITIALIZE WS-ENREGISTREMENT.'
    };
}

function explainOpen(stmt, context) {
    const mode = stmt.mode || 'INPUT';
    const files = stmt.files?.join(', ') || '?';

    const modeExplain = {
        'INPUT': 'lecture seule',
        'OUTPUT': 'écriture (création/écrasement)',
        'I-O': 'lecture et écriture',
        'EXTEND': 'ajout en fin de fichier'
    };

    return {
        title: '📂 OPEN - Ouverture fichier',
        description: `Ouvre ${files} en mode ${modeExplain[mode] || mode}.`,
        syntax: 'OPEN mode fichier-1 [fichier-2 ...]',
        tip: 'Tout fichier doit être ouvert avant utilisation. INPUT pour lire, OUTPUT pour écrire (écrase le contenu existant), I-O pour les deux, EXTEND pour ajouter à la fin.',
        category: 'file',
        example: 'OPEN INPUT FICHIER-CLIENTS.'
    };
}

function explainClose(stmt, context) {
    const files = stmt.files?.join(', ') || '?';

    return {
        title: '📁 CLOSE - Fermeture fichier',
        description: `Ferme ${files}.`,
        syntax: 'CLOSE fichier-1 [fichier-2 ...]',
        tip: 'Fermez toujours vos fichiers ! CLOSE libère les ressources et garantit que toutes les données sont écrites sur le disque.',
        category: 'file',
        example: 'CLOSE FICHIER-CLIENTS.'
    };
}

function explainRead(stmt, context) {
    const file = stmt.file || '?';

    return {
        title: '📖 READ - Lecture',
        description: `Lit un enregistrement depuis ${file}.`,
        syntax: 'READ fichier [INTO variable] [AT END instructions] [NOT AT END instructions] END-READ',
        tip: 'READ lit le prochain enregistrement (séquentiel) ou un enregistrement par clé (indexé). AT END détecte la fin du fichier. INTO copie dans une variable de travail.',
        category: 'file',
        example: 'READ FICHIER-CLIENTS\n  AT END SET FIN-FICHIER TO TRUE\nEND-READ.'
    };
}

function explainWrite(stmt, context) {
    const record = stmt.record || '?';

    return {
        title: '✏️ WRITE - Écriture',
        description: `Écrit l'enregistrement ${record} dans le fichier.`,
        syntax: 'WRITE enregistrement [FROM variable]',
        tip: 'WRITE ajoute un nouvel enregistrement au fichier. Pour les fichiers indexés, la clé doit être unique. FROM permet d\'écrire depuis une variable de travail.',
        category: 'file',
        example: 'WRITE ENREG-CLIENT FROM WS-CLIENT.'
    };
}

function explainRewrite(stmt, context) {
    const record = stmt.record || '?';

    return {
        title: '📝 REWRITE - Mise à jour',
        description: `Met à jour l'enregistrement ${record} en place.`,
        syntax: 'REWRITE enregistrement [FROM variable]',
        tip: 'REWRITE modifie l\'enregistrement qui vient d\'être lu. Le fichier doit être ouvert en mode I-O. La longueur de l\'enregistrement ne doit pas changer.',
        category: 'file',
        example: 'REWRITE ENREG-CLIENT.'
    };
}

function explainDelete(stmt, context) {
    const file = stmt.file || '?';

    return {
        title: '🗑️ DELETE - Suppression',
        description: `Supprime l'enregistrement courant de ${file}.`,
        syntax: 'DELETE fichier [INVALID KEY instructions] END-DELETE',
        tip: 'DELETE supprime l\'enregistrement qui vient d\'être lu. Pour les fichiers indexés, la clé de l\'enregistrement courant est utilisée.',
        category: 'file',
        example: 'DELETE FICHIER-CLIENTS.'
    };
}

function explainStart(stmt, context) {
    const file = stmt.file || '?';

    return {
        title: '🎯 START - Positionnement',
        description: `Positionne le fichier ${file} à un enregistrement spécifique.`,
        syntax: 'START fichier KEY condition [INVALID KEY instructions] END-START',
        tip: 'START positionne le pointeur de fichier pour une lecture séquentielle à partir d\'une clé donnée. Utile pour les fichiers indexés.',
        category: 'file',
        example: 'START FICHIER-CLIENTS KEY >= WS-CLE-RECHERCHE.'
    };
}

function explainString(stmt, context) {
    const sources = stmt.sources?.length || 0;
    const target = stmt.into?.name || '?';

    return {
        title: '🔗 STRING - Concaténation',
        description: `Concatène ${sources} élément(s) dans ${target}.`,
        syntax: 'STRING source-1 DELIMITED BY délimiteur-1 source-2 DELIMITED BY ... INTO variable [WITH POINTER ptr] END-STRING',
        tip: 'STRING concatène des chaînes. DELIMITED BY SIZE prend tout le champ. DELIMITED BY SPACE s\'arrête au premier espace. WITH POINTER permet de continuer à une position.',
        category: 'string',
        example: 'STRING WS-PRENOM DELIMITED BY SPACE\n  " " DELIMITED BY SIZE\n  WS-NOM DELIMITED BY SPACE\n  INTO WS-NOM-COMPLET.'
    };
}

function explainUnstring(stmt, context) {
    const source = stmt.source?.name || '?';
    const targets = stmt.targets?.length || 0;

    return {
        title: '✂️ UNSTRING - Découpage',
        description: `Découpe ${source} en ${targets} partie(s).`,
        syntax: 'UNSTRING source DELIMITED BY délimiteur INTO dest-1 dest-2 ... [TALLYING compteur] END-UNSTRING',
        tip: 'UNSTRING est l\'inverse de STRING. Il découpe une chaîne selon un délimiteur. TALLYING compte le nombre de parties trouvées.',
        category: 'string',
        example: 'UNSTRING WS-LIGNE DELIMITED BY ";"\n  INTO WS-CHAMP1 WS-CHAMP2 WS-CHAMP3.'
    };
}

function explainInspect(stmt, context) {
    const target = stmt.target?.name || '?';
    let action = '';

    if (stmt.tallying) action = 'compte les occurrences';
    else if (stmt.replacing) action = 'remplace des caractères';
    else if (stmt.converting) action = 'convertit des caractères';

    return {
        title: '🔍 INSPECT - Analyse/Transformation',
        description: `Analyse ${target} et ${action}.`,
        syntax: 'INSPECT variable [TALLYING compteur FOR ...] [REPLACING ...] [CONVERTING ... TO ...]',
        tip: 'INSPECT est un outil puissant pour analyser et transformer des chaînes. TALLYING compte, REPLACING remplace, CONVERTING fait une substitution caractère par caractère.',
        category: 'string',
        example: 'INSPECT WS-TEXTE CONVERTING "abc" TO "ABC".'
    };
}

function explainSearch(stmt, context) {
    const table = stmt.table?.name || '?';
    const isAll = stmt.all || false;

    return {
        title: isAll ? '🔎 SEARCH ALL - Recherche binaire' : '🔍 SEARCH - Recherche linéaire',
        description: `Recherche dans la table ${table}${isAll ? ' (dichotomique)' : ' (séquentielle)'}.`,
        syntax: isAll
            ? 'SEARCH ALL table [AT END instructions] WHEN condition instructions END-SEARCH'
            : 'SEARCH table [AT END instructions] [WHEN condition instructions] END-SEARCH',
        tip: isAll
            ? 'SEARCH ALL effectue une recherche binaire (rapide). La table DOIT être triée selon la clé de recherche !'
            : 'SEARCH parcourt la table élément par élément. L\'index associé est automatiquement incrémenté.',
        category: 'table',
        example: 'SEARCH WS-TABLE\n  AT END DISPLAY "Non trouvé"\n  WHEN WS-CODE(IDX) = WS-RECHERCHE\n    DISPLAY WS-LIBELLE(IDX)\nEND-SEARCH.'
    };
}

function explainSet(stmt, context) {
    let description = '';

    if (stmt.toTrue !== undefined) {
        description = `Met la condition 88 "${stmt.target?.name}" à TRUE.`;
    } else if (stmt.upBy !== undefined) {
        description = `Incrémente ${stmt.target?.name} de ${formatValue(stmt.upBy)}.`;
    } else if (stmt.downBy !== undefined) {
        description = `Décrémente ${stmt.target?.name} de ${formatValue(stmt.downBy)}.`;
    } else {
        description = `Affecte une valeur à ${stmt.target?.name || '?'}.`;
    }

    return {
        title: '⚙️ SET - Positionnement',
        description,
        syntax: 'SET variable TO valeur | SET variable UP/DOWN BY n | SET condition TO TRUE',
        tip: 'SET est polyvalent : il positionne des index, des pointeurs, ou active des conditions 88-level. Pour les 88-levels, "SET condition TO TRUE" est plus lisible que MOVE.',
        category: 'data',
        example: 'SET IDX-PRODUIT TO 1.\nSET CLIENT-ACTIF TO TRUE.'
    };
}

function explainCall(stmt, context) {
    const program = stmt.program || '?';
    const args = stmt.using?.length || 0;

    return {
        title: '📞 CALL - Appel sous-programme',
        description: `Appelle "${program}"${args > 0 ? ` avec ${args} paramètre(s)` : ''}.`,
        syntax: 'CALL "programme" [USING paramètres] [ON EXCEPTION instructions] END-CALL',
        tip: 'CALL invoque un sous-programme (paragraphe interne ou module externe). BY REFERENCE permet au sous-programme de modifier les paramètres. BY CONTENT envoie une copie.',
        category: 'control',
        example: 'CALL "CALCUL-TVA" USING WS-MONTANT-HT WS-TVA.'
    };
}

function explainCancel(stmt, context) {
    const programs = stmt.programs?.join(', ') || '?';

    return {
        title: '🚫 CANCEL - Annulation',
        description: `Libère les ressources du(des) sous-programme(s): ${programs}.`,
        syntax: 'CANCEL "programme-1" ["programme-2" ...]',
        tip: 'CANCEL libère la mémoire utilisée par un sous-programme chargé. Le prochain CALL rechargera le programme depuis le disque.',
        category: 'control',
        example: 'CANCEL "SOUS-PROGRAMME-1".'
    };
}

function explainSort(stmt, context) {
    const file = stmt.file || '?';

    return {
        title: '📊 SORT - Tri',
        description: `Trie les enregistrements de ${file}.`,
        syntax: 'SORT fichier-tri ON ASCENDING/DESCENDING KEY clé USING fichier-in GIVING fichier-out',
        tip: 'SORT trie un fichier selon une ou plusieurs clés. ASCENDING = croissant, DESCENDING = décroissant. Très efficace pour les gros volumes.',
        category: 'file',
        example: 'SORT FICHIER-TRI ON ASCENDING KEY TRI-NOM\n  USING FICHIER-ENTREE\n  GIVING FICHIER-SORTIE.'
    };
}

function explainMerge(stmt, context) {
    const file = stmt.file || '?';

    return {
        title: '🔀 MERGE - Fusion',
        description: `Fusionne des fichiers triés dans ${file}.`,
        syntax: 'MERGE fichier-tri ON KEY clé USING fichier-1 fichier-2 GIVING fichier-out',
        tip: 'MERGE fusionne plusieurs fichiers déjà triés en un seul. Les fichiers d\'entrée doivent être triés selon la même clé.',
        category: 'file',
        example: 'MERGE FICHIER-TRI ON ASCENDING KEY TRI-CLE\n  USING FICHIER-1 FICHIER-2\n  GIVING FICHIER-RESULTAT.'
    };
}

function explainRelease(stmt, context) {
    const record = stmt.record || '?';

    return {
        title: '📤 RELEASE - Libération',
        description: `Envoie l'enregistrement ${record} vers le processus de tri.`,
        syntax: 'RELEASE enregistrement [FROM variable]',
        tip: 'RELEASE est utilisé dans une INPUT PROCEDURE de SORT. Il envoie un enregistrement au tri avec possibilité de filtrage ou transformation.',
        category: 'file',
        example: 'RELEASE TRI-ENREG FROM WS-ENREG.'
    };
}

function explainReturn(stmt, context) {
    const file = stmt.file || '?';

    return {
        title: '📥 RETURN - Récupération',
        description: `Récupère un enregistrement trié de ${file}.`,
        syntax: 'RETURN fichier-tri [INTO variable] [AT END instructions] END-RETURN',
        tip: 'RETURN est utilisé dans une OUTPUT PROCEDURE de SORT. Il récupère les enregistrements triés un par un.',
        category: 'file',
        example: 'RETURN FICHIER-TRI INTO WS-ENREG\n  AT END SET FIN-TRI TO TRUE\nEND-RETURN.'
    };
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Format a value for display
 */
function formatValue(value) {
    if (!value) return '?';
    if (typeof value === 'string') return `"${value}"`;
    if (typeof value === 'number') return value.toString();
    if (value.name) return value.name;
    if (value.value !== undefined) {
        if (typeof value.value === 'string') return `"${value.value}"`;
        return value.value.toString();
    }
    return JSON.stringify(value);
}

/**
 * Get category information
 */
export function getCategoryInfo(category) {
    const categories = {
        io: { name: 'Entrées/Sorties', icon: '📺', color: '#4CAF50' },
        data: { name: 'Manipulation de données', icon: '📋', color: '#2196F3' },
        arithmetic: { name: 'Arithmétique', icon: '🔢', color: '#FF9800' },
        control: { name: 'Contrôle de flux', icon: '🔀', color: '#9C27B0' },
        file: { name: 'Fichiers', icon: '📁', color: '#795548' },
        string: { name: 'Chaînes de caractères', icon: '🔤', color: '#00BCD4' },
        table: { name: 'Tables', icon: '📊', color: '#E91E63' },
        general: { name: 'Général', icon: '⚙️', color: '#607D8B' }
    };
    return categories[category] || categories.general;
}

/**
 * Get a short summary for the statement
 */
export function getShortSummary(stmt) {
    const explanation = explainStatement(stmt);
    return explanation ? explanation.title : stmt.type;
}

export default {
    explainStatement,
    getCategoryInfo,
    getShortSummary
};
