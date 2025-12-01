/**
 * COBOL Compiler/Interpreter Module
 * Main entry point for the COBOL runtime
 */

import { Lexer, tokenize, TokenType } from './lexer.js';
import { Parser, parse, NodeType, ASTNode } from './parser.js';
import { Interpreter, interpret } from './interpreter.js';
import { DialectManager, CobolDialect, DialectFeatures } from './dialects.js';

/**
 * Built-in Copybook Library
 * Users can register their own copybooks here
 */
const CopybookLibrary = new Map();

/**
 * Preprocessor for COPY statements
 * Expands COPY statements before lexical analysis
 */
class CopyPreprocessor {
    constructor(copybooks = CopybookLibrary) {
        this.copybooks = copybooks;
        this.maxDepth = 10; // Prevent infinite recursion
    }

    /**
     * Process source and expand all COPY statements
     */
    process(source, depth = 0) {
        if (depth > this.maxDepth) {
            throw new Error('COPY imbriqués trop profonds (max 10 niveaux)');
        }

        // Match COPY copybook-name [REPLACING old BY new ...].
        const copyRegex = /^\s{6}\s+COPY\s+([A-Z0-9-]+)(?:\s+REPLACING\s+(.+?))?\s*\.\s*$/gmi;

        return source.replace(copyRegex, (match, copybookName, replacingClause) => {
            const normalizedName = copybookName.toUpperCase();
            const copybook = this.copybooks.get(normalizedName);

            if (!copybook) {
                // Copybook not found - leave as comment for error reporting
                return `      * COPY ${copybookName} - Copybook non trouvé`;
            }

            let expandedCode = copybook;

            // Handle REPLACING clause
            if (replacingClause) {
                const replacements = this.parseReplacingClause(replacingClause);
                for (const { from, to } of replacements) {
                    // COBOL uses == == for pseudo-text or plain identifiers
                    const fromPattern = from.replace(/==/g, '').trim();
                    const toText = to.replace(/==/g, '').trim();
                    expandedCode = expandedCode.split(fromPattern).join(toText);
                }
            }

            // Recursively process nested COPY statements
            expandedCode = this.process(expandedCode, depth + 1);

            return `      * >>> COPY ${copybookName}\n${expandedCode}\n      * <<< FIN COPY ${copybookName}`;
        });
    }

    /**
     * Parse REPLACING clause
     * Format: old-text BY new-text [old-text-2 BY new-text-2] ...
     */
    parseReplacingClause(clause) {
        const replacements = [];
        // Match patterns like: ==:PREFIX:== BY ==WS-== or FIELD-A BY FIELD-B
        const pattern = /(==.+?==|[A-Z0-9-]+)\s+BY\s+(==.+?==|[A-Z0-9-]+)/gi;
        let match;
        while ((match = pattern.exec(clause)) !== null) {
            replacements.push({ from: match[1], to: match[2] });
        }
        return replacements;
    }
}

/**
 * Register a copybook in the library
 */
export function registerCopybook(name, source) {
    CopybookLibrary.set(name.toUpperCase(), source);
}

/**
 * Get all registered copybooks
 */
export function getCopybooks() {
    return new Map(CopybookLibrary);
}

/**
 * Clear all copybooks
 */
export function clearCopybooks() {
    CopybookLibrary.clear();
}

// Register some useful default copybooks
registerCopybook('DATE-VARS', `
       01 WS-DATE-VARS.
          05 WS-CURRENT-DATE.
             10 WS-YEAR         PIC 9(4).
             10 WS-MONTH        PIC 9(2).
             10 WS-DAY          PIC 9(2).
          05 WS-CURRENT-TIME.
             10 WS-HOUR         PIC 9(2).
             10 WS-MINUTE       PIC 9(2).
             10 WS-SECOND       PIC 9(2).
          05 WS-DATE-FORMATTED  PIC X(10).
`);

registerCopybook('SCREEN-CONTROL', `
       01 WS-SCREEN-CONTROL.
          05 WS-SCREEN-LINE     PIC 9(2) VALUE 1.
          05 WS-SCREEN-COL      PIC 9(3) VALUE 1.
          05 WS-SCREEN-ATTR     PIC X(1) VALUE SPACE.
             88 ATTR-NORMAL     VALUE SPACE.
             88 ATTR-HIGHLIGHT  VALUE "H".
             88 ATTR-BLINK      VALUE "B".
             88 ATTR-REVERSE    VALUE "R".
`);

registerCopybook('ERROR-HANDLING', `
       01 WS-ERROR-HANDLING.
          05 WS-ERROR-CODE      PIC 9(4) VALUE 0.
          05 WS-ERROR-MSG       PIC X(80) VALUE SPACES.
          05 WS-ERROR-FLAG      PIC 9(1) VALUE 0.
             88 NO-ERROR        VALUE 0.
             88 HAS-ERROR       VALUE 1.
`);

registerCopybook('FILE-STATUS', `
       01 WS-FILE-STATUS.
          05 WS-FS-CODE         PIC X(2) VALUE "00".
             88 FS-SUCCESS      VALUE "00".
             88 FS-EOF          VALUE "10".
             88 FS-KEY-NOT-FOUND VALUE "23".
             88 FS-DUPLICATE-KEY VALUE "22".
             88 FS-FILE-NOT-FOUND VALUE "35".
          05 WS-FS-MSG          PIC X(40) VALUE SPACES.
`);

/**
 * COBOL Runtime - High-level API for compiling and executing COBOL programs
 */
export class CobolRuntime {
    constructor(options = {}) {
        this.options = options;
        this.callbacks = {
            onDisplay: options.onDisplay || console.log,
            onDisplayWithOptions: options.onDisplayWithOptions || null, // Screen control display
            onAccept: options.onAccept || (() => {}),
            onAcceptWithOptions: options.onAcceptWithOptions || null, // Screen control accept
            onError: options.onError || console.error,
            onStatus: options.onStatus || (() => {}),
            onStep: options.onStep || null, // Debug step callback
            onDialectWarning: options.onDialectWarning || null, // Dialect warning callback
            onDiskIO: options.onDiskIO || null, // Disk I/O visualization callback
        };
        this.dataManager = options.dataManager || {};
        this.interpreter = null;
        this.ast = null;
        this.tokens = null;
        this.errors = [];
        this.dialectWarnings = [];

        // Dialect configuration
        const dialect = options.dialect || CobolDialect.COBOL_85;
        this.dialectManager = new DialectManager(dialect);
        this.dialectManager.setStrictMode(options.strictDialect || false);
    }

    /**
     * Set the COBOL dialect
     * @param {string} dialect - One of CobolDialect values
     */
    setDialect(dialect) {
        this.dialectManager.setDialect(dialect);
    }

    /**
     * Enable/disable strict dialect mode
     * @param {boolean} strict - If true, unsupported features throw errors
     */
    setStrictMode(strict) {
        this.dialectManager.setStrictMode(strict);
    }

    /**
     * Get current dialect information
     */
    getDialectInfo() {
        return this.dialectManager.getDialectInfo();
    }

    /**
     * Get available dialects
     */
    static getAvailableDialects() {
        return DialectManager.getAvailableDialects();
    }

    /**
     * Compile COBOL source code
     * @param {string} source - COBOL source code
     * @returns {object} Compilation result with success status and any errors
     */
    compile(source) {
        this.errors = [];
        this.tokens = null;
        this.ast = null;
        this.dialectWarnings = [];
        this.dialectManager.clearWarnings();

        try {
            // Preprocess COPY statements
            this.callbacks.onStatus('Prétraitement COPY...');
            const preprocessor = new CopyPreprocessor();
            const processedSource = preprocessor.process(source);

            // Auto-detect dialect if set to AUTO
            if (this.dialectManager.dialect === CobolDialect.AUTO) {
                const detected = this.dialectManager.detectDialect(processedSource);
                this.callbacks.onStatus(`Dialecte détecté: ${detected}`);
            }

            const dialectInfo = this.dialectManager.getDialectInfo();
            this.callbacks.onStatus(`Compilation en ${dialectInfo.name}...`);

            // Lexical analysis
            this.callbacks.onStatus('Analyse lexicale...');
            const lexer = new Lexer(processedSource, this.dialectManager);
            this.tokens = lexer.tokenize();

            // Filter out comments for display
            const tokenCount = this.tokens.filter(t => t.type !== TokenType.COMMENT && t.type !== TokenType.EOF).length;
            this.callbacks.onStatus(`${tokenCount} tokens identifiés`);

            // Parsing
            this.callbacks.onStatus('Analyse syntaxique...');
            const parser = new Parser(this.tokens, this.dialectManager);
            this.ast = parser.parse();

            if (parser.errors.length > 0) {
                this.errors.push(...parser.errors);
                return {
                    success: false,
                    errors: this.errors,
                    tokens: this.tokens,
                    ast: this.ast,
                };
            }

            // Validate required elements
            if (!this.ast.identification) {
                this.errors.push('IDENTIFICATION DIVISION manquante');
            }
            if (!this.ast.procedure) {
                this.errors.push('PROCEDURE DIVISION manquante');
            }

            if (this.errors.length > 0) {
                return {
                    success: false,
                    errors: this.errors,
                    tokens: this.tokens,
                    ast: this.ast,
                    dialect: this.dialectManager.getDialectInfo(),
                };
            }

            // Collect dialect warnings
            this.dialectWarnings = this.dialectManager.getWarnings();
            if (this.dialectWarnings.length > 0) {
                // Notify about warnings
                for (const warning of this.dialectWarnings) {
                    if (this.callbacks.onDialectWarning) {
                        this.callbacks.onDialectWarning(warning);
                    }
                }
            }

            this.callbacks.onStatus('Compilation réussie');
            return {
                success: true,
                errors: [],
                warnings: this.dialectWarnings,
                tokens: this.tokens,
                ast: this.ast,
                programId: this.ast.identification?.programId || 'UNKNOWN',
                dialect: this.dialectManager.getDialectInfo(),
            };

        } catch (error) {
            this.errors.push(error.message);
            this.callbacks.onError(error.message);
            return {
                success: false,
                errors: this.errors,
                tokens: this.tokens,
                ast: this.ast,
                dialect: this.dialectManager.getDialectInfo(),
            };
        }
    }

    /**
     * Execute the compiled program
     * @param {object} options - Execution options
     * @param {boolean} options.stepMode - Enable step-by-step execution
     * @param {Function} options.onStep - Callback for each step in debug mode
     * @param {Function} options.onInterpreterReady - Callback when interpreter is ready
     * @returns {Promise<object>} Execution result
     */
    async run(options = {}) {
        if (!this.ast) {
            return {
                success: false,
                error: 'Programme non compilé',
            };
        }

        try {
            this.callbacks.onStatus('Exécution...');

            this.interpreter = new Interpreter(this.ast, {
                callbacks: this.callbacks,
                dataManager: this.dataManager,
            });

            // Enable step mode if requested
            if (options.stepMode) {
                this.interpreter.setStepMode(true);
                if (options.onStep) {
                    this.interpreter.onStep = options.onStep;
                }
            }

            // Notify that interpreter is ready (for debug mode)
            if (options.onInterpreterReady) {
                options.onInterpreterReady(this.interpreter);
            }

            await this.interpreter.run();

            this.callbacks.onStatus('Exécution terminée');
            return {
                success: true,
            };

        } catch (error) {
            if (error.message === 'STOP RUN') {
                this.callbacks.onStatus('Programme terminé (STOP RUN)');
                return { success: true };
            }

            this.callbacks.onError(`Erreur d'exécution: ${error.message}`);
            return {
                success: false,
                error: error.message,
            };
        }
    }

    /**
     * Compile and run in one step
     * @param {string} source - COBOL source code
     * @returns {Promise<object>} Execution result
     */
    async compileAndRun(source) {
        const compileResult = this.compile(source);

        if (!compileResult.success) {
            return compileResult;
        }

        return await this.run();
    }

    /**
     * Provide input when the program is waiting
     * @param {string} value - Input value
     */
    provideInput(value) {
        if (this.interpreter) {
            this.interpreter.provideInput(value);
        }
    }

    /**
     * Check if the program is waiting for input
     * @returns {boolean}
     */
    isWaitingForInput() {
        return this.interpreter?.isWaitingForInput() || false;
    }

    /**
     * Get the current AST (for debugging)
     * @returns {ASTNode}
     */
    getAST() {
        return this.ast;
    }

    /**
     * Get tokens (for debugging)
     * @returns {Token[]}
     */
    getTokens() {
        return this.tokens;
    }

    /**
     * Get the interpreter instance (for debug mode)
     * @returns {Interpreter}
     */
    getInterpreter() {
        return this.interpreter;
    }

    /**
     * Format AST for display
     * @returns {string}
     */
    formatAST() {
        if (!this.ast) return 'No AST';
        return JSON.stringify(this.ast, null, 2);
    }
}

// Export all components
export { Lexer, tokenize, TokenType };
export { Parser, parse, NodeType, ASTNode };
export { Interpreter, interpret };
export { DialectManager, CobolDialect, DialectFeatures };

// Default export
export default CobolRuntime;
