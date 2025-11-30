/**
 * COBOL Compiler/Interpreter Module
 * Main entry point for the COBOL runtime
 */

import { Lexer, tokenize, TokenType } from './lexer.js';
import { Parser, parse, NodeType, ASTNode } from './parser.js';
import { Interpreter, interpret } from './interpreter.js';

/**
 * COBOL Runtime - High-level API for compiling and executing COBOL programs
 */
export class CobolRuntime {
    constructor(options = {}) {
        this.options = options;
        this.callbacks = {
            onDisplay: options.onDisplay || console.log,
            onAccept: options.onAccept || (() => {}),
            onError: options.onError || console.error,
            onStatus: options.onStatus || (() => {}),
            onStep: options.onStep || null, // Debug step callback
        };
        this.dataManager = options.dataManager || {};
        this.interpreter = null;
        this.ast = null;
        this.tokens = null;
        this.errors = [];
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

        try {
            // Lexical analysis
            this.callbacks.onStatus('Analyse lexicale...');
            const lexer = new Lexer(source);
            this.tokens = lexer.tokenize();

            // Filter out comments for display
            const tokenCount = this.tokens.filter(t => t.type !== TokenType.COMMENT && t.type !== TokenType.EOF).length;
            this.callbacks.onStatus(`${tokenCount} tokens identifiés`);

            // Parsing
            this.callbacks.onStatus('Analyse syntaxique...');
            const parser = new Parser(this.tokens);
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
                };
            }

            this.callbacks.onStatus('Compilation réussie');
            return {
                success: true,
                errors: [],
                tokens: this.tokens,
                ast: this.ast,
                programId: this.ast.identification?.programId || 'UNKNOWN',
            };

        } catch (error) {
            this.errors.push(error.message);
            this.callbacks.onError(error.message);
            return {
                success: false,
                errors: this.errors,
                tokens: this.tokens,
                ast: this.ast,
            };
        }
    }

    /**
     * Execute the compiled program
     * @returns {Promise<object>} Execution result
     */
    async run() {
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

// Default export
export default CobolRuntime;
