/**
 * COBOL Parser
 * Builds an Abstract Syntax Tree from tokens
 */

import { TokenType } from './lexer.js';
import { DialectManager, CobolDialect } from './dialects.js';

/**
 * AST Node Types
 */
export const NodeType = {
    PROGRAM: 'PROGRAM',
    IDENTIFICATION_DIVISION: 'IDENTIFICATION_DIVISION',
    ENVIRONMENT_DIVISION: 'ENVIRONMENT_DIVISION',
    DATA_DIVISION: 'DATA_DIVISION',
    PROCEDURE_DIVISION: 'PROCEDURE_DIVISION',

    // Data items
    DATA_ITEM: 'DATA_ITEM',
    CONDITION_NAME: 'CONDITION_NAME',  // 88 level condition names
    FILE_DESCRIPTION: 'FILE_DESCRIPTION',
    FILE_CONTROL: 'FILE_CONTROL',
    SELECT_STATEMENT: 'SELECT_STATEMENT',

    // Procedure division
    SECTION: 'SECTION',
    PARAGRAPH: 'PARAGRAPH',
    SENTENCE: 'SENTENCE',

    // Statements
    DISPLAY: 'DISPLAY',
    ACCEPT: 'ACCEPT',
    MOVE: 'MOVE',
    ADD: 'ADD',
    SUBTRACT: 'SUBTRACT',
    MULTIPLY: 'MULTIPLY',
    DIVIDE: 'DIVIDE',
    COMPUTE: 'COMPUTE',
    IF: 'IF',
    EVALUATE: 'EVALUATE',
    PERFORM: 'PERFORM',
    GO_TO: 'GO_TO',
    STOP_RUN: 'STOP_RUN',
    EXIT: 'EXIT',
    INITIALIZE: 'INITIALIZE',
    OPEN: 'OPEN',
    CLOSE: 'CLOSE',
    READ: 'READ',
    WRITE: 'WRITE',
    REWRITE: 'REWRITE',
    DELETE: 'DELETE',
    START: 'START',
    SORT: 'SORT',
    MERGE: 'MERGE',
    RELEASE: 'RELEASE',
    RETURN: 'RETURN',

    // Expressions
    IDENTIFIER: 'IDENTIFIER',
    LITERAL: 'LITERAL',
    BINARY_EXPR: 'BINARY_EXPR',
    UNARY_EXPR: 'UNARY_EXPR',
    CONDITION: 'CONDITION',
    FIGURATIVE_CONSTANT: 'FIGURATIVE_CONSTANT',
};

/**
 * AST Node class
 */
export class ASTNode {
    constructor(type, props = {}) {
        this.type = type;
        Object.assign(this, props);
    }
}

/**
 * Parser class
 */
export class Parser {
    constructor(tokens, dialectManager = null) {
        this.tokens = tokens.filter(t => t.type !== TokenType.COMMENT);
        this.pos = 0;
        this.errors = [];
        this.dialectManager = dialectManager || new DialectManager(CobolDialect.COBOL_85);
    }

    /**
     * Set dialect manager
     */
    setDialectManager(dialectManager) {
        this.dialectManager = dialectManager;
    }

    /**
     * Check if a feature is available in current dialect
     */
    checkFeature(featureName, context = '') {
        return this.dialectManager.checkFeature(featureName, context);
    }

    /**
     * Get dialect warnings
     */
    getDialectWarnings() {
        return this.dialectManager.getWarnings();
    }

    /**
     * Get current token
     */
    current() {
        return this.tokens[this.pos] || null;
    }

    /**
     * Peek ahead n tokens
     */
    peek(n = 1) {
        return this.tokens[this.pos + n] || null;
    }

    /**
     * Check if current token matches type
     */
    check(type) {
        return this.current()?.type === type;
    }

    /**
     * Check if current token matches any of types
     */
    checkAny(...types) {
        return types.includes(this.current()?.type);
    }

    /**
     * Advance and return current token
     */
    advance() {
        const token = this.current();
        this.pos++;
        return token;
    }

    /**
     * Expect and consume a token
     */
    expect(type, message) {
        if (this.check(type)) {
            return this.advance();
        }
        const current = this.current();
        const error = `${message || `Expected ${type}`} at line ${current?.line || '?'}, got ${current?.type || 'EOF'}`;
        this.errors.push(error);
        throw new Error(error);
    }

    /**
     * Optional token consumption
     */
    optional(type) {
        if (this.check(type)) {
            return this.advance();
        }
        return null;
    }

    /**
     * Skip optional period
     */
    skipPeriod() {
        this.optional(TokenType.DOT);
    }

    /**
     * Parse the entire program
     */
    parse() {
        const program = new ASTNode(NodeType.PROGRAM, {
            identification: null,
            environment: null,
            data: null,
            procedure: null,
        });

        try {
            // IDENTIFICATION DIVISION (required)
            if (this.check(TokenType.IDENTIFICATION)) {
                program.identification = this.parseIdentificationDivision();
            }

            // ENVIRONMENT DIVISION (optional)
            if (this.check(TokenType.ENVIRONMENT)) {
                program.environment = this.parseEnvironmentDivision();
            }

            // DATA DIVISION (optional)
            if (this.check(TokenType.DATA)) {
                program.data = this.parseDataDivision();
            }

            // PROCEDURE DIVISION (required for execution)
            if (this.check(TokenType.PROCEDURE)) {
                program.procedure = this.parseProcedureDivision();
            }
        } catch (e) {
            this.errors.push(e.message);
        }

        return program;
    }

    /**
     * Parse IDENTIFICATION DIVISION
     */
    parseIdentificationDivision() {
        this.expect(TokenType.IDENTIFICATION);
        this.expect(TokenType.DIVISION);
        this.skipPeriod();

        const node = new ASTNode(NodeType.IDENTIFICATION_DIVISION, {
            programId: null,
        });

        // PROGRAM-ID
        if (this.check(TokenType.PROGRAM_ID)) {
            this.advance();
            this.skipPeriod();
            if (this.check(TokenType.IDENTIFIER)) {
                node.programId = this.advance().value;
            }
            this.skipPeriod();
        }

        // Skip other identification entries
        while (!this.checkAny(TokenType.ENVIRONMENT, TokenType.DATA, TokenType.PROCEDURE, TokenType.EOF)) {
            this.advance();
        }

        return node;
    }

    /**
     * Parse ENVIRONMENT DIVISION
     */
    parseEnvironmentDivision() {
        this.expect(TokenType.ENVIRONMENT);
        this.expect(TokenType.DIVISION);
        this.skipPeriod();

        const node = new ASTNode(NodeType.ENVIRONMENT_DIVISION, {
            sourceComputer: null,
            objectComputer: null,
            specialNames: null,
            fileControl: [],
        });

        // CONFIGURATION SECTION (optional)
        if (this.check(TokenType.CONFIGURATION)) {
            this.advance();
            this.expect(TokenType.SECTION);
            this.skipPeriod();

            // SOURCE-COMPUTER (optional)
            if (this.check(TokenType.SOURCE_COMPUTER)) {
                this.advance();
                this.skipPeriod();
                // Read computer name (everything until next keyword or period)
                let computerName = '';
                while (!this.check(TokenType.DOT) &&
                       !this.check(TokenType.OBJECT_COMPUTER) &&
                       !this.check(TokenType.SPECIAL_NAMES) &&
                       !this.check(TokenType.INPUT_OUTPUT) &&
                       !this.check(TokenType.DATA) &&
                       !this.check(TokenType.PROCEDURE) &&
                       !this.check(TokenType.EOF)) {
                    computerName += (computerName ? ' ' : '') + this.advance().value;
                }
                node.sourceComputer = computerName || null;
                this.skipPeriod();
            }

            // OBJECT-COMPUTER (optional)
            if (this.check(TokenType.OBJECT_COMPUTER)) {
                this.advance();
                this.skipPeriod();
                // Read computer name (everything until next keyword or period)
                let computerName = '';
                while (!this.check(TokenType.DOT) &&
                       !this.check(TokenType.SPECIAL_NAMES) &&
                       !this.check(TokenType.INPUT_OUTPUT) &&
                       !this.check(TokenType.DATA) &&
                       !this.check(TokenType.PROCEDURE) &&
                       !this.check(TokenType.EOF)) {
                    computerName += (computerName ? ' ' : '') + this.advance().value;
                }
                node.objectComputer = computerName || null;
                this.skipPeriod();
            }

            // SPECIAL-NAMES (optional) - just skip for now
            if (this.check(TokenType.SPECIAL_NAMES)) {
                this.advance();
                this.skipPeriod();
                // Skip until next section or division
                while (!this.check(TokenType.INPUT_OUTPUT) &&
                       !this.check(TokenType.DATA) &&
                       !this.check(TokenType.PROCEDURE) &&
                       !this.check(TokenType.EOF)) {
                    if (this.check(TokenType.DOT)) {
                        this.advance();
                        // Check if next token starts a new section
                        if (this.checkAny(TokenType.INPUT_OUTPUT, TokenType.DATA, TokenType.PROCEDURE)) {
                            break;
                        }
                    } else {
                        this.advance();
                    }
                }
            }
        }

        // INPUT-OUTPUT SECTION (optional)
        if (this.check(TokenType.INPUT_OUTPUT)) {
            this.advance();
            this.expect(TokenType.SECTION);
            this.skipPeriod();

            // FILE-CONTROL
            if (this.check(TokenType.FILE_CONTROL)) {
                this.advance();
                this.skipPeriod();

                // Parse SELECT statements
                while (this.check(TokenType.SELECT)) {
                    node.fileControl.push(this.parseSelectStatement());
                }
            }
        }

        // Skip to next division
        while (!this.checkAny(TokenType.DATA, TokenType.PROCEDURE, TokenType.EOF)) {
            this.advance();
        }

        return node;
    }

    /**
     * Parse SELECT statement
     */
    parseSelectStatement() {
        this.expect(TokenType.SELECT);
        const fileName = this.expect(TokenType.IDENTIFIER).value;

        const node = new ASTNode(NodeType.SELECT_STATEMENT, {
            fileName,
            assignTo: null,
            organization: 'SEQUENTIAL',
            accessMode: 'SEQUENTIAL',
            recordKey: null,
        });

        // ASSIGN TO
        if (this.check(TokenType.ASSIGN)) {
            this.advance();
            this.optional(TokenType.TO);
            if (this.check(TokenType.STRING_LITERAL)) {
                node.assignTo = this.advance().value;
            } else if (this.check(TokenType.IDENTIFIER)) {
                node.assignTo = this.advance().value;
            }
        }

        // ORGANIZATION
        while (!this.check(TokenType.DOT) && !this.check(TokenType.SELECT) && !this.check(TokenType.EOF)) {
            if (this.check(TokenType.ORGANIZATION)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.SEQUENTIAL, TokenType.INDEXED, TokenType.RELATIVE)) {
                    node.organization = this.advance().value;
                }
            } else if (this.check(TokenType.ACCESS)) {
                this.advance();
                this.optional(TokenType.MODE);
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.SEQUENTIAL, TokenType.RANDOM, TokenType.DYNAMIC)) {
                    node.accessMode = this.advance().value;
                }
            } else if (this.check(TokenType.RECORD)) {
                this.advance();
                this.optional(TokenType.KEY);
                this.optional(TokenType.IS);
                if (this.check(TokenType.IDENTIFIER)) {
                    node.recordKey = this.advance().value;
                }
            } else {
                this.advance();
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse DATA DIVISION
     */
    parseDataDivision() {
        this.expect(TokenType.DATA);
        this.expect(TokenType.DIVISION);
        this.skipPeriod();

        const node = new ASTNode(NodeType.DATA_DIVISION, {
            fileSection: [],
            workingStorage: [],
        });

        // FILE SECTION (can be FILE_SECTION token or FILE + SECTION tokens)
        if (this.check(TokenType.FILE_SECTION)) {
            this.advance();
            this.expect(TokenType.SECTION);
            this.skipPeriod();

            while (this.check(TokenType.FD)) {
                node.fileSection.push(this.parseFileDescription());
            }
        } else if (this.check(TokenType.FILE_CONTROL)) {
            // This is FILE-CONTROL, not FILE SECTION - skip for now
        } else if (this.check(TokenType.FILE) && this.peek()?.type === TokenType.SECTION) {
            // Handle "FILE SECTION" as two separate tokens
            this.advance(); // FILE
            this.advance(); // SECTION
            this.skipPeriod();

            while (this.check(TokenType.FD)) {
                node.fileSection.push(this.parseFileDescription());
            }
        }

        // WORKING-STORAGE SECTION
        if (this.check(TokenType.WORKING_STORAGE)) {
            this.advance();
            this.expect(TokenType.SECTION);
            this.skipPeriod();

            while (this.check(TokenType.LEVEL_NUMBER) &&
                   !this.checkAny(TokenType.PROCEDURE, TokenType.EOF)) {
                node.workingStorage.push(this.parseDataItem());
            }
        }

        return node;
    }

    /**
     * Parse File Description (FD)
     */
    parseFileDescription() {
        this.expect(TokenType.FD);
        const fileName = this.expect(TokenType.IDENTIFIER).value;

        const node = new ASTNode(NodeType.FILE_DESCRIPTION, {
            fileName,
            records: [],
        });

        // Skip FD clauses until period
        while (!this.check(TokenType.DOT) && !this.check(TokenType.EOF)) {
            this.advance();
        }
        this.skipPeriod();

        // Parse record descriptions with hierarchy
        while (this.check(TokenType.LEVEL_NUMBER)) {
            const item = this.parseDataItem();
            // Level 01 items are top-level records
            if (item.level === 1 || item.level === '1' || item.level === '01') {
                node.records.push(item);
            } else {
                // Sub-level items belong to the last 01 record
                const lastRecord = node.records[node.records.length - 1];
                if (lastRecord) {
                    lastRecord.children.push(item);
                }
            }
        }

        return node;
    }

    /**
     * Parse data item (variable declaration)
     */
    parseDataItem() {
        const level = this.expect(TokenType.LEVEL_NUMBER).value;

        // Level 88 = Condition name (special handling)
        if (level === 88) {
            return this.parseConditionName();
        }

        const node = new ASTNode(NodeType.DATA_ITEM, {
            level,
            name: null,
            pic: null,
            value: null,
            redefines: null,
            children: [],
            conditionNames: [],  // 88 level condition names attached to this item
        });

        // Name or FILLER
        if (this.check(TokenType.IDENTIFIER)) {
            node.name = this.advance().value;
        } else if (this.check(TokenType.FILLER)) {
            this.advance();
            node.name = 'FILLER';
        }

        // Clauses - stop at DOT, EOF, or next division/level number
        while (!this.check(TokenType.DOT) && !this.check(TokenType.EOF) &&
               !this.check(TokenType.LEVEL_NUMBER) && !this.check(TokenType.PROCEDURE)) {
            if (this.checkAny(TokenType.PIC, TokenType.PICTURE)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.check(TokenType.PIC_STRING)) {
                    node.pic = this.advance().value;
                }
            } else if (this.check(TokenType.VALUE)) {
                this.advance();
                this.optional(TokenType.IS);
                node.value = this.parseValue();
            } else if (this.check(TokenType.REDEFINES)) {
                this.advance();
                if (this.check(TokenType.IDENTIFIER)) {
                    node.redefines = this.advance().value;
                }
            } else {
                this.advance();
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse 88 level condition name
     * Syntax: 88 condition-name VALUE[S] [IS|ARE] value-1 [THRU value-2] [value-3 [THRU value-4]]...
     */
    parseConditionName() {
        const node = new ASTNode(NodeType.CONDITION_NAME, {
            level: 88,
            name: null,
            values: [],  // Array of {value, thru} objects
        });

        // Condition name
        if (this.check(TokenType.IDENTIFIER)) {
            node.name = this.advance().value;
        }

        // VALUE or VALUES keyword
        if (this.checkAny(TokenType.VALUE, TokenType.VALUES)) {
            this.advance();
            this.optional(TokenType.IS);
            this.optional(TokenType.ARE);

            // Parse value list
            while (!this.check(TokenType.DOT) && !this.check(TokenType.EOF) &&
                   !this.check(TokenType.LEVEL_NUMBER) && !this.check(TokenType.PROCEDURE)) {
                const valueEntry = { value: null, thru: null };

                // Parse value
                valueEntry.value = this.parseConditionValue();

                // Check for THRU/THROUGH
                if (this.checkAny(TokenType.THRU, TokenType.THROUGH)) {
                    this.advance();
                    valueEntry.thru = this.parseConditionValue();
                }

                node.values.push(valueEntry);

                // Skip comma between values (if any)
                this.optional(TokenType.COMMA);

                // Check if next token could be another value
                if (!this.checkAny(TokenType.STRING_LITERAL, TokenType.NUMBER, TokenType.IDENTIFIER,
                                   TokenType.SPACES, TokenType.SPACE, TokenType.ZEROS, TokenType.ZEROES,
                                   TokenType.ZERO, TokenType.TRUE, TokenType.FALSE)) {
                    break;
                }
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse a value for condition name (88 level)
     */
    parseConditionValue() {
        if (this.check(TokenType.STRING_LITERAL)) {
            return { type: 'string', value: this.advance().value };
        }
        if (this.check(TokenType.NUMBER)) {
            return { type: 'number', value: this.advance().value };
        }
        if (this.checkAny(TokenType.SPACES, TokenType.SPACE)) {
            this.advance();
            return { type: 'figurative', value: 'SPACES' };
        }
        if (this.checkAny(TokenType.ZEROS, TokenType.ZEROES, TokenType.ZERO)) {
            this.advance();
            return { type: 'figurative', value: 'ZEROS' };
        }
        if (this.check(TokenType.TRUE)) {
            this.advance();
            return { type: 'boolean', value: true };
        }
        if (this.check(TokenType.FALSE)) {
            this.advance();
            return { type: 'boolean', value: false };
        }
        // Default: treat as identifier (for named constants)
        if (this.check(TokenType.IDENTIFIER)) {
            return { type: 'identifier', value: this.advance().value };
        }
        return null;
    }

    /**
     * Parse a value (literal or figurative constant)
     */
    parseValue() {
        if (this.check(TokenType.STRING_LITERAL)) {
            return { type: 'string', value: this.advance().value };
        }
        if (this.check(TokenType.NUMBER)) {
            return { type: 'number', value: this.advance().value };
        }
        if (this.checkAny(TokenType.SPACES, TokenType.SPACE)) {
            this.advance();
            return { type: 'figurative', value: 'SPACES' };
        }
        if (this.checkAny(TokenType.ZEROS, TokenType.ZEROES, TokenType.ZERO)) {
            this.advance();
            return { type: 'figurative', value: 'ZEROS' };
        }
        if (this.checkAny(TokenType.LOW_VALUES)) {
            this.advance();
            return { type: 'figurative', value: 'LOW-VALUES' };
        }
        if (this.checkAny(TokenType.HIGH_VALUES)) {
            this.advance();
            return { type: 'figurative', value: 'HIGH-VALUES' };
        }
        if (this.checkAny(TokenType.QUOTES, TokenType.QUOTE)) {
            this.advance();
            return { type: 'figurative', value: 'QUOTES' };
        }

        return null;
    }

    /**
     * Parse PROCEDURE DIVISION
     */
    parseProcedureDivision() {
        this.expect(TokenType.PROCEDURE);
        this.expect(TokenType.DIVISION);
        this.skipPeriod();

        const node = new ASTNode(NodeType.PROCEDURE_DIVISION, {
            sections: [],
            paragraphs: [],
            statements: [],
        });

        // Parse sections, paragraphs, and statements
        while (!this.check(TokenType.EOF)) {
            // Check for section
            if (this.check(TokenType.IDENTIFIER) && this.peek()?.type === TokenType.SECTION) {
                node.sections.push(this.parseSection());
            }
            // Check for paragraph
            else if (this.check(TokenType.IDENTIFIER) && this.peek()?.type === TokenType.DOT) {
                node.paragraphs.push(this.parseParagraph());
            }
            // Statement
            else if (this.isStatementStart()) {
                const stmt = this.parseStatement();
                if (stmt) node.statements.push(stmt);
            }
            else {
                this.advance();
            }
        }

        return node;
    }

    /**
     * Parse a section
     */
    parseSection() {
        const name = this.advance().value;
        this.expect(TokenType.SECTION);
        this.skipPeriod();

        const node = new ASTNode(NodeType.SECTION, {
            name,
            paragraphs: [],
            statements: [],
        });

        while (!this.check(TokenType.EOF) &&
               !(this.check(TokenType.IDENTIFIER) && this.peek()?.type === TokenType.SECTION)) {
            if (this.check(TokenType.IDENTIFIER) && this.peek()?.type === TokenType.DOT) {
                node.paragraphs.push(this.parseParagraph());
            } else if (this.isStatementStart()) {
                const stmt = this.parseStatement();
                if (stmt) node.statements.push(stmt);
            } else {
                this.advance();
            }
        }

        return node;
    }

    /**
     * Parse a paragraph
     */
    parseParagraph() {
        const name = this.advance().value;
        this.skipPeriod();

        const node = new ASTNode(NodeType.PARAGRAPH, {
            name,
            statements: [],
        });

        while (!this.check(TokenType.EOF) &&
               !(this.check(TokenType.IDENTIFIER) && this.peek()?.type === TokenType.DOT) &&
               !(this.check(TokenType.IDENTIFIER) && this.peek()?.type === TokenType.SECTION)) {
            if (this.isStatementStart()) {
                const stmt = this.parseStatement();
                if (stmt) node.statements.push(stmt);
            } else if (this.check(TokenType.DOT)) {
                this.advance();
            } else {
                break;
            }
        }

        return node;
    }

    /**
     * Check if current token starts a statement
     */
    isStatementStart() {
        return this.checkAny(
            TokenType.DISPLAY, TokenType.ACCEPT, TokenType.MOVE,
            TokenType.ADD, TokenType.SUBTRACT, TokenType.MULTIPLY, TokenType.DIVIDE,
            TokenType.COMPUTE, TokenType.IF, TokenType.EVALUATE, TokenType.PERFORM,
            TokenType.GO, TokenType.STOP, TokenType.EXIT, TokenType.INITIALIZE,
            TokenType.OPEN, TokenType.CLOSE, TokenType.READ, TokenType.WRITE,
            TokenType.REWRITE, TokenType.DELETE, TokenType.START, TokenType.CONTINUE,
            TokenType.SORT, TokenType.MERGE, TokenType.RELEASE, TokenType.RETURN
        );
    }

    /**
     * Parse a statement
     */
    parseStatement() {
        switch (this.current()?.type) {
            case TokenType.DISPLAY: return this.parseDisplay();
            case TokenType.ACCEPT: return this.parseAccept();
            case TokenType.MOVE: return this.parseMove();
            case TokenType.ADD: return this.parseAdd();
            case TokenType.SUBTRACT: return this.parseSubtract();
            case TokenType.MULTIPLY: return this.parseMultiply();
            case TokenType.DIVIDE: return this.parseDivide();
            case TokenType.COMPUTE: return this.parseCompute();
            case TokenType.IF: return this.parseIf();
            case TokenType.EVALUATE: return this.parseEvaluate();
            case TokenType.PERFORM: return this.parsePerform();
            case TokenType.GO: return this.parseGoTo();
            case TokenType.STOP: return this.parseStopRun();
            case TokenType.EXIT: return this.parseExit();
            case TokenType.INITIALIZE: return this.parseInitialize();
            case TokenType.OPEN: return this.parseOpen();
            case TokenType.CLOSE: return this.parseClose();
            case TokenType.READ: return this.parseRead();
            case TokenType.WRITE: return this.parseWrite();
            case TokenType.REWRITE: return this.parseRewrite();
            case TokenType.DELETE: return this.parseDelete();
            case TokenType.START: return this.parseStart();
            case TokenType.SORT: return this.parseSort();
            case TokenType.MERGE: return this.parseMerge();
            case TokenType.RELEASE: return this.parseRelease();
            case TokenType.RETURN: return this.parseReturn();
            case TokenType.CONTINUE:
                this.advance();
                this.skipPeriod();
                return null;
            default:
                return null;
        }
    }

    /**
     * Parse DISPLAY statement with screen control extensions
     */
    parseDisplay() {
        this.expect(TokenType.DISPLAY);

        const node = new ASTNode(NodeType.DISPLAY, {
            items: [],
            noAdvancing: false,
            // Screen control extensions
            line: null,
            column: null,
            erase: null,        // 'EOS', 'EOL', or 'SCREEN'
            highlight: false,
            lowlight: false,
            blink: false,
            reverseVideo: false,
            underline: false,
            bell: false,
            foregroundColor: null,
            backgroundColor: null,
        });

        // Parse display items and screen control clauses
        while (!this.check(TokenType.DOT) && !this.check(TokenType.EOF) &&
               !this.isStatementStart() && !this.check(TokenType.END_IF) &&
               !this.check(TokenType.END_PERFORM) && !this.check(TokenType.ELSE)) {

            // NO ADVANCING
            if (this.check(TokenType.NO)) {
                this.advance();
                if (this.check(TokenType.ADVANCING)) {
                    this.advance();
                    node.noAdvancing = true;
                }
            }
            // WITH (optional prefix for attributes)
            else if (this.check(TokenType.WITH)) {
                this.advance();
            }
            // UPON device
            else if (this.check(TokenType.UPON)) {
                this.advance();
                this.advance(); // device name
            }
            // LINE number
            else if (this.check(TokenType.LINE)) {
                this.advance();
                this.optional(TokenType.NUMBER); // LINE NUMBER IS ...
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.line = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.line = this.advance().value;
                }
            }
            // COLUMN/COL/POSITION number
            else if (this.checkAny(TokenType.COLUMN, TokenType.COL, TokenType.POSITION)) {
                this.advance();
                this.optional(TokenType.NUMBER); // COLUMN NUMBER IS ...
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.column = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.column = this.advance().value;
                }
            }
            // ERASE EOS/EOL/SCREEN
            else if (this.check(TokenType.ERASE)) {
                this.advance();
                if (this.check(TokenType.EOS)) {
                    this.advance();
                    node.erase = 'EOS';
                } else if (this.check(TokenType.EOL)) {
                    this.advance();
                    node.erase = 'EOL';
                } else if (this.check(TokenType.SCREEN)) {
                    this.advance();
                    node.erase = 'SCREEN';
                } else {
                    node.erase = 'EOS'; // default
                }
            }
            // BLANK SCREEN/LINE
            else if (this.check(TokenType.BLANK)) {
                this.advance();
                if (this.check(TokenType.SCREEN)) {
                    this.advance();
                    node.erase = 'SCREEN';
                } else if (this.check(TokenType.LINE)) {
                    this.advance();
                    node.erase = 'EOL';
                }
            }
            // Display attributes
            else if (this.check(TokenType.HIGHLIGHT)) {
                this.advance();
                node.highlight = true;
            }
            else if (this.check(TokenType.LOWLIGHT)) {
                this.advance();
                node.lowlight = true;
            }
            else if (this.check(TokenType.BLINK)) {
                this.advance();
                node.blink = true;
            }
            else if (this.check(TokenType.REVERSE_VIDEO)) {
                this.advance();
                node.reverseVideo = true;
            }
            else if (this.check(TokenType.UNDERLINE)) {
                this.advance();
                node.underline = true;
            }
            else if (this.checkAny(TokenType.BELL, TokenType.BEEP)) {
                this.advance();
                node.bell = true;
            }
            // FOREGROUND-COLOR / BACKGROUND-COLOR
            else if (this.check(TokenType.FOREGROUND_COLOR)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.foregroundColor = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.foregroundColor = this.advance().value;
                }
            }
            else if (this.check(TokenType.BACKGROUND_COLOR)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.backgroundColor = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.backgroundColor = this.advance().value;
                }
            }
            // Display item (expression)
            else {
                const item = this.parseExpression();
                if (item) {
                    node.items.push(item);
                } else {
                    // Can't parse this token, break to avoid infinite loop
                    break;
                }
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse ACCEPT statement with screen control extensions
     */
    parseAccept() {
        this.expect(TokenType.ACCEPT);

        const node = new ASTNode(NodeType.ACCEPT, {
            target: null,
            from: null,
            // Screen control extensions
            line: null,
            column: null,
            highlight: false,
            lowlight: false,
            blink: false,
            reverseVideo: false,
            underline: false,
            bell: false,
            foregroundColor: null,
            backgroundColor: null,
            // Input control
            secure: false,      // Hide input (like password)
            required: false,    // Field must be filled
            full: false,        // Field must be completely filled
            auto: false,        // Auto-tab when filled
            cursor: null,       // Initial cursor position
            size: null,         // Input field size
        });

        // Target variable
        if (this.check(TokenType.IDENTIFIER)) {
            node.target = this.advance().value;
        }

        // Parse clauses until period or next statement
        while (!this.check(TokenType.DOT) && !this.check(TokenType.EOF) &&
               !this.isStatementStart() && !this.check(TokenType.END_IF) &&
               !this.check(TokenType.END_PERFORM) && !this.check(TokenType.ELSE)) {

            // FROM source
            if (this.check(TokenType.FROM)) {
                this.advance();
                if (this.check(TokenType.IDENTIFIER)) {
                    node.from = this.advance().value;
                }
            }
            // LINE number
            else if (this.check(TokenType.LINE)) {
                this.advance();
                this.optional(TokenType.NUMBER);
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.line = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.line = this.advance().value;
                }
            }
            // COLUMN/COL/POSITION number
            else if (this.checkAny(TokenType.COLUMN, TokenType.COL, TokenType.POSITION)) {
                this.advance();
                this.optional(TokenType.NUMBER);
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.column = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.column = this.advance().value;
                }
            }
            // WITH (optional prefix for attributes)
            else if (this.check(TokenType.WITH)) {
                this.advance();
            }
            // Display attributes
            else if (this.check(TokenType.HIGHLIGHT)) {
                this.advance();
                node.highlight = true;
            }
            else if (this.check(TokenType.LOWLIGHT)) {
                this.advance();
                node.lowlight = true;
            }
            else if (this.check(TokenType.BLINK)) {
                this.advance();
                node.blink = true;
            }
            else if (this.check(TokenType.REVERSE_VIDEO)) {
                this.advance();
                node.reverseVideo = true;
            }
            else if (this.check(TokenType.UNDERLINE)) {
                this.advance();
                node.underline = true;
            }
            else if (this.checkAny(TokenType.BELL, TokenType.BEEP)) {
                this.advance();
                node.bell = true;
            }
            // Input control attributes
            else if (this.check(TokenType.SECURE)) {
                this.advance();
                node.secure = true;
            }
            else if (this.check(TokenType.REQUIRED)) {
                this.advance();
                node.required = true;
            }
            else if (this.check(TokenType.FULL)) {
                this.advance();
                node.full = true;
            }
            else if (this.check(TokenType.AUTO)) {
                this.advance();
                node.auto = true;
            }
            // CURSOR position
            else if (this.check(TokenType.CURSOR)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.cursor = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.cursor = this.advance().value;
                }
            }
            // SIZE
            else if (this.check(TokenType.SIZE)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.size = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.size = this.advance().value;
                }
            }
            // FOREGROUND-COLOR / BACKGROUND-COLOR
            else if (this.check(TokenType.FOREGROUND_COLOR)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.foregroundColor = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.foregroundColor = this.advance().value;
                }
            }
            else if (this.check(TokenType.BACKGROUND_COLOR)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.checkAny(TokenType.NUMBER, TokenType.LEVEL_NUMBER)) {
                    node.backgroundColor = parseInt(this.advance().value);
                } else if (this.check(TokenType.IDENTIFIER)) {
                    node.backgroundColor = this.advance().value;
                }
            }
            else {
                // Unknown token, stop parsing clauses
                break;
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse MOVE statement
     */
    parseMove() {
        this.expect(TokenType.MOVE);

        const node = new ASTNode(NodeType.MOVE, {
            source: null,
            targets: [],
        });

        node.source = this.parseExpression();

        this.expect(TokenType.TO);

        while (this.check(TokenType.IDENTIFIER)) {
            node.targets.push(this.advance().value);
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse ADD statement
     */
    parseAdd() {
        this.expect(TokenType.ADD);

        const node = new ASTNode(NodeType.ADD, {
            operands: [],
            to: null,
            giving: null,
        });

        // Operands
        while (!this.checkAny(TokenType.TO, TokenType.GIVING, TokenType.DOT, TokenType.EOF)) {
            const expr = this.parseExpression();
            if (expr) {
                node.operands.push(expr);
            } else {
                break;
            }
        }

        if (this.check(TokenType.TO)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.to = this.advance().value;
            }
        }

        if (this.check(TokenType.GIVING)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.giving = this.advance().value;
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse SUBTRACT statement
     */
    parseSubtract() {
        this.expect(TokenType.SUBTRACT);

        const node = new ASTNode(NodeType.SUBTRACT, {
            operands: [],
            from: null,
            giving: null,
        });

        while (!this.checkAny(TokenType.FROM, TokenType.DOT, TokenType.EOF)) {
            const expr = this.parseExpression();
            if (expr) {
                node.operands.push(expr);
            } else {
                break;
            }
        }

        if (this.check(TokenType.FROM)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.from = this.advance().value;
            }
        }

        if (this.check(TokenType.GIVING)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.giving = this.advance().value;
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse MULTIPLY statement
     */
    parseMultiply() {
        this.expect(TokenType.MULTIPLY);

        const node = new ASTNode(NodeType.MULTIPLY, {
            operand1: null,
            operand2: null,
            giving: null,
        });

        node.operand1 = this.parseExpression();

        this.expect(TokenType.BY);
        node.operand2 = this.parseExpression();

        if (this.check(TokenType.GIVING)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.giving = this.advance().value;
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse DIVIDE statement
     */
    parseDivide() {
        this.expect(TokenType.DIVIDE);

        const node = new ASTNode(NodeType.DIVIDE, {
            dividend: null,
            divisor: null,
            giving: null,
            remainder: null,
        });

        node.dividend = this.parseExpression();

        if (this.check(TokenType.BY)) {
            this.advance();
            node.divisor = this.parseExpression();
        } else if (this.check(TokenType.INTO)) {
            this.advance();
            node.divisor = node.dividend;
            node.dividend = this.parseExpression();
        }

        if (this.check(TokenType.GIVING)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.giving = this.advance().value;
            }
        }

        if (this.check(TokenType.REMAINDER)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.remainder = this.advance().value;
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse COMPUTE statement
     */
    parseCompute() {
        this.expect(TokenType.COMPUTE);

        const node = new ASTNode(NodeType.COMPUTE, {
            target: null,
            expression: null,
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.target = this.advance().value;
        }

        this.expect(TokenType.EQUALS);

        node.expression = this.parseArithmeticExpression();

        this.skipPeriod();
        return node;
    }

    /**
     * Parse IF statement
     */
    parseIf() {
        this.expect(TokenType.IF);

        const node = new ASTNode(NodeType.IF, {
            condition: null,
            thenStatements: [],
            elseStatements: [],
        });

        node.condition = this.parseCondition();
        this.optional(TokenType.THEN);

        // Parse THEN statements
        while (!this.checkAny(TokenType.ELSE, TokenType.END_IF, TokenType.DOT, TokenType.EOF)) {
            if (this.isStatementStart()) {
                const stmt = this.parseStatement();
                if (stmt) node.thenStatements.push(stmt);
            } else {
                break;
            }
        }

        // Parse ELSE statements
        if (this.check(TokenType.ELSE)) {
            this.advance();
            while (!this.checkAny(TokenType.END_IF, TokenType.DOT, TokenType.EOF)) {
                if (this.isStatementStart()) {
                    const stmt = this.parseStatement();
                    if (stmt) node.elseStatements.push(stmt);
                } else {
                    break;
                }
            }
        }

        this.optional(TokenType.END_IF);
        this.skipPeriod();
        return node;
    }

    /**
     * Parse EVALUATE statement
     */
    parseEvaluate() {
        // Check dialect support
        this.checkFeature('evaluate', 'EVALUATE');
        this.expect(TokenType.EVALUATE);

        const node = new ASTNode(NodeType.EVALUATE, {
            subject: null,
            whenClauses: [],
            whenOther: [],
        });

        node.subject = this.parseExpression();

        while (this.check(TokenType.WHEN)) {
            this.advance();

            if (this.check(TokenType.OTHER)) {
                this.advance();
                while (!this.checkAny(TokenType.WHEN, TokenType.END_EVALUATE, TokenType.DOT, TokenType.EOF)) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) node.whenOther.push(stmt);
                    } else {
                        break;
                    }
                }
            } else {
                const whenClause = {
                    values: [],
                    statements: [],
                };

                // Parse WHEN values
                const value = this.parseExpression();
                if (value) whenClause.values.push(value);

                // Parse statements for this WHEN
                while (!this.checkAny(TokenType.WHEN, TokenType.END_EVALUATE, TokenType.DOT, TokenType.EOF)) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) whenClause.statements.push(stmt);
                    } else {
                        break;
                    }
                }

                node.whenClauses.push(whenClause);
            }
        }

        this.optional(TokenType.END_EVALUATE);
        this.skipPeriod();
        return node;
    }

    /**
     * Parse PERFORM statement
     */
    parsePerform() {
        this.expect(TokenType.PERFORM);

        const node = new ASTNode(NodeType.PERFORM, {
            target: null,
            thru: null,
            times: null,
            until: null,
            varying: null,
            inline: [],
        });

        // PERFORM n TIMES
        if (this.check(TokenType.NUMBER)) {
            node.times = this.advance().value;
            this.expect(TokenType.TIMES);
        }
        // PERFORM UNTIL condition
        else if (this.check(TokenType.UNTIL)) {
            this.advance();
            node.until = this.parseCondition();
        }
        // PERFORM VARYING
        else if (this.check(TokenType.VARYING)) {
            this.advance();
            node.varying = this.parseVaryingClause();
        }
        // PERFORM paragraph-name
        else if (this.check(TokenType.IDENTIFIER)) {
            node.target = this.advance().value;

            if (this.checkAny(TokenType.THRU, TokenType.THROUGH)) {
                this.advance();
                if (this.check(TokenType.IDENTIFIER)) {
                    node.thru = this.advance().value;
                }
            }

            // Check for TIMES, UNTIL, VARYING after target
            if (this.check(TokenType.NUMBER)) {
                node.times = this.advance().value;
                this.optional(TokenType.TIMES);
            } else if (this.check(TokenType.UNTIL)) {
                this.advance();
                node.until = this.parseCondition();
            } else if (this.check(TokenType.VARYING)) {
                this.advance();
                node.varying = this.parseVaryingClause();
            }
        }

        // Inline PERFORM (statements until END-PERFORM)
        if (!node.target && !this.check(TokenType.DOT)) {
            while (!this.checkAny(TokenType.END_PERFORM, TokenType.DOT, TokenType.EOF)) {
                if (this.isStatementStart()) {
                    const stmt = this.parseStatement();
                    if (stmt) node.inline.push(stmt);
                } else {
                    break;
                }
            }
            this.optional(TokenType.END_PERFORM);
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse VARYING clause
     */
    parseVaryingClause() {
        const varying = {
            variable: null,
            from: null,
            by: null,
            until: null,
        };

        if (this.check(TokenType.IDENTIFIER)) {
            varying.variable = this.advance().value;
        }

        if (this.check(TokenType.FROM)) {
            this.advance();
            varying.from = this.parseExpression();
        }

        if (this.check(TokenType.BY)) {
            this.advance();
            varying.by = this.parseExpression();
        }

        if (this.check(TokenType.UNTIL)) {
            this.advance();
            varying.until = this.parseCondition();
        }

        return varying;
    }

    /**
     * Parse a block of statements until a stop token is found
     * Used for INVALID KEY, NOT INVALID KEY, AT END, etc.
     */
    parseStatementBlock(stopTokens) {
        const statements = [];

        // Convert string token names to TokenType values
        const stopTypes = stopTokens.map(t => TokenType[t] || t);

        while (this.pos < this.tokens.length) {
            // Check if we hit a stop token
            const currentType = this.current()?.type;
            if (stopTypes.some(st => currentType === st)) {
                break;
            }

            // Check for END-* tokens or period
            if (currentType === TokenType.DOT || currentType === TokenType.EOF) {
                break;
            }

            // Parse statement if we can
            if (this.isStatementStart()) {
                const stmt = this.parseStatement();
                if (stmt) statements.push(stmt);
            } else {
                break;
            }
        }

        return statements;
    }

    /**
     * Parse GO TO statement
     */
    parseGoTo() {
        this.expect(TokenType.GO);
        this.optional(TokenType.TO);

        const node = new ASTNode(NodeType.GO_TO, {
            target: null,
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.target = this.advance().value;
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse STOP RUN statement
     */
    parseStopRun() {
        this.expect(TokenType.STOP);
        this.expect(TokenType.RUN);
        this.skipPeriod();
        return new ASTNode(NodeType.STOP_RUN);
    }

    /**
     * Parse EXIT statement
     */
    parseExit() {
        this.expect(TokenType.EXIT);
        this.skipPeriod();
        return new ASTNode(NodeType.EXIT);
    }

    /**
     * Parse INITIALIZE statement
     */
    parseInitialize() {
        // Check dialect support
        this.checkFeature('initialize', 'INITIALIZE');
        this.expect(TokenType.INITIALIZE);

        const node = new ASTNode(NodeType.INITIALIZE, {
            targets: [],
        });

        while (this.check(TokenType.IDENTIFIER)) {
            node.targets.push(this.advance().value);
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse OPEN statement
     */
    parseOpen() {
        this.expect(TokenType.OPEN);

        const node = new ASTNode(NodeType.OPEN, {
            files: [],
        });

        while (this.checkAny(TokenType.INPUT, TokenType.OUTPUT, TokenType.I_O, TokenType.EXTEND)) {
            const mode = this.advance().value;
            while (this.check(TokenType.IDENTIFIER)) {
                node.files.push({ name: this.advance().value, mode });
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse CLOSE statement
     */
    parseClose() {
        this.expect(TokenType.CLOSE);

        const node = new ASTNode(NodeType.CLOSE, {
            files: [],
        });

        while (this.check(TokenType.IDENTIFIER)) {
            node.files.push(this.advance().value);
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse READ statement
     */
    parseRead() {
        this.expect(TokenType.READ);

        const node = new ASTNode(NodeType.READ, {
            file: null,
            into: null,
            key: null,
            next: false,
            atEnd: [],
            notAtEnd: [],
            invalidKey: [],
            notInvalidKey: [],
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.file = this.advance().value;
        }

        // Check for NEXT keyword (READ file NEXT)
        if (this.check(TokenType.NEXT)) {
            this.advance();
            node.next = true;
        }

        while (!this.checkAny(TokenType.DOT, TokenType.END_READ, TokenType.EOF)) {
            if (this.check(TokenType.NEXT)) {
                // NEXT can also appear here
                this.advance();
                node.next = true;
            } else if (this.check(TokenType.INTO)) {
                this.advance();
                if (this.check(TokenType.IDENTIFIER)) {
                    node.into = this.advance().value;
                }
            } else if (this.check(TokenType.KEY)) {
                this.advance();
                this.optional(TokenType.IS);
                if (this.check(TokenType.IDENTIFIER)) {
                    node.key = this.advance().value;
                }
            } else if (this.check(TokenType.AT) || (this.check(TokenType.NOT) && this.peek()?.type === TokenType.AT)) {
                const isNot = this.check(TokenType.NOT);
                if (isNot) this.advance();
                this.optional(TokenType.AT);
                this.expect(TokenType.END);

                const statements = [];
                while (!this.checkAny(TokenType.NOT, TokenType.END_READ, TokenType.DOT, TokenType.EOF) &&
                       !(this.check(TokenType.AT) || (this.check(TokenType.NOT) && this.peek()?.type === TokenType.AT))) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) statements.push(stmt);
                    } else {
                        break;
                    }
                }

                if (isNot) {
                    node.notAtEnd = statements;
                } else {
                    node.atEnd = statements;
                }
            } else if (this.check(TokenType.NOT) && this.peek()?.type === TokenType.INVALID) {
                // NOT INVALID KEY
                this.advance(); // NOT
                this.advance(); // INVALID
                this.optional(TokenType.KEY);

                // Initialize notInvalidKey if not exists
                if (!node.notInvalidKey) node.notInvalidKey = [];

                while (!this.checkAny(TokenType.END_READ, TokenType.DOT, TokenType.EOF) &&
                       !this.check(TokenType.INVALID) &&
                       !(this.check(TokenType.NOT) && this.peek()?.type === TokenType.INVALID) &&
                       !(this.check(TokenType.AT) || (this.check(TokenType.NOT) && this.peek()?.type === TokenType.AT))) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) node.notInvalidKey.push(stmt);
                    } else {
                        break;
                    }
                }
            } else if (this.check(TokenType.INVALID)) {
                // INVALID KEY
                this.advance();
                this.optional(TokenType.KEY);

                while (!this.checkAny(TokenType.END_READ, TokenType.DOT, TokenType.EOF) &&
                       !(this.check(TokenType.NOT) && this.peek()?.type === TokenType.INVALID)) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) node.invalidKey.push(stmt);
                    } else {
                        break;
                    }
                }
            } else {
                this.advance();
            }
        }

        this.optional(TokenType.END_READ);
        this.skipPeriod();
        return node;
    }

    /**
     * Parse WRITE statement
     */
    parseWrite() {
        this.expect(TokenType.WRITE);

        const node = new ASTNode(NodeType.WRITE, {
            record: null,
            from: null,
            invalidKey: [],
            notInvalidKey: [],
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.record = this.advance().value;
        }

        while (!this.checkAny(TokenType.DOT, TokenType.END_WRITE, TokenType.EOF)) {
            if (this.check(TokenType.FROM)) {
                this.advance();
                if (this.check(TokenType.IDENTIFIER)) {
                    node.from = this.advance().value;
                }
            } else if (this.check(TokenType.NOT) && this.peek()?.type === TokenType.INVALID) {
                // NOT INVALID KEY
                this.advance(); // NOT
                this.advance(); // INVALID
                this.optional(TokenType.KEY);

                while (!this.checkAny(TokenType.END_WRITE, TokenType.DOT, TokenType.EOF) &&
                       !this.check(TokenType.INVALID) &&
                       !(this.check(TokenType.NOT) && this.peek()?.type === TokenType.INVALID)) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) node.notInvalidKey.push(stmt);
                    } else {
                        break;
                    }
                }
            } else if (this.check(TokenType.INVALID)) {
                // INVALID KEY
                this.advance();
                this.optional(TokenType.KEY);

                while (!this.checkAny(TokenType.END_WRITE, TokenType.DOT, TokenType.EOF) &&
                       !(this.check(TokenType.NOT) && this.peek()?.type === TokenType.INVALID)) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) node.invalidKey.push(stmt);
                    } else {
                        break;
                    }
                }
            } else {
                // WRITE without INVALID KEY clause - stop if we see a statement keyword
                // This handles "WRITE rec" followed by another statement
                break;
            }
        }

        this.optional(TokenType.END_WRITE);
        this.skipPeriod();
        return node;
    }

    /**
     * Parse REWRITE statement
     */
    parseRewrite() {
        this.expect(TokenType.REWRITE);

        const node = new ASTNode(NodeType.REWRITE, {
            record: null,
            from: null,
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.record = this.advance().value;
        }

        if (this.check(TokenType.FROM)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.from = this.advance().value;
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse DELETE statement
     */
    parseDelete() {
        this.expect(TokenType.DELETE);

        const node = new ASTNode(NodeType.DELETE, {
            file: null,
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.file = this.advance().value;
        }

        // Handle INVALID KEY / NOT INVALID KEY
        if (this.check(TokenType.INVALID)) {
            this.advance();
            if (this.check(TokenType.KEY)) this.advance();
            node.invalidKey = this.parseStatementBlock(['NOT', 'END_DELETE']);
        }
        if (this.check(TokenType.NOT)) {
            this.advance();
            if (this.check(TokenType.INVALID)) this.advance();
            if (this.check(TokenType.KEY)) this.advance();
            node.notInvalidKey = this.parseStatementBlock(['END_DELETE']);
        }
        if (this.check(TokenType.END_DELETE)) {
            this.advance();
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse START statement - position in indexed file
     */
    parseStart() {
        this.expect(TokenType.START);

        const node = new ASTNode(NodeType.START, {
            file: null,
            key: null,
            operator: '>=',
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.file = this.advance().value;
        }

        // KEY clause: KEY IS EQUAL TO, KEY >=, KEY >, etc.
        if (this.check(TokenType.KEY)) {
            this.advance();
            if (this.check(TokenType.IS)) this.advance();

            // Parse comparison operator
            if (this.check(TokenType.EQUAL)) {
                this.advance();
                if (this.check(TokenType.TO)) this.advance();
                node.operator = '=';
            } else if (this.check(TokenType.GREATER)) {
                this.advance();
                if (this.check(TokenType.THAN)) this.advance();
                if (this.check(TokenType.OR)) {
                    this.advance();
                    if (this.check(TokenType.EQUAL)) this.advance();
                    if (this.check(TokenType.TO)) this.advance();
                    node.operator = '>=';
                } else {
                    node.operator = '>';
                }
            } else if (this.check(TokenType.NOT)) {
                this.advance();
                if (this.check(TokenType.LESS)) this.advance();
                if (this.check(TokenType.THAN)) this.advance();
                node.operator = '>=';
            } else if (this.checkAny(TokenType.EQUALS, TokenType.EQUAL_SIGN)) {
                this.advance();
                node.operator = '=';
            } else if (this.check(TokenType.GREATER_EQUAL)) {
                this.advance();
                node.operator = '>=';
            } else if (this.check(TokenType.GREATER_THAN)) {
                this.advance();
                node.operator = '>';
            }

            // Key field name
            if (this.check(TokenType.IDENTIFIER)) {
                node.key = this.advance().value;
            }
        }

        // Handle INVALID KEY / NOT INVALID KEY
        if (this.check(TokenType.INVALID)) {
            this.advance();
            if (this.check(TokenType.KEY)) this.advance();
            node.invalidKey = this.parseStatementBlock(['NOT', 'END_START']);
        }
        if (this.check(TokenType.NOT)) {
            this.advance();
            if (this.check(TokenType.INVALID)) this.advance();
            if (this.check(TokenType.KEY)) this.advance();
            node.notInvalidKey = this.parseStatementBlock(['END_START']);
        }
        if (this.check(TokenType.END_START)) {
            this.advance();
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse SORT statement
     * SORT sort-file ON ASCENDING/DESCENDING KEY key-name
     *      USING input-file / INPUT PROCEDURE proc-name
     *      GIVING output-file / OUTPUT PROCEDURE proc-name
     */
    parseSort() {
        this.expect(TokenType.SORT);

        const node = new ASTNode(NodeType.SORT, {
            sortFile: null,
            keys: [],           // Array of {key, order: 'ASCENDING'|'DESCENDING'}
            using: [],          // Input files
            giving: [],         // Output files
            inputProcedure: null,
            outputProcedure: null,
        });

        // Sort file name
        if (this.check(TokenType.IDENTIFIER)) {
            node.sortFile = this.advance().value;
        }

        // Parse ON ASCENDING/DESCENDING KEY clauses
        while (this.check(TokenType.ON) || this.check(TokenType.ASCENDING) || this.check(TokenType.DESCENDING)) {
            this.optional(TokenType.ON);

            let order = 'ASCENDING';
            if (this.check(TokenType.ASCENDING)) {
                this.advance();
                order = 'ASCENDING';
            } else if (this.check(TokenType.DESCENDING)) {
                this.advance();
                order = 'DESCENDING';
            }

            this.optional(TokenType.KEY);

            // Parse key field(s)
            while (this.check(TokenType.IDENTIFIER)) {
                node.keys.push({
                    key: this.advance().value,
                    order: order
                });
                // Continue if more keys for same order
                if (!this.check(TokenType.IDENTIFIER)) break;
            }
        }

        // Parse USING or INPUT PROCEDURE
        if (this.check(TokenType.USING)) {
            this.advance();
            while (this.check(TokenType.IDENTIFIER)) {
                node.using.push(this.advance().value);
            }
        } else if (this.check(TokenType.INPUT_PROCEDURE)) {
            this.advance();
            this.optional(TokenType.IS);
            if (this.check(TokenType.IDENTIFIER)) {
                node.inputProcedure = this.advance().value;
            }
            if (this.checkAny(TokenType.THRU, TokenType.THROUGH)) {
                this.advance();
                if (this.check(TokenType.IDENTIFIER)) {
                    node.inputProcedureThru = this.advance().value;
                }
            }
        }

        // Parse GIVING or OUTPUT PROCEDURE
        if (this.check(TokenType.GIVING)) {
            this.advance();
            while (this.check(TokenType.IDENTIFIER)) {
                node.giving.push(this.advance().value);
            }
        } else if (this.check(TokenType.OUTPUT_PROCEDURE)) {
            this.advance();
            this.optional(TokenType.IS);
            if (this.check(TokenType.IDENTIFIER)) {
                node.outputProcedure = this.advance().value;
            }
            if (this.checkAny(TokenType.THRU, TokenType.THROUGH)) {
                this.advance();
                if (this.check(TokenType.IDENTIFIER)) {
                    node.outputProcedureThru = this.advance().value;
                }
            }
        }

        this.optional(TokenType.END_SORT);
        this.skipPeriod();
        return node;
    }

    /**
     * Parse MERGE statement
     * MERGE merge-file ON ASCENDING/DESCENDING KEY key-name
     *       USING file-1 file-2 ...
     *       GIVING output-file / OUTPUT PROCEDURE proc-name
     */
    parseMerge() {
        this.expect(TokenType.MERGE);

        const node = new ASTNode(NodeType.MERGE, {
            mergeFile: null,
            keys: [],
            using: [],
            giving: [],
            outputProcedure: null,
        });

        // Merge file name
        if (this.check(TokenType.IDENTIFIER)) {
            node.mergeFile = this.advance().value;
        }

        // Parse ON ASCENDING/DESCENDING KEY clauses
        while (this.check(TokenType.ON) || this.check(TokenType.ASCENDING) || this.check(TokenType.DESCENDING)) {
            this.optional(TokenType.ON);

            let order = 'ASCENDING';
            if (this.check(TokenType.ASCENDING)) {
                this.advance();
                order = 'ASCENDING';
            } else if (this.check(TokenType.DESCENDING)) {
                this.advance();
                order = 'DESCENDING';
            }

            this.optional(TokenType.KEY);

            while (this.check(TokenType.IDENTIFIER)) {
                node.keys.push({
                    key: this.advance().value,
                    order: order
                });
                if (!this.check(TokenType.IDENTIFIER)) break;
            }
        }

        // USING (required for MERGE - at least 2 files)
        if (this.check(TokenType.USING)) {
            this.advance();
            while (this.check(TokenType.IDENTIFIER)) {
                node.using.push(this.advance().value);
            }
        }

        // Parse GIVING or OUTPUT PROCEDURE
        if (this.check(TokenType.GIVING)) {
            this.advance();
            while (this.check(TokenType.IDENTIFIER)) {
                node.giving.push(this.advance().value);
            }
        } else if (this.check(TokenType.OUTPUT_PROCEDURE)) {
            this.advance();
            this.optional(TokenType.IS);
            if (this.check(TokenType.IDENTIFIER)) {
                node.outputProcedure = this.advance().value;
            }
            if (this.checkAny(TokenType.THRU, TokenType.THROUGH)) {
                this.advance();
                if (this.check(TokenType.IDENTIFIER)) {
                    node.outputProcedureThru = this.advance().value;
                }
            }
        }

        this.optional(TokenType.END_MERGE);
        this.skipPeriod();
        return node;
    }

    /**
     * Parse RELEASE statement (used in INPUT PROCEDURE of SORT)
     * RELEASE record-name [FROM identifier]
     */
    parseRelease() {
        this.expect(TokenType.RELEASE);

        const node = new ASTNode(NodeType.RELEASE, {
            record: null,
            from: null,
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.record = this.advance().value;
        }

        if (this.check(TokenType.FROM)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.from = this.advance().value;
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse RETURN statement (used in OUTPUT PROCEDURE of SORT/MERGE)
     * RETURN sort-file [INTO identifier] AT END statements [NOT AT END statements]
     */
    parseReturn() {
        this.expect(TokenType.RETURN);

        const node = new ASTNode(NodeType.RETURN, {
            file: null,
            into: null,
            atEnd: [],
            notAtEnd: [],
        });

        if (this.check(TokenType.IDENTIFIER)) {
            node.file = this.advance().value;
        }

        // INTO clause
        if (this.check(TokenType.INTO)) {
            this.advance();
            if (this.check(TokenType.IDENTIFIER)) {
                node.into = this.advance().value;
            }
        }

        // AT END / NOT AT END clauses
        while (!this.check(TokenType.DOT) && !this.check(TokenType.EOF)) {
            if (this.check(TokenType.NOT) && this.peek()?.type === TokenType.AT) {
                // NOT AT END
                this.advance(); // NOT
                this.advance(); // AT
                this.optional(TokenType.END);
                while (!this.check(TokenType.DOT) && !this.check(TokenType.EOF) &&
                       !this.check(TokenType.AT)) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) node.notAtEnd.push(stmt);
                    } else {
                        break;
                    }
                }
            } else if (this.check(TokenType.AT)) {
                // AT END
                this.advance();
                this.optional(TokenType.END);
                while (!this.check(TokenType.DOT) && !this.check(TokenType.EOF) &&
                       !(this.check(TokenType.NOT) && this.peek()?.type === TokenType.AT)) {
                    if (this.isStatementStart()) {
                        const stmt = this.parseStatement();
                        if (stmt) node.atEnd.push(stmt);
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        this.skipPeriod();
        return node;
    }

    /**
     * Parse condition (for IF, UNTIL, etc.)
     */
    parseCondition() {
        return this.parseOrCondition();
    }

    parseOrCondition() {
        let left = this.parseAndCondition();

        while (this.check(TokenType.OR)) {
            this.advance();
            const right = this.parseAndCondition();
            left = new ASTNode(NodeType.CONDITION, {
                operator: 'OR',
                left,
                right,
            });
        }

        return left;
    }

    parseAndCondition() {
        let left = this.parseComparisonCondition();

        while (this.check(TokenType.AND)) {
            this.advance();
            const right = this.parseComparisonCondition();
            left = new ASTNode(NodeType.CONDITION, {
                operator: 'AND',
                left,
                right,
            });
        }

        return left;
    }

    parseComparisonCondition() {
        const isNot = this.check(TokenType.NOT);
        if (isNot) this.advance();

        const left = this.parseExpression();
        let operator = null;
        let right = null;

        // Parse comparison operator
        if (this.check(TokenType.EQUALS)) {
            this.advance(); // consume '='
            operator = '=';
            right = this.parseExpression();
        } else if (this.check(TokenType.IS) && this.peek()?.type === TokenType.EQUAL) {
            this.advance(); // IS
            this.advance(); // EQUAL
            this.optional(TokenType.TO);
            operator = '=';
            right = this.parseExpression();
        } else if (this.check(TokenType.EQUAL)) {
            this.advance(); // EQUAL
            this.optional(TokenType.TO);
            operator = '=';
            right = this.parseExpression();
        } else if (this.check(TokenType.GREATER_THAN) || this.check(TokenType.GREATER)) {
            this.advance();
            this.optional(TokenType.THAN);
            if (this.check(TokenType.OR) && this.peek()?.type === TokenType.EQUAL) {
                this.advance();
                this.advance();
                this.optional(TokenType.TO);
                operator = '>=';
            } else {
                operator = '>';
            }
            right = this.parseExpression();
        } else if (this.check(TokenType.LESS_THAN) || this.check(TokenType.LESS)) {
            this.advance();
            this.optional(TokenType.THAN);
            if (this.check(TokenType.OR) && this.peek()?.type === TokenType.EQUAL) {
                this.advance();
                this.advance();
                this.optional(TokenType.TO);
                operator = '<=';
            } else {
                operator = '<';
            }
            right = this.parseExpression();
        } else if (this.check(TokenType.GREATER_EQUAL)) {
            this.advance();
            operator = '>=';
            right = this.parseExpression();
        } else if (this.check(TokenType.LESS_EQUAL)) {
            this.advance();
            operator = '<=';
            right = this.parseExpression();
        } else if (this.check(TokenType.NOT_EQUAL)) {
            this.advance();
            operator = '<>';
            right = this.parseExpression();
        } else if (this.check(TokenType.NUMERIC)) {
            this.advance();
            operator = 'NUMERIC';
        } else if (this.check(TokenType.ALPHABETIC)) {
            this.advance();
            operator = 'ALPHABETIC';
        }

        if (operator) {
            const condition = new ASTNode(NodeType.CONDITION, {
                operator: isNot ? 'NOT ' + operator : operator,
                left,
                right,
            });
            return condition;
        }

        return left;
    }

    /**
     * Parse expression
     */
    parseExpression() {
        if (this.check(TokenType.STRING_LITERAL)) {
            return new ASTNode(NodeType.LITERAL, { value: this.advance().value, dataType: 'string' });
        }
        if (this.check(TokenType.NUMBER)) {
            return new ASTNode(NodeType.LITERAL, { value: this.advance().value, dataType: 'number' });
        }
        if (this.check(TokenType.IDENTIFIER)) {
            return new ASTNode(NodeType.IDENTIFIER, { name: this.advance().value });
        }
        if (this.checkAny(TokenType.SPACES, TokenType.SPACE)) {
            this.advance();
            return new ASTNode(NodeType.FIGURATIVE_CONSTANT, { value: 'SPACES' });
        }
        if (this.checkAny(TokenType.ZEROS, TokenType.ZEROES, TokenType.ZERO)) {
            this.advance();
            return new ASTNode(NodeType.FIGURATIVE_CONSTANT, { value: 'ZEROS' });
        }
        if (this.check(TokenType.LOW_VALUES)) {
            this.advance();
            return new ASTNode(NodeType.FIGURATIVE_CONSTANT, { value: 'LOW-VALUES' });
        }
        if (this.check(TokenType.HIGH_VALUES)) {
            this.advance();
            return new ASTNode(NodeType.FIGURATIVE_CONSTANT, { value: 'HIGH-VALUES' });
        }
        if (this.check(TokenType.TRUE)) {
            this.advance();
            return new ASTNode(NodeType.LITERAL, { value: true, dataType: 'boolean' });
        }
        if (this.check(TokenType.FALSE)) {
            this.advance();
            return new ASTNode(NodeType.LITERAL, { value: false, dataType: 'boolean' });
        }

        return null;
    }

    /**
     * Parse arithmetic expression (for COMPUTE)
     */
    parseArithmeticExpression() {
        return this.parseAddSubtract();
    }

    parseAddSubtract() {
        let left = this.parseMultiplyDivide();

        while (this.checkAny(TokenType.PLUS, TokenType.MINUS)) {
            const operator = this.advance().value;
            const right = this.parseMultiplyDivide();
            left = new ASTNode(NodeType.BINARY_EXPR, { operator, left, right });
        }

        return left;
    }

    parseMultiplyDivide() {
        let left = this.parsePower();

        while (this.checkAny(TokenType.STAR, TokenType.SLASH)) {
            const operator = this.advance().value;
            const right = this.parsePower();
            left = new ASTNode(NodeType.BINARY_EXPR, { operator, left, right });
        }

        return left;
    }

    parsePower() {
        let left = this.parseUnary();

        while (this.check(TokenType.POWER)) {
            this.advance();
            const right = this.parseUnary();
            left = new ASTNode(NodeType.BINARY_EXPR, { operator: '**', left, right });
        }

        return left;
    }

    parseUnary() {
        if (this.checkAny(TokenType.PLUS, TokenType.MINUS)) {
            const operator = this.advance().value;
            const operand = this.parseUnary();
            return new ASTNode(NodeType.UNARY_EXPR, { operator, operand });
        }

        return this.parsePrimary();
    }

    parsePrimary() {
        if (this.check(TokenType.LPAREN)) {
            this.advance();
            const expr = this.parseArithmeticExpression();
            this.expect(TokenType.RPAREN);
            return expr;
        }

        return this.parseExpression();
    }
}

/**
 * Helper function to parse COBOL source
 * @param {Token[]} tokens - Array of tokens from lexer
 * @returns {ASTNode} The program AST
 */
export function parse(tokens) {
    const parser = new Parser(tokens);
    return parser.parse();
}
