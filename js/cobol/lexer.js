/**
 * COBOL Lexer
 * Tokenizes COBOL source code into a stream of tokens
 */

import { DialectManager, CobolDialect } from './dialects.js';

// Token types
export const TokenType = {
    // Keywords - Divisions
    IDENTIFICATION: 'IDENTIFICATION',
    ENVIRONMENT: 'ENVIRONMENT',
    DATA: 'DATA',
    PROCEDURE: 'PROCEDURE',
    DIVISION: 'DIVISION',
    SECTION: 'SECTION',

    // Keywords - Sections
    WORKING_STORAGE: 'WORKING_STORAGE',
    FILE: 'FILE',
    FILE_SECTION: 'FILE_SECTION',
    CONFIGURATION: 'CONFIGURATION',
    INPUT_OUTPUT: 'INPUT_OUTPUT',
    FILE_CONTROL: 'FILE_CONTROL',
    SOURCE_COMPUTER: 'SOURCE_COMPUTER',
    OBJECT_COMPUTER: 'OBJECT_COMPUTER',
    SPECIAL_NAMES: 'SPECIAL_NAMES',

    // Keywords - Data
    PIC: 'PIC',
    PICTURE: 'PICTURE',
    VALUE: 'VALUE',
    FILLER: 'FILLER',
    REDEFINES: 'REDEFINES',
    FD: 'FD',
    SELECT: 'SELECT',
    ASSIGN: 'ASSIGN',
    ORGANIZATION: 'ORGANIZATION',
    ACCESS: 'ACCESS',
    RECORD: 'RECORD',
    KEY: 'KEY',
    SEQUENTIAL: 'SEQUENTIAL',
    INDEXED: 'INDEXED',
    RELATIVE: 'RELATIVE',
    DYNAMIC: 'DYNAMIC',
    RANDOM: 'RANDOM',
    MODE: 'MODE',

    // Keywords - Statements
    PROGRAM_ID: 'PROGRAM_ID',
    DISPLAY: 'DISPLAY',
    ACCEPT: 'ACCEPT',
    MOVE: 'MOVE',
    TO: 'TO',
    ADD: 'ADD',
    SUBTRACT: 'SUBTRACT',
    MULTIPLY: 'MULTIPLY',
    DIVIDE: 'DIVIDE',
    GIVING: 'GIVING',
    REMAINDER: 'REMAINDER',
    BY: 'BY',
    FROM: 'FROM',
    INTO: 'INTO',
    COMPUTE: 'COMPUTE',
    INITIALIZE: 'INITIALIZE',
    SET: 'SET',
    STRING: 'STRING',
    UNSTRING: 'UNSTRING',
    INSPECT: 'INSPECT',
    REPLACING: 'REPLACING',
    TALLYING: 'TALLYING',
    ALL: 'ALL',
    LEADING: 'LEADING',
    FIRST: 'FIRST',
    TRAILING: 'TRAILING',
    BEFORE: 'BEFORE',
    AFTER: 'AFTER',
    INITIAL: 'INITIAL',
    DELIMITED: 'DELIMITED',
    DELIMITER: 'DELIMITER',
    COUNT: 'COUNT',
    POINTER: 'POINTER',
    OVERFLOW: 'OVERFLOW',
    END_STRING: 'END_STRING',
    END_UNSTRING: 'END_UNSTRING',
    END_INSPECT: 'END_INSPECT',
    CONVERTING: 'CONVERTING',

    // Table handling
    OCCURS: 'OCCURS',
    DEPENDING: 'DEPENDING',
    INDEXED: 'INDEXED',
    ASCENDING: 'ASCENDING',
    DESCENDING: 'DESCENDING',
    SEARCH: 'SEARCH',
    END_SEARCH: 'END_SEARCH',
    AT_END: 'AT_END',
    UP: 'UP',
    DOWN: 'DOWN',

    // Keywords - Control Flow
    IF: 'IF',
    THEN: 'THEN',
    ELSE: 'ELSE',
    END_IF: 'END_IF',
    EVALUATE: 'EVALUATE',
    WHEN: 'WHEN',
    OTHER: 'OTHER',
    END_EVALUATE: 'END_EVALUATE',
    PERFORM: 'PERFORM',
    TIMES: 'TIMES',
    UNTIL: 'UNTIL',
    VARYING: 'VARYING',
    END_PERFORM: 'END_PERFORM',
    THRU: 'THRU',
    THROUGH: 'THROUGH',
    GO: 'GO',
    STOP: 'STOP',
    RUN: 'RUN',
    EXIT: 'EXIT',
    CONTINUE: 'CONTINUE',
    NEXT: 'NEXT',
    SENTENCE: 'SENTENCE',

    // Keywords - Subprograms
    CALL: 'CALL',
    CANCEL: 'CANCEL',
    USING: 'USING',
    BY_REFERENCE: 'BY_REFERENCE',
    BY_CONTENT: 'BY_CONTENT',
    BY_VALUE: 'BY_VALUE',
    END_CALL: 'END_CALL',
    ON_EXCEPTION: 'ON_EXCEPTION',
    NOT_ON_EXCEPTION: 'NOT_ON_EXCEPTION',

    // Keywords - Copybooks
    COPY: 'COPY',
    // REPLACING already defined in string manipulation

    // Keywords - File I/O
    OPEN: 'OPEN',
    CLOSE: 'CLOSE',
    READ: 'READ',
    WRITE: 'WRITE',
    REWRITE: 'REWRITE',
    DELETE: 'DELETE',
    START: 'START',
    INPUT: 'INPUT',
    OUTPUT: 'OUTPUT',
    I_O: 'I_O',
    EXTEND: 'EXTEND',
    AT: 'AT',
    END: 'END',
    NOT: 'NOT',
    INVALID: 'INVALID',
    END_READ: 'END_READ',
    END_WRITE: 'END_WRITE',
    END_DELETE: 'END_DELETE',
    END_START: 'END_START',

    // Keywords - Sort/Merge
    SORT: 'SORT',
    MERGE: 'MERGE',
    ASCENDING: 'ASCENDING',
    DESCENDING: 'DESCENDING',
    USING: 'USING',
    INPUT_PROCEDURE: 'INPUT_PROCEDURE',
    OUTPUT_PROCEDURE: 'OUTPUT_PROCEDURE',
    RELEASE: 'RELEASE',
    RETURN: 'RETURN',
    SD: 'SD',
    END_SORT: 'END_SORT',
    END_MERGE: 'END_MERGE',

    // Keywords - Conditions
    AND: 'AND',
    OR: 'OR',
    TRUE: 'TRUE',
    FALSE: 'FALSE',
    EQUAL: 'EQUAL',
    GREATER: 'GREATER',
    LESS: 'LESS',
    THAN: 'THAN',
    NUMERIC: 'NUMERIC',
    ALPHABETIC: 'ALPHABETIC',
    SPACES: 'SPACES',
    ZEROS: 'ZEROS',
    ZEROES: 'ZEROES',
    ZERO: 'ZERO',
    SPACE: 'SPACE',
    LOW_VALUES: 'LOW_VALUES',
    HIGH_VALUES: 'HIGH_VALUES',
    QUOTES: 'QUOTES',
    QUOTE: 'QUOTE',
    VALUES: 'VALUES',

    // Keywords - Other
    IS: 'IS',
    ARE: 'ARE',
    OF: 'OF',
    IN: 'IN',
    ON: 'ON',
    WITH: 'WITH',
    NO: 'NO',
    ADVANCING: 'ADVANCING',
    LINE: 'LINE',
    LINES: 'LINES',
    PAGE: 'PAGE',
    UPON: 'UPON',
    ALSO: 'ALSO',

    // Screen control extensions (IBM/MicroFocus)
    POSITION: 'POSITION',
    COL: 'COL',
    COLUMN: 'COLUMN',
    ERASE: 'ERASE',
    EOS: 'EOS',
    EOL: 'EOL',
    BLANK: 'BLANK',
    SCREEN: 'SCREEN',
    HIGHLIGHT: 'HIGHLIGHT',
    LOWLIGHT: 'LOWLIGHT',
    BLINK: 'BLINK',
    REVERSE_VIDEO: 'REVERSE_VIDEO',
    UNDERLINE: 'UNDERLINE',
    BELL: 'BELL',
    BEEP: 'BEEP',
    SIZE: 'SIZE',
    FOREGROUND_COLOR: 'FOREGROUND_COLOR',
    BACKGROUND_COLOR: 'BACKGROUND_COLOR',
    CONTROL: 'CONTROL',
    FULL: 'FULL',
    REQUIRED: 'REQUIRED',
    SECURE: 'SECURE',
    AUTO: 'AUTO',
    CURSOR: 'CURSOR',

    // Literals
    NUMBER: 'NUMBER',
    STRING_LITERAL: 'STRING_LITERAL',
    IDENTIFIER: 'IDENTIFIER',
    LEVEL_NUMBER: 'LEVEL_NUMBER',
    PIC_STRING: 'PIC_STRING',

    // Operators
    EQUALS: 'EQUALS',
    PLUS: 'PLUS',
    MINUS: 'MINUS',
    STAR: 'STAR',
    SLASH: 'SLASH',
    POWER: 'POWER',
    GREATER_THAN: 'GREATER_THAN',
    LESS_THAN: 'LESS_THAN',
    GREATER_EQUAL: 'GREATER_EQUAL',
    LESS_EQUAL: 'LESS_EQUAL',
    NOT_EQUAL: 'NOT_EQUAL',

    // Punctuation
    DOT: 'DOT',
    COMMA: 'COMMA',
    LPAREN: 'LPAREN',
    RPAREN: 'RPAREN',
    COLON: 'COLON',

    // Special
    NEWLINE: 'NEWLINE',
    EOF: 'EOF',
    COMMENT: 'COMMENT',
};

// Keywords map
const KEYWORDS = {
    'IDENTIFICATION': TokenType.IDENTIFICATION,
    'ENVIRONMENT': TokenType.ENVIRONMENT,
    'DATA': TokenType.DATA,
    'PROCEDURE': TokenType.PROCEDURE,
    'DIVISION': TokenType.DIVISION,
    'SECTION': TokenType.SECTION,
    'WORKING-STORAGE': TokenType.WORKING_STORAGE,
    'FILE': TokenType.FILE,
    'FILE-SECTION': TokenType.FILE_SECTION,
    'CONFIGURATION': TokenType.CONFIGURATION,
    'INPUT-OUTPUT': TokenType.INPUT_OUTPUT,
    'FILE-CONTROL': TokenType.FILE_CONTROL,
    'SOURCE-COMPUTER': TokenType.SOURCE_COMPUTER,
    'OBJECT-COMPUTER': TokenType.OBJECT_COMPUTER,
    'SPECIAL-NAMES': TokenType.SPECIAL_NAMES,
    'PIC': TokenType.PIC,
    'PICTURE': TokenType.PICTURE,
    'VALUE': TokenType.VALUE,
    'FILLER': TokenType.FILLER,
    'REDEFINES': TokenType.REDEFINES,
    'FD': TokenType.FD,
    'SELECT': TokenType.SELECT,
    'ASSIGN': TokenType.ASSIGN,
    'ORGANIZATION': TokenType.ORGANIZATION,
    'ACCESS': TokenType.ACCESS,
    'RECORD': TokenType.RECORD,
    'KEY': TokenType.KEY,
    'SEQUENTIAL': TokenType.SEQUENTIAL,
    'INDEXED': TokenType.INDEXED,
    'RELATIVE': TokenType.RELATIVE,
    'DYNAMIC': TokenType.DYNAMIC,
    'RANDOM': TokenType.RANDOM,
    'MODE': TokenType.MODE,
    'PROGRAM-ID': TokenType.PROGRAM_ID,
    'DISPLAY': TokenType.DISPLAY,
    'ACCEPT': TokenType.ACCEPT,
    'MOVE': TokenType.MOVE,
    'TO': TokenType.TO,
    'ADD': TokenType.ADD,
    'SUBTRACT': TokenType.SUBTRACT,
    'MULTIPLY': TokenType.MULTIPLY,
    'DIVIDE': TokenType.DIVIDE,
    'GIVING': TokenType.GIVING,
    'REMAINDER': TokenType.REMAINDER,
    'BY': TokenType.BY,
    'FROM': TokenType.FROM,
    'INTO': TokenType.INTO,
    'COMPUTE': TokenType.COMPUTE,
    'INITIALIZE': TokenType.INITIALIZE,
    'SET': TokenType.SET,
    'STRING': TokenType.STRING,
    'UNSTRING': TokenType.UNSTRING,
    'INSPECT': TokenType.INSPECT,
    'REPLACING': TokenType.REPLACING,
    'TALLYING': TokenType.TALLYING,
    'ALL': TokenType.ALL,
    'LEADING': TokenType.LEADING,
    'FIRST': TokenType.FIRST,
    'TRAILING': TokenType.TRAILING,
    'BEFORE': TokenType.BEFORE,
    'AFTER': TokenType.AFTER,
    'INITIAL': TokenType.INITIAL,
    'DELIMITED': TokenType.DELIMITED,
    'DELIMITER': TokenType.DELIMITER,
    'COUNT': TokenType.COUNT,
    'POINTER': TokenType.POINTER,
    'OVERFLOW': TokenType.OVERFLOW,
    'END-STRING': TokenType.END_STRING,
    'END-UNSTRING': TokenType.END_UNSTRING,
    'END-INSPECT': TokenType.END_INSPECT,
    'CONVERTING': TokenType.CONVERTING,
    'OCCURS': TokenType.OCCURS,
    'DEPENDING': TokenType.DEPENDING,
    'ASCENDING': TokenType.ASCENDING,
    'DESCENDING': TokenType.DESCENDING,
    'SEARCH': TokenType.SEARCH,
    'END-SEARCH': TokenType.END_SEARCH,
    'UP': TokenType.UP,
    'DOWN': TokenType.DOWN,
    'IF': TokenType.IF,
    'THEN': TokenType.THEN,
    'ELSE': TokenType.ELSE,
    'END-IF': TokenType.END_IF,
    'EVALUATE': TokenType.EVALUATE,
    'WHEN': TokenType.WHEN,
    'OTHER': TokenType.OTHER,
    'END-EVALUATE': TokenType.END_EVALUATE,
    'PERFORM': TokenType.PERFORM,
    'TIMES': TokenType.TIMES,
    'UNTIL': TokenType.UNTIL,
    'VARYING': TokenType.VARYING,
    'END-PERFORM': TokenType.END_PERFORM,
    'THRU': TokenType.THRU,
    'THROUGH': TokenType.THROUGH,
    'GO': TokenType.GO,
    'STOP': TokenType.STOP,
    'RUN': TokenType.RUN,
    'EXIT': TokenType.EXIT,
    'CONTINUE': TokenType.CONTINUE,
    'NEXT': TokenType.NEXT,
    'SENTENCE': TokenType.SENTENCE,
    'CALL': TokenType.CALL,
    'CANCEL': TokenType.CANCEL,
    'USING': TokenType.USING,
    'BY': TokenType.BY,
    'REFERENCE': TokenType.BY_REFERENCE,
    'CONTENT': TokenType.BY_CONTENT,
    'END-CALL': TokenType.END_CALL,
    'COPY': TokenType.COPY,
    'OPEN': TokenType.OPEN,
    'CLOSE': TokenType.CLOSE,
    'READ': TokenType.READ,
    'WRITE': TokenType.WRITE,
    'REWRITE': TokenType.REWRITE,
    'DELETE': TokenType.DELETE,
    'START': TokenType.START,
    'INPUT': TokenType.INPUT,
    'OUTPUT': TokenType.OUTPUT,
    'I-O': TokenType.I_O,
    'EXTEND': TokenType.EXTEND,
    'AT': TokenType.AT,
    'END': TokenType.END,
    'NOT': TokenType.NOT,
    'INVALID': TokenType.INVALID,
    'END-READ': TokenType.END_READ,
    'END-WRITE': TokenType.END_WRITE,
    'END-DELETE': TokenType.END_DELETE,
    'END-START': TokenType.END_START,
    'SORT': TokenType.SORT,
    'MERGE': TokenType.MERGE,
    'ASCENDING': TokenType.ASCENDING,
    'DESCENDING': TokenType.DESCENDING,
    'USING': TokenType.USING,
    'INPUT-PROCEDURE': TokenType.INPUT_PROCEDURE,
    'OUTPUT-PROCEDURE': TokenType.OUTPUT_PROCEDURE,
    'RELEASE': TokenType.RELEASE,
    'RETURN': TokenType.RETURN,
    'SD': TokenType.SD,
    'END-SORT': TokenType.END_SORT,
    'END-MERGE': TokenType.END_MERGE,
    'AND': TokenType.AND,
    'OR': TokenType.OR,
    'TRUE': TokenType.TRUE,
    'FALSE': TokenType.FALSE,
    'EQUAL': TokenType.EQUAL,
    'GREATER': TokenType.GREATER,
    'LESS': TokenType.LESS,
    'THAN': TokenType.THAN,
    'NUMERIC': TokenType.NUMERIC,
    'ALPHABETIC': TokenType.ALPHABETIC,
    'SPACES': TokenType.SPACES,
    'ZEROS': TokenType.ZEROS,
    'ZEROES': TokenType.ZEROES,
    'ZERO': TokenType.ZERO,
    'SPACE': TokenType.SPACE,
    'LOW-VALUES': TokenType.LOW_VALUES,
    'HIGH-VALUES': TokenType.HIGH_VALUES,
    'QUOTES': TokenType.QUOTES,
    'QUOTE': TokenType.QUOTE,
    'VALUES': TokenType.VALUES,
    'IS': TokenType.IS,
    'ARE': TokenType.ARE,
    'OF': TokenType.OF,
    'IN': TokenType.IN,
    'ON': TokenType.ON,
    'WITH': TokenType.WITH,
    'NO': TokenType.NO,
    'ADVANCING': TokenType.ADVANCING,
    'LINE': TokenType.LINE,
    'LINES': TokenType.LINES,
    'PAGE': TokenType.PAGE,
    'UPON': TokenType.UPON,
    'ALSO': TokenType.ALSO,
    // Screen control extensions
    'POSITION': TokenType.POSITION,
    'POS': TokenType.POSITION,
    'COL': TokenType.COL,
    'COLUMN': TokenType.COLUMN,
    'ERASE': TokenType.ERASE,
    'EOS': TokenType.EOS,
    'EOL': TokenType.EOL,
    'BLANK': TokenType.BLANK,
    'SCREEN': TokenType.SCREEN,
    'HIGHLIGHT': TokenType.HIGHLIGHT,
    'LOWLIGHT': TokenType.LOWLIGHT,
    'BLINK': TokenType.BLINK,
    'REVERSE-VIDEO': TokenType.REVERSE_VIDEO,
    'UNDERLINE': TokenType.UNDERLINE,
    'BELL': TokenType.BELL,
    'BEEP': TokenType.BEEP,
    'SIZE': TokenType.SIZE,
    'FOREGROUND-COLOR': TokenType.FOREGROUND_COLOR,
    'BACKGROUND-COLOR': TokenType.BACKGROUND_COLOR,
    'CONTROL': TokenType.CONTROL,
    'FULL': TokenType.FULL,
    'REQUIRED': TokenType.REQUIRED,
    'SECURE': TokenType.SECURE,
    'AUTO': TokenType.AUTO,
    'CURSOR': TokenType.CURSOR,
};

/**
 * Token class
 */
export class Token {
    constructor(type, value, line, column) {
        this.type = type;
        this.value = value;
        this.line = line;
        this.column = column;
    }

    toString() {
        return `Token(${this.type}, ${JSON.stringify(this.value)}, ${this.line}:${this.column})`;
    }
}

/**
 * Lexer class
 */
/**
 * Keywords that require specific dialect features
 */
const DIALECT_KEYWORDS = {
    // COBOL-85+ keywords
    'END-IF': 'endIf',
    'END-PERFORM': 'endPerform',
    'END-EVALUATE': 'endEvaluate',
    'END-READ': 'endRead',
    'END-WRITE': 'endWrite',
    'END-SEARCH': 'endSearch',
    'EVALUATE': 'evaluate',
    'INITIALIZE': 'initialize',
    'CONTINUE': 'continueStatement',
};

export class Lexer {
    constructor(source, dialectManager = null) {
        this.source = source;
        this.pos = 0;
        this.line = 1;
        this.column = 1;
        this.tokens = [];
        this.dialectManager = dialectManager || new DialectManager(CobolDialect.COBOL_85);
    }

    /**
     * Set dialect manager
     */
    setDialectManager(dialectManager) {
        this.dialectManager = dialectManager;
    }

    /**
     * Check if a keyword is valid for the current dialect
     */
    isKeywordValid(keyword) {
        const requiredFeature = DIALECT_KEYWORDS[keyword];
        if (!requiredFeature) {
            return true; // Not dialect-specific
        }
        return this.dialectManager.hasFeature(requiredFeature);
    }

    /**
     * Check keyword and record warning if not available
     */
    checkKeyword(keyword) {
        const requiredFeature = DIALECT_KEYWORDS[keyword];
        if (requiredFeature) {
            this.dialectManager.checkFeature(requiredFeature, `mot-clé ${keyword}`);
        }
    }

    /**
     * Get current character
     */
    current() {
        return this.source[this.pos] || null;
    }

    /**
     * Peek ahead n characters
     */
    peek(n = 1) {
        return this.source[this.pos + n] || null;
    }

    /**
     * Advance position
     */
    advance() {
        const char = this.current();
        this.pos++;
        if (char === '\n') {
            this.line++;
            this.column = 1;
        } else {
            this.column++;
        }
        return char;
    }

    /**
     * Skip whitespace (but not newlines in fixed format)
     */
    skipWhitespace() {
        while (this.current() && /[ \t]/.test(this.current())) {
            this.advance();
        }
    }

    /**
     * Check if we're in the indicator area (column 7)
     */
    isIndicatorColumn() {
        return this.column === 7;
    }

    /**
     * Read a string literal
     */
    readString() {
        const quote = this.current();
        const startLine = this.line;
        const startColumn = this.column;
        let value = '';

        this.advance(); // Skip opening quote

        while (this.current() && this.current() !== quote) {
            if (this.current() === '\n') {
                throw new Error(`Unterminated string at line ${startLine}`);
            }
            value += this.advance();
        }

        if (this.current() === quote) {
            this.advance(); // Skip closing quote
        } else {
            throw new Error(`Unterminated string at line ${startLine}`);
        }

        return new Token(TokenType.STRING_LITERAL, value, startLine, startColumn);
    }

    /**
     * Read a number
     */
    readNumber() {
        const startLine = this.line;
        const startColumn = this.column;
        let value = '';
        let hasDecimal = false;
        let hasSign = false;

        // Handle sign
        if (this.current() === '+' || this.current() === '-') {
            value += this.advance();
            hasSign = true;
        }

        while (this.current() && (/\d/.test(this.current()) || this.current() === '.' || this.current() === ',')) {
            if (this.current() === '.' || this.current() === ',') {
                if (hasDecimal) break;
                // Check if it's a decimal point or end of statement
                if (this.current() === '.' && (!this.peek() || /\s/.test(this.peek()))) {
                    break; // It's a statement terminator
                }
                // Check if comma is followed by space (argument separator, not decimal)
                if (this.current() === ',' && (!this.peek() || /\s/.test(this.peek()))) {
                    break; // It's an argument separator like TABLE(1, 2)
                }
                hasDecimal = true;
                value += this.current() === ',' ? '.' : this.current();
                this.advance();
            } else {
                value += this.advance();
            }
        }

        return new Token(TokenType.NUMBER, parseFloat(value), startLine, startColumn);
    }

    /**
     * Read an identifier or keyword
     */
    readIdentifier() {
        const startLine = this.line;
        const startColumn = this.column;
        let value = '';

        while (this.current() && /[A-Za-z0-9\-_]/.test(this.current())) {
            value += this.advance();
        }

        const upperValue = value.toUpperCase();

        // Check for level numbers (01-49, 66, 77, 88)
        if (/^\d{1,2}$/.test(value)) {
            const num = parseInt(value);
            if ((num >= 1 && num <= 49) || num === 66 || num === 77 || num === 88) {
                return new Token(TokenType.LEVEL_NUMBER, num, startLine, startColumn);
            }
        }

        // Check for keywords
        if (KEYWORDS[upperValue]) {
            // Check if this keyword is valid for the current dialect
            this.checkKeyword(upperValue);
            return new Token(KEYWORDS[upperValue], upperValue, startLine, startColumn);
        }

        return new Token(TokenType.IDENTIFIER, upperValue, startLine, startColumn);
    }

    /**
     * Get dialect warnings
     */
    getDialectWarnings() {
        return this.dialectManager.getWarnings();
    }

    /**
     * Get effective dialect info
     */
    getDialectInfo() {
        return this.dialectManager.getDialectInfo();
    }

    /**
     * Read PIC string (special handling for PICTURE clauses)
     */
    readPicString() {
        const startLine = this.line;
        const startColumn = this.column;
        let value = '';

        // PIC strings can contain X, 9, A, V, S, P, Z, *, +, -, ( )
        // Also . for decimal, but NOT if followed by whitespace (statement terminator)
        while (this.current()) {
            const char = this.current();

            // Check for period - could be decimal or statement terminator
            if (char === '.') {
                const nextChar = this.peek();
                // If period is followed by whitespace or end, it's a statement terminator
                if (!nextChar || /[\s\n]/.test(nextChar)) {
                    break;
                }
                // Otherwise it's part of the PIC (decimal point)
                value += this.advance();
            }
            else if (/[X9AVSPZ*+\-,()0-9]/.test(char)) {
                value += this.advance();
            }
            else {
                break;
            }
        }

        return new Token(TokenType.PIC_STRING, value, startLine, startColumn);
    }

    /**
     * Skip comment line
     */
    skipComment() {
        const startLine = this.line;
        let value = '';

        while (this.current() && this.current() !== '\n') {
            value += this.advance();
        }

        return new Token(TokenType.COMMENT, value.trim(), startLine, 7);
    }

    /**
     * Tokenize the source code
     */
    tokenize() {
        this.tokens = [];
        let lastWasPic = false;

        while (this.pos < this.source.length) {
            const char = this.current();

            // Handle newlines
            if (char === '\n') {
                this.advance();
                continue;
            }

            // Skip sequence number area (columns 1-6) and identification area (columns 73-80)
            // In free format, we'll be more lenient
            if (this.column <= 6) {
                this.advance();
                continue;
            }

            // Handle indicator area (column 7)
            if (this.column === 7) {
                if (char === '*' || char === '/') {
                    this.tokens.push(this.skipComment());
                    continue;
                } else if (char === '-') {
                    // Continuation line - skip for now
                    this.advance();
                    continue;
                } else if (char === 'D' || char === 'd') {
                    // Debug line - treat as comment for now
                    this.tokens.push(this.skipComment());
                    continue;
                }
                this.advance();
                continue;
            }

            // Skip whitespace
            if (/[ \t]/.test(char)) {
                this.skipWhitespace();
                continue;
            }

            // Handle PIC string right after PIC keyword
            if (lastWasPic && /[X9AVSPZ(]/.test(char)) {
                this.tokens.push(this.readPicString());
                lastWasPic = false;
                continue;
            }
            lastWasPic = false;

            // String literals
            if (char === '"' || char === "'") {
                this.tokens.push(this.readString());
                continue;
            }

            // Level numbers (01-49, 66, 77, 88) - check before general numbers
            // A level number is 2 digits (with leading zero for 01-09) followed by whitespace
            // OR special levels like 66, 77, 88
            if (/\d/.test(char)) {
                // Peek ahead to determine if this is a level number
                const nextChar = this.peek();
                const twoCharsAhead = this.peek(2);

                // Check for 2-digit level number pattern: two digits followed by whitespace
                // Only recognize as level number if it starts with 0 (for 01-09) or is 66/77/88
                if (/\d/.test(nextChar) && (!twoCharsAhead || /[\s]/.test(twoCharsAhead))) {
                    const levelStr = char + nextChar;
                    const levelNum = parseInt(levelStr);
                    // Only treat as level number if:
                    // - starts with 0 (like 01, 02, etc.) - these are definitely levels
                    // - or is a special level (66, 77, 88)
                    // - or is 10-49 (but only if in data context - hard to determine, so skip for now)
                    if (char === '0' && levelNum >= 1 && levelNum <= 49) {
                        // Definitely a level number (01-09)
                        const startLine = this.line;
                        const startColumn = this.column;
                        this.advance(); // first digit
                        this.advance(); // second digit
                        this.tokens.push(new Token(TokenType.LEVEL_NUMBER, levelNum, startLine, startColumn));
                        continue;
                    }
                    if (levelNum === 66 || levelNum === 77 || levelNum === 88) {
                        // Special level numbers
                        const startLine = this.line;
                        const startColumn = this.column;
                        this.advance();
                        this.advance();
                        this.tokens.push(new Token(TokenType.LEVEL_NUMBER, levelNum, startLine, startColumn));
                        continue;
                    }
                    // For 10-49 without leading zero, check if followed by an identifier
                    // This is a heuristic: level numbers are followed by variable names
                    // NOT followed by keywords like TO, GIVING, FROM, etc.
                    if (levelNum >= 10 && levelNum <= 49) {
                        // Look ahead past the space to see if it's followed by an identifier
                        let lookAhead = 2;
                        while (this.peek(lookAhead) && /[\s]/.test(this.peek(lookAhead))) {
                            lookAhead++;
                        }
                        const afterSpace = this.peek(lookAhead);
                        if (afterSpace && /[A-Za-z]/.test(afterSpace)) {
                            // Read the word to check if it's a keyword
                            let word = '';
                            let wordLookAhead = lookAhead;
                            while (this.peek(wordLookAhead) && /[A-Za-z0-9\-]/.test(this.peek(wordLookAhead))) {
                                word += this.peek(wordLookAhead);
                                wordLookAhead++;
                            }
                            word = word.toUpperCase();
                            // If followed by a COBOL keyword, this is a NUMBER not a LEVEL_NUMBER
                            const keywords = ['TO', 'FROM', 'GIVING', 'BY', 'INTO', 'TIMES', 'UNTIL', 'VARYING', 'THRU', 'THROUGH'];
                            if (keywords.includes(word)) {
                                // It's a number (like MOVE 25 TO X)
                                this.tokens.push(this.readNumber());
                                continue;
                            }
                            // Probably a level number followed by a variable name
                            const startLine = this.line;
                            const startColumn = this.column;
                            this.advance();
                            this.advance();
                            this.tokens.push(new Token(TokenType.LEVEL_NUMBER, levelNum, startLine, startColumn));
                            continue;
                        }
                    }
                }

                // Otherwise it's a regular number
                this.tokens.push(this.readNumber());
                continue;
            }

            // Numbers with sign
            if ((char === '+' || char === '-') && this.peek() && /\d/.test(this.peek())) {
                this.tokens.push(this.readNumber());
                continue;
            }

            // Identifiers and keywords
            if (/[A-Za-z]/.test(char)) {
                const token = this.readIdentifier();
                this.tokens.push(token);
                if (token.type === TokenType.PIC || token.type === TokenType.PICTURE) {
                    lastWasPic = true;
                    // Skip optional IS
                    this.skipWhitespace();
                    if (this.source.substring(this.pos, this.pos + 2).toUpperCase() === 'IS') {
                        this.pos += 2;
                        this.column += 2;
                        this.skipWhitespace();
                    }
                }
                continue;
            }

            // Operators and punctuation
            const startLine = this.line;
            const startColumn = this.column;

            switch (char) {
                case '.':
                    this.advance();
                    this.tokens.push(new Token(TokenType.DOT, '.', startLine, startColumn));
                    break;
                case ',':
                    this.advance();
                    this.tokens.push(new Token(TokenType.COMMA, ',', startLine, startColumn));
                    break;
                case '(':
                    this.advance();
                    this.tokens.push(new Token(TokenType.LPAREN, '(', startLine, startColumn));
                    break;
                case ')':
                    this.advance();
                    this.tokens.push(new Token(TokenType.RPAREN, ')', startLine, startColumn));
                    break;
                case ':':
                    this.advance();
                    this.tokens.push(new Token(TokenType.COLON, ':', startLine, startColumn));
                    break;
                case '=':
                    this.advance();
                    this.tokens.push(new Token(TokenType.EQUALS, '=', startLine, startColumn));
                    break;
                case '+':
                    this.advance();
                    this.tokens.push(new Token(TokenType.PLUS, '+', startLine, startColumn));
                    break;
                case '-':
                    this.advance();
                    this.tokens.push(new Token(TokenType.MINUS, '-', startLine, startColumn));
                    break;
                case '*':
                    if (this.peek() === '*') {
                        this.advance();
                        this.advance();
                        this.tokens.push(new Token(TokenType.POWER, '**', startLine, startColumn));
                    } else {
                        this.advance();
                        this.tokens.push(new Token(TokenType.STAR, '*', startLine, startColumn));
                    }
                    break;
                case '/':
                    this.advance();
                    this.tokens.push(new Token(TokenType.SLASH, '/', startLine, startColumn));
                    break;
                case '>':
                    this.advance();
                    if (this.current() === '=') {
                        this.advance();
                        this.tokens.push(new Token(TokenType.GREATER_EQUAL, '>=', startLine, startColumn));
                    } else {
                        this.tokens.push(new Token(TokenType.GREATER_THAN, '>', startLine, startColumn));
                    }
                    break;
                case '<':
                    this.advance();
                    if (this.current() === '=') {
                        this.advance();
                        this.tokens.push(new Token(TokenType.LESS_EQUAL, '<=', startLine, startColumn));
                    } else if (this.current() === '>') {
                        this.advance();
                        this.tokens.push(new Token(TokenType.NOT_EQUAL, '<>', startLine, startColumn));
                    } else {
                        this.tokens.push(new Token(TokenType.LESS_THAN, '<', startLine, startColumn));
                    }
                    break;
                default:
                    // Skip unknown characters
                    this.advance();
            }
        }

        this.tokens.push(new Token(TokenType.EOF, null, this.line, this.column));
        return this.tokens;
    }
}

/**
 * Helper function to tokenize source code
 * @param {string} source - COBOL source code
 * @returns {Token[]} Array of tokens
 */
export function tokenize(source) {
    const lexer = new Lexer(source);
    return lexer.tokenize();
}
