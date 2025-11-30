/**
 * COBOL Dialect Configuration
 * Defines features available in each COBOL standard version
 */

export const CobolDialect = {
    COBOL_68: 'COBOL-68',
    COBOL_74: 'COBOL-74',
    COBOL_85: 'COBOL-85',
    COBOL_2002: 'COBOL-2002',
    COBOL_2014: 'COBOL-2014',
    AUTO: 'AUTO'  // Auto-detect
};

/**
 * Feature flags for each dialect
 */
export const DialectFeatures = {
    [CobolDialect.COBOL_68]: {
        name: 'COBOL-68 (ANSI)',
        year: 1968,
        description: 'First ANSI standard - basic COBOL',
        features: {
            // Control structures
            endIf: false,
            endPerform: false,
            endEvaluate: false,
            endRead: false,
            endWrite: false,
            endSearch: false,
            evaluate: false,
            inlinePerform: false,
            continueStatement: false,

            // Data handling
            initialize: false,
            referenceModification: false,

            // File handling
            sortMerge: true,
            relativeFiles: false,

            // Program structure
            nestedPrograms: false,
            copyReplace: false,

            // Expressions
            computeExtended: false,

            // Format
            freeFormat: false,

            // OO features
            objectOriented: false,
        }
    },

    [CobolDialect.COBOL_74]: {
        name: 'COBOL-74 (ANSI X3.23-1974)',
        year: 1974,
        description: 'Enhanced file handling and communications',
        features: {
            // Control structures
            endIf: false,
            endPerform: false,
            endEvaluate: false,
            endRead: false,
            endWrite: false,
            endSearch: false,
            evaluate: false,
            inlinePerform: false,
            continueStatement: false,

            // Data handling
            initialize: false,
            referenceModification: false,

            // File handling
            sortMerge: true,
            relativeFiles: true,

            // Program structure
            nestedPrograms: false,
            copyReplace: true,

            // Expressions
            computeExtended: false,

            // Format
            freeFormat: false,

            // OO features
            objectOriented: false,
        }
    },

    [CobolDialect.COBOL_85]: {
        name: 'COBOL-85 (ANSI X3.23-1985)',
        year: 1985,
        description: 'Structured programming - END-IF, EVALUATE, inline PERFORM',
        features: {
            // Control structures
            endIf: true,
            endPerform: true,
            endEvaluate: true,
            endRead: true,
            endWrite: true,
            endSearch: true,
            evaluate: true,
            inlinePerform: true,
            continueStatement: true,

            // Data handling
            initialize: true,
            referenceModification: true,

            // File handling
            sortMerge: true,
            relativeFiles: true,

            // Program structure
            nestedPrograms: true,
            copyReplace: true,

            // Expressions
            computeExtended: true,

            // Format
            freeFormat: false,

            // OO features
            objectOriented: false,
        }
    },

    [CobolDialect.COBOL_2002]: {
        name: 'COBOL 2002 (ISO/IEC 1989:2002)',
        year: 2002,
        description: 'Object-oriented COBOL, free-form source',
        features: {
            // Control structures
            endIf: true,
            endPerform: true,
            endEvaluate: true,
            endRead: true,
            endWrite: true,
            endSearch: true,
            evaluate: true,
            inlinePerform: true,
            continueStatement: true,

            // Data handling
            initialize: true,
            referenceModification: true,

            // File handling
            sortMerge: true,
            relativeFiles: true,

            // Program structure
            nestedPrograms: true,
            copyReplace: true,

            // Expressions
            computeExtended: true,

            // Format
            freeFormat: true,

            // OO features
            objectOriented: true,
        }
    },

    [CobolDialect.COBOL_2014]: {
        name: 'COBOL 2014 (ISO/IEC 1989:2014)',
        year: 2014,
        description: 'Latest standard - enhanced OO, XML/JSON support',
        features: {
            // Control structures
            endIf: true,
            endPerform: true,
            endEvaluate: true,
            endRead: true,
            endWrite: true,
            endSearch: true,
            evaluate: true,
            inlinePerform: true,
            continueStatement: true,

            // Data handling
            initialize: true,
            referenceModification: true,

            // File handling
            sortMerge: true,
            relativeFiles: true,

            // Program structure
            nestedPrograms: true,
            copyReplace: true,

            // Expressions
            computeExtended: true,

            // Format
            freeFormat: true,

            // OO features
            objectOriented: true,
        }
    }
};

/**
 * Detection patterns for auto-detecting dialect
 */
export const DetectionPatterns = {
    // COBOL-2002+ indicators
    cobol2002: [
        /\bCLASS-ID\b/i,
        /\bMETHOD-ID\b/i,
        /\bINVOKE\b/i,
        /\bOBJECT\b/i,
        /\bFACTORY\b/i,
        /\bRAISE\b/i,
    ],

    // COBOL-85 indicators (not in 74)
    cobol85: [
        /\bEND-IF\b/i,
        /\bEND-PERFORM\b/i,
        /\bEND-EVALUATE\b/i,
        /\bEND-READ\b/i,
        /\bEVALUATE\b/i,
        /\bINITIALIZE\b/i,
        /\bCONTINUE\b/i,
        /\bEND-CALL\b/i,
        /\bEND-SEARCH\b/i,
    ],

    // COBOL-74 indicators (not in 68)
    cobol74: [
        /\bCOPY\b.*\bREPLACING\b/i,
        /\bRELATIVE\s+KEY\b/i,
        /\bORGANIZATION\s+IS\s+RELATIVE\b/i,
    ],

    // Free format detection (COBOL-2002+)
    freeFormat: [
        /^[^\s\d*\/].{0,5}[A-Z]/m,  // Code starting before column 7
    ]
};

/**
 * Dialect manager class
 */
export class DialectManager {
    constructor(dialect = CobolDialect.COBOL_85) {
        this.dialect = dialect;
        this.detectedDialect = null;
        this.strictMode = false;  // If true, reject unsupported features
        this.warnings = [];
    }

    /**
     * Set the dialect
     */
    setDialect(dialect) {
        this.dialect = dialect;
        this.warnings = [];
    }

    /**
     * Enable/disable strict mode
     */
    setStrictMode(strict) {
        this.strictMode = strict;
    }

    /**
     * Get current dialect configuration
     */
    getConfig() {
        if (this.dialect === CobolDialect.AUTO && this.detectedDialect) {
            return DialectFeatures[this.detectedDialect];
        }
        return DialectFeatures[this.dialect] || DialectFeatures[CobolDialect.COBOL_85];
    }

    /**
     * Get current effective dialect
     */
    getEffectiveDialect() {
        if (this.dialect === CobolDialect.AUTO && this.detectedDialect) {
            return this.detectedDialect;
        }
        return this.dialect;
    }

    /**
     * Check if a feature is available
     */
    hasFeature(featureName) {
        const config = this.getConfig();
        return config?.features?.[featureName] ?? false;
    }

    /**
     * Check feature and optionally warn/error
     */
    checkFeature(featureName, context = '') {
        const hasIt = this.hasFeature(featureName);

        if (!hasIt) {
            const dialect = this.getEffectiveDialect();
            const message = `${featureName} n'est pas disponible en ${dialect}${context ? ` (${context})` : ''}`;

            if (this.strictMode) {
                throw new Error(message);
            } else {
                this.warnings.push(message);
            }
        }

        return hasIt;
    }

    /**
     * Auto-detect dialect from source code
     */
    detectDialect(sourceCode) {
        // Check for COBOL-2002+ features first
        for (const pattern of DetectionPatterns.cobol2002) {
            if (pattern.test(sourceCode)) {
                this.detectedDialect = CobolDialect.COBOL_2002;
                return this.detectedDialect;
            }
        }

        // Check for free format (COBOL-2002+)
        for (const pattern of DetectionPatterns.freeFormat) {
            if (pattern.test(sourceCode)) {
                this.detectedDialect = CobolDialect.COBOL_2002;
                return this.detectedDialect;
            }
        }

        // Check for COBOL-85 features
        for (const pattern of DetectionPatterns.cobol85) {
            if (pattern.test(sourceCode)) {
                this.detectedDialect = CobolDialect.COBOL_85;
                return this.detectedDialect;
            }
        }

        // Check for COBOL-74 features
        for (const pattern of DetectionPatterns.cobol74) {
            if (pattern.test(sourceCode)) {
                this.detectedDialect = CobolDialect.COBOL_74;
                return this.detectedDialect;
            }
        }

        // Default to COBOL-68 if nothing specific found
        this.detectedDialect = CobolDialect.COBOL_68;
        return this.detectedDialect;
    }

    /**
     * Get all warnings
     */
    getWarnings() {
        return [...this.warnings];
    }

    /**
     * Clear warnings
     */
    clearWarnings() {
        this.warnings = [];
    }

    /**
     * Get dialect info for display
     */
    getDialectInfo() {
        const config = this.getConfig();
        return {
            dialect: this.getEffectiveDialect(),
            name: config.name,
            year: config.year,
            description: config.description,
            isAutoDetected: this.dialect === CobolDialect.AUTO,
        };
    }

    /**
     * Get list of all available dialects
     */
    static getAvailableDialects() {
        return Object.entries(DialectFeatures).map(([key, config]) => ({
            id: key,
            name: config.name,
            year: config.year,
            description: config.description,
        }));
    }
}

// Default export
export default DialectManager;
