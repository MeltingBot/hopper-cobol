/**
 * COBOL Interpreter
 * Executes COBOL AST in the browser
 */

import { NodeType } from './parser.js';
import * as storage from './storage.js';

// Re-export NodeType for use in buildSymbolTable
const { CONDITION_NAME } = NodeType;

/**
 * PIC clause parser - extracts type and length from PIC strings
 */
function parsePic(picString) {
    if (!picString) return {
        type: 'alphanumeric', length: 1, decimals: 0, signed: false, edited: false, editMask: '',
        intDigits: 0, decDigits: 0
    };

    const pic = picString.toUpperCase();
    let type = 'alphanumeric';
    let length = 0;
    let decimals = 0;
    let intDigits = 0;   // Count of integer digits (before V)
    let decDigits = 0;   // Count of decimal digits (after V)
    let signed = false;
    let edited = false;

    // Check for sign
    if (pic.includes('S')) {
        signed = true;
    }

    // Check for edited picture (Z, *, comma, period in numeric context)
    if (pic.includes('Z') || pic.includes('*') ||
        (pic.includes(',') && (pic.includes('9') || pic.includes('Z'))) ||
        (pic.includes('.') && (pic.includes('9') || pic.includes('Z')))) {
        edited = true;
    }

    // Check for type
    if (pic.includes('9') || pic.includes('V') || pic.includes('Z') || pic.includes('*')) {
        type = 'numeric';
    } else if (pic.includes('A')) {
        type = 'alphabetic';
    }

    // Build expanded edit mask and calculate length
    let editMask = '';
    let i = 0;
    let inDecimal = false;

    while (i < pic.length) {
        const char = pic[i];

        if (char === 'V') {
            inDecimal = true;
            i++;
            continue;
        }

        if (char === '(') {
            // Find the number inside parentheses
            let numStr = '';
            i++;
            while (i < pic.length && pic[i] !== ')') {
                numStr += pic[i];
                i++;
            }
            const count = parseInt(numStr) || 1;
            // Get the character before the parenthesis
            const prevChar = editMask.length > 0 ? editMask[editMask.length - 1] : '';
            // Add count-1 more of that character (one was already added)
            for (let j = 1; j < count; j++) {
                editMask += prevChar;
                if ('9Z*'.includes(prevChar)) {
                    if (inDecimal) {
                        decimals++;
                        decDigits++;
                    } else {
                        intDigits++;
                    }
                }
                if ('X9AZ*'.includes(prevChar)) {
                    length++;
                }
            }
            i++;
            continue;
        }

        if ('X9AVSZ*+-.,'.includes(char)) {
            editMask += char;
            if ('9Z*'.includes(char)) {
                if (inDecimal) {
                    decimals++;
                    decDigits++;
                } else {
                    intDigits++;
                }
            }
            if ('X9AZ*'.includes(char)) {
                length++;
            }
            // Comma and period add to display length for edited pictures
            if (edited && (char === ',' || char === '.')) {
                length++;
            }
        } else if (char !== 'S') {
            // Other characters (like spaces in edit masks)
            editMask += char;
        }

        i++;
    }

    return { type, length: length || 1, decimals, signed, edited, editMask, intDigits, decDigits };
}

/**
 * Format a value according to PIC clause
 */
function formatValue(value, pic) {
    const { type, length, decimals, signed, edited, editMask } = parsePic(pic);

    if (type === 'numeric') {
        // Convert to numeric value first
        let numValue;
        if (typeof value === 'number') {
            numValue = value;
        } else {
            const str = String(value).replace(/[^0-9.-]/g, '');
            numValue = parseFloat(str) || 0;
        }

        // For edited pictures, apply the edit mask
        if (edited) {
            return applyEditMask(numValue, editMask, decimals);
        }

        // For non-edited numeric, format as digits
        if (decimals > 0) {
            const scaled = Math.round(Math.abs(numValue) * Math.pow(10, decimals));
            return String(scaled).padStart(length, '0').slice(-length);
        }
        return String(Math.abs(Math.floor(numValue))).padStart(length, '0').slice(-length);
    }

    // Alphanumeric - left align and pad with spaces
    const str = String(value || '');
    return str.padEnd(length, ' ').substring(0, length);
}

/**
 * Apply edit mask to a numeric value
 * Z = zero suppress, * = asterisk fill, comma/period = insertion
 */
function applyEditMask(value, mask, decimals) {
    // Scale value for decimals
    const scaled = decimals > 0 ? Math.round(value * Math.pow(10, decimals)) : Math.floor(value);
    const isNegative = value < 0;
    const absValue = Math.abs(scaled);

    // Count digit positions (9, Z, *)
    let digitCount = 0;
    for (const ch of mask) {
        if ('9Z*'.includes(ch)) digitCount++;
    }

    // Convert value to string with leading zeros
    const digits = String(absValue).padStart(digitCount, '0').slice(-digitCount);

    // Apply mask
    let result = '';
    let digitIndex = 0;
    let inSignificant = false;

    for (let i = 0; i < mask.length; i++) {
        const maskChar = mask[i];

        if (maskChar === '9') {
            result += digits[digitIndex++];
            inSignificant = true;
        } else if (maskChar === 'Z') {
            const digit = digits[digitIndex++];
            if (digit === '0' && !inSignificant) {
                result += ' ';
            } else {
                result += digit;
                inSignificant = true;
            }
        } else if (maskChar === '*') {
            const digit = digits[digitIndex++];
            if (digit === '0' && !inSignificant) {
                result += '*';
            } else {
                result += digit;
                inSignificant = true;
            }
        } else if (maskChar === ',') {
            // Comma: show only if we've had significant digits
            if (inSignificant) {
                result += ',';
            } else {
                result += ' '; // Zero suppress comma too
            }
        } else if (maskChar === '.') {
            // Decimal point: always show
            result += '.';
            inSignificant = true; // Digits after decimal always shown
        } else if (maskChar === 'V') {
            // Virtual decimal - skip, no output
            inSignificant = true;
        } else if (maskChar === '-' || maskChar === '+') {
            // Sign
            if (i === 0 || i === mask.length - 1) {
                // Leading or trailing sign
                result += isNegative ? '-' : (maskChar === '+' ? '+' : ' ');
            } else {
                // Floating sign acts like Z
                const digit = digits[digitIndex++];
                if (digit === '0' && !inSignificant) {
                    result += ' ';
                } else {
                    result += digit;
                    inSignificant = true;
                }
            }
        } else {
            result += maskChar;
        }
    }

    return result;
}

/**
 * Variable class - represents a COBOL data item
 */
class Variable {
    constructor(definition) {
        this.name = definition.name;
        this.level = definition.level;
        this.pic = definition.pic;
        this.picInfo = parsePic(definition.pic);
        this.initialValue = definition.value;
        this.sign = 1; // 1 for positive, -1 for negative
        // Initialize children and parent BEFORE getInitialValue (which may call getTotalLength)
        this.children = [];
        this.parent = null;
        this.redefines = definition.redefines;
        this.redefinesVar = null; // Will be set to point to the redefined variable
        // For OCCURS x TO y DEPENDING ON: occurs=x (min), occursMax=y (max)
        // For simple OCCURS n: occurs=n (fixed count)
        if (definition.occursMax) {
            // Variable-length table: OCCURS x TO y DEPENDING ON
            this.occursMin = definition.occurs || 1;
            this.occursCount = definition.occursMax;
        } else {
            // Fixed-length table: OCCURS n
            this.occursMin = definition.occurs || null;
            this.occursCount = definition.occurs || null;
        }
        this.occursDependingOn = definition.occursDependingOn || null; // Variable name for dynamic size
        this.indexedBy = definition.indexedBy || []; // Index names for SEARCH
        this.ascendingKey = definition.ascendingKey || null; // Key for SEARCH ALL
        this.descendingKey = definition.descendingKey || null;
        // USAGE clause: DISPLAY (default), BINARY (COMP/COMP-4), PACKED-DECIMAL (COMP-3), etc.
        this.usage = definition.usage || 'DISPLAY';
        // BLANK WHEN ZERO: display spaces instead of zeros
        this.blankWhenZero = definition.blankWhenZero || false;
        // JUSTIFIED RIGHT: right-justify alphanumeric data
        this.justified = definition.justified || null;
        // SIGN clause: { position: 'LEADING'|'TRAILING', separate: boolean }
        this.signClause = definition.signClause || null;
        // Initialize value AFTER children array exists (for getTotalLength in groups)
        this.value = this.getInitialValue();
    }

    getInitialValue() {
        if (this.initialValue) {
            let result;
            if (this.initialValue.type === 'string') {
                result = this.initialValue.value;
            } else if (this.initialValue.type === 'number') {
                // Store sign for signed numeric variables
                if (this.picInfo.signed && this.initialValue.value < 0) {
                    this.sign = -1;
                }
                return formatValue(this.initialValue.value, this.pic);
            } else if (this.initialValue.type === 'figurative') {
                return this.getFigurativeValue(this.initialValue.value);
            } else if (this.initialValue.type === 'all') {
                // ALL "x" - repeat the character/string to fill the field
                const pattern = this.initialValue.value;
                const repeats = Math.ceil(this.picInfo.length / pattern.length);
                return pattern.repeat(repeats).substring(0, this.picInfo.length);
            }

            // Apply JUSTIFIED RIGHT for alphanumeric string values
            if (result !== undefined && this.justified === 'RIGHT' && this.picInfo.type !== 'numeric') {
                return result.padStart(this.picInfo.length, ' ').slice(-this.picInfo.length);
            }
            if (result !== undefined) {
                return formatValue(result, this.pic);
            }
        }

        // Default initialization
        // For groups (no PIC), we'll initialize properly after children are attached
        // For elementary items with PIC, initialize based on type
        if (this.pic) {
            const len = this.picInfo.length;
            if (this.picInfo.type === 'numeric') {
                return '0'.repeat(len);
            }
            return ' '.repeat(len);
        }

        // Group item - use placeholder, will be reinitialized after children attached
        // Return empty string initially, buildSymbolTable will fix this
        return '';
    }

    getFigurativeValue(figurative) {
        switch (figurative) {
            case 'SPACES':
                return ' '.repeat(this.picInfo.length);
            case 'ZEROS':
                return '0'.repeat(this.picInfo.length);
            case 'LOW-VALUES':
                return '\x00'.repeat(this.picInfo.length);
            case 'HIGH-VALUES':
                return '\xFF'.repeat(this.picInfo.length);
            case 'QUOTES':
                return '"'.repeat(this.picInfo.length);
            default:
                return ' '.repeat(this.picInfo.length);
        }
    }

    getValue() {
        // Check if this variable or any ancestor has REDEFINES
        const redefinesRoot = this.findRedefinesRoot();
        if (redefinesRoot) {
            // Get the base storage from the redefined variable
            const baseValue = redefinesRoot.redefinesVar.getValue();
            // Calculate our offset within the base storage
            const offset = this.calculateOffsetFrom(redefinesRoot);
            // Return our portion of the base storage
            if (this.children.length > 0) {
                return baseValue.substring(offset, offset + this.getTotalLength());
            }
            return baseValue.substring(offset, offset + this.picInfo.length);
        }

        if (this.children.length > 0) {
            // Group item - return value directly (it stores the full group data)
            // For groups with OCCURS children, the storage is in this.value
            return this.value;
        }
        return this.value;
    }

    /**
     * Find the root ancestor that has REDEFINES
     */
    findRedefinesRoot() {
        if (this.redefinesVar) return this;
        if (this.parent) return this.parent.findRedefinesRoot();
        return null;
    }

    /**
     * Calculate offset of this variable from a given ancestor
     */
    calculateOffsetFrom(ancestor) {
        if (this === ancestor) return 0;
        if (!this.parent) return 0;

        // Find our position within parent's children
        let offset = 0;
        for (const sibling of this.parent.children) {
            if (sibling === this) break;
            offset += sibling.getTotalLength();
        }

        // Add parent's offset
        return offset + this.parent.calculateOffsetFrom(ancestor);
    }

    /**
     * Get total length of this variable (including all children)
     * For OCCURS items, returns the size of ONE occurrence
     */
    getTotalLength() {
        if (this.children.length > 0) {
            // Sum up children, multiplying by their occursCount if they have one
            return this.children.reduce((sum, c) => {
                const childLen = c.getTotalLength();
                const count = c.occursCount || 1;
                return sum + (childLen * count);
            }, 0);
        }
        return this.getStorageLength();
    }

    /**
     * Get the storage length based on USAGE
     * DISPLAY: 1 byte per character
     * BINARY: 2/4/8 bytes depending on digit count
     * PACKED-DECIMAL: (n+1)/2 bytes (2 digits per byte + sign nibble)
     */
    getStorageLength() {
        const digits = this.picInfo.intDigits + this.picInfo.decDigits;

        switch (this.usage) {
            case 'BINARY':
            case 'COMP-5':
                // Binary storage: halfword, fullword, or doubleword
                if (digits <= 4) return 2;      // 2 bytes (halfword)
                if (digits <= 9) return 4;      // 4 bytes (fullword)
                return 8;                        // 8 bytes (doubleword)

            case 'PACKED-DECIMAL':
                // Packed decimal: 2 digits per byte + 1 nibble for sign
                // For n digits: ceil((n + 1) / 2) bytes
                return Math.ceil((digits + 1) / 2);

            case 'COMP-1':
                return 4;  // Single-precision float (4 bytes)

            case 'COMP-2':
                return 8;  // Double-precision float (8 bytes)

            case 'DISPLAY':
            default:
                // Add 1 byte for SIGN SEPARATE CHARACTER
                const signExtra = (this.signClause?.separate) ? 1 : 0;
                return this.picInfo.length + signExtra;
        }
    }

    /**
     * Get effective OCCURS count (considering DEPENDING ON)
     * @param {CobolRuntime} runtime - Runtime to look up DEPENDING ON variable
     * @returns {number} Current effective count
     */
    getEffectiveOccursCount(runtime) {
        if (!this.occursCount) return 1;

        if (this.occursDependingOn && runtime) {
            // Get the current value of the DEPENDING ON variable
            const depValue = runtime.getNumericValue(this.occursDependingOn);
            // Clamp to valid range
            const min = this.occursMin || 1;
            const max = this.occursCount;
            return Math.max(min, Math.min(max, depValue));
        }

        return this.occursCount;
    }

    /**
     * Get the size of one element (without OCCURS multiplication)
     */
    getElementSize() {
        if (this.children.length > 0) {
            return this.children.reduce((sum, c) => {
                const childLen = c.getElementSize();
                const count = c.occursCount || 1;
                return sum + (childLen * count);
            }, 0);
        }
        return this.getStorageLength();
    }

    getNumericValue() {
        const val = this.getValue().trim();
        let result;
        if (this.picInfo.decimals > 0) {
            const intPart = val.substring(0, val.length - this.picInfo.decimals);
            const decPart = val.substring(val.length - this.picInfo.decimals);
            result = parseFloat(intPart + '.' + decPart) || 0;
        } else {
            result = parseInt(val) || 0;
        }
        // Apply sign for signed numeric variables
        return this.picInfo.signed ? result * this.sign : result;
    }

    /**
     * Get value formatted for display (with decimal point if needed)
     * Applies COBOL edited picture formatting (Z, *, comma, period)
     */
    getDisplayValue() {
        // For group items, return the stored value directly
        // (it contains all occurrences if children have OCCURS)
        if (this.children.length > 0) {
            return this.getValue();
        }

        if (this.picInfo.type === 'numeric') {
            // Check BLANK WHEN ZERO - return spaces if value is zero
            if (this.blankWhenZero) {
                const numVal = this.getNumericValue();
                if (numVal === 0) {
                    return ' '.repeat(this.picInfo.length);
                }
            }

            // For edited pictures, apply the edit mask
            if (this.picInfo.edited) {
                return this.applyEditMask();
            }

            const val = this.getValue().trim();
            let displayVal;
            if (this.picInfo.decimals > 0) {
                const intPart = val.substring(0, val.length - this.picInfo.decimals) || '0';
                const decPart = val.substring(val.length - this.picInfo.decimals);
                displayVal = intPart + '.' + decPart;
            } else {
                displayVal = val;
            }
            // Handle SIGN clause for display
            if (this.picInfo.signed) {
                const signChar = this.sign < 0 ? '-' : '+';
                if (this.signClause?.separate) {
                    // SIGN SEPARATE: explicit + or - character
                    if (this.signClause.position === 'LEADING') {
                        return signChar + displayVal;
                    } else {
                        return displayVal + signChar;
                    }
                } else if (this.sign < 0) {
                    // Default: only show - for negative
                    return '-' + displayVal;
                }
            }
            return displayVal;
        }
        return this.getValue();
    }

    /**
     * Apply COBOL edit mask to format numeric value for display
     * Handles Z (zero suppress), * (check protect), comma, period
     */
    applyEditMask() {
        const mask = this.picInfo.editMask;
        const rawValue = this.getValue().trim();

        // Get numeric value and handle decimals
        let numValue = parseInt(rawValue) || 0;

        // Count digit positions in mask (9, Z, *)
        const digitPositions = [];
        const insertPositions = []; // positions of , and .

        for (let i = 0; i < mask.length; i++) {
            const c = mask[i];
            if ('9Z*'.includes(c)) {
                digitPositions.push({ pos: i, type: c });
            } else if (c === ',' || c === '.') {
                insertPositions.push({ pos: i, char: c });
            }
        }

        // Convert number to string with proper padding
        const numStr = String(Math.abs(numValue)).padStart(digitPositions.length, '0');

        // Build result by applying mask
        let result = '';
        let digitIndex = 0;
        let inSignificant = false; // Have we seen a non-zero digit?

        for (let i = 0; i < mask.length; i++) {
            const maskChar = mask[i];

            if (maskChar === '9') {
                // Always show digit
                result += numStr[digitIndex] || '0';
                if (numStr[digitIndex] !== '0') inSignificant = true;
                digitIndex++;
            } else if (maskChar === 'Z') {
                // Zero suppress - show space if leading zero
                const digit = numStr[digitIndex] || '0';
                if (digit === '0' && !inSignificant) {
                    result += ' ';
                } else {
                    result += digit;
                    inSignificant = true;
                }
                digitIndex++;
            } else if (maskChar === '*') {
                // Check protect - show * if leading zero
                const digit = numStr[digitIndex] || '0';
                if (digit === '0' && !inSignificant) {
                    result += '*';
                } else {
                    result += digit;
                    inSignificant = true;
                }
                digitIndex++;
            } else if (maskChar === ',') {
                // Comma insertion - show comma only if we have significant digits
                if (inSignificant) {
                    result += ',';
                } else {
                    result += ' ';
                }
            } else if (maskChar === '.') {
                // Decimal point - always show (marks transition to decimals)
                result += '.';
                inSignificant = true; // Digits after decimal point always shown
            } else {
                // Other characters (space, etc.) - keep as-is
                result += maskChar;
            }
        }

        // Handle sign for negative values
        if (this.picInfo.signed && this.sign < 0) {
            result = '-' + result.trimStart();
        }

        return result;
    }

    setValue(value) {
        if (this.children.length > 0) {
            // Group item - distribute value to children
            let str = String(value);
            let offset = 0;
            for (const child of this.children) {
                const len = child.getTotalLength();
                child.setValue(str.substring(offset, offset + len));
                offset += len;
            }
            // Update our own value too
            this.value = str.padEnd(this.getTotalLength(), ' ').substring(0, this.getTotalLength());
        } else {
            // Handle sign for signed numeric variables
            if (this.picInfo.signed && typeof value === 'number') {
                this.sign = value < 0 ? -1 : 1;
            }
            // Apply JUSTIFIED RIGHT for alphanumeric data
            if (this.justified === 'RIGHT' && this.picInfo.type !== 'numeric') {
                const str = String(value || '');
                this.value = str.padStart(this.picInfo.length, ' ').slice(-this.picInfo.length);
            } else {
                this.value = formatValue(value, this.pic);
            }
            // Propagate change to parent
            this.propagateToParent();
        }
    }

    /**
     * Propagate value change to parent group
     */
    propagateToParent() {
        if (!this.parent) return;

        // Find our offset in parent
        let offset = 0;
        for (const sibling of this.parent.children) {
            if (sibling === this) break;
            const len = sibling.getTotalLength();
            const occursCount = sibling.occursCount || 1;
            offset += len * occursCount;
        }

        // Update parent's value at our position
        const myLen = this.getTotalLength();
        const myOccurs = this.occursCount || 1;
        const myValue = this.value.padEnd(myLen, ' ').substring(0, myLen);

        const parentVal = this.parent.value || '';
        const totalOffset = offset;
        const newParentVal = parentVal.substring(0, totalOffset) +
                            myValue +
                            parentVal.substring(totalOffset + myLen);

        this.parent.value = newParentVal;

        // Recursively propagate up
        this.parent.propagateToParent();
    }

    /**
     * Set raw value without reformatting (for values already in COBOL format from storage)
     */
    setRawValue(value) {
        const str = String(value || '');
        if (this.children.length > 0) {
            // For groups, store directly - don't distribute to children
            // Children access this storage via offset calculations
            const totalLen = this.getTotalLength();
            this.value = str.padEnd(totalLen, ' ').substring(0, totalLen);
        } else {
            // Elementary item: store as-is, padding/truncating to fit the PIC length
            if (this.picInfo.type === 'numeric') {
                this.value = str.padStart(this.picInfo.length, '0').substring(0, this.picInfo.length);
            } else if (this.justified === 'RIGHT') {
                // JUSTIFIED RIGHT: right-align alphanumeric data
                this.value = str.padStart(this.picInfo.length, ' ').slice(-this.picInfo.length);
            } else {
                this.value = str.padEnd(this.picInfo.length, ' ').substring(0, this.picInfo.length);
            }
        }
    }

    initialize() {
        if (this.children.length > 0) {
            this.children.forEach(c => c.initialize());
        } else {
            this.value = this.getInitialValue();
        }
    }
}

/**
 * File class - represents a COBOL file
 */
class CobolFile {
    constructor(definition, dataManager) {
        this.name = definition.fileName;
        this.assignTo = definition.assignTo;
        this.organization = definition.organization || 'SEQUENTIAL';
        this.accessMode = definition.accessMode || 'SEQUENTIAL';
        this.recordKey = definition.recordKey;
        this.dataManager = dataManager;
        this.isOpen = false;
        this.mode = null;
        this.currentRecord = 0;
        this.records = [];
        this.recordBuffer = null;
    }

    /**
     * Get clean file name (remove quotes and .DAT extension)
     */
    getFileName() {
        return (this.assignTo?.replace(/['"]/g, '').replace('.DAT', '') || this.name).toUpperCase();
    }

    async open(mode) {
        this.mode = mode;
        this.isOpen = true;
        this.currentRecord = 0;

        const fileName = this.getFileName();

        try {
            // Check if file exists in IndexedDB
            const exists = await storage.fileExists(fileName);

            if (exists) {
                // Load records from IndexedDB
                this.records = await storage.readAllRecords(fileName);
            } else {
                // Create the file in IndexedDB for I-O and OUTPUT modes
                if (mode === 'I-O' || mode === 'OUTPUT') {
                    await storage.createFile({
                        name: fileName,
                        organization: this.organization,
                        recordKey: this.recordKey
                    });
                    // Refresh UI
                    if (typeof window !== 'undefined' && window.dataManagerModule?.renderFileList) {
                        window.dataManagerModule.renderFileList();
                    }
                }
                this.records = [];
            }
        } catch (error) {
            
            this.records = [];
        }

        return true;
    }

    async close() {
        const fileName = this.getFileName();

        try {
            // Create file if it doesn't exist
            const exists = await storage.fileExists(fileName);
            if (!exists) {
                await storage.createFile({
                    name: fileName,
                    organization: this.organization,
                    recordKey: this.recordKey
                });
            }

            // Clear and rewrite all records to IndexedDB
            await storage.clearFile(fileName);
            for (const record of this.records) {
                await storage.writeRecord(fileName, record);
            }

            // Refresh data manager cache and UI
            if (typeof window !== 'undefined' && window.dataManagerModule) {
                // Refresh the sync cache for next compilation
                if (window.dataManagerModule.refreshCache) {
                    await window.dataManagerModule.refreshCache();
                }
                // Update UI
                if (window.dataManagerModule.renderFileList) {
                    window.dataManagerModule.renderFileList();
                }
            }
        } catch (error) {
            
        }

        this.isOpen = false;
        this.mode = null;
        this.currentRecord = 0;
        return true;
    }

    read() {
        if (!this.isOpen) return { success: false, atEnd: true };

        if (this.currentRecord >= this.records.length) {
            return { success: false, atEnd: true };
        }

        const record = this.records[this.currentRecord];
        this.currentRecord++;
        return { success: true, atEnd: false, record };
    }

    readKey(keyValue) {
        if (!this.isOpen || this.organization !== 'INDEXED') {
            return { success: false, invalidKey: true };
        }

        // Normalize key field name (FRN-CODE -> FRN_CODE)
        const normalizedKeyName = this.recordKey?.toUpperCase().replace(/-/g, '_');

        const record = this.records.find(r => {
            const keyField = Object.keys(r).find(k =>
                k.toUpperCase() === normalizedKeyName
            );
            const match = keyField && r[keyField]?.toString().trim() === keyValue?.toString().trim();
            if (keyField) {
            }
            return match;
        });

        if (record) {
            return { success: true, record };
        }
        return { success: false, invalidKey: true };
    }

    write(record) {
        if (!this.isOpen || (this.mode !== 'OUTPUT' && this.mode !== 'I-O' && this.mode !== 'EXTEND')) {
            return { success: false };
        }

        this.records.push({ ...record });
        return { success: true };
    }

    rewrite(record) {
        if (!this.isOpen || this.mode !== 'I-O') {
            return { success: false };
        }

        if (this.currentRecord > 0 && this.currentRecord <= this.records.length) {
            this.records[this.currentRecord - 1] = { ...record };
            return { success: true };
        }
        return { success: false };
    }

    delete() {
        if (!this.isOpen || this.mode !== 'I-O') {
            return false;
        }

        if (this.currentRecord > 0 && this.currentRecord <= this.records.length) {
            this.records.splice(this.currentRecord - 1, 1);
            this.currentRecord--;
            return true;
        }
        return false;
    }

    /**
     * START - position cursor for indexed file reading
     */
    start(keyValue, operator = '>=') {
        if (!this.isOpen || this.organization !== 'INDEXED') {
            return false;
        }

        if (this.records.length === 0) {
            return false;
        }

        // Find the position based on the key and operator
        let foundIndex = -1;
        const keyField = this.recordKey;

        for (let i = 0; i < this.records.length; i++) {
            const record = this.records[i];
            const recordKeyValue = record[keyField] || '';
            const trimmedRecordKey = recordKeyValue.toString().trim();
            const trimmedSearchKey = keyValue.toString().trim();

            let match = false;
            switch (operator) {
                case '=':
                    match = trimmedRecordKey === trimmedSearchKey;
                    break;
                case '>':
                    match = trimmedRecordKey > trimmedSearchKey;
                    break;
                case '>=':
                default:
                    match = trimmedRecordKey >= trimmedSearchKey;
                    break;
            }

            if (match) {
                foundIndex = i;
                break;
            }
        }

        if (foundIndex >= 0) {
            this.currentRecord = foundIndex;
            return true;
        }

        return false;
    }

    /**
     * Read all records from the file (for SORT/MERGE)
     */
    readAll() {
        return [...this.records];
    }

    /**
     * Clear all records from the file (for SORT/MERGE output)
     */
    clear() {
        this.records = [];
        this.currentRecord = 0;
        return true;
    }

    /**
     * Set the current record buffer (for RETURN statement)
     */
    setCurrentRecord(record) {
        this.recordBuffer = { ...record };
    }
}

/**
 * CobolFileIDB - IndexedDB-backed COBOL file
 * Uses async operations for true indexed file behavior
 */
class CobolFileIDB {
    constructor(definition, storage) {
        this.name = definition.fileName;
        this.assignTo = definition.assignTo;
        this.organization = definition.organization || 'SEQUENTIAL';
        this.accessMode = definition.accessMode || 'SEQUENTIAL';
        this.recordKey = definition.recordKey;
        this.storage = storage; // IndexedDB storage module
        this.isOpen = false;
        this.mode = null;
        this.cursor = null; // For sequential reading
        this.cursorRecords = []; // Cached records for cursor
        this.cursorPosition = 0;
        this.lastReadRecord = null;
    }

    getFileName() {
        return (this.assignTo?.replace(/['"]/g, '').replace('.DAT', '') || this.name).toUpperCase();
    }

    async open(mode) {
        this.mode = mode;
        this.isOpen = true;
        this.cursorPosition = 0;
        this.cursorRecords = [];

        const fileName = this.getFileName();

        try {
            // Create file if it doesn't exist (for I-O and OUTPUT modes)
            if (mode === 'I-O' || mode === 'OUTPUT') {
                const exists = await this.storage.fileExists(fileName);
                if (!exists) {
                    await this.storage.createFile({
                        name: fileName,
                        organization: this.organization,
                        recordKey: this.recordKey
                    });
                    // Refresh data manager UI
                    if (typeof window !== 'undefined' && window.dataManagerModule?.renderFileList) {
                        window.dataManagerModule.renderFileList();
                    }
                }
            }

            // For OUTPUT mode, clear existing records
            if (mode === 'OUTPUT') {
                await this.storage.clearFile(fileName);
            }
        } catch (error) {
            console.error(`COBOL OPEN error for ${fileName}:`, error);
            // Continue execution even if storage fails
        }

        return true;
    }

    async close() {
        this.isOpen = false;
        this.mode = null;
        this.cursorPosition = 0;
        this.cursorRecords = [];
        this.lastReadRecord = null;

        // Refresh data manager cache and UI
        if (typeof window !== 'undefined' && window.dataManagerModule) {
            if (window.dataManagerModule.refreshCache) {
                await window.dataManagerModule.refreshCache();
            }
            if (window.dataManagerModule.renderFileList) {
                window.dataManagerModule.renderFileList();
            }
        }

        return true;
    }

    async read() {
        if (!this.isOpen) return { success: false, atEnd: true };

        const fileName = this.getFileName();

        // Load records if not cached
        if (this.cursorRecords.length === 0) {
            this.cursorRecords = await this.storage.readAllRecords(fileName);
        }

        if (this.cursorPosition >= this.cursorRecords.length) {
            return { success: false, atEnd: true };
        }

        const record = this.cursorRecords[this.cursorPosition];
        this.cursorPosition++;
        this.lastReadRecord = record;
        return { success: true, atEnd: false, record };
    }

    async readKey(keyValue) {
        if (!this.isOpen || this.organization !== 'INDEXED') {
            return { success: false, invalidKey: true };
        }

        const fileName = this.getFileName();
        // Pad the key value to match the stored format (COBOL pads strings with spaces)
        const searchKey = keyValue?.toString();
        const record = await this.storage.readByKey(fileName, searchKey);

        if (record) {
            this.lastReadRecord = record;
            return { success: true, record };
        }
        return { success: false, invalidKey: true };
    }

    async write(record) {
        if (!this.isOpen || (this.mode !== 'OUTPUT' && this.mode !== 'I-O' && this.mode !== 'EXTEND')) {
            return { success: false };
        }

        const fileName = this.getFileName();

        // Normalize field names and trim the key field value for IndexedDB
        const keyField = this.recordKey?.toUpperCase().replace(/-/g, '_');
        const normalizedRecord = {};
        for (const [key, value] of Object.entries(record)) {
            const normalizedKey = key.toUpperCase().replace(/-/g, '_');
            // Trim the key field value to avoid padding issues
            if (normalizedKey === keyField && typeof value === 'string') {
                normalizedRecord[normalizedKey] = value.trim();
            } else {
                normalizedRecord[normalizedKey] = value;
            }
        }

        const result = await this.storage.writeRecord(fileName, normalizedRecord);

        // Invalidate cursor cache
        this.cursorRecords = [];

        return result;
    }

    async rewrite(record) {
        if (!this.isOpen || this.mode !== 'I-O') {
            return { success: false };
        }

        const fileName = this.getFileName();

        // Normalize field names and trim the key field value for IndexedDB
        const keyField = this.recordKey?.toUpperCase().replace(/-/g, '_');
        const normalizedRecord = {};
        for (const [key, value] of Object.entries(record)) {
            const normalizedKey = key.toUpperCase().replace(/-/g, '_');
            // Trim the key field value to match stored format
            if (normalizedKey === keyField && typeof value === 'string') {
                normalizedRecord[normalizedKey] = value.trim();
            } else {
                normalizedRecord[normalizedKey] = value;
            }
        }

        const result = await this.storage.rewriteRecord(fileName, normalizedRecord);

        // Invalidate cursor cache
        this.cursorRecords = [];

        return result;
    }

    async delete(keyValue) {
        if (!this.isOpen || this.mode !== 'I-O') {
            return { success: false };
        }

        const fileName = this.getFileName();

        // Use provided key or get from last read record
        let key = keyValue;
        if (!key && this.lastReadRecord && this.recordKey) {
            const keyField = this.recordKey.toUpperCase().replace(/-/g, '_');
            key = this.lastReadRecord[keyField];
        }

        if (!key) {
            return { success: false, invalidKey: true };
        }

        const result = await this.storage.deleteRecord(fileName, key);

        // Invalidate cursor cache
        this.cursorRecords = [];

        return result;
    }

    async start(keyValue, operator = '>=') {
        if (!this.isOpen || this.organization !== 'INDEXED') {
            return false;
        }

        const fileName = this.getFileName();

        // Get records from the start key
        this.cursorRecords = await this.storage.readFromKey(fileName, keyValue?.toString().trim(), operator);
        this.cursorPosition = 0;

        return this.cursorRecords.length > 0;
    }

    /**
     * Read all records from the file (for SORT/MERGE)
     */
    async readAll() {
        const fileName = this.getFileName();
        return await this.storage.readAllRecords(fileName);
    }

    /**
     * Clear all records from the file (for SORT/MERGE output)
     */
    async clear() {
        const fileName = this.getFileName();
        await this.storage.clearFile(fileName);
        this.cursorRecords = [];
        this.cursorPosition = 0;
        return true;
    }

    /**
     * Set the current record buffer (for RETURN statement)
     */
    setCurrentRecord(record) {
        this.lastReadRecord = { ...record };
    }
}

// Export storage configuration
export const USE_INDEXEDDB = true; // IndexedDB storage enabled

/**
 * Runtime class - manages program execution state
 */
class Runtime {
    constructor(callbacks = {}) {
        this.variables = new Map();
        this.conditionNames = new Map();  // 88 level condition names: name -> {parentVar, values}
        this.files = new Map();
        this.paragraphs = new Map();
        this.sections = new Map();
        this.callbacks = callbacks;
        this.running = false;
        this.waitingForInput = false;
        this.inputResolve = null;
        this.inputTarget = null;
    }

    // Variable operations
    getVariable(name) {
        const upperName = this.normalizeVarName(name);
        return this.variables.get(upperName);
    }

    normalizeVarName(name) {
        // COBOL uses hyphens, but we normalize to underscores for consistency
        return name.toUpperCase().replace(/-/g, '_');
    }

    setVariable(target, value, interpreter = null) {
        // Handle subscripted variables - target is an AST node
        if (target && typeof target === 'object' && target.type === 'SUBSCRIPT') {
            if (!interpreter) {
                console.error('Interpreter required for subscript assignment');
                return;
            }
            interpreter.setSubscriptedValue(target, value);
            return;
        }

        // Simple variable name
        const name = typeof target === 'object' && target.name ? target.name : target;
        const upperName = this.normalizeVarName(name);
        const variable = this.variables.get(upperName);
        if (variable) {
            variable.setValue(value);
        }
    }

    /**
     * Set raw value without reformatting (for values from storage)
     */
    setRawVariable(name, value) {
        const upperName = this.normalizeVarName(name);
        const variable = this.variables.get(upperName);
        if (variable) {
            variable.setRawValue(value);
        }
    }

    /**
     * Create an implicit index variable for INDEXED BY or SEARCH
     */
    createImplicitIndex(name, initialValue = 1) {
        const upperName = this.normalizeVarName(name);
        if (!this.variables.has(upperName)) {
            // Create a simple numeric variable for the index
            const indexVar = new Variable({
                name: name,
                level: 77,
                pic: '9(4)',
                value: { type: 'number', value: initialValue }
            });
            this.variables.set(upperName, indexVar);
        }
    }

    getValue(name) {
        const variable = this.getVariable(name);
        return variable ? variable.getValue() : null;
    }

    getNumericValue(name) {
        const variable = this.getVariable(name);
        return variable ? variable.getNumericValue() : 0;
    }

    getDisplayValue(name) {
        const variable = this.getVariable(name);
        return variable ? variable.getDisplayValue() : '';
    }

    /**
     * Check if a name is a condition name (88 level)
     */
    isConditionName(name) {
        const upperName = this.normalizeVarName(name);
        return this.conditionNames.has(upperName);
    }

    /**
     * Evaluate a condition name (88 level)
     * Returns true if the parent variable's value matches any of the condition's values
     */
    evaluateConditionName(name) {
        const upperName = this.normalizeVarName(name);
        const condition = this.conditionNames.get(upperName);
        if (!condition) return false;

        const parentVar = this.getVariable(condition.parentVar);
        if (!parentVar) return false;

        const currentValue = parentVar.getValue();
        const numericValue = parentVar.getNumericValue();
        const isNumeric = parentVar.picInfo.type === 'numeric';

        // Check each value/range in the condition
        for (const valueEntry of condition.values) {
            const condValue = this.getConditionValueAsString(valueEntry.value, parentVar);

            if (valueEntry.thru) {
                // Range check (THRU)
                const thruValue = this.getConditionValueAsString(valueEntry.thru, parentVar);

                if (isNumeric) {
                    const numCondValue = parseFloat(condValue) || 0;
                    const numThruValue = parseFloat(thruValue) || 0;
                    if (numericValue >= numCondValue && numericValue <= numThruValue) {
                        return true;
                    }
                } else {
                    // String range comparison
                    if (currentValue >= condValue && currentValue <= thruValue) {
                        return true;
                    }
                }
            } else {
                // Single value check
                if (isNumeric) {
                    const numCondValue = parseFloat(condValue) || 0;
                    if (numericValue === numCondValue) {
                        return true;
                    }
                } else {
                    if (currentValue.trim() === condValue.trim()) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Convert a condition value to string for comparison
     */
    getConditionValueAsString(condValue, parentVar) {
        if (!condValue) return '';

        switch (condValue.type) {
            case 'string':
                return condValue.value;
            case 'number':
                // Format number to match parent's PIC
                if (parentVar && parentVar.picInfo.type === 'numeric') {
                    return String(condValue.value).padStart(parentVar.picInfo.length, '0');
                }
                return String(condValue.value);
            case 'figurative':
                const len = parentVar ? parentVar.picInfo.length : 1;
                switch (condValue.value) {
                    case 'SPACES': return ' '.repeat(len);
                    case 'ZEROS': return '0'.repeat(len);
                    default: return '';
                }
            case 'boolean':
                return condValue.value ? 'TRUE' : 'FALSE';
            default:
                return String(condValue.value || '');
        }
    }

    // Output
    display(message) {
        if (this.callbacks.onDisplay) {
            this.callbacks.onDisplay(message);
        }
    }

    // Output with screen control options
    displayWithOptions(message, options) {
        if (this.callbacks.onDisplayWithOptions) {
            this.callbacks.onDisplayWithOptions(message, options);
        } else if (this.callbacks.onDisplay) {
            // Fallback to simple display if screen control not supported
            this.callbacks.onDisplay(message);
        }
    }

    // Input
    async accept(variableName) {
        this.waitingForInput = true;
        this.inputTarget = variableName;

        if (this.callbacks.onAccept) {
            this.callbacks.onAccept(variableName);
        }

        return new Promise((resolve) => {
            this.inputResolve = resolve;
        });
    }

    // Input with screen control options
    async acceptWithOptions(variableName, options) {
        this.waitingForInput = true;
        this.inputTarget = variableName;

        if (this.callbacks.onAcceptWithOptions) {
            this.callbacks.onAcceptWithOptions(variableName, options);
        } else if (this.callbacks.onAccept) {
            // Fallback to basic accept
            this.callbacks.onAccept(variableName);
        }

        return new Promise((resolve) => {
            this.inputResolve = resolve;
        });
    }

    provideInput(value) {
        if (this.waitingForInput && this.inputResolve) {
            this.setVariable(this.inputTarget, value);
            this.waitingForInput = false;
            this.inputTarget = null;
            const resolve = this.inputResolve;
            this.inputResolve = null;
            resolve(value);
        }
    }

    // File operations
    getFile(name) {
        return this.files.get(name?.toUpperCase());
    }

    /**
     * Get all field values for a record as an object
     * Used by RELEASE statement for SORT
     */
    getRecordValues(recordName) {
        const normalizedName = this.normalizeVarName(recordName);
        const record = {};

        // Look for variables that belong to this record
        for (const [varName, variable] of this.variables) {
            // Check if this variable is part of the record
            if (variable.parent === normalizedName || varName === normalizedName) {
                record[varName] = variable.getValue();
            }
        }

        return Object.keys(record).length > 0 ? record : null;
    }

    /**
     * Get the record definition (fields and their properties)
     * Used by RELEASE statement for SORT
     */
    getRecordDefinition(recordName) {
        const normalizedName = this.normalizeVarName(recordName);
        const fields = [];

        // Find all child variables of this record
        for (const [varName, variable] of this.variables) {
            if (variable.parent === normalizedName) {
                fields.push({
                    name: varName,
                    length: variable.picInfo?.length || 1,
                    type: variable.picInfo?.type || 'alphanumeric',
                });
            }
        }

        return fields.length > 0 ? { name: normalizedName, fields } : null;
    }
}

/**
 * Interpreter class
 */
export class Interpreter {
    constructor(ast, options = {}) {
        this.ast = ast;
        this.runtime = new Runtime(options.callbacks || {});
        this.dataManager = options.dataManager || {};
        this.executionStack = [];
        this.halted = false;
        this.loopCounter = 0; // For infinite loop detection

        // Step mode support
        this.stepMode = false;
        this.paused = false;
        this.stepResolve = null;
        this.currentStatement = null;
        this.onStep = options.callbacks?.onStep || null; // Callback for step notification
    }

    /**
     * Enable/disable step mode
     */
    setStepMode(enabled) {
        this.stepMode = enabled;
        if (!enabled && this.stepResolve) {
            // Resume if we were paused
            this.stepResolve();
            this.stepResolve = null;
        }
    }

    /**
     * Set breakpoints (line numbers where we should stop)
     */
    setBreakpoints(breakpointSet) {
        this.breakpoints = breakpointSet || new Set();
    }

    /**
     * Continue to next step (when paused)
     */
    stepNext() {
        if (this.stepResolve) {
            this.stepResolve();
            this.stepResolve = null;
        }
    }

    /**
     * Continue execution until next breakpoint
     */
    continueToBreakpoint() {
        this.runToBreakpoint = true;
        this.stepNext();
    }

    /**
     * Get current variables state for debugging
     */
    getVariablesState() {
        const state = {};
        for (const [name, variable] of this.runtime.variables) {
            state[name] = {
                value: variable.value,
                pic: variable.pic,
                level: variable.level
            };
        }
        return state;
    }

    /**
     * Wait for step if in step mode
     */
    async waitForStep(stmt) {
        if (!this.stepMode || this.halted) return;

        this.currentStatement = stmt;

        // If running to breakpoint, check if we hit one
        if (this.runToBreakpoint) {
            const lineNum = stmt.line;
            if (this.breakpoints && this.breakpoints.has(lineNum)) {
                // Hit a breakpoint - stop running
                this.runToBreakpoint = false;
            } else {
                // Continue without stopping - just notify and move on
                if (this.onStep) {
                    this.onStep({
                        statement: stmt,
                        line: stmt.line,
                        type: stmt.type,
                        variables: this.getVariablesState(),
                        isRunning: true  // Indicate we're not pausing
                    });
                }
                return;
            }
        }

        this.paused = true;

        // Notify UI of current statement
        if (this.onStep) {
            this.onStep({
                statement: stmt,
                line: stmt.line,
                type: stmt.type,
                variables: this.getVariablesState()
            });
        }

        // Wait for stepNext() to be called
        await new Promise(resolve => {
            this.stepResolve = resolve;
        });

        this.paused = false;
    }

    /**
     * Initialize the interpreter
     */
    initialize() {
        // Build symbol table from DATA DIVISION
        if (this.ast.data) {
            this.buildSymbolTable(this.ast.data);
        }

        // Build file table from ENVIRONMENT DIVISION
        if (this.ast.environment) {
            this.buildFileTable(this.ast.environment);
        }

        // Build paragraph/section table from PROCEDURE DIVISION
        if (this.ast.procedure) {
            this.buildProcedureTable(this.ast.procedure);
        }
    }

    /**
     * Build symbol table from DATA DIVISION
     */
    buildSymbolTable(dataDivision) {
        const processItems = (items, parent = null) => {
            const stack = [];
            let lastDataItem = null;  // Track last non-88 item for condition names

            for (const item of items) {
                if (!item.name) continue;

                // Handle 88 level condition names
                if (item.type === NodeType.CONDITION_NAME || item.level === 88) {
                    if (lastDataItem) {
                        // Register condition name with reference to parent variable
                        this.runtime.conditionNames.set(
                            this.runtime.normalizeVarName(item.name),
                            {
                                parentVar: lastDataItem.name,
                                values: item.values || []
                            }
                        );
                    }
                    continue;  // Don't add 88 levels to variable stack
                }

                const variable = new Variable(item);
                const isFiller = item.name === 'FILLER';

                if (!isFiller) {
                    lastDataItem = item;  // Track for 88 level attachment
                }

                // Handle hierarchy based on level numbers
                while (stack.length > 0 && stack[stack.length - 1].level >= item.level) {
                    stack.pop();
                }

                if (stack.length > 0) {
                    variable.parent = stack[stack.length - 1];
                    stack[stack.length - 1].children.push(variable);
                }

                // Only add named variables (not FILLER) to the symbol table
                if (!isFiller) {
                    this.runtime.variables.set(this.runtime.normalizeVarName(item.name), variable);
                }

                // FILLER can't have children, so only push named items to stack
                if (!isFiller) {
                    stack.push(variable);
                }

                // Recursively process children (for FD records with nested fields)
                if (item.children && item.children.length > 0) {
                    processItems(item.children);
                }
            }
        };

        if (dataDivision.workingStorage) {
            processItems(dataDivision.workingStorage);
        }

        if (dataDivision.fileSection) {
            for (const fd of dataDivision.fileSection) {
                processItems(fd.records);
            }
        }

        // Link REDEFINES variables to their targets
        for (const [name, variable] of this.runtime.variables) {
            if (variable.redefines) {
                const targetName = this.runtime.normalizeVarName(variable.redefines);
                const targetVar = this.runtime.variables.get(targetName);
                if (targetVar) {
                    variable.redefinesVar = targetVar;
                }
            }
        }

        // Create implicit index variables for INDEXED BY clauses
        for (const [name, variable] of this.runtime.variables) {
            if (variable.indexedBy && variable.indexedBy.length > 0) {
                for (const indexName of variable.indexedBy) {
                    this.runtime.createImplicitIndex(indexName, 1);
                }
            }
        }

        // Re-initialize group variables now that children are attached
        // This fixes the circular dependency where getInitialValue() was called
        // in constructor before children existed
        // Process in order from leaves to root to properly compose group values
        const groupVars = [];
        for (const [name, variable] of this.runtime.variables) {
            if (variable.children.length > 0 && !variable.pic) {
                groupVars.push(variable);
            }
        }
        // Sort by level descending (process innermost first)
        groupVars.sort((a, b) => b.level - a.level);

        for (const variable of groupVars) {
            // Build group value from children
            let groupValue = '';
            for (const child of variable.children) {
                const childVal = child.getValue();
                const childLen = child.getTotalLength();
                const occursCount = child.occursCount || 1;
                // Repeat child value for OCCURS
                for (let i = 0; i < occursCount; i++) {
                    groupValue += childVal.length === childLen ? childVal : childVal.padEnd(childLen, ' ').substring(0, childLen);
                }
            }
            variable.value = groupValue;
        }
    }

    /**
     * Build file table from ENVIRONMENT DIVISION
     */
    buildFileTable(envDivision) {
        if (!envDivision.fileControl) return;

        for (const select of envDivision.fileControl) {
            // Use IndexedDB-backed file if enabled
            const file = USE_INDEXEDDB
                ? new CobolFileIDB(select, storage)
                : new CobolFile(select, this.dataManager);
            this.runtime.files.set(select.fileName.toUpperCase(), file);
        }
    }

    /**
     * Build procedure table
     */
    buildProcedureTable(procedure) {
        // Index sections
        for (const section of procedure.sections || []) {
            this.runtime.sections.set(section.name.toUpperCase(), section);

            // Index paragraphs within sections
            for (const para of section.paragraphs || []) {
                this.runtime.paragraphs.set(para.name.toUpperCase(), para);
            }
        }

        // Index standalone paragraphs
        for (const para of procedure.paragraphs || []) {
            this.runtime.paragraphs.set(para.name.toUpperCase(), para);
        }
    }

    /**
     * Run the program
     */
    async run() {
        this.initialize();
        this.runtime.running = true;
        this.halted = false;

        try {
            if (this.ast.procedure) {
                await this.executeProcedure(this.ast.procedure);
            }
        } catch (e) {
            if (e.message !== 'STOP RUN') {
                throw e;
            }
        } finally {
            this.runtime.running = false;
        }
    }

    /**
     * Execute PROCEDURE DIVISION
     */
    async executeProcedure(procedure) {
        // Execute sections in order
        for (const section of procedure.sections || []) {
            await this.executeSection(section);
            if (this.halted) return;
        }

        // Execute standalone paragraphs
        for (const para of procedure.paragraphs || []) {
            await this.executeParagraph(para);
            if (this.halted) return;
        }

        // Execute loose statements
        for (const stmt of procedure.statements || []) {
            await this.executeStatement(stmt);
            if (this.halted) return;
        }
    }

    /**
     * Execute a section
     */
    async executeSection(section) {
        for (const para of section.paragraphs || []) {
            await this.executeParagraph(para);
            if (this.halted) return;
        }

        for (const stmt of section.statements || []) {
            await this.executeStatement(stmt);
            if (this.halted) return;
        }
    }

    /**
     * Execute a paragraph
     */
    async executeParagraph(paragraph) {
        for (const stmt of paragraph.statements || []) {
            await this.executeStatement(stmt);
            if (this.halted) return;
        }
    }

    /**
     * Execute a paragraph or section by name
     * Used by CALL and PERFORM statements
     */
    async executeParagraphByName(name) {
        const target = this.findParagraphOrSection(name);
        if (!target) {
            throw new Error(`Paragraphe ou section "${name}" non trouvé`);
        }

        // If it's a section, execute all its paragraphs
        if (target.paragraphs) {
            for (const para of target.paragraphs) {
                await this.executeParagraph(para);
                if (this.halted) return;
            }
        } else {
            // It's a paragraph
            await this.executeParagraph(target);
        }
    }

    /**
     * Execute a statement
     */
    async executeStatement(stmt) {
        if (!stmt || this.halted) return;

        // Wait for step in debug mode
        await this.waitForStep(stmt);

        switch (stmt.type) {
            case NodeType.DISPLAY:
                await this.executeDisplay(stmt);
                break;
            case NodeType.ACCEPT:
                await this.executeAccept(stmt);
                break;
            case NodeType.MOVE:
                this.executeMove(stmt);
                break;
            case NodeType.ADD:
                this.executeAdd(stmt);
                break;
            case NodeType.SUBTRACT:
                this.executeSubtract(stmt);
                break;
            case NodeType.MULTIPLY:
                this.executeMultiply(stmt);
                break;
            case NodeType.DIVIDE:
                this.executeDivide(stmt);
                break;
            case NodeType.COMPUTE:
                this.executeCompute(stmt);
                break;
            case NodeType.IF:
                await this.executeIf(stmt);
                break;
            case NodeType.EVALUATE:
                await this.executeEvaluate(stmt);
                break;
            case NodeType.PERFORM:
                await this.executePerform(stmt);
                break;
            case NodeType.GO_TO:
                await this.executeGoTo(stmt);
                break;
            case NodeType.STOP_RUN:
                this.halted = true;
                throw new Error('STOP RUN');
            case NodeType.EXIT:
                // EXIT just returns from current paragraph
                break;
            case NodeType.INITIALIZE:
                this.executeInitialize(stmt);
                break;
            case NodeType.OPEN:
                await this.executeOpen(stmt);
                break;
            case NodeType.CLOSE:
                await this.executeClose(stmt);
                break;
            case NodeType.READ:
                await this.executeRead(stmt);
                break;
            case NodeType.WRITE:
                await this.executeWrite(stmt);
                break;
            case NodeType.REWRITE:
                await this.executeRewrite(stmt);
                break;
            case NodeType.DELETE:
                await this.executeDelete(stmt);
                break;
            case NodeType.START:
                await this.executeStart(stmt);
                break;
            case NodeType.SORT:
                await this.executeSort(stmt);
                break;
            case NodeType.MERGE:
                await this.executeMerge(stmt);
                break;
            case NodeType.RELEASE:
                this.executeRelease(stmt);
                break;
            case NodeType.RETURN:
                await this.executeReturn(stmt);
                break;
            case NodeType.STRING:
                this.executeString(stmt);
                break;
            case NodeType.UNSTRING:
                this.executeUnstring(stmt);
                break;
            case NodeType.INSPECT:
                this.executeInspect(stmt);
                break;
            case NodeType.SEARCH:
                await this.executeSearch(stmt);
                break;
            case NodeType.SET:
                this.executeSet(stmt);
                break;
            case NodeType.CALL:
                await this.executeCall(stmt);
                break;
            case NodeType.CANCEL:
                this.executeCancel(stmt);
                break;
        }
    }

    /**
     * Execute DISPLAY statement with screen control support
     */
    async executeDisplay(stmt) {
        const parts = [];

        for (const item of stmt.items) {
            // For identifiers, use getDisplayValue to show decimals properly
            if (item.type === NodeType.IDENTIFIER) {
                parts.push(this.runtime.getDisplayValue(item.name));
            } else {
                parts.push(this.evaluateExpression(item));
            }
        }

        const message = parts.join('');

        // Check if screen control options are present
        const hasScreenControl = stmt.line !== null || stmt.column !== null ||
            stmt.erase !== null || stmt.highlight || stmt.blink ||
            stmt.reverseVideo || stmt.underline || stmt.bell ||
            stmt.foregroundColor !== null || stmt.backgroundColor !== null;

        if (hasScreenControl) {
            // Pass screen control options to runtime
            this.runtime.displayWithOptions(message, {
                line: stmt.line,
                column: stmt.column,
                erase: stmt.erase,
                highlight: stmt.highlight,
                blink: stmt.blink,
                reverseVideo: stmt.reverseVideo,
                underline: stmt.underline,
                bell: stmt.bell,
                foregroundColor: stmt.foregroundColor,
                backgroundColor: stmt.backgroundColor,
                noAdvancing: stmt.noAdvancing
            });
        } else {
            this.runtime.display(message);
        }
    }

    /**
     * Execute ACCEPT statement
     */
    async executeAccept(stmt) {
        // Check if screen control options are present
        const hasScreenControl = stmt.line !== null || stmt.column !== null ||
            stmt.highlight || stmt.blink || stmt.reverseVideo || stmt.underline ||
            stmt.secure || stmt.required || stmt.full || stmt.auto ||
            stmt.foregroundColor !== null || stmt.backgroundColor !== null;

        if (hasScreenControl) {
            const options = {
                line: stmt.line,
                column: stmt.column,
                highlight: stmt.highlight || false,
                blink: stmt.blink || false,
                reverseVideo: stmt.reverseVideo || false,
                underline: stmt.underline || false,
                secure: stmt.secure || false,
                required: stmt.required || false,
                full: stmt.full || false,
                auto: stmt.auto || false,
                foregroundColor: stmt.foregroundColor,
                backgroundColor: stmt.backgroundColor,
                size: stmt.size,
                cursor: stmt.cursor,
            };
            await this.runtime.acceptWithOptions(stmt.target, options);
        } else {
            await this.runtime.accept(stmt.target);
        }
        // Reset loop counter after user input (prevents false infinite loop detection)
        this.loopCounter = 0;
    }

    /**
     * Execute MOVE statement
     */
    executeMove(stmt) {
        const value = this.evaluateExpression(stmt.source);

        for (const target of stmt.targets) {
            this.runtime.setVariable(target, value, this);
        }
    }

    /**
     * Execute ADD statement
     */
    executeAdd(stmt) {
        let sum = 0;

        for (const operand of stmt.operands) {
            sum += this.evaluateNumeric(operand);
        }

        if (stmt.to) {
            sum += this.runtime.getNumericValue(stmt.to);
        }

        const target = stmt.giving || stmt.to;
        if (target) {
            this.runtime.setVariable(target, sum);
        }
    }

    /**
     * Execute SUBTRACT statement
     */
    executeSubtract(stmt) {
        let total = 0;

        for (const operand of stmt.operands) {
            total += this.evaluateNumeric(operand);
        }

        let result;
        if (stmt.from) {
            result = this.runtime.getNumericValue(stmt.from) - total;
        } else {
            result = -total;
        }

        const target = stmt.giving || stmt.from;
        if (target) {
            this.runtime.setVariable(target, result);
        }
    }

    /**
     * Execute MULTIPLY statement
     */
    executeMultiply(stmt) {
        const val1 = this.evaluateNumeric(stmt.operand1);
        const val2 = this.evaluateNumeric(stmt.operand2);
        const result = val1 * val2;

        const target = stmt.giving || (stmt.operand2?.name);
        if (target) {
            this.runtime.setVariable(target, result);
        }
    }

    /**
     * Execute DIVIDE statement
     */
    executeDivide(stmt) {
        const dividend = this.evaluateNumeric(stmt.dividend);
        const divisor = this.evaluateNumeric(stmt.divisor);

        if (divisor === 0) {
            throw new Error('Division by zero');
        }

        const quotient = Math.floor(dividend / divisor);
        const remainder = dividend % divisor;

        if (stmt.giving) {
            this.runtime.setVariable(stmt.giving, quotient);
        }
        if (stmt.remainder) {
            this.runtime.setVariable(stmt.remainder, remainder);
        }
    }

    /**
     * Execute COMPUTE statement
     */
    executeCompute(stmt) {
        const value = this.evaluateArithmetic(stmt.expression);
        this.runtime.setVariable(stmt.target, value);
    }

    /**
     * Execute IF statement
     */
    async executeIf(stmt) {
        const condition = this.evaluateCondition(stmt.condition);

        if (condition) {
            for (const s of stmt.thenStatements) {
                await this.executeStatement(s);
                if (this.halted) return;
            }
        } else {
            for (const s of stmt.elseStatements) {
                await this.executeStatement(s);
                if (this.halted) return;
            }
        }
    }

    /**
     * Execute EVALUATE statement
     */
    async executeEvaluate(stmt) {
        const subject = this.evaluateExpression(stmt.subject);

        for (const whenClause of stmt.whenClauses) {
            for (const value of whenClause.values) {
                const whenValue = this.evaluateExpression(value);
                // Try direct equality first
                if (subject === whenValue) {
                    for (const s of whenClause.statements) {
                        await this.executeStatement(s);
                        if (this.halted) return;
                    }
                    return;
                }
                // Try string comparison
                if (String(subject).trim() === String(whenValue).trim()) {
                    for (const s of whenClause.statements) {
                        await this.executeStatement(s);
                        if (this.halted) return;
                    }
                    return;
                }
                // Try numeric comparison (important for COBOL numeric variables stored as strings)
                const numSubject = parseFloat(subject);
                const numWhenValue = parseFloat(whenValue);
                if (!isNaN(numSubject) && !isNaN(numWhenValue) && numSubject === numWhenValue) {
                    for (const s of whenClause.statements) {
                        await this.executeStatement(s);
                        if (this.halted) return;
                    }
                    return;
                }
            }
        }

        // WHEN OTHER
        for (const s of stmt.whenOther) {
            await this.executeStatement(s);
            if (this.halted) return;
        }
    }

    /**
     * Execute PERFORM statement
     */
    async executePerform(stmt) {
        // PERFORM n TIMES
        if (stmt.times !== null && stmt.times !== undefined) {
            const times = typeof stmt.times === 'number' ? stmt.times : this.evaluateNumeric(stmt.times);

            for (let i = 0; i < times; i++) {
                if (stmt.target) {
                    await this.performParagraph(stmt.target, stmt.thru);
                } else {
                    for (const s of stmt.inline) {
                        await this.executeStatement(s);
                        if (this.halted) return;
                    }
                }
                if (this.halted) return;
            }
        }
        // PERFORM UNTIL
        else if (stmt.until) {
            const maxLoops = 100000; // Safety limit
            while (!this.evaluateCondition(stmt.until)) {
                if (++this.loopCounter > maxLoops) {
                    this.runtime.display('*** BOUCLE INFINIE DETECTEE - ARRET ***');
                    this.halted = true;
                    return;
                }
                if (stmt.target) {
                    await this.performParagraph(stmt.target, stmt.thru);
                } else {
                    for (const s of stmt.inline) {
                        await this.executeStatement(s);
                        if (this.halted) return;
                    }
                }
                if (this.halted) return;
            }
        }
        // PERFORM VARYING
        else if (stmt.varying) {
            const { variable, from, by, until } = stmt.varying;

            // Initialize variable
            const fromValue = this.evaluateNumeric(from);
            this.runtime.setVariable(variable, fromValue);

            const byValue = by ? this.evaluateNumeric(by) : 1;

            const maxLoops = 100000; // Safety limit
            while (!this.evaluateCondition(until)) {
                if (++this.loopCounter > maxLoops) {
                    this.runtime.display('*** BOUCLE INFINIE DETECTEE - ARRET ***');
                    this.halted = true;
                    return;
                }
                if (stmt.target) {
                    await this.performParagraph(stmt.target, stmt.thru);
                } else {
                    for (const s of stmt.inline) {
                        await this.executeStatement(s);
                        if (this.halted) return;
                    }
                }
                if (this.halted) return;

                // Increment variable
                const current = this.runtime.getNumericValue(variable);
                this.runtime.setVariable(variable, current + byValue);
            }
        }
        // Simple PERFORM paragraph
        else if (stmt.target) {
            await this.performParagraph(stmt.target, stmt.thru);
        }
        // Inline PERFORM
        else {
            for (const s of stmt.inline) {
                await this.executeStatement(s);
                if (this.halted) return;
            }
        }
    }

    /**
     * Perform a paragraph or range of paragraphs
     */
    async performParagraph(name, thru = null) {
        const para = this.runtime.paragraphs.get(name.toUpperCase());
        if (para) {
            await this.executeParagraph(para);
        }

        // TODO: Handle THRU clause properly
        if (thru) {
            const thruPara = this.runtime.paragraphs.get(thru.toUpperCase());
            if (thruPara) {
                await this.executeParagraph(thruPara);
            }
        }
    }

    /**
     * Execute GO TO statement
     */
    async executeGoTo(stmt) {
        // GO TO is tricky - for now, just perform the target
        if (stmt.target) {
            await this.performParagraph(stmt.target);
        }
    }

    /**
     * Execute INITIALIZE statement
     */
    executeInitialize(stmt) {
        for (const target of stmt.targets) {
            const variable = this.runtime.getVariable(target);
            if (variable) {
                variable.initialize();
            }
        }
    }

    /**
     * Execute OPEN statement
     */
    async executeOpen(stmt) {
        for (const fileSpec of stmt.files) {
            const file = this.runtime.getFile(fileSpec.name);
            if (file) {
                await file.open(fileSpec.mode);
            }
        }
    }

    /**
     * Execute CLOSE statement
     */
    async executeClose(stmt) {
        for (const fileName of stmt.files) {
            const file = this.runtime.getFile(fileName);
            if (file) {
                await file.close();
            }
        }
    }

    /**
     * Execute READ statement
     */
    async executeRead(stmt) {
        const file = this.runtime.getFile(stmt.file);

        // If file doesn't exist, treat as AT END or INVALID KEY
        if (!file) {
            // Try with common name transformations
            const altNames = [
                stmt.file,
                stmt.file?.toUpperCase(),
                stmt.file?.toLowerCase()
            ];

            if (stmt.atEnd && stmt.atEnd.length > 0) {
                for (const s of stmt.atEnd) {
                    await this.executeStatement(s);
                }
            } else if (stmt.invalidKey && stmt.invalidKey.length > 0) {
                for (const s of stmt.invalidKey) {
                    await this.executeStatement(s);
                }
            }
            return;
        }

        let result;
        if (stmt.key) {
            // Explicit key specified (e.g., READ file KEY IS var)
            const keyValue = this.runtime.getValue(stmt.key);
            result = await file.readKey(keyValue);
        } else if (stmt.next) {
            // READ NEXT - sequential read
            result = await file.read();
        } else if (file.organization === 'INDEXED' && file.recordKey) {
            // For indexed files without explicit key, use the record key field value
            // Keep the original name format (with hyphens) for variable lookup
            const keyField = file.recordKey.toUpperCase();
            const keyValue = this.runtime.getValue(keyField);
            if (keyValue && keyValue.toString().trim()) {
                result = await file.readKey(keyValue);
            } else {
                result = await file.read();
            }
        } else {
            // Sequential read
            result = await file.read();
        }

        if (result.success && result.record) {
            // Copy record fields to variables (use setRawVariable to avoid re-formatting already-formatted values)
            for (const [key, value] of Object.entries(result.record)) {
                this.runtime.setRawVariable(key, value);
            }

            // Execute NOT AT END / NOT INVALID KEY statements
            if (stmt.notAtEnd && stmt.notAtEnd.length > 0) {
                for (const s of stmt.notAtEnd) {
                    await this.executeStatement(s);
                }
            } else if (stmt.notInvalidKey && stmt.notInvalidKey.length > 0) {
                for (const s of stmt.notInvalidKey) {
                    await this.executeStatement(s);
                }
            }
        } else if (result.atEnd) {
            // Execute AT END statements
            for (const s of stmt.atEnd || []) {
                await this.executeStatement(s);
            }
        } else if (result.invalidKey || !result.success) {
            // Execute INVALID KEY statements (for indexed files)
            // Also handle general failure as INVALID KEY
            if (stmt.invalidKey && stmt.invalidKey.length > 0) {
                for (const s of stmt.invalidKey) {
                    await this.executeStatement(s);
                }
            } else {
            }
        } else {
        }
    }

    /**
     * Execute WRITE statement
     */
    async executeWrite(stmt) {

        // Find the file that owns this record
        let targetFile = null;
        let recordDef = null;

        const recordName = stmt.record?.toUpperCase();

        // AST stores FD in data.fileSection (not data.files)
        const fileSection = this.ast.data?.fileSection || [];

        // Look for the record definition in the AST file section
        for (const fileDef of fileSection) {
            // Check in records array (FD can have multiple 01 level records)
            for (const rec of fileDef.records || []) {
                if (rec?.name?.toUpperCase() === recordName) {
                    targetFile = this.runtime.getFile(fileDef.fileName);
                    recordDef = rec;
                    break;
                }
            }
            if (targetFile) break;
        }

        // If not found by record name, try to match by file
        if (!targetFile) {
            for (const [fileName, file] of this.runtime.files) {
                if (file.isOpen && (file.mode === 'OUTPUT' || file.mode === 'I-O' || file.mode === 'EXTEND')) {
                    targetFile = file;
                    break;
                }
            }
        }

        if (!targetFile) {
            return;
        }

        // Build record from the record's fields defined in AST
        const record = {};


        // Get field names from record definition in AST
        const recordFields = new Set();
        if (recordDef?.children && recordDef.children.length > 0) {
            const collectFields = (items) => {
                for (const item of items) {
                    if (item.name && item.name !== 'FILLER') {
                        recordFields.add(item.name.toUpperCase().replace(/-/g, '_'));
                    }
                    if (item.children && item.children.length > 0) {
                        collectFields(item.children);
                    }
                }
            };
            collectFields(recordDef.children);
        }

        // Use field definitions from AST
        for (const fieldName of recordFields) {
            const variable = this.runtime.variables.get(fieldName);
            if (variable) {
                record[fieldName] = variable.getValue();
            }
        }

        // If no fields found from AST, the record structure wasn't parsed correctly
        if (Object.keys(record).length === 0) {
            
        }

        const result = await targetFile.write(record);

        // Refresh data manager UI in real-time
        if (typeof window !== 'undefined' && window.dataManagerModule?.renderFileList) {
            window.dataManagerModule.renderFileList();
        }
    }

    /**
     * Execute REWRITE statement
     */
    async executeRewrite(stmt) {
        // Find the file that owns this record (same logic as executeWrite)
        let targetFile = null;
        let recordDef = null;

        const recordName = stmt.record?.toUpperCase();
        const fileSection = this.ast.data?.fileSection || [];

        for (const fileDef of fileSection) {
            for (const rec of fileDef.records || []) {
                if (rec?.name?.toUpperCase() === recordName) {
                    targetFile = this.runtime.getFile(fileDef.fileName);
                    recordDef = rec;
                    break;
                }
            }
            if (targetFile) break;
        }

        if (!targetFile) {
            // Fallback: find first file open in I-O mode
            for (const [, file] of this.runtime.files) {
                if (file.isOpen && file.mode === 'I-O') {
                    targetFile = file;
                    break;
                }
            }
        }

        if (!targetFile) {
            
            return;
        }

        // Build record from the record's fields defined in AST
        const record = {};
        const recordFields = new Set();

        if (recordDef?.children && recordDef.children.length > 0) {
            const collectFields = (items) => {
                for (const item of items) {
                    if (item.name && item.name !== 'FILLER') {
                        recordFields.add(item.name.toUpperCase().replace(/-/g, '_'));
                    }
                    if (item.children && item.children.length > 0) {
                        collectFields(item.children);
                    }
                }
            };
            collectFields(recordDef.children);
        }

        for (const fieldName of recordFields) {
            const variable = this.runtime.variables.get(fieldName);
            if (variable) {
                record[fieldName] = variable.getValue();
            }
        }

        await targetFile.rewrite(record);
    }

    /**
     * Execute DELETE statement
     */
    async executeDelete(stmt) {
        const file = this.runtime.getFile(stmt.file);
        if (!file) return;

        const result = await file.delete();
        const success = result?.success;
        if (!success && stmt.invalidKey) {
            for (const s of stmt.invalidKey) {
                await this.executeStatement(s);
            }
        } else if (success && stmt.notInvalidKey) {
            for (const s of stmt.notInvalidKey) {
                await this.executeStatement(s);
            }
        }
    }

    /**
     * Execute START statement - position cursor in indexed file
     */
    async executeStart(stmt) {
        const file = this.runtime.getFile(stmt.file);
        if (!file) {
            if (stmt.invalidKey) {
                for (const s of stmt.invalidKey) {
                    await this.executeStatement(s);
                }
            }
            return;
        }

        // Get the key value to search for
        let keyValue = '';
        if (stmt.key) {
            keyValue = this.runtime.getValue(stmt.key) || '';
        }

        // Position the file cursor
        const success = await file.start(keyValue, stmt.operator);

        if (!success && stmt.invalidKey) {
            for (const s of stmt.invalidKey) {
                await this.executeStatement(s);
            }
        } else if (success && stmt.notInvalidKey) {
            for (const s of stmt.notInvalidKey) {
                await this.executeStatement(s);
            }
        }
    }

    /**
     * Execute SORT statement
     * Sorts records from input file(s) and writes to output file(s)
     */
    async executeSort(stmt) {
        // Initialize sort work area
        this.sortWorkArea = [];
        this.sortKeys = stmt.keys || [];
        this.sortFile = stmt.sortFile;

        // Get input records
        if (stmt.using && stmt.using.length > 0) {
            // USING clause: read all records from input files
            for (const inputFile of stmt.using) {
                const file = this.runtime.getFile(inputFile);
                if (file) {
                    const records = await file.readAll();
                    if (records) {
                        this.sortWorkArea.push(...records);
                    }
                }
            }
        } else if (stmt.inputProcedure) {
            // INPUT PROCEDURE: execute procedure that uses RELEASE
            await this.executeNamedProcedure(stmt.inputProcedure);
        }

        // Sort the records
        this.sortWorkArea.sort((a, b) => {
            for (const keyDef of this.sortKeys) {
                const keyName = keyDef.key.toUpperCase().replace(/-/g, '_');
                const aVal = a[keyName] ?? '';
                const bVal = b[keyName] ?? '';

                let comparison;
                // Try numeric comparison first
                const aNum = parseFloat(aVal);
                const bNum = parseFloat(bVal);
                if (!isNaN(aNum) && !isNaN(bNum)) {
                    comparison = aNum - bNum;
                } else {
                    // String comparison
                    comparison = String(aVal).localeCompare(String(bVal));
                }

                if (comparison !== 0) {
                    return keyDef.order === 'DESCENDING' ? -comparison : comparison;
                }
            }
            return 0;
        });

        // Output sorted records
        if (stmt.giving && stmt.giving.length > 0) {
            // GIVING clause: write all records to output files
            for (const outputFile of stmt.giving) {
                const file = this.runtime.getFile(outputFile);
                if (file) {
                    // Clear the output file first
                    await file.clear();
                    // Write sorted records
                    for (const record of this.sortWorkArea) {
                        await file.write(record);
                    }
                }
            }
        } else if (stmt.outputProcedure) {
            // OUTPUT PROCEDURE: execute procedure that uses RETURN
            this.sortOutputIndex = 0;
            await this.executeNamedProcedure(stmt.outputProcedure);
        }

        // Clear work area
        this.sortWorkArea = [];
        this.sortKeys = [];
        this.sortOutputIndex = 0;
    }

    /**
     * Execute MERGE statement
     * Merges records from multiple pre-sorted input files
     */
    async executeMerge(stmt) {
        // Initialize merge work area
        this.sortWorkArea = [];
        this.sortKeys = stmt.keys || [];
        this.sortFile = stmt.mergeFile;

        // Read all records from all input files
        if (stmt.using && stmt.using.length > 0) {
            for (const inputFile of stmt.using) {
                const file = this.runtime.getFile(inputFile);
                if (file) {
                    const records = await file.readAll();
                    if (records) {
                        this.sortWorkArea.push(...records);
                    }
                }
            }
        }

        // Merge-sort the records (assuming input files are pre-sorted)
        this.sortWorkArea.sort((a, b) => {
            for (const keyDef of this.sortKeys) {
                const keyName = keyDef.key.toUpperCase().replace(/-/g, '_');
                const aVal = a[keyName] ?? '';
                const bVal = b[keyName] ?? '';

                let comparison;
                const aNum = parseFloat(aVal);
                const bNum = parseFloat(bVal);
                if (!isNaN(aNum) && !isNaN(bNum)) {
                    comparison = aNum - bNum;
                } else {
                    comparison = String(aVal).localeCompare(String(bVal));
                }

                if (comparison !== 0) {
                    return keyDef.order === 'DESCENDING' ? -comparison : comparison;
                }
            }
            return 0;
        });

        // Output merged records
        if (stmt.giving && stmt.giving.length > 0) {
            for (const outputFile of stmt.giving) {
                const file = this.runtime.getFile(outputFile);
                if (file) {
                    await file.clear();
                    for (const record of this.sortWorkArea) {
                        await file.write(record);
                    }
                }
            }
        } else if (stmt.outputProcedure) {
            this.sortOutputIndex = 0;
            await this.executeNamedProcedure(stmt.outputProcedure);
        }

        // Clear work area
        this.sortWorkArea = [];
        this.sortKeys = [];
        this.sortOutputIndex = 0;
    }

    /**
     * Execute RELEASE statement
     * Passes a record to the sort process (used in INPUT PROCEDURE)
     */
    executeRelease(stmt) {
        // Get the record to release
        const recordName = stmt.record;

        if (stmt.from) {
            // RELEASE record FROM identifier
            const value = this.runtime.getValue(stmt.from);
            // Build a record object from the value
            const record = {};
            const recordDef = this.runtime.getRecordDefinition(recordName);
            if (recordDef && recordDef.fields) {
                // Map value to record fields
                let offset = 0;
                for (const field of recordDef.fields) {
                    const fieldName = field.name.toUpperCase().replace(/-/g, '_');
                    const len = field.length || 1;
                    record[fieldName] = String(value).substr(offset, len);
                    offset += len;
                }
            } else {
                // Simple case: use record name as single field
                record[recordName.toUpperCase().replace(/-/g, '_')] = value;
            }
            this.sortWorkArea.push(record);
        } else {
            // RELEASE record - get current record values
            const recordValues = this.runtime.getRecordValues(recordName);
            if (recordValues) {
                this.sortWorkArea.push({ ...recordValues });
            }
        }
    }

    /**
     * Execute RETURN statement
     * Retrieves the next sorted record (used in OUTPUT PROCEDURE)
     */
    async executeReturn(stmt) {
        // Check if there are more records
        if (!this.sortWorkArea || this.sortOutputIndex >= this.sortWorkArea.length) {
            // AT END condition
            if (stmt.atEnd) {
                for (const s of stmt.atEnd) {
                    await this.executeStatement(s);
                }
            }
            return;
        }

        // Get the next sorted record
        const record = this.sortWorkArea[this.sortOutputIndex++];

        if (stmt.into) {
            // RETURN file INTO identifier - copy record to identifier
            const recordValues = Object.values(record).join('');
            this.runtime.setVariable(stmt.into, recordValues);
        } else {
            // Update the file's record area
            const file = this.runtime.getFile(stmt.file);
            if (file) {
                file.setCurrentRecord(record);
            }
        }

        // NOT AT END condition
        if (stmt.notAtEnd) {
            for (const s of stmt.notAtEnd) {
                await this.executeStatement(s);
            }
        }
    }

    /**
     * Execute a named procedure (section or paragraph) - used by SORT/MERGE
     */
    async executeNamedProcedure(procName) {
        // Find the procedure in the AST
        const procedures = this.ast.procedure?.sections || [];

        for (const section of procedures) {
            if (section.name?.toUpperCase() === procName?.toUpperCase()) {
                // Execute all paragraphs in this section
                for (const para of section.paragraphs || []) {
                    for (const stmt of para.statements || []) {
                        await this.executeStatement(stmt);
                    }
                }
                return;
            }

            // Check paragraphs within sections
            for (const para of section.paragraphs || []) {
                if (para.name?.toUpperCase() === procName?.toUpperCase()) {
                    for (const stmt of para.statements || []) {
                        await this.executeStatement(stmt);
                    }
                    return;
                }
            }
        }

        // Also check standalone paragraphs
        for (const para of this.ast.procedure?.paragraphs || []) {
            if (para.name?.toUpperCase() === procName?.toUpperCase()) {
                for (const stmt of para.statements || []) {
                    await this.executeStatement(stmt);
                }
                return;
            }
        }
    }

    /**
     * Evaluate an expression
     */
    evaluateExpression(expr) {
        if (!expr) return '';

        switch (expr.type) {
            case NodeType.LITERAL:
                return expr.value;

            case NodeType.IDENTIFIER:
                return this.runtime.getValue(expr.name) || '';

            case NodeType.SUBSCRIPT:
                return this.evaluateSubscript(expr);

            case NodeType.REFERENCE_MOD:
                return this.evaluateReferenceMod(expr);

            case NodeType.FIGURATIVE_CONSTANT:
                switch (expr.value) {
                    case 'SPACES': return ' ';
                    case 'ZEROS': return '0';
                    case 'LOW-VALUES': return '\x00';
                    case 'HIGH-VALUES': return '\xFF';
                    case 'QUOTES': return '"';
                    default: return '';
                }

            case NodeType.BINARY_EXPR:
                return this.evaluateArithmetic(expr);

            case NodeType.UNARY_EXPR:
                return this.evaluateArithmetic(expr);

            default:
                return '';
        }
    }

    /**
     * Evaluate subscripted variable (e.g., TABLE(1) or TABLE(I))
     */
    evaluateSubscript(expr) {
        // Get subscript values
        const subscripts = expr.subscripts.map(s => this.evaluateNumeric(s));
        const baseName = expr.name;

        // Get the base variable
        const baseVar = this.runtime.getVariable(baseName);
        if (!baseVar) {
            return '';
        }

        // Find all OCCURS levels from baseVar up to root
        const occursChain = this.findOccursChain(baseVar);
        if (occursChain.length === 0) {
            // No OCCURS found - just return the value
            return baseVar.getValue();
        }

        // Get the root storage (considering REDEFINES at the top level)
        const topOccurs = occursChain[0];
        const redefinesRoot = topOccurs.findRedefinesRoot();
        let fullStorage;
        if (redefinesRoot) {
            fullStorage = redefinesRoot.redefinesVar.getValue();
        } else if (topOccurs.parent) {
            fullStorage = topOccurs.parent.getValue();
        } else {
            fullStorage = topOccurs.getValue();
        }

        // Calculate total offset by walking through each OCCURS dimension
        let offset = 0;
        for (let i = 0; i < occursChain.length; i++) {
            const occursVar = occursChain[i];
            const subscript = subscripts[i] || 1;
            const elementSize = occursVar.getTotalLength();
            offset += (subscript - 1) * elementSize;
        }

        // If accessing a child of the innermost OCCURS, add its offset within one element
        const innermostOccurs = occursChain[occursChain.length - 1];
        if (baseVar !== innermostOccurs) {
            const childOffset = this.calculateChildOffset(baseVar, innermostOccurs);
            offset += childOffset;
        }

        // Return the extracted value
        const len = baseVar.getTotalLength();
        return fullStorage.substring(offset, offset + len);
    }

    /**
     * Find all OCCURS variables in the chain from variable up to root
     * Returns array ordered from outermost to innermost OCCURS
     */
    findOccursChain(variable) {
        const chain = [];
        let current = variable;

        // First check if the variable itself has OCCURS
        if (current.occursCount) {
            chain.unshift(current);
        }

        // Walk up the parent chain
        while (current.parent) {
            current = current.parent;
            if (current.occursCount) {
                chain.unshift(current);
            }
        }

        return chain;
    }

    /**
     * Set value to a subscripted variable (e.g., TABLE(1, 2))
     */
    setSubscriptedValue(expr, value) {
        const subscripts = expr.subscripts.map(s => this.evaluateNumeric(s));
        const baseName = expr.name;

        const baseVar = this.runtime.getVariable(baseName);
        if (!baseVar) return;

        // Find all OCCURS levels
        const occursChain = this.findOccursChain(baseVar);
        if (occursChain.length === 0) {
            // No OCCURS - just set the value normally
            baseVar.setValue(value);
            return;
        }

        // Get the storage holder (parent of outermost OCCURS or REDEFINES target)
        const topOccurs = occursChain[0];
        const redefinesRoot = topOccurs.findRedefinesRoot();
        let storageVar;
        if (redefinesRoot) {
            storageVar = redefinesRoot.redefinesVar;
        } else if (topOccurs.parent) {
            storageVar = topOccurs.parent;
        } else {
            storageVar = topOccurs;
        }

        // Get current full storage
        let fullStorage = storageVar.getValue();

        // Calculate offset
        let offset = 0;
        for (let i = 0; i < occursChain.length; i++) {
            const occursVar = occursChain[i];
            const subscript = subscripts[i] || 1;
            const elementSize = occursVar.getTotalLength();
            offset += (subscript - 1) * elementSize;
        }

        // Add child offset if accessing a child of innermost OCCURS
        const innermostOccurs = occursChain[occursChain.length - 1];
        if (baseVar !== innermostOccurs) {
            const childOffset = this.calculateChildOffset(baseVar, innermostOccurs);
            offset += childOffset;
        }

        // Format the value according to the target's PIC
        const formattedValue = formatValue(value, baseVar.pic);
        const len = baseVar.picInfo.length;

        // Replace the portion of storage
        const newStorage = fullStorage.substring(0, offset) +
                          formattedValue +
                          fullStorage.substring(offset + len);

        // Set the new storage value
        storageVar.setRawValue(newStorage);
    }

    /**
     * Find the parent variable that has OCCURS
     */
    findOccursParent(variable) {
        // Check the variable's parent chain for OCCURS
        let current = variable.parent;
        while (current) {
            // Check if any child of current is marked with occurs (based on AST)
            // For now, we detect OCCURS by looking at parent structure
            if (current.children.length > 0) {
                // If all siblings have same structure and parent has multiple identical children
                // it's likely an OCCURS - but we need the actual occurs property
                // Let's check if the variable was defined with occurs
                if (current.occursCount) {
                    return current;
                }
            }
            current = current.parent;
        }

        // Also check the variable itself
        if (variable.occursCount) {
            return variable;
        }

        // Fallback: check if parent has identical children (heuristic for OCCURS)
        if (variable.parent && variable.parent.children.length > 1) {
            const firstChild = variable.parent.children[0];
            const allSame = variable.parent.children.every(c =>
                c.name === firstChild.name && c.getTotalLength() === firstChild.getTotalLength()
            );
            if (allSame && firstChild.name === variable.name) {
                return variable;
            }
        }

        return null;
    }

    /**
     * Calculate offset of a child variable within its OCCURS parent
     */
    calculateChildOffset(child, occursParent) {
        if (child === occursParent) return 0;

        let offset = 0;
        let current = child;

        while (current && current.parent && current !== occursParent) {
            // Find position in parent's children
            for (const sibling of current.parent.children) {
                if (sibling === current) break;
                offset += sibling.getTotalLength();
            }
            current = current.parent;
        }

        return offset;
    }

    /**
     * Evaluate reference modification (e.g., VAR(1:5))
     */
    evaluateReferenceMod(expr) {
        // Get base value
        let baseValue;
        if (expr.subscripts) {
            // Has subscripts before reference mod: TABLE(1)(2:3)
            baseValue = this.evaluateSubscript({
                type: NodeType.SUBSCRIPT,
                name: expr.name,
                subscripts: expr.subscripts
            });
        } else {
            baseValue = this.runtime.getValue(expr.name) || '';
        }

        // Get start position (1-based in COBOL)
        const start = this.evaluateNumeric(expr.start);
        const startPos = start - 1; // Convert to 0-based

        // Get length (optional - if not specified, to end of string)
        let length;
        if (expr.length) {
            length = this.evaluateNumeric(expr.length);
        } else {
            length = baseValue.length - startPos;
        }

        // Extract substring
        return String(baseValue).substring(startPos, startPos + length);
    }

    /**
     * Evaluate a numeric expression
     */
    evaluateNumeric(expr) {
        if (!expr) return 0;

        if (typeof expr === 'number') return expr;

        switch (expr.type) {
            case NodeType.LITERAL:
                return parseFloat(expr.value) || 0;

            case NodeType.IDENTIFIER:
                return this.runtime.getNumericValue(expr.name);

            case NodeType.SUBSCRIPT:
                // Handle subscripted variables like WS-PRIX(1)
                const strValue = this.evaluateSubscript(expr);
                return parseFloat(strValue) || 0;

            case NodeType.BINARY_EXPR:
            case NodeType.UNARY_EXPR:
                return this.evaluateArithmetic(expr);

            default:
                return 0;
        }
    }

    /**
     * Evaluate arithmetic expression
     */
    evaluateArithmetic(expr) {
        if (!expr) return 0;

        switch (expr.type) {
            case NodeType.LITERAL:
                return parseFloat(expr.value) || 0;

            case NodeType.IDENTIFIER:
                return this.runtime.getNumericValue(expr.name);

            case NodeType.SUBSCRIPT:
                // Handle subscripted variables like WS-PRIX(1)
                const strValue = this.evaluateSubscript(expr);
                return parseFloat(strValue) || 0;

            case NodeType.UNARY_EXPR:
                const operand = this.evaluateArithmetic(expr.operand);
                return expr.operator === '-' ? -operand : operand;

            case NodeType.BINARY_EXPR:
                const left = this.evaluateArithmetic(expr.left);
                const right = this.evaluateArithmetic(expr.right);
                switch (expr.operator) {
                    case '+': return left + right;
                    case '-': return left - right;
                    case '*': return left * right;
                    case '/': return right !== 0 ? left / right : 0;
                    case '**': return Math.pow(left, right);
                    default: return 0;
                }

            default:
                return 0;
        }
    }

    /**
     * Evaluate a condition
     */
    evaluateCondition(condition) {
        if (!condition) return false;

        // Check if it's an identifier that's actually a condition name (88 level)
        if (condition.type === NodeType.IDENTIFIER) {
            if (this.runtime.isConditionName(condition.name)) {
                return this.runtime.evaluateConditionName(condition.name);
            }
        }

        if (condition.type === NodeType.CONDITION) {
            const { operator, left, right } = condition;

            // Handle NOT prefix
            const isNot = operator?.startsWith('NOT ');
            const op = isNot ? operator.substring(4) : operator;

            let result;

            switch (op) {
                case 'AND':
                    result = this.evaluateCondition(left) && this.evaluateCondition(right);
                    break;

                case 'OR':
                    result = this.evaluateCondition(left) || this.evaluateCondition(right);
                    break;

                case '=':
                    const leftVal = this.evaluateExpression(left);
                    const rightVal = this.evaluateExpression(right);
                    result = String(leftVal).trim() === String(rightVal).trim();
                    break;

                case '>':
                    result = this.evaluateNumeric(left) > this.evaluateNumeric(right);
                    break;

                case '<':
                    result = this.evaluateNumeric(left) < this.evaluateNumeric(right);
                    break;

                case '>=':
                    result = this.evaluateNumeric(left) >= this.evaluateNumeric(right);
                    break;

                case '<=':
                    result = this.evaluateNumeric(left) <= this.evaluateNumeric(right);
                    break;

                case '<>':
                    const lv = this.evaluateExpression(left);
                    const rv = this.evaluateExpression(right);
                    result = String(lv).trim() !== String(rv).trim();
                    break;

                case 'NUMERIC':
                    const val = this.evaluateExpression(left);
                    result = /^\d+$/.test(String(val).trim());
                    break;

                case 'ALPHABETIC':
                    const aval = this.evaluateExpression(left);
                    result = /^[A-Za-z ]+$/.test(String(aval));
                    break;

                default:
                    // Check if operator is empty and left is a condition name (88 level)
                    if (!op && left?.type === NodeType.IDENTIFIER) {
                        if (this.runtime.isConditionName(left.name)) {
                            result = this.runtime.evaluateConditionName(left.name);
                            break;
                        }
                    }
                    result = false;
            }

            return isNot ? !result : result;
        }

        // Simple truthy check for non-condition nodes
        // Also check if it's a condition name (88 level)
        if (condition.name && this.runtime.isConditionName(condition.name)) {
            return this.runtime.evaluateConditionName(condition.name);
        }

        const value = this.evaluateExpression(condition);
        return Boolean(value) && value !== '0' && value !== 'N' && value !== 'FALSE';
    }

    /**
     * Provide input when waiting
     */
    provideInput(value) {
        this.runtime.provideInput(value);
    }

    /**
     * Check if waiting for input
     */
    isWaitingForInput() {
        return this.runtime.waitingForInput;
    }

    /**
     * Execute STRING statement
     * Concatenates multiple source strings into a destination
     */
    executeString(stmt) {
        let result = '';
        let overflow = false;
        const destVar = this.runtime.getVariable(stmt.into);
        const destLength = destVar ? destVar.picInfo.length : 0;

        // Get starting position from pointer (1-based in COBOL)
        let pointer = stmt.pointer ? this.runtime.getNumericValue(stmt.pointer) : 1;
        let startPos = pointer - 1; // Convert to 0-based

        // Get current destination value for partial update
        let destValue = destVar ? destVar.getValue() : '';

        // Process each source
        for (const source of stmt.sources) {
            let sourceValue = this.evaluateExpression(source.value);
            sourceValue = String(sourceValue);

            // Apply delimiter if specified
            if (source.delimiter) {
                if (source.delimiter === 'SIZE') {
                    // Use full size (no trimming)
                } else {
                    // Find delimiter and truncate
                    const delimValue = this.evaluateExpression(source.delimiter);
                    const delimPos = sourceValue.indexOf(String(delimValue));
                    if (delimPos !== -1) {
                        sourceValue = sourceValue.substring(0, delimPos);
                    }
                }
            }

            // Append to result
            for (const char of sourceValue) {
                if (startPos >= destLength) {
                    overflow = true;
                    break;
                }
                // Replace character at position
                destValue = destValue.substring(0, startPos) + char + destValue.substring(startPos + 1);
                startPos++;
            }

            if (overflow) break;
        }

        // Update pointer variable
        if (stmt.pointer) {
            this.runtime.setVariable(stmt.pointer, startPos + 1); // Back to 1-based
        }

        // Update destination
        this.runtime.setVariable(stmt.into, destValue);

        // Execute overflow/not overflow handlers
        if (overflow && stmt.onOverflow.length > 0) {
            for (const s of stmt.onOverflow) {
                this.executeStatement(s);
            }
        } else if (!overflow && stmt.notOnOverflow.length > 0) {
            for (const s of stmt.notOnOverflow) {
                this.executeStatement(s);
            }
        }
    }

    /**
     * Execute UNSTRING statement
     * Splits a source string into multiple destination fields
     */
    executeUnstring(stmt) {
        let sourceValue = this.runtime.getValue(stmt.source);
        sourceValue = String(sourceValue);

        let overflow = false;
        let fieldCount = 0;

        // Get starting position from pointer (1-based in COBOL)
        let pointer = stmt.pointer ? this.runtime.getNumericValue(stmt.pointer) : 1;
        let position = pointer - 1; // Convert to 0-based

        // Process each destination
        for (const dest of stmt.destinations) {
            if (position >= sourceValue.length) {
                overflow = true;
                break;
            }

            let endPos = sourceValue.length;
            let foundDelim = null;

            // Find the nearest delimiter
            for (const delim of stmt.delimiters) {
                const delimValue = String(this.evaluateExpression(delim.value));
                let searchPos = position;

                if (delim.all) {
                    // ALL means treat consecutive delimiters as one
                    let idx = sourceValue.indexOf(delimValue, searchPos);
                    if (idx !== -1 && idx < endPos) {
                        endPos = idx;
                        foundDelim = delimValue;
                    }
                } else {
                    let idx = sourceValue.indexOf(delimValue, searchPos);
                    if (idx !== -1 && idx < endPos) {
                        endPos = idx;
                        foundDelim = delimValue;
                    }
                }
            }

            // Extract the field value
            const fieldValue = sourceValue.substring(position, endPos);
            this.runtime.setVariable(dest.into, fieldValue);
            fieldCount++;

            // Store delimiter if requested
            if (dest.delimiterIn && foundDelim !== null) {
                this.runtime.setVariable(dest.delimiterIn, foundDelim);
            }

            // Store count if requested
            if (dest.countIn) {
                this.runtime.setVariable(dest.countIn, fieldValue.length);
            }

            // Move past the field and delimiter
            position = endPos + (foundDelim ? foundDelim.length : 0);

            // Skip consecutive delimiters if ALL was specified
            for (const delim of stmt.delimiters) {
                if (delim.all) {
                    const delimValue = String(this.evaluateExpression(delim.value));
                    while (sourceValue.substring(position, position + delimValue.length) === delimValue) {
                        position += delimValue.length;
                    }
                }
            }
        }

        // Update pointer variable
        if (stmt.pointer) {
            this.runtime.setVariable(stmt.pointer, position + 1); // Back to 1-based
        }

        // Update tallying variable
        if (stmt.tallying) {
            this.runtime.setVariable(stmt.tallying, fieldCount);
        }

        // Execute overflow/not overflow handlers
        if (overflow && stmt.onOverflow.length > 0) {
            for (const s of stmt.onOverflow) {
                this.executeStatement(s);
            }
        } else if (!overflow && stmt.notOnOverflow.length > 0) {
            for (const s of stmt.notOnOverflow) {
                this.executeStatement(s);
            }
        }
    }

    /**
     * Execute INSPECT statement
     * Counts and/or replaces characters in a string
     */
    executeInspect(stmt) {
        let value = String(this.runtime.getValue(stmt.target));

        switch (stmt.mode) {
            case 'TALLYING':
                this.executeInspectTallying(stmt, value);
                break;

            case 'REPLACING':
                value = this.executeInspectReplacing(stmt, value);
                this.runtime.setVariable(stmt.target, value);
                break;

            case 'TALLYING_REPLACING':
                this.executeInspectTallying(stmt, value);
                value = this.executeInspectReplacing(stmt, value);
                this.runtime.setVariable(stmt.target, value);
                break;

            case 'CONVERTING':
                value = this.executeInspectConverting(stmt, value);
                this.runtime.setVariable(stmt.target, value);
                break;
        }
    }

    /**
     * Execute INSPECT TALLYING
     */
    executeInspectTallying(stmt, value) {
        for (const tally of stmt.tallying) {
            let count = 0;
            let searchValue = value;

            // Apply BEFORE/AFTER constraints
            if (tally.before) {
                const beforeStr = String(this.evaluateExpression(tally.before));
                const idx = searchValue.indexOf(beforeStr);
                if (idx !== -1) {
                    searchValue = searchValue.substring(0, idx);
                }
            }
            if (tally.after) {
                const afterStr = String(this.evaluateExpression(tally.after));
                const idx = searchValue.indexOf(afterStr);
                if (idx !== -1) {
                    searchValue = searchValue.substring(idx + afterStr.length);
                }
            }

            if (tally.type === 'CHARACTERS') {
                count = searchValue.length;
            } else if (tally.type === 'ALL' || tally.type === 'LEADING') {
                const pattern = String(this.evaluateExpression(tally.pattern));
                if (pattern.length > 0) {
                    if (tally.type === 'ALL') {
                        // Count all occurrences
                        let pos = 0;
                        while ((pos = searchValue.indexOf(pattern, pos)) !== -1) {
                            count++;
                            pos += pattern.length;
                        }
                    } else {
                        // LEADING - count only at the beginning
                        let pos = 0;
                        while (searchValue.substring(pos, pos + pattern.length) === pattern) {
                            count++;
                            pos += pattern.length;
                        }
                    }
                }
            }

            // Add to counter variable
            const currentCount = this.runtime.getNumericValue(tally.counter);
            this.runtime.setVariable(tally.counter, currentCount + count);
        }
    }

    /**
     * Execute INSPECT REPLACING
     */
    executeInspectReplacing(stmt, value) {
        for (const repl of stmt.replacing) {
            const byValue = String(this.evaluateExpression(repl.by));

            // Determine search range
            let startIdx = 0;
            let endIdx = value.length;

            if (repl.before) {
                const beforeStr = String(this.evaluateExpression(repl.before));
                const idx = value.indexOf(beforeStr);
                if (idx !== -1) {
                    endIdx = idx;
                }
            }
            if (repl.after) {
                const afterStr = String(this.evaluateExpression(repl.after));
                const idx = value.indexOf(afterStr);
                if (idx !== -1) {
                    startIdx = idx + afterStr.length;
                }
            }

            const prefix = value.substring(0, startIdx);
            let middle = value.substring(startIdx, endIdx);
            const suffix = value.substring(endIdx);

            if (repl.type === 'CHARACTERS') {
                // Replace all characters with first char of replacement
                const replChar = byValue.charAt(0) || ' ';
                middle = replChar.repeat(middle.length);
            } else {
                const pattern = String(this.evaluateExpression(repl.pattern));
                if (pattern.length > 0) {
                    if (repl.type === 'ALL') {
                        // Replace all occurrences
                        while (middle.includes(pattern)) {
                            middle = middle.replace(pattern, byValue);
                        }
                    } else if (repl.type === 'LEADING') {
                        // Replace only leading occurrences
                        while (middle.startsWith(pattern)) {
                            middle = byValue + middle.substring(pattern.length);
                        }
                    } else if (repl.type === 'FIRST') {
                        // Replace only the first occurrence
                        middle = middle.replace(pattern, byValue);
                    }
                }
            }

            value = prefix + middle + suffix;
        }

        return value;
    }

    /**
     * Execute INSPECT CONVERTING
     */
    executeInspectConverting(stmt, value) {
        const fromChars = String(this.evaluateExpression(stmt.converting.from));
        const toChars = String(this.evaluateExpression(stmt.converting.to));

        // Determine search range
        let startIdx = 0;
        let endIdx = value.length;

        if (stmt.converting.before) {
            const beforeStr = String(this.evaluateExpression(stmt.converting.before));
            const idx = value.indexOf(beforeStr);
            if (idx !== -1) {
                endIdx = idx;
            }
        }
        if (stmt.converting.after) {
            const afterStr = String(this.evaluateExpression(stmt.converting.after));
            const idx = value.indexOf(afterStr);
            if (idx !== -1) {
                startIdx = idx + afterStr.length;
            }
        }

        // Convert characters in the range
        let result = '';
        for (let i = 0; i < value.length; i++) {
            const char = value.charAt(i);
            if (i >= startIdx && i < endIdx) {
                const fromIdx = fromChars.indexOf(char);
                if (fromIdx !== -1 && fromIdx < toChars.length) {
                    result += toChars.charAt(fromIdx);
                } else {
                    result += char;
                }
            } else {
                result += char;
            }
        }

        return result;
    }

    /**
     * Execute SEARCH statement
     * Performs linear (SEARCH) or binary (SEARCH ALL) search on a table
     */
    async executeSearch(stmt) {
        // Get the table variable (must have OCCURS clause)
        const tableVar = this.runtime.getVariable(stmt.table);
        if (!tableVar || !tableVar.occursCount) {
            // Execute AT END if table not found or not an OCCURS table
            for (const s of stmt.atEnd) {
                await this.executeStatement(s);
            }
            return;
        }

        // Use effective count (respects DEPENDING ON)
        const maxIndex = tableVar.getEffectiveOccursCount(this.runtime);

        // Determine index variable: use VARYING if specified, or table's INDEXED BY
        let indexVarName = stmt.varying;
        if (!indexVarName && tableVar.indexedBy && tableVar.indexedBy.length > 0) {
            indexVarName = tableVar.indexedBy[0];
        }
        if (!indexVarName) {
            indexVarName = stmt.table + '-IDX';
        }

        // Ensure index variable exists
        if (!this.runtime.getVariable(indexVarName)) {
            // Create implicit index variable
            this.runtime.createImplicitIndex(indexVarName, 1);
        }

        if (stmt.all) {
            // SEARCH ALL - Binary search (requires ASCENDING/DESCENDING KEY)
            await this.executeBinarySearch(stmt, tableVar, indexVarName, maxIndex);
        } else {
            // SEARCH - Linear search
            await this.executeLinearSearch(stmt, tableVar, indexVarName, maxIndex);
        }
    }

    /**
     * Execute linear search (SEARCH without ALL)
     */
    async executeLinearSearch(stmt, tableVar, indexVarName, maxIndex) {
        let currentIndex = this.runtime.getNumericValue(indexVarName);

        // Start from current index value (COBOL doesn't reset it)
        while (currentIndex <= maxIndex) {
            // Check each WHEN condition
            for (const whenClause of stmt.whenClauses) {
                if (this.evaluateCondition(whenClause.condition)) {
                    // Condition matched - execute statements
                    for (const s of whenClause.statements) {
                        await this.executeStatement(s);
                    }
                    return; // Exit search after first match
                }
            }

            // Increment index for next iteration
            currentIndex++;
            this.runtime.setVariable(indexVarName, currentIndex);
        }

        // No match found - execute AT END
        for (const s of stmt.atEnd) {
            await this.executeStatement(s);
        }
    }

    /**
     * Execute binary search (SEARCH ALL)
     */
    async executeBinarySearch(stmt, tableVar, indexVarName, maxIndex) {
        // Binary search assumes table is sorted by the KEY
        let low = 1;
        let high = maxIndex;

        while (low <= high) {
            const mid = Math.floor((low + high) / 2);
            this.runtime.setVariable(indexVarName, mid);

            // For SEARCH ALL, there should be exactly one WHEN with an equality condition
            const whenClause = stmt.whenClauses[0];
            if (!whenClause) break;

            // Evaluate the condition
            const result = this.evaluateCondition(whenClause.condition);

            if (result) {
                // Found it!
                for (const s of whenClause.statements) {
                    await this.executeStatement(s);
                }
                return;
            }

            // For binary search, we need to know which direction to go
            // This requires comparing the key - simplified: use linear approach
            // In a full implementation, we'd need to extract and compare the key
            high = mid - 1; // Simplified: just search lower half then upper
            if (high < low) {
                low = mid + 1;
                high = maxIndex;
                if (low > maxIndex) break;
            }
        }

        // Not found - execute AT END
        for (const s of stmt.atEnd) {
            await this.executeStatement(s);
        }
    }

    /**
     * Execute SET statement
     * Sets index values or condition names
     */
    executeSet(stmt) {
        for (const target of stmt.targets) {
            switch (stmt.operation) {
                case 'TO':
                    const toValue = this.evaluateExpression(stmt.value);
                    // Check if it's a condition name (88 level)
                    if (this.runtime.isConditionName(target)) {
                        // SET condition TO TRUE/FALSE
                        const condition = this.runtime.conditionNames.get(this.runtime.normalizeVarName(target));
                        if (condition && toValue === true) {
                            // Set the parent variable to the first value of the condition
                            if (condition.values.length > 0) {
                                const firstValue = condition.values[0].value;
                                const actualValue = this.runtime.getConditionValueAsString(firstValue,
                                    this.runtime.getVariable(condition.parentVar));
                                this.runtime.setVariable(condition.parentVar, actualValue);
                            }
                        }
                    } else {
                        // Regular SET TO value
                        this.runtime.setVariable(target, toValue);
                    }
                    break;

                case 'UP':
                    const upValue = this.evaluateNumeric(stmt.value);
                    const currentUp = this.runtime.getNumericValue(target);
                    this.runtime.setVariable(target, currentUp + upValue);
                    break;

                case 'DOWN':
                    const downValue = this.evaluateNumeric(stmt.value);
                    const currentDown = this.runtime.getNumericValue(target);
                    this.runtime.setVariable(target, currentDown - downValue);
                    break;
            }
        }
    }

    /**
     * Execute CALL statement
     * Calls a subprogram (internal section/paragraph or external program)
     */
    async executeCall(stmt) {
        const programName = stmt.program;
        let exception = false;

        try {
            // First, try to find an internal section or paragraph with this name
            const internalTarget = this.findParagraphOrSection(programName);

            if (internalTarget) {
                // Save current USING parameters for BY REFERENCE
                const savedValues = {};
                for (const arg of stmt.using) {
                    if (arg.mode === 'REFERENCE' || arg.mode === 'CONTENT') {
                        savedValues[arg.name] = this.runtime.getValue(arg.name);
                    }
                }

                // Execute the internal section/paragraph
                await this.executeParagraphByName(programName);

                // BY CONTENT: restore original values (read-only)
                for (const arg of stmt.using) {
                    if (arg.mode === 'CONTENT') {
                        this.runtime.setVariable(arg.name, savedValues[arg.name]);
                    }
                }
            } else {
                // Try external program from the subprogram library
                const subprogram = this.runtime.getSubprogram?.(programName);

                if (subprogram) {
                    // Build argument list
                    const args = {};
                    for (const arg of stmt.using) {
                        args[arg.name] = {
                            value: this.runtime.getValue(arg.name),
                            mode: arg.mode
                        };
                    }

                    // Execute the subprogram
                    const result = await subprogram.execute(args, this.runtime);

                    // Update BY REFERENCE parameters from result
                    if (result && result.outputs) {
                        for (const [name, value] of Object.entries(result.outputs)) {
                            const arg = stmt.using.find(a => a.name === name);
                            if (arg && arg.mode === 'REFERENCE') {
                                this.runtime.setVariable(name, value);
                            }
                        }
                    }
                } else {
                    // Program not found - trigger exception
                    exception = true;
                    this.runtime.callbacks.onError?.(`CALL: Programme "${programName}" non trouvé`);
                }
            }
        } catch (error) {
            exception = true;
            this.runtime.callbacks.onError?.(`CALL ${programName}: ${error.message}`);
        }

        // Handle ON EXCEPTION / NOT ON EXCEPTION
        if (exception && stmt.onException.length > 0) {
            for (const s of stmt.onException) {
                await this.executeStatement(s);
            }
        } else if (!exception && stmt.notOnException.length > 0) {
            for (const s of stmt.notOnException) {
                await this.executeStatement(s);
            }
        }
    }

    /**
     * Find a paragraph or section by name
     */
    findParagraphOrSection(name) {
        const normalizedName = name.toUpperCase().replace(/-/g, '-');

        // Check sections
        for (const section of this.ast.procedure?.sections || []) {
            if (section.name?.toUpperCase() === normalizedName) {
                return section;
            }
            // Check paragraphs in section
            for (const para of section.paragraphs || []) {
                if (para.name?.toUpperCase() === normalizedName) {
                    return para;
                }
            }
        }

        // Check top-level paragraphs
        for (const para of this.ast.procedure?.paragraphs || []) {
            if (para.name?.toUpperCase() === normalizedName) {
                return para;
            }
        }

        return null;
    }

    /**
     * Execute CANCEL statement
     * Releases resources associated with a subprogram
     */
    executeCancel(stmt) {
        for (const programName of stmt.programs) {
            // Notify runtime to release subprogram resources
            this.runtime.cancelSubprogram?.(programName);
            // In our simplified implementation, this is mostly a no-op
            // but maintains COBOL compatibility
        }
    }
}

/**
 * Create and run interpreter
 */
export async function interpret(ast, options = {}) {
    const interpreter = new Interpreter(ast, options);
    await interpreter.run();
    return interpreter;
}
