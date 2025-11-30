/**
 * COBOL Interpreter
 * Executes COBOL AST in the browser
 */

import { NodeType } from './parser.js';
import * as storage from './storage.js';

/**
 * PIC clause parser - extracts type and length from PIC strings
 */
function parsePic(picString) {
    if (!picString) return { type: 'alphanumeric', length: 1, decimals: 0, signed: false };

    const pic = picString.toUpperCase();
    let type = 'alphanumeric';
    let length = 0;
    let decimals = 0;
    let signed = false;

    // Check for sign
    if (pic.includes('S')) {
        signed = true;
    }

    // Check for type
    if (pic.includes('9') || pic.includes('V') || pic.includes('Z')) {
        type = 'numeric';
    } else if (pic.includes('A')) {
        type = 'alphabetic';
    }

    // Calculate length
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
            if (inDecimal) {
                decimals += count;
            }
            length += count;
            i++;
            continue;
        }

        if ('X9AVSZ*+-.,'.includes(char)) {
            if (inDecimal && '9Z*'.includes(char)) {
                decimals++;
            }
            if ('X9AZ*'.includes(char)) {
                length++;
            }
        }

        i++;
    }

    return { type, length: length || 1, decimals, signed };
}

/**
 * Format a value according to PIC clause
 */
function formatValue(value, pic) {
    const { type, length, decimals, signed } = parsePic(pic);

    if (type === 'numeric') {
        // Check if value is a number (result of computation) vs string (user input)
        if (typeof value === 'number') {
            // For computed values, scale by decimals and format
            if (decimals > 0) {
                const scaled = Math.round(value * Math.pow(10, decimals));
                return String(Math.abs(scaled)).padStart(length, '0').slice(-length);
            }
            return String(Math.abs(Math.floor(value))).padStart(length, '0').slice(-length);
        }
        // For string input (user entry), treat as raw digits (COBOL style)
        const str = String(value).replace(/[^0-9]/g, '');
        return str.padStart(length, '0').slice(-length);
    }

    // Alphanumeric - left align and pad with spaces
    const str = String(value || '');
    return str.padEnd(length, ' ').substring(0, length);
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
        this.value = this.getInitialValue();
        this.children = [];
        this.parent = null;
        this.redefines = definition.redefines;
    }

    getInitialValue() {
        if (this.initialValue) {
            if (this.initialValue.type === 'string') {
                return formatValue(this.initialValue.value, this.pic);
            }
            if (this.initialValue.type === 'number') {
                return formatValue(this.initialValue.value, this.pic);
            }
            if (this.initialValue.type === 'figurative') {
                return this.getFigurativeValue(this.initialValue.value);
            }
        }

        // Default initialization
        if (this.picInfo.type === 'numeric') {
            return '0'.repeat(this.picInfo.length);
        }
        return ' '.repeat(this.picInfo.length);
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
        if (this.children.length > 0) {
            // Group item - concatenate children values
            return this.children.map(c => c.getValue()).join('');
        }
        return this.value;
    }

    getNumericValue() {
        const val = this.getValue().trim();
        if (this.picInfo.decimals > 0) {
            const intPart = val.substring(0, val.length - this.picInfo.decimals);
            const decPart = val.substring(val.length - this.picInfo.decimals);
            return parseFloat(intPart + '.' + decPart) || 0;
        }
        return parseInt(val) || 0;
    }

    /**
     * Get value formatted for display (with decimal point if needed)
     */
    getDisplayValue() {
        if (this.picInfo.type === 'numeric' && this.picInfo.decimals > 0) {
            const val = this.getValue().trim();
            const intPart = val.substring(0, val.length - this.picInfo.decimals) || '0';
            const decPart = val.substring(val.length - this.picInfo.decimals);
            return intPart + '.' + decPart;
        }
        return this.getValue();
    }

    setValue(value) {
        if (this.children.length > 0) {
            // Group item - distribute value to children
            let str = String(value);
            let offset = 0;
            for (const child of this.children) {
                const len = child.picInfo.length;
                child.setValue(str.substring(offset, offset + len));
                offset += len;
            }
        } else {
            this.value = formatValue(value, this.pic);
        }
    }

    /**
     * Set raw value without reformatting (for values already in COBOL format from storage)
     */
    setRawValue(value) {
        if (this.children.length > 0) {
            let str = String(value);
            let offset = 0;
            for (const child of this.children) {
                const len = child.picInfo.length;
                child.setRawValue(str.substring(offset, offset + len));
                offset += len;
            }
        } else {
            // Just store as-is, padding/truncating to fit the PIC length
            const str = String(value || '');
            if (this.picInfo.type === 'numeric') {
                this.value = str.padStart(this.picInfo.length, '0').substring(0, this.picInfo.length);
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
        console.log('[FILE] Opening', fileName, 'mode:', mode);

        try {
            // Check if file exists in IndexedDB
            const exists = await storage.fileExists(fileName);
            console.log('[FILE] File exists in IndexedDB:', exists);

            if (exists) {
                // Load records from IndexedDB
                this.records = await storage.readAllRecords(fileName);
                console.log('[FILE] Loaded', this.records.length, 'records from IndexedDB');
            } else {
                // Create the file in IndexedDB for I-O and OUTPUT modes
                if (mode === 'I-O' || mode === 'OUTPUT') {
                    console.log('[FILE] Creating new file in IndexedDB');
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
            console.error('[FILE] Error opening file:', error);
            this.records = [];
        }

        return true;
    }

    async close() {
        const fileName = this.getFileName();
        console.log('[FILE] Closing', fileName, 'with', this.records.length, 'records');

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
            console.log('[FILE] Saved', this.records.length, 'records to IndexedDB');

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
            console.error('[FILE] Error closing file:', error);
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
        console.log('[FILE] readKey - looking for key:', normalizedKeyName, '=', keyValue);

        const record = this.records.find(r => {
            const keyField = Object.keys(r).find(k =>
                k.toUpperCase() === normalizedKeyName
            );
            const match = keyField && r[keyField]?.toString().trim() === keyValue?.toString().trim();
            if (keyField) {
                console.log('[FILE] readKey - record key value:', r[keyField], 'match:', match);
            }
            return match;
        });

        if (record) {
            console.log('[FILE] readKey - found record:', record);
            return { success: true, record };
        }
        console.log('[FILE] readKey - no record found');
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
        console.log('[OPEN] Opening file:', fileName, 'mode:', mode);

        // Create file if it doesn't exist (for I-O and OUTPUT modes)
        if (mode === 'I-O' || mode === 'OUTPUT') {
            const exists = await this.storage.fileExists(fileName);
            console.log('[OPEN] File exists:', exists);
            if (!exists) {
                console.log('[OPEN] Creating file with org:', this.organization, 'key:', this.recordKey);
                await this.storage.createFile({
                    name: fileName,
                    organization: this.organization,
                    recordKey: this.recordKey
                });
                console.log('[OPEN] File created');
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
        console.log('[readKey] fileName:', fileName, 'searchKey:', JSON.stringify(searchKey));
        const record = await this.storage.readByKey(fileName, searchKey);
        console.log('[readKey] result:', record);

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
}

// Export storage configuration
export const USE_INDEXEDDB = true; // IndexedDB storage enabled

/**
 * Runtime class - manages program execution state
 */
class Runtime {
    constructor(callbacks = {}) {
        this.variables = new Map();
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

    setVariable(name, value) {
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

    // Output
    display(message) {
        if (this.callbacks.onDisplay) {
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
     * Continue to next step (when paused)
     */
    stepNext() {
        if (this.stepResolve) {
            this.stepResolve();
            this.stepResolve = null;
        }
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

            for (const item of items) {
                if (!item.name || item.name === 'FILLER') continue;

                const variable = new Variable(item);

                // Handle hierarchy based on level numbers
                while (stack.length > 0 && stack[stack.length - 1].level >= item.level) {
                    stack.pop();
                }

                if (stack.length > 0) {
                    variable.parent = stack[stack.length - 1];
                    stack[stack.length - 1].children.push(variable);
                }

                this.runtime.variables.set(this.runtime.normalizeVarName(item.name), variable);
                stack.push(variable);

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
                console.log('[EXEC] STOP RUN executed');
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
                console.log('[EXEC] About to execute READ statement');
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
        }
    }

    /**
     * Execute DISPLAY statement
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
        this.runtime.display(message);
    }

    /**
     * Execute ACCEPT statement
     */
    async executeAccept(stmt) {
        await this.runtime.accept(stmt.target);
        // Reset loop counter after user input (prevents false infinite loop detection)
        this.loopCounter = 0;
    }

    /**
     * Execute MOVE statement
     */
    executeMove(stmt) {
        const value = this.evaluateExpression(stmt.source);

        for (const target of stmt.targets) {
            this.runtime.setVariable(target, value);
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
                if (subject === whenValue || String(subject).trim() === String(whenValue).trim()) {
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
        console.log('[READ] executeRead called for file:', stmt.file);
        const file = this.runtime.getFile(stmt.file);
        console.log('[READ] file object:', file ? 'found' : 'NOT FOUND');

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
            console.log('[READ] INDEXED file - keyField:', keyField, 'keyValue:', keyValue);
            if (keyValue && keyValue.toString().trim()) {
                result = await file.readKey(keyValue);
                console.log('[READ] readKey result:', result);
            } else {
                result = await file.read();
                console.log('[READ] sequential read result:', result);
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
            console.log('[READ] INVALID KEY branch - stmt.invalidKey:', stmt.invalidKey?.length, 'statements');
            if (stmt.invalidKey && stmt.invalidKey.length > 0) {
                for (const s of stmt.invalidKey) {
                    console.log('[READ] Executing INVALID KEY statement:', s.type);
                    await this.executeStatement(s);
                }
            } else {
                console.log('[READ] No INVALID KEY statements to execute');
            }
        } else {
            console.log('[READ] No branch matched - result:', result);
        }
    }

    /**
     * Execute WRITE statement
     */
    async executeWrite(stmt) {
        console.log('[WRITE] executeWrite called for record:', stmt.record);

        // Find the file that owns this record
        let targetFile = null;
        let recordDef = null;

        const recordName = stmt.record?.toUpperCase();
        console.log('[WRITE] Looking for record:', recordName);

        // AST stores FD in data.fileSection (not data.files)
        const fileSection = this.ast.data?.fileSection || [];
        console.log('[WRITE] Available files in AST:', fileSection.map(f => ({
            name: f.fileName,
            records: f.records?.map(r => r.name)
        })));

        // Look for the record definition in the AST file section
        for (const fileDef of fileSection) {
            // Check in records array (FD can have multiple 01 level records)
            for (const rec of fileDef.records || []) {
                console.log('[WRITE] Checking fileDef:', fileDef.fileName, 'record:', rec?.name);
                if (rec?.name?.toUpperCase() === recordName) {
                    targetFile = this.runtime.getFile(fileDef.fileName);
                    recordDef = rec;
                    console.log('[WRITE] Found file:', fileDef.fileName, 'for record:', recordName);
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
                    console.log('[WRITE] Using first writable file:', fileName);
                    break;
                }
            }
        }

        if (!targetFile) {
            console.error('[WRITE] No writable file found');
            return;
        }

        // Build record from the record's fields defined in AST
        const record = {};

        console.log('[WRITE] recordDef:', recordDef);
        console.log('[WRITE] recordDef.children:', recordDef?.children);

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
        console.log('[WRITE] Record fields from AST definition:', Array.from(recordFields));

        // Use field definitions from AST
        for (const fieldName of recordFields) {
            const variable = this.runtime.variables.get(fieldName);
            if (variable) {
                record[fieldName] = variable.getValue();
            }
        }

        // If no fields found from AST, the record structure wasn't parsed correctly
        if (Object.keys(record).length === 0) {
            console.warn('[WRITE] No fields found from AST! Check parser output.');
        }

        console.log('[WRITE] Writing record:', record);
        const result = await targetFile.write(record);
        console.log('[WRITE] Result:', result);

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
            console.error('[REWRITE] No writable file found');
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
        console.log('START result:', success, 'cursor records:', file.cursorRecords?.length || 0);

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
     * Evaluate an expression
     */
    evaluateExpression(expr) {
        if (!expr) return '';

        switch (expr.type) {
            case NodeType.LITERAL:
                return expr.value;

            case NodeType.IDENTIFIER:
                return this.runtime.getValue(expr.name) || '';

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
                    result = false;
            }

            return isNot ? !result : result;
        }

        // Simple truthy check for non-condition nodes
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
}

/**
 * Create and run interpreter
 */
export async function interpret(ast, options = {}) {
    const interpreter = new Interpreter(ast, options);
    await interpreter.run();
    return interpreter;
}
