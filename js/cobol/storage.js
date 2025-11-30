/**
 * COBOL IndexedDB Storage
 * Provides indexed file storage similar to COBOL VSAM/ISAM
 *
 * Structure:
 * - Database: HOPPER_COBOL
 * - Object Stores (one per file): stores records with indexed keys
 * - Metadata store: file definitions (organization, keys, etc.)
 */

const DB_NAME = 'HOPPER_COBOL';
const META_STORE = '_metadata';

let db = null;
let currentVersion = 1;

/**
 * Delete the database (for reset/recovery)
 */
export function deleteDatabase() {
    return new Promise((resolve, reject) => {
        if (db) {
            db.close();
            db = null;
        }
        const request = indexedDB.deleteDatabase(DB_NAME);
        request.onsuccess = () => resolve();
        request.onerror = () => reject(request.error);
    });
}

/**
 * Initialize the IndexedDB database
 */
export async function initStorage() {
    if (db) return db;

    return new Promise((resolve, reject) => {
        // First, open without version to get current version
        const infoRequest = indexedDB.open(DB_NAME);

        infoRequest.onsuccess = () => {
            const existingDb = infoRequest.result;
            currentVersion = existingDb.version;
            existingDb.close();

            // Now open with the correct version
            const request = indexedDB.open(DB_NAME, currentVersion);

            request.onerror = () => reject(request.error);

            request.onsuccess = () => {
                db = request.result;
                console.log('[STORAGE] Database opened, version:', db.version, 'stores:', Array.from(db.objectStoreNames));
                resolve(db);
            };

            request.onupgradeneeded = (event) => {
                const database = event.target.result;
                if (!database.objectStoreNames.contains(META_STORE)) {
                    database.createObjectStore(META_STORE, { keyPath: 'name' });
                }
            };
        };

        infoRequest.onerror = () => reject(infoRequest.error);

        infoRequest.onupgradeneeded = (event) => {
            // First time - create metadata store
            const database = event.target.result;
            if (!database.objectStoreNames.contains(META_STORE)) {
                database.createObjectStore(META_STORE, { keyPath: 'name' });
            }
        };
    });
}

/**
 * Get or create a file store
 */
async function getFileStore(fileName, mode = 'readonly') {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(storeName)) {
        // Need to create the store - requires version upgrade
        return null;
    }

    const tx = db.transaction(storeName, mode);
    return tx.objectStore(storeName);
}

/**
 * Create a new COBOL file (object store)
 */
export async function createFile(fileDefinition) {
    const { name, organization, recordKey, fields } = fileDefinition;
    const storeName = name.toUpperCase();

    // Close current connection to upgrade
    if (db) {
        db.close();
        db = null;
    }

    // Increment version for schema change
    currentVersion++;
    console.log('[STORAGE] Creating file:', storeName, 'new version:', currentVersion);

    return new Promise((resolve, reject) => {
        const request = indexedDB.open(DB_NAME, currentVersion);

        request.onerror = () => reject(request.error);

        request.onsuccess = () => {
            db = request.result;
            resolve(true);
        };

        request.onupgradeneeded = (event) => {
            const database = event.target.result;

            // Create file store if it doesn't exist
            if (!database.objectStoreNames.contains(storeName)) {
                const storeOptions = {};

                // For indexed files, use the record key as keyPath
                if (organization === 'INDEXED' && recordKey) {
                    storeOptions.keyPath = recordKey.toUpperCase().replace(/-/g, '_');
                } else {
                    // Sequential files use auto-increment
                    storeOptions.autoIncrement = true;
                }

                const store = database.createObjectStore(storeName, storeOptions);

                // Create index on record key for INDEXED organization
                if (organization === 'INDEXED' && recordKey) {
                    const keyName = recordKey.toUpperCase().replace(/-/g, '_');
                    store.createIndex(keyName, keyName, { unique: true });
                }
            }

            // Ensure metadata store exists
            if (!database.objectStoreNames.contains(META_STORE)) {
                database.createObjectStore(META_STORE, { keyPath: 'name' });
            }

            // Save file metadata after upgrade completes
            const metaTx = event.target.transaction;
            if (metaTx && database.objectStoreNames.contains(META_STORE)) {
                const metaStore = metaTx.objectStore(META_STORE);
                metaStore.put({
                    name: storeName,
                    organization: organization || 'SEQUENTIAL',
                    recordKey: recordKey,
                    fields: fields || [],
                    created: new Date().toISOString()
                });
            }
        };
    });
}

/**
 * Check if a file exists
 */
export async function fileExists(fileName) {
    await initStorage();
    return db.objectStoreNames.contains(fileName.toUpperCase());
}

/**
 * Get file metadata
 */
export async function getFileMetadata(fileName) {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(META_STORE)) {
        return null;
    }

    return new Promise((resolve, reject) => {
        const tx = db.transaction(META_STORE, 'readonly');
        const store = tx.objectStore(META_STORE);
        const request = store.get(storeName);

        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
    });
}

/**
 * List all files
 */
export async function listFiles() {
    await initStorage();

    console.log('[STORAGE] listFiles - db.objectStoreNames:', Array.from(db.objectStoreNames));

    const files = [];
    for (const name of db.objectStoreNames) {
        if (name !== META_STORE) {
            const meta = await getFileMetadata(name);
            const count = await countRecords(name);
            files.push({
                name,
                ...meta,
                recordCount: count
            });
        }
    }
    console.log('[STORAGE] listFiles - returning:', files);
    return files;
}

/**
 * Count records in a file
 */
export async function countRecords(fileName) {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(storeName)) {
        return 0;
    }

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readonly');
        const store = tx.objectStore(storeName);
        const request = store.count();

        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
    });
}

/**
 * Read all records from a file
 */
export async function readAllRecords(fileName) {
    await initStorage();
    const storeName = fileName.toUpperCase();

    console.log('[STORAGE] readAllRecords for:', storeName);
    console.log('[STORAGE] db.objectStoreNames:', Array.from(db.objectStoreNames));

    if (!db.objectStoreNames.contains(storeName)) {
        console.log('[STORAGE] Store not found');
        return [];
    }

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readonly');
        const store = tx.objectStore(storeName);
        const request = store.getAll();

        request.onsuccess = () => {
            console.log('[STORAGE] readAllRecords result:', request.result);
            resolve(request.result);
        };
        request.onerror = () => reject(request.error);
    });
}

/**
 * Read a record by key (for INDEXED files)
 */
export async function readByKey(fileName, keyValue) {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(storeName)) {
        return null;
    }

    // Trim the key value (COBOL pads strings with spaces)
    const trimmedKey = keyValue?.toString().trim();

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readonly');
        const store = tx.objectStore(storeName);
        const request = store.get(trimmedKey);

        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
    });
}

/**
 * Read records with key >= startKey (for START statement)
 */
export async function readFromKey(fileName, startKey, operator = '>=') {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(storeName)) {
        return [];
    }

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readonly');
        const store = tx.objectStore(storeName);

        let range;
        switch (operator) {
            case '>=':
                range = IDBKeyRange.lowerBound(startKey, false);
                break;
            case '>':
                range = IDBKeyRange.lowerBound(startKey, true);
                break;
            case '=':
                range = IDBKeyRange.only(startKey);
                break;
            default:
                range = IDBKeyRange.lowerBound(startKey, false);
        }

        const records = [];
        const request = store.openCursor(range);

        request.onsuccess = (event) => {
            const cursor = event.target.result;
            if (cursor) {
                records.push(cursor.value);
                cursor.continue();
            } else {
                resolve(records);
            }
        };

        request.onerror = () => reject(request.error);
    });
}

/**
 * Write a record
 */
export async function writeRecord(fileName, record) {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(storeName)) {
        throw new Error(`File ${storeName} does not exist`);
    }

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readwrite');
        const store = tx.objectStore(storeName);
        const request = store.add(record);

        request.onsuccess = () => resolve({ success: true });
        request.onerror = () => {
            if (request.error.name === 'ConstraintError') {
                resolve({ success: false, invalidKey: true, message: 'Duplicate key' });
            } else {
                reject(request.error);
            }
        };
    });
}

/**
 * Rewrite (update) a record
 */
export async function rewriteRecord(fileName, record) {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(storeName)) {
        throw new Error(`File ${storeName} does not exist`);
    }

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readwrite');
        const store = tx.objectStore(storeName);
        const request = store.put(record);

        request.onsuccess = () => resolve({ success: true });
        request.onerror = () => reject(request.error);
    });
}

/**
 * Delete a record by key
 */
export async function deleteRecord(fileName, keyValue) {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(storeName)) {
        return { success: false, invalidKey: true };
    }

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readwrite');
        const store = tx.objectStore(storeName);
        const request = store.delete(keyValue);

        request.onsuccess = () => resolve({ success: true });
        request.onerror = () => reject(request.error);
    });
}

/**
 * Delete all records from a file (INITIALIZE)
 */
export async function clearFile(fileName) {
    await initStorage();
    const storeName = fileName.toUpperCase();

    if (!db.objectStoreNames.contains(storeName)) {
        return false;
    }

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readwrite');
        const store = tx.objectStore(storeName);
        const request = store.clear();

        request.onsuccess = () => resolve(true);
        request.onerror = () => reject(request.error);
    });
}

/**
 * Delete a file completely
 */
export async function deleteFile(fileName) {
    const storeName = fileName.toUpperCase();

    // Close current connection to upgrade
    if (db) {
        db.close();
        db = null;
    }

    return new Promise((resolve, reject) => {
        const request = indexedDB.open(DB_NAME, Date.now());

        request.onerror = () => reject(request.error);

        request.onsuccess = () => {
            db = request.result;
            resolve(true);
        };

        request.onupgradeneeded = (event) => {
            const database = event.target.result;

            if (database.objectStoreNames.contains(storeName)) {
                database.deleteObjectStore(storeName);
            }

            // Remove metadata
            if (database.objectStoreNames.contains(META_STORE)) {
                const metaTx = event.target.transaction;
                const metaStore = metaTx.objectStore(META_STORE);
                metaStore.delete(storeName);
            }
        };
    });
}

/**
 * Import records from array (for migration from localStorage)
 */
export async function importRecords(fileName, records, fileDefinition) {
    // Create the file first
    await createFile({
        name: fileName,
        ...fileDefinition
    });

    // Import records
    await initStorage();
    const storeName = fileName.toUpperCase();

    return new Promise((resolve, reject) => {
        const tx = db.transaction(storeName, 'readwrite');
        const store = tx.objectStore(storeName);

        let imported = 0;
        for (const record of records) {
            // Normalize field names (replace - with _)
            const normalizedRecord = {};
            for (const [key, value] of Object.entries(record)) {
                normalizedRecord[key.toUpperCase().replace(/-/g, '_')] = value;
            }
            store.put(normalizedRecord);
            imported++;
        }

        tx.oncomplete = () => resolve(imported);
        tx.onerror = () => reject(tx.error);
    });
}

/**
 * Export all data (for backup)
 */
export async function exportAllData() {
    const files = await listFiles();
    const data = {};

    for (const file of files) {
        const records = await readAllRecords(file.name);
        data[file.name] = {
            metadata: file,
            records
        };
    }

    return data;
}

/**
 * Migrate from localStorage to IndexedDB
 */
export async function migrateFromLocalStorage() {
    const stored = localStorage.getItem('cobolFiles');
    if (!stored) return { migrated: 0, files: [] };

    const oldFiles = JSON.parse(stored);
    const migrated = [];

    for (const [name, fileData] of Object.entries(oldFiles)) {
        try {
            await importRecords(name, fileData.records || [], {
                organization: fileData.organization || 'SEQUENTIAL',
                recordKey: fileData.key,
                fields: fileData.fields || []
            });
            migrated.push(name);
        } catch (e) {
            console.error(`Migration failed for ${name}:`, e);
        }
    }

    // Clear localStorage after successful migration
    if (migrated.length > 0) {
        localStorage.removeItem('cobolFiles');
        localStorage.setItem('cobolFiles_migrated', 'true');
    }

    return { migrated: migrated.length, files: migrated };
}

/**
 * Check if migration is needed
 */
export function needsMigration() {
    return localStorage.getItem('cobolFiles') !== null &&
           localStorage.getItem('cobolFiles_migrated') !== 'true';
}

// Export for use as a module
export default {
    initStorage,
    createFile,
    fileExists,
    getFileMetadata,
    listFiles,
    countRecords,
    readAllRecords,
    readByKey,
    readFromKey,
    writeRecord,
    rewriteRecord,
    deleteRecord,
    clearFile,
    deleteFile,
    importRecords,
    exportAllData,
    migrateFromLocalStorage,
    needsMigration
};
