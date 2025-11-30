/**
 * Data Manager Module - IndexedDB Version
 * Handles COBOL file and record management using IndexedDB
 */

import { logIO, openModal, closeModal } from './utils.js';
import { setCode, generateCards } from './editor.js';
import { switchTab } from './utils.js';
import * as storage from './cobol/storage.js';

// State
let currentDMFile = null;
let editingRecordIndex = -1;

/**
 * Get all files (for interpreter access)
 * Returns a proxy that works with both sync and async access
 */
export async function getFiles() {
    const files = await storage.listFiles();
    const result = {};

    for (const file of files) {
        const records = await storage.readAllRecords(file.name);
        result[file.name] = {
            name: file.name,
            organization: file.organization || 'SEQUENTIAL',
            key: file.recordKey,
            records: records
        };
    }

    return result;
}

/**
 * Get files synchronously (for compatibility - uses cached data)
 */
let cachedFiles = {};
export function getFilesSync() {
    return cachedFiles;
}

/**
 * Refresh the file cache
 */
export async function refreshCache() {
    cachedFiles = await getFiles();
}

/**
 * Initialize the data manager
 */
export async function initDataManager() {
    // Initialize IndexedDB storage
    await storage.initStorage();

    // Check for migration from localStorage
    if (storage.needsMigration()) {
        console.log('Migrating from localStorage to IndexedDB...');
        const result = await storage.migrateFromLocalStorage();
        console.log(`Migration complete: ${result.migrated} files migrated`);
    }

    // Refresh cache
    await refreshCache();

    // Render UI
    await renderFileList();
}

/**
 * Save a file
 */
async function saveFile(fileName, fileData) {
    const exists = await storage.fileExists(fileName);

    if (!exists) {
        await storage.createFile({
            name: fileName,
            organization: fileData.organization || 'SEQUENTIAL',
            recordKey: fileData.key
        });
    }

    // Clear and re-import records
    await storage.clearFile(fileName);
    for (const record of fileData.records || []) {
        await storage.writeRecord(fileName, record);
    }

    await refreshCache();
}

/**
 * Render the file list
 */
export async function renderFileList() {
    const list = document.getElementById('fileList');
    if (!list) return;

    const files = await storage.listFiles();
    console.log('[DATA] renderFileList - files:', files);


    if (files.length === 0) {
        list.innerHTML = '<div class="no-files">Aucun fichier. Créez-en un ou exécutez un programme.</div>';
        return;
    }

    list.innerHTML = files.map(file => `
        <div class="file-item ${currentDMFile === file.name ? 'selected' : ''}"
             onclick="window.dataManagerModule.selectFile('${file.name}')">
            <div class="file-info">
                <span class="file-name">${file.name}</span>
                <span class="file-meta">${file.organization || 'SEQ'} | ${file.recordCount || 0} enr.</span>
            </div>
            <div class="file-actions">
                <button class="file-action-btn" onclick="event.stopPropagation();window.dataManagerModule.generateFileCode('${file.name}')">📝</button>
                <button class="file-action-btn" onclick="event.stopPropagation();window.dataManagerModule.exportFile('${file.name}')">📤</button>
                <button class="file-action-btn delete" onclick="event.stopPropagation();window.dataManagerModule.deleteFile('${file.name}')">🗑️</button>
            </div>
        </div>
    `).join('');
}


/**
 * Select a file
 */
export async function selectFile(fileName) {
    currentDMFile = fileName;
    await renderFileList();
    await renderRecords(fileName);
}

/**
 * Render records for a file
 */
async function renderRecords(fileName) {
    const schemaContainer = document.getElementById('recordSchema');
    const listContainer = document.getElementById('recordList');

    console.log('[DATA] renderRecords for:', fileName);

    if (!fileName) {
        if (schemaContainer) schemaContainer.innerHTML = 'Sélectionnez un fichier';
        if (listContainer) listContainer.innerHTML = '';
        return;
    }

    const meta = await storage.getFileMetadata(fileName);
    const records = await storage.readAllRecords(fileName);
    console.log('[DATA] records:', records);

    // Update schema display
    if (schemaContainer) {
        const keyInfo = meta?.recordKey ? ` (Clé: ${meta.recordKey})` : '';
        schemaContainer.innerHTML = `<strong>${fileName}</strong> - ${meta?.organization || 'SEQ'}${keyInfo} - ${records.length} enr.`;
    }

    if (!listContainer) return;

    if (!records || records.length === 0) {
        listContainer.innerHTML = '<div class="no-records">Aucun enregistrement</div>';
        return;
    }

    // Get field names from first record
    const fields = Object.keys(records[0]);

    listContainer.innerHTML = `
        <table class="records-table">
            <thead>
                <tr>
                    ${fields.map(f => `<th>${f}</th>`).join('')}
                    <th>Actions</th>
                </tr>
            </thead>
            <tbody>
                ${records.map((record, index) => `
                    <tr>
                        ${fields.map(f => `<td>${record[f] || ''}</td>`).join('')}
                        <td class="record-actions">
                            <button class="file-action-btn" onclick="window.dataManagerModule.editRecord(${index})">✏️</button>
                            <button class="file-action-btn delete" onclick="window.dataManagerModule.deleteRecord(${index})">🗑️</button>
                        </td>
                    </tr>
                `).join('')}
            </tbody>
        </table>
    `;
}

/**
 * Create a new file
 */
export function createNewFile() {
    openModal('fileModal');
}

/**
 * Save new file from modal
 */
export async function saveNewFile() {
    const name = document.getElementById('newFileName')?.value?.toUpperCase().trim();
    const org = document.getElementById('newFileOrg')?.value || 'SEQUENTIAL';
    const key = document.getElementById('newFileKey')?.value?.toUpperCase().trim();

    if (!name) {
        alert('Nom de fichier requis');
        return;
    }

    await storage.createFile({
        name,
        organization: org,
        recordKey: key || null
    });

    closeModal('fileModal');
    await refreshCache();
    await renderFileList();
    await selectFile(name);
}

/**
 * Delete a file
 */
export async function deleteFile(fileName) {
    if (!confirm(`Supprimer le fichier ${fileName} ?`)) return;

    await storage.deleteFile(fileName);

    if (currentDMFile === fileName) {
        currentDMFile = null;
        const container = document.getElementById('recordsContainer');
        if (container) container.innerHTML = '';
    }

    await refreshCache();
    await renderFileList();
}

/**
 * Add a record to current file
 */
export async function addRecord() {
    if (!currentDMFile) {
        alert('Sélectionnez un fichier d\'abord');
        return;
    }

    editingRecordIndex = -1; // New record mode
    await openRecordModal(null);
}

/**
 * Open record modal for add/edit
 */
async function openRecordModal(record) {
    const form = document.getElementById('recordForm');
    if (!form) return;

    const meta = await storage.getFileMetadata(currentDMFile);
    const records = await storage.readAllRecords(currentDMFile);

    // Get field names from existing records or metadata
    let fields = [];
    if (records.length > 0) {
        fields = Object.keys(records[0]);
    } else if (meta?.recordKey) {
        // Create basic structure with key field
        fields = [meta.recordKey.toUpperCase().replace(/-/g, '_')];
    }

    if (fields.length === 0) {
        alert('Structure de fichier inconnue. Ajoutez d\'abord un enregistrement via le programme COBOL.');
        return;
    }

    // Build form with terminal/DBA style
    form.innerHTML = `
        <div class="dba-form">
            <div class="dba-header">
                ┌─ ${currentDMFile} ─ ${editingRecordIndex >= 0 ? 'MODIFIER' : 'NOUVEAU'} ─┐
            </div>
            ${fields.map(field => `
                <div class="dba-field">
                    <label class="dba-label">${field}</label>
                    <input type="text"
                           class="dba-input"
                           id="field_${field}"
                           value="${record?.[field] || ''}"
                           placeholder="${field}">
                </div>
            `).join('')}
            <div class="dba-footer">
                └${'─'.repeat(30)}┘
            </div>
        </div>
    `;

    openModal('recordModal');
}

/**
 * Save edited record
 */
export async function saveRecord() {
    if (!currentDMFile) return;

    const form = document.getElementById('recordForm');
    const inputs = form.querySelectorAll('.dba-input');
    const record = {};

    inputs.forEach(input => {
        const fieldName = input.id.replace('field_', '');
        record[fieldName] = input.value;
    });

    const meta = await storage.getFileMetadata(currentDMFile);

    if (editingRecordIndex >= 0) {
        // Update existing record
        const keyField = meta?.recordKey?.toUpperCase().replace(/-/g, '_');
        if (keyField && record[keyField]) {
            await storage.deleteRecord(currentDMFile, record[keyField]);
        }
    }

    await storage.writeRecord(currentDMFile, record);

    closeModal('recordModal');
    await refreshCache();
    await renderRecords(currentDMFile);
    logIO(`Enregistrement ${editingRecordIndex >= 0 ? 'modifié' : 'ajouté'} dans ${currentDMFile}`);
}

/**
 * Edit a record
 */
export async function editRecord(index) {
    if (!currentDMFile) return;

    const records = await storage.readAllRecords(currentDMFile);
    const record = records[index];
    if (!record) return;

    editingRecordIndex = index;
    await openRecordModal(record);
}

/**
 * Delete a record
 */
export async function deleteRecord(index) {
    if (!currentDMFile) return;
    if (!confirm('Supprimer cet enregistrement ?')) return;

    const records = await storage.readAllRecords(currentDMFile);
    const record = records[index];

    if (!record) return;

    // Get the key field
    const meta = await storage.getFileMetadata(currentDMFile);
    const keyField = meta?.recordKey?.toUpperCase().replace(/-/g, '_');

    if (keyField && record[keyField]) {
        await storage.deleteRecord(currentDMFile, record[keyField]);
    } else {
        // For sequential files, we need to clear and rewrite without this record
        records.splice(index, 1);
        await storage.clearFile(currentDMFile);
        for (const r of records) {
            await storage.writeRecord(currentDMFile, r);
        }
    }

    await refreshCache();
    await renderRecords(currentDMFile);
}

/**
 * Clear all records in current file
 */
export async function clearRecords() {
    if (!currentDMFile) return;
    if (!confirm(`Supprimer tous les enregistrements de ${currentDMFile} ?`)) return;

    await storage.clearFile(currentDMFile);
    await refreshCache();
    await renderRecords(currentDMFile);
}

/**
 * Generate COBOL code for file definition
 */
export async function generateFileCode(fileName) {
    const meta = await storage.getFileMetadata(fileName);
    const records = await storage.readAllRecords(fileName);

    let code = `       SELECT ${fileName} ASSIGN TO '${fileName}.DAT'\n`;
    code += `           ORGANIZATION IS ${meta?.organization || 'SEQUENTIAL'}\n`;

    if (meta?.organization === 'INDEXED' && meta?.recordKey) {
        code += `           ACCESS MODE IS DYNAMIC\n`;
        code += `           RECORD KEY IS ${meta.recordKey}.\n`;
    } else {
        code += `           ACCESS MODE IS SEQUENTIAL.\n`;
    }

    // Generate record structure from data
    if (records.length > 0) {
        code += `\n       FD ${fileName}.\n`;
        code += `       01 ${fileName}-REC.\n`;

        const fields = Object.keys(records[0]);
        for (const field of fields) {
            const sample = records[0][field] || '';
            const length = Math.max(sample.length, 10);
            const isNumeric = /^\d+$/.test(sample);
            const pic = isNumeric ? `9(${length})` : `X(${length})`;
            code += `           05 ${field.toUpperCase().replace(/_/g, '-')} PIC ${pic}.\n`;
        }
    }

    setCode(code);
    switchTab('tab-editor');
}

/**
 * Export file as JSON
 */
export async function exportFile(fileName) {
    const meta = await storage.getFileMetadata(fileName);
    const records = await storage.readAllRecords(fileName);

    const data = {
        name: fileName,
        metadata: meta,
        records: records
    };

    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `${fileName}.json`;
    a.click();
    URL.revokeObjectURL(url);
}

/**
 * Import file from JSON
 */
export async function importFile() {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.json';

    input.onchange = async (e) => {
        const file = e.target.files[0];
        if (!file) return;

        const text = await file.text();
        const data = JSON.parse(text);

        if (data.name && data.records) {
            await storage.importRecords(data.name, data.records, data.metadata || {});
            await refreshCache();
            await renderFileList();
            await selectFile(data.name);
        }
    };

    input.click();
}

/**
 * Export all files
 */
export async function exportAllFiles() {
    const data = await storage.exportAllData();

    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'cobol-files-backup.json';
    a.click();
    URL.revokeObjectURL(url);
}

/**
 * Submit I/O input
 */
export async function submitIOInput() {
    await addRecord();
}

// Export the storage module for direct access if needed
export { storage };
