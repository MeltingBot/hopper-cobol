/**
 * Data Manager Module
 * Handles COBOL file and record management
 */

import { logIO, openModal, closeModal } from './utils.js';
import { setCode, generateCards } from './editor.js';
import { switchTab } from './utils.js';

// State
let cobolFiles = {};
let currentDMFile = null;
let editingRecordIndex = -1;

// Default sample files
const DEFAULT_FILES = {
    'CLIENTS': {
        name: 'CLIENTS',
        organization: 'INDEXED',
        key: 'CLIENT-ID',
        record: '01 REC.\n   05 CLIENT-ID PIC 9(6).\n   05 CLIENT-NOM PIC X(20).',
        fields: [
            { name: 'CLIENT-ID', pic: '9(6)', length: 6 },
            { name: 'CLIENT-NOM', pic: 'X(20)', length: 20 }
        ],
        records: [
            { 'CLIENT-ID': '000001', 'CLIENT-NOM': 'DUPONT JEAN' },
            { 'CLIENT-ID': '000002', 'CLIENT-NOM': 'MARTIN MARIE' }
        ]
    },
    'PRODUITS': {
        name: 'PRODUITS',
        organization: 'SEQUENTIAL',
        key: null,
        record: '01 REC.\n   05 PROD-CODE PIC X(8).\n   05 PROD-NOM PIC X(20).',
        fields: [
            { name: 'PROD-CODE', pic: 'X(8)', length: 8 },
            { name: 'PROD-NOM', pic: 'X(20)', length: 20 }
        ],
        records: [
            { 'PROD-CODE': 'CLV-001', 'PROD-NOM': 'CLAVIER' }
        ]
    }
};

/**
 * Get all files (for interpreter access)
 * @returns {object} All COBOL files
 */
export function getFiles() {
    return cobolFiles;
}

/**
 * Initialize the data manager
 */
export function initDataManager() {
    const stored = localStorage.getItem('cobolFiles');
    cobolFiles = stored ? JSON.parse(stored) : { ...DEFAULT_FILES };

    renderFileList();
    updateFileSelect();
}

/**
 * Save files to localStorage
 */
function saveFiles() {
    localStorage.setItem('cobolFiles', JSON.stringify(cobolFiles));
}

/**
 * Render the file list
 */
export function renderFileList() {
    const list = document.getElementById('fileList');
    if (!list) return;

    const files = Object.values(cobolFiles);

    if (!files.length) {
        list.innerHTML = '<div class="empty-state">Aucun fichier</div>';
        return;
    }

    list.innerHTML = files.map(file => {
        const badgeClass = file.organization === 'SEQUENTIAL' ? 'seq' : 'idx';
        return `
            <div class="file-item${currentDMFile === file.name ? ' selected' : ''}"
                 onclick="window.dataManagerModule.selectFile('${file.name}')">
                <div class="file-name">${file.name}</div>
                <div class="file-meta">
                    <span class="file-badge ${badgeClass}">${file.organization}</span>
                    <span>📄 ${file.records.length}</span>
                    ${file.key ? '<span>🔑 ' + file.key + '</span>' : ''}
                </div>
                <div class="file-actions">
                    <button class="file-action-btn" onclick="event.stopPropagation();window.dataManagerModule.generateFileCode('${file.name}')">📝</button>
                    <button class="file-action-btn" onclick="event.stopPropagation();window.dataManagerModule.exportFile('${file.name}')">📤</button>
                    <button class="file-action-btn delete" onclick="event.stopPropagation();window.dataManagerModule.deleteFile('${file.name}')">🗑️</button>
                </div>
            </div>
        `;
    }).join('');
}

/**
 * Update the file select dropdown
 */
export function updateFileSelect() {
    const select = document.getElementById('recordFileSelect');
    if (!select) return;

    select.innerHTML = '<option value="">-- Fichier --</option>' +
        Object.keys(cobolFiles).map(name => `<option value="${name}">${name}</option>`).join('');
}

/**
 * Select a file
 * @param {string} name - File name to select
 */
export function selectFile(name) {
    currentDMFile = name;
    renderFileList();

    const select = document.getElementById('recordFileSelect');
    if (select) select.value = name;

    loadFileRecords();
}

/**
 * Load records for the selected file
 */
export function loadFileRecords() {
    const select = document.getElementById('recordFileSelect');
    const schema = document.getElementById('recordSchema');
    const list = document.getElementById('recordList');

    if (!select || !schema || !list) return;

    const name = select.value;

    if (!name || !cobolFiles[name]) {
        schema.textContent = 'Sélectionnez un fichier';
        list.innerHTML = '';
        currentDMFile = null;
        return;
    }

    currentDMFile = name;
    const file = cobolFiles[name];

    schema.innerHTML = '<strong>Schema:</strong> ' +
        file.fields.map(f => f.name + '(' + f.pic + ')').join(' | ');

    if (!file.records.length) {
        list.innerHTML = '<div class="empty-state">Vide</div>';
        return;
    }

    list.innerHTML = file.records.map((record, index) => {
        const key = file.key ? record[file.key] : String(index + 1);
        const data = file.fields.map(f => record[f.name] || '').join(' ');
        return `
            <div class="record-item">
                <span class="record-key">[${key}]</span>
                <span class="record-data">${data}</span>
                <div class="record-actions">
                    <button class="file-action-btn" onclick="window.dataManagerModule.editRecord(${index})">✏️</button>
                    <button class="file-action-btn delete" onclick="window.dataManagerModule.deleteRecord(${index})">🗑️</button>
                </div>
            </div>
        `;
    }).join('');
}

/**
 * Open the create file modal
 */
export function createNewFile() {
    const nameInput = document.getElementById('newFileName');
    const orgSelect = document.getElementById('newFileOrg');
    const keyInput = document.getElementById('newFileKey');
    const recordTextarea = document.getElementById('newFileRecord');

    if (nameInput) nameInput.value = '';
    if (orgSelect) orgSelect.value = 'SEQUENTIAL';
    if (keyInput) keyInput.value = '';
    if (recordTextarea) recordTextarea.value = '01 REC.\n   05 FIELD-1 PIC X(10).\n   05 FIELD-2 PIC 9(5).';

    openModal('fileModal');
}

/**
 * Save the new file from modal
 */
export function saveNewFile() {
    const name = document.getElementById('newFileName')?.value.toUpperCase().trim();
    const org = document.getElementById('newFileOrg')?.value;
    const key = document.getElementById('newFileKey')?.value.toUpperCase().trim();
    const rec = document.getElementById('newFileRecord')?.value;

    if (!name) {
        alert('Nom requis');
        return;
    }

    // Parse fields from record definition
    const fields = [];
    rec.split('\n').forEach(line => {
        const match = line.match(/\d+\s+(\S+)\s+PIC\s+(\S+)/i);
        if (match) {
            const fieldName = match[1].replace('.', '');
            const pic = match[2].replace('.', '');
            let length = 1;
            const lengthMatch = pic.match(/[X9]\((\d+)\)/);
            if (lengthMatch) length = parseInt(lengthMatch[1]);
            fields.push({ name: fieldName, pic, length });
        }
    });

    cobolFiles[name] = {
        name,
        organization: org,
        key: org === 'INDEXED' ? (key || fields[0]?.name) : null,
        record: rec,
        fields,
        records: []
    };

    saveFiles();
    renderFileList();
    updateFileSelect();
    closeModal('fileModal');
    logIO('file-op', 'CRÉÉ: ' + name);
}

/**
 * Delete a file
 * @param {string} name - File name to delete
 */
export function deleteFile(name) {
    if (!confirm('Supprimer ' + name + '?')) return;

    delete cobolFiles[name];
    saveFiles();
    renderFileList();
    updateFileSelect();

    if (currentDMFile === name) {
        currentDMFile = null;
        loadFileRecords();
    }

    logIO('file-op', 'SUPPRIMÉ: ' + name);
}

/**
 * Open the add record modal
 */
export function addRecord() {
    if (!currentDMFile) {
        alert('Sélectionnez un fichier');
        return;
    }

    editingRecordIndex = -1;
    const file = cobolFiles[currentDMFile];
    const form = document.getElementById('recordForm');

    if (form) {
        form.innerHTML = file.fields.map(field => `
            <div class="form-group">
                <label>${field.name} (${field.pic})</label>
                <input type="text" id="field-${field.name}" maxlength="${field.length}">
            </div>
        `).join('');
    }

    openModal('recordModal');
}

/**
 * Open the edit record modal
 * @param {number} index - Record index to edit
 */
export function editRecord(index) {
    if (!currentDMFile) return;

    editingRecordIndex = index;
    const file = cobolFiles[currentDMFile];
    const record = file.records[index];
    const form = document.getElementById('recordForm');

    if (form) {
        form.innerHTML = file.fields.map(field => `
            <div class="form-group">
                <label>${field.name}</label>
                <input type="text" id="field-${field.name}" maxlength="${field.length}" value="${record[field.name] || ''}">
            </div>
        `).join('');
    }

    openModal('recordModal');
}

/**
 * Save the record from modal
 */
export function saveRecord() {
    if (!currentDMFile) return;

    const file = cobolFiles[currentDMFile];
    const record = {};

    file.fields.forEach(field => {
        let value = document.getElementById('field-' + field.name)?.value.toUpperCase() || '';
        // Pad based on field type
        value = field.pic.includes('9')
            ? value.padStart(field.length, '0')
            : value.padEnd(field.length, ' ');
        record[field.name] = value.substring(0, field.length);
    });

    if (editingRecordIndex >= 0) {
        file.records[editingRecordIndex] = record;
    } else {
        file.records.push(record);
    }

    saveFiles();
    loadFileRecords();
    closeModal('recordModal');
    logIO('file-op', editingRecordIndex >= 0 ? 'REWRITE' : 'WRITE');
}

/**
 * Delete a record
 * @param {number} index - Record index to delete
 */
export function deleteRecord(index) {
    if (!currentDMFile) return;

    cobolFiles[currentDMFile].records.splice(index, 1);
    saveFiles();
    loadFileRecords();
    logIO('file-op', 'DELETE');
}

/**
 * Clear all records in current file
 */
export function clearRecords() {
    if (!currentDMFile || !confirm('Vider?')) return;

    cobolFiles[currentDMFile].records = [];
    saveFiles();
    loadFileRecords();
}

/**
 * Generate COBOL code for a file
 * @param {string} name - File name
 */
export function generateFileCode(name) {
    const file = cobolFiles[name];
    if (!file) return;

    const code = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. ${name}-PROG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ${name} ASSIGN TO '${name}.DAT'
               ORGANIZATION IS ${file.organization}${file.key ? `
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ${file.key}` : ''}.
       DATA DIVISION.
       FILE SECTION.
       FD ${name}.
       ${file.record}
       WORKING-STORAGE SECTION.
       01 WS-EOF PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT ${name}.
           PERFORM UNTIL WS-EOF = 'Y'
               READ ${name}
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END DISPLAY ${file.fields[0].name}
               END-READ
           END-PERFORM.
           CLOSE ${name}.
           STOP RUN.`;

    setCode(code);

    // Switch to editor tab
    document.querySelectorAll('.header-tab').forEach(tab => tab.classList.remove('active'));
    document.querySelectorAll('.header-tab')[0].classList.add('active');
    document.querySelectorAll('.tab-content').forEach(content => content.classList.remove('active'));
    document.getElementById('tab-editor')?.classList.add('active');

    generateCards();
}

/**
 * Export a single file as .DAT
 * @param {string} name - File name to export
 */
export function exportFile(name) {
    const file = cobolFiles[name];
    if (!file) return;

    const content = file.records.map(record =>
        file.fields.map(field => (record[field.name] || '').padEnd(field.length, ' ')).join('')
    ).join('\n');

    const blob = new Blob([content], { type: 'text/plain' });
    const link = document.createElement('a');
    link.href = URL.createObjectURL(blob);
    link.download = name + '.DAT';
    link.click();
}

/**
 * Export all files as JSON
 */
export function exportAllFiles() {
    const blob = new Blob([JSON.stringify(cobolFiles, null, 2)], { type: 'application/json' });
    const link = document.createElement('a');
    link.href = URL.createObjectURL(blob);
    link.download = 'cobol-files.json';
    link.click();
}

/**
 * Import files from JSON
 */
export function importFile() {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.json';

    input.onchange = (event) => {
        const reader = new FileReader();
        reader.onload = (e) => {
            try {
                Object.assign(cobolFiles, JSON.parse(e.target.result));
                saveFiles();
                renderFileList();
                updateFileSelect();
            } catch (err) {
                alert('Erreur');
            }
        };
        reader.readAsText(event.target.files[0]);
    };

    input.click();
}

/**
 * Submit I/O input
 */
export function submitIOInput() {
    const input = document.getElementById('ioInput');
    if (input && input.value.trim()) {
        logIO('input', input.value);
        input.value = '';
    }
}
