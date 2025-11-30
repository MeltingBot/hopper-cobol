/**
 * Main Entry Point
 * IBM 029 Keypunch COBOL Card Generator
 * Initializes all modules and sets up global event handlers
 */

import { switchTab, closeModal, initResizers } from './utils.js';
import * as editor from './editor.js';
import * as dataManager from './dataManagerIDB.js';
import * as tutorial from './tutorial.js';

// Expose modules globally for HTML onclick handlers
window.editorModule = editor;
window.dataManagerModule = dataManager;
window.tutorialModule = tutorial;

// Global utility functions for HTML
window.switchTab = switchTab;
window.closeModal = closeModal;

// Editor functions
window.generateCards = editor.generateCards;
window.compileOnly = editor.compileOnly;
window.runProgram = editor.runProgram;
window.compileAndRun = editor.compileAndRun;
window.clearAll = editor.clearAll;
window.downloadCards = editor.downloadCards;
window.downloadAs = editor.downloadAs;
window.importCobFile = editor.importCobFile;
window.handleCobFileImport = editor.handleCobFileImport;
window.prevCard = editor.prevCard;
window.nextCard = editor.nextCard;

// Step mode functions (card reading)
window.startStepMode = editor.startStepMode;
window.stopStepMode = editor.stopStepMode;
window.stepNext = editor.stepNext;
window.stepPrevious = editor.stepPrevious;

// Debug mode functions (real step-by-step execution)
window.startDebugMode = editor.startDebugMode;
window.stopDebugMode = editor.stopDebugMode;
window.debugStepNext = editor.debugStepNext;
window.debugContinue = editor.debugContinue;

// Console input (in editor tab)
window.submitConsoleInput = editor.submitConsoleInput;

// Terminal modal functions
window.submitTerminalInput = editor.submitTerminalInput;
window.closeTerminal = editor.closeTerminal;

// Data manager functions
window.createNewFile = dataManager.createNewFile;
window.saveNewFile = dataManager.saveNewFile;
window.importFile = dataManager.importFile;
window.exportAllFiles = dataManager.exportAllFiles;
window.addRecord = dataManager.addRecord;
window.editRecord = dataManager.editRecord;
window.deleteRecord = dataManager.deleteRecord;
window.saveRecord = dataManager.saveRecord;
window.clearRecords = dataManager.clearRecords;

// I/O Input handling - check if editor is waiting for input
window.submitIOInput = function() {
    const input = document.getElementById('ioInput');
    if (input && input.value.trim()) {
        // Check if program is waiting for input
        if (editor.isWaitingForInput()) {
            editor.provideInput(input.value.trim());
        } else {
            // Normal data manager input
            dataManager.submitIOInput();
        }
        input.value = '';
    }
};

// Tutorial functions
window.prevLesson = tutorial.prevLesson;
window.nextLesson = tutorial.nextLesson;
window.loadExample = tutorial.loadExample;
window.selectExample = tutorial.selectExample;

/**
 * Initialize the application
 */
function init() {
    editor.initEditor();
    dataManager.initDataManager();
    tutorial.initTutorial();
    initResizers();

    console.log('HOPPER - COBOL Emulator initialized');
    console.log('In memory of Grace Hopper (1906-1992), mother of COBOL');
}

// Initialize when DOM is ready
document.addEventListener('DOMContentLoaded', init);
