/**
 * Main Entry Point
 * IBM 029 Keypunch COBOL Card Generator
 * Initializes all modules and sets up global event handlers
 */

import { switchTab, closeModal, initResizers } from './utils.js';
import * as editor from './editor.js';
import * as dataManager from './dataManagerIDB.js';
import * as tutorial from './tutorial.js';
import { DiskView } from './diskView.js';

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
window.changeDialect = editor.changeDialect;

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
window.debugRunToBreakpoint = editor.debugRunToBreakpoint;
window.toggleDebugCollapse = editor.toggleDebugCollapse;

// Printer function
window.printOutput = editor.printOutput;

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
 * Fake mainframe boot sequence for console enthusiasts
 */
function mainframeBoot() {
    const cyan = 'color: #00d4ff; font-weight: bold;';
    const green = 'color: #39ff14; font-weight: bold;';
    const yellow = 'color: #ffcc00;';
    const white = 'color: #ffffff;';
    const dim = 'color: #888888;';
    const bold = 'color: #ffffff; font-weight: bold; font-size: 14px;';

    console.clear();
    console.log('%c╔════════════════════════════════════════════════════════════════╗', cyan);
    console.log('%c║                                                                ║', cyan);
    console.log('%c║   ██╗  ██╗ ██████╗ ██████╗ ██████╗ ███████╗██████╗            ║', cyan);
    console.log('%c║   ██║  ██║██╔═══██╗██╔══██╗██╔══██╗██╔════╝██╔══██╗           ║', cyan);
    console.log('%c║   ███████║██║   ██║██████╔╝██████╔╝█████╗  ██████╔╝           ║', cyan);
    console.log('%c║   ██╔══██║██║   ██║██╔═══╝ ██╔═══╝ ██╔══╝  ██╔══██╗           ║', cyan);
    console.log('%c║   ██║  ██║╚██████╔╝██║     ██║     ███████╗██║  ██║           ║', cyan);
    console.log('%c║   ╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚═╝     ╚══════╝╚═╝  ╚═╝           ║', cyan);
    console.log('%c║                                                                ║', cyan);
    console.log('%c║            COBOL EMULATOR - VIRTUAL MAINFRAME                 ║', cyan);
    console.log('%c╚════════════════════════════════════════════════════════════════╝', cyan);
    console.log('');
    console.log('%c MAINFRAME SYSTEM INITIALIZATION', bold);
    console.log('%c ═══════════════════════════════════════════════════════════════', dim);
    console.log('');
    console.log('%c IPL FROM DEVICE 0A80', yellow);
    console.log('%c PROCESSOR COMPLEX ID: HOPPER-VM', white);
    console.log('%c MODEL: VIRTUAL  SERIAL: GH-1906-1992', white);
    console.log('');
    console.log('%c STORAGE', green);
    console.log('%c   CENTRAL STORAGE ........ 256M (BROWSER HEAP)', dim);
    console.log('%c   EXPANDED STORAGE ....... INDEXEDDB VSAM', dim);
    console.log('%c   VIRTUAL STORAGE ........ 2G ADDRESS SPACE', dim);
    console.log('');
    console.log('%c DEVICES', green);
    console.log('%c   0A80 ................... IPL DEVICE (ES MODULE)', dim);
    console.log('%c   00C0-00C2 .............. CARD READER/PUNCH', dim);
    console.log('%c   0580-058F .............. DASD STORAGE (INDEXEDDB)', dim);
    console.log('%c   0700 ................... 3270 CONSOLE (DOM)', dim);
    console.log('');
    console.log('%c SUBSYSTEMS', green);
    console.log('%c   JOB SCHEDULER .......... ACTIVE', dim);
    console.log('%c   NETWORK MANAGER ........ ACTIVE', dim);
    console.log('%c   TERMINAL SERVICES ...... READY', dim);
    console.log('%c   COBOL COMPILER ......... COBOL-85 V1.0', dim);
    console.log('%c   RUNTIME ................ HOPPER/JS V1.0', dim);
    console.log('');
    console.log('%c ═══════════════════════════════════════════════════════════════', dim);
    console.log('%c SYS001 SYSTEM READY', green);
    console.log('%c JOB003 HOPPER - STARTED - TIME=' + new Date().toTimeString().slice(0,8), green);
    console.log('');
    console.log('%c ┌─────────────────────────────────────────────────────────────┐', dim);
    console.log('%c │  In memory of Rear Admiral Grace Hopper (1906-1992)        │', dim);
    console.log('%c │  Pioneer of computer programming, co-inventor of COBOL     │', dim);
    console.log('%c │  "The most dangerous phrase is: We\'ve always done it       │', dim);
    console.log('%c │   this way."                                               │', dim);
    console.log('%c └─────────────────────────────────────────────────────────────┘', dim);
    console.log('');
    console.log('%c Type %cwindow.HOPPER%c to access runtime API', dim, 'color: #ff6b6b; font-weight: bold;', dim);
}

/**
 * Initialize terminal console redirection
 */
function initTerminalConsole() {
    const terminalOutput = document.getElementById('terminalOutput');
    const terminalInput = document.getElementById('terminalInput');

    // Function to append text to terminal
    window.terminalWrite = (text, className = '') => {
        const line = document.createElement('div');
        line.className = className;
        line.textContent = text;
        terminalOutput.appendChild(line);
        terminalOutput.scrollTop = terminalOutput.scrollHeight;
    };

    // Function to clear terminal
    window.terminalClear = () => {
        terminalOutput.innerHTML = '';
    };

    // Handle terminal input - delegate to submitTerminalInput from editor.js
    // The onkeypress handler in HTML calls submitTerminalInput() directly

    // Welcome message
    window.terminalWrite('HOPPER - READY', 'terminal-header');
}

/**
 * Initialize the application
 */
function init() {
    // Easter egg: Fake mainframe boot
    mainframeBoot();

    editor.initEditor();
    editor.initDebugResizer();
    dataManager.initDataManager();
    tutorial.initTutorial();
    initResizers();

    // Initialize Disk View for terminal tab
    window.diskView = new DiskView('diskViewContainer');

    // Redirect console output to terminal
    initTerminalConsole();

    // Expose runtime API for console hackers
    window.HOPPER = {
        version: '1.0.0',
        author: 'MeltingBot',
        tribute: 'Grace Hopper (1906-1992)',
        modules: {
            editor,
            dataManager,
            tutorial
        },
        help: () => {
            console.log('%c HOPPER Runtime API', 'color: #00d4ff; font-weight: bold;');
            console.log('%c ═════════════════', 'color: #888;');
            console.log('  HOPPER.modules.editor     - Editor functions');
            console.log('  HOPPER.modules.dataManager - File I/O');
            console.log('  HOPPER.modules.tutorial   - Examples');
            console.log('  HOPPER.reboot()           - Show boot sequence');
        },
        reboot: mainframeBoot
    };
}

// Initialize when DOM is ready
document.addEventListener('DOMContentLoaded', init);
