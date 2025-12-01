/**
 * Editor Module
 * Handles COBOL code editing, card generation, and program execution
 */

import { renderPunchCard, renderPunchCardAnimated, stopPunchAnimation } from './punchCard.js';
import { setStatus, showOutput, showOutputCursor, hideOutputCursor, updateCardCount, openModal, closeModal, downloadFile, logIO } from './utils.js';
import { CobolRuntime, CobolDialect } from './cobol/index.js';
import { explainStatement, getCategoryInfo, getShortSummary } from './cobol/explanations.js';
import { printCompilationOutput, closePrinter } from './printer.js';

// State
let cards = [];
let currentCardIndex = 0;
let cobolRuntime = null;
let sourceCode = '';        // Full source code (including comments) for compilation
let currentDialect = CobolDialect.COBOL_85;  // Current dialect setting

// Workflow state
let isPunched = false;      // Cards have been punched
let isCompiled = false;     // Program has been compiled successfully

// Step mode state
let stepMode = false;
let stepCardIndex = 0;
let steppedCards = [];

// Debug mode state
let debugMode = false;
let debugInterpreter = null;
let previousVariables = {};
let breakpoints = new Set();  // Set of line numbers with breakpoints

// Screen control state (IBM 3270 style - 24 lines x 80 columns)
const SCREEN_ROWS = 24;
const SCREEN_COLS = 80;
let screenBuffer = null;     // 2D array of {char, highlight, blink, reverse, underline, fgColor, bgColor}
let screenCursorLine = 1;    // 1-based line position
let screenCursorCol = 1;     // 1-based column position
let screenModeEnabled = false;

/**
 * Initialize screen buffer for screen control mode
 */
function initScreenBuffer() {
    screenBuffer = [];
    for (let row = 0; row < SCREEN_ROWS; row++) {
        screenBuffer[row] = [];
        for (let col = 0; col < SCREEN_COLS; col++) {
            screenBuffer[row][col] = {
                char: ' ',
                highlight: false,
                blink: false,
                reverse: false,
                underline: false,
                fgColor: null,
                bgColor: null
            };
        }
    }
    screenCursorLine = 1;
    screenCursorCol = 1;
    screenModeEnabled = true;
}

/**
 * Clear screen buffer
 */
function clearScreen() {
    if (!screenBuffer) initScreenBuffer();
    for (let row = 0; row < SCREEN_ROWS; row++) {
        for (let col = 0; col < SCREEN_COLS; col++) {
            screenBuffer[row][col] = {
                char: ' ',
                highlight: false,
                blink: false,
                reverse: false,
                underline: false,
                fgColor: null,
                bgColor: null
            };
        }
    }
    screenCursorLine = 1;
    screenCursorCol = 1;
}

/**
 * Erase from cursor to end of screen (EOS)
 */
function eraseToEndOfScreen() {
    if (!screenBuffer) return;
    const startRow = Math.max(0, Math.min(SCREEN_ROWS - 1, screenCursorLine - 1));
    const startCol = Math.max(0, Math.min(SCREEN_COLS - 1, screenCursorCol - 1));

    // Erase rest of current line
    if (screenBuffer[startRow]) {
        for (let col = startCol; col < SCREEN_COLS; col++) {
            screenBuffer[startRow][col] = { char: ' ', highlight: false, blink: false, reverse: false, underline: false, fgColor: null, bgColor: null };
        }
    }
    // Erase all following lines
    for (let row = startRow + 1; row < SCREEN_ROWS; row++) {
        if (screenBuffer[row]) {
            for (let col = 0; col < SCREEN_COLS; col++) {
                screenBuffer[row][col] = { char: ' ', highlight: false, blink: false, reverse: false, underline: false, fgColor: null, bgColor: null };
            }
        }
    }
}

/**
 * Erase from cursor to end of line (EOL)
 */
function eraseToEndOfLine() {
    if (!screenBuffer) return;
    const row = Math.max(0, Math.min(SCREEN_ROWS - 1, screenCursorLine - 1));
    const startCol = Math.max(0, Math.min(SCREEN_COLS - 1, screenCursorCol - 1));

    if (screenBuffer[row]) {
        for (let col = startCol; col < SCREEN_COLS; col++) {
            screenBuffer[row][col] = { char: ' ', highlight: false, blink: false, reverse: false, underline: false, fgColor: null, bgColor: null };
        }
    }
}

/**
 * Write text to screen buffer at specified position
 */
function writeToScreen(text, options = {}) {
    if (!screenBuffer) initScreenBuffer();

    const line = options.line || screenCursorLine;
    const col = options.column || screenCursorCol;
    const row = Math.max(0, Math.min(SCREEN_ROWS - 1, line - 1));
    let currentCol = Math.max(0, Math.min(SCREEN_COLS - 1, col - 1));

    // Ensure row exists in buffer (defensive check)
    if (!screenBuffer[row]) {
        console.warn(`writeToScreen: row ${row} not in buffer, reinitializing`);
        initScreenBuffer();
    }

    for (let i = 0; i < text.length && currentCol < SCREEN_COLS; i++) {
        const ch = text[i];
        if (ch === '\n') {
            // Don't handle newlines in screen mode - use LINE positioning
            continue;
        }
        screenBuffer[row][currentCol] = {
            char: ch,
            highlight: options.highlight || false,
            blink: options.blink || false,
            reverse: options.reverseVideo || false,
            underline: options.underline || false,
            fgColor: options.foregroundColor || null,
            bgColor: options.backgroundColor || null
        };
        currentCol++;
    }

    // Update cursor position to end of written text
    screenCursorLine = line;
    screenCursorCol = currentCol + 1;
}

/**
 * Render screen buffer to terminal output
 */
function renderScreenBuffer() {
    if (!screenBuffer) return;

    const output = document.getElementById('terminalOutput');
    if (!output) return;

    // Clear existing output
    output.innerHTML = '';

    // Create screen grid container
    const grid = document.createElement('div');
    grid.className = 'screen-grid';

    for (let row = 0; row < SCREEN_ROWS; row++) {
        const lineDiv = document.createElement('div');
        lineDiv.className = 'screen-line';

        for (let col = 0; col < SCREEN_COLS; col++) {
            const cell = screenBuffer[row][col];
            const span = document.createElement('span');
            span.className = 'screen-cell';
            span.textContent = cell.char;

            // Apply attributes
            if (cell.highlight) span.classList.add('highlight');
            if (cell.blink) span.classList.add('blink');
            if (cell.reverse) span.classList.add('reverse');
            if (cell.underline) span.classList.add('underline');
            if (cell.fgColor !== null) span.style.color = getColor(cell.fgColor);
            if (cell.bgColor !== null) span.style.backgroundColor = getColor(cell.bgColor);

            lineDiv.appendChild(span);
        }
        grid.appendChild(lineDiv);
    }

    output.appendChild(grid);
}

/**
 * Convert COBOL color number to CSS color
 */
function getColor(colorNum) {
    const colors = {
        0: '#000000',  // Black
        1: '#0000aa',  // Blue
        2: '#00aa00',  // Green
        3: '#00aaaa',  // Cyan
        4: '#aa0000',  // Red
        5: '#aa00aa',  // Magenta
        6: '#aa5500',  // Brown/Yellow
        7: '#aaaaaa',  // White
    };
    return colors[colorNum] || colors[2]; // Default to green
}

// Default COBOL program with screen control demo
const DEFAULT_CODE = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(15).
       01 WS-COUNT PIC 99 VALUE 0.
       PROCEDURE DIVISION.
      *    Clear screen and display title
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== HOPPER COBOL ===' LINE 2 POSITION 30
               HIGHLIGHT.
           DISPLAY 'Demonstrateur IBM 3270' LINE 3 POSITION 28.
      *    Form display with positioning
           DISPLAY 'Entrez votre nom:' LINE 6 POSITION 10.
           ACCEPT WS-NAME LINE 6 POSITION 28.
           DISPLAY 'Bonjour, ' LINE 8 POSITION 10 HIGHLIGHT.
           DISPLAY WS-NAME LINE 8 POSITION 19 REVERSE-VIDEO.
      *    Counter with blinking
           DISPLAY 'Compteur:' LINE 11 POSITION 10.
           PERFORM 5 TIMES
               ADD 1 TO WS-COUNT
               DISPLAY WS-COUNT LINE 11 POSITION 20 BLINK
           END-PERFORM.
      *    End message
           DISPLAY 'Appuyez sur ENTREE...' LINE 23 POSITION 1.
           ACCEPT WS-NAME.
           DISPLAY 'FIN DU PROGRAMME' LINE 24 POSITION 32 HIGHLIGHT.
           STOP RUN.`;

/**
 * Initialize the editor
 */
export function initEditor() {
    const codeEditor = document.getElementById('codeEditor');
    const lineNumbers = document.getElementById('lineNumbers');
    if (!codeEditor) return;

    codeEditor.addEventListener('input', updateLineNumbers);
    codeEditor.addEventListener('scroll', syncLineNumbersScroll);
    codeEditor.value = DEFAULT_CODE;

    // Add click handler for breakpoints on line numbers (also works from editor)
    if (lineNumbers) {
        lineNumbers.addEventListener('click', (e) => {
            const lineSpan = e.target.closest('.line-number');
            if (lineSpan) {
                const lineNum = parseInt(lineSpan.dataset.line, 10);
                if (!isNaN(lineNum)) {
                    toggleBreakpoint(lineNum);
                }
            }
        });
    }

    updateLineNumbers();
    renderPunchCard('');
}

/**
 * Update line numbers display
 */
export function updateLineNumbers() {
    const codeEditor = document.getElementById('codeEditor');
    const lineNumbers = document.getElementById('lineNumbers');
    if (!codeEditor || !lineNumbers) return;

    const lineCount = codeEditor.value.split('\n').length;
    const lines = Array.from({ length: lineCount }, (_, i) => {
        const lineNum = i + 1;
        const hasBreakpoint = breakpoints.has(lineNum);
        return `<span class="line-number${hasBreakpoint ? ' breakpoint' : ''}" data-line="${lineNum}">${lineNum}</span>`;
    });
    lineNumbers.innerHTML = lines.join('<br>');
}

/**
 * Toggle breakpoint on a line (card number in hopper)
 */
export function toggleBreakpoint(lineNum) {
    if (breakpoints.has(lineNum)) {
        breakpoints.delete(lineNum);
        showOutput('info', `● Breakpoint retiré - carte ${lineNum}`);
    } else {
        breakpoints.add(lineNum);
        showOutput('info', `● Breakpoint ajouté - carte ${lineNum}`);
    }
    // Update both editor line numbers and card stack display
    updateLineNumbers();
    updateCardStack();
}

/**
 * Clear all breakpoints
 */
export function clearBreakpoints() {
    breakpoints.clear();
    updateLineNumbers();
}

/**
 * Check if line has a breakpoint
 */
export function hasBreakpoint(lineNum) {
    return breakpoints.has(lineNum);
}

/**
 * Sync line numbers scroll with editor
 */
function syncLineNumbersScroll() {
    const codeEditor = document.getElementById('codeEditor');
    const lineNumbers = document.getElementById('lineNumbers');
    if (codeEditor && lineNumbers) {
        lineNumbers.scrollTop = codeEditor.scrollTop;
    }
}

// Animation state
let punchingInProgress = false;

/**
 * Generate punch cards from the current code (Step 1: Punch)
 */
export function generateCards() {
    const codeEditor = document.getElementById('codeEditor');
    if (!codeEditor) return;

    const code = codeEditor.value;
    if (!code.trim()) {
        showOutput('error', 'Aucun code à perforer');
        return;
    }

    if (punchingInProgress) {
        showOutput('warning', 'Perforation en cours...');
        return;
    }

    // Stop any ongoing animation
    stopPunchAnimation();

    // Reset workflow state
    isPunched = false;
    isCompiled = false;
    cobolRuntime = null;
    punchingInProgress = true;
    updateWorkflowButtons();

    // Save full source code for compilation (with comments)
    sourceCode = code;

    // Filter out comment lines (* in column 7 or *> inline comments) for punch cards only
    const lines = code.split('\n');
    const filteredLines = lines.filter(line => {
        // Skip empty lines
        if (line.trim() === '') return false;
        // Skip full-line comments (* in column 7)
        if (line.length >= 7 && line[6] === '*') return false;
        // Skip lines that are only *> comments
        if (line.trim().startsWith('*>')) return false;
        return true;
    });

    const skippedCount = lines.length - filteredLines.length;

    cards = filteredLines.map((line, index) => ({
        seq: String(index + 1).padStart(6, '0'),
        text: line.padEnd(80, ' ').substring(0, 80),
        original: line
    }));

    if (skippedCount > 0) {
        showOutput('info', `${skippedCount} ligne(s) ignorée(s) (commentaires/vides)`);
    }

    // Clear both stations
    clearCardStations();
    updateCardCount(cards.length);

    // Simulate keypunch with card-by-card animation
    showOutput('system', '═══════════════════════════════════════');
    showOutput('info', '⚡ IBM 029 KEYPUNCH - PERFORATION...');
    setStatus('busy', 'PERFORATION');

    // Animate punching all cards
    punchAllCards(0, () => {
        showOutput('success', `✓ ${cards.length} carte(s) perforée(s)`);
        showOutput('info', '📋 Cartes prêtes pour la trémie');
        showOutput('system', '═══════════════════════════════════════');

        isPunched = true;
        punchingInProgress = false;
        updateWorkflowButtons();
        setStatus('ok', cards.length + ' CARTES');
    });
}

/**
 * Animate punching all cards
 * Fast mode for large decks, animated for small ones
 */
function punchAllCards(cardIndex, onComplete) {
    const totalCards = cards.length;

    // For large decks (>20 cards), use fast batch mode
    if (totalCards > 20) {
        punchCardsBatch(cardIndex, onComplete);
    } else {
        punchCardsSlow(cardIndex, onComplete);
    }
}

/**
 * Fast batch punching - shows progress but doesn't animate each card
 */
function punchCardsBatch(cardIndex, onComplete) {
    if (cardIndex >= cards.length) {
        // Show last card with quick animation
        showCardAnimated(cards.length - 1, onComplete);
        return;
    }

    // Batch size scales with deck size for consistent ~1s total
    const batchSize = cards.length > 200 ? 50 : (cards.length > 100 ? 25 : 10);
    const endIndex = Math.min(cardIndex + batchSize, cards.length);

    setStatus('busy', `CARTE ${endIndex}/${cards.length}`);

    // Add cards to stacker
    for (let i = cardIndex; i < endIndex; i++) {
        addCardToStacker(i);
    }

    // Just show the last card of this batch (no animation)
    showCard(endIndex - 1);

    // Continue with next batch
    setTimeout(() => {
        punchCardsBatch(endIndex, onComplete);
    }, 20); // 20ms per batch
}

/**
 * Slow animated punching for small decks
 */
function punchCardsSlow(cardIndex, onComplete) {
    if (cardIndex >= cards.length) {
        if (onComplete) onComplete();
        return;
    }

    setStatus('busy', `CARTE ${cardIndex + 1}/${cards.length}`);

    // Add card to stacker
    addCardToStacker(cardIndex);

    showCardAnimated(cardIndex, () => {
        setTimeout(() => {
            punchCardsSlow(cardIndex + 1, onComplete);
        }, 50);
    });
}

/**
 * Compile the punched cards (Step 2: Compile)
 * Animates card reading through the hopper
 */
export function compileOnly() {
    if (!isPunched || !cards.length) {
        showOutput('error', 'Perforez les cartes d\'abord');
        return;
    }

    if (compilingInProgress) {
        showOutput('warning', 'Compilation en cours...');
        return;
    }

    isCompiled = false;
    compilingInProgress = true;
    updateWorkflowButtons();

    showOutput('system', '═══════════════════════════════════════');
    showOutput('info', '⚙ TRANSFERT VERS LA TRÉMIE...');
    showOutput('info', `📚 ${cards.length} cartes`);
    setStatus('busy', 'TRANSFERT');

    // Transfer cards from stacker to hopper with animation
    transferToHopper(() => {
        showOutput('info', '⚙ LECTEUR DE CARTES - LECTURE...');
        setStatus('busy', 'LECTURE');

        // Animate card reading through hopper
        animateCardReading(0, () => {
        // All cards read - now compile
        showOutput('info', '⚙ COMPILATION EN COURS...');
        setStatus('busy', 'COMPILATION');

        // Use full source code (with comments) for compilation
        const code = sourceCode;

        // Clear output buffer for printer
        programOutputBuffer = [];

        // Create runtime with terminal callbacks and dialect (will be used during execution)
        cobolRuntime = new CobolRuntime({
            dialect: currentDialect,
            onDisplay: (msg) => {
                terminalOutput(msg);
                showOutput('output', msg); // Also show in main console
                programOutputBuffer.push(msg); // Store for printer
                // Also send to terminal tab
                if (window.terminalWrite) {
                    window.terminalWrite(msg);
                }
            },
            onDisplayWithOptions: (msg, options) => {
                terminalOutputWithOptions(msg, options);
                showOutput('output', msg); // Also show in main console
                programOutputBuffer.push(msg); // Store for printer
            },
            onAccept: (varName) => {
                // In screen mode, don't display "ACCEPT varName:" text on screen
                // Just position cursor for input
                if (screenModeEnabled && screenBuffer) {
                    // Show blinking cursor at current position
                    writeToScreen('_', {
                        line: screenCursorLine,
                        column: screenCursorCol,
                        blink: true
                    });
                    renderScreenBuffer();
                    window.currentAcceptOptions = {
                        line: screenCursorLine,
                        column: screenCursorCol
                    };
                } else {
                    // Show blinking cursor for input
                    terminalShowInputCursor();
                }
                setTerminalWaiting(true);
            },
            onAcceptWithOptions: (varName, options) => {
                terminalAcceptWithOptions(varName, options);
                setTerminalWaiting(true);
            },
            onError: (err) => {
                terminalOutput(err, 'error');
                showOutput('error', err);
            },
            onStatus: (status) => terminalOutput(status, 'system'),
            onDialectWarning: (warning) => {
                terminalOutput(`⚠ ${warning}`, 'warning');
                showOutput('warning', `⚠ ${warning}`);
            },
            dataManager: window.dataManagerModule?.getFilesSync() || {},
            onDiskIO: async (event) => {
                // Send I/O operations to disk view and terminal
                if (window.diskView) {
                    window.diskView.onDiskIO(event);

                    // For OPEN operations, get current record count from dataManager
                    if (event.operation === 'OPEN' && window.dataManagerModule) {
                        try {
                            const files = await window.dataManagerModule.getFiles();
                            const file = files.find(f => f.name.toUpperCase() === event.fileName.toUpperCase());
                            if (file) {
                                const dataset = window.diskView.datasets.get(event.fileName);
                                if (dataset) {
                                    dataset.records = file.records?.length || 0;
                                    window.diskView.render();
                                }
                            }
                        } catch (e) {
                            console.error('Failed to get file count:', e);
                        }
                    }
                }
                if (window.terminalWrite) {
                    const { operation, fileName, recordNumber } = event;
                    const msg = `[${operation}] ${fileName}${recordNumber !== undefined ? ` #${recordNumber}` : ''}`;
                    window.terminalWrite(msg, 'terminal-io');
                }
            }
        });

        // Small delay for visual effect
        setTimeout(() => {
            const result = cobolRuntime.compile(code);

            if (result.success) {
                showOutput('success', '✓ COMPILATION RÉUSSIE');
                showOutput('info', `Programme: ${result.programId}`);
                showOutput('info', '▶ Prêt pour exécution');
                showOutput('system', '═══════════════════════════════════════');
                isCompiled = true;
                compiledProgramId = result.programId;
                setStatus('ok', 'COMPILÉ');
            } else {
                showOutput('error', '✗ ERREURS DE COMPILATION');
                result.errors.forEach(err => showOutput('error', `  ${err}`));
                showOutput('info', '⚠ Corrigez et reperforez les cartes');
                showOutput('system', '═══════════════════════════════════════');
                setStatus('error', 'ERREUR');
            }

            compilingInProgress = false;
            resetCardStack();
            updateWorkflowButtons();
        }, 300);
        });
    });
}

// Animation state
let compilingInProgress = false;
let compiledProgramId = 'PROGRAM';

// Program output buffer for printer
let programOutputBuffer = [];

/**
 * Animate cards being read through the hopper
 * Fast batch mode for large decks
 */
function animateCardReading(cardIndex, onComplete) {
    if (cardIndex >= cards.length) {
        if (onComplete) onComplete();
        return;
    }

    // Batch size depends on deck size (faster for large decks)
    const batchSize = cards.length > 50 ? 20 : (cards.length > 20 ? 5 : 1);
    const endIndex = Math.min(cardIndex + batchSize, cards.length);

    const stack = document.getElementById('cardStack');
    if (stack) {
        const cardMinis = stack.querySelectorAll('.card-mini');

        // Mark all cards up to endIndex as processed
        cardMinis.forEach((el, i) => {
            el.classList.remove('being-read');
            if (i < endIndex) {
                el.classList.add('processed');
            }
        });

        // Mark last card of batch as being read
        if (cardMinis[endIndex - 1]) {
            cardMinis[endIndex - 1].classList.add('being-read');
        }
    }

    // Update hopper count
    const hopperCount = document.getElementById('hopperCount');
    if (hopperCount) {
        hopperCount.textContent = Math.max(0, cards.length - endIndex);
    }

    // Show current card in card reader
    showCard(endIndex - 1);

    // Continue to next batch
    setTimeout(() => {
        animateCardReading(endIndex, onComplete);
    }, 25); // 25ms per batch
}

/**
 * Reset card stack appearance after compilation
 */
function resetCardStack() {
    const stack = document.getElementById('cardStack');
    if (stack) {
        const cardMinis = stack.querySelectorAll('.card-mini');
        cardMinis.forEach(el => {
            el.classList.remove('being-read', 'processed');
        });
    }

    const hopperCount = document.getElementById('hopperCount');
    if (hopperCount) {
        hopperCount.textContent = cards.length;
    }
}

/**
 * Run the compiled program (Step 3: Execute)
 * Switches to Terminal tab and executes interactively
 */
export async function runProgram() {
    if (!isCompiled || !cobolRuntime) {
        showOutput('error', 'Compilez le programme d\'abord');
        return;
    }

    // Clear output buffer for fresh execution
    programOutputBuffer = [];

    // Switch to Terminal tab
    switchToTerminalTab();

    // Initialize terminal for execution
    initTerminalForExecution();

    showOutput('system', '═══════════════════════════════════════');
    showOutput('info', '▶ EXÉCUTION DANS LE TERMINAL...');
    showOutput('system', '═══════════════════════════════════════');

    setStatus('running', 'EXÉCUTION');

    try {
        await cobolRuntime.run();
        terminalOutput('─'.repeat(40), 'separator');
        terminalOutput('*** FIN D\'EXÉCUTION ***', 'system');
        setTerminalStatus('ended');
        setStatus('ok', 'TERMINÉ');
    } catch (error) {
        terminalOutput(`ERREUR: ${error.message}`, 'error');
        setTerminalStatus('ended');
        setStatus('error', 'ERREUR');
    }

    setTerminalWaiting(false);
    terminalHideInputCursor();
    highlightConsoleInput(false);

    // Refresh data manager to show any new files/records
    if (window.dataManagerModule?.renderFileList) {
        await window.dataManagerModule.renderFileList();
    }
}

// ============================================
// TERMINAL TAB FUNCTIONS
// ============================================

/**
 * Switch to Terminal tab programmatically
 */
function switchToTerminalTab() {
    const terminalTab = document.querySelector('[onclick*="terminal"]');
    if (terminalTab) {
        window.switchTab('terminal', terminalTab);
    }
}

/**
 * Initialize terminal for execution
 */
function initTerminalForExecution() {
    const output = document.getElementById('terminalOutput');
    const statusEl = document.getElementById('terminalStatus');
    const inputArea = document.getElementById('terminalInputArea');
    const terminalInput = document.getElementById('terminalInput');

    if (output) output.innerHTML = '';
    if (statusEl) {
        statusEl.textContent = 'RUNNING';
        statusEl.className = 'panel-badge running';
    }
    if (inputArea) inputArea.style.display = 'flex';
    if (terminalInput) {
        terminalInput.disabled = false;
        terminalInput.value = '';
    }

    terminalOutput('HOPPER 3270 TERMINAL - SESSION ACTIVE', 'system');
    terminalOutput(`PROGRAMME: ${compiledProgramId}`, 'system');
    terminalOutput('─'.repeat(40), 'separator');

    // Reset disk view
    if (window.diskView) {
        window.diskView.reset();
    }
}

/**
 * Close terminal (no longer needed, kept for compatibility)
 */
export function closeTerminal() {
    // Reset screen mode
    screenModeEnabled = false;
    screenBuffer = null;
    document.getElementById('terminalScreen')?.classList.remove('screen-mode');
}

/**
 * Add output to terminal
 */
function terminalOutput(msg, type = '') {
    const output = document.getElementById('terminalOutput');
    if (!output) return;

    // If screen mode is enabled, use screen buffer instead
    if (screenModeEnabled && screenBuffer) {
        // In screen mode, append at current cursor position, then move to next line
        writeToScreen(msg, {});
        screenCursorLine++;
        screenCursorCol = 1;
        renderScreenBuffer();
        return;
    }

    const line = document.createElement('div');
    line.className = 'line' + (type ? ` ${type}` : '');
    line.textContent = msg;
    output.appendChild(line);

    // Auto-scroll
    const screen = document.getElementById('terminalScreen');
    if (screen) screen.scrollTop = screen.scrollHeight;
}

/**
 * Show blinking cursor for ACCEPT input
 */
function terminalShowInputCursor() {
    const output = document.getElementById('terminalOutput');
    if (!output) return;

    const cursor = document.createElement('span');
    cursor.className = 'terminal-cursor blink';
    cursor.id = 'terminalInputCursor';
    cursor.textContent = '▌';

    const line = document.createElement('div');
    line.className = 'line';
    line.appendChild(cursor);
    output.appendChild(line);

    // Auto-scroll
    const screen = document.getElementById('terminalScreen');
    if (screen) screen.scrollTop = screen.scrollHeight;
}

/**
 * Remove blinking cursor after input received
 */
function terminalHideInputCursor() {
    const cursor = document.getElementById('terminalInputCursor');
    if (cursor) {
        cursor.remove();
    }
}

/**
 * Add output to terminal with screen control options
 */
function terminalOutputWithOptions(msg, options) {

    // Enable screen mode if not already enabled
    if (!screenModeEnabled) {
        initScreenBuffer();
        document.getElementById('terminalScreen')?.classList.add('screen-mode');
    }

    // Handle ERASE before writing
    if (options.erase) {
        if (options.line) {
            screenCursorLine = options.line;
            screenCursorCol = options.column || 1;
        }
        switch (options.erase) {
            case 'SCREEN':
                clearScreen();
                break;
            case 'EOS':
                eraseToEndOfScreen();
                break;
            case 'EOL':
                eraseToEndOfLine();
                break;
        }
    }

    // Handle BELL/BEEP
    if (options.bell) {
        // Play a beep sound (simple audio API beep)
        try {
            const audioCtx = new (window.AudioContext || window.webkitAudioContext)();
            const oscillator = audioCtx.createOscillator();
            oscillator.type = 'sine';
            oscillator.frequency.setValueAtTime(800, audioCtx.currentTime);
            oscillator.connect(audioCtx.destination);
            oscillator.start();
            oscillator.stop(audioCtx.currentTime + 0.1);
        } catch (e) {
            // Audio not supported, ignore
        }
    }

    // Write to screen buffer
    if (msg && msg.length > 0) {
        writeToScreen(msg, {
            line: options.line || screenCursorLine,
            column: options.column || screenCursorCol,
            highlight: options.highlight,
            blink: options.blink,
            reverseVideo: options.reverseVideo,
            underline: options.underline,
            foregroundColor: options.foregroundColor,
            backgroundColor: options.backgroundColor
        });
    }

    // Move cursor to next line unless NO ADVANCING
    if (!options.noAdvancing) {
        screenCursorLine++;
        screenCursorCol = 1;
    }

    // Render the updated screen
    renderScreenBuffer();
}

/**
 * Handle ACCEPT with screen control options
 * Shows input field at specified screen position
 */
function terminalAcceptWithOptions(varName, options) {
    const modal = document.getElementById('terminalModal');

    // Only if terminal is open
    if (!modal?.classList.contains('active')) return;

    // Enable screen mode if not already enabled
    if (!screenModeEnabled) {
        initScreenBuffer();
        document.getElementById('terminalScreen')?.classList.add('screen-mode');
    }

    // Position cursor for input
    if (options.line) {
        screenCursorLine = options.line;
    }
    if (options.column) {
        screenCursorCol = options.column;
    }

    // Show input prompt marker at position
    writeToScreen('_', {
        line: screenCursorLine,
        column: screenCursorCol,
        blink: true,
        reverseVideo: options.reverseVideo || false,
        highlight: options.highlight || false
    });

    renderScreenBuffer();

    // Store accept options for when input is received
    window.currentAcceptOptions = {
        line: screenCursorLine,
        column: screenCursorCol,
        secure: options.secure || false,
        size: options.size,
        highlight: options.highlight || false,
        reverseVideo: options.reverseVideo || false
    };
}

/**
 * Set terminal waiting state
 */
function setTerminalWaiting(waiting) {
    const statusEl = document.getElementById('terminalStatus');
    const input = document.getElementById('terminalInput');

    if (statusEl) {
        if (waiting) {
            statusEl.textContent = 'WAITING';
            statusEl.className = 'terminal-status waiting';
        } else {
            statusEl.textContent = 'RUNNING';
            statusEl.className = 'terminal-status';
        }
    }

    if (input && waiting) input.focus();
}

/**
 * Set terminal status
 */
function setTerminalStatus(status) {
    const statusEl = document.getElementById('terminalStatus');
    if (statusEl) {
        if (status === 'ended') {
            statusEl.textContent = 'ENDED';
            statusEl.className = 'terminal-status ended';
        }
    }
}

/**
 * Submit input from terminal
 */
export function submitTerminalInput() {
    const input = document.getElementById('terminalInput');
    if (!input) return;

    const value = input.value.trim();
    if (!value && !cobolRuntime?.isWaitingForInput()) return;

    // Check if we have screen control options for this accept
    if (window.currentAcceptOptions && screenModeEnabled) {
        const opts = window.currentAcceptOptions;
        // Write the input value at the accept position
        writeToScreen(opts.secure ? '*'.repeat(value.length) : value, {
            line: opts.line,
            column: opts.column,
            highlight: opts.highlight,
            reverseVideo: opts.reverseVideo
        });
        renderScreenBuffer();
        // Clear the stored options
        window.currentAcceptOptions = null;
    } else {
        // Show input in terminal (classic mode)
        terminalOutput(value, 'input');
    }

    // Provide to runtime
    if (cobolRuntime && cobolRuntime.isWaitingForInput()) {
        cobolRuntime.provideInput(value);
        setTerminalWaiting(false);
        terminalHideInputCursor();

        const statusEl = document.getElementById('terminalStatus');
        if (statusEl) {
            statusEl.textContent = debugMode ? 'DEBUG' : 'RUNNING';
            statusEl.className = 'terminal-status';
        }

        // Reset debug panel waiting indicator
        if (debugMode) {
            const debugStmt = document.getElementById('debugStmt');
            if (debugStmt) debugStmt.style.color = '';
        }
    }

    input.value = '';
}

/**
 * Update workflow buttons based on current state
 */
function updateWorkflowButtons() {
    const btnCompile = document.getElementById('btnCompile');
    const btnRun = document.getElementById('btnRun');
    const btnStep = document.getElementById('btnStep');

    if (btnCompile) btnCompile.disabled = !isPunched;
    if (btnRun) btnRun.disabled = !isCompiled;
    if (btnStep) btnStep.disabled = !isPunched;
}

/**
 * Highlight console input when waiting for ACCEPT
 */
function highlightConsoleInput(highlight) {
    const consoleInput = document.getElementById('consoleInput');
    const consoleInputArea = document.getElementById('consoleInputArea');
    const badge = document.getElementById('consoleBadge');

    if (consoleInput) {
        if (highlight) {
            consoleInput.focus();
            consoleInput.classList.add('waiting');
            consoleInput.placeholder = 'Tapez votre réponse...';
        } else {
            consoleInput.classList.remove('waiting');
            consoleInput.placeholder = 'Entrée...';
            hideOutputCursor();
        }
    }

    if (consoleInputArea) {
        consoleInputArea.classList.toggle('waiting', highlight);
    }

    if (badge) {
        badge.textContent = highlight ? 'WAITING' : 'IDLE';
    }
}

/**
 * Legacy function - compile and run in one step
 */
export async function compileAndRun() {
    generateCards();
    if (isPunched) {
        compileOnly();
        if (isCompiled) {
            await runProgram();
        }
    }
}

/**
 * Update the card stack display
 */
export function updateCardStack() {
    const stack = document.getElementById('cardStack');
    const hopperCount = document.getElementById('hopperCount');

    if (hopperCount) {
        hopperCount.textContent = cards.length;
    }

    if (!stack) return;

    if (!cards.length) {
        stack.innerHTML = '<div class="empty-state">⚡ Perforez des cartes</div>';
        return;
    }

    stack.innerHTML = cards.map((card, index) => {
        const lineNum = index + 1;  // Line numbers are 1-based
        const hasBreakpoint = breakpoints.has(lineNum);
        const isActive = index === currentCardIndex;
        return `
        <div class="card-mini${isActive ? ' active' : ''}${hasBreakpoint ? ' breakpoint' : ''}"
             data-line="${lineNum}">
            <span class="card-breakpoint" onclick="event.stopPropagation(); window.editorModule.toggleBreakpoint(${lineNum})" title="Cliquez pour ajouter/retirer un breakpoint">●</span>
            <span class="seq-num" onclick="window.editorModule.showCard(${index})">${card.seq}</span>
            <span class="card-text" onclick="window.editorModule.showCard(${index})">${card.original.trim().substring(0, 50)}</span>
        </div>
    `}).join('');
}

/**
 * Clear both stacker and hopper
 */
function clearCardStations() {
    const stacker = document.getElementById('cardStacker');
    const stack = document.getElementById('cardStack');
    const stackerCount = document.getElementById('stackerCount');
    const hopperCount = document.getElementById('hopperCount');

    if (stackerCount) stackerCount.textContent = '0';
    if (hopperCount) hopperCount.textContent = '0';

    if (stacker) {
        stacker.innerHTML = '<div class="empty-state">⚡ Perforez des cartes</div>';
    }
    if (stack) {
        stack.innerHTML = '<div class="empty-state">◄ Chargez les cartes</div>';
    }
}

/**
 * Add a card to the stacker (after punching)
 */
function addCardToStacker(cardIndex) {
    const stacker = document.getElementById('cardStacker');
    const stackerCount = document.getElementById('stackerCount');

    if (!stacker || !cards[cardIndex]) return;

    // Remove empty state on first card
    if (cardIndex === 0) {
        stacker.innerHTML = '';
    }

    const card = cards[cardIndex];
    const cardEl = document.createElement('div');
    cardEl.className = 'stacker-card new';
    cardEl.textContent = `${card.seq} ${card.original.trim().substring(0, 40)}`;
    stacker.appendChild(cardEl);

    // Auto-scroll to bottom
    stacker.scrollTop = stacker.scrollHeight;

    // Update count
    if (stackerCount) {
        stackerCount.textContent = cardIndex + 1;
    }

    // Remove animation class after it plays
    setTimeout(() => cardEl.classList.remove('new'), 150);
}

/**
 * Transfer cards from stacker to hopper (at compilation)
 */
function transferToHopper(onComplete) {
    const stacker = document.getElementById('cardStacker');
    const stack = document.getElementById('cardStack');
    const arrow = document.getElementById('transferArrow');
    const stackerCount = document.getElementById('stackerCount');
    const hopperCount = document.getElementById('hopperCount');

    if (!stacker || !stack) {
        if (onComplete) onComplete();
        return;
    }

    // Activate transfer arrow
    if (arrow) arrow.classList.add('active');

    // Clear hopper
    stack.innerHTML = '';

    // Animate transfer
    let transferred = 0;
    const totalCards = cards.length;
    const batchSize = Math.max(1, Math.floor(totalCards / 20)); // 20 steps max

    function transferBatch() {
        const endIndex = Math.min(transferred + batchSize, totalCards);

        for (let i = transferred; i < endIndex; i++) {
            const card = cards[i];
            const cardEl = document.createElement('div');
            cardEl.className = 'card-mini';
            cardEl.innerHTML = `<span class="seq-num">${card.seq}</span><span class="card-text">${card.original.trim().substring(0, 50)}</span>`;
            cardEl.onclick = () => window.editorModule.showCard(i);
            stack.appendChild(cardEl);
        }

        transferred = endIndex;

        // Update counts
        if (stackerCount) stackerCount.textContent = totalCards - transferred;
        if (hopperCount) hopperCount.textContent = transferred;

        // Update stacker visual
        const stackerCards = stacker.querySelectorAll('.stacker-card');
        stackerCards.forEach((el, i) => {
            if (i < transferred) el.style.opacity = '0.3';
        });

        if (transferred < totalCards) {
            setTimeout(transferBatch, 25);
        } else {
            // Transfer complete
            if (arrow) arrow.classList.remove('active');
            stacker.innerHTML = '<div class="empty-state">✓ Cartes transférées</div>';
            if (stackerCount) stackerCount.textContent = '0';
            if (onComplete) onComplete();
        }
    }

    transferBatch();
}

/**
 * Display a specific card (instant, no animation)
 * @param {number} index - Card index to display
 */
export function showCard(index) {
    if (!cards.length) return;

    stopPunchAnimation();
    currentCardIndex = Math.max(0, Math.min(index, cards.length - 1));
    const card = cards[currentCardIndex];

    renderPunchCard(card.text);
    updateCardDisplay(card);
}

/**
 * Display a specific card with punch animation
 * @param {number} index - Card index to display
 * @param {function} onComplete - Callback when animation completes
 */
function showCardAnimated(index, onComplete) {
    if (!cards.length) return;

    stopPunchAnimation();
    currentCardIndex = Math.max(0, Math.min(index, cards.length - 1));
    const card = cards[currentCardIndex];

    renderPunchCardAnimated(card.text, onComplete);
    updateCardDisplay(card);
}

/**
 * Update card display elements (seq, badge, navigator)
 */
function updateCardDisplay(card) {
    const cardSeq = document.getElementById('cardSeq');
    const currentCardBadge = document.getElementById('currentCardBadge');
    const cardNavigator = document.getElementById('cardNavigator');

    if (cardSeq) cardSeq.textContent = 'SEQ: ' + card.seq;
    if (currentCardBadge) currentCardBadge.textContent = (currentCardIndex + 1) + '/' + cards.length;
    if (cardNavigator) cardNavigator.textContent = (currentCardIndex + 1) + ' / ' + cards.length;

    // Don't update card stack during punching (hopper fills at compilation)
    if (!punchingInProgress) {
        updateCardStack();
    }
}

/**
 * Navigate to previous card
 */
export function prevCard() {
    showCard(currentCardIndex - 1);
}

/**
 * Navigate to next card
 */
export function nextCard() {
    showCard(currentCardIndex + 1);
}

/**
 * Compile the COBOL code using the real compiler
 */
export function compileCode() {
    const codeEditor = document.getElementById('codeEditor');
    if (!codeEditor) return;

    const code = codeEditor.value;
    if (!code.trim()) {
        showOutput('error', 'Aucun code');
        return;
    }

    setStatus('busy', 'COMPILATION...');
    showOutput('info', '> Compilation COBOL...');

    // Get data manager files for file operations (use sync cache)
    const dataManager = window.dataManagerModule?.getFilesSync?.() || {};

    // Clear output buffer for printer
    programOutputBuffer = [];

    // Create runtime with callbacks and dialect
    cobolRuntime = new CobolRuntime({
        dialect: currentDialect,
        onDisplay: (msg) => {
            showOutput('success', msg);
            logIO('output', msg);
            programOutputBuffer.push(msg); // Store for printer
        },
        onAccept: (varName) => {
            showOutput('info', `En attente d'entrée pour ${varName}...`);
            logIO('info', `ACCEPT ${varName} - en attente...`);
        },
        onError: (err) => {
            showOutput('error', err);
        },
        onStatus: (status) => {
            showOutput('info', '> ' + status);
        },
        onDialectWarning: (warning) => {
            showOutput('warning', `⚠ ${warning}`);
        },
        dataManager: dataManager,
    });

    // Compile
    const result = cobolRuntime.compile(code);

    if (result.success) {
        showOutput('success', `> Programme: ${result.programId}`);
        showOutput('success', '> Compilation réussie!');
        setStatus('ok', 'COMPILÉ');
    } else {
        result.errors.forEach(err => showOutput('error', err));
        setStatus('error', 'ERREUR');
    }
}

/**
 * Run the compiled COBOL program
 */
export async function runCode() {
    if (!cobolRuntime) {
        showOutput('error', 'Compilez d\'abord le programme');
        return;
    }

    setStatus('busy', 'EXÉCUTION...');
    showOutput('info', '> Exécution du programme...');
    showOutput('info', '─'.repeat(40));

    try {
        await cobolRuntime.run();
        showOutput('info', '─'.repeat(40));
        setStatus('ok', 'TERMINÉ');
    } catch (error) {
        showOutput('error', `Erreur: ${error.message}`);
        setStatus('error', 'ERREUR');
    }
}


/**
 * Provide input to the running program
 * @param {string} value - Input value
 */
export function provideInput(value) {
    if (cobolRuntime && cobolRuntime.isWaitingForInput()) {
        logIO('input', value);
        cobolRuntime.provideInput(value);

        // Reset placeholder
        const ioInput = document.getElementById('ioInput');
        if (ioInput) {
            ioInput.placeholder = 'ACCEPT...';
        }
    }
}

/**
 * Check if program is waiting for input
 * @returns {boolean}
 */
export function isWaitingForInput() {
    return cobolRuntime?.isWaitingForInput() || false;
}

/**
 * Get the current runtime (for debugging)
 * @returns {CobolRuntime}
 */
export function getRuntime() {
    return cobolRuntime;
}

/**
 * Clear all content
 */
export function clearAll() {
    const codeEditor = document.getElementById('codeEditor');
    if (codeEditor) {
        codeEditor.value = '';
    }

    updateLineNumbers();
    cards = [];
    currentCardIndex = 0;
    cobolRuntime = null;

    // Reset workflow state
    isPunched = false;
    isCompiled = false;
    updateWorkflowButtons();

    clearCardStations();
    renderPunchCard('');

    updateCardCount(0);

    const currentCardBadge = document.getElementById('currentCardBadge');
    const cardNavigator = document.getElementById('cardNavigator');
    const compilerOutput = document.getElementById('compilerOutput');

    if (currentCardBadge) currentCardBadge.textContent = '0/0';
    if (cardNavigator) cardNavigator.textContent = '0 / 0';
    if (compilerOutput) compilerOutput.innerHTML = '<div class="output-line info">█ COBOL Runtime</div><div class="output-line info">█ Prêt.</div>';

    setStatus('ok', 'PRÊT');
}

/**
 * Print program output to dot-matrix printer
 */
export function printOutput() {
    if (programOutputBuffer.length === 0) {
        showOutput('warning', 'Aucune sortie à imprimer. Exécutez d\'abord le programme.');
        return;
    }

    printCompilationOutput(compiledProgramId, programOutputBuffer);
}

/**
 * Open download modal
 */
export function downloadCards() {
    if (!cards.length) return;
    openModal('downloadModal');
}

/**
 * Download cards in specified format
 * @param {string} format - File format: 'txt' or 'cob'
 */
export function downloadAs(format) {
    const content = cards.map(card => card.text).join('\n');
    downloadFile(content, 'program.' + format);
    closeModal('downloadModal');
}

/**
 * Trigger file import dialog
 */
export function importCobFile() {
    const fileInput = document.getElementById('cobFileInput');
    if (fileInput) {
        fileInput.click();
    }
}

/**
 * Handle imported .cob file
 * @param {Event} event - File input change event
 */
export function handleCobFileImport(event) {
    const file = event.target.files[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = function(e) {
        const content = e.target.result;
        const codeEditor = document.getElementById('codeEditor');

        if (codeEditor) {
            codeEditor.value = content;
            updateLineNumbers();

            // Reset state
            cards = [];
            isPunched = false;
            isCompiled = false;
            cobolRuntime = null;
            updateWorkflowButtons();
            clearCardStations();

            showOutput('success', `✓ Fichier "${file.name}" importé`);
            showOutput('info', `${content.split('\n').length} lignes chargées`);
        }
    };

    reader.onerror = function() {
        showOutput('error', `✗ Erreur de lecture du fichier`);
    };

    reader.readAsText(file);

    // Reset input to allow re-importing same file
    event.target.value = '';
}

/**
 * Set editor code content
 * @param {string} code - The code to set
 */
export function setCode(code) {
    const codeEditor = document.getElementById('codeEditor');
    if (codeEditor) {
        codeEditor.value = code;
        updateLineNumbers();
    }
}

/**
 * Get current cards array
 * @returns {Array} Current cards
 */
export function getCards() {
    return cards;
}

// ============================================
// STEP MODE - Card Reader Simulation
// ============================================

/**
 * Start step-by-step card reading mode
 */
export function startStepMode() {
    // First generate cards if not done
    if (!cards.length) {
        generateCards();
    }

    if (!cards.length) return;

    stepMode = true;
    stepCardIndex = 0;
    steppedCards = [];

    // Update UI
    const stepBtn = document.querySelector('.btn-step');
    const stepIndicator = document.getElementById('stepModeIndicator');
    const btnStepPrev = document.getElementById('btnStepPrev');
    const btnStepNext = document.getElementById('btnStepNext');
    const readerReady = document.getElementById('readerReady');

    if (stepBtn) stepBtn.classList.add('active');
    if (stepIndicator) stepIndicator.classList.remove('hidden');
    if (btnStepPrev) btnStepPrev.disabled = true;
    if (btnStepNext) btnStepNext.disabled = false;
    if (readerReady) readerReady.classList.add('active');

    // Show first card with animation
    showStepCard(0);
    updateHopperDisplay();

    showOutput('system', '═══════════════════════════════════════');
    showOutput('info', '⏯ MODE PAS À PAS ACTIVÉ');
    showOutput('info', `📚 ${cards.length} cartes dans la trémie`);
    showOutput('system', 'Utilisez ◄ Préc / Suiv ► pour lire');
    showOutput('system', '═══════════════════════════════════════');

    setStatus('ok', 'PAS À PAS');
}

/**
 * Stop step mode
 */
export function stopStepMode() {
    stepMode = false;

    const stepBtn = document.querySelector('.btn-step');
    const stepIndicator = document.getElementById('stepModeIndicator');
    const btnStepPrev = document.getElementById('btnStepPrev');
    const btnStepNext = document.getElementById('btnStepNext');
    const readerReady = document.getElementById('readerReady');
    const readerReading = document.getElementById('readerReading');

    if (stepBtn) stepBtn.classList.remove('active');
    if (stepIndicator) stepIndicator.classList.add('hidden');
    if (btnStepPrev) btnStepPrev.disabled = true;
    if (btnStepNext) btnStepNext.disabled = true;
    if (readerReady) readerReady.classList.remove('active');
    if (readerReading) readerReading.classList.remove('active');

    setStatus('ok', 'PRÊT');
}

/**
 * Step to next card
 */
export function stepNext() {
    if (!stepMode || stepCardIndex >= cards.length) return;

    // Animate reading
    animateCardRead(() => {
        // Mark current card as read
        if (!steppedCards.includes(stepCardIndex)) {
            steppedCards.push(stepCardIndex);
        }

        // Show card content in console
        const card = cards[stepCardIndex];
        showOutput('highlight', `[${card.seq}] ${card.original}`);

        stepCardIndex++;

        if (stepCardIndex < cards.length) {
            showStepCard(stepCardIndex);
        } else {
            // All cards read
            showOutput('success', '═══════════════════════════════════════');
            showOutput('success', '✓ TOUTES LES CARTES ONT ÉTÉ LUES');
            showOutput('success', '═══════════════════════════════════════');
            stopStepMode();
        }

        updateStepButtons();
        updateHopperDisplay();
    });
}

/**
 * Step to previous card
 */
export function stepPrevious() {
    if (!stepMode || stepCardIndex <= 0) return;

    stepCardIndex--;
    showStepCard(stepCardIndex);

    showOutput('info', `◄ Retour à la carte ${stepCardIndex + 1}`);

    updateStepButtons();
    updateHopperDisplay();
}

/**
 * Show card in step mode with animation
 */
function showStepCard(index) {
    const card = cards[index];
    const punchCardEl = document.getElementById('punchCardMain');

    // Add feeding animation
    if (punchCardEl) {
        punchCardEl.classList.remove('feeding');
        void punchCardEl.offsetWidth; // Force reflow
        punchCardEl.classList.add('feeding');
    }

    // Render the card
    renderPunchCard(card.text);

    // Update UI
    const cardSeq = document.getElementById('cardSeq');
    const currentCardBadge = document.getElementById('currentCardBadge');
    const cardNavigator = document.getElementById('cardNavigator');

    if (cardSeq) cardSeq.textContent = 'SEQ: ' + card.seq;
    if (currentCardBadge) currentCardBadge.textContent = (index + 1) + '/' + cards.length;
    if (cardNavigator) cardNavigator.textContent = (index + 1) + ' / ' + cards.length;
}

/**
 * Animate card reading
 */
function animateCardRead(callback) {
    const punchCardEl = document.getElementById('punchCardMain');
    const readerReading = document.getElementById('readerReading');

    // Start reading animation
    if (punchCardEl) punchCardEl.classList.add('reading');
    if (readerReading) readerReading.classList.add('active');

    // Simulate reading time
    setTimeout(() => {
        if (punchCardEl) punchCardEl.classList.remove('reading');
        if (readerReading) readerReading.classList.remove('active');
        callback();
    }, 300);
}

/**
 * Update step navigation buttons
 */
function updateStepButtons() {
    const btnStepPrev = document.getElementById('btnStepPrev');
    const btnStepNext = document.getElementById('btnStepNext');

    if (btnStepPrev) btnStepPrev.disabled = !stepMode || stepCardIndex <= 0;
    if (btnStepNext) btnStepNext.disabled = !stepMode || stepCardIndex >= cards.length;
}

/**
 * Update hopper display for step mode
 */
function updateHopperDisplay() {
    const stack = document.getElementById('cardStack');
    const hopperCount = document.getElementById('hopperCount');

    if (hopperCount) {
        hopperCount.textContent = stepMode ? `${cards.length - stepCardIndex}` : cards.length;
    }

    if (!stack || !cards.length) return;

    stack.innerHTML = cards.map((card, index) => {
        let classes = 'card-mini';
        if (stepMode) {
            if (index === stepCardIndex) classes += ' current';
            else if (steppedCards.includes(index)) classes += ' read';
        } else if (index === currentCardIndex) {
            classes += ' active';
        }

        // Truncate text for display, show meaningful content
        const displayText = card.original.trim().substring(0, 50);

        return `<div class="${classes}" onclick="window.editorModule.showCard(${index})">
            <span class="seq-num">${card.seq}</span>
            <span class="card-text">${displayText}</span>
        </div>`;
    }).join('');
}

/**
 * Submit console input
 */
export function submitConsoleInput() {
    const input = document.getElementById('consoleInput');
    if (!input) return;

    const value = input.value.trim();
    if (!value) return;

    // Show input in console
    showOutput('input', `► ${value}`);

    // Provide to runtime if waiting
    if (cobolRuntime && cobolRuntime.isWaitingForInput()) {
        cobolRuntime.provideInput(value);
        input.classList.remove('waiting');
        input.placeholder = 'Entrée...';

        // Update console badge
        const badge = document.getElementById('consoleBadge');
        if (badge) badge.textContent = 'RUNNING';
    }

    input.value = '';
    logIO('input', value);
}

/**
 * Check if in step mode
 */
export function isStepMode() {
    return stepMode;
}

// ============================================
// DEBUG MODE - Real Step-by-Step Execution
// ============================================

/**
 * Start debug mode - compile and execute step by step
 */
export async function startDebugMode() {
    if (!isPunched || !cards.length) {
        showOutput('error', 'Perforez et compilez les cartes d\'abord');
        return;
    }

    // Always create new runtime with debug callbacks (need onStep callback)
    showOutput('info', '⚙ Compilation pour debug...');

    // Use full source code (with comments) for compilation
    const code = sourceCode;

    // Create runtime with debug callbacks and dialect (console only, no terminal modal)
    cobolRuntime = new CobolRuntime({
        dialect: currentDialect,
        onDisplay: (msg) => {
            showOutput('output', msg);
        },
        onAccept: (varName) => {
            showOutputCursor();
            showDebugWaitingInput(varName);
            // Highlight console input
            const consoleInput = document.getElementById('consoleInput');
            if (consoleInput) {
                consoleInput.classList.add('waiting');
                consoleInput.placeholder = `Entrez ${varName}...`;
                consoleInput.focus();
            }
        },
        onError: (err) => {
            showOutput('error', err);
        },
        onStatus: (status) => showOutput('system', status),
        onDialectWarning: (warning) => {
            showOutput('warning', `⚠ ${warning}`);
        },
        onStep: handleDebugStep,
        dataManager: window.dataManagerModule?.getFilesSync() || {}
    });

    const result = cobolRuntime.compile(code);
    if (!result.success) {
        showOutput('error', '✗ ERREUR DE COMPILATION');
        result.errors.forEach(err => showOutput('error', `  ${err}`));
        return;
    }

    isCompiled = true;
    compiledProgramId = result.programId;

    // Enter debug mode
    debugMode = true;
    previousVariables = {};

    // Update UI
    const btnStep = document.getElementById('btnStep');
    const btnStopDebug = document.getElementById('btnStopDebug');
    const stepIndicator = document.getElementById('stepModeIndicator');
    const debugPanel = document.getElementById('debugPanel');

    if (btnStep) btnStep.classList.add('active');
    if (btnStopDebug) btnStopDebug.classList.remove('hidden');
    if (stepIndicator) stepIndicator.classList.remove('hidden');
    if (debugPanel) debugPanel.classList.add('active');
    document.body.classList.add('debug-active');

    showOutput('system', '═══════════════════════════════════════');
    showOutput('info', '⏯ MODE DEBUG ACTIVÉ');
    showOutput('info', `Programme: ${compiledProgramId}`);
    showOutput('system', 'Cliquez "Suivant" pour exécuter pas à pas');
    showOutput('system', '═══════════════════════════════════════');

    setStatus('debug', 'DEBUG');

    // Don't open terminal modal in debug mode - use console panel instead
    // Show console input for ACCEPT statements
    const consoleInputArea = document.getElementById('consoleInputArea');
    if (consoleInputArea) consoleInputArea.style.display = 'flex';

    // Start execution with step mode enabled
    try {
        await cobolRuntime.run({
            stepMode: true,
            onStep: handleDebugStep,
            onInterpreterReady: (interpreter) => {
                debugInterpreter = interpreter;
                // Pass breakpoints to interpreter
                debugInterpreter.setBreakpoints(breakpoints);
            }
        });
        // Execution completed
        endDebugMode('completed');
    } catch (error) {
        if (error.message === 'STOP RUN') {
            endDebugMode('stopped');
        } else if (error.message === 'DEBUG_STOPPED') {
            endDebugMode('aborted');
        } else {
            showOutput('error', `Erreur: ${error.message}`);
            endDebugMode('error');
        }
    }
}

/**
 * Handle step notification from interpreter
 */
function handleDebugStep(stepInfo) {
    const debugLine = document.getElementById('debugLine');
    const debugStmt = document.getElementById('debugStmt');
    const debugVariables = document.getElementById('debugVariables');
    const debugExplanation = document.getElementById('debugExplanation');

    // Highlight current line in editor
    highlightCurrentLine(stepInfo.line);

    // Update current statement info
    if (debugLine) {
        debugLine.textContent = `Ligne: ${stepInfo.line || '?'}`;
    }
    if (debugStmt) {
        debugStmt.textContent = stepInfo.type || '-';
    }

    // Update variables display
    if (debugVariables && stepInfo.variables) {
        updateDebugVariables(stepInfo.variables);
    }

    // Get and display pedagogical explanation
    const explanation = explainStatement(stepInfo.statement, stepInfo.variables);
    if (explanation && debugExplanation) {
        const categoryInfo = getCategoryInfo(explanation.category);
        updateDebugExplanation(debugExplanation, explanation, categoryInfo);
    }

    // Highlight line in console with short summary
    const summary = explanation ? explanation.title : stepInfo.type;
    showOutput('highlight', `► ${summary} (ligne ${stepInfo.line || '?'})`);

    // Show explanation tip in console
    if (explanation && explanation.tip) {
        showOutput('info', `   💡 ${explanation.tip}`);
    }
}

/**
 * Highlight the current line in the editor during debug
 */
function highlightCurrentLine(lineNum) {
    // Remove previous current line highlight
    document.querySelectorAll('.line-number.current').forEach(el => {
        el.classList.remove('current');
    });

    // Add current highlight
    if (lineNum) {
        const lineSpan = document.querySelector(`.line-number[data-line="${lineNum}"]`);
        if (lineSpan) {
            lineSpan.classList.add('current');
            // Scroll line into view if needed
            lineSpan.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
        }
    }
}

/**
 * Update the debug explanation panel
 */
function updateDebugExplanation(container, explanation, categoryInfo) {
    const html = `
        <div class="debug-explanation-header">
            <span class="debug-category" style="color: ${categoryInfo.color}">
                ${categoryInfo.icon} ${categoryInfo.name}
            </span>
        </div>
        <div class="debug-explanation-title">${explanation.title}</div>
        <div class="debug-explanation-desc">${escapeHtml(explanation.description)}</div>
        ${explanation.syntax ? `
            <div class="debug-explanation-syntax">
                <strong>Syntaxe:</strong> <code>${escapeHtml(explanation.syntax)}</code>
            </div>
        ` : ''}
        ${explanation.example ? `
            <div class="debug-explanation-example">
                <strong>Exemple:</strong>
                <pre>${escapeHtml(explanation.example)}</pre>
            </div>
        ` : ''}
    `;
    container.innerHTML = html;
}

/**
 * Update debug variables display
 */
function updateDebugVariables(variables) {
    const container = document.getElementById('debugVariables');
    if (!container) return;

    const html = [];
    for (const [name, info] of Object.entries(variables)) {
        // Skip internal/system variables
        if (name.startsWith('_')) continue;

        const hasChanged = previousVariables[name] !== undefined &&
                          previousVariables[name] !== info.value;

        const displayValue = info.value?.toString().trim() || '(vide)';

        html.push(`
            <div class="debug-var${hasChanged ? ' changed' : ''}">
                <span class="debug-var-name">${name}</span>
                <span class="debug-var-value">${escapeHtml(displayValue)}</span>
            </div>
        `);
    }

    container.innerHTML = html.join('');

    // Store for next comparison
    for (const [name, info] of Object.entries(variables)) {
        previousVariables[name] = info.value;
    }
}

/**
 * Show that debug is waiting for user input
 */
function showDebugWaitingInput(varName) {
    const debugStmt = document.getElementById('debugStmt');
    if (debugStmt) {
        debugStmt.textContent = `ACCEPT ${varName} - EN ATTENTE`;
        debugStmt.style.color = '#ffaa00';
    }
}

/**
 * Step to next statement in debug mode
 */
export function debugStepNext() {
    if (!debugMode || !debugInterpreter) return;

    // Reset waiting style
    const debugStmt = document.getElementById('debugStmt');
    if (debugStmt) debugStmt.style.color = '';

    // Tell interpreter to continue
    debugInterpreter.stepNext();
}

/**
 * Continue execution until next breakpoint
 */
export function debugRunToBreakpoint() {
    if (!debugMode || !debugInterpreter) return;

    if (breakpoints.size === 0) {
        showOutput('warning', '⚠ Aucun breakpoint défini. Cliquez sur un numéro de ligne pour en ajouter.');
        return;
    }

    showOutput('info', '▶ Exécution jusqu\'au prochain breakpoint...');
    debugInterpreter.continueToBreakpoint();
}

/**
 * Continue execution without stepping (disable step mode)
 */
export function debugContinue() {
    if (!debugMode || !debugInterpreter) return;

    // Disable step mode and continue
    debugInterpreter.setStepMode(false);

    showOutput('info', '▶▶ Exécution continue...');

    // Hide debug panel but keep terminal open
    const debugPanel = document.getElementById('debugPanel');
    if (debugPanel) debugPanel.classList.remove('active');
    document.body.classList.remove('debug-active');

    setStatus('running', 'EXÉCUTION');
}

/**
 * Stop debug mode completely
 */
export function stopDebugMode() {
    if (!debugMode) return;

    // Stop interpreter
    if (debugInterpreter) {
        debugInterpreter.halted = true;
        debugInterpreter.setStepMode(false);
        // Resume if paused to let execution end
        debugInterpreter.stepNext();
    }

    endDebugMode('aborted');
}

/**
 * End debug mode (called when execution ends or is stopped)
 */
function endDebugMode(reason) {
    debugMode = false;
    debugInterpreter = null;

    // Update UI
    const btnStep = document.getElementById('btnStep');
    const btnStopDebug = document.getElementById('btnStopDebug');
    const stepIndicator = document.getElementById('stepModeIndicator');
    const debugPanel = document.getElementById('debugPanel');
    const consoleInput = document.getElementById('consoleInput');

    if (btnStep) btnStep.classList.remove('active');
    if (btnStopDebug) btnStopDebug.classList.add('hidden');
    if (stepIndicator) stepIndicator.classList.add('hidden');
    if (debugPanel) debugPanel.classList.remove('active');
    document.body.classList.remove('debug-active');

    // Clear current line highlight
    document.querySelectorAll('.line-number.current').forEach(el => {
        el.classList.remove('current');
    });

    // Reset console input
    if (consoleInput) {
        consoleInput.classList.remove('waiting');
        consoleInput.placeholder = 'Entrée...';
    }

    // Output to console (not terminal modal)
    showOutput('system', '─'.repeat(40));

    switch (reason) {
        case 'completed':
            showOutput('success', '*** FIN NORMALE ***');
            setStatus('ok', 'TERMINÉ');
            break;
        case 'stopped':
            showOutput('success', '*** STOP RUN ***');
            setStatus('ok', 'ARRÊTÉ');
            break;
        case 'aborted':
            showOutput('error', '*** DEBUG ARRÊTÉ ***');
            setStatus('warning', 'ARRÊTÉ');
            break;
        case 'error':
            showOutput('error', '*** ERREUR ***');
            setStatus('error', 'ERREUR');
            break;
    }

    showOutput('system', '═══════════════════════════════════════');
    showOutput('info', '⏹ MODE DEBUG TERMINÉ');
    showOutput('system', '═══════════════════════════════════════');
}

/**
 * Check if in debug mode
 */
export function isDebugMode() {
    return debugMode;
}

/**
 * Toggle debug panel collapsed state
 */
export function toggleDebugCollapse() {
    const debugPanel = document.getElementById('debugPanel');
    if (debugPanel) {
        debugPanel.classList.toggle('collapsed');
        // Update layout height
        updateDebugLayoutHeight();
    }
}

/**
 * Update layout height based on debug panel state
 */
function updateDebugLayoutHeight() {
    const debugPanel = document.getElementById('debugPanel');
    const layout = document.querySelector('.editor-layout-new');
    if (!debugPanel || !layout) return;

    if (debugPanel.classList.contains('collapsed')) {
        layout.style.height = 'calc(100vh - 160px)';
    } else {
        layout.style.height = '';  // Reset to CSS default
    }
}

/**
 * Initialize debug panel resizer
 */
export function initDebugResizer() {
    const resizer = document.getElementById('debugResizer');
    const debugPanel = document.getElementById('debugPanel');
    const debugContent = document.getElementById('debugContent');

    if (!resizer || !debugPanel || !debugContent) return;

    let startY, startHeight;

    resizer.addEventListener('mousedown', (e) => {
        startY = e.clientY;
        startHeight = debugContent.offsetHeight;
        resizer.classList.add('dragging');

        document.addEventListener('mousemove', onMouseMove);
        document.addEventListener('mouseup', onMouseUp);
        e.preventDefault();
    });

    function onMouseMove(e) {
        const deltaY = startY - e.clientY;
        const newHeight = Math.max(50, Math.min(400, startHeight + deltaY));
        debugContent.style.maxHeight = newHeight + 'px';
    }

    function onMouseUp() {
        resizer.classList.remove('dragging');
        document.removeEventListener('mousemove', onMouseMove);
        document.removeEventListener('mouseup', onMouseUp);
    }
}

/**
 * Escape HTML special characters
 */
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

/**
 * Change the COBOL dialect
 * @param {string} dialect - Dialect value (e.g., 'COBOL-85', 'AUTO')
 */
export function changeDialect(dialect) {
    currentDialect = dialect;

    // Reset compilation state since dialect changed
    isCompiled = false;
    updateWorkflowButtons();

    // Show status message
    if (dialect === 'AUTO') {
        setStatus('ok', 'Dialecte: Auto-détection');
        showOutput('info', '> Dialecte: Auto-détection activée');
    } else {
        setStatus('ok', `Dialecte: ${dialect}`);
        showOutput('info', `> Dialecte: ${dialect}`);
    }
}

/**
 * Get the current dialect
 * @returns {string}
 */
export function getCurrentDialect() {
    return currentDialect;
}
