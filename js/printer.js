/**
 * Dot Matrix Printer Module
 * Simulates a retro 70s/80s dot matrix printer output
 * Features:
 * - Green-bar continuous paper effect
 * - Dot matrix font rendering
 * - Page headers and footers
 * - Print animation with sound simulation
 */

// Printer configuration
const PRINTER_CONFIG = {
    charsPerLine: 132,      // Standard line printer width
    linesPerPage: 66,       // Standard page length
    topMargin: 3,
    bottomMargin: 3,
    headerLines: 2,
    footerLines: 2,
    greenBarInterval: 3,    // Lines between green bars
};

// Current print state
let printBuffer = [];
let currentPage = 1;
let currentLine = 0;
let printJobActive = false;
let printerWindow = null;

/**
 * Initialize printer with a new print job
 */
export function initPrinter() {
    printBuffer = [];
    currentPage = 1;
    currentLine = 0;
    printJobActive = true;
}

/**
 * Print a line of text
 * @param {string} text - Text to print
 * @param {object} options - Print options (bold, underline, etc.)
 */
export function printLine(text, options = {}) {
    if (!printJobActive) {
        initPrinter();
    }

    // Handle page overflow
    const effectiveLines = PRINTER_CONFIG.linesPerPage -
                          PRINTER_CONFIG.topMargin -
                          PRINTER_CONFIG.bottomMargin;

    if (currentLine >= effectiveLines) {
        formFeed();
    }

    printBuffer.push({
        type: 'line',
        text: text || '',
        page: currentPage,
        line: currentLine,
        options
    });

    currentLine++;
}

/**
 * Print multiple lines (respects ADVANCING)
 * @param {string} text - Text to print
 * @param {number} advance - Number of lines to advance after printing
 */
export function printWithAdvancing(text, advance = 1) {
    printLine(text);
    for (let i = 1; i < advance; i++) {
        printLine('');
    }
}

/**
 * Form feed - advance to next page
 */
export function formFeed() {
    printBuffer.push({
        type: 'formfeed',
        page: currentPage
    });
    currentPage++;
    currentLine = 0;
}

/**
 * End the print job and display the output
 * @param {string} title - Document title
 */
export function endPrintJob(title = 'IMPRESSION COBOL') {
    printJobActive = false;
    displayPrinterOutput(title);
}

/**
 * Display the printer output in a modal window
 * @param {string} title - Document title
 */
function displayPrinterOutput(title) {
    // Create or get the printer modal
    let modal = document.getElementById('printerModal');
    if (!modal) {
        modal = createPrinterModal();
    }

    const content = document.getElementById('printerContent');
    const pageInfo = document.getElementById('printerPageInfo');

    // Generate the paper content
    const paperHTML = generatePaperHTML(title);
    content.innerHTML = paperHTML;

    // Update page info
    const totalPages = currentPage;
    pageInfo.textContent = `${printBuffer.filter(b => b.type === 'line').length} lignes - ${totalPages} page(s)`;

    // Show the modal
    modal.classList.remove('hidden');

    // Play print sound effect (optional)
    playPrintSound();
}

/**
 * Generate HTML for the printer paper
 * @param {string} title - Document title
 * @returns {string} HTML content
 */
function generatePaperHTML(title) {
    const html = [];
    let pageLines = [];
    let currentPageNum = 1;

    const renderPage = (pageNum, lines) => {
        const pageHtml = [];
        pageHtml.push(`<div class="printer-page" data-page="${pageNum}">`);

        // Tractor feed holes (left side)
        pageHtml.push('<div class="tractor-feed left">');
        for (let i = 0; i < 11; i++) {
            pageHtml.push('<div class="tractor-hole"></div>');
        }
        pageHtml.push('</div>');

        // Paper content
        pageHtml.push('<div class="paper-content">');

        // Page header
        pageHtml.push('<div class="page-header">');
        pageHtml.push(`<span class="header-title">${escapeHtml(title)}</span>`);
        pageHtml.push(`<span class="header-date">${formatDate()}</span>`);
        pageHtml.push(`<span class="header-page">PAGE ${pageNum}</span>`);
        pageHtml.push('</div>');

        // Print lines with green bar effect
        pageHtml.push('<div class="print-lines">');
        for (let i = 0; i < lines.length; i++) {
            const isGreenBar = Math.floor(i / PRINTER_CONFIG.greenBarInterval) % 2 === 0;
            const lineClass = isGreenBar ? 'print-line green-bar' : 'print-line';
            const lineText = lines[i]?.text || '';
            const options = lines[i]?.options || {};

            let textClass = '';
            if (options.bold) textClass += ' bold';
            if (options.underline) textClass += ' underline';

            pageHtml.push(`<div class="${lineClass}">`);
            pageHtml.push(`<span class="line-number">${String(i + 1).padStart(4, ' ')}</span>`);
            pageHtml.push(`<span class="line-text${textClass}">${escapeHtml(lineText)}</span>`);
            pageHtml.push('</div>');
        }
        pageHtml.push('</div>');

        // Page footer
        pageHtml.push('<div class="page-footer">');
        pageHtml.push(`<span class="footer-text">*** FIN DE PAGE ${pageNum} ***</span>`);
        pageHtml.push('</div>');

        pageHtml.push('</div>');

        // Tractor feed holes (right side)
        pageHtml.push('<div class="tractor-feed right">');
        for (let i = 0; i < 11; i++) {
            pageHtml.push('<div class="tractor-hole"></div>');
        }
        pageHtml.push('</div>');

        pageHtml.push('</div>');
        return pageHtml.join('');
    };

    // Process buffer and split into pages
    for (const item of printBuffer) {
        if (item.type === 'formfeed') {
            html.push(renderPage(currentPageNum, pageLines));
            pageLines = [];
            currentPageNum++;
        } else if (item.type === 'line') {
            pageLines.push(item);
        }
    }

    // Render last page if there's content
    if (pageLines.length > 0) {
        html.push(renderPage(currentPageNum, pageLines));
    }

    return html.join('');
}

/**
 * Create the printer modal structure
 */
function createPrinterModal() {
    const modal = document.createElement('div');
    modal.id = 'printerModal';
    modal.className = 'printer-modal hidden';

    modal.innerHTML = `
        <div class="printer-container">
            <div class="printer-header">
                <div class="printer-title">
                    <span class="printer-icon">🖨️</span>
                    <span>IMPRIMANTE DOT-MATRIX</span>
                </div>
                <div class="printer-info">
                    <span id="printerPageInfo">0 lignes</span>
                </div>
                <div class="printer-controls">
                    <button class="printer-btn" onclick="window.printPrinterOutput()">🖨️ Imprimer</button>
                    <button class="printer-btn" onclick="window.downloadPrinterOutput()">💾 Télécharger</button>
                    <button class="printer-btn close" onclick="window.closePrinterModal()">✕ Fermer</button>
                </div>
            </div>
            <div class="printer-paper-container">
                <div class="printer-paper" id="printerContent">
                </div>
            </div>
        </div>
    `;

    document.body.appendChild(modal);

    // Add global functions for button handlers
    window.closePrinterModal = () => {
        modal.classList.add('hidden');
    };

    window.printPrinterOutput = () => {
        const content = document.getElementById('printerContent');
        const printWindow = window.open('', '_blank');
        printWindow.document.write(`
            <!DOCTYPE html>
            <html>
            <head>
                <title>Impression COBOL</title>
                <style>
                    body { font-family: monospace; background: white; }
                    .printer-page { page-break-after: always; }
                    .tractor-feed { display: none; }
                    .paper-content { padding: 10mm; }
                    .print-line { font-family: 'Courier New', monospace; font-size: 10pt; }
                    .green-bar { background: #e8f5e9; }
                    .line-number { color: #999; margin-right: 10px; }
                    .page-header, .page-footer { margin: 5mm 0; font-weight: bold; }
                </style>
            </head>
            <body>${content.innerHTML}</body>
            </html>
        `);
        printWindow.document.close();
        printWindow.print();
    };

    window.downloadPrinterOutput = () => {
        // Generate plain text version
        let text = '';
        for (const item of printBuffer) {
            if (item.type === 'line') {
                text += item.text + '\n';
            } else if (item.type === 'formfeed') {
                text += '\f\n';
            }
        }

        const blob = new Blob([text], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'listing.txt';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    };

    return modal;
}

/**
 * Format current date for header
 */
function formatDate() {
    const now = new Date();
    const year = now.getFullYear();
    const month = String(now.getMonth() + 1).padStart(2, '0');
    const day = String(now.getDate()).padStart(2, '0');
    const hour = String(now.getHours()).padStart(2, '0');
    const min = String(now.getMinutes()).padStart(2, '0');
    return `${year}/${month}/${day} ${hour}:${min}`;
}

/**
 * Escape HTML entities
 */
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

/**
 * Play a simulated print sound
 */
function playPrintSound() {
    // Create audio context for retro printer sound
    try {
        const audioContext = new (window.AudioContext || window.webkitAudioContext)();

        // Simulate dot matrix printer noise
        const duration = 0.5;
        const oscillator = audioContext.createOscillator();
        const gainNode = audioContext.createGain();

        oscillator.connect(gainNode);
        gainNode.connect(audioContext.destination);

        oscillator.type = 'square';
        oscillator.frequency.setValueAtTime(200, audioContext.currentTime);
        oscillator.frequency.exponentialRampToValueAtTime(50, audioContext.currentTime + duration);

        gainNode.gain.setValueAtTime(0.05, audioContext.currentTime);
        gainNode.gain.exponentialRampToValueAtTime(0.01, audioContext.currentTime + duration);

        oscillator.start(audioContext.currentTime);
        oscillator.stop(audioContext.currentTime + duration);
    } catch (e) {
        // Audio not supported, silently ignore
    }
}

/**
 * Convenience function to print compilation output
 * @param {string} programId - Program identifier
 * @param {string[]} outputs - Array of output lines
 */
export function printCompilationOutput(programId, outputs) {
    initPrinter();

    // Header section
    printLine('═'.repeat(80), { bold: true });
    printLine(`   PROGRAMME: ${programId}`, { bold: true });
    printLine(`   DATE D'EXECUTION: ${formatDate()}`);
    printLine('═'.repeat(80), { bold: true });
    printLine('');
    printLine('   *** SORTIE DU PROGRAMME ***');
    printLine('');

    // Program output
    for (const line of outputs) {
        printLine('   ' + line);
    }

    // Footer section
    printLine('');
    printLine('═'.repeat(80), { bold: true });
    printLine('   *** FIN DE L\'EXECUTION ***');
    printLine(`   NOMBRE DE LIGNES: ${outputs.length}`);
    printLine('═'.repeat(80), { bold: true });

    endPrintJob(`LISTING ${programId}`);
}

/**
 * Close the printer modal
 */
export function closePrinter() {
    const modal = document.getElementById('printerModal');
    if (modal) {
        modal.classList.add('hidden');
    }
}

export default {
    initPrinter,
    printLine,
    printWithAdvancing,
    formFeed,
    endPrintJob,
    printCompilationOutput,
    closePrinter
};
