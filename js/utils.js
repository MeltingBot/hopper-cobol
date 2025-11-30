/**
 * Utility Module
 * Shared utilities for UI, modals, and status management
 */

/**
 * Switch between tabs
 * @param {string} tabName - Name of the tab to switch to
 * @param {HTMLElement} button - The tab button that was clicked
 */
export function switchTab(tabName, button) {
    // Deactivate all tabs
    document.querySelectorAll('.header-tab').forEach(tab => {
        tab.classList.remove('active');
    });
    document.querySelectorAll('.tab-content').forEach(content => {
        content.classList.remove('active');
    });

    // Activate selected tab
    button.classList.add('active');
    const tabContent = document.getElementById('tab-' + tabName);
    if (tabContent) {
        tabContent.classList.add('active');
    }
}

/**
 * Close a modal by ID
 * @param {string} modalId - The ID of the modal to close
 */
export function closeModal(modalId) {
    const modal = document.getElementById(modalId);
    if (modal) {
        modal.classList.remove('active');
    }
}

/**
 * Open a modal by ID
 * @param {string} modalId - The ID of the modal to open
 */
export function openModal(modalId) {
    const modal = document.getElementById(modalId);
    if (modal) {
        modal.classList.add('active');
    }
}

/**
 * Set the status indicator
 * @param {string} type - Status type: 'ok', 'error', 'busy', 'debug', 'warning', 'running'
 * @param {string} text - Status text to display
 */
export function setStatus(type, text) {
    const statusLight = document.getElementById('statusLight');
    const statusText = document.getElementById('statusText');

    if (statusLight) {
        let className = 'status-light';
        if (type === 'error') className += ' error';
        else if (type === 'debug') className += ' debug';
        else if (type === 'warning') className += ' warning';
        statusLight.className = className;
    }
    if (statusText) {
        statusText.textContent = text;
    }
}

/**
 * Add a line to the compiler output
 * @param {string} type - Line type: 'error', 'success', or 'info'
 * @param {string} message - Message to display
 */
export function showOutput(type, message) {
    const output = document.getElementById('compilerOutput');
    if (!output) return;

    const line = document.createElement('div');
    line.className = 'output-line ' + type;
    line.textContent = message;
    output.appendChild(line);
    output.scrollTop = output.scrollHeight;
}

/**
 * Show blinking cursor for ACCEPT input in console output
 */
export function showOutputCursor() {
    const output = document.getElementById('compilerOutput');
    if (!output) return;

    const line = document.createElement('div');
    line.className = 'output-line';

    const cursor = document.createElement('span');
    cursor.className = 'terminal-cursor blink';
    cursor.id = 'outputCursor';
    cursor.textContent = '▌';

    line.appendChild(cursor);
    output.appendChild(line);
    output.scrollTop = output.scrollHeight;
}

/**
 * Hide the blinking cursor after input received
 */
export function hideOutputCursor() {
    const cursor = document.getElementById('outputCursor');
    if (cursor) {
        cursor.parentElement?.remove();
    }
}

/**
 * Log a message to the I/O console
 * @param {string} type - Line type: 'input', 'output', 'info', or 'file-op'
 * @param {string} message - Message to display
 */
export function logIO(type, message) {
    const console = document.getElementById('ioConsole');
    if (!console) return;

    const line = document.createElement('div');
    line.className = 'console-line ' + type;
    line.textContent = '> ' + message;
    console.appendChild(line);
    console.scrollTop = console.scrollHeight;
}

/**
 * Update the card count display
 * @param {number} count - Number of cards
 */
export function updateCardCount(count) {
    const cardCount = document.getElementById('cardCount');
    if (cardCount) {
        cardCount.textContent = count;
    }
}

/**
 * Download content as a file
 * @param {string} content - File content
 * @param {string} filename - Name for the downloaded file
 * @param {string} mimeType - MIME type of the file
 */
export function downloadFile(content, filename, mimeType = 'text/plain') {
    const blob = new Blob([content], { type: mimeType });
    const link = document.createElement('a');
    link.href = URL.createObjectURL(blob);
    link.download = filename;
    link.click();
    URL.revokeObjectURL(link.href);
}

/**
 * Initialize column resizers for the editor layout
 */
export function initResizers() {
    const layout = document.getElementById('editorLayout');
    const resizer1 = document.getElementById('resizer1');
    const resizer2 = document.getElementById('resizer2');

    if (!layout || !resizer1 || !resizer2) return;

    let isResizing = false;
    let currentResizer = null;
    let startX = 0;
    let startWidths = [];

    function startResize(e, resizer) {
        isResizing = true;
        currentResizer = resizer;
        startX = e.clientX;

        // Get current column widths
        const cols = layout.querySelectorAll('.editor-column, .card-column, .console-column');
        startWidths = Array.from(cols).map(col => col.getBoundingClientRect().width);

        layout.classList.add('resizing');
        resizer.classList.add('dragging');

        e.preventDefault();
    }

    function doResize(e) {
        if (!isResizing) return;

        const deltaX = e.clientX - startX;
        const layoutRect = layout.getBoundingClientRect();
        const totalWidth = layoutRect.width - 12; // Subtract resizer widths

        let newWidths;

        if (currentResizer === resizer1) {
            // Resizing between editor and card columns
            const newCol1 = Math.max(200, Math.min(startWidths[0] + deltaX, totalWidth - 400));
            const newCol2 = startWidths[1] - deltaX;
            const newCol3 = startWidths[2];

            if (newCol2 >= 300) {
                newWidths = [newCol1, newCol2, newCol3];
            }
        } else {
            // Resizing between card and console columns
            const newCol1 = startWidths[0];
            const newCol2 = Math.max(300, Math.min(startWidths[1] + deltaX, totalWidth - 400));
            const newCol3 = startWidths[2] - deltaX;

            if (newCol3 >= 150) {
                newWidths = [newCol1, newCol2, newCol3];
            }
        }

        if (newWidths) {
            layout.style.gridTemplateColumns = `${newWidths[0]}px 6px ${newWidths[1]}px 6px ${newWidths[2]}px`;
        }
    }

    function stopResize() {
        if (!isResizing) return;

        isResizing = false;
        layout.classList.remove('resizing');
        if (currentResizer) {
            currentResizer.classList.remove('dragging');
        }
        currentResizer = null;
    }

    resizer1.addEventListener('mousedown', (e) => startResize(e, resizer1));
    resizer2.addEventListener('mousedown', (e) => startResize(e, resizer2));

    document.addEventListener('mousemove', doResize);
    document.addEventListener('mouseup', stopResize);
}
