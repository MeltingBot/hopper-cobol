/**
 * Punch Card Module
 * Handles IBM punch card encoding and rendering
 */

// IBM 029 Keypunch encoding - maps characters to punch hole positions
export const PUNCH_ENCODING = {
    '': [],
    'A': [12, 1], 'B': [12, 2], 'C': [12, 3], 'D': [12, 4], 'E': [12, 5],
    'F': [12, 6], 'G': [12, 7], 'H': [12, 8], 'I': [12, 9],
    'J': [11, 1], 'K': [11, 2], 'L': [11, 3], 'M': [11, 4], 'N': [11, 5],
    'O': [11, 6], 'P': [11, 7], 'Q': [11, 8], 'R': [11, 9],
    'S': [0, 2], 'T': [0, 3], 'U': [0, 4], 'V': [0, 5], 'W': [0, 6],
    'X': [0, 7], 'Y': [0, 8], 'Z': [0, 9],
    '0': [0], '1': [1], '2': [2], '3': [3], '4': [4],
    '5': [5], '6': [6], '7': [7], '8': [8], '9': [9],
    '.': [12, 3, 8], ',': [0, 3, 8], "'": [5, 8],
    '(': [0, 5, 8], ')': [12, 5, 8],
    '+': [12], '-': [11], '*': [11, 4, 8], '/': [0, 1],
    '=': [3, 8], '$': [11, 3, 8], ' ': []
};

// Row labels for the punch card display
export const ROW_LABELS = ['12', '11', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

// Animation state
let punchAnimationId = null;

/**
 * Render a punch card visualization (instant, no animation)
 * @param {string} text - The text to encode on the card
 */
export function renderPunchCard(text) {
    renderPunchCardInternal(text, false);
}

/**
 * Render a punch card with punch animation
 * @param {string} text - The text to encode on the card
 * @param {function} onComplete - Callback when animation completes
 */
export function renderPunchCardAnimated(text, onComplete) {
    renderPunchCardInternal(text, true, onComplete);
}

/**
 * Internal render function
 */
function renderPunchCardInternal(text, animate = false, onComplete = null) {
    // Cancel any ongoing animation
    if (punchAnimationId) {
        cancelAnimationFrame(punchAnimationId);
        punchAnimationId = null;
    }

    const normalizedText = text.toUpperCase().padEnd(80, ' ').substring(0, 80);

    // Update text row display
    const cardTextRow = document.getElementById('cardTextRow');
    if (cardTextRow) {
        cardTextRow.textContent = normalizedText;
    }

    // Build punch grid HTML - initially without punched class if animating
    let html = '';
    for (let row = 0; row < 12; row++) {
        html += `<div class="punch-row"><span class="row-label">${ROW_LABELS[row]}</span>`;

        for (let col = 0; col < 80; col++) {
            const char = normalizedText[col];
            const encoding = PUNCH_ENCODING[char] || [];
            const shouldPunch = encoding.includes(parseInt(ROW_LABELS[row]));
            // If animating, don't add punched class yet
            const punchedClass = (!animate && shouldPunch) ? ' punched' : '';
            html += `<div class="punch-hole${punchedClass}" data-col="${col}" data-punch="${shouldPunch}"></div>`;
        }

        html += '</div>';
    }

    const punchGrid = document.getElementById('punchGrid');
    if (punchGrid) {
        punchGrid.innerHTML = html;

        if (animate) {
            animatePunchColumns(punchGrid, 0, onComplete);
        }
    }
}

/**
 * Animate punching column by column (fast sweep)
 */
function animatePunchColumns(grid, col, onComplete) {
    if (col >= 80) {
        punchAnimationId = null;
        if (onComplete) onComplete();
        return;
    }

    // Punch 8 columns at a time for faster animation (80/8 = 10 steps × 15ms = 150ms per card)
    const colsPerStep = 8;
    for (let c = col; c < Math.min(col + colsPerStep, 80); c++) {
        const holes = grid.querySelectorAll(`.punch-hole[data-col="${c}"][data-punch="true"]`);
        holes.forEach(hole => hole.classList.add('punched'));
    }

    // Schedule next batch
    punchAnimationId = setTimeout(() => {
        animatePunchColumns(grid, col + colsPerStep, onComplete);
    }, 15);
}

/**
 * Stop any ongoing punch animation
 */
export function stopPunchAnimation() {
    if (punchAnimationId) {
        clearTimeout(punchAnimationId);
        punchAnimationId = null;
    }
}

/**
 * Get the encoding for a specific character
 * @param {string} char - Single character to encode
 * @returns {number[]} Array of row numbers where holes should be punched
 */
export function getCharacterEncoding(char) {
    return PUNCH_ENCODING[char.toUpperCase()] || [];
}
