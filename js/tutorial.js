/**
 * Tutorial Module
 * COBOL example library - loads examples from external files
 */

import { setCode, generateCards } from './editor.js';

// Current example index
let currentExample = 0;

// Examples loaded from JSON
let examples = [];

// Category icons and colors
const categoryInfo = {
    'ECRAN': { icon: '🖥️', color: '#00ff88' },
    'ERP': { icon: '🏢', color: '#4a9eff' },
    'RH': { icon: '👥', color: '#aa66ff' },
    'BANQUE': { icon: '🏦', color: '#44cc88' },
    'TOURISME': { icon: '🏨', color: '#ff6699' },
    'BATCH': { icon: '⚙️', color: '#888888' }
};

/**
 * Initialize the tutorial (example library)
 */
export async function initTutorial() {
    try {
        // Load examples index from JSON
        const response = await fetch('./cobol-examples/examples.json');
        examples = await response.json();

        // Render the categorized list
        renderCategorizedList();

        // Select first example by default
        if (examples.length > 0) {
            selectExample(0);
        }
    } catch (error) {
        console.error('Erreur chargement exemples:', error);
        // Fallback - show error in UI
        const container = document.getElementById('libraryCategories');
        if (container) {
            container.innerHTML = '<div class="error">Erreur de chargement des exemples</div>';
        }
    }
}

/**
 * Load COBOL code from file
 */
async function loadCobolFile(filename) {
    try {
        const response = await fetch(`./cobol-examples/${filename}`);
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        return await response.text();
    } catch (error) {
        console.error(`Erreur chargement ${filename}:`, error);
        return `      * ERREUR: Impossible de charger ${filename}`;
    }
}

/**
 * Render the categorized sidebar
 */
function renderCategorizedList() {
    const container = document.getElementById('libraryCategories');
    if (!container) return;

    // Group examples by category
    const categories = {};
    examples.forEach((example, index) => {
        if (!categories[example.category]) {
            categories[example.category] = [];
        }
        categories[example.category].push({ ...example, index });
    });

    // Build HTML
    let html = '';
    for (const [category, items] of Object.entries(categories)) {
        const info = categoryInfo[category] || { icon: '📄', color: '#888' };
        html += `
            <div class="library-category">
                <div class="category-header" style="border-left-color: ${info.color}">
                    <span class="category-icon">${info.icon}</span>
                    <span class="category-name">${category}</span>
                    <span class="category-count">${items.length}</span>
                </div>
                <div class="category-items">
                    ${items.map(item => `
                        <div class="library-item ${item.index === currentExample ? 'active' : ''}"
                             onclick="tutorialModule.selectExample(${item.index})"
                             data-index="${item.index}">
                            ${item.title}
                        </div>
                    `).join('')}
                </div>
            </div>
        `;
    }

    container.innerHTML = html;

    // Update example count
    const countEl = document.getElementById('exampleCount');
    if (countEl) countEl.textContent = examples.length + ' exemples';
}

/**
 * Select and display an example
 */
export async function selectExample(index) {
    currentExample = index;
    const example = examples[index];
    if (!example) return;

    // Update preview title
    const titleEl = document.getElementById('previewTitle');
    if (titleEl) {
        const info = categoryInfo[example.category] || { icon: '📄' };
        titleEl.innerHTML = `${info.icon} ${example.title}`;
    }

    // Update description
    const descEl = document.getElementById('previewDescription');
    if (descEl) {
        descEl.innerHTML = `<span class="preview-category">${example.category}</span> ${example.description}`;
    }

    // Load and display code from file
    const codeEl = document.getElementById('previewCode');
    if (codeEl) {
        codeEl.innerHTML = '<pre class="library-code-content">Chargement...</pre>';
        const code = await loadCobolFile(example.file);
        example.code = code; // Cache the code
        codeEl.innerHTML = `<pre class="library-code-content">${escapeHtml(code)}</pre>`;
    }

    // Update active state in sidebar
    document.querySelectorAll('.library-item').forEach(item => {
        item.classList.toggle('active', parseInt(item.dataset.index) === index);
    });
}

/**
 * Render the current example (legacy compatibility)
 */
export function renderExample() {
    selectExample(currentExample);
}

/**
 * Escape HTML special characters
 */
function escapeHtml(text) {
    return text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;');
}

/**
 * Navigate to previous example
 */
export function prevLesson() {
    if (currentExample > 0) {
        currentExample--;
        renderExample();
    }
}

/**
 * Navigate to next example
 */
export function nextLesson() {
    if (currentExample < examples.length - 1) {
        currentExample++;
        renderExample();
    }
}

/**
 * Load current example into editor
 */
export async function loadExample() {
    const example = examples[currentExample];
    if (!example) return;

    // Load code if not cached
    if (!example.code) {
        example.code = await loadCobolFile(example.file);
    }

    setCode(example.code);

    // Switch to editor tab
    document.querySelectorAll('.header-tab').forEach(tab => tab.classList.remove('active'));
    document.querySelectorAll('.header-tab')[0].classList.add('active');
    document.querySelectorAll('.tab-content').forEach(content => content.classList.remove('active'));
    document.getElementById('tab-editor')?.classList.add('active');

    // Don't auto-punch - let user click "Perforer"
}

// Keep old function name for compatibility
export const renderLesson = renderExample;
export const checkQuiz = () => {};
export const getLessonCount = () => examples.length;
export const getCurrentLessonIndex = () => currentExample;
