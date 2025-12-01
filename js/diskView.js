/**
 * DiskView - IBM 3330 Disk Drive Visualization
 * Visualise les opérations I/O COBOL sur un disque magnétique virtuel
 */

export class DiskView {
    constructor(containerId, options = {}) {
        this.container = document.getElementById(containerId);
        if (!this.container) {
            throw new Error(`Container ${containerId} not found`);
        }

        this.options = {
            diskRadius: 180,
            innerRadius: 40,
            trackCount: 8,
            rotationSpeed: 3000,
            ...options
        };

        this.datasets = new Map();
        this.currentTrack = 0;
        this.currentRecord = 0;
        this.ioLog = [];
        this.isSpinning = false;
        this.headAngle = 0;
        this.diskAngle = 0;

        this.datasetColors = [
            '#00ff88', '#ff6b6b', '#4ecdc4', '#ffe66d',
            '#a29bfe', '#fd79a8', '#74b9ff', '#55efc4'
        ];
        this.colorIndex = 0;

        this.init();
    }

    init() {
        this.container.innerHTML = this.renderHTML();
        this.canvas = this.container.querySelector('#disk-canvas');
        this.ctx = this.canvas.getContext('2d');
        this.recordDisplay = this.container.querySelector('#record-display');
        this.ioTimeline = this.container.querySelector('#io-timeline');
        this.statusBar = this.container.querySelector('#disk-status');

        this.setupCanvas();
        this.startSpinning();
        this.render();
    }

    renderHTML() {
        return `
        <div class="disk-view-container">
            <div class="disk-panel">
                <div class="disk-header">
                    <span class="disk-icon">💾</span>
                    <span class="disk-label">IBM 3330 - VOL=WORK01</span>
                    <span class="disk-rpm">●</span>
                </div>
                <canvas id="disk-canvas" width="400" height="400"></canvas>
                <div id="disk-status" class="disk-status">
                    <span class="status-cyl">CYL 000</span>
                    <span class="status-trk">TRK 00</span>
                    <span class="status-rec">REC 000</span>
                </div>
            </div>

            <div class="info-panel">
                <div class="record-panel">
                    <div class="panel-header">📄 ENREGISTREMENT COURANT</div>
                    <pre id="record-display" class="record-content">--- AUCUNE DONNÉE ---</pre>
                </div>

                <div class="timeline-panel">
                    <div class="panel-header">📊 JOURNAL I/O</div>
                    <div id="io-timeline" class="timeline-content"></div>
                </div>
            </div>
        </div>
        `;
    }

    setupCanvas() {
        const dpr = window.devicePixelRatio || 1;
        const rect = this.canvas.getBoundingClientRect();
        this.canvas.width = rect.width * dpr;
        this.canvas.height = rect.height * dpr;
        this.ctx.scale(dpr, dpr);
        this.canvas.style.width = rect.width + 'px';
        this.canvas.style.height = rect.height + 'px';

        this.centerX = rect.width / 2;
        this.centerY = rect.height / 2;
    }

    startSpinning() {
        this.isSpinning = true;
        this.lastTime = performance.now();
        this.animate();
    }

    animate() {
        if (!this.isSpinning) return;

        const now = performance.now();
        const delta = now - this.lastTime;
        this.lastTime = now;

        // Si on a une cible de seek, on tourne vers elle
        if (this.targetDiskAngle !== undefined) {
            let current = this.diskAngle % (Math.PI * 2);
            let target = this.targetDiskAngle % (Math.PI * 2);

            let diff = target - current;
            if (diff > Math.PI) diff -= Math.PI * 2;
            if (diff < -Math.PI) diff += Math.PI * 2;

            if (Math.abs(diff) > 0.02) {
                this.diskAngle += diff * 0.15;
            } else {
                this.diskAngle = this.targetDiskAngle;
                this.targetDiskAngle = undefined;
            }
        } else {
            // Rotation libre continue
            this.diskAngle = (this.diskAngle || 0) + (delta / this.options.rotationSpeed) * Math.PI * 2;
        }

        this.render();
        requestAnimationFrame(() => this.animate());
    }

    render() {
        const ctx = this.ctx;
        const { diskRadius, innerRadius, trackCount } = this.options;

        ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);

        // Platter
        ctx.beginPath();
        ctx.arc(this.centerX, this.centerY, diskRadius, 0, Math.PI * 2);
        ctx.fillStyle = '#1a1a1a';
        ctx.fill();

        // Tracks
        const trackWidth = (diskRadius - innerRadius) / trackCount;

        for (let i = 0; i < trackCount; i++) {
            const radius = innerRadius + (i + 0.5) * trackWidth;

            ctx.beginPath();
            ctx.arc(this.centerX, this.centerY, radius, 0, Math.PI * 2);
            ctx.strokeStyle = '#2a2a2a';
            ctx.lineWidth = trackWidth - 2;
            ctx.stroke();

            ctx.beginPath();
            ctx.arc(this.centerX, this.centerY, radius, 0, Math.PI * 2);
            ctx.strokeStyle = '#333';
            ctx.lineWidth = 1;
            ctx.stroke();
        }

        // Datasets
        this.datasets.forEach((dataset) => {
            this.drawDataset(ctx, dataset, trackWidth);
        });

        // Center hub
        ctx.beginPath();
        ctx.arc(this.centerX, this.centerY, innerRadius, 0, Math.PI * 2);
        ctx.fillStyle = '#333';
        ctx.fill();
        ctx.strokeStyle = '#444';
        ctx.lineWidth = 2;
        ctx.stroke();

        // Spindle
        ctx.beginPath();
        ctx.arc(this.centerX, this.centerY, 15, 0, Math.PI * 2);
        ctx.fillStyle = '#555';
        ctx.fill();

        // Rotation indicator
        ctx.beginPath();
        ctx.moveTo(this.centerX, this.centerY);
        ctx.lineTo(
            this.centerX + Math.cos(this.diskAngle) * (innerRadius - 5),
            this.centerY + Math.sin(this.diskAngle) * (innerRadius - 5)
        );
        ctx.strokeStyle = '#00ff88';
        ctx.lineWidth = 2;
        ctx.stroke();

        // Head
        this.drawHead(ctx, trackWidth);
    }

    drawDataset(ctx, dataset, trackWidth) {
        const { innerRadius } = this.options;
        const radius = innerRadius + (dataset.track + 0.5) * trackWidth;

        const recordCount = dataset.records || 10;
        const arcLength = Math.min(recordCount / 20, 0.8) * Math.PI * 2;
        const startAngle = (dataset.startAngle || 0) + this.diskAngle;

        ctx.beginPath();
        ctx.arc(this.centerX, this.centerY, radius, startAngle, startAngle + arcLength);
        ctx.strokeStyle = dataset.color;
        ctx.lineWidth = trackWidth - 6;
        ctx.lineCap = 'round';
        ctx.stroke();

        // Label
        const labelAngle = startAngle + arcLength / 2;
        const labelX = this.centerX + Math.cos(labelAngle) * radius;
        const labelY = this.centerY + Math.sin(labelAngle) * radius;

        ctx.save();
        ctx.translate(labelX, labelY);
        ctx.rotate(labelAngle + Math.PI / 2);
        ctx.fillStyle = '#000';
        ctx.font = 'bold 9px "IBM Plex Mono", monospace';
        ctx.textAlign = 'center';
        ctx.fillText(dataset.name.substring(0, 8), 0, 3);
        ctx.restore();

        // Active record highlight
        if (dataset.activeRecord !== undefined) {
            const recordAngle = startAngle + (dataset.activeRecord / Math.max(recordCount, 1)) * arcLength;
            ctx.beginPath();
            ctx.arc(
                this.centerX + Math.cos(recordAngle) * radius,
                this.centerY + Math.sin(recordAngle) * radius,
                5, 0, Math.PI * 2
            );
            ctx.fillStyle = dataset.flashColor || '#fff';
            ctx.fill();
        }
    }

    drawHead(ctx, trackWidth) {
        const { innerRadius, diskRadius } = this.options;
        const headRadius = innerRadius + (this.currentTrack + 0.5) * trackWidth;

        const headX = this.centerX + headRadius;
        const headY = this.centerY;

        // Bras de lecture
        ctx.beginPath();
        ctx.moveTo(this.centerX + diskRadius + 40, this.centerY);
        ctx.lineTo(headX + 10, headY);
        ctx.strokeStyle = '#666';
        ctx.lineWidth = 8;
        ctx.lineCap = 'round';
        ctx.stroke();

        // Pivot du bras
        ctx.beginPath();
        ctx.arc(this.centerX + diskRadius + 40, this.centerY, 6, 0, Math.PI * 2);
        ctx.fillStyle = '#555';
        ctx.fill();

        // Tête de lecture
        ctx.beginPath();
        ctx.arc(headX, headY, 8, 0, Math.PI * 2);
        ctx.fillStyle = this.headColor || '#888';
        ctx.fill();
        ctx.strokeStyle = '#aaa';
        ctx.lineWidth = 2;
        ctx.stroke();

        // Glow pendant I/O
        if (this.headGlow) {
            ctx.beginPath();
            ctx.arc(headX, headY, 14, 0, Math.PI * 2);
            ctx.fillStyle = this.headGlow;
            ctx.globalAlpha = 0.5;
            ctx.fill();
            ctx.globalAlpha = 1;
        }
    }

    seekToRecord(fileName, recordNumber) {
        const dataset = this.datasets.get(fileName);
        if (!dataset) return;

        const recordCount = Math.max(dataset.records || 10, 1);
        const arcLength = Math.min(recordCount / 20, 0.8) * Math.PI * 2;
        const recordPosInArc = (recordNumber || 0) / recordCount * arcLength;
        const recordBaseAngle = dataset.startAngle + recordPosInArc;

        this.targetDiskAngle = -recordBaseAngle;
    }

    registerDataset(fileName, options = {}) {
        if (this.datasets.has(fileName)) return;

        const track = this.datasets.size % this.options.trackCount;
        const color = this.datasetColors[this.colorIndex++ % this.datasetColors.length];

        this.datasets.set(fileName, {
            name: fileName,
            track: track,
            color: color,
            startAngle: Math.random() * Math.PI * 2,
            records: options.records || 0,
            ...options
        });

        this.render();
    }

    onDiskIO(event) {
        const { operation, fileName, record, recordNumber, cobolLine } = event;

        if (!this.datasets.has(fileName)) {
            this.registerDataset(fileName);
        }

        const dataset = this.datasets.get(fileName);
        this.currentTrack = dataset.track;
        this.currentRecord = recordNumber || 0;

        switch (operation) {
            case 'READ':
                this.flashRead(dataset, recordNumber);
                this.displayRecord(record, fileName, recordNumber);
                break;
            case 'WRITE':
                this.flashWrite(dataset, recordNumber);
                dataset.records = Math.max(dataset.records, (recordNumber || 0) + 1);
                this.displayRecord(record, fileName, recordNumber);
                break;
            case 'OPEN':
                this.flashOpen(dataset);
                break;
            case 'CLOSE':
                this.flashClose(dataset);
                break;
        }

        this.updateStatus();
        this.addToTimeline(event);
        this.render();
    }

    flashRead(dataset, recordNumber) {
        dataset.activeRecord = recordNumber;
        dataset.flashColor = '#00ff88';
        this.headGlow = 'rgba(0, 255, 136, 0.6)';
        this.headColor = '#00ff88';
        this.canvas.classList.add('flash-read');

        this.seekToRecord(dataset.name, recordNumber);

        setTimeout(() => {
            dataset.activeRecord = undefined;
            this.headGlow = null;
            this.headColor = '#888';
            this.canvas.classList.remove('flash-read');
            this.render();
        }, 300);
    }

    flashWrite(dataset, recordNumber) {
        dataset.activeRecord = recordNumber;
        dataset.flashColor = '#ff6b6b';
        this.headGlow = 'rgba(255, 107, 107, 0.6)';
        this.headColor = '#ff6b6b';
        this.canvas.classList.add('flash-write');

        this.seekToRecord(dataset.name, recordNumber);

        setTimeout(() => {
            dataset.activeRecord = undefined;
            this.headGlow = null;
            this.headColor = '#888';
            this.canvas.classList.remove('flash-write');
            this.render();
        }, 300);
    }

    flashOpen(dataset) {
        this.headColor = '#4ecdc4';
        this.seekToRecord(dataset.name, 0);
        setTimeout(() => { this.headColor = '#888'; this.render(); }, 200);
    }

    flashClose(dataset) {
        this.headColor = '#ffe66d';
        setTimeout(() => { this.headColor = '#888'; this.render(); }, 200);
    }

    displayRecord(record, fileName, recordNumber) {
        if (!record) {
            this.recordDisplay.textContent = '--- AUCUNE DONNÉE ---';
            return;
        }

        let output = `┌─ ${fileName} ─ REC #${String(recordNumber || 0).padStart(3, '0')} ─┐\n`;
        output += '│' + '─'.repeat(48) + '│\n';

        if (typeof record === 'object') {
            for (const [key, value] of Object.entries(record)) {
                const line = `${key}: ${value}`;
                output += '│ ' + line.padEnd(47) + '│\n';
            }
        } else {
            output += '│ ' + String(record).padEnd(47) + '│\n';
        }

        output += '└' + '─'.repeat(48) + '┘';
        this.recordDisplay.textContent = output;
    }

    addToTimeline(event) {
        const { operation, fileName, recordNumber, cobolLine } = event;

        const opClass = operation.toLowerCase();
        const entry = document.createElement('div');
        entry.className = `io-entry ${opClass}`;
        entry.innerHTML = `
            <span class="io-op ${opClass}">${operation}</span>
            <span class="io-file">${fileName}</span>
            <span class="io-rec">${recordNumber !== undefined ? `#${recordNumber}` : ''}</span>
            <span class="io-line" data-line="${cobolLine}">L.${cobolLine || '?'}</span>
        `;

        entry.querySelector('.io-line').addEventListener('click', () => {
            if (this.onLineClick && cobolLine) {
                this.onLineClick(cobolLine);
            }
        });

        this.ioTimeline.insertBefore(entry, this.ioTimeline.firstChild);

        while (this.ioTimeline.children.length > 20) {
            this.ioTimeline.removeChild(this.ioTimeline.lastChild);
        }
    }

    updateStatus() {
        const cyl = String(Math.floor(this.currentTrack / 2)).padStart(3, '0');
        const trk = String(this.currentTrack % 2).padStart(2, '0');
        const rec = String(this.currentRecord).padStart(3, '0');

        this.statusBar.innerHTML = `
            <span class="status-cyl">CYL ${cyl}</span>
            <span class="status-trk">TRK ${trk}</span>
            <span class="status-rec">REC ${rec}</span>
        `;
    }

    reset() {
        this.datasets.clear();
        this.ioLog = [];
        this.currentTrack = 0;
        this.currentRecord = 0;
        this.colorIndex = 0;
        this.ioTimeline.innerHTML = '';
        this.recordDisplay.textContent = '--- AUCUNE DONNÉE ---';
        this.updateStatus();
        this.render();
    }

    setLineClickHandler(callback) {
        this.onLineClick = callback;
    }
}
