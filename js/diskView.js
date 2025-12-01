/**
 * DiskView - IBM 3330 Disk Drive Visualization
 * Visualise les opérations I/O COBOL sur un disque magnétique virtuel
 * Basé sur disk-demo.html
 */

export class DiskView {
    constructor(containerId, options = {}) {
        this.container = document.getElementById(containerId);
        if (!this.container) {
            console.warn(`DiskView: Container ${containerId} not found`);
            return;
        }

        this.options = {
            diskRadius: 180,
            innerRadius: 40,
            trackCount: 8,
            sectorsPerTrack: 20,  // Capacité max par piste
            rotationSpeed: 3000,
            maxRecordsPerFile: 40, // Rotation automatique des logs au-delà
            ...options
        };

        this.datasets = new Map();
        this.currentTrack = 0;
        this.currentRecord = 0;
        this.ioLog = [];
        this.isSpinning = false;
        this.headAngle = 0;
        this.diskAngle = 0;

        // Disk allocation map: track -> [{dataset, startSector, sectorCount}]
        this.diskMap = new Array(this.options.trackCount).fill(null).map(() => []);

        this.datasetColors = [
            '#00ff88', '#ff6b6b', '#4ecdc4', '#ffe66d',
            '#a29bfe', '#fd79a8', '#74b9ff', '#55efc4'
        ];
        this.colorIndex = 0;

        this.init();
    }

    init() {
        if (!this.container) return;
        this.container.innerHTML = this.renderHTML();
        this.canvas = this.container.querySelector('#disk-canvas');
        this.ctx = this.canvas.getContext('2d');
        this.recordDisplay = this.container.querySelector('#record-display');
        this.ioTimeline = this.container.querySelector('#io-timeline');
        this.statusBar = this.container.querySelector('#disk-status');

        this.setupCanvas();
        this.render();  // Initial render without spinning
    }

    renderHTML() {
        return `
        <div class="disk-view-container">
            <div class="disk-panel">
                <div class="disk-header">
                    <span class="disk-icon">💾</span>
                    <span class="disk-label">HOPPER 33XX - VOL=WORK01</span>
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

        <style>
            .disk-view-container {
                display: flex;
                gap: 20px;
                padding: 20px;
                background: #0a0a0a;
                border: 2px solid #00ff88;
                border-radius: 8px;
                font-family: 'IBM Plex Mono', 'Courier New', monospace;
                height: 100%;
                box-sizing: border-box;
            }

            @media (max-width: 800px) {
                .disk-view-container {
                    flex-direction: column;
                }
            }

            .disk-view-container .disk-panel {
                display: flex;
                flex-direction: column;
                align-items: center;
                background: #111;
                padding: 15px;
                border-radius: 8px;
                border: 1px solid #333;
            }

            .disk-header {
                display: flex;
                align-items: center;
                gap: 10px;
                margin-bottom: 10px;
                color: #00ff88;
                font-size: 14px;
            }

            .disk-icon { font-size: 20px; }

            .disk-rpm {
                color: #ff0;
                animation: diskBlink 0.5s infinite;
            }

            @keyframes diskBlink {
                50% { opacity: 0.3; }
            }

            #disk-canvas {
                background: radial-gradient(circle, #1a1a1a 0%, #0a0a0a 100%);
                border-radius: 50%;
                box-shadow:
                    0 0 20px rgba(0, 255, 136, 0.2),
                    inset 0 0 60px rgba(0, 0, 0, 0.8);
            }

            .disk-status {
                display: flex;
                gap: 20px;
                margin-top: 10px;
                padding: 8px 15px;
                background: #000;
                border: 1px solid #00ff88;
                border-radius: 4px;
                font-size: 12px;
                color: #00ff88;
            }

            .disk-view-container .info-panel {
                display: flex;
                flex-direction: column;
                gap: 15px;
                flex: 1;
                min-width: 300px;
            }

            .record-panel, .timeline-panel {
                background: #111;
                border: 1px solid #333;
                border-radius: 8px;
                overflow: hidden;
            }

            .disk-view-container .panel-header {
                background: #1a1a1a;
                padding: 8px 12px;
                color: #00ff88;
                font-size: 12px;
                border-bottom: 1px solid #333;
            }

            .record-content {
                margin: 0;
                padding: 12px;
                font-size: 10px;
                color: #0f0;
                background: #000;
                min-height: 100px;
                max-height: 180px;
                overflow-y: auto;
                white-space: pre-wrap;
                word-break: break-all;
            }

            .timeline-content {
                padding: 8px;
                max-height: 200px;
                overflow-y: auto;
                background: #000;
            }

            .io-entry {
                display: flex;
                align-items: center;
                gap: 8px;
                padding: 6px 8px;
                margin-bottom: 4px;
                border-radius: 4px;
                font-size: 11px;
                background: #111;
                border-left: 3px solid #333;
                transition: background 0.2s;
            }

            .io-entry:hover { background: #1a1a1a; }
            .io-entry.read { border-left-color: #00ff88; }
            .io-entry.write { border-left-color: #ff6b6b; }
            .io-entry.open { border-left-color: #4ecdc4; }
            .io-entry.close { border-left-color: #ffe66d; }

            .io-op { font-weight: bold; width: 60px; }
            .io-op.read { color: #00ff88; }
            .io-op.write { color: #ff6b6b; }
            .io-op.open { color: #4ecdc4; }
            .io-op.close { color: #ffe66d; }

            .io-file { color: #888; }
            .io-rec { color: #aaa; }
            .io-line { color: #666; margin-left: auto; cursor: pointer; }
            .io-line:hover { color: #00ff88; }

            .flash-read { animation: flashGreen 0.3s ease-out; }
            .flash-write { animation: flashRed 0.3s ease-out; }

            @keyframes flashGreen {
                0% { box-shadow: 0 0 30px rgba(0, 255, 136, 0.8); }
                100% { box-shadow: 0 0 20px rgba(0, 255, 136, 0.2); }
            }

            @keyframes flashRed {
                0% { box-shadow: 0 0 30px rgba(255, 107, 107, 0.8); }
                100% { box-shadow: 0 0 20px rgba(0, 255, 136, 0.2); }
            }
        </style>
        `;
    }

    setupCanvas() {
        if (!this.canvas) return;
        const dpr = window.devicePixelRatio || 1;
        const rect = this.canvas.getBoundingClientRect();

        // Use fallback dimensions if tab is hidden (getBoundingClientRect returns 0)
        const width = rect.width > 0 ? rect.width : 400;
        const height = rect.height > 0 ? rect.height : 400;

        this.canvas.width = width * dpr;
        this.canvas.height = height * dpr;
        this.ctx.scale(dpr, dpr);
        this.canvas.style.width = width + 'px';
        this.canvas.style.height = height + 'px';

        this.centerX = width / 2;
        this.centerY = height / 2;
    }

    startSpinning() {
        if (this.isSpinning) return;
        this.isSpinning = true;
        this.lastTime = performance.now();
        this.animate();
    }

    stopSpinning() {
        this.isSpinning = false;
        this.spinStopTime = undefined;
    }

    /**
     * Spin the disk for a specific duration (for I/O operations)
     * @param {number} duration - Duration in ms
     */
    spinFor(duration = 500) {
        this.spinStopTime = performance.now() + duration;
        this.startSpinning();
    }

    animate() {
        if (!this.isSpinning) return;

        const now = performance.now();
        const delta = now - this.lastTime;
        this.lastTime = now;

        // Check if we should stop spinning (timed spin)
        if (this.spinStopTime && now >= this.spinStopTime) {
            // If we have a target angle, finish the seek first
            if (this.targetDiskAngle === undefined) {
                this.isSpinning = false;
                this.spinStopTime = undefined;
                return;
            }
        }

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
                // Stop spinning after reaching target if no spinStopTime or time elapsed
                if (!this.spinStopTime || now >= this.spinStopTime) {
                    this.isSpinning = false;
                    this.spinStopTime = undefined;
                    this.render();
                    return;
                }
            }
        } else if (!this.spinStopTime) {
            // No target and no timed spin - stop
            this.isSpinning = false;
            return;
        } else {
            // Timed rotation without target
            this.diskAngle = (this.diskAngle || 0) + (delta / this.options.rotationSpeed) * Math.PI * 2;
        }

        this.render();
        requestAnimationFrame(() => this.animate());
    }

    render() {
        if (!this.ctx) return;
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
        const { innerRadius, sectorsPerTrack } = this.options;

        // Draw each extent on its track
        for (const extent of dataset.extents || []) {
            const radius = innerRadius + (extent.track + 0.5) * trackWidth;

            // Convert sectors to arc angles
            const sectorAngle = (Math.PI * 2) / sectorsPerTrack;
            const startAngle = extent.startSector * sectorAngle + this.diskAngle;
            const arcLength = extent.sectorCount * sectorAngle;

            ctx.beginPath();
            ctx.arc(this.centerX, this.centerY, radius, startAngle, startAngle + arcLength);
            ctx.strokeStyle = dataset.color;
            ctx.lineWidth = trackWidth - 6;
            ctx.lineCap = 'round';
            ctx.stroke();

            // Label on first extent only
            if (extent === dataset.extents[0]) {
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
            }
        }

        // Active record highlight
        if (dataset.activeRecord !== undefined) {
            const pos = this.getRecordPosition(dataset, dataset.activeRecord);
            if (pos) {
                ctx.beginPath();
                ctx.arc(pos.x, pos.y, 5, 0, Math.PI * 2);
                ctx.fillStyle = dataset.flashColor || '#fff';
                ctx.fill();
            }
        }
    }

    /**
     * Get the x,y position of a record on the disk
     */
    getRecordPosition(dataset, recordNumber) {
        const { innerRadius, sectorsPerTrack } = this.options;
        const trackWidth = (this.options.diskRadius - innerRadius) / this.options.trackCount;

        // Find which extent contains this record
        let sectorOffset = 0;
        for (const extent of dataset.extents || []) {
            if (recordNumber < sectorOffset + extent.sectorCount) {
                // Record is in this extent
                const sectorInExtent = recordNumber - sectorOffset;
                const sectorAngle = (Math.PI * 2) / sectorsPerTrack;
                const angle = (extent.startSector + sectorInExtent) * sectorAngle + this.diskAngle;
                const radius = innerRadius + (extent.track + 0.5) * trackWidth;

                return {
                    x: this.centerX + Math.cos(angle) * radius,
                    y: this.centerY + Math.sin(angle) * radius,
                    track: extent.track,
                    angle: angle
                };
            }
            sectorOffset += extent.sectorCount;
        }
        return null;
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

        // Find which extent and sector for this record
        let sectorOffset = 0;
        for (const extent of dataset.extents || []) {
            if (recordNumber < sectorOffset + extent.sectorCount) {
                const sectorInExtent = recordNumber - sectorOffset;
                const sectorAngle = (Math.PI * 2) / this.options.sectorsPerTrack;
                const absoluteSector = extent.startSector + sectorInExtent;

                // Move head to correct track
                this.currentTrack = extent.track;

                // Rotate disk so the sector is under the head (at angle 0 = right side)
                // We want: absoluteSector * sectorAngle + diskAngle = 0
                // So: diskAngle = -absoluteSector * sectorAngle
                this.targetDiskAngle = -absoluteSector * sectorAngle;
                return;
            }
            sectorOffset += extent.sectorCount;
        }

        // Fallback: just go to first extent
        if (dataset.extents && dataset.extents.length > 0) {
            this.currentTrack = dataset.extents[0].track;
        }
    }

    registerDataset(fileName, options = {}) {
        if (this.datasets.has(fileName)) return;

        const color = this.datasetColors[this.colorIndex++ % this.datasetColors.length];
        const initialRecords = options.records || 1;

        // Allocate space on disk
        const extents = this.allocateSpace(fileName, initialRecords);

        this.datasets.set(fileName, {
            name: fileName,
            color: color,
            extents: extents,  // [{track, startSector, sectorCount}]
            records: initialRecords,
            ...options
        });

        this.render();
    }

    /**
     * Allocate disk space for a dataset
     * Returns array of extents: [{track, startSector, sectorCount}]
     */
    allocateSpace(fileName, sectorCount) {
        const extents = [];
        let remaining = sectorCount;
        const { trackCount, sectorsPerTrack } = this.options;

        // Find first track that has space OR is empty
        // This ensures different files start on different tracks when possible
        let startTrack = 0;

        // If this is a new dataset (not expansion), try to find a track with no allocations
        // or continue from where we left off
        const existingExtents = this.datasets.get(fileName)?.extents;
        if (!existingExtents || existingExtents.length === 0) {
            // New dataset - find first track with free space, preferring empty tracks
            for (let t = 0; t < trackCount; t++) {
                if (this.diskMap[t].length === 0) {
                    startTrack = t;
                    break;
                }
            }
        } else {
            // Expansion - continue from last extent's track
            const lastExtent = existingExtents[existingExtents.length - 1];
            startTrack = lastExtent.track;
        }

        for (let track = startTrack; track < trackCount && remaining > 0; track++) {
            const trackAllocs = this.diskMap[track];
            const usedSectors = this.getUsedSectorsOnTrack(track);
            const freeSectors = sectorsPerTrack - usedSectors;

            if (freeSectors > 0) {
                // Find starting position (after last allocation on this track)
                let startSector = 0;
                if (trackAllocs.length > 0) {
                    const lastAlloc = trackAllocs[trackAllocs.length - 1];
                    startSector = lastAlloc.startSector + lastAlloc.sectorCount;
                }

                const allocSize = Math.min(remaining, freeSectors);
                const extent = {
                    dataset: fileName,
                    track: track,
                    startSector: startSector,
                    sectorCount: allocSize
                };

                trackAllocs.push(extent);
                extents.push(extent);
                remaining -= allocSize;
            }
        }

        // If we couldn't allocate from startTrack, try from beginning
        if (remaining > 0 && startTrack > 0) {
            for (let track = 0; track < startTrack && remaining > 0; track++) {
                const trackAllocs = this.diskMap[track];
                const usedSectors = this.getUsedSectorsOnTrack(track);
                const freeSectors = sectorsPerTrack - usedSectors;

                if (freeSectors > 0) {
                    let startSector = 0;
                    if (trackAllocs.length > 0) {
                        const lastAlloc = trackAllocs[trackAllocs.length - 1];
                        startSector = lastAlloc.startSector + lastAlloc.sectorCount;
                    }

                    const allocSize = Math.min(remaining, freeSectors);
                    const extent = {
                        dataset: fileName,
                        track: track,
                        startSector: startSector,
                        sectorCount: allocSize
                    };

                    trackAllocs.push(extent);
                    extents.push(extent);
                    remaining -= allocSize;
                }
            }
        }

        return extents;
    }

    /**
     * Get total used sectors on a track
     */
    getUsedSectorsOnTrack(track) {
        return this.diskMap[track].reduce((sum, alloc) => sum + alloc.sectorCount, 0);
    }

    /**
     * Expand dataset allocation when records are added
     */
    expandDataset(fileName, newRecordCount) {
        const dataset = this.datasets.get(fileName);
        if (!dataset) return;

        const currentTotal = dataset.extents.reduce((sum, ext) => sum + ext.sectorCount, 0);
        const needed = newRecordCount - currentTotal;

        if (needed > 0) {
            const newExtents = this.allocateSpace(fileName, needed);
            dataset.extents.push(...newExtents);
        }

        dataset.records = newRecordCount;
    }

    /**
     * Shrink dataset allocation when records are deleted
     * Frees sectors from the end of the allocation
     */
    shrinkDataset(fileName, sectorsToFree = 1) {
        const dataset = this.datasets.get(fileName);
        if (!dataset || !dataset.extents || dataset.extents.length === 0) return;

        let remaining = sectorsToFree;

        // Free from the last extents first
        while (remaining > 0 && dataset.extents.length > 0) {
            const lastExtent = dataset.extents[dataset.extents.length - 1];

            if (lastExtent.sectorCount <= remaining) {
                // Remove entire extent
                remaining -= lastExtent.sectorCount;
                dataset.extents.pop();

                // Remove from diskMap
                const trackAllocs = this.diskMap[lastExtent.track];
                const idx = trackAllocs.findIndex(a =>
                    a.dataset === fileName &&
                    a.startSector === lastExtent.startSector
                );
                if (idx !== -1) {
                    trackAllocs.splice(idx, 1);
                }
            } else {
                // Shrink this extent
                lastExtent.sectorCount -= remaining;

                // Update diskMap
                const trackAllocs = this.diskMap[lastExtent.track];
                const alloc = trackAllocs.find(a =>
                    a.dataset === fileName &&
                    a.startSector === lastExtent.startSector
                );
                if (alloc) {
                    alloc.sectorCount = lastExtent.sectorCount;
                }
                remaining = 0;
            }
        }

        dataset.records = Math.max(0, dataset.records - sectorsToFree);
    }

    /**
     * Shrink dataset from the beginning (for log rotation - FIFO)
     * Frees oldest sectors first
     */
    shrinkDatasetFromStart(fileName, sectorsToFree = 1) {
        const dataset = this.datasets.get(fileName);
        if (!dataset || !dataset.extents || dataset.extents.length === 0) return;

        let remaining = sectorsToFree;

        // Free from the first extents (oldest data)
        while (remaining > 0 && dataset.extents.length > 0) {
            const firstExtent = dataset.extents[0];

            if (firstExtent.sectorCount <= remaining) {
                // Remove entire extent
                remaining -= firstExtent.sectorCount;
                dataset.extents.shift();

                // Remove from diskMap
                const trackAllocs = this.diskMap[firstExtent.track];
                const idx = trackAllocs.findIndex(a =>
                    a.dataset === fileName &&
                    a.startSector === firstExtent.startSector
                );
                if (idx !== -1) {
                    trackAllocs.splice(idx, 1);
                }
            } else {
                // Shrink from start of this extent
                const freed = remaining;
                firstExtent.startSector += freed;
                firstExtent.sectorCount -= freed;

                // Update diskMap
                const trackAllocs = this.diskMap[firstExtent.track];
                const alloc = trackAllocs.find(a =>
                    a.dataset === fileName &&
                    a.startSector === firstExtent.startSector - freed
                );
                if (alloc) {
                    alloc.startSector = firstExtent.startSector;
                    alloc.sectorCount = firstExtent.sectorCount;
                }
                remaining = 0;
            }
        }

        dataset.records = Math.max(0, dataset.records - sectorsToFree);
    }

    onDiskIO(event) {
        if (!this.container) return;
        const { operation, fileName, record, recordNumber, cobolLine } = event;

        // Spin the disk for I/O operation
        this.spinFor(400);

        if (!this.datasets.has(fileName)) {
            this.registerDataset(fileName);
        }

        const dataset = this.datasets.get(fileName);

        // Update current track based on record position
        const pos = this.getRecordPosition(dataset, recordNumber || 0);
        if (pos) {
            this.currentTrack = pos.track;
        } else if (dataset.extents && dataset.extents.length > 0) {
            this.currentTrack = dataset.extents[0].track;
        }
        this.currentRecord = recordNumber || 0;

        switch (operation) {
            case 'READ':
                this.flashRead(dataset, recordNumber);
                this.displayRecord(record, fileName, recordNumber);
                break;
            case 'WRITE':
                // Check if we need log rotation (max 40 records per file for visual clarity)
                const maxRecords = this.options.maxRecordsPerFile || 40;
                if (dataset.records >= maxRecords) {
                    // Rotate: free oldest sector to make room
                    this.shrinkDatasetFromStart(fileName, 1);
                }
                // Expand allocation if needed
                const newRecordCount = Math.max(dataset.records, (recordNumber || 0) + 1);
                if (newRecordCount > dataset.records) {
                    this.expandDataset(fileName, newRecordCount);
                }
                this.flashWrite(dataset, recordNumber);
                this.displayRecord(record, fileName, recordNumber);
                break;
            case 'REWRITE':
                this.flashRewrite(dataset, recordNumber);
                this.displayRecord(record, fileName, recordNumber);
                break;
            case 'DELETE':
                // Free the sector used by this record
                this.shrinkDataset(fileName, 1);
                this.flashDelete(dataset, recordNumber);
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
        if (this.canvas) this.canvas.classList.add('flash-read');

        this.seekToRecord(dataset.name, recordNumber);

        setTimeout(() => {
            dataset.activeRecord = undefined;
            this.headGlow = null;
            this.headColor = '#888';
            if (this.canvas) this.canvas.classList.remove('flash-read');
            this.render();
        }, 300);
    }

    flashWrite(dataset, recordNumber) {
        dataset.activeRecord = recordNumber;
        dataset.flashColor = '#ff6b6b';
        this.headGlow = 'rgba(255, 107, 107, 0.6)';
        this.headColor = '#ff6b6b';
        if (this.canvas) this.canvas.classList.add('flash-write');

        this.seekToRecord(dataset.name, recordNumber);

        setTimeout(() => {
            dataset.activeRecord = undefined;
            this.headGlow = null;
            this.headColor = '#888';
            if (this.canvas) this.canvas.classList.remove('flash-write');
            this.render();
        }, 300);
    }

    flashRewrite(dataset, recordNumber) {
        dataset.activeRecord = recordNumber;
        dataset.flashColor = '#ffa500'; // Orange for update
        this.headGlow = 'rgba(255, 165, 0, 0.6)';
        this.headColor = '#ffa500';
        if (this.canvas) this.canvas.classList.add('flash-write');

        this.seekToRecord(dataset.name, recordNumber);

        setTimeout(() => {
            dataset.activeRecord = undefined;
            this.headGlow = null;
            this.headColor = '#888';
            if (this.canvas) this.canvas.classList.remove('flash-write');
            this.render();
        }, 300);
    }

    flashDelete(dataset, recordNumber) {
        dataset.activeRecord = recordNumber;
        dataset.flashColor = '#ff0000'; // Red for delete
        this.headGlow = 'rgba(255, 0, 0, 0.6)';
        this.headColor = '#ff0000';

        this.seekToRecord(dataset.name, recordNumber);

        setTimeout(() => {
            dataset.activeRecord = undefined;
            this.headGlow = null;
            this.headColor = '#888';
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
        if (!this.recordDisplay) return;
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
        if (!this.ioTimeline) return;
        const { operation, fileName, recordNumber, cobolLine } = event;

        const opClass = operation.toLowerCase();
        const entry = document.createElement('div');
        entry.className = `io-entry ${opClass}`;
        entry.innerHTML = `
            <span class="io-op ${opClass}">${operation}</span>
            <span class="io-file">${fileName}</span>
            <span class="io-rec">${recordNumber !== undefined ? `#${recordNumber}` : ''}</span>
            ${cobolLine ? `<span class="io-line" data-line="${cobolLine}">L.${cobolLine}</span>` : ''}
        `;

        const lineEl = entry.querySelector('.io-line');
        if (lineEl) {
            lineEl.addEventListener('click', () => {
                if (this.onLineClick && cobolLine) {
                    this.onLineClick(cobolLine);
                }
            });
        }

        this.ioTimeline.insertBefore(entry, this.ioTimeline.firstChild);

        while (this.ioTimeline.children.length > 20) {
            this.ioTimeline.removeChild(this.ioTimeline.lastChild);
        }
    }

    updateStatus() {
        if (!this.statusBar) return;
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
        // Reset disk allocation map
        this.diskMap = new Array(this.options.trackCount).fill(null).map(() => []);
        if (this.ioTimeline) this.ioTimeline.innerHTML = '';
        if (this.recordDisplay) this.recordDisplay.textContent = '--- AUCUNE DONNÉE ---';
        this.updateStatus();
        this.render();
    }

    setLineClickHandler(callback) {
        this.onLineClick = callback;
    }
}
