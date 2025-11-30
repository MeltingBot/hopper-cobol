# HOPPER - COBOL Emulator

A web-based COBOL emulator with IBM 029 punch card visualization, running entirely in the browser.

**[Live Demo](https://meltingbot.github.io/hopper-cobol/)**

![HOPPER Screenshot](screenshot.png)

## Features

- **Multi-Dialect COBOL Interpreter** - Supports COBOL-68, 74, 85, 2002, and 2014
- **Auto-Detection** - Automatically detects the appropriate dialect from source code
- **IBM 3270 Screen Control** - Full-screen terminal emulation with positioning and attributes
- **Punch Card Visualization** - Realistic IBM 029 keypunch simulation with animations
- **Debug Mode** - Step-by-step execution with variable inspection
- **File I/O** - Simulated SEQUENTIAL and INDEXED file operations via IndexedDB
- **SORT/MERGE** - Full support for sorting and merging data files
- **10 Example Programs** - Banking, invoicing, payroll, inventory management, etc.
- **Retro Terminal UI** - Authentic phosphor green aesthetic

## Supported COBOL Dialects

| Dialect | Standard | Key Features |
|---------|----------|--------------|
| COBOL-68 | ANSI | Basic COBOL, GO TO, PERFORM |
| COBOL-74 | ANSI X3.23-1974 | CALL, nested programs |
| COBOL-85 | ANSI X3.23-1985 | END-IF, EVALUATE, inline PERFORM |
| COBOL-2002 | ISO/IEC 1989:2002 | OOP, FUNCTION, Unicode |
| COBOL-2014 | ISO/IEC 1989:2014 | Dynamic tables, JSON support |

Use the dialect selector in the editor to choose a specific version or let auto-detection find the right one.

## Supported COBOL Features

### Data Division
- Level numbers (01-49, 66, 77, 88)
- 88-level condition names with VALUE/VALUES/THRU
- PIC clauses (X, 9, A, V, S, Z)
- VALUE clauses
- WORKING-STORAGE and FILE SECTION

### Procedure Division
- DISPLAY, ACCEPT
- MOVE, INITIALIZE
- ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
- IF/ELSE/END-IF
- EVALUATE/WHEN/END-EVALUATE
- PERFORM (TIMES, UNTIL, VARYING, inline)
- GO TO, EXIT, STOP RUN
- SORT/MERGE with INPUT/OUTPUT PROCEDURE
- File operations: OPEN, READ, WRITE, REWRITE, DELETE, CLOSE

### Screen Control (IBM 3270 Extensions)

HOPPER supports IBM 3270 style screen control for building interactive terminal applications:

```cobol
      * Positioning
       DISPLAY 'Hello' LINE 5 POSITION 10.
       ACCEPT WS-NAME LINE 5 COLUMN 20.

      * Screen clearing
       DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.    *> Erase to End Of Screen
       DISPLAY ' ' LINE 1 POSITION 1 ERASE EOL.    *> Erase to End Of Line
       DISPLAY ' ' LINE 1 POSITION 1 ERASE SCREEN. *> Clear entire screen

      * Display attributes
       DISPLAY 'Important!' LINE 3 POSITION 1 HIGHLIGHT.
       DISPLAY 'Warning' LINE 4 POSITION 1 BLINK.
       DISPLAY 'Selected' LINE 5 POSITION 1 REVERSE-VIDEO.
       DISPLAY 'Link' LINE 6 POSITION 1 UNDERLINE.

      * Combined attributes
       DISPLAY 'Critical' LINE 7 POSITION 1 HIGHLIGHT BLINK.
```

**Supported DISPLAY options:**
- `LINE n` / `POSITION n` / `COLUMN n` / `COL n` - Cursor positioning (1-based)
- `ERASE EOS` / `ERASE EOL` / `ERASE SCREEN` - Screen clearing
- `HIGHLIGHT` - Bright/bold text
- `BLINK` - Blinking text
- `REVERSE-VIDEO` - Inverted colors
- `UNDERLINE` - Underlined text
- `BELL` / `BEEP` - Terminal beep

**Supported ACCEPT options:**
- `LINE n` / `POSITION n` / `COLUMN n` - Input field positioning
- `SECURE` - Password input (hidden characters)
- Display attributes (HIGHLIGHT, REVERSE-VIDEO, etc.)

## Getting Started

### Local Development
```bash
# Clone the repository
git clone https://github.com/MeltingBot/hopper-cobol.git
cd hopper-cobol

# Start a local server (Python)
python -m http.server 8000

# Or with Node.js
npx serve .

# Open http://localhost:8000
```

### GitHub Pages

The site is deployed automatically via GitHub Pages at:
https://meltingbot.github.io/hopper-cobol/

No build step required - it's pure static HTML/CSS/JS!

## Usage

1. **Write COBOL** - Type or paste code in the editor
2. **Perforer** - Generate punch cards from code
3. **Compiler** - Compile the program
4. **Executer** - Run the program (opens terminal)
5. **Debug** - Step through execution with variable inspection

## Project Structure

```
hopper-cobol/
├── index.html              # Main application
├── css/styles.css          # Retro terminal styling
├── js/
│   ├── main.js             # App initialization
│   ├── editor.js           # Code editor & runtime
│   ├── punchCard.js        # IBM punch card encoding
│   ├── dataManagerIDB.js   # IndexedDB file storage
│   ├── tutorial.js         # Example programs
│   └── cobol/
│       ├── index.js        # CobolRuntime API
│       ├── lexer.js        # Tokenizer
│       ├── parser.js       # AST builder
│       ├── interpreter.js  # Execution engine
│       └── dialects.js     # Dialect configuration & detection
└── cobol-examples/         # Sample programs
    ├── demo-ecran.cob      # Screen control demo (IBM 3270)
    ├── gestion-clients.cob # Customer management (ERP)
    ├── facturation.cob     # Invoicing (ERP)
    ├── gestion-stock.cob   # Inventory management (ERP)
    ├── banque-comptes.cob  # Bank accounts (BANQUE)
    ├── paie-employes.cob   # Payroll (RH)
    └── ...
```

### Example Categories

| Category | Description |
|----------|-------------|
| ECRAN | Screen control demonstrations (IBM 3270) |
| ERP | Enterprise resource planning (clients, invoicing, inventory) |
| RH | Human resources (payroll) |
| BANQUE | Banking applications |
| TOURISME | Hotel reservations |
| BATCH | Batch processing (CSV import/export) |

## In Memory Of

**Grace Hopper (1906-1992)** - Pioneer of computer programming, co-inventor of COBOL, and the inspiration for this project's name.

## License

MIT License - Feel free to use, modify, and distribute.
