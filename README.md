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

| Feature | Syntax | Status |
|---------|--------|--------|
| Level numbers | `01` to `49`, `66`, `77`, `88` | ✅ |
| PICTURE clause | `PIC X(10)`, `PIC 9(5)V99`, `PIC S9(4)` | ✅ |
| VALUE clause | `VALUE "text"`, `VALUE 123`, `VALUE ZEROS` | ✅ |
| REDEFINES | `05 VAR-B REDEFINES VAR-A` | ✅ |
| OCCURS | `OCCURS 10 TIMES`, `OCCURS 1 TO 100 DEPENDING ON` | ✅ |
| INDEXED BY | `OCCURS 10 INDEXED BY IDX-1` | ✅ |
| 88-level conditions | `88 IS-VALID VALUE "Y" "O".` | ✅ |
| VALUE THRU | `88 IN-RANGE VALUE 1 THRU 100.` | ✅ |
| WORKING-STORAGE | `WORKING-STORAGE SECTION.` | ✅ |
| FILE SECTION | `FD`, `SELECT`, `ASSIGN` | ✅ |
| USAGE | `COMP`, `BINARY`, `PACKED-DECIMAL` | ⚠️ Parsed |

### Procedure Division - Statements

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| DISPLAY | `DISPLAY "Hello" WS-VAR` | ✅ |
| ACCEPT | `ACCEPT WS-INPUT` | ✅ |
| MOVE | `MOVE X TO Y Z` | ✅ |
| INITIALIZE | `INITIALIZE WS-RECORD` | ✅ |
| ADD | `ADD A B TO C GIVING D` | ✅ |
| SUBTRACT | `SUBTRACT A FROM B GIVING C` | ✅ |
| MULTIPLY | `MULTIPLY A BY B GIVING C` | ✅ |
| DIVIDE | `DIVIDE A INTO B GIVING C REMAINDER R` | ✅ |
| COMPUTE | `COMPUTE X = (A + B) * C / D ** 2` | ✅ |
| IF/ELSE | `IF cond THEN ... ELSE ... END-IF` | ✅ |
| EVALUATE | `EVALUATE var WHEN val ... END-EVALUATE` | ✅ |
| PERFORM | `PERFORM para`, `PERFORM n TIMES`, `PERFORM UNTIL`, `PERFORM VARYING` | ✅ |
| GO TO | `GO TO paragraph-name` | ✅ |
| EXIT | `EXIT`, `EXIT PARAGRAPH` | ✅ |
| STOP RUN | `STOP RUN` | ✅ |
| CONTINUE | `CONTINUE` | ✅ |
| SET | `SET idx TO 5`, `SET idx UP BY 1`, `SET cond TO TRUE` | ✅ |

### String Manipulation (COBOL-74/85)

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| STRING | `STRING a DELIMITED BY " " b DELIMITED BY SIZE INTO c` | ✅ |
| UNSTRING | `UNSTRING src DELIMITED BY ";" INTO a b c TALLYING cnt` | ✅ |
| INSPECT TALLYING | `INSPECT var TALLYING cnt FOR ALL "X"` | ✅ |
| INSPECT REPLACING | `INSPECT var REPLACING ALL "X" BY "Y"` | ✅ |
| INSPECT CONVERTING | `INSPECT var CONVERTING "abc" TO "ABC"` | ✅ |
| Reference Modification | `MOVE WS-VAR(1:5) TO WS-SUB` | ✅ |
| Subscripting | `MOVE TABLE-ITEM(I) TO WS-VAR` | ✅ |

### File Operations

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| OPEN | `OPEN INPUT file`, `OPEN OUTPUT file`, `OPEN I-O file` | ✅ |
| CLOSE | `CLOSE file-1 file-2` | ✅ |
| READ | `READ file AT END ... NOT AT END ... END-READ` | ✅ |
| READ KEY | `READ file KEY IS key-field INVALID KEY ...` | ✅ |
| WRITE | `WRITE record-name` | ✅ |
| REWRITE | `REWRITE record-name` | ✅ |
| DELETE | `DELETE file-name INVALID KEY ...` | ✅ |
| START | `START file KEY >= key-field` | ✅ |
| SORT | `SORT file ON ASCENDING KEY ... USING ... GIVING ...` | ✅ |
| MERGE | `MERGE file ON ASCENDING KEY ... USING ... GIVING ...` | ✅ |
| RELEASE | `RELEASE sort-record` | ✅ |
| RETURN | `RETURN sort-file AT END ...` | ✅ |

### Table Handling

| Feature | Syntax Example | Status |
|---------|----------------|--------|
| SEARCH | `SEARCH table AT END ... WHEN cond ... END-SEARCH` | ✅ |
| SEARCH ALL | `SEARCH ALL table WHEN key = value ...` | ✅ |
| OCCURS | `OCCURS 100 TIMES` | ✅ |
| OCCURS DEPENDING ON | `OCCURS 1 TO 100 DEPENDING ON WS-SIZE` | ✅ |
| INDEXED BY | `INDEXED BY IDX-1` | ✅ |
| ASCENDING KEY | `ASCENDING KEY IS field-name` | ✅ |

### File Organizations

| Type | Status | Notes |
|------|--------|-------|
| SEQUENTIAL | ✅ | Line sequential access |
| INDEXED | ✅ | Key-based access via IndexedDB |
| RELATIVE | ⚠️ | Parsed, limited support |

### Figurative Constants

| Constant | Status |
|----------|--------|
| SPACES / SPACE | ✅ |
| ZEROS / ZEROES / ZERO | ✅ |
| LOW-VALUES | ✅ |
| HIGH-VALUES | ✅ |
| QUOTES | ✅ |

### Legend

| Symbol | Meaning |
|--------|---------|
| ✅ | Fully implemented |
| ⚠️ | Partially implemented or parsed only |
| ❌ | Not implemented |

### Not Yet Implemented

| Feature | Standard | Notes |
|---------|----------|-------|
| CALL / CANCEL | COBOL-85 | Subprogram calls |
| COPY | COBOL-74 | Copybook inclusion |
| COPY REPLACING | COBOL-85 | Copybook with substitution |
| LINAGE | COBOL-74 | Print page control |
| WRITE ADVANCING | COBOL-68 | Printer line control |
| DECLARATIVES | COBOL-85 | Error handling procedures |
| ALTER | COBOL-68 | Dynamic GO TO modification (obsolete) |

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
