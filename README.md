# HOPPER - COBOL Emulator

A web-based COBOL emulator with IBM 029 punch card visualization, running entirely in the browser.

**[Live Demo](https://meltingbot.github.io/hopper-cobol/)**

![HOPPER Screenshot](screenshot.png)

## Features

- **Full COBOL-85 Interpreter** - Runs COBOL programs directly in JavaScript
- **Punch Card Visualization** - Realistic IBM 029 keypunch simulation with animations
- **Debug Mode** - Step-by-step execution with variable inspection
- **File I/O** - Simulated SEQUENTIAL and INDEXED file operations via IndexedDB
- **10 Example Programs** - Banking, invoicing, payroll, inventory management, etc.
- **Retro Terminal UI** - Authentic phosphor green aesthetic

## Supported COBOL Features

### Data Division
- Level numbers (01-49, 66, 77, 88)
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
- File operations: OPEN, READ, WRITE, REWRITE, DELETE, CLOSE

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
│       └── interpreter.js  # Execution engine
└── cobol-examples/         # Sample programs
```

## In Memory Of

**Grace Hopper (1906-1992)** - Pioneer of computer programming, co-inventor of COBOL, and the inspiration for this project's name.

## License

MIT License - Feel free to use, modify, and distribute.
