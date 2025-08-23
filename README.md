# FreeFem++ Mode for Emacs

This is an Emacs parser for FreeFEM++ for use with recent Linux versions such as Ubuntu 22LTS and 24LTS, etc. This is not duplicated work, but created to support recent Emacs versions on recent releases of Linux. The existing Emacs parser on www.freefem.org and its official GitHub repo (https://github.com/rrgalvan/freefem-mode) were found to have issues. This version of freefem++-mode.el will help users on the latest Linux releases.

`freefem++-mode.el` is an advanced Integrated Development Environment (IDE) for the FreeFem++ programming language, specifically designed for solving partial differential equations (PDEs) using finite element methods. This comprehensive Emacs mode provides a sophisticated set of tools to enhance productivity, code quality, and scientific computing efficiency.

## Table of Contents
- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Keybindings](#keybindings)
- [Customization](#customization)
- [Advanced Features](#advanced-features)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)
- [License](#license)

## Features

### 1. Syntax Highlighting
- **Context-Aware Highlighting**
  - Specialized syntax coloring for FreeFem++ constructs
  - Distinct highlighting for:
    - Language keywords
    - Finite element types
    - Mathematical operators
    - Boundary conditions
    - Macro definitions
  - Advanced font lock with multiple levels of highlighting

### 2. Intelligent Code Completion
- **Company Mode Integration**
  - Context-sensitive symbol completion
  - Intelligent suggestions for:
    - Functions
    - Types
    - Mathematical functions
    - Finite element spaces
- **Function Signature Documentation**
  - Inline documentation for functions
  - Eldoc support for quick reference

### 3. Error Checking and Debugging
- **Flycheck Integration**
  - Real-time syntax checking
  - Compilation error detection
  - Error navigation support
- **Debugging Utilities**
  - Debug mode toggle
  - Debug print statement insertion
  - Performance timing options

### 4. Project Management
- **Project Templates**
  - Quick project initialization
  - Predefined templates for common PDE problems:
    - Poisson Equation
    - Linear Elasticity
    - Navier-Stokes Flow
    - Heat Transfer
    - Wave Equations
    - Eigenvalue Problems
- **Project Structure Generation**
  - Automatic creation of project directories
  - Sample Makefile generation
  - Main script template

### 5. Visualization and Analysis
- **Mesh Visualization**
  - Integration with Medit
  - ParaView support
  - Mesh file viewing
- **Graphics Management**
  - Toggle graphics output
  - Compile with/without graphics
  - Safe mode for avoiding rendering issues

### 6. Code Refactoring
- **Advanced Refactoring Tools**
  - Variable renaming across buffer
  - Function extraction
  - Macro insertion
  - Mathematical symbol input methods

### 7. Navigation and Documentation
- **Enhanced Navigation**
  - Imenu support
  - Function and event finding
  - Error navigation
- **Documentation Access**
  - Quick access to FreeFem++ documentation
  - Inline function descriptions
  - Mathematical symbol input

## Requirements

### Software Dependencies
- **Emacs**: Version 28.1 or higher
- **FreeFem++**: Latest stable version
- **Optional Dependencies**:
  - `company-mode`: For intelligent code completion
  - `flycheck`: For real-time syntax checking
  - `eldoc`: For inline documentation
  - `medit`: For mesh visualization
  - `paraview`: For advanced visualization

### System Requirements
- Unix-like operating system (Linux, macOS)
- Recommended: 8GB RAM
- Disk Space: 500MB+ for development environment

## Installation

### 1. Manual Installation
1. Download `freefem++-mode.el`
```sh
mkdir -p ~/.emacs.d/lisp/
wget https://github.com/AdityasOcean/FreeFEM-Emacs-Parser/blob/main/freefem%2B%2B-mode.el
```

2. Configure Emacs
Add to `~/.emacs` or `~/.emacs.d/init.el`:
```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'freefem++-mode)
```

### 2. Package Manager (Recommended)
- Coming soon: MELPA package support

## Usage

### Basic Workflow
1. Open a `.edp` or `.idp` file
2. Use templates to quickly start your PDE project
3. Write and debug your finite element code
4. Compile and visualize results

### Compilation Methods
- Standard compilation
- Safe mode compilation
- Graphics-enabled compilation
- MPI/Parallel computation support

## Keybindings

### Compilation and Execution
| Keybinding    | Function                      | Description                           |
|---------------|-------------------------------|---------------------------------------|
| `C-c C-c`     | `freefem++-compile`           | Compile current buffer                |
| `C-c C-r`     | `freefem++-run-region`        | Run selected region                   |
| `C-c C-b`     | `freefem++-run-buffer`        | Run entire buffer                     |
| `C-c C-G`     | `freefem++-compile-with-graphics` | Compile with graphics enabled     |

### Project and Templates
| Keybinding    | Function                      | Description                           |
|---------------|-------------------------------|---------------------------------------|
| `C-c C-t`     | `freefem++-insert-template`   | Insert problem template               |
| `C-c C-m`     | `freefem++-insert-macro`      | Insert macro template                 |
| `C-c C-n`     | `freefem++-new-project`       | Create new project                    |

### Debugging and Development
| Keybinding    | Function                      | Description                           |
|---------------|-------------------------------|---------------------------------------|
| `C-c C-d`     | `freefem++-toggle-debug`      | Toggle debug mode                     |
| `C-c C-p`     | `freefem++-insert-debug-print`| Insert debug print                    |
| `C-c C-e`     | `freefem++-extract-function`  | Extract selected code to function     |
| `C-c C-R`     | `freefem++-rename-variable`   | Rename variable across buffer         |

### Visualization
| Keybinding    | Function                      | Description                           |
|---------------|-------------------------------|---------------------------------------|
| `C-c C-v m`   | `freefem++-view-mesh`         | View mesh with Medit                  |
| `C-c C-v p`   | `freefem++-view-paraview`     | Open in ParaView                      |

### System Toggles
| Keybinding    | Function                      | Description                           |
|---------------|-------------------------------|---------------------------------------|
| `C-c C-S`     | `freefem++-toggle-safe-mode`  | Toggle safe mode                      |
| `C-c C-g`     | `freefem++-toggle-graphics`   | Toggle graphics output                |

## Customization

### Customizable Variables
- `freefem++-program`: FreeFem++ compiler path
- `freefem++-compile-args`: Compilation arguments
- `freefem++-safe-mode-args`: Safe mode compilation args
- `freefem++-enable-graphics`: Graphics output toggle
- `freefem++-basic-offset`: Indentation offset
- `freefem++-enable-company`: Code completion
- `freefem++-enable-flycheck`: Syntax checking
- `freefem++-medit-program`: Medit viewer path
- `freefem++-paraview-program`: ParaView path

### Configuration Example
```emacs-lisp
(setq freefem++-program "/custom/path/to/FreeFem++")
(setq freefem++-basic-offset 4)
(setq freefem++-enable-graphics t)
```

## Advanced Features

### Mathematical Symbol Input
- Easy insertion of Greek letters
- Calculus and mathematical notation support
- Custom symbol mapping

### Performance Monitoring
- Execution time tracking
- Resource usage insights

## Troubleshooting

### Common Issues
- **Compilation Errors**
  - Check console output
  - Navigate errors with `M-g M-n`, `M-g M-p`
  - Use safe mode if experiencing segmentation faults

- **Visualization Problems**
  - Verify Medit and ParaView installations
  - Check configuration paths
  - Ensure graphics are enabled

### Debugging Tips
- Use `C-c C-d` to toggle debug mode
- Insert debug prints with `C-c C-p`
- Examine compilation buffer for detailed error information

## Contributing

### How to Contribute
1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Submit a pull request

### Reporting Issues
- Open GitHub issues
- Provide detailed error descriptions
- Include Emacs and FreeFem++ versions

## License

MIT License

Copyright (c) 2025 FreeFem++ Community

Permission is granted to use, modify, and distribute under the terms of the MIT License.

## Author and Maintainer

- **Author**: Arun K Eswara (eakishore@gmail.com)
- **Maintainer**: FreeFem++ Community
- **Version**: 2.0
- **Keywords**: FreeFem++, PDE, Finite Elements, Emacs IDE

---

Enhance your scientific computing and finite element programming with the FreeFem++ Emacs mode!

## Additional Resources
- [Official FreeFem++ Documentation](https://doc.freefem.org/)
- [FreeFem++ Community](https://freefem.org)
