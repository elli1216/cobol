# GnuCOBOL Setup & Execution Guide

This guide provides the necessary steps to install, configure, and run **COBOL** programs on a Windows environment using the GnuCOBOL compiler.

## 1. Installation

You can download the GnuCOBOL compiler from either of these sources:

* **Official SourceForge (Standard):** [Download GnuCOBOL](https://sourceforge.net/projects/gnucobol/)
* **Arnold Trembley’s Build (Recommended for Windows):** [Download .7z Installer](https://www.arnoldtrembley.com/GC32-BDB-SP1-rename-7z-to-exe.7z)
* *Note: If using the Arnold Trembley link, you **must** rename the file extension from `.7z` to `.exe` before running the installer.*



---

## 2. System Environment Variables

To ensure the compiler can find its configuration files and binary tools, set the following variables in your **System Environment Variables**.

### Primary Variables

| Variable Name | Value | Purpose |
| --- | --- | --- |
| **`COB_MAIN_DIR`** | `C:\gnucobol` | Defines the root installation folder. |
| **`COB_CONFIG_DIR`** | `C:\gnucobol\config` | Points to the compiler configuration files. |
| **`COB_COPY_DIR`** | `C:\gnucobol\copy` | Specifies where to look for Copybooks. |

### System Path

Add the following entry to your existing **Path** variable to allow the `cobc` command to work in any terminal:

> `C:\gnucobol\bin`

---

## 3. Verification

Open **PowerShell** or **Command Prompt** and run the following commands to verify the installation:

```powershell
# Check the version of the compiler
cobc --version

# Check detailed configuration info (helps debug path issues)
cobc --info

```

---

## 4. Compiling & Running Programs

Follow these commands to turn your source code into an executable application.

### Step A: Compile

Use the `-x` flag to create a standalone executable:

```powershell
cobc -x <file-name>.cob

```

### Step B: Run

Execute the generated program directly:

```powershell
./<file-name>.exe

```

---

## Troubleshooting Pro-Tip

If you encounter a `configuration error` stating `default.conf` is missing, ensure your **COB_CONFIG_DIR** is set correctly or run the environment script provided in your installation folder:

> `C:\gnucobol\set_env.cmd`

## Must-have extensions for VSCode/Cursor
<img src='./images/Extensions.png' alt="VS Code Extensions List" width="600" />

---