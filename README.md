# 🔄 Rings Converter

**Rings Converter** is a cross-platform project converter built with 
[Free Pascal](https://www.freepascal.org/) and [Lazarus](https://www.lazarus-ide.org/).  
It aims to simplify the process of converting Pascal projects into modern formats, including `.lpr`, `.rproj`, and `.xcodeproj`.


---

## ✨ Features

- 📄 Save project files as `.rproj` (custom XML-based format)
- 🧱 Generate `.lpr` for Free Pascal builds
- 🍏 Export `.xcodeproj` files for Xcode support (macOS, iOS)
- 🛠 Supports multiple opened source files
- 🔍 Clean and simple Lazarus-based GUI
- 🔗 About dialog with project info and GitHub link

---

## 🚀 Getting Started

### 📦 Requirements

- [Free Pascal](https://www.freepascal.org/)
- [Lazarus IDE](https://www.lazarus-ide.org/) (tested with version 2.2+)
- macOS, Linux (ARM64), Windows (x64/ARM64)

### 🔧 Build Instructions

1. Clone this repo:
   ```bash
   git clone https://github.com/ringsce/ringsConverter.git
   cd ringsConverter
   ```

2. Open `converter.lpi` in Lazarus.

3. Hit **Run ▶️** or press **F9** to build and launch the app.

---

## 🧩 Project Structure

```
/units/
  RProject.pas       // Project save/load system (.rproj, .lpr, .xcodeproj)
  PastoSwift.pas     // Experimental Pascal-to-Swift 6.1 converter
  CToPas.pas         // C to Pascal converter
  AboutForm.pas      // Custom About dialog
converter.lpi        // Lazarus project file
README.md            // You're here!
```

---

## 💬 About

Built with ❤️ by the [RingsCE](https://github.com/ringsce) team.  
This project is part of the **TildeDesktop** toolset.

📂 Output project files are stored in:  
`~/Documents/MyProj/MyProject.rproj` (customizable in code)

---

## 📄 License

This project is licensed under the **GPL v3**.  
See [LICENSE](LICENSE) for details.

---

## 🔗 Links

- 🛠 GitHub: [github.com/ringsce/ringsConverter](https://github.com/ringsce/ringsConverter)
- 🧠 Website: [ringscejs.gleentech.com](https://ringscejs.gleentech.com)

---

