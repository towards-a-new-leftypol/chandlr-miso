// init.js
const MODE_WASM = "wasm";
const MODE_JS = "ghcjs";
const MODE_NONE = "none";
const DEFAULT_MODE = MODE_JS; // JavaScript is default per requirements
const STORAGE_KEY = "runtimePreference";

// Get preference from localStorage or use default
const preference = localStorage.getItem(STORAGE_KEY) || DEFAULT_MODE;

// Initialize based on preference
if (preference === MODE_WASM) {
    initializeWasm();
} else if (preference === MODE_JS) {
    loadJavaScript();
} // MODE_NONE does nothing

// Create toggle UI when DOM is ready
if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", createRuntimeToggle);
} else {
    createRuntimeToggle();
}

async function initializeWasm() {
    const { WASI, OpenFile, File, ConsoleStdout } = await import("./browser_wasi_shim/index.js");
    const ghc_wasm_jsffi = (await import("./wasm.js")).default;

    const args = [];
    const env = ["GHCRTS=-H64m"];
    const fds = [
        new OpenFile(new File([])), // stdin
        ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
        ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
    ];
    const wasi = new WASI(args, env, fds, { debug: false });

    const instance_exports = {};
    const { instance } = await WebAssembly.instantiateStreaming(
        fetch("./chandlr.wasm"),
        {
            wasi_snapshot_preview1: wasi.wasiImport,
            ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
        }
    );

    Object.assign(instance_exports, instance.exports);
    wasi.initialize(instance);
    console.log("WASM exports ready.");
    await instance.exports.hs_start();
    console.log("Program started.");
}

function loadJavaScript() {
    const script = document.createElement("script");
    script.src = "./all.js";
    script.defer = true;
    document.head.appendChild(script);
}

function createRuntimeToggle() {
    // Avoid duplicate creation during hot-reloads
    if (document.getElementById("runtime-toggle-container")) return;

    // Inject styles
    const style = document.createElement("style");
    style.textContent = `
    #runtime-toggle-container {
      position: fixed;
      top: 0;
      right: 0;
      z-index: 2147483647;
    }

    #runtime-toggle-btn {
      border: 2px dashed #F0ECD6;
      background-color: rgba(0, 0, 0, 0);
      border-radius: 0px 0px 0px 4px;
      color: white;
      width: 1.8em;
      height: 1.8em;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 14px;
      cursor: pointer;
      transition: all 0.2s ease;
    }

    #runtime-toggle-btn:hover {
      background: rgba(0, 0, 0, 0);
      border-color: #D9CBB7;
      transform: scale(1.05);
    }

    #runtime-toggle-menu {
      position: absolute;
      top: 2.5em;
      right: 0;
      background: white;
      border-radius: 10px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.15);
      overflow: hidden;
      min-width: 280px;
      max-width: 95vw;
      opacity: 0;
      transform: translateY(-10px);
      transition: opacity 0.2s ease, transform 0.2s ease;
      pointer-events: none;
      padding: 12px;
    }

    #runtime-toggle-menu.active {
      opacity: 1;
      transform: translateY(0);
      pointer-events: all;
    }

    .runtime-option {
      padding: 10px 12px;
      border-radius: 8px;
      margin-bottom: 8px;
      cursor: pointer;
      transition: background 0.2s;
      border: 1px solid #eee;
    }

    .runtime-option:hover {
      background: #f8f9fa;
    }

    .runtime-option input {
      margin-right: 8px;
    }

    .runtime-option strong {
      display: block;
      font-size: 14px;
      color: #1a1a1a;
    }

    .runtime-option .desc {
      font-size: 12px;
      color: #666;
      margin-top: 2px;
      line-height: 1.4;
    }

    #apply-runtime {
      background: #0d6efd;
      color: white;
      border: none;
      border-radius: 6px;
      padding: 8px 16px;
      font-size: 14px;
      font-weight: 500;
      width: 100%;
      margin-top: 8px;
      cursor: pointer;
      transition: background 0.2s;
    }

    #apply-runtime:hover {
      background: #0a58ca;
    }
  `;
    document.head.appendChild(style);

    // Create container
    const container = document.createElement("div");
    container.id = "runtime-toggle-container";
    document.body.appendChild(container);

    // Toggle button
    const btn = document.createElement("button");
    btn.id = "runtime-toggle-btn";
    container.appendChild(btn);

    // Menu container
    const menu = document.createElement("div");
    menu.id = "runtime-toggle-menu";
    container.appendChild(menu);

    // === Instruction text ===
    const instruction = document.createElement("h4");
    instruction.id = "runtime-toggle-instruction";
    instruction.textContent = "Change application runtime";
    menu.appendChild(instruction);

    // Menu content
    const options = [
        {
            mode: MODE_NONE,
            title: "None",
            desc: "Don't load single page application code"
        },
        {
            mode: MODE_JS,
            title: "JavaScript",
            desc: "Haskell code compiled to JavaScript using GHC 9.12 (default)"
        },
        {
            mode: MODE_WASM,
            title: "WASM",
            desc: "Experimental WASM backend (currently less performant)"
        }
    ];

    // Current stored preference
    const currentMode = localStorage.getItem(STORAGE_KEY) || DEFAULT_MODE;

    // Create option elements
    options.forEach(opt => {
        const div = document.createElement("div");
        div.className = "runtime-option";

        div.innerHTML = `
      <input type="radio" name="runtime-mode" id="mode-${opt.mode}" value="${opt.mode}"
        ${opt.mode === currentMode ? 'checked' : ''}>
      <label for="mode-${opt.mode}">
        <strong>${opt.title}</strong>
        <div class="desc">${opt.desc}</div>
      </label>
    `;
        menu.appendChild(div);
    });

    // Apply button
    const applyBtn = document.createElement("button");
    applyBtn.id = "apply-runtime";
    applyBtn.textContent = "Apply Selection";
    menu.appendChild(applyBtn);

    // Toggle menu visibility
    btn.addEventListener("click", (e) => {
        e.stopPropagation();
        menu.classList.toggle("active");
    });

    // Handle apply click
    applyBtn.addEventListener("click", () => {
        const selected = menu.querySelector('input[name="runtime-mode"]:checked').value;
        const currentStored = localStorage.getItem(STORAGE_KEY) || DEFAULT_MODE;

        if (selected !== currentStored) {
            localStorage.setItem(STORAGE_KEY, selected);
            location.reload();
        }

        // Always close menu after apply
        menu.classList.remove("active");
    });

    // Close menu on outside click
    document.addEventListener("click", (e) => {
        if (!container.contains(e.target)) {
            menu.classList.remove("active");
        }
    });

    // Prevent menu close when clicking inside
    menu.addEventListener("click", (e) => e.stopPropagation());
}
