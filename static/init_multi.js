// init.js
const MODE_WASM = "wasm";
const MODE_JS = "ghcjs";
const MODE_NONE = "none";
const DEFAULT_MODE = MODE_JS;
const STORAGE_KEY = "runtimePreference";

if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initApp);
} else {
    initApp();
}

async function initApp() {
    createWasmProgressBar(); // Create progress bar FIRST
    createRuntimeToggle();   // Create toggle UI
    
    const preference = localStorage.getItem(STORAGE_KEY) || DEFAULT_MODE;
    if (preference === MODE_WASM) {
        await initializeWasm();
    } else if (preference === MODE_JS) {
        loadJavaScript();
    }
}

async function initializeWasm() {
    const progressBar = document.getElementById("wasm-progress-bar");
    progressBar.style.display = "block";
    progressBar.style.width = "0%";

    try {
        // Parallelize imports while downloading WASM
        const [wasiModule, ffiModule, wasmBytes] = await Promise.all([
            import("./browser_wasi_shim/index.js"),
            import("./wasm.js"),
            loadWasmWithProgress(progressBar)
        ]);

        const { WASI, OpenFile, File, ConsoleStdout } = wasiModule;
        const ghc_wasm_jsffi = ffiModule.default;

        const args = [];
        const env = ["GHCRTS=-H64m"];
        const fds = [
            new OpenFile(new File([])),
            ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
            ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
        ];
        const wasi = new WASI(args, env, fds, { debug: false });

        const instance_exports = {};
        const { instance } = await WebAssembly.instantiate(wasmBytes, {
            wasi_snapshot_preview1: wasi.wasiImport,
            ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
        });

        Object.assign(instance_exports, instance.exports);
        wasi.initialize(instance);
        console.log("WASM exports ready.");
        await instance.exports.hs_start();
        console.log("Program started.");
    } finally {
        // Hide progress bar after short delay for visual polish
        setTimeout(() => {
            progressBar.style.display = "none";
        }, 300);
    }
}

function loadJavaScript() {
    const script = document.createElement("script");
    script.src = "/static/all.js";
    script.defer = true;
    document.head.appendChild(script);
}

// REAL PROGRESS BAR IMPLEMENTATION
function createWasmProgressBar() {
    if (document.getElementById("wasm-progress-bar")) return;
    
    const style = document.createElement("style");
    style.textContent = `
        #wasm-progress-bar {
            position: fixed;
            top: 0;
            left: 0;
            height: 4px;
            background-color: #F0ECD6;
            width: 0%;
            z-index: 2147483647;
            transition: width 0.05s linear; /* Responsive but not sluggish */
            display: none;
        }
    `;
    document.head.appendChild(style);
    
    const bar = document.createElement("div");
    bar.id = "wasm-progress-bar";
    document.body.prepend(bar); // Ensure it's the topmost element
}

// ACTUAL PROGRESS TRACKING VIA XHR
function loadWasmWithProgress(progressBar) {
    return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();
        xhr.open('GET', '/static/chandlr.wasm', true);
        xhr.responseType = 'arraybuffer';

        xhr.onprogress = function(event) {
            if (event.lengthComputable) {
                const percent = (event.loaded / event.total) * 100;
                progressBar.style.width = `${percent}%`;
            }
        };

        xhr.onload = function() {
            if (xhr.status === 200) {
                resolve(xhr.response);
            } else {
                reject(new Error(`WASM load failed: HTTP ${xhr.status}`));
            }
        };

        xhr.onerror = function() {
            reject(new Error('Network error while loading WASM'));
        };

        xhr.send();
    });
}

function createRuntimeToggle() {
    if (document.getElementById("runtime-toggle-container")) return;

    const style = document.createElement("style");
    style.textContent = `
    #runtime-toggle-container {
      position: absolute;
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

    const container = document.createElement("div");
    container.id = "runtime-toggle-container";
    document.body.appendChild(container);

    const btn = document.createElement("button");
    btn.id = "runtime-toggle-btn";
    container.appendChild(btn);

    const menu = document.createElement("div");
    menu.id = "runtime-toggle-menu";
    container.appendChild(menu);

    const instruction = document.createElement("div");
    instruction.id = "runtime-toggle-instruction";
    instruction.textContent = "Select application runtime:";
    menu.appendChild(instruction);

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

    const currentMode = localStorage.getItem(STORAGE_KEY) || DEFAULT_MODE;

    options.forEach(opt => {
        const div = document.createElement("div");
        div.className = "runtime-option";
        
        const id = `mode-${opt.mode}`;
        div.innerHTML = `
            <input type="radio" name="runtime-mode" id="${id}" value="${opt.mode}" ${opt.mode === currentMode ? 'checked' : ''}>
            <label for="${id}">
                <strong>${opt.title}</strong>
                <div class="desc">${opt.desc}</div>
            </label>
        `;
        menu.appendChild(div);
    });

    const applyBtn = document.createElement("button");
    applyBtn.id = "apply-runtime";
    applyBtn.textContent = "Apply Selection";
    menu.appendChild(applyBtn);

    btn.addEventListener("click", (e) => {
        e.stopPropagation();
        menu.classList.toggle("active");
    });

    applyBtn.addEventListener("click", () => {
        const selected = menu.querySelector('input[name="runtime-mode"]:checked').value;
        const currentStored = localStorage.getItem(STORAGE_KEY) || DEFAULT_MODE;
        
        if (selected !== currentStored) {
            localStorage.setItem(STORAGE_KEY, selected);
            location.reload();
        }
        menu.classList.remove("active");
    });

    document.addEventListener("click", (e) => {
        if (!container.contains(e.target)) {
            menu.classList.remove("active");
        }
    });

    menu.addEventListener("click", (e) => e.stopPropagation());
}
