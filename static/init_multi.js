// init.js
const MODE_WASM = "wasm";
const MODE_JS = "ghcjs";
const MODE_NONE = "none";
const DEFAULT_MODE = MODE_WASM;
const STORAGE_KEY = "runtimePreference";
// const STATIC_MOUNT = "./";
const STATIC_MOUNT = "/static/";

if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initApp);
} else {
    initApp();
}

async function initApp() {
    const preference = localStorage.getItem(STORAGE_KEY) || DEFAULT_MODE;
    if (preference === MODE_WASM) {
        await initializeWasm();
    } else if (preference === MODE_JS) {
        loadJavaScript();
    }
}

async function initializeWasm() {
    // Parallelize imports while downloading WASM
    const [wasiModule, ffiModule, wasmBytes] = await Promise.all([
        import("./browser_wasi_shim/index.js"),
        import("./wasm.js"),
        loadWasm()
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
}

function loadWasm() {
    return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();
        xhr.open('GET', STATIC_MOUNT + 'chandlr.wasm', true);
        xhr.responseType = 'arraybuffer';

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

function loadJavaScript() {
    const script = document.createElement("script");
    script.src = STATIC_MOUNT + "all.js";
    script.defer = true;
    document.head.appendChild(script);
}

function modeWASM() {
    localStorage.setItem(STORAGE_KEY, MODE_WASM);
    return true;
}

function modeJS() {
    localStorage.setItem(STORAGE_KEY, MODE_JS);
    return true;
}

function modeNone() {
    localStorage.setItem(STORAGE_KEY, MODE_NONE);
    return true;
}

globalThis.modeWASM = modeWASM;
globalThis.modeJS = modeJS;
globalThis.modeNone = modeNone;
