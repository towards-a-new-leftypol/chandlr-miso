// profile.js

class Profiler {
  constructor() {
    // Stack per tag to support nesting
    this._active = new Map(); // tag → Array<startTime: number>
    // Aggregated stats per tag
    this._totals = new Map(); // tag → { count, totalMs, minMs, maxMs }
  }

  sectionStart(tag) {
    if (typeof tag !== 'string') {
      console.warn('[Profiler] sectionStart: tag must be a string');
      return;
    }
    const now = performance.now();
    if (!this._active.has(tag)) {
      this._active.set(tag, []);
    }
    this._active.get(tag).push(now);
  }

  sectionEnd(tag) {
    if (typeof tag !== 'string') {
      console.warn('[Profiler] sectionEnd: tag must be a string');
      return;
    }
    const stack = this._active.get(tag);
    if (!stack || stack.length === 0) {
      console.warn(`[Profiler] Unmatched sectionEnd for tag: "${tag}"`);
      return;
    }

    const start = stack.pop();
    const duration = performance.now() - start;

    let stats = this._totals.get(tag);
    if (!stats) {
      stats = { count: 0, totalMs: 0, minMs: Infinity, maxMs: -Infinity };
      this._totals.set(tag, stats);
    }

    stats.count++;
    stats.totalMs += duration;
    stats.minMs = Math.min(stats.minMs, duration);
    stats.maxMs = Math.max(stats.maxMs, duration);

    const avg = stats.totalMs / stats.count;
    // console.log(
    //   `[Profiler] ${tag}: ${duration.toFixed(2)}ms ` +
    //   `(avg: ${avg.toFixed(2)}ms, min: ${stats.minMs.toFixed(2)}ms, max: ${stats.maxMs.toFixed(2)}ms)`
    // );
  }

  displayTotals() {
    if (this._totals.size === 0) {
      console.log('[Profiler] No profiling data collected.');
      return;
    }

    console.table(
      Array.from(this._totals.entries()).map(([tag, stats]) => ({
        Tag: tag,
        Count: stats.count,
        'Total (ms)': stats.totalMs.toFixed(2),
        'Avg (ms)': (stats.totalMs / stats.count).toFixed(2),
        'Min (ms)': stats.minMs.toFixed(2),
        'Max (ms)': stats.maxMs.toFixed(2)
      }))
    );
  }

  reset() {
    this._active.clear();
    this._totals.clear();
    console.log('[Profiler] All profiling data cleared.');
  }
}

// Create singleton and attach to global scope
const profilerInstance = new Profiler();

// In a browser module, `globalThis` is the true global
globalThis.__profiler = profilerInstance;

// Optionally expose convenience functions globally for easy FFI
globalThis.sectionStart = (tag) => profilerInstance.sectionStart(tag);
globalThis.sectionEnd   = (tag) => profilerInstance.sectionEnd(tag);
globalThis.displayTotals = () => profilerInstance.displayTotals();

// You can also export if needed for other modules (optional)
export { Profiler, profilerInstance as profiler };
