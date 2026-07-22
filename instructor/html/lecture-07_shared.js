/* ============================================================
   The data model shared by table and wheel.
   Both pages read/write the SAME localStorage key, which is how they
   stay in sync: edit counts on the table page, then open the wheel
   page (or come back to it) and it already sees them.
   ============================================================ */

const BIGRAM_STORAGE_KEY = 'bigram-corpus-v1';
const BIGRAM_END_TOKEN = '<end>';

const BIGRAM_DEFAULT_DATA = {
  vocab: ['the', 'dog', 'food', 'eating', 'loves', 'is'],
  counts: { 'the|dog': 6, 'the|food': 2 }
};

function bigramClone(obj) {
  return JSON.parse(JSON.stringify(obj));
}

function bigramLoad() {
  try {
    const raw = localStorage.getItem(BIGRAM_STORAGE_KEY);
    if (!raw) return bigramClone(BIGRAM_DEFAULT_DATA);
    const parsed = JSON.parse(raw);
    if (!parsed || !Array.isArray(parsed.vocab) || typeof parsed.counts !== 'object') {
      return bigramClone(BIGRAM_DEFAULT_DATA);
    }
    return parsed;
  } catch (err) {
    console.warn('bigram: could not read saved data, using the example instead', err);
    return bigramClone(BIGRAM_DEFAULT_DATA);
  }
}

function bigramSave(data) {
  localStorage.setItem(BIGRAM_STORAGE_KEY, JSON.stringify(data));
  // same-tab listeners (the wheel page, if open) can react immediately
  window.dispatchEvent(new CustomEvent('bigram-updated', { detail: data }));
}

function bigramKey(row, col) {
  return row + '|' + col;
}

function bigramGetCount(data, row, col) {
  return data.counts[bigramKey(row, col)] || 0;
}

function bigramSetCount(data, row, col, value) {
  const v = Math.max(0, Math.floor(Number(value) || 0));
  const k = bigramKey(row, col);
  if (v === 0) delete data.counts[k];
  else data.counts[k] = v;
}

function bigramColumns(data) {
  return [...data.vocab, BIGRAM_END_TOKEN];
}

function bigramRowTotal(data, row) {
  return bigramColumns(data).reduce((sum, col) => sum + bigramGetCount(data, row, col), 0);
}

const BIGRAM_PALETTE = ['var(--bigram-cat-0)', 'var(--bigram-cat-1)', 'var(--bigram-cat-2)', 'var(--bigram-cat-3)', 'var(--bigram-cat-4)', 'var(--bigram-cat-5)'];
function bigramColorFor(index) {
  return BIGRAM_PALETTE[index % BIGRAM_PALETTE.length];
}

function bigramEscapeHtml(s) {
  return String(s).replace(/[&<>"']/g, (m) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[m]));
}

// Point on a circle at `angleDeg` measured clockwise from the top (12 o'clock) — used by the wheel.
function bigramPolarPoint(cx, cy, r, angleDeg) {
  const rad = (angleDeg * Math.PI) / 180;
  return { x: cx + r * Math.sin(rad), y: cy - r * Math.cos(rad) };
}
