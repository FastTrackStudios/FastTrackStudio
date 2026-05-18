// KaTeX loader — pulls KaTeX from a CDN and runs auto-render
// over the document. Math syntax: `$inline$` and `$$display$$`.
//
// Falls back gracefully when offline + uncached: math notation
// renders as raw `$…$` instead of typeset, but the page still
// works.
//
// We use the CDN here instead of bundling (~300KB) so the
// publisher binary stays small. Self-hosted users can replace
// the URLs with paths to local copies.

(function () {
  const KATEX_VERSION = '0.16.11';
  const BASE = `https://cdn.jsdelivr.net/npm/katex@${KATEX_VERSION}/dist`;

  function injectStylesheet() {
    const link = document.createElement('link');
    link.rel = 'stylesheet';
    link.href = `${BASE}/katex.min.css`;
    link.crossOrigin = 'anonymous';
    document.head.appendChild(link);
  }

  function loadScript(src) {
    return new Promise((resolve, reject) => {
      const s = document.createElement('script');
      s.src = src;
      s.crossOrigin = 'anonymous';
      s.defer = true;
      s.onload = resolve;
      s.onerror = reject;
      document.head.appendChild(s);
    });
  }

  // Only run when there's actually math on the page — avoid the
  // network round-trip for vaults that don't use math notation.
  function looksLikeMath() {
    const text = document.body.textContent || '';
    return text.includes('$') &&
      (/\$[^\s$].*?\$/.test(text) || text.includes('$$'));
  }

  if (!looksLikeMath()) return;

  injectStylesheet();
  loadScript(`${BASE}/katex.min.js`)
    .then(() => loadScript(`${BASE}/contrib/auto-render.min.js`))
    .then(() => {
      if (window.renderMathInElement) {
        window.renderMathInElement(document.body, {
          delimiters: [
            { left: '$$', right: '$$', display: true },
            { left: '$', right: '$', display: false },
            { left: '\\(', right: '\\)', display: false },
            { left: '\\[', right: '\\]', display: true },
          ],
          throwOnError: false,
        });
      }
    })
    .catch((e) => console.warn('katex load failed:', e));
})();
