// task-publish Mermaid loader — initializes any `pre.mermaid`
// blocks on the page via the Mermaid CDN. Skips load entirely
// when no diagrams are present.
(function () {
  const blocks = document.querySelectorAll('pre.mermaid');
  if (!blocks.length) return;
  const dark =
    window.matchMedia &&
    window.matchMedia('(prefers-color-scheme: dark)').matches;
  // Pin the version — `latest` invalidates the CDN cache too often.
  const src = 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js';
  const s = document.createElement('script');
  s.src = src;
  s.onload = function () {
    if (!window.mermaid) return;
    window.mermaid.initialize({
      startOnLoad: false,
      theme: dark ? 'dark' : 'default',
      securityLevel: 'strict',
    });
    window.mermaid.run({ querySelector: 'pre.mermaid' });
  };
  document.head.appendChild(s);
})();
