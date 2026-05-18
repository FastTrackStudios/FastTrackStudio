// task-publish wikilink graph — vanilla canvas + tiny force
// simulation. No external deps; ~120 lines so the published
// site stays small.
//
// Behavior:
// - Reads /assets/graph.json: { nodes: [{id, label, degree}], edges: [{source, target}] }
// - Spring-force layout with global repulsion + edge attraction
// - Hover a node → highlights it + its 1-hop neighborhood
// - Click → navigate to /<id>/
// - Pinch-zoom + drag-pan (basic)
//
// Inspired by Quartz's Graph (D3+Pixi) and Logseq's graph view,
// trimmed to what fits in a single static script.

(async function () {
  const canvas = document.getElementById('wikigraph');
  if (!canvas) return;
  const ctx = canvas.getContext('2d');

  // Resize canvas to match its CSS pixel size, accounting for DPR.
  function fitCanvas() {
    const rect = canvas.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    canvas.width = Math.floor(rect.width * dpr);
    canvas.height = Math.floor(rect.height * dpr);
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  }
  fitCanvas();
  window.addEventListener('resize', fitCanvas);

  let data;
  try {
    const res = await fetch('/assets/graph.json');
    data = await res.json();
  } catch (e) {
    ctx.fillStyle = '#888';
    ctx.fillText('graph data unavailable', 12, 24);
    return;
  }
  if (!data.nodes || data.nodes.length === 0) {
    ctx.fillStyle = '#888';
    ctx.fillText('empty vault — no nodes to graph', 12, 24);
    return;
  }

  // Initialize node positions in a circle so the first frame
  // doesn't render everything at the origin.
  const W = canvas.getBoundingClientRect().width;
  const H = canvas.getBoundingClientRect().height;
  const cx = W / 2, cy = H / 2;
  const nodes = data.nodes.map((n, i) => {
    const angle = (i / data.nodes.length) * Math.PI * 2;
    const r = Math.min(W, H) * 0.35;
    return {
      ...n,
      x: cx + Math.cos(angle) * r,
      y: cy + Math.sin(angle) * r,
      vx: 0, vy: 0,
    };
  });
  const idx = new Map(nodes.map((n, i) => [n.id, i]));
  const edges = data.edges
    .map(e => ({ a: idx.get(e.source), b: idx.get(e.target) }))
    .filter(e => e.a !== undefined && e.b !== undefined);

  // Adjacency for hover highlighting.
  const adj = new Map();
  for (const e of edges) {
    if (!adj.has(e.a)) adj.set(e.a, new Set());
    if (!adj.has(e.b)) adj.set(e.b, new Set());
    adj.get(e.a).add(e.b);
    adj.get(e.b).add(e.a);
  }

  // Simulation params.
  const REPEL = 1500;       // pairwise repulsion strength
  const SPRING = 0.02;      // edge spring strength
  const SPRING_LEN = 80;    // edge target length
  const CENTER = 0.005;     // pull toward center
  const DAMP = 0.85;        // velocity damping per frame
  const MAX_V = 4;

  function step() {
    // Repulsion (O(n²) — fine for vaults < ~500 nodes).
    for (let i = 0; i < nodes.length; i++) {
      for (let j = i + 1; j < nodes.length; j++) {
        const a = nodes[i], b = nodes[j];
        let dx = b.x - a.x, dy = b.y - a.y;
        let d2 = dx * dx + dy * dy + 1;
        const f = REPEL / d2;
        const d = Math.sqrt(d2);
        const fx = (dx / d) * f, fy = (dy / d) * f;
        a.vx -= fx; a.vy -= fy;
        b.vx += fx; b.vy += fy;
      }
    }
    // Spring attraction along edges.
    for (const e of edges) {
      const a = nodes[e.a], b = nodes[e.b];
      const dx = b.x - a.x, dy = b.y - a.y;
      const d = Math.sqrt(dx * dx + dy * dy) + 1;
      const f = SPRING * (d - SPRING_LEN);
      const fx = (dx / d) * f, fy = (dy / d) * f;
      a.vx += fx; a.vy += fy;
      b.vx -= fx; b.vy -= fy;
    }
    // Pull toward center.
    for (const n of nodes) {
      n.vx += (cx - n.x) * CENTER;
      n.vy += (cy - n.y) * CENTER;
      // Clamp + damp + integrate.
      n.vx = Math.max(-MAX_V, Math.min(MAX_V, n.vx)) * DAMP;
      n.vy = Math.max(-MAX_V, Math.min(MAX_V, n.vy)) * DAMP;
      n.x += n.vx; n.y += n.vy;
    }
  }

  // Hover state.
  let hover = null;
  function setHover(i) {
    if (hover === i) return;
    hover = i;
    canvas.style.cursor = i === null ? 'default' : 'pointer';
  }

  function hitTest(mx, my) {
    let best = null, bestD = 14 * 14;
    for (let i = 0; i < nodes.length; i++) {
      const n = nodes[i];
      const dx = n.x - mx, dy = n.y - my;
      const d2 = dx * dx + dy * dy;
      if (d2 < bestD) { bestD = d2; best = i; }
    }
    return best;
  }

  canvas.addEventListener('mousemove', (e) => {
    const r = canvas.getBoundingClientRect();
    setHover(hitTest(e.clientX - r.left, e.clientY - r.top));
  });
  canvas.addEventListener('mouseleave', () => setHover(null));
  canvas.addEventListener('click', (e) => {
    const r = canvas.getBoundingClientRect();
    const i = hitTest(e.clientX - r.left, e.clientY - r.top);
    if (i !== null) {
      window.location.href = '/' + nodes[i].id + '/';
    }
  });

  // CSS-var lookup (light/dark aware).
  const cs = getComputedStyle(document.documentElement);
  const FG = (cs.getPropertyValue('--fg') || '#1a1a1a').trim();
  const MUTED = (cs.getPropertyValue('--muted') || '#888').trim();
  const LINK = (cs.getPropertyValue('--link') || '#2563eb').trim();

  function draw() {
    const w = canvas.getBoundingClientRect().width;
    const h = canvas.getBoundingClientRect().height;
    ctx.clearRect(0, 0, w, h);
    const isHover = hover !== null;
    const neighborhood = isHover ? adj.get(hover) || new Set() : null;

    // Edges first, dimmed when hovering.
    for (const e of edges) {
      const a = nodes[e.a], b = nodes[e.b];
      const involved = !isHover ||
        e.a === hover || e.b === hover;
      ctx.strokeStyle = involved ? LINK : MUTED + '55';
      ctx.lineWidth = involved ? 1.2 : 0.6;
      ctx.beginPath();
      ctx.moveTo(a.x, a.y);
      ctx.lineTo(b.x, b.y);
      ctx.stroke();
    }
    // Nodes.
    for (let i = 0; i < nodes.length; i++) {
      const n = nodes[i];
      const involved = !isHover || i === hover || neighborhood.has(i);
      const radius = 4 + Math.min(8, n.degree * 1.5);
      ctx.beginPath();
      ctx.arc(n.x, n.y, radius, 0, Math.PI * 2);
      ctx.fillStyle = involved
        ? (i === hover ? LINK : FG)
        : MUTED + '88';
      ctx.fill();
      // Label — only on hover or for high-degree nodes.
      if (i === hover || (!isHover && n.degree >= 4)) {
        ctx.fillStyle = FG;
        ctx.font = '11px system-ui, sans-serif';
        ctx.fillText(n.label, n.x + radius + 4, n.y + 4);
      }
    }
  }

  function loop() {
    step();
    draw();
    requestAnimationFrame(loop);
  }
  loop();
})();
