const UI_WIDTH = 460;
const UI_HEIGHT = 680;

type ExportOptions = {
  includeSvg: boolean;
  includePng: boolean;
  maxDepth: number;
};

type ExportNode = {
  id: string;
  name: string;
  type: string;
  visible: boolean;
  locked?: boolean;
  opacity?: number;
  blendMode?: string;
  absoluteBoundingBox?: RectLike;
  relativeTransform?: number[][];
  size?: SizeLike;
  rotation?: number;
  constraints?: unknown;
  clipsContent?: boolean;
  layoutMode?: string;
  layoutWrap?: string;
  layoutSizingHorizontal?: string;
  layoutSizingVertical?: string;
  layoutAlign?: string;
  layoutGrow?: number;
  primaryAxisSizingMode?: string;
  primaryAxisAlignItems?: string;
  counterAxisSizingMode?: string;
  counterAxisAlignItems?: string;
  paddingTop?: number;
  paddingRight?: number;
  paddingBottom?: number;
  paddingLeft?: number;
  itemSpacing?: number;
  effects?: unknown[];
  fills?: unknown[];
  strokes?: unknown[];
  strokeWeight?: number;
  strokeAlign?: string;
  strokeJoin?: string;
  strokeCap?: string;
  dashPattern?: number[];
  cornerRadius?: number;
  topLeftRadius?: number;
  topRightRadius?: number;
  bottomLeftRadius?: number;
  bottomRightRadius?: number;
  rectangleCornerRadii?: number[];
  fillGeometry?: unknown;
  strokeGeometry?: unknown;
  characters?: string;
  textAlignHorizontal?: string;
  textAlignVertical?: string;
  textAutoResize?: string;
  lineHeight?: unknown;
  letterSpacing?: unknown;
  paragraphSpacing?: number;
  paragraphIndent?: number;
  textCase?: string;
  textDecoration?: string;
  textStyleId?: string;
  componentId?: string;
  componentSetId?: string;
  variantProperties?: Record<string, string>;
  exports?: {
    svgBase64?: string;
    pngBase64?: string;
  };
  children?: ExportNode[];
};

type RectLike = {
  x: number;
  y: number;
  width: number;
  height: number;
};

type SizeLike = {
  x: number;
  y: number;
};

type ExportPayload = {
  schema: 'fts.figma.export/v1';
  generatedAt: string;
  source: {
    plugin: string;
    pluginVersion: string;
    editorType: string;
    fileKey?: string;
    page: { id: string; name: string };
  };
  selection: {
    ids: string[];
    names: string[];
    totalRoots: number;
    totalNodes: number;
  };
  options: ExportOptions;
  nodes: ExportNode[];
};

type ExportRoot = SceneNode;

let liveSyncEnabled = false;
let exportQueued = false;
let bridgeLiveEnabled = true;
let bridgeLiveUrl = 'http://localhost:43123/figma-export';
let pinnedExportRootIds: string[] = [];
let hasManualRootSelection = false;

figma.showUI(__html__, { width: UI_WIDTH, height: UI_HEIGHT, themeColors: true });
postSelectionSummary();
maybeAutoPinMainFrame();
postPinnedRootsSummary();
postFrameCatalog();

figma.on('selectionchange', () => {
  postSelectionSummary();
  if (liveSyncEnabled) {
    queueExport();
  }
});

figma.on('documentchange', () => {
  prunePinnedRoots();
  maybeAutoPinMainFrame();
  postFrameCatalog();
  postPinnedRootsSummary();
  if (liveSyncEnabled) {
    queueExport();
  }
});

figma.ui.onmessage = async (msg: unknown) => {
  if (!msg || typeof msg !== 'object') {
    return;
  }

  const message = msg as {
    type?: string;
    options?: Partial<ExportOptions>;
    enabled?: boolean;
    bridgeUrl?: string;
    bridgeEnabled?: boolean;
    nodeId?: string;
  };

  if (message.type === 'close') {
    figma.closePlugin();
    return;
  }

  if (message.type === 'set-live') {
    liveSyncEnabled = Boolean(message.enabled);
    figma.notify(liveSyncEnabled ? 'Live export enabled' : 'Live export paused');
    if (liveSyncEnabled) {
      queueExport();
    }
    return;
  }

  if (message.type === 'export-now') {
    const options = normalizeOptions(message.options);
    await exportSelection(options, 'manual');
    return;
  }

  if (message.type === 'export-to-bridge') {
    const options = normalizeOptions(message.options);
    await exportToBridge(options, message.bridgeUrl);
    return;
  }

  if (message.type === 'set-live-bridge') {
    bridgeLiveEnabled = Boolean(message.bridgeEnabled);
    if (typeof message.bridgeUrl === 'string' && message.bridgeUrl.trim().length > 0) {
      bridgeLiveUrl = message.bridgeUrl.trim();
    }
    figma.notify(bridgeLiveEnabled ? 'Live bridge enabled' : 'Live bridge disabled');
    if (liveSyncEnabled && bridgeLiveEnabled) {
      queueExport();
    }
    return;
  }

  if (message.type === 'set-export-roots-from-selection') {
    hasManualRootSelection = true;
    const roots = rootsFromSelection();
    if (roots.length === 0) {
      figma.notify('Select at least one frame before pinning export roots.');
      figma.ui.postMessage({
        type: 'export-error',
        message: 'Select at least one frame before pinning export roots.',
      });
      return;
    }
    pinnedExportRootIds = roots.map((node) => node.id);
    postPinnedRootsSummary();
    postFrameCatalog();
    figma.notify(`Pinned ${roots.length} export root(s).`);
    if (liveSyncEnabled) {
      queueExport();
    }
    return;
  }

  if (message.type === 'set-export-root-by-id') {
    hasManualRootSelection = true;
    const nodeId = message.nodeId?.trim();
    if (!nodeId) {
      return;
    }
    const node = figma.getNodeById(nodeId);
    if (!node || node.type !== 'FRAME') {
      figma.notify('Selected node is no longer a frame.');
      postFrameCatalog();
      return;
    }
    pinnedExportRootIds = [node.id];
    postPinnedRootsSummary();
    postFrameCatalog();
    figma.notify(`Pinned frame: ${node.name}`);
    if (liveSyncEnabled) {
      queueExport();
    }
    return;
  }

  if (message.type === 'clear-export-roots') {
    hasManualRootSelection = true;
    pinnedExportRootIds = [];
    postPinnedRootsSummary();
    postFrameCatalog();
    figma.notify('Cleared pinned export roots.');
    return;
  }

  if (message.type === 'refresh-frames') {
    prunePinnedRoots();
    maybeAutoPinMainFrame();
    postFrameCatalog();
    postPinnedRootsSummary();
  }
};

function postSelectionSummary(): void {
  const selection = figma.currentPage.selection;
  figma.ui.postMessage({
    type: 'selection-summary',
    selectionCount: selection.length,
    selectedNames: selection.map((n) => `${n.name} (${n.type})`),
    hasSelection: selection.length > 0,
  });
}

function postPinnedRootsSummary(): void {
  const roots = resolvePinnedRoots();
  figma.ui.postMessage({
    type: 'pinned-roots-summary',
    count: roots.length,
    names: roots.map((n) => `${n.name} (${n.type})`),
    hasPinned: roots.length > 0,
  });
}

function postFrameCatalog(): void {
  const frames = figma.currentPage.findAll((n) => n.type === 'FRAME') as FrameNode[];
  figma.ui.postMessage({
    type: 'frame-catalog',
    frames: frames.map((f) => ({ id: f.id, name: f.name, type: f.type })),
    pinnedIds: [...pinnedExportRootIds],
  });
}

function prunePinnedRoots(): void {
  pinnedExportRootIds = pinnedExportRootIds.filter((id) => {
    const node = figma.getNodeById(id);
    return Boolean(node && node.type === 'FRAME');
  });
}

function maybeAutoPinMainFrame(): void {
  if (hasManualRootSelection || pinnedExportRootIds.length > 0) {
    return;
  }
  const frames = figma.currentPage.findAll((n) => n.type === 'FRAME') as FrameNode[];
  const main = frames.find((f) => f.name.trim().toLowerCase() === 'main');
  if (main) {
    pinnedExportRootIds = [main.id];
  }
}

function rootsFromSelection(): ExportRoot[] {
  const selection = figma.currentPage.selection;
  return selection.filter((n): n is ExportRoot => n.type === 'FRAME');
}

function resolvePinnedRoots(): ExportRoot[] {
  const roots: ExportRoot[] = [];
  for (const id of pinnedExportRootIds) {
    const node = figma.getNodeById(id);
    if (node && node.type === 'FRAME') {
      roots.push(node as ExportRoot);
    }
  }
  return roots;
}

function resolveExportRoots(): ExportRoot[] {
  const pinned = resolvePinnedRoots();
  if (pinned.length > 0) {
    return pinned;
  }
  return rootsFromSelection();
}

function queueExport(): void {
  if (exportQueued) {
    return;
  }

  exportQueued = true;
  setTimeout(async () => {
    exportQueued = false;
    const options = normalizeOptions(undefined);
    await exportSelection(options, 'live');
    if (bridgeLiveEnabled) {
      await exportToBridge(options, bridgeLiveUrl, true);
    }
  }, 150);
}

function normalizeOptions(input?: Partial<ExportOptions>): ExportOptions {
  return {
    includeSvg: input?.includeSvg ?? true,
    includePng: input?.includePng ?? false,
    maxDepth: Math.max(1, Math.min(32, input?.maxDepth ?? 16)),
  };
}

async function exportSelection(options: ExportOptions, reason: 'manual' | 'live'): Promise<void> {
  const roots = resolveExportRoots();
  if (roots.length === 0) {
    figma.ui.postMessage({
      type: 'export-error',
      message:
        'No export roots. Pin a frame with "Select Frame" or select a frame on canvas.',
    });
    return;
  }

  try {
    const nodes = await Promise.all(roots.map((node) => serializeNode(node, 0, options)));
    const totalNodes = nodes.reduce((count, node) => count + countNodes(node), 0);

    const payload: ExportPayload = {
      schema: 'fts.figma.export/v1',
      generatedAt: new Date().toISOString(),
      source: {
        plugin: 'fts-figma-import',
        pluginVersion: '0.1.0',
        editorType: figma.editorType,
        fileKey: figma.fileKey ?? undefined,
        page: {
          id: figma.currentPage.id,
          name: figma.currentPage.name,
        },
      },
      selection: {
        ids: roots.map((node) => node.id),
        names: roots.map((node) => node.name),
        totalRoots: roots.length,
        totalNodes,
      },
      options,
      nodes,
    };

    const json = JSON.stringify(payload, null, 2);
    figma.ui.postMessage({
      type: 'export-result',
      reason,
      json,
      stats: {
        totalRoots: roots.length,
        totalNodes,
        bytes: json.length,
      },
    });

    if (reason === 'manual') {
      figma.notify(`Exported ${totalNodes} nodes from ${roots.length} root(s).`);
    }
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    figma.ui.postMessage({
      type: 'export-error',
      message,
    });
  }
}

async function exportToBridge(
  options: ExportOptions,
  bridgeUrl?: string,
  silent = false,
): Promise<void> {
  const roots = resolveExportRoots();
  if (roots.length === 0) {
    figma.ui.postMessage({
      type: 'bridge-error',
      message:
        'No export roots. Pin a frame with "Select Frame" or select a frame on canvas.',
    });
    return;
  }

  try {
    const nodes = await Promise.all(roots.map((node) => serializeNode(node, 0, options)));
    const totalNodes = nodes.reduce((count, node) => count + countNodes(node), 0);
    const payload: ExportPayload = {
      schema: 'fts.figma.export/v1',
      generatedAt: new Date().toISOString(),
      source: {
        plugin: 'fts-figma-import',
        pluginVersion: '0.1.0',
        editorType: figma.editorType,
        fileKey: figma.fileKey ?? undefined,
        page: {
          id: figma.currentPage.id,
          name: figma.currentPage.name,
        },
      },
      selection: {
        ids: roots.map((node) => node.id),
        names: roots.map((node) => node.name),
        totalRoots: roots.length,
        totalNodes,
      },
      options,
      nodes,
    };
    const json = JSON.stringify(payload, null, 2);
    const url = bridgeUrl?.trim() || 'http://localhost:43123/figma-export';
    const res = await fetch(url, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: json,
    });

    if (!res.ok) {
      throw new Error(`Bridge responded ${res.status} ${res.statusText}`);
    }

    const responseBody = (await res.json()) as {
      ok?: boolean;
      path?: string;
      bytes?: number;
      error?: string;
    };

    if (!responseBody.ok) {
      throw new Error(responseBody.error ?? 'Bridge failed to write file.');
    }

    figma.ui.postMessage({
      type: 'bridge-result',
      path: responseBody.path,
      bytes: responseBody.bytes,
      totalNodes,
    });
    if (!silent) {
      figma.notify(
        `Sent ${totalNodes} nodes to bridge (${responseBody.path ?? '/tmp/fts-figma-export.json'}).`,
      );
    }
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    figma.ui.postMessage({
      type: 'bridge-error',
      message,
    });
  }
}

async function serializeNode(
  node: SceneNode,
  depth: number,
  options: ExportOptions,
  parentBounds?: RectLike,
): Promise<ExportNode> {
  const absoluteBoundingBox = readAbsoluteBoundingBox(node);
  const relativeTransform = normalizeRelativeTransform(
    sanitize(readProp(node, 'relativeTransform')),
    absoluteBoundingBox,
    parentBounds,
  );

  const base: ExportNode = {
    id: node.id,
    name: node.name,
    type: node.type,
    visible: node.visible,
    locked: readProp(node, 'locked'),
    opacity: readProp(node, 'opacity'),
    blendMode: readProp(node, 'blendMode'),
    absoluteBoundingBox,
    relativeTransform,
    size: readSize(node),
    rotation: readProp(node, 'rotation'),
    constraints: sanitize(readProp(node, 'constraints')),
    clipsContent: readProp(node, 'clipsContent'),
    layoutMode: readProp(node, 'layoutMode'),
    layoutWrap: readProp(node, 'layoutWrap'),
    layoutSizingHorizontal: readProp(node, 'layoutSizingHorizontal'),
    layoutSizingVertical: readProp(node, 'layoutSizingVertical'),
    layoutAlign: readProp(node, 'layoutAlign'),
    layoutGrow: readProp(node, 'layoutGrow'),
    primaryAxisSizingMode: readProp(node, 'primaryAxisSizingMode'),
    primaryAxisAlignItems: readProp(node, 'primaryAxisAlignItems'),
    counterAxisSizingMode: readProp(node, 'counterAxisSizingMode'),
    counterAxisAlignItems: readProp(node, 'counterAxisAlignItems'),
    paddingTop: readProp(node, 'paddingTop'),
    paddingRight: readProp(node, 'paddingRight'),
    paddingBottom: readProp(node, 'paddingBottom'),
    paddingLeft: readProp(node, 'paddingLeft'),
    itemSpacing: readProp(node, 'itemSpacing'),
    effects: sanitize(readProp(node, 'effects')),
    fills: sanitize(readProp(node, 'fills')),
    strokes: sanitize(readProp(node, 'strokes')),
    strokeWeight: readProp(node, 'strokeWeight'),
    strokeAlign: readProp(node, 'strokeAlign'),
    strokeJoin: readProp(node, 'strokeJoin'),
    strokeCap: readProp(node, 'strokeCap'),
    dashPattern: sanitize(readProp(node, 'dashPattern')),
    cornerRadius: readProp(node, 'cornerRadius'),
    topLeftRadius: readProp(node, 'topLeftRadius'),
    topRightRadius: readProp(node, 'topRightRadius'),
    bottomLeftRadius: readProp(node, 'bottomLeftRadius'),
    bottomRightRadius: readProp(node, 'bottomRightRadius'),
    rectangleCornerRadii: sanitize(readProp(node, 'rectangleCornerRadii')),
    fillGeometry: sanitize(readProp(node, 'fillGeometry')),
    strokeGeometry: sanitize(readProp(node, 'strokeGeometry')),
    componentId: readProp(node, 'componentId'),
    componentSetId: readProp(node, 'componentSetId'),
    variantProperties: sanitize(readProp(node, 'variantProperties')),
    textStyleId: readProp(node, 'textStyleId'),
  };

  if (node.type === 'TEXT') {
    base.characters = node.characters;
    base.textAlignHorizontal = node.textAlignHorizontal;
    base.textAlignVertical = node.textAlignVertical;
    base.textAutoResize = node.textAutoResize;
    base.lineHeight = sanitize(node.lineHeight);
    base.letterSpacing = sanitize(node.letterSpacing);
    base.paragraphSpacing = node.paragraphSpacing;
    base.paragraphIndent = node.paragraphIndent;
    base.textCase = sanitize(node.textCase) as string;
    base.textDecoration = sanitize(node.textDecoration) as string;
  }

  if (options.includeSvg || options.includePng) {
    base.exports = await exportNodeAssets(node, options);
  }

  if ('children' in node && depth < options.maxDepth) {
    const sceneChildren = node.children.filter(isSceneNode);
    base.children = await Promise.all(
      sceneChildren.map((child) => serializeNode(child, depth + 1, options, absoluteBoundingBox)),
    );
  }

  return pruneUndefined(base);
}

function normalizeRelativeTransform(
  transform: unknown,
  nodeBounds?: RectLike,
  parentBounds?: RectLike,
): number[][] | undefined {
  if (!transform || !Array.isArray(transform)) {
    return undefined;
  }
  if (!nodeBounds || !parentBounds) {
    return sanitize(transform) as number[][] | undefined;
  }
  if (transform.length < 2) {
    return sanitize(transform) as number[][] | undefined;
  }
  const row0 = transform[0];
  const row1 = transform[1];
  if (!Array.isArray(row0) || !Array.isArray(row1) || row0.length < 3 || row1.length < 3) {
    return sanitize(transform) as number[][] | undefined;
  }

  const localX = nodeBounds.x - parentBounds.x;
  const localY = nodeBounds.y - parentBounds.y;
  const next0 = [...row0];
  const next1 = [...row1];
  next0[2] = localX;
  next1[2] = localY;
  return [next0 as number[], next1 as number[]];
}

async function exportNodeAssets(
  node: SceneNode,
  options: ExportOptions,
): Promise<{ svgBase64?: string; pngBase64?: string }> {
  const result: { svgBase64?: string; pngBase64?: string } = {};

  if (options.includeSvg && supportsVectorExport(node)) {
    try {
      const svgBytes = await node.exportAsync({ format: 'SVG' });
      result.svgBase64 = bytesToBase64(svgBytes);
    } catch {
      // Ignore unsupported SVG exports for individual nodes.
    }
  }

  if (options.includePng && supportsRasterExport(node)) {
    try {
      const pngBytes = await node.exportAsync({
        format: 'PNG',
        constraint: { type: 'SCALE', value: 1 },
      });
      result.pngBase64 = bytesToBase64(pngBytes);
    } catch {
      // Ignore unsupported PNG exports for individual nodes.
    }
  }

  return result;
}

function supportsVectorExport(node: SceneNode): boolean {
  return node.type !== 'SLICE';
}

function supportsRasterExport(node: SceneNode): boolean {
  return node.type !== 'SLICE';
}

function isSceneNode(node: BaseNode): node is SceneNode {
  return (
    node.type !== 'PAGE' &&
    node.type !== 'DOCUMENT' &&
    node.type !== 'SECTION' &&
    node.type !== 'STICKY' &&
    node.type !== 'SHAPE_WITH_TEXT' &&
    node.type !== 'CONNECTOR' &&
    node.type !== 'WASHI_TAPE' &&
    node.type !== 'HIGHLIGHT'
  );
}

function readAbsoluteBoundingBox(node: SceneNode): RectLike | undefined {
  const bounds = readProp<RectLike>(node, 'absoluteRenderBounds') ?? readProp<RectLike>(node, 'absoluteBoundingBox');
  if (!bounds) {
    return undefined;
  }
  return {
    x: bounds.x,
    y: bounds.y,
    width: bounds.width,
    height: bounds.height,
  };
}

function readSize(node: SceneNode): SizeLike | undefined {
  const width = readProp<number>(node, 'width');
  const height = readProp<number>(node, 'height');
  if (typeof width !== 'number' || typeof height !== 'number') {
    return undefined;
  }
  return { x: width, y: height };
}

function readProp<T>(node: unknown, key: string): T | undefined {
  const value = (node as Record<string, unknown>)[key];
  return value as T | undefined;
}

function sanitize<T>(value: T): T {
  if (value === undefined || value === null) {
    return value;
  }

  if (value === figma.mixed) {
    return '__MIXED__' as T;
  }

  // Primitives do not need deep JSON sanitization.
  if (typeof value !== 'object') {
    return value;
  }

  const json = JSON.stringify(value, (_k, v) => {
    if (typeof v === 'bigint') {
      return v.toString();
    }
    if (typeof v === 'symbol') {
      return String(v);
    }
    if (v === figma.mixed) {
      return '__MIXED__';
    }
    return v;
  });

  if (json === undefined) {
    return value;
  }

  return JSON.parse(json) as T;
}

function pruneUndefined<T extends Record<string, unknown>>(input: T): T {
  const output: Record<string, unknown> = {};
  for (const key of Object.keys(input)) {
    const value = input[key];
    if (value === undefined) {
      continue;
    }
    if (Array.isArray(value) && value.length === 0) {
      continue;
    }
    output[key] = value;
  }
  return output as T;
}

function countNodes(node: ExportNode): number {
  let count = 1;
  for (const child of node.children ?? []) {
    count += countNodes(child);
  }
  return count;
}

function bytesToBase64(bytes: Uint8Array): string {
  const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  let output = '';
  for (let i = 0; i < bytes.length; i += 3) {
    const a = bytes[i];
    const b = i + 1 < bytes.length ? bytes[i + 1] : 0;
    const c = i + 2 < bytes.length ? bytes[i + 2] : 0;

    const triple = (a << 16) | (b << 8) | c;
    const enc1 = (triple >> 18) & 63;
    const enc2 = (triple >> 12) & 63;
    const enc3 = (triple >> 6) & 63;
    const enc4 = triple & 63;

    output += alphabet[enc1] + alphabet[enc2];
    output += i + 1 < bytes.length ? alphabet[enc3] : '=';
    output += i + 2 < bytes.length ? alphabet[enc4] : '=';
  }
  return output;
}
