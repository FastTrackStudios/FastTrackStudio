import {
  App,
  Notice,
  Plugin,
  PluginSettingTab,
  Setting,
  TFile,
  parseYaml,
  stringifyYaml,
} from "obsidian";

// WASM bindings built by `bash build.sh` → `pkg/task_vault_core.{js,wasm}`
import init, {
  execute_query,
  parse_task_yaml,
  serialize_task_yaml,
  sort_by_urgency,
  urgency_score,
  validate_task,
} from "./pkg/task_vault_core.js";
import wasmBytes from "./pkg/task_vault_core_bg.wasm";

// ── Settings ────────────────────────────────────────────────────────────────

interface TaskPluginSettings {
  /** Port the desktop companion's Vox WebSocket server listens on. */
  desktopPort: number;
  /** Auto-reload task cache when vault files change. */
  autoRefresh: boolean;
}

const DEFAULT_SETTINGS: TaskPluginSettings = {
  desktopPort: 9000,
  autoRefresh: true,
};

// ── Plugin ───────────────────────────────────────────────────────────────────

export default class TaskPlugin extends Plugin {
  settings!: TaskPluginSettings;
  private wasmReady = false;

  async onload() {
    await this.loadSettings();

    // Initialise WASM from inlined bytes (no fetch/import.meta.url needed).
    await init(wasmBytes);
    this.wasmReady = true;

    // ── Commands ────────────────────────────────────────────────────────────

    this.addCommand({
      id: "create-task",
      name: "Create task",
      callback: () => this.createTaskInActiveFile(),
    });

    this.addCommand({
      id: "validate-frontmatter",
      name: "Validate task frontmatter",
      checkCallback: (checking) => {
        const file = this.app.workspace.getActiveFile();
        if (!file || file.extension !== "md") return false;
        if (!checking) this.validateActiveFile(file);
        return true;
      },
    });

    this.addCommand({
      id: "sort-by-urgency",
      name: "Sort task list by urgency",
      callback: () => this.sortOpenTasksByUrgency(),
    });

    // ── Vault event wiring ──────────────────────────────────────────────────

    if (this.settings.autoRefresh) {
      this.registerEvent(
        this.app.vault.on("modify", (file) => {
          if (file instanceof TFile && file.extension === "md") {
            this.onMarkdownModified(file);
          }
        })
      );

      this.registerEvent(
        this.app.vault.on("create", (file) => {
          if (file instanceof TFile && file.extension === "md") {
            this.onMarkdownModified(file);
          }
        })
      );
    }

    // ── Settings tab ────────────────────────────────────────────────────────

    this.addSettingTab(new TaskSettingTab(this.app, this));

    console.log("Task plugin loaded");
  }

  onunload() {
    console.log("Task plugin unloaded");
  }

  // ── Vault helpers ─────────────────────────────────────────────────────────

  /**
   * Called whenever a markdown file is modified.
   * Parses its frontmatter via WASM and validates the task schema if present.
   */
  private async onMarkdownModified(file: TFile) {
    if (!this.wasmReady) return;
    const content = await this.app.vault.read(file);
    const yaml = extractFrontmatter(content);
    if (!yaml) return;

    const error = validate_task(yaml);
    if (error) {
      console.debug(`[task] ${file.path}: ${error}`);
    }
  }

  /** Parse the YAML frontmatter of a file and return it as a task JSON string. */
  async readTaskJson(file: TFile): Promise<string | null> {
    if (!this.wasmReady) return null;
    const content = await this.app.vault.read(file);
    const yaml = extractFrontmatter(content);
    if (!yaml) return null;
    const json = parse_task_yaml(yaml);
    if (json.startsWith('{"error"')) return null;
    return json;
  }

  /** Collect all task JSON objects from markdown files in the vault. */
  async allTasksJson(): Promise<string[]> {
    const files = this.app.vault.getMarkdownFiles();
    const results: string[] = [];
    for (const file of files) {
      const json = await this.readTaskJson(file);
      if (json) results.push(json);
    }
    return results;
  }

  // ── Command implementations ───────────────────────────────────────────────

  private async createTaskInActiveFile() {
    const file = this.app.workspace.getActiveFile();
    if (!file) {
      new Notice("No active file.");
      return;
    }
    const content = await this.app.vault.read(file);
    if (extractFrontmatter(content) !== null) {
      new Notice("File already has frontmatter.");
      return;
    }
    const title = file.basename;
    const frontmatter = [
      "---",
      `title: "${title}"`,
      "status: Open",
      "priority: Normal",
      "---",
      "",
    ].join("\n");
    await this.app.vault.modify(file, frontmatter + content);
    new Notice(`Task created: ${title}`);
  }

  private async validateActiveFile(file: TFile) {
    if (!this.wasmReady) return;
    const content = await this.app.vault.read(file);
    const yaml = extractFrontmatter(content);
    if (!yaml) {
      new Notice("No frontmatter found.");
      return;
    }
    const error = validate_task(yaml);
    if (error) {
      new Notice(`Invalid task: ${error}`);
    } else {
      new Notice("Task frontmatter is valid.");
    }
  }

  private async sortOpenTasksByUrgency() {
    if (!this.wasmReady) return;
    const jsons = await this.allTasksJson();
    if (jsons.length === 0) {
      new Notice("No tasks found in vault.");
      return;
    }
    const arrayJson = `[${jsons.join(",")}]`;
    const sorted = sort_by_urgency(arrayJson);
    if (sorted.startsWith('{"error"')) {
      new Notice("Failed to sort tasks.");
      return;
    }
    new Notice(`Sorted ${jsons.length} tasks by urgency.`);
    console.log("[task] urgency-sorted tasks:", sorted);
  }

  // ── Settings ──────────────────────────────────────────────────────────────

  async loadSettings() {
    this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());
  }

  async saveSettings() {
    await this.saveData(this.settings);
  }
}

// ── Settings tab ─────────────────────────────────────────────────────────────

class TaskSettingTab extends PluginSettingTab {
  plugin: TaskPlugin;

  constructor(app: App, plugin: TaskPlugin) {
    super(app, plugin);
    this.plugin = plugin;
  }

  display(): void {
    const { containerEl } = this;
    containerEl.empty();

    new Setting(containerEl)
      .setName("Desktop companion port")
      .setDesc("WebSocket port the desktop Task app listens on (default 9000).")
      .addText((text) =>
        text
          .setPlaceholder("9000")
          .setValue(String(this.plugin.settings.desktopPort))
          .onChange(async (value) => {
            const n = parseInt(value, 10);
            if (!isNaN(n)) {
              this.plugin.settings.desktopPort = n;
              await this.plugin.saveSettings();
            }
          })
      );

    new Setting(containerEl)
      .setName("Auto-refresh on file change")
      .setDesc("Validate task frontmatter whenever a markdown file is saved.")
      .addToggle((toggle) =>
        toggle
          .setValue(this.plugin.settings.autoRefresh)
          .onChange(async (value) => {
            this.plugin.settings.autoRefresh = value;
            await this.plugin.saveSettings();
          })
      );
  }
}

// ── Utilities ─────────────────────────────────────────────────────────────────

/**
 * Extract the YAML block between the first pair of `---` delimiters.
 * Returns null if the file has no frontmatter.
 */
function extractFrontmatter(content: string): string | null {
  if (!content.startsWith("---")) return null;
  const end = content.indexOf("\n---", 3);
  if (end === -1) return null;
  return content.slice(4, end).trim();
}
