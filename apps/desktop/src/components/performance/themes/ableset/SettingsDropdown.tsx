import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { Switch } from "@/components/ui/switch";
import { cn } from "@/lib/utils";
import { Settings } from "lucide-react";
import * as React from "react";

type SettingOption = {
  id: string;
  label: string;
  enabled: boolean;
  isSubOption?: boolean;
  description?: string;
};

// Context to share settings state across components
export const SettingsContext = React.createContext<{
  settings: Array<SettingOption>;
  toggleSetting: (id: string) => void;
}>({
  settings: [],
  toggleSetting: () => {},
});

export const SettingsDropdownProvider: React.FC<{
  children: React.ReactNode;
}> = ({ children }) => {
  const [settings, setSettings] = React.useState<Array<SettingOption>>([
    { id: "songDescriptions", label: "Song Descriptions", enabled: true },
    { id: "songInformation", label: "Song Information", enabled: true },
    { id: "songTags", label: "Song Tags", enabled: true, isSubOption: true },
    {
      id: "currentSection",
      label: "Current Section",
      enabled: true,
      isSubOption: true,
    },
    {
      id: "nextSection",
      label: "Next Section",
      enabled: true,
      isSubOption: true,
    },
    { id: "tempo", label: "Tempo", enabled: true, isSubOption: true },
    {
      id: "timeSignature",
      label: "Time Signature",
      enabled: true,
      isSubOption: true,
    },
    {
      id: "songDuration",
      label: "Song Duration",
      enabled: true,
      isSubOption: true,
    },
    { id: "songProgress", label: "Song Progress", enabled: true },
    { id: "sectionNames", label: "Section Names", enabled: true },
    { id: "sectionProgress", label: "Section Progress", enabled: false },
    { id: "clock", label: "Clock", enabled: false },
    { id: "quickPlay", label: "Quick Play", enabled: false },
    { id: "nextQueuedSong", label: "Next/Queued Song", enabled: false },
    { id: "remainingSongs", label: "Remaining Songs", enabled: false },
    { id: "visualMetronome", label: "Visual Metronome", enabled: false },
    { id: "sectionColors", label: "Section Colors", enabled: false },
  ]);

  const toggleSetting = (id: string) => {
    setSettings(
      settings.map((setting) =>
        setting.id === id ? { ...setting, enabled: !setting.enabled } : setting
      )
    );
  };

  return (
    <SettingsContext.Provider value={{ settings, toggleSetting }}>
      {children}
    </SettingsContext.Provider>
  );
};

export const SettingsDropdown: React.FC = () => {
  const { settings, toggleSetting } = React.useContext(SettingsContext);
  const [open, setOpen] = React.useState(false);

  // Group settings for easier organization
  const songInfoId = "songInformation";
  const mainSettings = settings.filter(
    (s) => s.isSubOption !== true && s.id !== songInfoId
  );
  const songInfoSubOptions = settings.filter((s) => s.isSubOption === true);

  // Helper function to safely get setting status
  const isSettingEnabled = (id: string): boolean => {
    for (const setting of settings) {
      if (setting.id === id) {
        return setting.enabled;
      }
    }
    return false;
  };

  return (
    <DropdownMenu open={open} onOpenChange={setOpen}>
      <DropdownMenuTrigger asChild>
        <button
          className="flex h-12 w-12 items-center justify-center rounded-full transition-colors bg-[var(--ableset-color-background-light)] hover:bg-[var(--ableset-color-hover)]"
          title="Settings"
        >
          <Settings
            className="h-6 w-6 text-[var(--ableset-color-text)]"
            strokeWidth={1.5}
          />
        </button>
      </DropdownMenuTrigger>
      <DropdownMenuContent
        align="end"
        sideOffset={8}
        className={cn(
          "max-h-[85vh] w-80 overflow-y-auto",
          "rounded-2xl border-0 shadow-2xl",
          "bg-[var(--ableset-color-background-light)]",
          "data-[state=closed]:animate-out data-[state=open]:animate-in",
          "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
          "data-[state=closed]:zoom-out-95 data-[state=open]:zoom-in-95"
        )}
        style={{
          scrollbarWidth: "thin",
          scrollbarColor:
            "var(--ableset-color-default-700) var(--ableset-color-background-light)",
          zIndex: 9999,
        }}
      >
        <div className="space-y-1 p-6">
          {/* Song Information with sub-options */}
          <div className="space-y-1">
            <div className="flex items-center justify-between py-4">
              <span className="text-lg font-medium text-[var(--ableset-color-text)]">
                Song Information
              </span>
              <Switch
                checked={isSettingEnabled(songInfoId)}
                onCheckedChange={() => {
                  toggleSetting(songInfoId);
                }}
              />
            </div>

            {/* Sub-options */}
            <div className="space-y-1 pl-6">
              {songInfoSubOptions.map((setting) => (
                <div
                  key={setting.id}
                  className="flex items-center justify-between py-4"
                >
                  <span className="text-lg text-[var(--ableset-color-text)]">
                    {setting.label}
                  </span>
                  <Switch
                    checked={setting.enabled}
                    onCheckedChange={() => {
                      toggleSetting(setting.id);
                    }}
                  />
                </div>
              ))}
            </div>
          </div>

          {/* Other main settings */}
          {mainSettings.map((setting) => (
            <div
              key={setting.id}
              className="flex items-center justify-between py-4"
            >
              <span className="text-lg font-medium text-[var(--ableset-color-text)]">
                {setting.label}
              </span>
              <Switch
                checked={setting.enabled}
                onCheckedChange={() => {
                  toggleSetting(setting.id);
                }}
              />
            </div>
          ))}
        </div>
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
