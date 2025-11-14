import React from "react";
import { StatusResponse, KeymapInfo } from "../services/reaperApi";
import { Keyboard, RefreshCw } from "lucide-react";

import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { ScrollArea } from "@/components/ui/scroll-area";

interface KeymapsTabProps {
  status: StatusResponse;
  currentKeymap: KeymapInfo | null;
  loading: boolean;
  loadKeymaps: () => Promise<void>;
  handleSwitchKeymap: (keymapName: string) => Promise<void>;
}

export const KeymapsTab: React.FC<KeymapsTabProps> = ({
  status,
  currentKeymap,
  loading,
  loadKeymaps,
  handleSwitchKeymap,
}) => {
  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Keyboard className="h-5 w-5" />
          Keymap Management
        </CardTitle>
        <CardDescription>
          Switch between different keymap configurations
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-4">
        <div className="flex gap-4">
          <div className="flex-1">
            <Select
              value={status.current_keymap || ""}
              onValueChange={handleSwitchKeymap}
              disabled={loading}
            >
              <SelectTrigger>
                <SelectValue placeholder="Select a keymap..." />
              </SelectTrigger>
              <SelectContent>
                {status.available_keymaps.map((keymap) => (
                  <SelectItem key={keymap} value={keymap}>
                    {keymap}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
          <Button onClick={loadKeymaps} disabled={loading} variant="outline">
            <RefreshCw className={`h-4 w-4 ${loading ? "animate-spin" : ""}`} />
          </Button>
        </div>

        {/* Keymap Details */}
        {currentKeymap && Object.keys(currentKeymap.sections).length > 0 && (
          <Card>
            <CardHeader>
              <CardTitle className="text-lg">Keymap Details</CardTitle>
              <CardDescription>
                Sections and keybindings in {currentKeymap.name}
              </CardDescription>
            </CardHeader>
            <CardContent>
              <ScrollArea className="h-[400px]">
                <div className="space-y-4">
                  {Object.entries(currentKeymap.sections).map(
                    ([section, keybinds]) => (
                      <div key={section} className="space-y-2">
                        <div className="flex items-center gap-2">
                          <Badge variant="outline">{section}</Badge>
                          <span className="text-sm text-muted-foreground">
                            {keybinds.length} keybinds
                          </span>
                        </div>
                        <div className="ml-4 space-y-1">
                          {keybinds.slice(0, 5).map((keybind, index) => (
                            <div
                              key={index}
                              className="flex items-center justify-between text-sm"
                            >
                              <div className="flex items-center gap-2">
                                <Badge
                                  variant="secondary"
                                  className="font-mono text-xs"
                                >
                                  {keybind.modifiers.length > 0 &&
                                    `${keybind.modifiers.join("+")}+`}
                                  {keybind.key}
                                </Badge>
                                <span className="text-muted-foreground">→</span>
                                <code className="text-xs bg-muted px-1 py-0.5 rounded">
                                  {keybind.command_id}
                                </code>
                              </div>
                            </div>
                          ))}
                          {keybinds.length > 5 && (
                            <p className="text-xs text-muted-foreground ml-2">
                              ...and {keybinds.length - 5} more keybinds
                            </p>
                          )}
                        </div>
                      </div>
                    )
                  )}
                </div>
              </ScrollArea>
            </CardContent>
          </Card>
        )}
      </CardContent>
    </Card>
  );
};
