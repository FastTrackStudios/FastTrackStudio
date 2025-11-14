import React from "react";
import { StatusResponse, KeymapInfo } from "../services/reaperApi";
import {
  Settings,
  RefreshCw,
  ChevronUp,
  ChevronDown,
  FileText,
  Zap,
} from "lucide-react";

import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Separator } from "@/components/ui/separator";

interface OverviewTabProps {
  status: StatusResponse;
  currentKeymap: KeymapInfo | null;
  loading: boolean;
  loadKeymaps: () => Promise<void>;
  testWheelCommand: (direction: "up" | "down") => Promise<void>;
}

export const OverviewTab: React.FC<OverviewTabProps> = ({
  status,
  currentKeymap,
  loading,
  loadKeymaps,
  testWheelCommand,
}) => {
  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
      {/* Plugin Status */}
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
          <CardTitle className="text-sm font-medium">Plugin Status</CardTitle>
          <Settings className="h-4 w-4 text-muted-foreground" />
        </CardHeader>
        <CardContent>
          <div className="space-y-3">
            <div>
              <p className="text-2xl font-bold">{status.plugin_name}</p>
              <p className="text-xs text-muted-foreground">
                Version {status.version}
              </p>
            </div>
            <Separator />
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">
                  Current Keymap
                </span>
                <Badge variant="outline">
                  {status.current_keymap || "None"}
                </Badge>
              </div>
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">
                  Available Keymaps
                </span>
                <Badge variant="secondary">
                  {status.available_keymaps.length}
                </Badge>
              </div>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Quick Actions */}
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
          <CardTitle className="text-sm font-medium">Quick Actions</CardTitle>
          <Zap className="h-4 w-4 text-muted-foreground" />
        </CardHeader>
        <CardContent className="space-y-3">
          <Button
            onClick={loadKeymaps}
            disabled={loading}
            className="w-full"
            variant="outline"
          >
            <RefreshCw
              className={`h-4 w-4 mr-2 ${loading ? "animate-spin" : ""}`}
            />
            {loading ? "Refreshing..." : "Refresh Keymaps"}
          </Button>
          <Button
            onClick={() => testWheelCommand("up")}
            className="w-full"
            variant="outline"
          >
            <ChevronUp className="h-4 w-4 mr-2" />
            Test Wheel Up
          </Button>
          <Button
            onClick={() => testWheelCommand("down")}
            className="w-full"
            variant="outline"
          >
            <ChevronDown className="h-4 w-4 mr-2" />
            Test Wheel Down
          </Button>
        </CardContent>
      </Card>

      {/* Current Keymap Info */}
      {currentKeymap && (
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Active Keymap</CardTitle>
            <FileText className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="space-y-3">
              <div>
                <p className="font-medium">{currentKeymap.name}</p>
                <p className="text-xs text-muted-foreground">
                  {currentKeymap.file_path}
                </p>
              </div>
              <Separator />
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">Sections</span>
                <Badge variant="secondary">
                  {Object.keys(currentKeymap.sections).length}
                </Badge>
              </div>
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  );
};
