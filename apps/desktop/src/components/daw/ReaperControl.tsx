import React, { useState, useEffect } from "react";
import { reaperApi, StatusResponse, KeymapInfo } from "../services/reaperApi";
import {
  Wifi,
  WifiOff,
  Activity,
  Stethoscope,
  FolderOpen,
  PenTool,
  Mic,
  Edit3,
  Sliders,
  Volume2,
  Play,
  Download,
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
import { Alert, AlertDescription } from "@/components/ui/alert";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { OverviewTab } from "./OverviewTab";
import { KeymapsTab } from "./KeymapsTab";
import { RawEntriesTab } from "./RawEntriesTab";
import { TestingTab } from "./TestingTab";
import { TransportContainer, SetlistContainer, PerformanceContainer, MarkerRegionContainer } from "./containers";
import { BottomNavigation } from "./BottomNavigation";
import { SideNavigation } from "./SideNavigation";
import { Ableset } from "./performance/themes/ableset";
import { MixPage } from "./mix/MixPage";

export const ReaperControl: React.FC = () => {
  const [connected, setConnected] = useState(false);
  const [status, setStatus] = useState<StatusResponse | null>(null);
  const [keymaps, setKeymaps] = useState<KeymapInfo[]>([]);
  const [currentKeymap, setCurrentKeymap] = useState<KeymapInfo | null>(null);
  const [loading, setLoading] = useState(false);
  const [activeBottomSection, setActiveBottomSection] = useState("session");
  const [performanceTheme, setPerformanceTheme] = useState("testing");

  // Check connection status
  const checkConnection = async () => {
    const isConnected = await reaperApi.checkConnection();
    setConnected(isConnected);

    if (isConnected) {
      const statusData = await reaperApi.getStatus();
      setStatus(statusData);
    }
  };

  // Load keymaps
  const loadKeymaps = async () => {
    setLoading(true);
    try {
      const [keymapList, current] = await Promise.all([
        reaperApi.getKeymaps(),
        reaperApi.getCurrentKeymap(),
      ]);
      setKeymaps(keymapList);
      setCurrentKeymap(current);
    } catch (error) {
      console.error("Failed to load keymaps:", error);
    } finally {
      setLoading(false);
    }
  };

  // Switch keymap
  const handleSwitchKeymap = async (keymapName: string) => {
    setLoading(true);
    const success = await reaperApi.switchKeymap(keymapName);
    if (success) {
      await loadKeymaps(); // Reload to get updated current keymap
    }
    setLoading(false);
  };

  // Test wheel command
  const testWheelCommand = async (direction: "up" | "down") => {
    await reaperApi.executeWheelCommand({
      window_type: "main",
      wheel_type: "vertical",
      delta: direction === "up" ? 120 : -120,
    });
  };

  useEffect(() => {
    checkConnection();
    const interval = setInterval(checkConnection, 5000); // Check every 5 seconds
    return () => clearInterval(interval);
  }, []);

  useEffect(() => {
    if (connected) {
      loadKeymaps();
    }
  }, [connected]);

  // Check if we should render the full-screen Ableset theme
  const isAblesetTheme =
    activeBottomSection === "performance" && performanceTheme === "ableset";

  // If Ableset theme is active, render full-screen view
  if (isAblesetTheme) {
    return (
      <div className="fixed inset-0 ableset-theme">
        <Ableset
          className="h-full w-full"
          onExit={() => setPerformanceTheme("testing")}
        />

        {/* Side Navigation (only component that remains accessible) */}
        <SideNavigation
          activeSection={activeBottomSection}
          onSectionChange={setActiveBottomSection}
          performanceTheme={performanceTheme}
          onPerformanceThemeChange={setPerformanceTheme}
        />
      </div>
    );
  }

  return (
    <div className="flex flex-col h-screen">
      {/* Top Bar - Fixed height */}
      <div className="flex-shrink-0 bg-background border-b border-border">
        {/* Header */}
        <div className="flex items-center justify-between p-4">
          <div>
            <h1 className="text-xl font-bold tracking-tight">REAPER Control</h1>
            <p className="text-sm text-muted-foreground">
              Manage your REAPER keymap configurations and test controls
            </p>
          </div>
          <div className="flex items-center gap-3">
            <Badge
              variant={connected ? "default" : "destructive"}
              className="flex items-center gap-2"
            >
              {connected ? (
                <>
                  <Wifi className="h-4 w-4" />
                  Connected
                </>
              ) : (
                <>
                  <WifiOff className="h-4 w-4" />
                  Disconnected
                </>
              )}
            </Badge>
          </div>
        </div>

        {/* Connection Alert */}
        {!connected && (
          <div className="px-4 pb-4">
            <Alert>
              <Activity className="h-4 w-4" />
              <AlertDescription>
                Make sure REAPER is running with the FTS Extensions plugin
                loaded.
              </AlertDescription>
            </Alert>
          </div>
        )}
      </div>

      {/* Main Content Area - Takes remaining height */}
      <div className="flex-1 min-h-0">
        {activeBottomSection === "mix" ? (
          <MixPage />
        ) : (
          <div className="h-full overflow-auto">
            <div className="container mx-auto p-6 space-y-6">
              {/* Bottom Section Content */}
              {activeBottomSection === "session" && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <FolderOpen className="h-5 w-5" />
                      Session Manager
                    </CardTitle>
                    <CardDescription>
                      Manage your REAPER sessions and project files
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-muted-foreground">
                      Session management features coming soon...
                    </p>
                  </CardContent>
                </Card>
              )}

              {activeBottomSection === "writing" && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <PenTool className="h-5 w-5" />
                      Writing
                    </CardTitle>
                    <CardDescription>
                      Tools for songwriting and composition
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-muted-foreground">
                      Writing tools coming soon...
                    </p>
                  </CardContent>
                </Card>
              )}

              {activeBottomSection === "recording" && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <Mic className="h-5 w-5" />
                      Recording
                    </CardTitle>
                    <CardDescription>
                      Recording controls and settings
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-muted-foreground">
                      Recording features coming soon...
                    </p>
                  </CardContent>
                </Card>
              )}

              {activeBottomSection === "editing" && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <Edit3 className="h-5 w-5" />
                      Editing
                    </CardTitle>
                    <CardDescription>
                      Audio and MIDI editing tools
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-muted-foreground">
                      Editing tools coming soon...
                    </p>
                  </CardContent>
                </Card>
              )}

              {activeBottomSection === "master" && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <Volume2 className="h-5 w-5" />
                      Master
                    </CardTitle>
                    <CardDescription>
                      Master bus and final processing
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-muted-foreground">
                      Mastering tools coming soon...
                    </p>
                  </CardContent>
                </Card>
              )}

              {activeBottomSection === "performance" && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <Play className="h-5 w-5" />
                      Performance - Testing Theme
                    </CardTitle>
                    <CardDescription>
                      Live performance and playback controls (Testing View)
                    </CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-6">
                    {/* Main Row: Setlist and Song Sections side by side */}
                    <SetlistContainer view="songView" />

                    {/* Bottom Row: Markers and Regions */}
                    <div className="w-full">
                      <MarkerRegionContainer />
                    </div>

                    <p className="text-muted-foreground text-sm">
                      Additional performance features coming soon...
                    </p>
                  </CardContent>
                </Card>
              )}

              {activeBottomSection === "export" && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <Download className="h-5 w-5" />
                      Export
                    </CardTitle>
                    <CardDescription>
                      Export and render your projects
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-muted-foreground">
                      Export features coming soon...
                    </p>
                  </CardContent>
                </Card>
              )}

              {activeBottomSection === "diagnostics" && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <Stethoscope className="h-5 w-5" />
                      Diagnostics
                    </CardTitle>
                    <CardDescription>
                      System diagnostics and troubleshooting
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    {connected && status && (
                      <Tabs defaultValue="overview" className="space-y-6">
                        <TabsList className="grid w-full grid-cols-4">
                          <TabsTrigger value="overview">Overview</TabsTrigger>
                          <TabsTrigger value="keymaps">Keymaps</TabsTrigger>
                          <TabsTrigger value="raw-entries">
                            Raw Entries
                          </TabsTrigger>
                          <TabsTrigger value="testing">Testing</TabsTrigger>
                        </TabsList>

                        <TabsContent value="overview" className="space-y-6">
                          <OverviewTab
                            status={status}
                            currentKeymap={currentKeymap}
                            loading={loading}
                            loadKeymaps={loadKeymaps}
                            testWheelCommand={testWheelCommand}
                          />
                        </TabsContent>

                        <TabsContent value="keymaps" className="space-y-6">
                          <KeymapsTab
                            status={status}
                            currentKeymap={currentKeymap}
                            loading={loading}
                            handleSwitchKeymap={handleSwitchKeymap}
                            loadKeymaps={loadKeymaps}
                          />
                        </TabsContent>

                        <TabsContent value="raw-entries" className="space-y-6">
                          <RawEntriesTab
                            keymapName={status.current_keymap || "Default"}
                          />
                        </TabsContent>

                        <TabsContent value="testing" className="space-y-6">
                          <TestingTab
                            connected={connected}
                            checkConnection={checkConnection}
                            testWheelCommand={testWheelCommand}
                          />
                        </TabsContent>
                      </Tabs>
                    )}
                  </CardContent>
                </Card>
              )}
            </div>
          </div>
        )}
      </div>

      {/* Transport Bar (appears globally in top right) */}
      <TransportContainer />

      {/* Side Navigation (appears at left edge) */}
      <SideNavigation
        activeSection={activeBottomSection}
        onSectionChange={setActiveBottomSection}
        performanceTheme={performanceTheme}
        onPerformanceThemeChange={setPerformanceTheme}
      />

      {/* Bottom Navigation (appears at 20px from bottom) */}
      <BottomNavigation
        activeSection={activeBottomSection}
        onSectionChange={setActiveBottomSection}
      />
    </div>
  );
};
