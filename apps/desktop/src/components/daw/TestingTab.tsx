import React from "react";
import { MousePointer, ChevronUp, ChevronDown, RefreshCw } from "lucide-react";

import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Button } from "@/components/ui/button";

interface TestingTabProps {
  connected: boolean;
  testWheelCommand: (direction: "up" | "down") => Promise<void>;
  checkConnection: () => Promise<void>;
}

export const TestingTab: React.FC<TestingTabProps> = ({
  connected,
  testWheelCommand,
  checkConnection,
}) => {
  return (
    <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
      {/* Wheel Testing */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <MousePointer className="h-5 w-5" />
            Wheel Testing
          </CardTitle>
          <CardDescription>
            Test wheel commands to verify functionality
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-2 gap-3">
            <Button
              onClick={() => testWheelCommand("up")}
              className="h-20 flex-col gap-2"
              variant="outline"
            >
              <ChevronUp className="h-6 w-6" />
              Wheel Up
            </Button>
            <Button
              onClick={() => testWheelCommand("down")}
              className="h-20 flex-col gap-2"
              variant="outline"
            >
              <ChevronDown className="h-6 w-6" />
              Wheel Down
            </Button>
          </div>
          <div className="text-sm text-muted-foreground">
            <p>
              These buttons simulate mouse wheel events with delta values of
              ±120.
            </p>
          </div>
        </CardContent>
      </Card>

      {/* Additional Testing */}
      <Card>
        <CardHeader>
          <CardTitle>Connection Testing</CardTitle>
          <CardDescription>
            Test and monitor the connection to REAPER
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex items-center justify-between p-4 border rounded-lg">
            <div className="flex items-center gap-3">
              {connected ? (
                <div className="h-3 w-3 bg-green-500 rounded-full animate-pulse" />
              ) : (
                <div className="h-3 w-3 bg-red-500 rounded-full" />
              )}
              <div>
                <p className="font-medium">
                  {connected ? "Connected" : "Disconnected"}
                </p>
                <p className="text-sm text-muted-foreground">
                  {connected
                    ? "Plugin is responding"
                    : "Check REAPER and plugin"}
                </p>
              </div>
            </div>
          </div>
          <Button
            onClick={checkConnection}
            className="w-full"
            variant="outline"
          >
            <RefreshCw className="h-4 w-4 mr-2" />
            Test Connection
          </Button>
        </CardContent>
      </Card>
    </div>
  );
};
