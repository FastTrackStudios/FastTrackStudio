import React from "react";
import { Table as TableIcon } from "lucide-react";

import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { KeymapTable } from "./KeymapTable";

interface RawEntriesTabProps {
  keymapName: string;
}

export const RawEntriesTab: React.FC<RawEntriesTabProps> = ({ keymapName }) => {
  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <TableIcon className="h-5 w-5" />
          Raw Keymap Entries
        </CardTitle>
        <CardDescription>
          View all raw entries from the current keymap file (KEY, SCR, ACT)
        </CardDescription>
      </CardHeader>
      <CardContent>
        <KeymapTable keymapName={keymapName} />
      </CardContent>
    </Card>
  );
};
