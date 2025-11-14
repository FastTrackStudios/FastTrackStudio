"use client";

import * as React from "react";
import {
  ColumnDef,
  ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from "@tanstack/react-table";
import {
  ArrowUpDown,
  ChevronDown,
  MoreHorizontal,
  Copy,
  Eye,
  Settings,
} from "lucide-react";

import { Button } from "@/components/ui/button";
import { Checkbox } from "@/components/ui/checkbox";
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { Input } from "@/components/ui/input";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Badge } from "@/components/ui/badge";

// Types for keymap entries
export type KeymapEntry = {
  id: string;
  entry_type: "KEY" | "SCR" | "ACT";
  raw_line: string;
  command_id: string;
  section: string;
  section_code: number;
  description?: string;
  key_input?: string;
  modifiers?: string;
  path?: string;
  action_ids?: string[];
  termination_behavior?: number;
  action_flags?: number;
};

// Transform raw API data to table format
function transformRawEntry(entry: any, index: number): KeymapEntry {
  const baseEntry = {
    id: `${entry.entry_type}-${index}`,
    entry_type: entry.entry_type,
    raw_line: entry.raw_line,
  };

  switch (entry.entry_type) {
    case "KEY":
      return {
        ...baseEntry,
        command_id: entry.details.command_id,
        section: entry.details.section,
        section_code: entry.details.section_code,
        key_input: entry.details.key_input,
        modifiers: entry.details.modifiers,
      };
    case "SCR":
      return {
        ...baseEntry,
        command_id: entry.details.command_id,
        section: entry.details.section,
        section_code: entry.details.section_code,
        description: entry.details.description,
        path: entry.details.path,
        termination_behavior: entry.details.termination_behavior,
      };
    case "ACT":
      return {
        ...baseEntry,
        command_id: entry.details.command_id,
        section: entry.details.section,
        section_code: entry.details.section_code,
        description: entry.details.description,
        action_ids: entry.details.action_ids,
        action_flags: entry.details.action_flags,
      };
    default:
      return baseEntry as KeymapEntry;
  }
}

export const columns: ColumnDef<KeymapEntry>[] = [
  {
    id: "select",
    header: ({ table }) => (
      <Checkbox
        checked={
          table.getIsAllPageRowsSelected() ||
          (table.getIsSomePageRowsSelected() && "indeterminate")
        }
        onCheckedChange={(value) => table.toggleAllPageRowsSelected(!!value)}
        aria-label="Select all"
      />
    ),
    cell: ({ row }) => (
      <Checkbox
        checked={row.getIsSelected()}
        onCheckedChange={(value) => row.toggleSelected(!!value)}
        aria-label="Select row"
      />
    ),
    enableSorting: false,
    enableHiding: false,
  },
  {
    accessorKey: "entry_type",
    header: "Type",
    cell: ({ row }) => {
      const type = row.getValue("entry_type") as string;
      const variant =
        type === "KEY" ? "default" : type === "SCR" ? "secondary" : "outline";
      return <Badge variant={variant}>{type}</Badge>;
    },
  },
  {
    accessorKey: "command_id",
    header: ({ column }) => {
      return (
        <Button
          variant="ghost"
          onClick={() => column.toggleSorting(column.getIsSorted() === "asc")}
        >
          Command ID
          <ArrowUpDown className="ml-2 h-4 w-4" />
        </Button>
      );
    },
    cell: ({ row }) => (
      <div className="font-mono text-sm">{row.getValue("command_id")}</div>
    ),
  },
  {
    accessorKey: "section",
    header: ({ column }) => {
      return (
        <Button
          variant="ghost"
          onClick={() => column.toggleSorting(column.getIsSorted() === "asc")}
        >
          Section
          <ArrowUpDown className="ml-2 h-4 w-4" />
        </Button>
      );
    },
    cell: ({ row }) => (
      <div className="capitalize">{row.getValue("section")}</div>
    ),
  },
  {
    accessorKey: "key_input",
    header: "Key/Input",
    cell: ({ row }) => {
      const keyInput = row.getValue("key_input") as string;
      const modifiers = row.original.modifiers;
      if (!keyInput) return <div className="text-muted-foreground">-</div>;

      return (
        <div className="space-y-1">
          <div className="font-mono text-sm">{keyInput}</div>
          {modifiers && modifiers !== "empty()" && (
            <div className="text-xs text-muted-foreground">{modifiers}</div>
          )}
        </div>
      );
    },
  },
  {
    accessorKey: "description",
    header: "Description",
    cell: ({ row }) => {
      const description = row.getValue("description") as string;
      const path = row.original.path;

      if (description) {
        return (
          <div className="max-w-[300px]">
            <div className="truncate">{description}</div>
            {path && (
              <div className="text-xs text-muted-foreground truncate">
                {path}
              </div>
            )}
          </div>
        );
      }

      if (path) {
        return (
          <div className="max-w-[300px] text-xs text-muted-foreground truncate">
            {path}
          </div>
        );
      }

      return <div className="text-muted-foreground">-</div>;
    },
  },
  {
    accessorKey: "raw_line",
    header: "Raw Entry",
    cell: ({ row }) => (
      <div className="font-mono text-xs max-w-[400px] truncate bg-muted p-1 rounded">
        {row.getValue("raw_line")}
      </div>
    ),
  },
  {
    id: "actions",
    enableHiding: false,
    cell: ({ row }) => {
      const entry = row.original;

      return (
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="ghost" className="h-8 w-8 p-0">
              <span className="sr-only">Open menu</span>
              <MoreHorizontal className="h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            <DropdownMenuLabel>Actions</DropdownMenuLabel>
            <DropdownMenuItem
              onClick={() => navigator.clipboard.writeText(entry.command_id)}
            >
              <Copy className="mr-2 h-4 w-4" />
              Copy Command ID
            </DropdownMenuItem>
            <DropdownMenuItem
              onClick={() => navigator.clipboard.writeText(entry.raw_line)}
            >
              <Copy className="mr-2 h-4 w-4" />
              Copy Raw Line
            </DropdownMenuItem>
            <DropdownMenuSeparator />
            <DropdownMenuItem>
              <Eye className="mr-2 h-4 w-4" />
              View Details
            </DropdownMenuItem>
            {entry.entry_type === "KEY" && (
              <DropdownMenuItem>
                <Settings className="mr-2 h-4 w-4" />
                Test Key Command
              </DropdownMenuItem>
            )}
          </DropdownMenuContent>
        </DropdownMenu>
      );
    },
  },
];

interface KeymapTableProps {
  keymapName?: string;
}

export function KeymapTable({ keymapName = "Default" }: KeymapTableProps) {
  const [data, setData] = React.useState<KeymapEntry[]>([]);
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState<string | null>(null);
  const [sorting, setSorting] = React.useState<SortingState>([]);
  const [columnFilters, setColumnFilters] = React.useState<ColumnFiltersState>(
    []
  );
  const [columnVisibility, setColumnVisibility] =
    React.useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = React.useState({});

  // Load keymap data
  React.useEffect(() => {
    const loadKeymapData = async () => {
      setLoading(true);
      setError(null);

      try {
        // Use the raw keymap endpoint
        const response = await fetch(
          `http://127.0.0.1:3001/api/keymaps/raw/${keymapName}`
        );
        const result = await response.json();

        if (result.success && result.data) {
          const transformedData = result.data.entries.map(transformRawEntry);
          setData(transformedData);
        } else {
          setError(result.error || "Failed to load keymap data");
        }
      } catch (err) {
        setError("Failed to connect to REAPER plugin");
        console.error("Failed to load keymap:", err);
      } finally {
        setLoading(false);
      }
    };

    loadKeymapData();
  }, [keymapName]);

  const table = useReactTable({
    data,
    columns,
    onSortingChange: setSorting,
    onColumnFiltersChange: setColumnFilters,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnFilters,
      columnVisibility,
      rowSelection,
    },
  });

  if (loading) {
    return (
      <div className="w-full h-64 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-4"></div>
          <p>Loading keymap entries...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="w-full h-64 flex items-center justify-center">
        <div className="text-center text-destructive">
          <p className="font-semibold">Error loading keymap</p>
          <p className="text-sm">{error}</p>
        </div>
      </div>
    );
  }

  return (
    <div className="w-full">
      <div className="flex items-center py-4 space-x-4">
        <Input
          placeholder="Filter by command ID..."
          value={
            (table.getColumn("command_id")?.getFilterValue() as string) ?? ""
          }
          onChange={(event) =>
            table.getColumn("command_id")?.setFilterValue(event.target.value)
          }
          className="max-w-sm"
        />
        <Input
          placeholder="Filter by section..."
          value={(table.getColumn("section")?.getFilterValue() as string) ?? ""}
          onChange={(event) =>
            table.getColumn("section")?.setFilterValue(event.target.value)
          }
          className="max-w-sm"
        />
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline" className="ml-auto">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id.replace("_", " ")}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>

      <div className="rounded-md border">
        <Table>
          <TableHeader>
            {table.getHeaderGroups().map((headerGroup) => (
              <TableRow key={headerGroup.id}>
                {headerGroup.headers.map((header) => {
                  return (
                    <TableHead key={header.id}>
                      {header.isPlaceholder
                        ? null
                        : flexRender(
                            header.column.columnDef.header,
                            header.getContext()
                          )}
                    </TableHead>
                  );
                })}
              </TableRow>
            ))}
          </TableHeader>
          <TableBody>
            {table.getRowModel().rows?.length ? (
              table.getRowModel().rows.map((row) => (
                <TableRow
                  key={row.id}
                  data-state={row.getIsSelected() && "selected"}
                >
                  {row.getVisibleCells().map((cell) => (
                    <TableCell key={cell.id}>
                      {flexRender(
                        cell.column.columnDef.cell,
                        cell.getContext()
                      )}
                    </TableCell>
                  ))}
                </TableRow>
              ))
            ) : (
              <TableRow>
                <TableCell
                  colSpan={columns.length}
                  className="h-24 text-center"
                >
                  No keymap entries found.
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>

      <div className="flex items-center justify-between space-x-2 py-4">
        <div className="flex items-center space-x-2">
          <div className="text-sm text-muted-foreground">
            {table.getFilteredSelectedRowModel().rows.length} of{" "}
            {table.getFilteredRowModel().rows.length} row(s) selected.
          </div>
          <div className="text-sm text-muted-foreground">
            Total entries: {data.length}
          </div>
        </div>
        <div className="space-x-2">
          <Button
            variant="outline"
            size="sm"
            onClick={() => table.previousPage()}
            disabled={!table.getCanPreviousPage()}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            size="sm"
            onClick={() => table.nextPage()}
            disabled={!table.getCanNextPage()}
          >
            Next
          </Button>
        </div>
      </div>
    </div>
  );
}
