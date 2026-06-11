/**
 * /projects — lab route: real projects from the home org, fetched over
 * vox-ws with the generated ProjectServiceRpc client. Fetch + render
 * only (title, status, kind), per the prototype→port loop.
 */
import { useState } from "react";
import { useQuery } from "@tanstack/react-query";
import { Link } from "@tanstack/react-router";
import { AlertCircle, RefreshCw, Search } from "lucide-react";

import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import {
  Card,
  CardAction,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Skeleton } from "@/components/ui/skeleton";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { projectListQuery } from "@/lib/queries";
import { VOX_URL } from "@/lib/vox";
import type { ProjectInfo } from "@/generated/projectservicerpc.generated";

function statusVariant(
  status: string,
): "default" | "secondary" | "destructive" | "outline" {
  switch (status) {
    case "in-progress":
    case "active":
      return "default";
    case "blocked":
    case "cancelled":
      return "destructive";
    case "done":
    case "completed":
      return "secondary";
    default:
      return "outline";
  }
}

function ProjectTableRow({ project }: { project: ProjectInfo }) {
  const id = String(project.id);
  return (
    <TableRow className="group cursor-pointer">
      <TableCell className="font-medium">
        <Link
          to="/projects/$projectId"
          params={{ projectId: id }}
          className="block hover:text-primary transition-colors"
        >
          {project.title || project.path}
        </Link>
      </TableCell>
      <TableCell className="text-muted-foreground font-mono text-xs hidden md:table-cell">
        {project.path}
      </TableCell>
      <TableCell>
        <Badge variant="outline" className="font-mono text-[10px]">
          {project.project_type || "project"}
        </Badge>
      </TableCell>
      <TableCell>
        <Badge variant={statusVariant(project.status)}>{project.status}</Badge>
      </TableCell>
    </TableRow>
  );
}

export function ProjectsPage() {
  const query = useQuery(projectListQuery);
  const [search, setSearch] = useState("");

  const filtered = query.data
    ? [...query.data]
        .sort((a, b) => a.title.localeCompare(b.title))
        .filter(
          (p) =>
            !search ||
            p.title.toLowerCase().includes(search.toLowerCase()) ||
            p.path.toLowerCase().includes(search.toLowerCase()),
        )
    : [];

  return (
    <div className="flex flex-col gap-4">
      <div className="flex items-center justify-between gap-4">
        <div>
          <h1 className="text-xl font-semibold tracking-tight">Projects</h1>
          <p className="text-muted-foreground text-sm mt-0.5">
            Live from{" "}
            <span className="font-mono">{VOX_URL}</span> via the generated
            ProjectServiceRpc client
          </p>
        </div>
        <Button
          variant="outline"
          size="sm"
          onClick={() => void query.refetch()}
          disabled={query.isFetching}
          aria-label="Refresh"
          className="gap-1.5"
        >
          <RefreshCw className={query.isFetching ? "animate-spin" : ""} />
          Refresh
        </Button>
      </div>

      <Card className="overflow-hidden py-0">
        <CardHeader className="px-4 py-3 border-b gap-0">
          <div className="flex items-center gap-3">
            <div className="relative flex-1 max-w-sm">
              <Search className="absolute left-2.5 top-1/2 -translate-y-1/2 size-3.5 text-muted-foreground pointer-events-none" />
              <Input
                placeholder="Filter projects…"
                value={search}
                onChange={(e) => setSearch(e.target.value)}
                className="pl-8 h-8 text-sm"
                disabled={query.isPending || query.isError}
              />
            </div>
            {query.isSuccess && (
              <span className="text-muted-foreground text-xs tabular-nums">
                {filtered.length} / {query.data.length}
              </span>
            )}
          </div>
        </CardHeader>
        <CardContent className="p-0">
          {query.isPending ? (
            <div className="flex flex-col">
              {Array.from({ length: 6 }, (_, i) => (
                <div key={i} className="flex items-center gap-4 px-4 py-3 border-b last:border-0">
                  <Skeleton className="h-4 flex-1 max-w-48" />
                  <Skeleton className="h-4 w-24 hidden md:block" />
                  <Skeleton className="h-5 w-16" />
                  <Skeleton className="h-5 w-20" />
                </div>
              ))}
            </div>
          ) : query.isError ? (
            <div className="border-destructive/30 text-destructive flex items-start gap-3 m-4 rounded-lg border bg-destructive/5 px-4 py-3 text-sm">
              <AlertCircle className="mt-0.5 size-4 shrink-0" />
              <div className="flex flex-col gap-2">
                <p className="font-medium">Couldn&apos;t reach the task-server</p>
                <p className="text-muted-foreground">
                  {query.error instanceof Error
                    ? query.error.message
                    : String(query.error)}
                </p>
                <p className="text-muted-foreground">
                  Is it running?{" "}
                  <span className="font-mono">{VOX_URL}</span>
                </p>
                <Button
                  variant="outline"
                  size="sm"
                  className="w-fit"
                  onClick={() => void query.refetch()}
                >
                  Retry
                </Button>
              </div>
            </div>
          ) : filtered.length === 0 ? (
            <p className="text-muted-foreground px-4 py-12 text-center text-sm">
              {search ? "No projects match the filter." : "No projects in this org yet."}
            </p>
          ) : (
            <Table>
              <TableHeader>
                <TableRow className="hover:bg-transparent">
                  <TableHead>Title</TableHead>
                  <TableHead className="hidden md:table-cell">Path</TableHead>
                  <TableHead>Type</TableHead>
                  <TableHead>Status</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {filtered.map((project) => (
                  <ProjectTableRow
                    key={String(project.id)}
                    project={project}
                  />
                ))}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
