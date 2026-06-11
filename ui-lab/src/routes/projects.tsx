/**
 * /projects — lab route: real projects from the home org, fetched over
 * vox-ws with the generated ProjectServiceRpc client. Fetch + render
 * only (title, status, kind), per the prototype→port loop.
 */
import { useQuery } from "@tanstack/react-query";
import { Link } from "@tanstack/react-router";
import { AlertCircle, RefreshCw } from "lucide-react";

import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import {
  Card,
  CardAction,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Skeleton } from "@/components/ui/skeleton";
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

function ProjectRow({ project }: { project: ProjectInfo }) {
  const id = String(project.id);
  return (
    <Link
      to="/projects/$projectId"
      params={{ projectId: id }}
      className="hover:bg-accent/50 flex items-center gap-3 rounded-lg border px-4 py-3 transition-colors"
    >
      <span className="min-w-0 flex-1 truncate text-sm font-medium">
        {project.title || project.path}
      </span>
      <Badge variant="outline" className="font-mono text-[10px]">
        {project.project_type || "project"}
      </Badge>
      <Badge variant={statusVariant(project.status)}>{project.status}</Badge>
    </Link>
  );
}

export function ProjectsPage() {
  const query = useQuery(projectListQuery);

  return (
    <Card>
      <CardHeader>
        <CardTitle>Projects</CardTitle>
        <CardDescription>
          Live from <span className="font-mono">{VOX_URL}</span> via the
          generated ProjectServiceRpc client
        </CardDescription>
        <CardAction>
          <Button
            variant="ghost"
            size="icon"
            onClick={() => void query.refetch()}
            disabled={query.isFetching}
            aria-label="Refresh"
          >
            <RefreshCw className={query.isFetching ? "animate-spin" : ""} />
          </Button>
        </CardAction>
      </CardHeader>
      <CardContent className="flex flex-col gap-2">
        {query.isPending ? (
          Array.from({ length: 6 }, (_, i) => (
            <Skeleton key={i} className="h-[46px] w-full" />
          ))
        ) : query.isError ? (
          <div className="border-destructive/50 text-destructive flex items-start gap-3 rounded-lg border px-4 py-3 text-sm">
            <AlertCircle className="mt-0.5 size-4 shrink-0" />
            <div className="flex flex-col gap-2">
              <p className="font-medium">Couldn&apos;t reach the task-server</p>
              <p className="text-muted-foreground">
                {query.error instanceof Error
                  ? query.error.message
                  : String(query.error)}
              </p>
              <p className="text-muted-foreground">
                Is it running? <span className="font-mono">{VOX_URL}</span>
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
        ) : query.data.length === 0 ? (
          <p className="text-muted-foreground px-1 py-6 text-center text-sm">
            No projects in this org yet.
          </p>
        ) : (
          [...query.data]
            .sort((a, b) => a.title.localeCompare(b.title))
            .map((project) => (
              <ProjectRow key={String(project.id)} project={project} />
            ))
        )}
      </CardContent>
    </Card>
  );
}
