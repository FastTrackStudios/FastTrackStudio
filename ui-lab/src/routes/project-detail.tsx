/**
 * /projects/$org/$projectId — one project + its tasks. Canvas for the
 * project-detail redesign prototype (separate issue).
 */
import { useQuery } from "@tanstack/react-query";
import { Link, useParams } from "@tanstack/react-router";
import { AlertCircle, ArrowLeft, CalendarClock, Info } from "lucide-react";

import { Avatar, AvatarFallback } from "@/components/ui/avatar";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Progress } from "@/components/ui/progress";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Separator } from "@/components/ui/separator";
import { Skeleton } from "@/components/ui/skeleton";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "@/components/ui/tooltip";
import { projectQuery, projectTasksQuery } from "@/lib/queries";
import { dump } from "@/lib/utils";

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

function initials(name: string): string {
  return name
    .split(/[\s._-]/)
    .filter(Boolean)
    .slice(0, 2)
    .map((s) => s[0]?.toUpperCase() ?? "")
    .join("");
}

function ErrorNote({ error }: { error: unknown }) {
  return (
    <div className="border-destructive/30 text-destructive bg-destructive/5 flex items-center gap-2 rounded-lg border px-4 py-3 text-sm">
      <AlertCircle className="size-4 shrink-0" />
      {error instanceof Error ? error.message : String(error)}
    </div>
  );
}

export function ProjectDetailPage() {
  const { org, projectId } = useParams({
    from: "/projects/$org/$projectId",
  });
  const project = useQuery(projectQuery(org, projectId));
  const tasks = useQuery(projectTasksQuery(org, projectId));

  const pct = project.data
    ? Number(project.data.progress_percent ?? 0)
    : 0;

  return (
    <div className="flex flex-col gap-6">
      <div className="flex items-center gap-2">
        <Button asChild variant="ghost" size="sm" className="-ml-2">
          <Link to="/projects">
            <ArrowLeft className="size-3.5" /> All projects
          </Link>
        </Button>
      </div>

      {project.isPending ? (
        <Card>
          <CardHeader>
            <Skeleton className="h-6 w-64" />
            <Skeleton className="h-4 w-48" />
          </CardHeader>
          <CardContent>
            <Skeleton className="h-2 w-full" />
          </CardContent>
        </Card>
      ) : project.isError ? (
        <ErrorNote error={project.error} />
      ) : (
        <Card>
          <CardHeader>
            <div className="flex items-start justify-between gap-4">
              <div className="flex flex-col gap-1 min-w-0">
                <CardTitle className="text-xl">{project.data.title}</CardTitle>
                <CardDescription className="font-mono text-xs">
                  {project.data.path} · {String(project.data.id)}
                </CardDescription>
              </div>
              <Badge
                variant={statusVariant(project.data.status)}
                className="shrink-0 mt-0.5"
              >
                {project.data.status}
              </Badge>
            </div>
          </CardHeader>
          <CardContent className="flex flex-col gap-4">
            <div className="flex flex-wrap items-center gap-2">
              <Badge variant="outline">
                {project.data.project_type || "project"}
              </Badge>
              <Badge variant="secondary">{project.data.priority}</Badge>
              {project.data.lead && (
                <div className="flex items-center gap-1.5">
                  <Avatar className="size-5">
                    <AvatarFallback className="text-[9px]">
                      {initials(project.data.lead)}
                    </AvatarFallback>
                  </Avatar>
                  <span className="text-muted-foreground text-xs">
                    {project.data.lead}
                  </span>
                </div>
              )}
            </div>
            <Separator />
            <div className="flex flex-col gap-1.5">
              <div className="flex items-center justify-between text-xs">
                <span className="text-muted-foreground flex items-center gap-1">
                  Progress
                  <Tooltip>
                    <TooltipTrigger asChild>
                      <Info className="size-3 cursor-default" />
                    </TooltipTrigger>
                    <TooltipContent>Tasks completed / total</TooltipContent>
                  </Tooltip>
                </span>
                <span className="font-medium tabular-nums">{pct}%</span>
              </div>
              <Progress value={pct} className="h-1.5" />
            </div>
          </CardContent>
        </Card>
      )}

      <Tabs defaultValue="tasks">
        <TabsList className="w-full justify-start">
          <TabsTrigger value="tasks">
            Tasks{tasks.isSuccess ? ` (${tasks.data.length})` : ""}
          </TabsTrigger>
          <TabsTrigger value="raw">Raw JSON</TabsTrigger>
        </TabsList>

        <TabsContent value="tasks" className="mt-3">
          {tasks.isPending ? (
            <div className="flex flex-col gap-2">
              {Array.from({ length: 4 }, (_, i) => (
                <Skeleton key={i} className="h-[52px] w-full" />
              ))}
            </div>
          ) : tasks.isError ? (
            <ErrorNote error={tasks.error} />
          ) : tasks.data.length === 0 ? (
            <p className="text-muted-foreground py-10 text-center text-sm">
              No tasks point at this project (matched on{" "}
              <span className="font-mono">project_id</span>).
            </p>
          ) : (
            <div className="flex flex-col rounded-lg border overflow-hidden">
              {tasks.data.map((task, i) => (
                <div key={String(task.id)}>
                  {i > 0 && <Separator />}
                  <div className="flex items-center gap-3 px-4 py-3">
                    <div className="min-w-0 flex-1">
                      <p className="truncate text-sm font-medium">
                        {task.title || task.path}
                      </p>
                      {task.due && (
                        <p className="text-muted-foreground flex items-center gap-1 mt-0.5 text-xs">
                          <CalendarClock className="size-3" />
                          {task.due}
                        </p>
                      )}
                    </div>
                    <div className="flex items-center gap-1.5 shrink-0">
                      <Badge variant="outline" className="text-[10px] font-mono">
                        {task.priority}
                      </Badge>
                      <Badge variant={statusVariant(task.status)}>
                        {task.status}
                      </Badge>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          )}
        </TabsContent>

        <TabsContent value="raw" className="mt-3">
          <ScrollArea className="h-[32rem] rounded-lg border">
            <pre className="bg-card text-muted-foreground p-4 text-xs leading-relaxed">
              {dump({
                project: project.data ?? null,
                tasks: tasks.data ?? null,
              })}
            </pre>
          </ScrollArea>
        </TabsContent>
      </Tabs>
    </div>
  );
}
