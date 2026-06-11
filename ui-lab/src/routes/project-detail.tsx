/**
 * /projects/$projectId — stub route: one project + its tasks, dumped
 * minimally. This is the canvas the project-detail redesign prototype
 * (separate issue) builds on; no design work here beyond clean
 * defaults.
 */
import { useQuery } from "@tanstack/react-query";
import { Link, useParams } from "@tanstack/react-router";
import { AlertCircle, ArrowLeft } from "lucide-react";

import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Skeleton } from "@/components/ui/skeleton";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { projectQuery, projectTasksQuery } from "@/lib/queries";
import { dump } from "@/lib/utils";

function ErrorNote({ error }: { error: unknown }) {
  return (
    <div className="border-destructive/50 text-destructive flex items-center gap-2 rounded-lg border px-4 py-3 text-sm">
      <AlertCircle className="size-4 shrink-0" />
      {error instanceof Error ? error.message : String(error)}
    </div>
  );
}

export function ProjectDetailPage() {
  const { projectId } = useParams({ from: "/projects/$projectId" });
  const project = useQuery(projectQuery(projectId));
  const tasks = useQuery(projectTasksQuery(projectId));

  return (
    <div className="flex flex-col gap-6">
      <div>
        <Button asChild variant="ghost" size="sm">
          <Link to="/projects">
            <ArrowLeft /> All projects
          </Link>
        </Button>
      </div>

      {project.isPending ? (
        <Skeleton className="h-40 w-full" />
      ) : project.isError ? (
        <ErrorNote error={project.error} />
      ) : (
        <Card>
          <CardHeader>
            <CardTitle className="text-xl">{project.data.title}</CardTitle>
            <CardDescription className="font-mono text-xs">
              {project.data.path} · {String(project.data.id)}
            </CardDescription>
          </CardHeader>
          <CardContent className="flex flex-wrap items-center gap-2">
            <Badge>{project.data.status}</Badge>
            <Badge variant="outline">
              {project.data.project_type || "project"}
            </Badge>
            <Badge variant="secondary">{project.data.priority}</Badge>
            {project.data.lead && (
              <Badge variant="outline">lead: {project.data.lead}</Badge>
            )}
            <span className="text-muted-foreground ml-auto text-xs">
              {project.data.progress_percent}% complete
            </span>
          </CardContent>
        </Card>
      )}

      <Tabs defaultValue="tasks">
        <TabsList>
          <TabsTrigger value="tasks">
            Tasks{tasks.isSuccess ? ` (${tasks.data.length})` : ""}
          </TabsTrigger>
          <TabsTrigger value="raw">Raw</TabsTrigger>
        </TabsList>

        <TabsContent value="tasks" className="flex flex-col gap-2">
          {tasks.isPending ? (
            Array.from({ length: 4 }, (_, i) => (
              <Skeleton key={i} className="h-[42px] w-full" />
            ))
          ) : tasks.isError ? (
            <ErrorNote error={tasks.error} />
          ) : tasks.data.length === 0 ? (
            <p className="text-muted-foreground py-6 text-center text-sm">
              No tasks point at this project (matched on{" "}
              <span className="font-mono">project_id</span>).
            </p>
          ) : (
            tasks.data.map((task) => (
              <div
                key={String(task.id)}
                className="flex items-center gap-3 rounded-lg border px-4 py-2.5"
              >
                <span className="min-w-0 flex-1 truncate text-sm">
                  {task.title || task.path}
                </span>
                {task.due && (
                  <span className="text-muted-foreground font-mono text-xs">
                    due {task.due}
                  </span>
                )}
                <Badge variant="outline">{task.priority}</Badge>
                <Badge
                  variant={task.status === "done" ? "secondary" : "default"}
                >
                  {task.status}
                </Badge>
              </div>
            ))
          )}
        </TabsContent>

        <TabsContent value="raw">
          <pre className="bg-card text-muted-foreground max-h-[32rem] overflow-auto rounded-lg border p-4 text-xs leading-relaxed">
            {dump({
              project: project.data ?? null,
              tasks: tasks.data ?? null,
            })}
          </pre>
        </TabsContent>
      </Tabs>
    </div>
  );
}
