/**
 * Code-based TanStack Router tree — one lab route per prototype, per
 * the prototype→port loop (see README.md). File-based routing + the
 * router vite plugin are deliberately skipped; a lab this size doesn't
 * need generated route trees.
 */
import {
  Link,
  Outlet,
  createRootRoute,
  createRoute,
  createRouter,
  redirect,
} from "@tanstack/react-router";
import { FlaskConical } from "lucide-react";

import { ProjectsPage } from "./routes/projects";
import { ProjectDetailPage } from "./routes/project-detail";
import { VOX_URL } from "./lib/vox";
import { Separator } from "./components/ui/separator";
import { TooltipProvider } from "./components/ui/tooltip";

const rootRoute = createRootRoute({
  component: () => (
    <TooltipProvider>
      <div className="min-h-screen flex flex-col">
        <header className="border-b bg-card/50 backdrop-blur-sm sticky top-0 z-10">
          <div className="mx-auto flex max-w-5xl items-center gap-4 px-6 h-12">
            <div className="flex items-center gap-2">
              <FlaskConical className="size-4 text-muted-foreground" />
              <span className="text-sm font-semibold tracking-tight">
                Task <span className="text-muted-foreground font-normal">ui-lab</span>
              </span>
            </div>
            <Separator orientation="vertical" className="h-4" />
            <nav className="flex items-center gap-1 text-sm">
              <Link
                to="/projects"
                className="px-3 py-1.5 rounded-md text-muted-foreground transition-colors hover:text-foreground hover:bg-accent [&.active]:text-foreground [&.active]:bg-accent"
              >
                Projects
              </Link>
            </nav>
            <span className="text-muted-foreground ml-auto font-mono text-[11px] hidden sm:block">
              {VOX_URL}
            </span>
          </div>
        </header>
        <main className="mx-auto w-full max-w-5xl px-6 py-8 flex-1">
          <Outlet />
        </main>
      </div>
    </TooltipProvider>
  ),
});

const indexRoute = createRoute({
  getParentRoute: () => rootRoute,
  path: "/",
  beforeLoad: () => {
    throw redirect({ to: "/projects" });
  },
});

const projectsRoute = createRoute({
  getParentRoute: () => rootRoute,
  path: "/projects",
  component: ProjectsPage,
});

const projectDetailRoute = createRoute({
  getParentRoute: () => rootRoute,
  path: "/projects/$projectId",
  component: ProjectDetailPage,
});

const routeTree = rootRoute.addChildren([
  indexRoute,
  projectsRoute,
  projectDetailRoute,
]);

export function createAppRouter() {
  return createRouter({
    routeTree,
    defaultPreload: "intent",
  });
}
