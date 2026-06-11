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

import { ProjectsPage } from "./routes/projects";
import { ProjectDetailPage } from "./routes/project-detail";
import { VOX_URL } from "./lib/vox";

const rootRoute = createRootRoute({
  component: () => (
    <div className="min-h-screen">
      <header className="border-b">
        <div className="mx-auto flex max-w-5xl items-center gap-6 px-6 py-3">
          <span className="text-sm font-semibold tracking-tight">
            Task <span className="text-muted-foreground">ui-lab</span>
          </span>
          <nav className="flex items-center gap-4 text-sm">
            <Link
              to="/projects"
              className="text-muted-foreground transition-colors hover:text-foreground [&.active]:text-foreground"
            >
              Projects
            </Link>
          </nav>
          <span className="text-muted-foreground ml-auto font-mono text-xs">
            {VOX_URL}
          </span>
        </div>
      </header>
      <main className="mx-auto max-w-5xl px-6 py-8">
        <Outlet />
      </main>
    </div>
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
