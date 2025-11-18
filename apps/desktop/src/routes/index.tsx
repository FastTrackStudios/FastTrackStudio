import { createFileRoute } from '@tanstack/react-router'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';

export const Route = createFileRoute('/')({
  component: HomePage,
})

function HomePage() {
  return (
    <div className="container mx-auto p-8">
      <div className="max-w-4xl mx-auto space-y-8">
        <div className="text-center space-y-4">
          <h1 className="text-4xl font-bold tracking-tight">
            FastTrackStudio
          </h1>
          <p className="text-xl text-muted-foreground">
            Web UI - Network Broadcast
          </p>
        </div>

        <div className="grid gap-6 md:grid-cols-2">
          <Card>
            <CardHeader>
              <CardTitle>Status</CardTitle>
              <CardDescription>Application Status</CardDescription>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-muted-foreground">
                Web server is running and broadcasting the UI on port 8080.
              </p>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Network Access</CardTitle>
              <CardDescription>Access from any device</CardDescription>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-muted-foreground">
                Connect to this application from any device on your network using:
              </p>
              <code className="mt-2 block p-2 bg-muted rounded text-sm">
                http://&lt;your-ip&gt;:8080
              </code>
            </CardContent>
          </Card>
        </div>

        <Card>
          <CardHeader>
            <CardTitle>Welcome</CardTitle>
            <CardDescription>FastTrackStudio Web Interface</CardDescription>
          </CardHeader>
          <CardContent>
            <p className="text-sm text-muted-foreground">
              This is a simple web UI that can be accessed from any device on your network.
              The Tauri desktop application is serving this interface via an embedded Axum web server.
            </p>
          </CardContent>
        </Card>
      </div>
    </div>
  )
}
