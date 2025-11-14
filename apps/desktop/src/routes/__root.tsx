import { createRootRoute } from '@tanstack/react-router'
import { TanStackRouterDevtools } from '@tanstack/router-devtools'
import { Ableset } from '@/components/daw/performance/themes/ableset/Ableset'

export const Route = createRootRoute({
  component: RootComponent,
})

function RootComponent() {
  return (
    <div className="h-screen w-screen ableset-theme overflow-hidden">
      <Ableset />
      {/* Router DevTools - only in development */}
      {process.env.NODE_ENV === 'development' && <TanStackRouterDevtools />}
    </div>
  )
}
