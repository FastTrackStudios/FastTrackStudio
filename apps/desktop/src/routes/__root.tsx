import { createRootRoute } from '@tanstack/react-router'
import { TanStackRouterDevtools } from '@tanstack/router-devtools'
import { MainApp } from '@/components/MainApp'

export const Route = createRootRoute({
  component: RootComponent,
})

function RootComponent() {
  return (
    <>
      <MainApp />
      {/* Router DevTools - only in development */}
      {process.env.NODE_ENV === 'development' && <TanStackRouterDevtools />}
    </>
  )
}
