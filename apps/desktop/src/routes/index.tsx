import { createFileRoute } from '@tanstack/react-router'
import { useState, useEffect, useCallback } from 'react';
import { createTauRPCProxy, type AppStatus, type User, type TaskProgress, type ApiError } from '../../bindings';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Progress } from '@/components/ui/progress';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { toast } from 'sonner';

export const Route = createFileRoute('/')({
  component: TauRPCDemo,
})

function TauRPCDemo() {
  const [taurpc] = useState(() => createTauRPCProxy());

  // State management
  const [status, setStatus] = useState<AppStatus | null>(null);
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Form states
  const [counterValue, setCounterValue] = useState<string>('');
  const [userName, setUserName] = useState('');
  const [userEmail, setUserEmail] = useState('');
  const [taskName, setTaskName] = useState('');
  const [taskDuration, setTaskDuration] = useState<string>('5');
  const [batchSize, setBatchSize] = useState<string>('10');

  // Progress tracking
  const [taskProgress, setTaskProgress] = useState<TaskProgress | null>(null);
  const [isRunningTask, setIsRunningTask] = useState(false);

  // Load initial data
  const loadStatus = useCallback(async () => {
    try {
      const result = await taurpc.get_status();
      setStatus(result);
      setError(null);
    } catch (err: any) {
      setError(`Failed to load status: ${err.message || err}`);
    }
  }, [taurpc]);

  const loadUsers = useCallback(async () => {
    try {
      const result = await taurpc.users.get_all_users();
      setUsers(result);
    } catch (err: any) {
      setError(`Failed to load users: ${err.message || err}`);
    }
  }, [taurpc]);

  useEffect(() => {
    loadStatus();
    loadUsers();
  }, [loadStatus, loadUsers]);

  // Event listeners
  useEffect(() => {
    let unlistenStatus: (() => void) | undefined;
    let unlistenError: (() => void) | undefined;

    const setupEventListeners = async () => {
      try {
        // Listen for status changes
        unlistenStatus = await taurpc.status_changed.on((newStatus: AppStatus) => {
          setStatus(newStatus);
          toast.info('Status updated from backend');
        });

        // Listen for errors
        unlistenError = await taurpc.error_occurred.on((error: ApiError) => {
          toast.error(`Backend Error: ${error.message}`);
          setError(`Error ${error.code}: ${error.message}`);
        });
      } catch (err) {
        console.error('Failed to setup event listeners:', err);
      }
    };

    setupEventListeners().catch(console.error);

    return () => {
      if (unlistenStatus) unlistenStatus();
      if (unlistenError) unlistenError();
    };
  }, [taurpc]);

  // Counter operations
  const incrementCounter = async () => {
    try {
      setLoading(true);
      const newValue = await taurpc.counter.increment();
      setStatus(prev => prev ? { ...prev, counter_value: newValue } : null);
      toast.success(`Counter incremented to ${newValue}`);
    } catch (err: any) {
      toast.error(`Failed to increment: ${err.message || err}`);
    } finally {
      setLoading(false);
    }
  };

  const decrementCounter = async () => {
    try {
      setLoading(true);
      const newValue = await taurpc.counter.decrement();
      setStatus(prev => prev ? { ...prev, counter_value: newValue } : null);
      toast.success(`Counter decremented to ${newValue}`);
    } catch (err: any) {
      toast.error(`Failed to decrement: ${err.message || err}`);
    } finally {
      setLoading(false);
    }
  };

  const setCounter = async () => {
    try {
      setLoading(true);
      const value = parseInt(counterValue);
      if (isNaN(value)) {
        toast.error('Please enter a valid number');
        return;
      }
      const newValue = await taurpc.counter.set_value(value);
      setStatus(prev => prev ? { ...prev, counter_value: newValue } : null);
      toast.success(`Counter set to ${newValue}`);
      setCounterValue('');
    } catch (err: any) {
      toast.error(`Failed to set counter: ${err.message || err}`);
    } finally {
      setLoading(false);
    }
  };

  const resetCounter = async () => {
    try {
      setLoading(true);
      const newValue = await taurpc.counter.reset();
      setStatus(prev => prev ? { ...prev, counter_value: newValue } : null);
      toast.success('Counter reset to 0');
    } catch (err: any) {
      toast.error(`Failed to reset counter: ${err.message || err}`);
    } finally {
      setLoading(false);
    }
  };

  // User operations
  const createUser = async () => {
    if (!userName.trim() || !userEmail.trim()) {
      toast.error('Please fill in both name and email');
      return;
    }

    try {
      setLoading(true);
      const newUser = await taurpc.users.create_user(userName.trim(), userEmail.trim());
      setUsers(prev => [...prev, newUser]);
      setUserName('');
      setUserEmail('');
      toast.success(`User ${newUser.name} created successfully`);
      await loadStatus(); // Refresh status to see updated user count
    } catch (err: any) {
      toast.error(`Failed to create user: ${err.message || err}`);
    } finally {
      setLoading(false);
    }
  };

  const toggleUserStatus = async (userId: number) => {
    try {
      const updatedUser = await taurpc.users.toggle_user_status(userId);
      setUsers(prev => prev.map(user => user.id === userId ? updatedUser : user));
      toast.success(`User status toggled`);
    } catch (err: any) {
      toast.error(`Failed to toggle user status: ${err.message || err}`);
    }
  };

  // Task operations
  const runLongTask = async () => {
    if (!taskName.trim()) {
      toast.error('Please enter a task name');
      return;
    }

    const duration = parseInt(taskDuration);
    if (isNaN(duration) || duration < 1 || duration > 60) {
      toast.error('Duration must be between 1 and 60 seconds');
      return;
    }

    try {
      setIsRunningTask(true);
      setTaskProgress(null);

      const result = await taurpc.tasks.run_long_task(
        taskName,
        duration,
        (progress: TaskProgress) => {
          setTaskProgress(progress);
        }
      );

      toast.success(`Task completed in ${result.duration_ms}ms`);
      setTaskName('');
    } catch (err: any) {
      toast.error(`Task failed: ${err.message || err}`);
    } finally {
      setIsRunningTask(false);
      setTaskProgress(null);
    }
  };

  const runBatchProcessing = async () => {
    const size = parseInt(batchSize);
    if (isNaN(size) || size < 1 || size > 1000) {
      toast.error('Batch size must be between 1 and 1000');
      return;
    }

    try {
      setIsRunningTask(true);
      setTaskProgress(null);

      const result = await taurpc.tasks.simulate_batch_processing(
        size,
        (progress: TaskProgress) => {
          setTaskProgress(progress);
        }
      );

      toast.success(`Batch processing completed in ${result.duration_ms}ms`);
    } catch (err: any) {
      toast.error(`Batch processing failed: ${err.message || err}`);
    } finally {
      setIsRunningTask(false);
      setTaskProgress(null);
    }
  };

  // App operations
  const resetApp = async () => {
    try {
      setLoading(true);
      const message = await taurpc.reset_app();
      toast.success(message);
      await loadStatus();
      await loadUsers();
    } catch (err: any) {
      toast.error(`Failed to reset app: ${err.message || err}`);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="container mx-auto p-6 space-y-6">
      <div className="text-center">
        <h1 className="text-3xl font-bold mb-2">TauRPC Advanced Demo</h1>
        <p className="text-muted-foreground">
          Comprehensive demonstration of TauRPC features including shared state, routing, events, and channels
        </p>
      </div>

      {error && (
        <Alert variant="destructive">
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      {/* Status Overview */}
      <Card>
        <CardHeader>
          <CardTitle>Application Status</CardTitle>
          <CardDescription>Real-time status from the backend (updates every 30 seconds)</CardDescription>
        </CardHeader>
        <CardContent>
          {status ? (
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="text-center">
                <div className="text-2xl font-bold">{status.counter_value}</div>
                <div className="text-sm text-muted-foreground">Counter Value</div>
              </div>
              <div className="text-center">
                <div className="text-2xl font-bold">{status.user_count}</div>
                <div className="text-sm text-muted-foreground">Users Created</div>
              </div>
              <div className="text-center">
                <Badge variant={status.is_processing ? "destructive" : "secondary"}>
                  {status.is_processing ? "Processing" : "Idle"}
                </Badge>
                <div className="text-sm text-muted-foreground">Status</div>
              </div>
              <div className="text-center">
                <div className="text-sm font-medium truncate">{status.last_message}</div>
                <div className="text-sm text-muted-foreground">Last Message</div>
              </div>
            </div>
          ) : (
            <div className="text-center text-muted-foreground">Loading status...</div>
          )}
        </CardContent>
      </Card>

      <Tabs defaultValue="counter" className="space-y-4">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="counter">Counter API</TabsTrigger>
          <TabsTrigger value="users">User Management</TabsTrigger>
          <TabsTrigger value="tasks">Task Processing</TabsTrigger>
          <TabsTrigger value="app">App Control</TabsTrigger>
        </TabsList>

        {/* Counter Tab */}
        <TabsContent value="counter">
          <Card>
            <CardHeader>
              <CardTitle>Counter Operations</CardTitle>
              <CardDescription>Demonstrates shared state management across API calls</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="text-center">
                <div className="text-4xl font-bold mb-2">{status?.counter_value ?? 0}</div>
                <div className="text-sm text-muted-foreground">Current Counter Value</div>
              </div>

              <div className="flex gap-2 justify-center">
                <Button onClick={decrementCounter} disabled={loading} variant="outline">
                  - Decrement
                </Button>
                <Button onClick={incrementCounter} disabled={loading}>
                  + Increment
                </Button>
                <Button onClick={resetCounter} disabled={loading} variant="secondary">
                  Reset
                </Button>
              </div>

              <Separator />

              <div className="flex gap-2">
                <Input
                  placeholder="Enter value"
                  value={counterValue}
                  onChange={(e) => setCounterValue(e.target.value)}
                  type="number"
                />
                <Button onClick={setCounter} disabled={loading}>
                  Set Value
                </Button>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Users Tab */}
        <TabsContent value="users">
          <div className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Create User</CardTitle>
                <CardDescription>Test custom struct handling and validation</CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="userName">Name</Label>
                    <Input
                      id="userName"
                      value={userName}
                      onChange={(e) => setUserName(e.target.value)}
                      placeholder="Enter name"
                    />
                  </div>
                  <div>
                    <Label htmlFor="userEmail">Email</Label>
                    <Input
                      id="userEmail"
                      type="email"
                      value={userEmail}
                      onChange={(e) => setUserEmail(e.target.value)}
                      placeholder="Enter email"
                    />
                  </div>
                </div>
                <Button onClick={createUser} disabled={loading} className="w-full">
                  Create User
                </Button>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Users List</CardTitle>
                <CardDescription>Current users in the system</CardDescription>
              </CardHeader>
              <CardContent>
                {users.length === 0 ? (
                  <div className="text-center text-muted-foreground">No users found</div>
                ) : (
                  <div className="space-y-2">
                    {users.map((user) => (
                      <div key={user.id} className="flex items-center justify-between p-3 border rounded">
                        <div>
                          <div className="font-medium">{user.name}</div>
                          <div className="text-sm text-muted-foreground">{user.email}</div>
                        </div>
                        <div className="flex items-center gap-2">
                          <Badge variant={user.active ? "default" : "secondary"}>
                            {user.active ? "Active" : "Inactive"}
                          </Badge>
                          <Button
                            size="sm"
                            variant="outline"
                            onClick={() => toggleUserStatus(user.id)}
                          >
                            Toggle Status
                          </Button>
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        {/* Tasks Tab */}
        <TabsContent value="tasks">
          <div className="space-y-4">
            <Card>
              <CardHeader>
                <CardTitle>Long Running Task</CardTitle>
                <CardDescription>Demonstrates channel-based progress updates</CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="taskName">Task Name</Label>
                    <Input
                      id="taskName"
                      value={taskName}
                      onChange={(e) => setTaskName(e.target.value)}
                      placeholder="Enter task name"
                      disabled={isRunningTask}
                    />
                  </div>
                  <div>
                    <Label htmlFor="taskDuration">Duration (seconds)</Label>
                    <Input
                      id="taskDuration"
                      type="number"
                      min="1"
                      max="60"
                      value={taskDuration}
                      onChange={(e) => setTaskDuration(e.target.value)}
                      disabled={isRunningTask}
                    />
                  </div>
                </div>
                <Button onClick={runLongTask} disabled={isRunningTask || !taskName.trim()} className="w-full">
                  {isRunningTask ? 'Running...' : 'Start Long Task'}
                </Button>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Batch Processing</CardTitle>
                <CardDescription>Simulate processing multiple items with progress updates</CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div>
                  <Label htmlFor="batchSize">Batch Size</Label>
                  <Input
                    id="batchSize"
                    type="number"
                    min="1"
                    max="1000"
                    value={batchSize}
                    onChange={(e) => setBatchSize(e.target.value)}
                    disabled={isRunningTask}
                  />
                </div>
                <Button onClick={runBatchProcessing} disabled={isRunningTask} className="w-full">
                  {isRunningTask ? 'Processing...' : 'Start Batch Processing'}
                </Button>
              </CardContent>
            </Card>

            {taskProgress && (
              <Card>
                <CardHeader>
                  <CardTitle>Task Progress</CardTitle>
                  <CardDescription>Real-time progress from backend</CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div>
                    <div className="flex justify-between text-sm mb-2">
                      <span>Task: {taskProgress.task_id}</span>
                      <span>{taskProgress.progress}%</span>
                    </div>
                    <Progress value={taskProgress.progress} className="w-full" />
                    <div className="text-sm text-muted-foreground mt-2">
                      {taskProgress.message}
                    </div>
                  </div>
                </CardContent>
              </Card>
            )}
          </div>
        </TabsContent>

        {/* App Control Tab */}
        <TabsContent value="app">
          <Card>
            <CardHeader>
              <CardTitle>Application Control</CardTitle>
              <CardDescription>Reset application state and manage global operations</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <Button onClick={resetApp} disabled={loading} variant="destructive" className="w-full">
                Reset Application State
              </Button>
              <div className="text-sm text-muted-foreground text-center">
                This will reset the counter, user count, and processing state
              </div>

              <Separator />

              <div className="space-y-2">
                <Button onClick={loadStatus} disabled={loading} variant="outline" className="w-full">
                  Refresh Status
                </Button>
                <Button onClick={loadUsers} disabled={loading} variant="outline" className="w-full">
                  Refresh Users
                </Button>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>

      <Card className="mt-8">
        <CardHeader>
          <CardTitle>Features Demonstrated</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
            <div className="space-y-2">
              <h4 className="font-semibold">✅ Shared State Management</h4>
              <ul className="space-y-1 text-muted-foreground ml-4">
                <li>• Arc&lt;RwLock&gt; for app data</li>
                <li>• Arc&lt;Mutex&gt; for counter</li>
                <li>• Cross-API state sharing</li>
              </ul>
            </div>
            <div className="space-y-2">
              <h4 className="font-semibold">✅ API Routing</h4>
              <ul className="space-y-1 text-muted-foreground ml-4">
                <li>• Root API at /</li>
                <li>• Counter API at /counter</li>
                <li>• User API at /users</li>
                <li>• Task API at /tasks</li>
              </ul>
            </div>
            <div className="space-y-2">
              <h4 className="font-semibold">✅ Real-time Events</h4>
              <ul className="space-y-1 text-muted-foreground ml-4">
                <li>• Backend-to-frontend events</li>
                <li>• Automatic status updates</li>
                <li>• Error propagation</li>
              </ul>
            </div>
            <div className="space-y-2">
              <h4 className="font-semibold">✅ Progress Channels</h4>
              <ul className="space-y-1 text-muted-foreground ml-4">
                <li>• Real-time progress updates</li>
                <li>• Long-running task handling</li>
                <li>• Batch processing simulation</li>
              </ul>
            </div>
            <div className="space-y-2">
              <h4 className="font-semibold">✅ Custom Types & Errors</h4>
              <ul className="space-y-1 text-muted-foreground ml-4">
                <li>• Custom struct definitions</li>
                <li>• Typed error handling</li>
                <li>• Validation and error propagation</li>
              </ul>
            </div>
            <div className="space-y-2">
              <h4 className="font-semibold">✅ Background Processing</h4>
              <ul className="space-y-1 text-muted-foreground ml-4">
                <li>• Tokio async tasks</li>
                <li>• Periodic event triggers</li>
                <li>• Non-blocking operations</li>
              </ul>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
