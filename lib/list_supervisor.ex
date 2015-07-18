defmodule ListSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      worker(ListServer, [], restart: :permanent)
    ]
    supervise(children, strategy: :simple_one_for_one)
  end

  def start_server(supervisor, name) do
    {:ok, pid} = Supervisor.start_child(supervisor, [name])
    ListManager.register(name, pid)
  end

  def stop_server(supervisor, pid, name) do
    Supervisor.delete_child(supervisor, pid)
    Supervisor.terminate_child(supervisor, pid)
    ListManager.unregister(supervisor, name)
  end

end
