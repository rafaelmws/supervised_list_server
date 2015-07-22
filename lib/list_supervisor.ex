defmodule ListSupervisor do
  use Supervisor

  def start_link do
  end

  def init([]) do
  end

  def start_server(supervisor, name) do
    IO.puts "ListSupervisor.start_server(#{name})"
    Supervisor.start_child(supervisor, name)
  end

  def stop_server(supervisor, pid) do
    Supervisor.delete_child(supervisor, pid)
    Supervisor.terminate_child(supervisor, pid)
  end

end
