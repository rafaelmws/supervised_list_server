import Supervisor.Spec

defmodule ListManager do
  use GenServer

  #public api
  def start_link do
    child_process = [
      worker(ListServer, [], restart: :transient)
    ]

    {:ok, sup} = Supervisor.start_link(child_process, strategy: :simple_one_for_one)

    :gen_server.start_link({:local, :list_manager}, __MODULE__, [sup], [])
  end

  def start(server_name) do
    :gen_server.cast :list_manager, {:start, server_name}
  end

  def stop(server_name) do
    :gen_server.cast :list_manager, {:stop, server_name}
  end

  def list do
    :gen_server.call :list_manager, :list
  end

  #server api
  def init(pid) do
    new_state = %{
      supervisor: pid,
      servers: []
    }

    {:ok, new_state}
  end

  def handle_cast({:start, server_name}, state) do
    IO.puts "[start]==================="
    {:ok, child_pid} = Supervisor.start_child(state[:supervisor], server_name)
    server = %{pid: child_pid, name: server_name}

    new_state = %{
      supervisor: state[:supervisor],
      servers: state[:servers] ++ [server]
    }

    {:noreply, new_state}
  end

  def handle_cast({:stop, server_name}, state) do
    {:ok, server} = find_server(state[:servers], server_name)
    Supervisor.delete_child(state[:supervisor], server[:pid])

    new_state = %{
      supervisor: state[:supervisor],
      servers: List.delete(state[:servers], server)
    }

    {:noreply, new_state}
  end

  def handle_call(:list, _from, state) do
    {:reply, state, state}
  end

  defp find_server(servers, server_name) do
    result = servers |> Enum.filter(fn (server) -> server[:name] == server_name end)

    if Enum.count(result) > 0 do
      {:ok, Enum.at(result, 0)}
    else
      {:err, nil}
    end
  end

end
