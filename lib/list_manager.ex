import Supervisor.Spec

defmodule ListManager do
  use GenServer

  #public api
  def start_link do
    {:ok, sup} = ListSupervisor.start_link
    :gen_server.start_link({:local, :list_manager}, __MODULE__, sup, [])
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

  def register(name, pid) do
    GenServer.call :list_manager, {:register, name, pid}
  end

  def unregister(name) do
    GenServer.call :list_manager, {:unregister, name}
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
    ListSupervisor.start_server(state[:supervisor], server_name)
    {:noreply, state}
  end

  def handle_cast({:stop, server_name}, state) do
    {:ok, server} = find_server(state, server_name)
    ListSupervisor.stop_server(state[:supervisor], server[:pid], server[:name])
    {:noreply, state}
  end

  def handle_cast({:register, name, pid}, state) do
    {:noreply, add_server(state, name, pid)}
  end

  def handle_cast({:unregister, name}, state) do
    {:noreply, remove_server(state, name)}
  end

  def handle_call(:list, _from, state) do
    {:reply, state, state}
  end

  defp add_server(state, name, pid) do
    %{
      supervisor: state[:supervisor],
      servers: state[:servers] ++ [%{name: name, pid: pid}]
    }
  end

  defp remove_server(state, name) do
    %{
      supervisor: state[:supervisor],
      servers: state[:servers] |> Enum.filter fn (item) -> item[:name] != name end
    }
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
