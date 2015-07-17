defmodule ListServer do
  use GenServer

  def start_link(process_name) do
    :gen_server.start_link({:local, process_name}, __MODULE__, [], [])
  end

  def clear(process_name) do
    :gen_server.cast process_name, :clear
  end

  def add(process_name, item) do
    :gen_server.cast process_name, {:add, item}
  end

  def remove(process_name, item) do
    :gen_server.cast process_name, {:remove, item}
  end

  def items(process_name) do
    :gen_server.call process_name, :items
  end

  def crash(process_name) do
    :gen_server.cast process_name, :crash
  end

  ##### gen_server
  def init do
    {:ok, []}
  end

  def handle_cast(:clear, _list) do
    {:noreply, []}
  end

  def handle_cast({:add, item}, list) do
    {:noreply, list ++ [item]}
  end

  def handle_cast({:remove, item}, list) do
    {:noreply, List.delete(list, item)}
  end

  def handle_cast(:crash, _list, _state) do
    1 = 2
  end

  def handle_call(:items, _from, list) do
    {:reply, list, list}
  end

end