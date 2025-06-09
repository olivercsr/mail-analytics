defmodule FileCollector do
  use GenServer

  @impl true
  def init(path) do
    {:ok, path}
  end

  @impl true
  def handle_call() do
  end
end

