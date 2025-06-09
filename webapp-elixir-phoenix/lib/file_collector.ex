defmodule FileCollector do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(opts) do
    schedule_work()
    {:ok, %{opts: opts}}
  end

  @impl true
  def handle_call(data, _from, state) do
    {:reply, data, state}
  end

  @impl true
  def handle_cast(_data, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(:work, state) do
    IO.puts("working with path: #{state.opts[:path]}")
    IO.inspect(state)

    schedule_work()
    {:noreply, state}
  end

  defp schedule_work() do
    Process.send_after(self(), :work, 10 * 1000)
  end
end

