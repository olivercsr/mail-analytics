defmodule FileCollector do
  use GenServer

  require Path
  require File

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
    wd = File.cwd!()
    path = "#{wd}/#{state.opts[:path]}"

    IO.puts("working with path: #{path}")
    IO.inspect(state)

    files = Path.wildcard("./lib/**/*")
    # files = File.ls!(path)
    IO.inspect(files)
    Enum.map(files, fn f -> IO.puts(f); File.stat!(f).mtime |> IO.inspect() end)

    schedule_work()
    {:noreply, state}
  end

  defp schedule_work() do
    Process.send_after(self(), :work, 10 * 1000)
  end
end

