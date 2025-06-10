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

  defp file_processable?(filepath) do
    with {:ok, filestat} <- File.stat(filepath, time: :posix),
      {:ok, mtime} <- DateTime.from_unix(filestat.mtime),
      {:ok, now} <- DateTime.now("Etc/UTC"),
      threshold_time <- DateTime.add(now, -5, :minute) do
      {:ok, DateTime.before?(mtime, threshold_time)}
    end
  end

  @impl true
  def handle_info(:work, state) do
    # wd = File.cwd!()
    srcpath = state.opts[:srcpath]
    destpath = state.opts[:destpath]

    IO.puts("working with srcpath:#{srcpath} and destpath:#{destpath}")
    # IO.inspect(state)

    files = Path.wildcard("#{srcpath}/**/*")
    # files = File.ls!(path)
    # IO.inspect(files)
    Enum.map(files, fn file ->
      case file_processable?(file) do
        {:ok, true} -> IO.puts("yes: #{file}")
        {:ok, false} -> IO.puts("no:  #{file}")
      end
    end)

    schedule_work()
    {:noreply, state}
  end

  defp schedule_work() do
    Process.send_after(self(), :work, 10 * 1000)
  end
end

