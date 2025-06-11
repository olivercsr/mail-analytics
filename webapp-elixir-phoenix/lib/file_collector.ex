defmodule FileCollector do
  use GenServer

  require Logger
  require Path
  require File

  def default_interval, do: 10

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(opts) do
    schedule_work(opts[:interval_seconds])

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

  defp move_file(_srcpath, destpath, filepath) do
    dest = "#{destpath}/#{Path.basename(filepath)}"
    File.rename(filepath, dest)
    dest
  end

  defp process_file(srcpath, tmppath, destpath, file, action) do
    case file_processable?(file) do
      {:ok, true} ->
        tmpfile = move_file(srcpath, tmppath, file)
        {:tmpfile, tmpfile, action
          && action.(tmpfile)
          || :ignore}
      {:ok, false} -> :ignore
      any -> any
    end |> case do
      {:tmpfile, tmpfile, {:ok, result}} ->
        Logger.debug([message: "action successful", file: file])
        move_file(tmppath, destpath, tmpfile)
        result
      {:tmpfile, tmpfile, {:error, reason}} ->
        Logger.warning([message: "action failed", reason: reason])
        File.touch!(tmpfile)
        move_file(tmppath, srcpath, tmpfile)
      :ignore -> nil
      any -> any
    end
  end

  @impl true
  def handle_info(:work, state) do
    # wd = File.cwd!()
    srcpath = state.opts[:srcpath]
    tmppath = state.opts[:tmppath]
    destpath = state.opts[:destpath]
    file_action = state.opts[:action]
    interval = state.opts[:interval_seconds] || default_interval()

    Logger.debug([message: "running file collector", state: state])
    # IO.inspect(state)

    files = Path.wildcard("#{srcpath}/**/*")
    # files = File.ls!(path)
    # IO.inspect(files)
    results = Enum.map(files, fn file ->
      try do
        process_file(srcpath, tmppath, destpath, file, file_action)
      rescue
        e -> {:error, e}
      end
    end)
      |> Enum.filter(fn item -> item != nil end)

    IO.inspect(results)

    schedule_work(interval)

    {:noreply, state}
  end

  defp schedule_work(interval) do
    Process.send_after(self(), :work, (interval || default_interval()) * 1000)
  end
end

