defmodule Ingress.FileCollector do
  use GenServer

  require Logger
  require Path
  require File

  def default_interval, do: 10

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
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
    with dest <- "#{destpath}/#{Path.basename(filepath)}",
      :ok <- File.mkdir_p(destpath),
      :ok <- File.rename(filepath, dest) do
      {:ok, dest}
    end
  end

  defp process_file(srcpath, destpath, file, action) do
    IO.puts("process_file #{file}")
    case file_processable?(file) do
      {:ok, true} ->
        {:ok, destfile} = move_file(srcpath, destpath, file)
        action
          && action.(destfile)
          || :ignore
      {:ok, false} -> :ignore
    end |> case do
      :ok ->
        Logger.debug([message: "action successful", file: file])
        {file, {:ok, nil}}
      {:ok, result} ->
        Logger.debug([message: "action successful", file: file])
        {file, {:ok, result}}
      {:error, reason} ->
        Logger.warning([message: "action failed", file: file, reason: reason])
        {file, {:error, reason}}
      any -> {file, any}
    end
  end

  @impl true
  def handle_info(:work, state) do
    # wd = File.cwd!()
    interval = state.opts[:interval_seconds] || default_interval()
    basepath = state.opts[:basepath]
    srcpath = "#{basepath}/#{state.opts[:srcpath]}"
    destpath = "#{basepath}/#{state.opts[:destpath]}"
    file_action = state.opts[:action]

    Logger.debug([message: "running file collector", state: state])
    # IO.inspect(state)

    files = Path.wildcard("#{srcpath}/**/*")
    # files = File.ls!(path)
    # IO.inspect(files)
    action_results = Enum.map(files, fn file ->
      try do
        process_file(srcpath, destpath, file, file_action)
      rescue
        e -> {file, {:error, e}}
      end
    end)
      |> Enum.filter(fn item -> item != :ignore end)
      |> Enum.reduce(%{}, fn {file, result}, acc -> Map.put(acc, file, result) end)

    IO.inspect(action_results)

    schedule_work(interval)

    {:noreply, state}
  end

  defp schedule_work(interval) do
    Process.send_after(self(), :work, (interval || default_interval()) * 1000)
  end
end

