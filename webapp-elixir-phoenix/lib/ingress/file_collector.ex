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

  # defp move_file(srcpath, pendingpath) do
  #   with :ok <- File.rename(srcpath, pendingpath) do
  #     {:ok, pendingpath}
  #   end
  # end

  defp process_file(filepath, successfilepath, action) do
    IO.puts("process_file #{filepath}")
    case file_processable?(filepath) do
      {:ok, true} ->
        # {:ok, destfile} = move_file(srcpath, destpath, file)
        action
          && action.(filepath, successfilepath)
          || :ignore
      {:ok, false} -> :ignore
    end |> case do
      :ok ->
        Logger.debug([message: "action successful", file: filepath])
        {filepath, {:ok, nil}}
      {:ok, result} ->
        Logger.debug([message: "action successful", file: filepath])
        {filepath, {:ok, result}}
      {:error, reason} ->
        Logger.warning([message: "action failed", file: filepath, reason: reason])
        {filepath, {:error, reason}}
      any -> {filepath, any}
    end
  end

  @impl true
  def handle_info(:work, state) do
    # wd = File.cwd!()
    interval = state.opts[:interval_seconds] || default_interval()
    basepath = state.opts[:basepath]
    newpath = "#{basepath}/#{state.opts[:srcpath]}"
    newpathlen = String.length(newpath)
    pendingpath = "#{basepath}/#{state.opts[:pendingpath]}"
    donepath = "#{basepath}/#{state.opts[:donepath]}"
    file_action = state.opts[:action]

    Logger.debug([message: "running file collector", name: state.opts[:name], state: state])
    # IO.inspect(state)

    files = Path.wildcard("#{newpath}/**/*")
    # files = File.ls!(path)
    # IO.inspect(files)
    action_results = Enum.map(files, fn file ->
      try do
        with filelen <- String.length(file),
          filepath = file
            |> String.slice(filelen - newpathlen, filelen),
          filedir = Path.dirname(filepath),
          # filename = Path.basename(filepath),
          pendingfiledir = "#{pendingpath}/#{filedir}",
          pendingfilepath = "#{pendingpath}/#{filepath}",
          donefiledir = "#{donepath}/#{filedir}",
          donefilepath = "#{donepath}/#{filepath}" do
          :ok = File.mkdir_p(pendingfiledir)
          :ok = File.rename(file, pendingfilepath)
          :ok = File.mkdir_p(donefiledir)
          process_file(pendingfilepath, donefilepath, file_action)
        end
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

