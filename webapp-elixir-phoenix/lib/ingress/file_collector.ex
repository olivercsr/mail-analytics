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

  defp file_processable?(filestat) do
    with {:ok, mtime} <- DateTime.from_unix(filestat.mtime),
      {:ok, now} <- DateTime.now("Etc/UTC"),
      threshold_time <- DateTime.add(now, -5, :minute) do
      {:ok, DateTime.before?(mtime, threshold_time)}
    end
  end

  defp process_file(filepath, filestat, donefilepath, action) do
    Logger.debug([module: __MODULE__, message: "process_file start", file: filepath, donefilepath: donefilepath])
    case file_processable?(filestat) do
      {:ok, true} ->
        action
          && action.(filepath, donefilepath)
          || :ignore
      {:ok, false} -> :ignore
    end |> case do
      :ok ->
        Logger.debug([module: __MODULE__, message: "action successful", file: filepath])
        {filepath, {:ok, nil}}
      {:ok, result} ->
        Logger.debug([module: __MODULE__, message: "action successful", file: filepath])
        {filepath, {:ok, result}}
      {:error, reason} ->
        Logger.warning([module: __MODULE__, message: "action failed", file: filepath, reason: reason])
        {filepath, {:error, reason}}
      any -> {filepath, any}
    end
  end

  @impl true
  def handle_info(:work, state) do
    # wd = File.cwd!()
    interval = state.opts[:interval_seconds] || default_interval()
    basepath = Path.absname(state.opts[:basepath])
    newpath = Path.absname("#{basepath}/#{state.opts[:newpath]}")
    newpathlen = String.length(newpath)
    pendingpath = Path.absname("#{basepath}/#{state.opts[:pendingpath]}")
    donepath = Path.absname("#{basepath}/#{state.opts[:donepath]}")
    file_action = state.opts[:action]

    Logger.debug([module: __MODULE__, message: "running file collector", name: state.opts[:name], state: state])

    action_results = Path.wildcard("#{newpath}/**/*")
      |> Enum.map(fn file ->
        try do
          {:ok, filestat} = File.stat(file, time: :posix)
          {:ok, file, filestat}
        rescue
          e -> {:error, file, e}
        end
      end)
      |> Enum.filter(fn info ->
        case info do
          {:ok, _file, filestat} -> filestat.type == :regular
          {:error, _file, _error} = info -> info
        end
      end)
      |> Enum.map(fn info ->
        case info do
          {:ok, file, filestat} ->
            try do
              with filelen <- String.length(file),
                filepath = file
                  |> String.slice(newpathlen - filelen + 1, filelen),
                filedir = Path.dirname(filepath),
                # filename = Path.basename(filepath),
                pendingfiledir = Path.absname("#{pendingpath}/#{filedir}"),
                pendingfilepath = Path.absname("#{pendingpath}/#{filepath}"),
                donefiledir = Path.absname("#{donepath}/#{filedir}"),
                donefilepath = Path.absname("#{donepath}/#{filepath}") do
                # IO.inspect({file, filepath, filedir, pendingfiledir, pendingfilepath, donefiledir, donefilepath})
                :ok = File.mkdir_p(pendingfiledir)
                :ok = File.rename(file, pendingfilepath)
                :ok = File.mkdir_p(donefiledir)
                process_file(pendingfilepath, filestat, donefilepath, file_action)
              end
            rescue
              e -> {file, {:error, e}}
            end
          {:error, _file, _error} = info -> info
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

