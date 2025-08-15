defmodule Ingress.FileCollector do
  use GenServer

  require Logger
  require Path
  require File

  def default_interval, do: 10
  def default_minfileage, do: 1

  defmodule Config do
    defstruct [:interval_seconds, :minfileage, :basepath, :newpath, :pendingpath, :donepath]
  end

  def start_link(opts) do
    # Logger.info([module: __MODULE__, message: "FileCollector start_link", opts: opts])

    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  @impl true
  def init(opts) do
    name = opts[:name]
    config = opts[:config]
    action = opts[:action]

    Logger.info([module: __MODULE__, message: "FileCollector init", name: name, config: config])

    schedule_work(config.interval_seconds)

    {:ok, %{name: name, config: config, action: action}}
  end

  @impl true
  def handle_call(data, _from, state) do
    {:reply, data, state}
  end

  @impl true
  def handle_cast(_data, state) do
    {:noreply, state}
  end

  defp file_processable?(filestat, minfileage) do
    with {:ok, mtime} <- DateTime.from_unix(filestat.mtime),
      {:ok, now} <- DateTime.now("Etc/UTC"),
      threshold_time <- DateTime.add(now, -minfileage, :minute) do
      {:ok, DateTime.before?(mtime, threshold_time)}
    end
  end

  defp process_file(filedir, filepath, filestat, pendingfiledir, pendingfilepath, donefiledir, donefilepath, minfileage, action) do
    Logger.debug([module: __MODULE__, message: "process_file start", filedir: filedir, file: filepath, pendingfilepath: pendingfilepath])
    case file_processable?(filestat, minfileage) do
      {:ok, true} ->
        :ok = File.mkdir_p(pendingfiledir)
        :ok = File.mkdir_p(donefiledir)
        :ok = File.rename(filepath, pendingfilepath)
        action
          && action.(pendingfilepath, donefilepath, filedir)
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
    interval = state.config.interval_seconds || default_interval()
    minfileage = state.config.minfileage || default_minfileage()
    basepath = Path.absname(state.config.basepath)
    newpath = Path.absname("#{basepath}/#{state.config.newpath}")
    newpathlen = String.length(newpath)
    pendingpath = Path.absname("#{basepath}/#{state.config.pendingpath}")
    donepath = Path.absname("#{basepath}/#{state.config.donepath}")
    file_action = state.action

    Logger.debug([module: __MODULE__, message: "running file collector", name: state.name, state: state])

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
                process_file(filedir, file, filestat, pendingfiledir, pendingfilepath, donefiledir, donefilepath, minfileage, file_action)
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

