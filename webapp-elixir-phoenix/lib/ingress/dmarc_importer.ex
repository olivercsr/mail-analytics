defmodule Ingress.DmarcImporter do
  use GenServer

  require Logger

  # Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  def import_file(pid, filepath, donefilepath, filesubdir) do
    GenServer.cast(pid, {:import, filepath, donefilepath, filesubdir})
  end

  # Server

  @impl true
  def init(opts) do
    {:ok, %{opts: opts}}
  end

  @impl true
  def handle_cast({:import, filepath, donefilepath, filesubdir}, state) do
    Logger.debug([module: __MODULE__, message: "DmarcImporter.import start", filepath: filepath, donefilepath: donefilepath])

    filename = Path.basename(filepath)
    stream = File.stream!(filepath)
    {:ok, _} = Db.ExistDb.store(Db.ExistDb, filesubdir, filename, stream)

    :ok = File.rename(filepath, donefilepath)

    Logger.debug([module: __MODULE__, message: "DmarcImporter.import done"])

    {:noreply, state}
  end
end

