defmodule Ingress.DmarcImporter do
  require Logger

  # defmodule Config do
  #   defstruct [:basepath, :attachmentsdir]
  # end

  def import_file(filepath, donefilepath, filesubdir) do
    Logger.debug([module: __MODULE__, message: "DmarcImporter.import start", filepath: filepath, donefilepath: donefilepath])

    filename = Path.basename(filepath)
    stream = File.stream!(filepath)
    {:ok, _} = Db.ExistDb.store(Db.ExistDb, filesubdir, filename, stream)

    :ok = File.rename(filepath, donefilepath)

    Logger.debug([module: __MODULE__, message: "DmarcImporter.import done"])

    :ok
  end
end

