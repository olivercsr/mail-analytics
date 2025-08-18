# TODO: make this a Task instead of a Genserver,
#   as otherwise a crash for a call of a client will
#   clear this Genserver's mailbox, potentially losing
#   the pending requests of other clients.
#   Although this can be mitigated by using call()
#   instead of cast(), using call() will serialize
#   all clients' calls, which is not what we intend.
#   A more appropriate handling would be to run the
#   decoding logic via a Task.

defmodule Ingress.AttachmentDecoder do
  use GenServer

  require Logger
  require MIME
  require StreamGzip
  require Zstream

  # Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  def decode(pid, filepath, donefilepath, filesubdir) do
    GenServer.cast(pid, {:decode, filepath, donefilepath, filesubdir})
  end

  # Server

  defp decode_attachment(filepath, reportsdir, :"application/gzip") do
    donefilename = Path.basename(filepath, Path.extname(filepath))
    reportpath = Path.absname("#{reportsdir}/#{donefilename}")

    :ok = File.stream!(filepath)
      |> StreamGzip.gunzip()
      |> Stream.into(File.stream!(reportpath))
      |> Stream.run()

    :ok
  end

  defp decode_attachment(filepath, reportsdir, :"application/zip") do
    %{} = File.stream!(filepath)
      |> Zstream.unzip()
      |> Enum.reduce(%{}, fn
        {:entry, %Zstream.Entry{name: filename}}, state ->
          reportpath = Path.absname("#{reportsdir}/#{filename}")
          # IO.puts("entry #{filename} #{donefilepath}")
          Map.merge(state, %{filename: filename, stream: File.stream!(reportpath)})
        {:data, :eof}, %{:stream => stream} ->
          # IO.puts("eof")
          Stream.run(stream)
          %{}
        {:data, data}, %{:stream => stream} = state ->
          # IO.puts("data #{inspect data}")
          nstream = Stream.into(data, stream)
          Map.put(state, :stream, nstream)
      end)

    :ok
  end

  @impl true
  def init(opts) do
    {:ok, %{opts: opts}}
  end

  @impl true
  def handle_cast({:decode, filepath, donefilepath, filesubdir}, state) do
    Logger.debug([module: __MODULE__, message: "AttachmentsDecoder.decode start", filepath: filepath, donefilepath: donefilepath, filesubdir: filesubdir])

    basepath = Path.absname(state.opts[:basepath])
    reportsdir = Path.absname("#{basepath}/#{state.opts[:dmarcreportsdir]}/#{filesubdir}")

    mime_type = MIME.from_path(filepath)
      |> String.to_atom()

    :ok = File.mkdir_p(reportsdir)
    :ok = decode_attachment(filepath, reportsdir, mime_type)

    :ok = File.rename(filepath, donefilepath)

    Logger.debug([module: __MODULE__, message: "AttachmentsDecoder.decode done"])

    {:noreply, state}
  end
end

