defmodule Ingress.AttachmentDecoder do
  use GenServer

  require MIME
  require StreamGzip
  require Zstream

  # Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  def decode(pid, filepath, donefilepath) do
    GenServer.cast(pid, {:decode, filepath, donefilepath})
  end

  # Server

  defp decode_attachment(filepath, donefiledir, :"application/gzip") do
    donefilename = Path.basename(filepath, Path.extname(filepath))
    donefilepath = Path.absname("#{donefiledir}/#{donefilename}")

    :ok = File.stream!(filepath)
      |> StreamGzip.gunzip()
      |> Stream.into(File.stream!(donefilepath))
      |> Stream.run()

    :ok
  end

  defp decode_attachment(filepath, donefiledir, :"application/zip") do
    %{} = File.stream!(filepath)
      |> Zstream.unzip()
      |> Enum.reduce(%{}, fn
        {:entry, %Zstream.Entry{name: filename}}, state ->
          donefilepath = Path.absname("#{donefiledir}/#{filename}")
          # IO.puts("entry #{filename} #{donefilepath}")
          Map.merge(state, %{filename: filename, stream: File.stream!(donefilepath)})
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
  def handle_cast({:decode, filepath, donefiledir}, state) do
    mime_type = MIME.from_path(filepath)
      |> String.to_atom()

    :ok = decode_attachment(filepath, donefiledir, mime_type)

    :ok = File.rm(filepath)

    {:noreply, state}
  end
end

