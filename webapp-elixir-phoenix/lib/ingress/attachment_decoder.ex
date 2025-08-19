defmodule Ingress.AttachmentDecoder do
  require Logger
  require MIME
  require StreamGzip
  require Zstream

  defmodule Config do
    defstruct [:basepath, :dmarcreportsdir]
  end

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

  def decode(config, filepath, donefilepath, filesubdir) do
    Logger.debug([module: __MODULE__, message: "AttachmentsDecoder.decode start", filepath: filepath, donefilepath: donefilepath, filesubdir: filesubdir])

    basepath = Path.absname(config.basepath)
    reportsdir = Path.absname("#{basepath}/#{config.dmarcreportsdir}/#{filesubdir}")

    mime_type = MIME.from_path(filepath)
      |> String.to_atom()

    :ok = File.mkdir_p(reportsdir)
    :ok = decode_attachment(filepath, reportsdir, mime_type)

    :ok = File.rename(filepath, donefilepath)

    Logger.debug([module: __MODULE__, message: "AttachmentsDecoder.decode done"])

    :ok
  end

  def decode_async(config, filepath, donefilepath, filesubdir) do
    fn -> decode(config, filepath, donefilepath, filesubdir) end
      |> Task.async
  end
end

