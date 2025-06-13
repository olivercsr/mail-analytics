defmodule Ingress.MailDecoder do
  use GenServer

  require Logger
  require File
  require Mail

  # Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  def decode(pid, mail) do
    GenServer.cast(pid, {:decode, mail})
  end

  # Server

  defp search_msg(mail_msg, attachments) do
    with dispositions <- get_in(mail_msg.headers["content-disposition"]) || [],
      disposition <- Enum.at(dispositions, 0, "")
        |> String.downcase() do
      case {mail_msg, disposition} do
        {%{multipart: false}, "attachment"} ->
          [mail_msg | attachments]
        {%{multipart: false}, _} ->
          attachments
        {%{multipart: true}, _} ->
          find_attachments(mail_msg.parts || [])
        _ -> Logger.warning([message: "unexpected mail message structure", mail_msg: mail_msg])
          attachments
      end
    end
  end

  defp get_value_from_param_header(header, key, default) do
    header
      |> Enum.at(1, {})
      |> Tuple.to_list()
      |> Enum.chunk_every(2)
      |> Enum.filter(fn [k, _] -> String.downcase(k || "") == key end)
      |> Enum.map(fn [_, v] -> v end)
      |> Enum.at(0, default)
  end

  defp default_filename() do
    # TODO: implement
    "defaultfilename"
  end

  defp convert(mail_msg) do
    with headers <- get_in(mail_msg.headers),
      disposition_header <- get_in(headers["content-disposition"]) || [],
      filename <- get_value_from_param_header(
        disposition_header,
        "filename",
        default_filename()
      ),
      transfer_encoding <- get_in(headers["transfer-encoding"]) || "base64",
      content_type_header <- get_in(headers["content-type"]),
      content_type <- Enum.at(content_type_header, 0),
      content_charset <- get_value_from_param_header(
        content_type_header,
        "charset",
        "utf8"
      ),
      data <- get_in(mail_msg.body) do
      %Attachment{
        filename: filename,
        transfer_encoding: transfer_encoding,
        content_type: content_type,
        content_charset: content_charset,
        data: Util.BinaryStream.from_binary(data)
      }
    end
  end

  defp find_attachments(mail_msgs) do
    mail_msgs
    |> Enum.reduce([], &search_msg/2)
  end

  # defp move_file(basepath, srcpath, destpath) do
  #   src = "#{basepath}/#{srcpath}"
  #   dest = "#{basepath}/#{destpath}"
  #   File.rename(src, dest)
  #   dest
  # end

  @impl true
  def init(opts) do
    {:ok, %{opts: opts}}
  end

  @impl true
  def handle_cast({:decode, file}, state) do
    # basepath = state.opts[:basepath]
    # maildestpath = "#{basepath}/#{state.opts[:maildestpath]}"
    # attachmentspath = "#{basepath}/#{state.opts[:attachmentspath]}"

    IO.puts("MailDecoder: #{inspect file}")

    with {:ok, file_contents} <- File.read(file),
      mail_msg <- Mail.parse(file_contents) do
      attachments = find_attachments([mail_msg])
        |> Enum.map(&convert/1)
        # |> Enum.map(&Ingress.AttachmentProcessor.process(AttachmentProcessor, &1))
      # case Enum.all?(submit_states, fn state -> state == :ok end) do
      #   true -> :ok
      #   false -> :error
      # end
      IO.inspect(attachments)

      # move_file()
      # TODO: move mail file to done-folder
      # TODO: save attachments as files

    end

    {:noreply, state}
  end
end

